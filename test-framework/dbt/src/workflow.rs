use std::{
    collections::HashSet,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};

use anyhow::{bail, Context};
use log::debug;

use crate::{
    cargo_test_directory::{CargoWorkspace, TestDefinition},
    debugger::{self, Debugger, DebuggerOutput},
    script::PhaseConfig,
    test_result::{Status, TestResult},
};

pub struct CompiledTestCases {
    /// The target directory that Cargo used when compiling the workspace
    pub cargo_target_directory: PathBuf,

    /// The Cargo workspace containing all test projects
    pub cargo_workspace: Arc<CargoWorkspace>,

    pub cargo_profiles: Vec<Arc<str>>,
}

pub fn compile_cargo_tests(
    cargo_test_directory: &Path,
    cargo_target_directory: &Path,
    cargo_profiles: &Vec<String>,
) -> anyhow::Result<CompiledTestCases> {
    // For now just allow debug and release Cargo profiles
    for cargo_profile in cargo_profiles {
        match &cargo_profile[..] {
            "debug" | "release" => {
                // OK
            }
            other => {
                bail!(
                    "Cargo profile `{}` is not supported. Use `debug` or `release` instead.",
                    other
                )
            }
        }
    }

    let cargo_test_directory = Arc::new(CargoWorkspace::load(cargo_test_directory)?);
    let cargo_target_directory = cargo_target_directory.canonicalize()?;

    for cargo_profile in cargo_profiles {
        let executable_directory = cargo_target_directory.join(cargo_profile);

        println!(
            "Compiling cargo test packages in {} for Cargo profile `{}`",
            cargo_test_directory.root_path.display(),
            cargo_profile
        );

        let mut cargo_command = Command::new("cargo");

        cargo_command.arg("build");

        if cargo_profile != "debug" {
            assert_eq!(cargo_profile, "release");
            cargo_command.arg("--release");
        }

        cargo_command
            .arg("--target-dir")
            .arg(&cargo_target_directory);

        cargo_command.current_dir(&cargo_test_directory.root_path);

        cargo_command.env("RUSTFLAGS", "-Ccodegen-units=1");
        cargo_command.env("RUSTFLAGS", "-Cdebuginfo=2");
        cargo_command.env("CARGO_INCREMENTAL", "0");

        debug!("Cargo command: {:?}", cargo_command);

        let exit_status = cargo_command.status()?;

        if exit_status.success() {
            for test_project_def in &cargo_test_directory.cargo_packages {
                for test_def in &test_project_def.test_definitions {
                    let expected_executable = executable_directory.join(&test_def.executable_name);
                    if !expected_executable.exists() {
                        bail!(
                            "Expected test executable at {} but it does not exist.",
                            expected_executable.display()
                        )
                    }
                }
            }
        }

        if !exit_status.success() {
            bail!("test case compilation failed");
        }
    }

    Ok(CompiledTestCases {
        cargo_target_directory,
        cargo_workspace: cargo_test_directory,
        cargo_profiles: cargo_profiles.iter().map(|p| Arc::from(p.trim())).collect(),
    })
}

pub fn run_cargo_tests(
    test_cases: &CompiledTestCases,
    debugger: &Debugger,
    output_dir: &Path,
) -> anyhow::Result<Vec<TestResult>> {
    let test_count = test_cases
        .cargo_workspace
        .cargo_packages
        .iter()
        .map(|p| p.test_definitions.len())
        .sum();

    let mut test_results = Vec::with_capacity(test_count);

    for cargo_profile in &test_cases.cargo_profiles {
        println!();
        println!(
            "{} ({}) -- running {} tests for Cargo profile `{}`",
            debugger.kind.name(),
            debugger.version,
            test_count,
            cargo_profile,
        );
        println!();

        for test_project_def in &test_cases.cargo_workspace.cargo_packages {
            for test_definition in &test_project_def.test_definitions {
                let phases = test_definition
                    .script
                    .phases(&debugger.evaluation_context(cargo_profile, &PhaseConfig::Live));

                let output_dir_for_test =
                    output_dir_for_test(test_definition, cargo_profile, output_dir)?;

                for phase in &phases {
                    if *phase == PhaseConfig::Live && phases.len() == 1 {
                        print!("test {} .. ", test_definition.name);
                    } else {
                        print!("test {} ({}) .. ", test_definition.name, phase);
                    }

                    let test_result = run_test(
                        debugger,
                        test_definition,
                        &test_cases.cargo_target_directory,
                        cargo_profile,
                        phase,
                        &output_dir_for_test,
                    )?;

                    println!("{}", test_result.status.short_description());

                    test_results.push(test_result);
                }
            }
        }
    }

    Ok(test_results)
}

fn run_test(
    debugger: &Debugger,
    test_definition: &TestDefinition,
    cargo_target_directory: &Path,
    cargo_profile: &Arc<str>,
    phase: &PhaseConfig,
    output_dir_for_test: &Path,
) -> anyhow::Result<TestResult> {
    if debugger.ignore_test(test_definition, cargo_profile, phase) {
        return Ok(TestResult::new(
            test_definition,
            debugger,
            cargo_profile,
            Status::Ignored,
        ));
    }

    let debugger_script = generate_debugger_script(
        test_definition,
        debugger,
        cargo_profile,
        phase,
        output_dir_for_test,
    )?;

    if debugger_script.is_empty() {
        return Ok(TestResult::new(
            test_definition,
            debugger,
            cargo_profile,
            Status::Ignored,
        ));
    }

    let debugger_script_path =
        save_debugger_script(debugger, debugger_script, output_dir_for_test, phase)?;

    let debugger_output = match phase {
        PhaseConfig::Live => {
            let debuggee_path =
                local_debuggee_path(cargo_target_directory, cargo_profile, test_definition);
            debugger.run(&debugger_script_path, &debuggee_path, None)?
        }
        PhaseConfig::CrashDump { tag } => {
            let DebuggeePaths {
                crashdump,
                executable,
            } = debuggee_paths(
                test_definition,
                cargo_profile,
                output_dir_for_test,
                cargo_target_directory,
                tag,
            )?;

            debugger.run(&debugger_script_path, &executable, Some(&crashdump))?
        }
    };

    process_debugger_output(
        debugger,
        test_definition,
        cargo_profile,
        phase,
        debugger_output,
        output_dir_for_test,
    )
}

struct DebuggeePaths {
    executable: PathBuf,
    crashdump: PathBuf,
}

// outdir/target/{cargo_profile}/{executable_name}
fn local_debuggee_path(
    cargo_target_directory: &Path,
    cargo_profile: &Arc<str>,
    test_definition: &TestDefinition,
) -> PathBuf {
    cargo_target_directory
        .join(&cargo_profile[..])
        .join(&test_definition.executable_name)
}

fn crashdump_path(output_dir_for_test: &Path, tag: &str) -> PathBuf {
    let crashdump_dir = output_dir_for_test.join("crashdumps").join(tag);
    crashdump_dir.join("crashdump.dmp")
}

fn debuggee_paths(
    test_definition: &TestDefinition,
    cargo_profile: &Arc<str>,
    output_dir_for_test: &Path,
    cargo_target_directory: &Path,
    tag: &Arc<str>,
) -> anyhow::Result<DebuggeePaths> {
    let crashdump_file_path = crashdump_path(output_dir_for_test, tag);

    let executable_paths_to_check = [
        Path::new(test_definition.executable_name.as_os_str()).with_extension(""),
        Path::new(test_definition.executable_name.as_os_str()).with_extension("exe"),
    ];

    let mut found_executable = None;

    for executable_name in executable_paths_to_check {
        let path = crashdump_file_path.with_file_name(executable_name);

        if path.exists() {
            found_executable = Some(path);
            break;
        }
    }

    let executable = found_executable.unwrap_or_else(|| {
        local_debuggee_path(cargo_target_directory, cargo_profile, test_definition)
    });

    Ok(DebuggeePaths {
        crashdump: crashdump_file_path,
        executable,
    })
}

fn save_debugger_script(
    debugger: &Debugger,
    script_contents: String,
    output_dir_for_test: &Path,
    phase: &PhaseConfig,
) -> anyhow::Result<PathBuf> {
    let file_name = format!(
        "{}-{}-{}.dbgscript",
        debugger.kind.name(),
        debugger.version,
        phase
    );
    let path = output_dir_for_test.join(file_name);
    std::fs::write(&path, script_contents)?;
    Ok(path)
}

fn generate_debugger_script(
    test_definition: &TestDefinition,
    debugger: &Debugger,
    cargo_profile: &Arc<str>,
    phase: &PhaseConfig,
    output_dir_for_test: &Path,
) -> anyhow::Result<String> {
    let mut crashdump_paths_generated: HashSet<PathBuf> = Default::default();

    let script = debugger::generate_debugger_script(
        debugger,
        test_definition,
        cargo_profile,
        phase,
        &mut |tag| {
            let path = crashdump_path(output_dir_for_test, tag);
            crashdump_paths_generated.insert(path.clone());
            path
        },
    );

    for path in crashdump_paths_generated {
        let directory = path.parent().unwrap();
        std::fs::create_dir_all(&directory).with_context(|| {
            format!(
                "while trying to create crashdumps directory for test: {}",
                directory.display()
            )
        })?;
    }

    Ok(script)
}

fn process_debugger_output(
    debugger: &Debugger,
    test_definition: &TestDefinition,
    cargo_profile: &Arc<str>,
    phase: &PhaseConfig,
    debugger_output: DebuggerOutput,
    output_dir_for_test: &Path,
) -> anyhow::Result<TestResult> {
    std::fs::write(
        output_dir_for_test.join(format!(
            "{}-{}-{}.stdout",
            debugger.kind.name(),
            debugger.version,
            phase,
        )),
        &debugger_output.stdout,
    )?;
    std::fs::write(
        output_dir_for_test.join(format!(
            "{}-{}-{}.stderr",
            debugger.kind.name(),
            debugger.version,
            phase,
        )),
        &debugger_output.stderr,
    )?;

    Ok(debugger::process_debugger_output(
        debugger,
        test_definition,
        debugger_output,
        cargo_profile,
        phase,
    ))
}

fn output_dir_for_test(
    test_definition: &TestDefinition,
    cargo_profile: &Arc<str>,
    output_dir: &Path,
) -> anyhow::Result<PathBuf> {
    let mut dir_name = test_definition.flat_name();
    dir_name.push('@');
    dir_name.push_str(&cargo_profile);

    let path = output_dir.join(dir_name);
    std::fs::create_dir_all(&path).with_context(|| {
        format!(
            "while trying to create working directory for test: {}",
            path.display()
        )
    })?;

    Ok(path)
}
