use std::{
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};

use anyhow::{bail, Context};
use log::debug;

use crate::{
    cargo_test_directory::{CargoWorkspace, TestDefinition},
    debugger::{self, Debugger, DebuggerOutput},
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
                print!("test {} .. ", test_definition.name);

                let test_result = run_test(
                    debugger,
                    test_definition,
                    &test_cases.cargo_target_directory,
                    cargo_profile,
                    output_dir,
                )?;

                println!("{}", test_result.status.short_description());

                test_results.push(test_result);
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
    output_dir: &Path,
) -> anyhow::Result<TestResult> {
    if debugger.ignore_test(test_definition, cargo_profile) {
        return Ok(TestResult::new(
            test_definition,
            debugger,
            cargo_profile,
            Status::Ignored,
        ));
    }

    let debugger_script = generate_debugger_script(test_definition, debugger, cargo_profile);

    if debugger_script.is_empty() {
        return Ok(TestResult::new(
            test_definition,
            debugger,
            cargo_profile,
            Status::Ignored,
        ));
    }

    let debugger_script_path = save_debugger_script(
        debugger,
        test_definition,
        cargo_profile,
        debugger_script,
        output_dir,
    )?;

    let debuggee_path = cargo_target_directory
        .join(&cargo_profile[..])
        .join(&test_definition.executable_name);

    let debugger_output = debugger.run(&debugger_script_path, &debuggee_path)?;
    process_debugger_output(
        debugger,
        test_definition,
        cargo_profile,
        debugger_output,
        output_dir,
    )
}

fn save_debugger_script(
    debugger: &Debugger,
    test_definition: &TestDefinition,
    cargo_profile: &Arc<str>,
    script_contents: String,
    output_dir: &Path,
) -> anyhow::Result<PathBuf> {
    let file_name = format!("{}-{}.dbgscript", debugger.kind.name(), debugger.version);
    let path = output_dir_for_test(test_definition, cargo_profile, output_dir)?.join(file_name);
    std::fs::write(&path, script_contents)?;
    Ok(path)
}

fn generate_debugger_script(
    test_definition: &TestDefinition,
    debugger: &Debugger,
    cargo_profile: &Arc<str>,
) -> String {
    debugger::generate_debugger_script(debugger, test_definition, cargo_profile)
}

fn process_debugger_output(
    debugger: &Debugger,
    test_definition: &TestDefinition,
    cargo_profile: &Arc<str>,
    debugger_output: DebuggerOutput,
    output_dir: &Path,
) -> anyhow::Result<TestResult> {
    let output_dir = output_dir_for_test(test_definition, cargo_profile, output_dir)?;

    std::fs::write(
        output_dir.join(format!(
            "{}-{}.stdout",
            debugger.kind.name(),
            debugger.version
        )),
        &debugger_output.stdout,
    )?;
    std::fs::write(
        output_dir.join(format!(
            "{}-{}.stderr",
            debugger.kind.name(),
            debugger.version
        )),
        &debugger_output.stderr,
    )?;

    Ok(debugger::process_debugger_output(
        debugger,
        test_definition,
        debugger_output,
        cargo_profile,
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
