use std::{
    collections::HashSet,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};

use anyhow::{bail, Context};
use log::debug;
use rayon::{prelude::*, ThreadPoolBuilder};
use regex::Regex;

use crate::{
    cargo_test_directory::{CargoWorkspace, TestDefinition},
    debugger::{self, Debugger, DebuggerOutput},
    import_export::GeneratedCrashDump,
    prettify_path,
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
    skip_rebuild: bool,
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

    if skip_rebuild {
        println!(
            "Skipping compilation of test cases in {} due to --no-rebuild flag.",
            prettify_path(&cargo_test_directory.root_path)
        )
    } else {
        for cargo_profile in cargo_profiles {
            let executable_directory = cargo_target_directory.join(cargo_profile);

            println!(
                "Compiling cargo test packages in {} for Cargo profile `{}`",
                prettify_path(&cargo_test_directory.root_path),
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

            cargo_command.env("CARGO_INCREMENTAL", "0");

            debug!("Cargo command: {:?}", cargo_command);

            let exit_status = cargo_command.status()?;

            if exit_status.success() {
                for test_project_def in &cargo_test_directory.cargo_packages {
                    for test_def in &test_project_def.test_definitions {
                        let expected_executable =
                            executable_directory.join(&test_def.executable_name);
                        if !expected_executable.exists() {
                            bail!(
                                "Expected test executable at {} but it does not exist.",
                                prettify_path(&expected_executable)
                            )
                        }
                    }
                }
            }

            if !exit_status.success() {
                bail!("test case compilation failed");
            }
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
    test_pattern: Option<&Regex>,
    test_threads: Option<usize>,
    verbose: bool,
) -> anyhow::Result<(Vec<TestResult>, Vec<GeneratedCrashDump>)> {
    assert_eq!(output_dir, output_dir.canonicalize()?);

    let mut test_results = vec![];

    let mut generated_crashdumps = vec![];

    for cargo_profile in &test_cases.cargo_profiles {
        let phases_evaluation_context =
            debugger.evaluation_context(cargo_profile, &PhaseConfig::Live);

        let test_count: usize = test_cases
            .cargo_workspace
            .cargo_packages
            .iter()
            .map(|p| {
                p.test_definitions
                    .iter()
                    .filter(|test_def| test_def.matches(test_pattern))
                    .map(|test_def| test_def.script.phases(&phases_evaluation_context).len())
                    .sum::<usize>()
            })
            .sum();

        println!();
        println!(
            "{} ({}) -- running {} tests for Cargo profile `{}`",
            debugger.kind.name(),
            debugger.version,
            test_count,
            cargo_profile,
        );
        println!();

        let tests_to_run = test_cases
            .cargo_workspace
            .cargo_packages
            .iter()
            .flat_map(|test_project_def| {
                test_project_def
                    .test_definitions
                    .iter()
                    .filter(|td| td.matches(test_pattern))
            })
            .collect::<Vec<_>>();

        let num_threads = if let Some(num_threads) = test_threads {
            num_threads.clamp(1, rayon::max_num_threads())
        } else {
            0
        };

        let thread_pool = ThreadPoolBuilder::new()
            .thread_name(|idx| format!("DBT test thread {}", idx))
            .num_threads(num_threads)
            .build()?;

        let (test_results0, generated_crashdumps0, mut errors) = thread_pool.install(move || {
            tests_to_run
                .par_iter()
                .map(|test_definition| {
                    run_all_test_phases(
                        debugger,
                        test_cases,
                        test_definition,
                        cargo_profile,
                        output_dir,
                        verbose,
                    )
                })
                .map(|result| match result {
                    Ok((test_results, generated_crash_dumps)) => {
                        (test_results, generated_crash_dumps, vec![])
                    }
                    Err(e) => (vec![], vec![], vec![e]),
                })
                .reduce(
                    || (vec![], vec![], vec![]),
                    |mut acc, results| {
                        acc.0.extend(results.0);
                        acc.1.extend(results.1);
                        acc.2.extend(results.2);
                        acc
                    },
                )
        });

        if !errors.is_empty() {
            return Err(errors.pop().unwrap());
        }

        test_results.extend(test_results0);
        generated_crashdumps.extend(generated_crashdumps0);
    }

    Ok((test_results, generated_crashdumps))
}

fn run_all_test_phases(
    debugger: &Debugger,
    test_cases: &CompiledTestCases,
    test_definition: &TestDefinition,
    cargo_profile: &Arc<str>,
    output_dir: &Path,
    verbose: bool,
) -> anyhow::Result<(Vec<TestResult>, Vec<GeneratedCrashDump>)> {
    let phases = test_definition
        .script
        .phases(&debugger.evaluation_context(cargo_profile, &PhaseConfig::Live));

    let output_dir_for_test = output_dir_for_test(test_definition, cargo_profile, output_dir)?;

    let mut test_results = vec![];
    let mut generated_crashdumps = vec![];

    for phase in &phases {
        let (test_result, crashdumps_generated_by_test) = run_test(
            debugger,
            test_definition,
            &test_cases.cargo_target_directory,
            cargo_profile,
            phase,
            &output_dir_for_test,
            verbose,
        )?;

        if *phase == PhaseConfig::Live && phases.len() == 1 {
            println!(
                "test {} .. {}",
                test_definition.name,
                test_result.status.short_description()
            );
        } else {
            println!(
                "test {} ({}) .. {}",
                test_definition.name,
                phase,
                test_result.status.short_description()
            );
        }

        test_results.push(test_result);
        generated_crashdumps.extend(crashdumps_generated_by_test);
    }

    Ok((test_results, generated_crashdumps))
}

fn run_test(
    debugger: &Debugger,
    test_definition: &TestDefinition,
    cargo_target_directory: &Path,
    cargo_profile: &Arc<str>,
    phase: &PhaseConfig,
    output_dir_for_test: &Path,
    verbose: bool,
) -> anyhow::Result<(TestResult, Vec<GeneratedCrashDump>)> {
    if debugger.ignore_test(test_definition, cargo_profile, phase) {
        return Ok((
            TestResult::new(
                test_definition,
                debugger,
                cargo_profile,
                phase,
                Status::Ignored,
            ),
            vec![],
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
        return Ok((
            TestResult::new(
                test_definition,
                debugger,
                cargo_profile,
                phase,
                Status::Ignored,
            ),
            vec![],
        ));
    }

    if !debugger.has_active_checks(test_definition, cargo_profile, phase) {
        return Ok((
            TestResult::new(
                test_definition,
                debugger,
                cargo_profile,
                phase,
                Status::Errored(
                    "The test script doesn't contain any active checks for the current configuration.".to_string()
                ),
            ),
            vec![],
        ));
    }

    let debugger_script_path =
        save_debugger_script(debugger, debugger_script, output_dir_for_test, phase)?;

    let (debugger_output, generated_crashdumps) = match phase {
        PhaseConfig::Live => {
            let debuggee_path =
                local_debuggee_path(cargo_target_directory, cargo_profile, test_definition);

            // Find the paths of all crashdump files this test is going to generate
            let generated_crashdump_paths: Vec<_> = debugger
                .active_crashdump_tags(test_definition, cargo_profile)
                .into_iter()
                .map(|tag| crashdump_path(output_dir_for_test, &tag))
                .collect();

            // Delete any crashdump files that may already exist
            for crashdump_path in &generated_crashdump_paths {
                if let Err(e) = std::fs::remove_file(crashdump_path) {
                    if e.kind() != std::io::ErrorKind::NotFound {
                        bail!(e);
                    }
                }
            }

            let debugger_output = debugger.run(&debugger_script_path, &debuggee_path, None)?;

            let mut generated_crashdumps = vec![];

            // Collect information about generated crashdumps so they can be exported.
            for crashdump_path in generated_crashdump_paths {
                if !crashdump_path.exists() {
                    bail!(
                        "Could not find expected crashdump file at: {}",
                        prettify_path(&crashdump_path)
                    );
                }

                let pdb_file = debuggee_path.with_extension("pdb");

                let extra_symbols = if pdb_file.exists() {
                    Some(pdb_file)
                } else {
                    None
                };

                generated_crashdumps.push(GeneratedCrashDump {
                    crashdump_path,
                    debuggee_path: debuggee_path.clone(),
                    extra_symbols,
                });
            }

            (debugger_output, generated_crashdumps)
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

            (
                debugger.run(&debugger_script_path, &executable, Some(&crashdump))?,
                vec![],
            )
        }
    };

    Ok((
        process_debugger_output(
            debugger,
            test_definition,
            cargo_profile,
            phase,
            debugger_output,
            output_dir_for_test,
            verbose,
        )?,
        generated_crashdumps,
    ))
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
        std::fs::create_dir_all(directory).with_context(|| {
            format!(
                "while trying to create crashdumps directory for test: {}",
                prettify_path(directory)
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
    verbose: bool,
) -> anyhow::Result<TestResult> {
    let stdout_path = output_dir_for_test.join(format!(
        "{}-{}-{}.stdout",
        debugger.kind.name(),
        debugger.version,
        phase,
    ));
    let stderr_path = stdout_path.with_extension("stderr");

    std::fs::write(&stdout_path, &debugger_output.stdout)?;
    std::fs::write(&stderr_path, &debugger_output.stderr)?;

    let mut test_result = debugger::process_debugger_output(
        debugger,
        test_definition,
        debugger_output,
        cargo_profile,
        phase,
    );

    test_result.debugger_output_stdout_path = Some(stdout_path);
    test_result.debugger_output_stderr_path = Some(stderr_path);

    if verbose {
        if let Status::Failed(_, ref debugger_output) = *test_result.status {
            println!("debugger stdout:\n{}\n\n", &debugger_output.stdout);
            println!("debugger stderr:\n{}\n\n", &debugger_output.stderr);
        }
    }

    Ok(test_result)
}

fn output_dir_for_test(
    test_definition: &TestDefinition,
    cargo_profile: &Arc<str>,
    output_dir: &Path,
) -> anyhow::Result<PathBuf> {
    let mut dir_name = test_definition.flat_name();
    dir_name.push('@');
    dir_name.push_str(cargo_profile);

    let path = output_dir.join(dir_name);
    std::fs::create_dir_all(&path).with_context(|| {
        format!(
            "while trying to create working directory for test: {}",
            prettify_path(&path)
        )
    })?;

    Ok(path)
}
