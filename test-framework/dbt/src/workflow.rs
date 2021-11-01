use std::{
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};

use anyhow::{bail, Context};

use crate::{
    cargo_test_directory::{CargoPackage, CargoWorkspace, TestDefinition},
    debugger::{self, Debugger, DebuggerOutput},
    test_result::{Status, TestResult},
};

pub struct CompiledTestCases {
    /// The target directory that Cargo used when compiling the workspace
    pub cargo_target_directory: PathBuf,

    /// The Cargo workspace containing all test projects
    pub cargo_workspace: Arc<CargoWorkspace>,

    /// The absolute path to the directory that is expected to contain the test executables,
    /// e.g. `cargo_target_directory.join("debug")`
    pub executable_directory: PathBuf,
}

pub fn compile_cargo_tests(
    cargo_test_directory: &Path,
    cargo_target_directory: &Path,
) -> anyhow::Result<CompiledTestCases> {
    let cargo_test_directory = Arc::new(CargoWorkspace::load(cargo_test_directory)?);
    let cargo_target_directory = cargo_target_directory.canonicalize()?;
    // TODO: "debug" is hardcoded
    let executable_directory = cargo_target_directory.join("debug");

    println!(
        "Compiling cargo test packages in {}",
        cargo_test_directory.root_path.display()
    );

    for test_project_def in cargo_test_directory.cargo_packages.iter() {
        println!(
            "{}:",
            CargoPackage::pretty_root_path(&test_project_def.root_path)
        );

        let mut cargo_command = Command::new("cargo");

        cargo_command.arg("build");

        cargo_command
            .arg("--target-dir")
            .arg(&cargo_target_directory);

        assert!(test_project_def.root_path.is_absolute());

        cargo_command.current_dir(&test_project_def.root_path);

        cargo_command.env("RUSTFLAGS", "-Ccodegen-units=1");
        cargo_command.env("RUSTFLAGS", "-Cdebuginfo=2");
        cargo_command.env("CARGO_INCREMENTAL", "0");

        let exit_status = cargo_command.status()?;

        if exit_status.success() {
            for test_def in &test_project_def.test_definitions {
                let expected_executable = executable_directory.join(&test_def.executable_name);
                assert!(expected_executable.exists());
            }
        }

        if !exit_status.success() {
            bail!("test case compilation failed");
        }
    }

    Ok(CompiledTestCases {
        cargo_target_directory,
        cargo_workspace: cargo_test_directory,
        executable_directory,
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

    println!();
    println!(
        "{} ({}) -- running {} tests",
        debugger.kind.name(),
        debugger.version,
        test_count
    );
    println!();

    for test_project_def in &test_cases.cargo_workspace.cargo_packages {
        for test_definition in &test_project_def.test_definitions {
            print!("test {} .. ", test_definition.name);

            let debugger_script = generate_debugger_script(test_definition, debugger);

            if debugger_script.is_empty() {
                test_results.push(TestResult::new(test_definition, debugger, Status::Ignored));
                continue;
            }

            let debugger_script_path =
                save_debugger_script(debugger, test_definition, debugger_script, output_dir)?;

            let debuggee_path = test_cases
                .cargo_target_directory
                .join("debug")
                .join(&test_definition.executable_name);

            let debugger_output = debugger.run(&debugger_script_path, &debuggee_path)?;
            let test_result =
                process_debugger_output(debugger, test_definition, debugger_output, output_dir)?;

            println!("{}", test_result.status.short_description());

            test_results.push(test_result);
        }
    }

    Ok(test_results)
}

fn save_debugger_script(
    debugger: &Debugger,
    test_definition: &TestDefinition,
    script_contents: String,
    output_dir: &Path,
) -> anyhow::Result<PathBuf> {
    let file_name = format!("{}-{}.dbgscript", debugger.kind.name(), debugger.version);
    let path = output_dir_for_test(test_definition, output_dir)?.join(file_name);
    std::fs::write(&path, script_contents)?;
    Ok(path)
}

fn generate_debugger_script(test_definition: &TestDefinition, debugger: &Debugger) -> String {
    debugger::generate_debugger_script(debugger, test_definition)
}

fn process_debugger_output(
    debugger: &Debugger,
    test_definition: &TestDefinition,
    debugger_output: DebuggerOutput,
    output_dir: &Path,
) -> anyhow::Result<TestResult> {
    let output_dir = output_dir_for_test(test_definition, output_dir)?;

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
    ))
}

fn output_dir_for_test(
    test_definition: &TestDefinition,
    output_dir: &Path,
) -> anyhow::Result<PathBuf> {
    let path = output_dir.join(test_definition.flat_name());
    std::fs::create_dir_all(&path).with_context(|| {
        format!(
            "while trying to create working directory for test: {}",
            path.display()
        )
    })?;
    Ok(path)
}
