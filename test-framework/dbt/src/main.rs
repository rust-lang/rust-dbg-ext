use dbt::{import_export, workflow};
use regex::Regex;
use std::{ffi::OsString, path::PathBuf};

use clap::Parser;

#[derive(Debug, Parser)]
#[command(name = "DBT", about = "A tool for testing debugger extensions.")]
struct Opt {
    #[arg(long = "cargo-workspace")]
    cargo_workspace: Vec<PathBuf>,

    #[arg(
        long = "cargo-target-directory",
        default_value = "target",
        help = "the target directory to be used by Cargo when compiling test cases"
    )]
    cargo_target_directory: PathBuf,

    #[arg(short = 'd', long = "debugger")]
    debuggers: Vec<PathBuf>,

    #[arg(
        short = 'p',
        long = "debugger-prelude",
        help = "a string of the form <debugger-kind>:<debugger command to \
                execute a beginning of each test script>"
    )]
    debugger_prelude: Vec<OsString>,

    #[arg(
        long = "debugger-arg",
        help = "commandline argument to be passed to the debugger"
    )]
    debugger_commandline_args: Vec<OsString>,

    #[arg(
        long = "debugger-env",
        help = "a string of the form <debugger-kind>:<env-var-name>=<env-var-value>"
    )]
    debugger_env: Vec<OsString>,

    #[arg(
        short = 'o',
        long = "output",
        default_value = "output",
        help = "the directory test results and debugger output will be written to"
    )]
    output_dir: PathBuf,

    #[arg(
        long = "cargo-profile",
        help = "the Cargo profile(s) to be used for compiling test cases"
    )]
    cargo_profiles: Vec<String>,

    #[arg(long)]
    verbose: bool,

    #[arg(
        long = "export-crashdumps",
        help = "export generated crashdumps to `<output>/exported_crashdumps.tar.gz`"
    )]
    export_crashdumps: bool,

    #[arg(
        long = "import-crashdumps",
        help = "import a set of crashdumps generated via `--export-crashdumps` before running tests"
    )]
    import_crashdumps: Option<PathBuf>,

    #[arg(
        short = 'D',
        long = "define",
        help = "define a value `xyz` that will be available as `@xyz` in test scripts"
    )]
    defines: Vec<String>,

    #[arg(
        short = 't',
        long = "test-pattern",
        help = "only run tests that match the given pattern"
    )]
    test_pattern: Option<String>,

    #[arg(
        short = 'j',
        long = "test-threads",
        help = "the max number tests that can run in parallel"
    )]
    test_threads: Option<usize>,
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let opt = Opt::parse();

    let test_pattern = opt.test_pattern.as_ref().map(|s| {
        Regex::new(s).unwrap_or_else(|e| {
            eprintln!(
                "--test-pattern `{}` is not a valid regular expression: {}",
                s, e
            );
            std::process::exit(1);
        })
    });

    let output_dir = opt.output_dir.canonicalize()?;

    if let Some(import_crashdumps) = &opt.import_crashdumps {
        import_export::import_crashdumps(&output_dir, import_crashdumps)?;
    }

    let debuggers = dbt::debugger::init_debuggers(
        &opt.debuggers,
        &opt.debugger_prelude,
        &opt.debugger_commandline_args,
        &opt.debugger_env,
        &opt.defines,
    )?;

    let mut compiled_test_cases = Vec::new();

    for cargo_test_directory in opt.cargo_workspace {
        compiled_test_cases.push(workflow::compile_cargo_tests(
            &cargo_test_directory,
            &opt.cargo_target_directory,
            &opt.cargo_profiles,
        )?);
    }

    let mut test_results = Vec::new();
    let mut crashdump_exporter = if opt.export_crashdumps {
        Some(import_export::CrashDumpExporter::new(
            output_dir.clone(),
            &output_dir.join("exported_crashdumps.tar.gz"),
        )?)
    } else {
        None
    };

    for debugger in &debuggers {
        for compiled_test_cases in &compiled_test_cases {
            let (results, generated_crashdumps) = workflow::run_cargo_tests(
                compiled_test_cases,
                debugger,
                &output_dir,
                test_pattern.as_ref(),
                opt.test_threads,
                opt.verbose,
            )?;

            if let Some(ref mut crashdump_exporter) = crashdump_exporter {
                for crashdump in generated_crashdumps {
                    crashdump_exporter.add_crashdump(crashdump)?;
                }
            }

            test_results.extend_from_slice(&results);
        }
    }

    drop(crashdump_exporter);

    if !dbt::test_result::print_report(test_results) {
        std::process::exit(1);
    }
    Ok(())
}
