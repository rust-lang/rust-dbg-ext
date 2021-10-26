use dbt::workflow;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "example", about = "An example of StructOpt usage.")]
struct Opt {
    #[structopt(long = "--cargo-workspace", parse(from_os_str))]
    cargo_workspace: Vec<PathBuf>,

    #[structopt(
        long = "--cargo-target-directory",
        parse(from_os_str),
        default_value = "target",
        help = "the target directory to be used by Cargo when compiling test cases"
    )]
    cargo_target_directory: PathBuf,

    #[structopt(short = "-d", long = "--debugger", parse(from_os_str))]
    debuggers: Vec<PathBuf>,

    #[structopt(
        default_value = "output",
        parse(from_os_str),
        help = "the directory test results and debugger output will be written to"
    )]
    output_dir: PathBuf,
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let opt = Opt::from_args();

    let debuggers = dbt::debugger::init_debuggers(&opt.debuggers)?;

    let mut compiled_test_cases = Vec::new();

    for cargo_test_directory in opt.cargo_workspace {
        compiled_test_cases.push(workflow::compile_cargo_tests(
            &cargo_test_directory,
            &opt.cargo_target_directory,
        )?);
    }

    let mut test_results = Vec::new();

    for debugger in &debuggers {
        for compiled_test_cases in &compiled_test_cases {
            test_results.extend_from_slice(&workflow::run_cargo_tests(
                compiled_test_cases,
                debugger,
                &opt.output_dir,
            )?);
        }
    }

    dbt::test_result::print_report(test_results);

    Ok(())
}
