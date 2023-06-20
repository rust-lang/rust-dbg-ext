use std::{path::PathBuf, sync::Arc};

use crate::{
    cargo_test_directory::TestDefinition,
    debugger::{Debugger, DebuggerKind, DebuggerOutput},
    prettify_path,
    script::PhaseConfig,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Status {
    Passed,
    Failed(String, DebuggerOutput),
    Errored(String),
    Ignored,
}

impl Status {
    pub fn short_description(&self) -> &'static str {
        match *self {
            Status::Passed => "OK",
            Status::Failed(..) => "failed",
            Status::Errored(..) => "ERROR",
            Status::Ignored => "ignored",
        }
    }
}

#[derive(Debug, Clone)]
pub struct TestResult {
    pub test_name: Arc<str>,
    pub phase: PhaseConfig,
    pub debugger_kind: DebuggerKind,
    pub debugger_version: Arc<str>,
    pub cargo_profile: Arc<str>,
    pub status: Box<Status>,
    pub debugger_output_stdout_path: Option<PathBuf>,
    pub debugger_output_stderr_path: Option<PathBuf>,
}

impl TestResult {
    pub fn new(
        test_definition: &TestDefinition,
        debugger: &Debugger,
        cargo_profile: &Arc<str>,
        phase: &PhaseConfig,
        status: Status,
    ) -> TestResult {
        TestResult {
            debugger_kind: debugger.kind,
            debugger_version: debugger.version.clone(),
            cargo_profile: cargo_profile.clone(),
            status: Box::new(status),
            test_name: test_definition.name.clone(),
            phase: phase.clone(),
            debugger_output_stdout_path: None,
            debugger_output_stderr_path: None,
        }
    }

    fn test_label(&self) -> String {
        format!(
            "{} ({}) - Cargo profile `{}`",
            self.test_name, self.phase, self.cargo_profile
        )
    }
}

pub fn print_report(test_results: Vec<TestResult>) -> bool {
    let mut errored = 0;
    let mut ignored = 0;
    let mut failed = 0;
    let mut passed = 0;

    println!();

    for test_result in test_results {
        let print_output_paths = match &*test_result.status {
            Status::Ignored => {
                ignored += 1;
                false
            }
            Status::Errored(msg) => {
                errored += 1;
                print!("Test {} errored:\n{}", test_result.test_label(), msg);
                true
            }
            Status::Passed => {
                passed += 1;
                false
            }
            Status::Failed(msg, _) => {
                failed += 1;
                print!("Test {} failed:\n{}", test_result.test_label(), msg);
                true
            }
        };

        if print_output_paths {
            if let (Some(stdout_path), Some(stderr_path)) = (
                &test_result.debugger_output_stdout_path,
                &test_result.debugger_output_stderr_path,
            ) {
                println!("StdOut: {}", prettify_path(stdout_path));
                println!("StdErr: {}", prettify_path(stderr_path));
            }
            println!("Test name: {}", test_result.test_label());
            println!();
        }
    }

    println!(
        "{} passed, {} failed, {} errored, {} ignored",
        passed, failed, errored, ignored
    );

    println!();

    if failed + errored == 0 {
        true
    } else {
        println!("Some tests were not successful");
        false
    }
}
