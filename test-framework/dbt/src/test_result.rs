use std::sync::Arc;

use crate::{
    cargo_test_directory::TestDefinition,
    debugger::{Debugger, DebuggerKind, DebuggerOutput},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Status {
    Passed,
    Failed(String, DebuggerOutput),
    Errored,
    Ignored,
}

impl Status {
    pub fn short_description(&self) -> &'static str {
        match *self {
            Status::Passed => "OK",
            Status::Failed(..) => "failed",
            Status::Errored => "ERROR",
            Status::Ignored => "ignored",
        }
    }
}

#[derive(Debug, Clone)]
pub struct TestResult {
    pub test_name: Arc<str>,
    pub debugger_kind: DebuggerKind,
    pub debugger_version: Arc<str>,
    pub cargo_profile: Arc<str>,
    pub status: Status,
}

impl TestResult {
    pub fn new(
        test_definition: &TestDefinition,
        debugger: &Debugger,
        cargo_profile: &Arc<str>,
        status: Status,
    ) -> TestResult {
        TestResult {
            debugger_kind: debugger.kind,
            debugger_version: debugger.version.clone(),
            cargo_profile: cargo_profile.clone(),
            status,
            test_name: test_definition.name.clone(),
        }
    }
}

pub fn print_report(test_results: Vec<TestResult>) -> bool {
    let mut errored = 0;
    let mut ignored = 0;
    let mut failed = 0;
    let mut passed = 0;

    println!();

    for test_result in test_results {
        match test_result.status {
            Status::Ignored => ignored += 1,
            Status::Errored => errored += 1,
            Status::Passed => passed += 1,
            Status::Failed(msg, _) => {
                failed += 1;
                println!("Test {} failed:\n{}", test_result.test_name, msg);
            }
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
