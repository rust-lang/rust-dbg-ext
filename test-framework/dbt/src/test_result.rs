use std::{collections::BTreeMap, sync::Arc};

use anyhow::bail;

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
    pub status: Status,
}

impl TestResult {
    pub fn new(
        test_definition: &TestDefinition,
        debugger: &Debugger,
        status: Status,
    ) -> TestResult {
        TestResult {
            debugger_kind: debugger.kind,
            debugger_version: debugger.version.clone(),
            status,
            test_name: test_definition.name.clone(),
        }
    }
}

pub fn print_report(test_results: Vec<TestResult>) -> anyhow::Result<()> {
    let mut sorted: BTreeMap<DebuggerKind, BTreeMap<_, Vec<_>>> = BTreeMap::new();

    for test_result in test_results {
        let debugger_kind = test_result.debugger_kind;
        let debugger_version = test_result.debugger_version.clone();

        sorted
            .entry(debugger_kind)
            .or_default()
            .entry(debugger_version)
            .or_default()
            .push(test_result);
    }

    let mut failed = Vec::new();

    println!();
    println!("FINAL REPORT");

    for (debugger_kind, versions) in sorted {
        for (debugger_version, test_results) in versions {
            println!();
            println!("{} ({}):", debugger_kind, debugger_version);

            for test_result in test_results {
                println!(
                    " - {} ... {}",
                    test_result.test_name,
                    test_result.status.short_description()
                );

                if test_result.status != Status::Passed {
                    failed.push(test_result);
                }
            }

            println!()
        }
    }

    for failed_test in &failed {
        #[allow(clippy::single_match)]
        match &failed_test.status {
            Status::Failed(msg, _) => {
                println!("Test {} failed: {}", failed_test.test_name, msg);
            }
            _ => {}
        }
    }

    if failed.is_empty() {
        Ok(())
    } else {
        bail!("{} tests failed", failed.len());
    }
}
