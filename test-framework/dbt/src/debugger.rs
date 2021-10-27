use std::collections::{BTreeMap, HashSet};
use std::fmt::Write;
use std::sync::Arc;
use std::{
    collections::HashMap,
    ffi::{OsStr, OsString},
    fmt::{Debug, Display},
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::bail;
use log::{info, warn};
use regex::Regex;
use structopt::lazy_static::lazy_static;

use crate::script::{Script, Statement};
use crate::test_result::Status;
use crate::{
    cargo_test_directory::TestDefinition,
    script::{self, CorrelationId, EvaluationContext},
    test_result::TestResult,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum DebuggerKind {
    Gdb,
    Cdb,
    Lldb,
    Mock,
}

impl Display for DebuggerKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl DebuggerKind {
    pub fn name(self) -> &'static str {
        match self {
            DebuggerKind::Gdb => "gdb",
            DebuggerKind::Cdb => "cdb",
            DebuggerKind::Lldb => "lldb",
            DebuggerKind::Mock => "mock",
        }
    }

    pub fn run(
        self,
        debugger_executable: &OsStr,
        script_file_path: &Path,
        debugee: &Path,
    ) -> anyhow::Result<DebuggerOutput> {
        let mut command = Command::new(debugger_executable);

        match self {
            DebuggerKind::Mock => {
                return Ok(create_mock_debugger_output(script_file_path));
            }
            DebuggerKind::Gdb => {
                command
                    .arg("--batch")
                    .arg("--quiet")
                    .arg("--command")
                    .arg(script_file_path);
            }
            DebuggerKind::Cdb => {
                command.arg("-cf").arg(script_file_path);
            }
            DebuggerKind::Lldb => {
                todo!()
            }
        }

        command.arg(debugee);

        let output = command.output()?;

        Ok(DebuggerOutput {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            exit_status: if output.status.success() {
                DebuggerExitStatus::Success
            } else {
                DebuggerExitStatus::Failure
            },
        })
    }
}

fn create_mock_debugger_output(script_file_path: &Path) -> DebuggerOutput {
    DebuggerOutput {
        stdout: std::fs::read_to_string(script_file_path).unwrap(),
        stderr: "".to_string(),
        exit_status: DebuggerExitStatus::Success,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DebuggerExitStatus {
    Success,
    Failure,
}

impl DebuggerExitStatus {
    pub fn success(self) -> bool {
        match self {
            DebuggerExitStatus::Success => true,
            DebuggerExitStatus::Failure => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DebuggerOutput {
    pub stdout: String,
    pub stderr: String,
    pub exit_status: DebuggerExitStatus,
}

pub struct Debugger {
    pub kind: DebuggerKind,
    pub version: Arc<str>,
    pub command: OsString,
    evaluation_context: EvaluationContext,
}

impl Debug for Debugger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} - {} ({})",
            self.kind,
            self.version,
            self.command.to_string_lossy()
        )
    }
}

impl Debugger {
    // TODO: Add environment for evaluation context (e.g. `target`)
    pub fn new(kind: DebuggerKind, version: &str, command: OsString) -> Debugger {
        let mut evaluation_context = HashMap::new();
        evaluation_context.insert("debugger".into(), kind.name().into());
        evaluation_context.insert("version".into(), version.into());

        let evaluation_context = EvaluationContext {
            values: evaluation_context,
        };

        Debugger {
            kind,
            version: version.into(),
            command,
            evaluation_context,
        }
    }

    pub fn run(&self, script_file_path: &Path, debuggee: &Path) -> anyhow::Result<DebuggerOutput> {
        self.kind.run(&self.command, script_file_path, debuggee)
    }

    pub fn mock() -> Debugger {
        Debugger::new(DebuggerKind::Mock, "1.0", "mockdbg".into())
    }

    /// Tries to create a Debugger object from its commandline command. Will invoke the command
    /// to get a version string.
    fn infer_from_command(command: &Path) -> anyhow::Result<Debugger> {
        if command.to_string_lossy() == "mockdbg" {
            return Ok(Debugger::mock());
        }

        if let Some(file_name) = command.file_name() {
            let file_name = file_name.to_string_lossy().to_lowercase();

            let version_arg = if file_name.ends_with("cdb.exe") || file_name.ends_with("cdb") {
                // This is probably CDB
                "-version"
            } else {
                "--version"
            };

            let output = Command::new(command).arg(version_arg).output()?;

            if !output.status.success() {
                bail!("failed to get debugger version from {}", command.display());
            }

            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);

            let output_lines = stdout.lines().chain(stderr.lines());

            for line in output_lines {
                if let Some(version) = extract_gdb_version(line) {
                    return Ok(Debugger::new(DebuggerKind::Gdb, version, command.into()));
                }

                if let Some(version) = extract_cdb_version(line) {
                    return Ok(Debugger::new(DebuggerKind::Cdb, version, command.into()));
                }

                // TODO: lldb
            }
        }

        bail!(
            "Could not infer debugger from command `{}`",
            command.display()
        )
    }

    pub fn maybe_emit_correlation_id_command(
        &self,
        begin: bool,
        correlation_id: Option<CorrelationId>,
        output: &mut String,
    ) {
        if let Some(correlation_id) = correlation_id {
            let marker = if begin {
                CORRELATION_ID_BEGIN_MARKER
            } else {
                CORRELATION_ID_END_MARKER
            };

            match self.kind {
                DebuggerKind::Cdb => {
                    writeln!(output, ".echo {}{}", marker, correlation_id.0).unwrap();
                }
                DebuggerKind::Gdb => {
                    writeln!(output, "python print('{}{}')", marker, correlation_id.0).unwrap();
                }
                DebuggerKind::Mock => {
                    writeln!(output, "{}{}", marker, correlation_id.0).unwrap();
                }
                DebuggerKind::Lldb => {
                    writeln!(output, "script print('{}{}')", marker, correlation_id.0).unwrap();
                }
            }
        }
    }

    fn assign_correlation_ids(&self, script: &mut Script) {
        let mut correlation_ids_checked = HashSet::new();
        let mut next_correlation_id = 0;
        let mut last_correlation_id_emitted = None;

        // Assign correlation ids
        script.walk_applicable_leaves_mut(&self.evaluation_context, &mut |statement| {
            match statement {
                Statement::Exec(_, correlation_id_slot) => {
                    debug_assert_eq!(correlation_id_slot, &None);

                    let last_id_has_been_checked = last_correlation_id_emitted
                        .map_or(true, |id| correlation_ids_checked.contains(&id));

                    let correlation_id = if last_id_has_been_checked {
                        // Allocate a new one
                        let correlation_id = CorrelationId(next_correlation_id);
                        next_correlation_id += 1;
                        correlation_id
                    } else {
                        // Re-use the existing one
                        last_correlation_id_emitted.unwrap()
                    };

                    *correlation_id_slot = Some(correlation_id);
                    last_correlation_id_emitted = Some(correlation_id);
                }
                Statement::Check(_, correlation_id_slot)
                | Statement::CheckUnorderedBlock(_, correlation_id_slot) => {
                    if let Some(last_correlation_id_emitted) = last_correlation_id_emitted {
                        debug_assert_eq!(last_correlation_id_emitted.0, next_correlation_id - 1);
                        *correlation_id_slot = Some(last_correlation_id_emitted);
                        assert!(correlation_ids_checked.insert(last_correlation_id_emitted));
                    } else {
                        warn!("{:?} has no output to check", statement);
                    }
                }
                Statement::IfBlock(..) | Statement::IgnoreTest => {
                    // Nothing to do
                }
            }
            true
        });
    }

    /// Write a debugger (and version) specific prelude to the debugger script
    /// Will be invoked before anything is written to the script.
    fn emit_script_prelude(&self, script: &mut String) {
        match self.kind {
            DebuggerKind::Cdb => {
                writeln!(script, ".lines -e").unwrap();
            }
            DebuggerKind::Gdb => {
                todo!()
            }
            DebuggerKind::Lldb => {
                todo!()
            }
            DebuggerKind::Mock => {
                // no prelude
            }
        }
    }

    /// Emit commands for setting breakpoints that have been specified via #break directives
    fn emit_breakpoints(&self, test_definition: &TestDefinition, script: &mut String) {
        for bp in &test_definition.breakpoints {
            match self.kind {
                DebuggerKind::Cdb | DebuggerKind::Mock => {
                    writeln!(
                        script,
                        "bp `{}:{}`",
                        test_definition.name.rsplit_once('/').unwrap().1,
                        bp.line_index + 1
                    )
                    .unwrap();
                }
                _ => todo!(),
            }
        }
    }
}

const CORRELATION_ID_BEGIN_MARKER: &str = "__output_with_correlation_id_begin__=";
const CORRELATION_ID_END_MARKER: &str = "__output_with_correlation_id_end__=";

fn extract_correlation_id(line: &str) -> CorrelationId {
    debug_assert!(
        line.contains(CORRELATION_ID_BEGIN_MARKER) || line.contains(CORRELATION_ID_BEGIN_MARKER)
    );
    let id_start = line.rfind('=').unwrap() + 1;
    let id_str = &line[id_start..];
    let id: u32 = id_str.parse().unwrap();
    CorrelationId(id)
}

/// Generates the debugger script for the given combination of test definition and debugger.
pub fn generate_debugger_script(debugger: &Debugger, test_definition: &TestDefinition) -> String {
    let mut debugger_script = String::new();

    debugger.emit_script_prelude(&mut debugger_script);
    debugger.emit_breakpoints(test_definition, &mut debugger_script);

    let mut script = test_definition.script.clone();
    debugger.assign_correlation_ids(&mut script);

    // Emit commands
    let mut last_correlation_id = None;
    script.walk_applicable_leaves(&debugger.evaluation_context, &mut |statement| {
        if let script::Statement::Exec(command, correlation_id) = statement {
            if last_correlation_id != *correlation_id {
                debugger.maybe_emit_correlation_id_command(
                    false,
                    last_correlation_id,
                    &mut debugger_script,
                );
                debugger.maybe_emit_correlation_id_command(
                    true,
                    *correlation_id,
                    &mut debugger_script,
                );
                last_correlation_id = *correlation_id;
            }

            writeln!(&mut debugger_script, "{}", command).unwrap();
        }
        true
    });

    debugger.maybe_emit_correlation_id_command(false, last_correlation_id, &mut debugger_script);

    debugger_script
}

pub fn process_debugger_output(
    debugger: &Debugger,
    test_definition: &TestDefinition,
    debugger_output: DebuggerOutput,
) -> TestResult {
    let mut script = test_definition.script.clone();
    debugger.assign_correlation_ids(&mut script);

    let mut checks_by_correlation_id: BTreeMap<CorrelationId, Vec<Statement>> = BTreeMap::new();

    script.walk_applicable_leaves(&debugger.evaluation_context, &mut |statement| {
        match statement {
            Statement::Check(_, cid) | Statement::CheckUnorderedBlock(_, cid) => {
                checks_by_correlation_id
                    .entry(cid.unwrap())
                    .or_default()
                    .push(statement.clone());
            }
            _ => {
                // Only collecting check statements
            }
        }

        true
    });

    let debugger_output_by_correlation_id =
        match debugger_output_by_correlation_id(&debugger_output, test_definition, debugger) {
            Ok(x) => x,
            Err(test_result) => {
                return test_result;
            }
        };

    for (cid, checks) in checks_by_correlation_id {
        let output = if let Some(output) = debugger_output_by_correlation_id.get(&cid) {
            output
        } else {
            return TestResult::new(
                test_definition,
                debugger,
                Status::Failed(format!("check {:?} failed", &checks[0]), debugger_output),
            );
        };

        let mut check_index = 0;
        for output_line in output {
            match &checks[check_index] {
                Statement::Check(check, _cid) => {
                    debug_assert_eq!(_cid, &Some(cid));

                    if check.check(output_line) {
                        // we have a match
                        check_index += 1;

                        if check_index == checks.len() {
                            // We have found all the output we wanted
                            break;
                        }
                    }
                }
                Statement::CheckUnorderedBlock(..) => {
                    todo!()
                }
                _ => {
                    // only interested in check statements
                }
            }
        }

        if check_index != checks.len() {
            let status = Status::Failed(
                format!(
                    "Could not find line '{:?}' in debugger output",
                    checks[check_index]
                ),
                debugger_output,
            );

            return TestResult::new(test_definition, debugger, status);
        }
    }

    TestResult::new(test_definition, debugger, Status::Passed)
}

/// Splits debugger output into sections that correspond to a single correlation ID.
fn debugger_output_by_correlation_id<'a>(
    debugger_output: &'a DebuggerOutput,
    test_definition: &TestDefinition,
    debugger: &Debugger,
) -> Result<BTreeMap<CorrelationId, Vec<&'a str>>, TestResult> {
    let mut result = BTreeMap::new();

    let mut current_id = None;
    let mut current_lines = vec![];
    for line in debugger_output.stdout.lines() {
        if line.starts_with(CORRELATION_ID_BEGIN_MARKER) {
            if current_id.is_some() {
                // malformed debugger output
                return Err(TestResult::new(test_definition, debugger, Status::Errored));
            }
            assert!(current_lines.is_empty());
            current_id = Some(extract_correlation_id(line));
        } else if line.starts_with(CORRELATION_ID_END_MARKER) {
            if let Some(current_id) = current_id {
                result.insert(current_id, current_lines);
                current_lines = vec![];
            } else {
                // malformed debugger output
                return Err(TestResult::new(test_definition, debugger, Status::Errored));
            }

            current_id = None;
        } else if current_id.is_some() {
            current_lines.push(line);
        }
    }

    Ok(result)
}

/// Takes a set of debugger commandline commands and tries to create a [Debugger] object for each.
pub fn init_debuggers(commands: &[PathBuf]) -> anyhow::Result<Vec<Debugger>> {
    info!("Setting up debuggers");
    let mut debuggers = vec![];

    for command in commands {
        info!("Trying to set up debugger {}", command.display());
        let debugger = Debugger::infer_from_command(command)?;
        info!("Successfully set up debugger: {:?}", debugger);
        debuggers.push(debugger);
    }

    Ok(debuggers)
}

fn extract_gdb_version(version_output: &str) -> Option<&str> {
    lazy_static! {
        // GNU gdb (Ubuntu 9.2-0ubuntu1~20.04) 9.2
        // GNU gdb (GDB) 10.2
        // GNU gdb (GDB) 8.2.1
        static ref GDB_REGEX: Regex = Regex::new(r"GNU\s+gdb\s+\(.+\)\s+(.+)").unwrap();
    }

    GDB_REGEX
        .captures(version_output)
        .map(|captures| captures.get(1).unwrap().as_str())
}

fn extract_cdb_version(version_output: &str) -> Option<&str> {
    lazy_static! {
        // example: cdb version 10.0.21349.1004
        static ref CDB_REGEX: Regex = Regex::new(r"cdb\s+version\s+([\d\.]+)").unwrap();
    }

    CDB_REGEX
        .captures(version_output)
        .map(|captures| captures.get(1).unwrap().as_str())
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{cargo_test_directory::TestDefinition, debugger::Debugger, script::parse_script};

    fn from_lines(lines: &[&str]) -> String {
        let mut output = String::new();

        for line in lines {
            output.push_str(line);
            output.push('\n');
        }
        output
    }

    fn mock_test_def(script: String) -> TestDefinition {
        let script = parse_script(&script).unwrap();
        TestDefinition::new(
            Path::new("src/main.rs"),
            "project",
            "main".into(),
            script,
            vec![],
        )
    }

    #[test]
    fn generate_debugger_script() {
        let test_def = mock_test_def(from_lines(&[
            "/***",
            "#if mock",
            "  print abc",
            "  #check __abc__",
            "  print xyz",
            "  #check __xyz__",
            "***/",
        ]));

        let script = super::generate_debugger_script(&Debugger::mock(), &test_def);

        assert_eq!(
            script,
            from_lines(&[
                "__output_with_correlation_id_begin__=0",
                "print abc",
                "__output_with_correlation_id_end__=0",
                "__output_with_correlation_id_begin__=1",
                "print xyz",
                "__output_with_correlation_id_end__=1",
            ])
        );
    }

    // TODO: unit test for sequence points

    #[test]
    fn gdb_version_extraction() {
        assert_eq!(
            super::extract_gdb_version("GNU gdb (GDB) 8.2.1"),
            Some("8.2.1")
        );
        assert_eq!(
            super::extract_gdb_version("GNU gdb (Ubuntu 9.2-0ubuntu1~20.04) 9.2"),
            Some("9.2")
        );
        assert_eq!(
            super::extract_gdb_version("cdb version 10.0.21349.1004"),
            None
        );
    }

    #[test]
    fn cdb_version_extraction() {
        assert_eq!(
            super::extract_cdb_version("cdb version 10.0.21349.1004"),
            Some("10.0.21349.1004")
        );
        assert_eq!(super::extract_cdb_version("GNU gdb (GDB) 8.2.1"), None);
        assert_eq!(
            super::extract_cdb_version("GNU gdb (Ubuntu 9.2-0ubuntu1~20.04) 9.2"),
            None
        );
    }
}
