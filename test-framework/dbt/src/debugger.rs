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

use anyhow::{bail, Context};
use log::{info, warn};
use regex::Regex;
use structopt::lazy_static::lazy_static;

use crate::script::{PhaseConfig, Script, Statement, Value};
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

impl TryFrom<&str> for DebuggerKind {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value.trim() {
            "gdb" => Ok(DebuggerKind::Gdb),
            "cdb" => Ok(DebuggerKind::Cdb),
            "lldb" => Ok(DebuggerKind::Lldb),
            "mock" => Ok(DebuggerKind::Mock),
            value => bail!("Unknown DebuggerKind `{}`", value),
        }
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
        debuggee: &Path,
        crashdump: Option<&Path>,
        command_line_args: &[String],
        env_vars: &[(String, String)],
        // TODO: add source path
    ) -> anyhow::Result<DebuggerOutput> {
        let mut command = Command::new(debugger_executable);

        command.envs(env_vars.iter().map(|(env_var_name, env_var_value)| {
            (OsString::from(env_var_name), OsString::from(env_var_value))
        }));

        command.args(command_line_args.iter().map(OsString::from));

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

                if let Some(crashdump) = crashdump {
                    command.arg("--core").arg(crashdump);
                }

                command.arg(debuggee);
            }
            DebuggerKind::Cdb => {
                command.arg("-cf").arg(script_file_path);

                if let Some(crashdump) = crashdump {
                    command.arg("-z").arg(crashdump);
                    // Add the directory of the debuggee to the symbol search path
                    // where we expect find the accompanying PDB.
                    command.arg("-y").arg(debuggee.parent().unwrap());
                } else {
                    command.arg(debuggee);
                }
            }
            DebuggerKind::Lldb => {
                command.arg("--batch");
                command.arg("--source").arg(script_file_path);
                command.arg(debuggee);
            }
        }

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
    prelude: Vec<String>,
    commandline_args: Vec<String>,
    env_vars: Vec<(String, String)>,
    defines: Arc<[Arc<str>]>,
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
    pub fn new(
        kind: DebuggerKind,
        version: Arc<str>,
        command: OsString,
        prelude: Vec<String>,
        commandline_args: Vec<String>,
        env_vars: Vec<(String, String)>,
        defines: Arc<[Arc<str>]>,
    ) -> Debugger {
        Debugger {
            kind,
            version,
            command,
            prelude,
            commandline_args,
            env_vars,
            defines,
        }
    }

    pub fn run(
        &self,
        script_file_path: &Path,
        debuggee: &Path,
        crashdump: Option<&Path>,
    ) -> anyhow::Result<DebuggerOutput> {
        if let Some(crashdump) = crashdump {
            if !crashdump.exists() {
                bail!("Could not find crashdump file: {}", crashdump.display());
            }
        }
        self.kind.run(
            &self.command,
            script_file_path,
            debuggee,
            crashdump,
            &self.commandline_args,
            &self.env_vars,
        )
    }

    pub fn mock() -> Debugger {
        Debugger::new(
            DebuggerKind::Mock,
            "1.0".into(),
            "mockdbg".into(),
            vec![],
            vec![],
            vec![],
            vec![].into(),
        )
    }

    pub fn ignore_test(
        &self,
        test_definition: &TestDefinition,
        cargo_profile: &Arc<str>,
        phase: &PhaseConfig,
    ) -> bool {
        let evaluation_context = self.evaluation_context(cargo_profile, phase);
        test_definition.script.ignore_test(&evaluation_context)
    }

    pub fn has_active_checks(
        &self,
        test_definition: &TestDefinition,
        cargo_profile: &Arc<str>,
        phase: &PhaseConfig,
    ) -> bool {
        let evaluation_context = self.evaluation_context(cargo_profile, phase);
        test_definition
            .script
            .has_active_checks(&evaluation_context)
    }

    pub fn active_crashdump_tags(
        &self,
        test_definition: &TestDefinition,
        cargo_profile: &Arc<str>,
    ) -> Vec<Arc<str>> {
        let evaluation_context = self.evaluation_context(cargo_profile, &PhaseConfig::Live);
        test_definition
            .script
            .active_crashdump_tags(&evaluation_context)
    }

    /// Tries to create a Debugger object from its commandline command. Will invoke the command
    /// to get a version string.
    fn infer_from_command(command: &Path) -> anyhow::Result<(DebuggerKind, Arc<str>)> {
        if command.to_string_lossy() == "mockdbg" {
            return Ok((DebuggerKind::Mock, "1.0".into()));
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
                    return Ok((DebuggerKind::Gdb, version.into()));
                }

                if let Some(version) = extract_cdb_version(line) {
                    return Ok((DebuggerKind::Cdb, version.into()));
                }

                if let Some(version) = extract_lldb_version(line) {
                    return Ok((DebuggerKind::Lldb, version.into()));
                }
            }
        }

        bail!(
            "Could not infer debugger from command `{}`",
            command.display()
        )
    }

    fn emit_crashdump_command(&self, path: &Path, output: &mut String) {
        match self.kind {
            DebuggerKind::Cdb => {
                writeln!(output, ".dump /ma {}", path.display()).unwrap();
            }
            DebuggerKind::Gdb => {
                writeln!(output, "generate-core-file {}", path.display()).unwrap();
            }
            DebuggerKind::Mock => {
                writeln!(output, "generate_crashdump {}", path.display()).unwrap();
            }
            DebuggerKind::Lldb => {
                todo!()
            }
        }
    }

    fn maybe_emit_correlation_id_command(
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

    fn assign_correlation_ids(
        &self,
        script: &mut Script,
        cargo_profile: &Arc<str>,
        phase: &PhaseConfig,
    ) {
        let mut correlation_ids_checked = HashSet::new();
        let mut next_correlation_id = 0;
        let mut last_correlation_id_emitted = None;

        let evaluation_context = self.evaluation_context(cargo_profile, phase);

        // Assign correlation ids
        script.walk_applicable_leaves_mut(&evaluation_context, &mut |statement| {
            match statement {
                Statement::Exec(_, correlation_id_slot, _) => {
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
                Statement::Check(_, correlation_id_slot, _)
                | Statement::CheckUnorderedBlock(_, correlation_id_slot, _) => {
                    if let Some(last_correlation_id_emitted) = last_correlation_id_emitted {
                        debug_assert_eq!(last_correlation_id_emitted.0, next_correlation_id - 1);
                        *correlation_id_slot = Some(last_correlation_id_emitted);
                        correlation_ids_checked.insert(last_correlation_id_emitted);
                    } else {
                        warn!("{:?} has no output to check", statement);
                    }
                }
                // Add any statements here that unconditionally get wrapped in their own correlation ID
                Statement::GenerateCrashDump(_, correlation_id_slot, _) => {
                    debug_assert_eq!(correlation_id_slot, &None);
                    let correlation_id = CorrelationId(next_correlation_id);
                    next_correlation_id += 1;
                    *correlation_id_slot = Some(correlation_id);
                    assert!(correlation_ids_checked.insert(correlation_id));
                    last_correlation_id_emitted = Some(correlation_id);
                }
                Statement::IfBlock(..) | Statement::IgnoreTest(_) | Statement::Phase(..) => {
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
            DebuggerKind::Gdb => {}
            DebuggerKind::Lldb => {
                // We want to actually stop at breakpoints
                writeln!(script, "script lldb.debugger.SetAsync(False)").unwrap();
                // Don't wait for user input
                writeln!(script, "settings set auto-confirm true").unwrap();
            }
            DebuggerKind::Mock => {
                // no prelude
            }
        };

        for command in &self.prelude {
            writeln!(script, "{}", command).unwrap();
        }
    }

    /// Emit commands for setting breakpoints that have been specified via #break directives
    fn emit_breakpoints(
        &self,
        test_definition: &TestDefinition,
        phase: &PhaseConfig,
        script: &mut String,
    ) {
        if *phase != PhaseConfig::Live {
            return;
        }

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
                DebuggerKind::Gdb => {
                    writeln!(
                        script,
                        "break '{}:{}'",
                        test_definition.name.rsplit_once('/').unwrap().1,
                        bp.line_index + 1
                    )
                    .unwrap();
                }
                DebuggerKind::Lldb => {
                    writeln!(
                        script,
                        "breakpoint set --file '{}' --line {}",
                        test_definition.name.rsplit_once('/').unwrap().1,
                        bp.line_index + 1
                    )
                    .unwrap();
                }
            }
        }
    }

    pub fn evaluation_context(
        &self,
        cargo_profile: &Arc<str>,
        phase: &PhaseConfig,
    ) -> EvaluationContext {
        let default_value: Value = "true".into();

        let mut evaluation_context = HashMap::new();
        evaluation_context.insert(format!("@{}", self.kind.name()), default_value.clone());
        evaluation_context.insert("@debugger".into(), self.kind.name().into());
        evaluation_context.insert("@version".into(), (&self.version).into());
        evaluation_context.insert("@cargo_profile".into(), cargo_profile.into());
        evaluation_context.insert("@phase".into(), phase.to_variable_value());

        for define in &self.defines[..] {
            if evaluation_context
                .insert(define.to_string(), default_value.clone())
                .is_some()
            {
                panic!("Duplicate define `{}`", define);
            }
        }

        EvaluationContext {
            values: evaluation_context,
        }
    }
}

const CORRELATION_ID_BEGIN_MARKER: &str = "__correlation_id_begin__=";
const CORRELATION_ID_END_MARKER: &str = "__correlation_id_end__=";

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
pub fn generate_debugger_script(
    debugger: &Debugger,
    test_definition: &TestDefinition,
    cargo_profile: &Arc<str>,
    phase: &PhaseConfig,
    mk_crashdump_path: &mut dyn FnMut(/* tag */ &str) -> PathBuf,
) -> String {
    let mut debugger_script = String::new();

    debugger.emit_script_prelude(&mut debugger_script);
    debugger.emit_breakpoints(test_definition, phase, &mut debugger_script);

    let mut script = test_definition.script.clone();
    debugger.assign_correlation_ids(&mut script, cargo_profile, phase);

    let evaluation_context = debugger.evaluation_context(cargo_profile, phase);

    // Emit commands
    let mut last_correlation_id = None;

    script.walk_applicable_leaves(&evaluation_context, &mut |statement| {
        // Emit new correlation id if necessary
        match statement {
            script::Statement::Exec(_, correlation_id, _)
            | script::Statement::GenerateCrashDump(_, correlation_id, _) => {
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
            }
            _ => {
                // Other statements don't produce output, so no correlation id for them
            }
        };

        // Emit the actual command
        match statement {
            script::Statement::Exec(command, _, _) => {
                writeln!(&mut debugger_script, "{}", command).unwrap();
            }
            script::Statement::GenerateCrashDump(tag, _, _) => {
                if *phase != PhaseConfig::Live {
                    warn!(
                        "Encountered {} command in crashdump phase. Ignoring.",
                        script::TOKEN_GENERATE_CRASHDUMP
                    );
                }

                let crashdump_path = mk_crashdump_path(tag);
                debugger.emit_crashdump_command(&crashdump_path, &mut debugger_script);
            }
            _ => {
                // other statements don't have an effect here
            }
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
    cargo_profile: &Arc<str>,
    phase: &PhaseConfig,
) -> TestResult {
    let mut script = test_definition.script.clone();
    debugger.assign_correlation_ids(&mut script, cargo_profile, phase);

    let mut checks_by_correlation_id: BTreeMap<CorrelationId, Vec<Statement>> = BTreeMap::new();

    let evaluation_context = debugger.evaluation_context(cargo_profile, phase);

    script.walk_applicable_leaves(&evaluation_context, &mut |statement| {
        match statement {
            Statement::Check(_, cid, _) | Statement::CheckUnorderedBlock(_, cid, _) => {
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

    let debugger_output_by_correlation_id = match debugger_output_by_correlation_id(
        &debugger_output,
        test_definition,
        debugger,
        cargo_profile,
    ) {
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
                cargo_profile,
                Status::Failed(format!("check {:?} failed", &checks[0]), debugger_output),
            );
        };

        let mut check_index = 0;
        for output_line in output {
            match &checks[check_index] {
                Statement::Check(check, _cid, _) => {
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
            let (expected, line_number) = match &checks[check_index] {
                Statement::Check(check, _, line_number) => (&check.source, line_number),
                &Statement::CheckUnorderedBlock(..) => {
                    todo!()
                }
                _ => {
                    unreachable!()
                }
            };

            let mut message = format!(
                "Could not find '{}' in debugger output. Expected to find it \
                 within the following lines:\n\n",
                expected
            );

            for line in output.iter().filter(|x| {
                !x.contains(CORRELATION_ID_BEGIN_MARKER) && !x.contains(CORRELATION_ID_END_MARKER)
            }) {
                writeln!(message, "> {}", line).unwrap();
            }

            writeln!(message).unwrap();
            writeln!(
                message,
                "Check failed at: {}:{}",
                test_definition.absolute_source_path.display(),
                line_number.0,
            )
            .unwrap();

            let status = Status::Failed(message, debugger_output);

            return TestResult::new(test_definition, debugger, cargo_profile, status);
        }
    }

    TestResult::new(test_definition, debugger, cargo_profile, Status::Passed)
}

/// Splits debugger output into sections that correspond to a single correlation ID.
fn debugger_output_by_correlation_id<'a>(
    debugger_output: &'a DebuggerOutput,
    test_definition: &TestDefinition,
    debugger: &Debugger,
    cargo_profile: &Arc<str>,
) -> Result<BTreeMap<CorrelationId, Vec<&'a str>>, TestResult> {
    let mut result = BTreeMap::new();

    let mut current_id = None;
    let mut current_lines = vec![];
    for line in debugger_output.stdout.lines() {
        if line.starts_with(CORRELATION_ID_BEGIN_MARKER) {
            if current_id.is_some() {
                // malformed debugger output
                return Err(TestResult::new(
                    test_definition,
                    debugger,
                    cargo_profile,
                    Status::Errored(
                        "Unexpected debugger output: correlation ID begin marker during active correlation section."
                            .to_string(),
                    ),
                ));
            }
            assert!(current_lines.is_empty());
            current_id = Some(extract_correlation_id(line));
        } else if line.starts_with(CORRELATION_ID_END_MARKER) {
            if let Some(current_id) = current_id {
                result.insert(current_id, current_lines);
                current_lines = vec![];
            } else {
                // malformed debugger output
                return Err(TestResult::new(
                    test_definition,
                    debugger,
                    cargo_profile,
                    Status::Errored(
                        "Unexpected debugger output: unopened correlation ID end marker."
                            .to_string(),
                    ),
                ));
            }

            current_id = None;
        } else if current_id.is_some() {
            current_lines.push(line);
        }
    }

    if let Some(current_id) = current_id {
        // Always add the current ID even if it was not closed.
        // This leads to better failure messages when the debugger scripts
        // aborts prematurely.
        result.insert(current_id, current_lines);
    }

    Ok(result)
}

/// Takes a set of debugger commandline commands and tries to create a [Debugger] object for each.
pub fn init_debuggers(
    commands: &[PathBuf],
    preludes: &[OsString],
    commandline_args: &[OsString],
    env_vars: &[OsString],
    defines: &[String],
) -> anyhow::Result<Vec<Debugger>> {
    let prelude_map = build_prelude_map(preludes)?;
    let commandline_arg_map = build_commandline_arg_map(commandline_args)?;
    let env_var_map = build_env_var_map(env_vars)?;

    let defines: Arc<[Arc<str>]> = defines
        .iter()
        .map(|s| Arc::from(format!("@{}", s)))
        .collect::<Vec<_>>()
        .into();

    info!("Setting up debuggers");
    let mut debuggers = vec![];

    for command in commands {
        info!("Trying to set up debugger {}", command.display());
        let (debugger_kind, version) = Debugger::infer_from_command(command)?;

        let debugger = Debugger::new(
            debugger_kind,
            version,
            command.into(),
            prelude_map.get(&debugger_kind).cloned().unwrap_or_default(),
            commandline_arg_map
                .get(&debugger_kind)
                .cloned()
                .unwrap_or_default(),
            env_var_map.get(&debugger_kind).cloned().unwrap_or_default(),
            defines.clone(),
        );

        info!("Successfully set up debugger: {:?}", debugger);
        debuggers.push(debugger);
    }

    Ok(debuggers)
}

fn build_prelude_map(preludes: &[OsString]) -> anyhow::Result<HashMap<DebuggerKind, Vec<String>>> {
    info!("Scanning debugger preludes");
    partition_by_debugger_kind(preludes).context("while scanning debugger preludes")
}

fn build_commandline_arg_map(
    preludes: &[OsString],
) -> anyhow::Result<HashMap<DebuggerKind, Vec<String>>> {
    info!("Scanning debugger commandline args");
    partition_by_debugger_kind(preludes).context("while scanning debugger commandline args")
}

fn build_env_var_map(
    env_vars: &[OsString],
) -> anyhow::Result<HashMap<DebuggerKind, Vec<(String, String)>>> {
    info!("Scanning debugger environment variables");
    let by_debugger_kind =
        partition_by_debugger_kind(env_vars).context("while scanning debugger env vars")?;

    let mut env_vars = HashMap::with_capacity(by_debugger_kind.len());

    for (debugger_kind, var_specs) in by_debugger_kind {
        let mut split_var_specs = Vec::with_capacity(var_specs.len());

        for var_spec in var_specs {
            if let Some((var_name, var_value)) = var_spec.split_once('=') {
                split_var_specs.push((var_name.trim().to_string(), var_value.trim().to_string()));
            } else {
                bail!(
                    "Could not parse env var spec: {}:{}",
                    debugger_kind,
                    var_spec
                );
            }
        }

        env_vars.insert(debugger_kind, split_var_specs);
    }

    Ok(env_vars)
}

fn partition_by_debugger_kind(
    strings: &[OsString],
) -> anyhow::Result<HashMap<DebuggerKind, Vec<String>>> {
    let mut by_kind: HashMap<DebuggerKind, Vec<String>> = Default::default();
    for s in strings {
        let as_str = s.to_string_lossy();
        if let Some((debugger, command)) = as_str.split_once(':') {
            let debugger_kind = DebuggerKind::try_from(debugger.trim())?;

            by_kind
                .entry(debugger_kind)
                .or_default()
                .push(command.trim().to_owned());
        } else {
            bail!("No debugger kind specified in {}", as_str);
        }
    }
    Ok(by_kind)
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

fn extract_lldb_version(version_output: &str) -> Option<&str> {
    lazy_static! {
        // example: lldb version 12.0.0
        static ref LLDB_REGEX: Regex = Regex::new(r"lldb\s+version\s+([\d\.]+)").unwrap();
    }

    LLDB_REGEX
        .captures(version_output)
        .map(|captures| captures.get(1).unwrap().as_str())
}

#[cfg(test)]
mod tests {
    use std::{
        path::{Path, PathBuf},
        sync::Arc,
    };

    use crate::{
        cargo_test_directory::TestDefinition,
        debugger::Debugger,
        script::{parse_script, CorrelationId, LineNumber, PhaseConfig, Statement},
    };

    fn from_lines(lines: &[&str]) -> String {
        let mut output = String::new();

        for line in lines {
            output.push_str(line);
            output.push('\n');
        }
        output
    }

    fn mock_test_def(script: String) -> TestDefinition {
        let script = parse_script(&script, None).unwrap();

        let root_path = if cfg!(windows) {
            Path::new("D:\\mock")
        } else {
            Path::new("/mock")
        };

        let relative_path = Path::new("src").join("main.rs");

        TestDefinition::new(
            relative_path.as_path(),
            root_path.join(relative_path.as_path()).as_path(),
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
            "#if @mock",
            "  print abc",
            "  #check __abc__",
            "  #generate-crashdump foo",
            "  print xyz",
            "  #check __xyz__",
            "***/",
        ]));

        let script = super::generate_debugger_script(
            &Debugger::mock(),
            &test_def,
            &Arc::from("debug"),
            &PhaseConfig::Live,
            &mut |tag| PathBuf::from(format!("base-dir/{}/crashdump.dmp", tag)),
        );

        assert_eq!(
            script,
            from_lines(&[
                "__correlation_id_begin__=0",
                "print abc",
                "__correlation_id_end__=0",
                "__correlation_id_begin__=1",
                "generate_crashdump base-dir/foo/crashdump.dmp",
                "__correlation_id_end__=1",
                "__correlation_id_begin__=2",
                "print xyz",
                "__correlation_id_end__=2",
            ])
        );
    }

    #[test]
    fn correlation_id_assignment_simple() {
        let mut script = mock_test_def(from_lines(&[
            "/***",
            "foo",
            "#check foo",
            "bar",
            "#check bar",
            "baz",
            "#check baz",
            "***/",
        ]))
        .script;

        Debugger::mock().assign_correlation_ids(
            &mut script,
            &Arc::from("debug"),
            &PhaseConfig::Live,
        );

        assert_eq!(
            script.statements,
            vec![
                Statement::Exec("foo".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Check("foo".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Exec("bar".into(), Some(CorrelationId(1)), LineNumber::ANY),
                Statement::Check("bar".into(), Some(CorrelationId(1)), LineNumber::ANY),
                Statement::Exec("baz".into(), Some(CorrelationId(2)), LineNumber::ANY),
                Statement::Check("baz".into(), Some(CorrelationId(2)), LineNumber::ANY),
            ]
        );
    }

    #[test]
    fn correlation_id_assignment_multi_exec() {
        let mut script = mock_test_def(from_lines(&[
            "/***",
            "foo",
            "bar",
            "baz",
            "#check baz",
            "***/",
        ]))
        .script;

        Debugger::mock().assign_correlation_ids(
            &mut script,
            &Arc::from("debug"),
            &PhaseConfig::Live,
        );

        assert_eq!(
            script.statements,
            vec![
                Statement::Exec("foo".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Exec("bar".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Exec("baz".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Check("baz".into(), Some(CorrelationId(0)), LineNumber::ANY),
            ]
        );
    }

    #[test]
    fn correlation_id_assignment_multi_check() {
        let mut script = mock_test_def(from_lines(&[
            "/***",
            "foo",
            "#check foo",
            "#check bar",
            "#check baz",
            "***/",
        ]))
        .script;

        Debugger::mock().assign_correlation_ids(
            &mut script,
            &Arc::from("debug"),
            &PhaseConfig::Live,
        );

        assert_eq!(
            script.statements,
            vec![
                Statement::Exec("foo".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Check("foo".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Check("bar".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Check("baz".into(), Some(CorrelationId(0)), LineNumber::ANY),
            ]
        );
    }

    #[test]
    fn correlation_id_assignment_multiple_of_both() {
        let mut script = mock_test_def(from_lines(&[
            "/***",
            "foo",
            "bar",
            "baz",
            "#check foo",
            "#check bar",
            "#check baz",
            "***/",
        ]))
        .script;

        Debugger::mock().assign_correlation_ids(
            &mut script,
            &Arc::from("debug"),
            &PhaseConfig::Live,
        );

        assert_eq!(
            script.statements,
            vec![
                Statement::Exec("foo".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Exec("bar".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Exec("baz".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Check("foo".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Check("bar".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Check("baz".into(), Some(CorrelationId(0)), LineNumber::ANY),
            ]
        );
    }

    #[test]
    fn correlation_id_assignment_generate_crashdump() {
        let mut script = mock_test_def(from_lines(&[
            "/***",
            "foo",
            "bar",
            "baz",
            "#generate-crashdump tag",
            "#check foo",
            "#check bar",
            "#check baz",
            "***/",
        ]))
        .script;

        Debugger::mock().assign_correlation_ids(
            &mut script,
            &Arc::from("debug"),
            &PhaseConfig::Live,
        );

        assert_eq!(
            script.statements,
            vec![
                Statement::Exec("foo".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Exec("bar".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::Exec("baz".into(), Some(CorrelationId(0)), LineNumber::ANY),
                Statement::GenerateCrashDump("tag".into(), Some(CorrelationId(1)), LineNumber::ANY),
                Statement::Check("foo".into(), Some(CorrelationId(1)), LineNumber::ANY),
                Statement::Check("bar".into(), Some(CorrelationId(1)), LineNumber::ANY),
                Statement::Check("baz".into(), Some(CorrelationId(1)), LineNumber::ANY),
            ]
        );
    }

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
        assert_eq!(super::extract_gdb_version("lldb version 12.0.0"), None);
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
        assert_eq!(super::extract_cdb_version("lldb version 12.0.0"), None);
    }

    #[test]
    fn lldb_version_extraction() {
        assert_eq!(
            super::extract_lldb_version("lldb version 12.0.0"),
            Some("12.0.0")
        );
        assert_eq!(super::extract_lldb_version("GNU gdb (GDB) 8.2.1"), None);
        assert_eq!(
            super::extract_lldb_version("GNU gdb (Ubuntu 9.2-0ubuntu1~20.04) 9.2"),
            None
        );
        assert_eq!(
            super::extract_lldb_version("cdb version 10.0.21349.1004"),
            None
        );
    }
}
