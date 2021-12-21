use anyhow::{bail, Context};
use regex::Regex;
use std::{
    collections::HashMap,
    convert::TryInto,
    iter::Peekable,
    sync::{Arc, Mutex},
};
use structopt::lazy_static::lazy_static;

use crate::regex_check::RegexCheck;

/// The AST of a test script. It is used for
///
/// - generating concrete debugger scripts, and
/// - checking debugger output
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Script {
    pub statements: Vec<Statement>,
}

/// Defines an environment for evaluating #if directives. It maps names like `version` to
/// actual values.
#[derive(Debug)]
pub struct EvaluationContext {
    pub values: HashMap<String, Value>,
}

impl EvaluationContext {
    pub fn with_additional_values(&self, additions: Vec<(String, Value)>) -> Self {
        let mut values = self.values.clone();
        values.extend(additions);
        Self { values }
    }
}

impl Script {
    pub fn new_empty() -> Self {
        Self {
            statements: Vec::new(),
        }
    }

    /// Returns true if this test should be ignored because and #ignore-test statement
    /// is encountered for the given evaluation context.
    pub fn ignore_test(&self, context: &EvaluationContext) -> bool {
        let mut ignore_test = false;

        self.walk_applicable_leaves(context, &mut |statement| {
            if matches!(statement, Statement::IgnoreTest) {
                ignore_test = true;
                false
            } else {
                true
            }
        });

        ignore_test
    }

    /// Invokes `f` for each leave directive (Exec, Check, CheckUnordered, IgnoreTest)
    /// that is encountered while walking the AST in definition order for the given
    /// evaluation context.
    pub fn walk_applicable_leaves(
        &self,
        context: &EvaluationContext,
        f: &mut dyn FnMut(&Statement) -> bool,
    ) {
        for statement in &self.statements {
            if !statement.walk_applicable_leaves(context, f) {
                return;
            }
        }
    }

    /// Invokes `f` for each leave directive (Exec, Check, CheckUnordered, IgnoreTest)
    /// that is encountered while walking the AST in definition order for the given
    /// evaluation context.
    pub fn walk_applicable_leaves_mut(
        &mut self,
        context: &EvaluationContext,
        f: &mut dyn FnMut(&mut Statement) -> bool,
    ) {
        for statement in &mut self.statements {
            if !statement.walk_applicable_leaves_mut(context, f) {
                return;
            }
        }
    }
}

#[derive(Debug, Eq, Clone)]
pub struct Value {
    string: Arc<str>,
    version: Option<Arc<[i128]>>,
}

impl Value {
    fn from_arc(s: Arc<str>) -> Self {
        assert!(s.trim().len() == s.len());

        let mut version_components = vec![];

        for component in s.split('.') {
            if let Ok(value) = component.parse() {
                version_components.push(value);
            } else {
                return Self {
                    string: s,
                    version: None,
                };
            }
        }

        assert!(!version_components.is_empty());

        // Remove trailing zeros
        while let Some(last) = version_components.last().cloned() {
            if last != 0 || version_components.len() == 1 {
                // Don't remove the last remaining component, even if it is zero
                break;
            }
            version_components.pop();
        }

        Self {
            string: s,
            version: Some(version_components.into()),
        }
    }

    fn new(s: &str) -> Self {
        Self::from_arc(Arc::from(s.trim()))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (&self.version, &other.version) {
            (Some(a), Some(b)) => a == b,
            _ => self.string == other.string,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ord::cmp(self, other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (&self.version, &other.version) {
            (Some(a), Some(b)) => a.cmp(b),
            _ => self.string.cmp(&other.string),
        }
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::new(s)
    }
}

impl From<Arc<str>> for Value {
    fn from(s: Arc<str>) -> Self {
        Value::from_arc(s)
    }
}

impl From<&Arc<str>> for Value {
    fn from(s: &Arc<str>) -> Self {
        Value::from_arc(s.clone())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CorrelationId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    IfBlock(Condition, Vec<Statement>),
    CheckUnorderedBlock(Vec<String>, Option<CorrelationId>),
    Exec(String, Option<CorrelationId>),
    Check(RegexCheck, Option<CorrelationId>),
    IgnoreTest,
}

impl Statement {
    fn walk_applicable_leaves<'a>(
        &'a self,
        context: &EvaluationContext,
        f: &mut dyn FnMut(&'a Statement) -> bool,
    ) -> bool {
        match self {
            Statement::IfBlock(condition, statements) => {
                if condition.eval(context) {
                    for statement in statements {
                        if !statement.walk_applicable_leaves(context, f) {
                            return false;
                        }
                    }
                }
                true
            }
            _ => f(self),
        }
    }

    fn walk_applicable_leaves_mut<'a>(
        &'a mut self,
        context: &EvaluationContext,
        f: &mut dyn FnMut(&'a mut Statement) -> bool,
    ) -> bool {
        match self {
            Statement::IfBlock(condition, statements) => {
                if condition.eval(context) {
                    for statement in statements {
                        if !statement.walk_applicable_leaves_mut(context, f) {
                            return false;
                        }
                    }
                }
                true
            }
            _ => f(self),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Line {
    If { indent: isize, condition: Condition },
    Check { indent: isize, check: RegexCheck },
    CheckUnordered { indent: isize },
    Raw { indent: isize, text: String },
    IgnoreTest { indent: isize },
}

impl Line {
    fn indent(&self) -> isize {
        match *self {
            Line::If { indent, .. }
            | Line::Check { indent, .. }
            | Line::CheckUnordered { indent }
            | Line::Raw { indent, .. }
            | Line::IgnoreTest { indent } => indent,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Comparison {
    /// `#if version == foo`
    Eq,
    /// `#if version != foo`
    NotEq,
    /// `#if version < foo`
    LessThan,
    /// `#if version <= foo`
    LessThanOrEq,
    /// `#if version > foo`
    GreaterThan,
    /// `#if version >= foo`
    GreaterThanOrEq,
    /// `#if version ~= foo` -> Regex.is_match()
    Matches,
    /// `#if version contains foo` -> str::contains
    Contains,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Condition {
    /// e.g. `#if gdb`. True if the current debugger kind is equals to the given string.
    Debugger(String),
    /// e.g. `#if version == 1`. The left hand side is expected to be a name defined in
    /// the evaluation context.
    Comparison(String, Comparison, Value),
    /// e.g. `#if gdb && version == 9`.
    And(Box<Condition>, Box<Condition>),
    /// e.g. `#if gdb || version == 9`.
    Or(Box<Condition>, Box<Condition>),
    /// e.g. `#if not gdb`.
    Not(Box<Condition>),
}

impl Condition {
    pub fn eval(&self, context: &EvaluationContext) -> bool {
        match self {
            Self::Debugger(debugger) => context
                .values
                .get("debugger")
                .unwrap()
                .string
                .eq_ignore_ascii_case(debugger),
            Self::Comparison(lhs, cmp, rhs) => {
                let lhs = context.values.get(lhs).unwrap();

                match *cmp {
                    Comparison::Eq => lhs == rhs,
                    Comparison::NotEq => lhs != rhs,
                    Comparison::LessThan => lhs < rhs,
                    Comparison::LessThanOrEq => lhs <= rhs,
                    Comparison::GreaterThan => lhs > rhs,
                    Comparison::GreaterThanOrEq => lhs >= rhs,
                    Comparison::Contains => lhs.string.contains(&rhs.string[..]),
                    Comparison::Matches => {
                        let regex = get_regex(&rhs.string).unwrap();
                        regex.is_match(&lhs.string)
                    }
                }
            }
            Self::And(lhs, rhs) => lhs.eval(context) && rhs.eval(context),
            Self::Or(lhs, rhs) => lhs.eval(context) || rhs.eval(context),
            Self::Not(inner) => !inner.eval(context),
        }
    }
}

fn parse_line(line: &str) -> anyhow::Result<Line> {
    let (line, indent) = trim_indent(line)?;

    if line.starts_with(TOKEN_IF) {
        return parse_if(line, indent);
    }

    if line.starts_with(TOKEN_CHECK_UNORDERED) {
        return parse_check_unordered(line, indent);
    }

    if line.starts_with(TOKEN_CHECK) {
        return parse_check(line, indent);
    }

    if line.starts_with(TOKEN_IGNORE_TEST) {
        return parse_ignore(line, indent);
    }

    if line.starts_with("#") {
        bail!(
            "Encountered unknown keyword `{}`",
            tokenize(line).next().unwrap()
        )
    }

    Ok(Line::Raw {
        indent,
        text: line.trim().to_string(),
    })
}

const TOKEN_IF: &str = "#if";
const TOKEN_CHECK: &str = "#check";
const TOKEN_CHECK_UNORDERED: &str = "#check-unordered";
const TOKEN_IGNORE_TEST: &str = "#ignore-test";
const TOKEN_SCRIPT_START: &str = "/***";
const TOKEN_SCRIPT_END: &str = "***/";
const TOKEN_COMMENT: &str = "//";
const TOKEN_AND: &str = "&&";
const TOKEN_OR: &str = "||";
const TOKEN_NOT: &str = "not";
const TOKEN_EQ: &str = "==";
const TOKEN_NEQ: &str = "!=";
const TOKEN_CONTAINS: &str = "contains";
const TOKEN_MATCHES: &str = "~=";
const TOKEN_LT: &str = "<";
const TOKEN_LT_EQ: &str = "<=";
const TOKEN_GT: &str = ">";
const TOKEN_GT_EQ: &str = ">=";

fn parse_if(line: &str, indent: isize) -> anyhow::Result<Line> {
    let mut tokens = tokenize(line);

    expect(&mut tokens, TOKEN_IF)?;

    Ok(Line::If {
        indent,
        condition: parse_condition(&mut tokens.peekable())?,
    })
}

fn parse_check(line: &str, indent: isize) -> anyhow::Result<Line> {
    let mut tokens = tokenize(line);

    expect(&mut tokens, TOKEN_CHECK)?;

    Ok(Line::Check {
        indent,
        check: RegexCheck::new(&concat(tokens))?,
    })
}

fn parse_check_unordered(line: &str, indent: isize) -> anyhow::Result<Line> {
    let mut tokens = tokenize(line);

    expect(&mut tokens, TOKEN_CHECK_UNORDERED)?;

    Ok(Line::CheckUnordered { indent })
}

fn parse_ignore(line: &str, indent: isize) -> anyhow::Result<Line> {
    let mut tokens = tokenize(line);
    expect(&mut tokens, TOKEN_IGNORE_TEST)?;
    Ok(Line::IgnoreTest { indent })
}

fn tokenize(line: &str) -> impl Iterator<Item = &str> {
    line.split(char::is_whitespace).filter(|x| !x.is_empty())
}

fn concat<'a>(it: impl Iterator<Item = &'a str>) -> String {
    let mut string = String::new();
    for piece in it {
        string.push_str(piece);
        string.push(' ');
    }

    string.pop();
    string
}

fn expect<'a>(tokens: &mut impl Iterator<Item = &'a str>, expected: &str) -> anyhow::Result<()> {
    match tokens.next() {
        Some(token) if token == expected => Ok(()),
        other => bail_expected(TOKEN_IF, other),
    }
}

fn bail_expected<T>(expected: &str, found: Option<&str>) -> anyhow::Result<T> {
    if let Some(found) = found {
        bail!("expected `{}`, found `{}`", expected, found);
    } else {
        bail!("expected `{}`, found nothing", expected);
    }
}

fn to_comparsion_op(s: &str) -> Option<Comparison> {
    match s {
        TOKEN_EQ => Some(Comparison::Eq),
        TOKEN_CONTAINS => Some(Comparison::Contains),
        TOKEN_MATCHES => Some(Comparison::Matches),
        TOKEN_NEQ => Some(Comparison::NotEq),
        TOKEN_GT => Some(Comparison::GreaterThan),
        TOKEN_GT_EQ => Some(Comparison::GreaterThanOrEq),
        TOKEN_LT => Some(Comparison::LessThan),
        TOKEN_LT_EQ => Some(Comparison::LessThanOrEq),
        _ => None,
    }
}

fn trim_indent(line: &str) -> anyhow::Result<(&str, isize)> {
    let first_non_whitespace = line.bytes().position(|x| x != b' ').unwrap();
    if line.as_bytes()[first_non_whitespace].is_ascii_whitespace() {
        bail!("only spaces allow for indentation")
    }
    Ok((
        &line[first_non_whitespace..],
        first_non_whitespace.try_into().unwrap(),
    ))
}

fn parse_condition<'a>(
    tokens: &mut Peekable<impl Iterator<Item = &'a str>>,
) -> anyhow::Result<Condition> {
    let term = parse_condition_term(tokens)?;

    match tokens.next() {
        Some(op) => {
            let rhs = parse_condition(tokens)?;

            if op == TOKEN_OR {
                Ok(Condition::Or(Box::new(term), Box::new(rhs)))
            } else if op == TOKEN_AND {
                Ok(Condition::And(Box::new(term), Box::new(rhs)))
            } else {
                bail!("unknown op")
            }
        }
        None => Ok(term),
    }
}

fn parse_condition_term<'a>(
    tokens: &mut Peekable<impl Iterator<Item = &'a str>>,
) -> anyhow::Result<Condition> {
    match tokens.next() {
        Some(lhs) => {
            if lhs == TOKEN_NOT {
                let inner = parse_condition_term(tokens)?;
                return Ok(Condition::Not(Box::new(inner)));
            }

            if let Some(&peek) = tokens.peek() {
                if let Some(comparison_op) = to_comparsion_op(peek) {
                    // Eat operator token
                    let _ = tokens.next();

                    if let Some(rhs) = tokens.next() {
                        if to_comparsion_op(rhs).is_some() {
                            bail!("rhs is operator")
                        }

                        let rhs = Value::new(rhs);

                        if comparison_op == Comparison::Matches {
                            // Validate that the RHS is a valid regular expression
                            get_regex(&rhs.string)?;
                        }

                        return Ok(Condition::Comparison(lhs.into(), comparison_op, rhs));
                    }
                }
            }

            Ok(Condition::Debugger(lhs.into()))
        }
        None => {
            bail!("");
        }
    }
}

fn parse_statement_list(
    lines: &mut Peekable<impl Iterator<Item = Line>>,
    parent_indent: isize,
    token: &str,
) -> anyhow::Result<Vec<Statement>> {
    parse_nested_block(lines, parent_indent, token, |line, lines| match line {
        Line::Raw { text, .. } => Ok(Statement::Exec(text, None)),
        Line::IgnoreTest { .. } => Ok(Statement::IgnoreTest),
        Line::Check { check, .. } => Ok(Statement::Check(check, None)),
        Line::CheckUnordered { indent } => parse_check_unordered_body(lines, indent),
        Line::If { condition, indent } => {
            let nested_body = parse_statement_list(lines, indent, TOKEN_IF)?;
            Ok(Statement::IfBlock(condition, nested_body))
        }
    })
}

fn parse_check_unordered_body(
    lines: &mut Peekable<impl Iterator<Item = Line>>,
    parent_indent: isize,
) -> anyhow::Result<Statement> {
    let checks =
        parse_nested_block(
            lines,
            parent_indent,
            TOKEN_CHECK_UNORDERED,
            |line, _| match line {
                Line::Raw { text, .. } => Ok(text),
                _ => bail!("{} cannot have nested statements", TOKEN_CHECK_UNORDERED),
            },
        )?;

    Ok(Statement::CheckUnorderedBlock(checks, None))
}

fn parse_nested_block<T, I: Iterator<Item = Line>>(
    lines: &mut Peekable<I>,
    parent_indent: isize,
    token: &str,
    process_line: fn(Line, &mut Peekable<I>) -> anyhow::Result<T>,
) -> anyhow::Result<Vec<T>> {
    let mut checks = vec![];
    let first_indent = lines.peek().map_or(-1, |line| line.indent());

    if first_indent <= parent_indent {
        bail!("Empty {} block", token)
    }

    loop {
        if lines
            .peek()
            .map_or(true, |line| line.indent() < first_indent)
        {
            return Ok(checks);
        }

        let line = lines.next().unwrap();
        checks.push(process_line(line, lines)?);
    }
}

pub fn parse_script(script: &str) -> anyhow::Result<Script> {
    lazy_static! {
        static ref START_FINDER: memchr::memmem::Finder<'static> =
            memchr::memmem::Finder::new(TOKEN_SCRIPT_START);
        static ref END_FINDER: memchr::memmem::Finder<'static> =
            memchr::memmem::Finder::new(TOKEN_SCRIPT_END);
        static ref COMMENT_FINDER: memchr::memmem::Finder<'static> =
            memchr::memmem::Finder::new(TOKEN_COMMENT);
    }

    let script_start = if let Some(script_start) = START_FINDER.find(script.as_bytes()) {
        script_start
    } else {
        bail!("Could not find start of debugger script")
    };

    let script_len = if let Some(script_end) = END_FINDER.find(&script.as_bytes()[script_start..]) {
        script_end + TOKEN_SCRIPT_END.len()
    } else {
        bail!("Could not find end of debugger script")
    };

    let mut lines = vec![];

    for (line_index, line) in script[script_start..script_start + script_len]
        .lines()
        .enumerate()
    {
        // Remove comments
        let line = COMMENT_FINDER
            .find(line.as_bytes())
            .map(|index| &line[..index])
            .unwrap_or(line);

        let trimmed = line.trim();

        if trimmed.is_empty() || trimmed == TOKEN_SCRIPT_START || trimmed == TOKEN_SCRIPT_END {
            continue;
        }

        let line =
            parse_line(line).with_context(|| format!("Parsing error at line {}", line_index))?;

        lines.push(line);
    }

    let statements = if lines.is_empty() {
        vec![]
    } else {
        parse_statement_list(&mut lines.into_iter().peekable(), -1, "")?
    };

    Ok(Script { statements })
}

fn get_regex(regex_str: &Arc<str>) -> anyhow::Result<Arc<Regex>> {
    lazy_static! {
        static ref CACHE: Mutex<HashMap<Arc<str>, Arc<Regex>>> = Mutex::new(HashMap::default());
    }

    {
        let mut cache = CACHE.lock().unwrap();

        if let Some(cached) = cache.get(&regex_str[..]) {
            return Ok(cached.clone());
        }

        let regex = Arc::new(Regex::new(regex_str)?);

        cache.insert(regex_str.clone(), regex.clone());

        Ok(regex)
    }
}

#[cfg(test)]
mod tests {
    use crate::script::{
        parse_script, parse_statement_list, Comparison, Statement, Value, TOKEN_SCRIPT_END,
        TOKEN_SCRIPT_START,
    };
    use std::fmt::Write;

    use super::{tokenize, Condition, EvaluationContext, Line, Script};

    #[test]
    fn parse_line() {
        use super::parse_line;

        assert_eq!(
            parse_line("#if cdb").unwrap(),
            Line::If {
                indent: 0,
                condition: Condition::Debugger("cdb".into()),
            }
        );

        assert_eq!(
            parse_line("  #check abc def").unwrap(),
            Line::Check {
                indent: 2,
                check: "abc def".into(),
            }
        );

        assert_eq!(
            parse_line("    foo bar quux").unwrap(),
            Line::Raw {
                indent: 4,
                text: "foo bar quux".into(),
            }
        );

        assert_eq!(
            parse_line("  #check-unordered").unwrap(),
            Line::CheckUnordered { indent: 2 }
        );
    }

    #[test]
    fn parse_condition() {
        use super::parse_condition;

        assert_eq!(
            parse_condition(&mut tokenize("cdb").peekable()).unwrap(),
            Condition::Debugger("cdb".into())
        );
        assert_eq!(
            parse_condition(&mut tokenize("cdb && gdb").peekable()).unwrap(),
            Condition::And(
                Box::new(Condition::Debugger("cdb".into())),
                Box::new(Condition::Debugger("gdb".into()))
            )
        );
        assert_eq!(
            parse_condition(&mut tokenize("cdb && version == 1.3.4").peekable()).unwrap(),
            Condition::And(
                Box::new(Condition::Debugger("cdb".into())),
                Box::new(Condition::Comparison(
                    "version".into(),
                    Comparison::Eq,
                    "1.3.4".into()
                ))
            )
        );

        assert_eq!(
            parse_condition(&mut tokenize("version == 1.3.4 || abc ~= 3.5").peekable()).unwrap(),
            Condition::Or(
                Box::new(Condition::Comparison(
                    "version".into(),
                    Comparison::Eq,
                    "1.3.4".into()
                )),
                Box::new(Condition::Comparison(
                    "abc".into(),
                    Comparison::Matches,
                    "3.5".into()
                ))
            )
        );

        assert_eq!(
            parse_condition(&mut tokenize("version == 1.3.4 && gdb || abc ~= 3.5").peekable())
                .unwrap(),
            Condition::And(
                Box::new(Condition::Comparison(
                    "version".into(),
                    Comparison::Eq,
                    "1.3.4".into()
                )),
                Box::new(Condition::Or(
                    Box::new(Condition::Debugger("gdb".into())),
                    Box::new(Condition::Comparison(
                        "abc".into(),
                        Comparison::Matches,
                        "3.5".into()
                    ))
                ))
            )
        );
    }

    fn script_from_lines(lines: &[&str]) -> Script {
        let mut script = String::new();

        writeln!(&mut script, "{}", TOKEN_SCRIPT_START).unwrap();

        for line in lines {
            writeln!(&mut script, "{}", line).unwrap();
        }

        writeln!(&mut script, "{}", TOKEN_SCRIPT_END).unwrap();

        parse_script(&script).unwrap()
    }

    #[test]
    fn parse_program_body() {
        use super::parse_line as line;

        let lines = vec![
            line("execute something").unwrap(),
            line("execute something else").unwrap(),
            line("#if gdb").unwrap(),
            line("  execute gdb 1").unwrap(),
            line("  #if version == 1").unwrap(),
            line("    #check abc").unwrap(),
            line("    #check def").unwrap(),
            line("    execute gdb 2").unwrap(),
            line("    #check ghi").unwrap(),
            line("    #check-unordered").unwrap(),
            line("      foo").unwrap(),
            line("      bar").unwrap(),
            line("    #check quux").unwrap(),
            line("  #if version == 2").unwrap(),
            line("    execute gdb 3").unwrap(),
            line("    #check xyz").unwrap(),
        ];

        let parsed = parse_statement_list(&mut lines.into_iter().peekable(), -1, "").unwrap();

        assert_eq!(
            parsed,
            vec![
                Statement::Exec("execute something".into(), None),
                Statement::Exec("execute something else".into(), None),
                Statement::IfBlock(
                    Condition::Debugger("gdb".into()),
                    vec![
                        Statement::Exec("execute gdb 1".into(), None),
                        Statement::IfBlock(
                            Condition::Comparison("version".into(), Comparison::Eq, "1".into()),
                            vec![
                                Statement::Check("abc".into(), None),
                                Statement::Check("def".into(), None),
                                Statement::Exec("execute gdb 2".into(), None),
                                Statement::Check("ghi".into(), None),
                                Statement::CheckUnorderedBlock(
                                    vec!["foo".into(), "bar".into(),],
                                    None
                                ),
                                Statement::Check("quux".into(), None),
                            ]
                        ),
                        Statement::IfBlock(
                            Condition::Comparison("version".into(), Comparison::Eq, "2".into()),
                            vec![
                                Statement::Exec("execute gdb 3".into(), None),
                                Statement::Check("xyz".into(), None),
                            ]
                        ),
                    ]
                ),
            ]
        );
    }

    fn context_from(values: &[(&str, &str)]) -> EvaluationContext {
        EvaluationContext {
            values: values
                .into_iter()
                .map(|(k, v)| (k.to_string(), (*v).into()))
                .collect(),
        }
    }

    #[test]
    fn walk_applicable_leaves() {
        let script = script_from_lines(&[
            "#if cdb",
            "  cdb x",
            "  #if version == 1.0",
            "    cdb 1.0",
            "  #if version == 2.0",
            "    cdb 2.0",
            "#if gdb",
            "  gdb x",
            "  #if version == 1.0",
            "    gdb 1.0",
            "  #if version == 2.0",
            "    gdb 2.0",
            "    #ignore-test",
            "#if not gdb",
            "  not-gdb",
        ]);

        let collect_for_context = |ctx| {
            let mut output = String::new();

            script.walk_applicable_leaves(&ctx, &mut |statement| {
                if let Statement::Exec(command, _) = statement {
                    write!(&mut output, "{};", command).unwrap();
                }
                if let Statement::IgnoreTest { .. } = statement {
                    write!(&mut output, "#ignore-test;").unwrap();
                }
                true
            });

            output
        };

        assert_eq!(
            collect_for_context(context_from(&[("debugger", "cdb"), ("version", "")])),
            "cdb x;not-gdb;"
        );
        assert_eq!(
            collect_for_context(context_from(&[("debugger", "cdb"), ("version", "1.0")])),
            "cdb x;cdb 1.0;not-gdb;"
        );
        assert_eq!(
            collect_for_context(context_from(&[("debugger", "cdb"), ("version", "2.0")])),
            "cdb x;cdb 2.0;not-gdb;"
        );

        assert_eq!(
            collect_for_context(context_from(&[("debugger", "gdb"), ("version", "")])),
            "gdb x;"
        );
        assert_eq!(
            collect_for_context(context_from(&[("debugger", "gdb"), ("version", "1.0")])),
            "gdb x;gdb 1.0;"
        );
        assert_eq!(
            collect_for_context(context_from(&[("debugger", "gdb"), ("version", "2.0")])),
            "gdb x;gdb 2.0;#ignore-test;"
        );
    }

    #[test]
    fn comparisons() {
        let ctx = &context_from(&[("x", "17")]);
        assert!(Condition::Comparison("x".into(), Comparison::Eq, "17".into()).eval(ctx));
        assert!(!Condition::Comparison("x".into(), Comparison::Eq, "0".into()).eval(ctx));

        assert!(!Condition::Comparison("x".into(), Comparison::NotEq, "17".into()).eval(ctx));
        assert!(Condition::Comparison("x".into(), Comparison::NotEq, "0".into()).eval(ctx));

        assert!(!Condition::Comparison("x".into(), Comparison::LessThan, "17".into()).eval(ctx));
        assert!(Condition::Comparison("x".into(), Comparison::LessThan, "18".into()).eval(ctx));

        assert!(
            !Condition::Comparison("x".into(), Comparison::LessThanOrEq, "16".into()).eval(ctx)
        );
        assert!(Condition::Comparison("x".into(), Comparison::LessThanOrEq, "17".into()).eval(ctx));
        assert!(Condition::Comparison("x".into(), Comparison::LessThanOrEq, "18".into()).eval(ctx));

        assert!(!Condition::Comparison("x".into(), Comparison::GreaterThan, "17".into()).eval(ctx));
        assert!(Condition::Comparison("x".into(), Comparison::GreaterThan, "16".into()).eval(ctx));

        assert!(
            !Condition::Comparison("x".into(), Comparison::GreaterThanOrEq, "18".into()).eval(ctx)
        );
        assert!(
            Condition::Comparison("x".into(), Comparison::GreaterThanOrEq, "17".into()).eval(ctx)
        );
        assert!(
            Condition::Comparison("x".into(), Comparison::GreaterThanOrEq, "16".into()).eval(ctx)
        );

        assert!(Condition::Comparison("x".into(), Comparison::Contains, "1".into()).eval(ctx));
        assert!(Condition::Comparison("x".into(), Comparison::Contains, "7".into()).eval(ctx));
        assert!(Condition::Comparison("x".into(), Comparison::Contains, "17".into()).eval(ctx));
        assert!(!Condition::Comparison("x".into(), Comparison::Contains, "2".into()).eval(ctx));

        assert!(Condition::Comparison("x".into(), Comparison::Matches, r"\d\d".into()).eval(ctx));
        assert!(Condition::Comparison("x".into(), Comparison::Matches, r"1.".into()).eval(ctx));
        assert!(Condition::Comparison("x".into(), Comparison::Matches, r".7".into()).eval(ctx));
        assert!(!Condition::Comparison("x".into(), Comparison::Matches, r"\s".into()).eval(ctx));
    }

    #[test]
    fn value_new() {
        assert_eq!(
            Value::new("abc"),
            Value {
                string: "abc".into(),
                version: None,
            }
        );

        assert_eq!(
            Value::new("123"),
            Value {
                string: "123".into(),
                version: Some(vec![123].into()),
            }
        );

        assert_eq!(
            Value::new("123."),
            Value {
                string: "123.".into(),
                version: None,
            }
        );

        assert_eq!(
            Value::new("1.2.3"),
            Value {
                string: "1.2.3".into(),
                version: Some(vec![1, 2, 3].into()),
            }
        );

        assert_eq!(
            Value::new("01.0002.003"),
            Value {
                string: "01.0002.003".into(),
                version: Some(vec![1, 2, 3].into()),
            }
        );

        assert_eq!(
            Value::new("1.x.3"),
            Value {
                string: "1.x.3".into(),
                version: None,
            }
        );

        assert_eq!(Value::new("1.0.0"), Value::new("1.0"),);

        assert_eq!(Value::new("1.0.0"), Value::new("1"),);
    }

    #[test]
    fn value_compare() {
        assert!(Value::new("01") == Value::new("1"));
        assert!(Value::new("_02") < Value::new("_1"));
        assert!(Value::new("2.0") > Value::new("1.1"));
        assert!(Value::new("1.2.3") > Value::new("1.01.3"));
        assert!(Value::new("1.0.1") >= Value::new("1.0.0"));
    }
}
