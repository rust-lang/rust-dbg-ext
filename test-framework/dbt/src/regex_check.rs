use std::{
    borrow::Cow,
    collections::HashMap,
    sync::{Arc, Mutex},
};

use anyhow::bail;
use regex::Regex;
use structopt::lazy_static::lazy_static;

/// A type for handling `#check` commands. It takes check specifications of the form
///
/// ```ignore
/// some plain text @{ some regex }@ some more plain text @{ another regex }@ plain text again
/// ```
///
/// and transform them into a single [Regex] that can be used for matching the specification.
#[derive(Debug, Clone)]
pub struct RegexCheck {
    regex: Arc<Regex>,
    pub source: Arc<str>,
}

impl From<&str> for RegexCheck {
    fn from(s: &str) -> Self {
        RegexCheck::new(s).unwrap()
    }
}

impl PartialEq for RegexCheck {
    fn eq(&self, other: &Self) -> bool {
        self.source == other.source
    }
}

impl Eq for RegexCheck {}

impl std::hash::Hash for RegexCheck {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let Self {
            // We only hash the source to keep this in line with Eq.
            regex: _,
            ref source,
        } = *self;

        source.hash(state);
    }
}

impl RegexCheck {
    pub fn new(source: &str) -> anyhow::Result<Self> {
        let ws_normalized = normalize_whitespace(source);

        lazy_static! {
            static ref INTERNER: Mutex<HashMap<Arc<str>, RegexCheck>> =
                Mutex::new(HashMap::default());
        }

        {
            let interner = INTERNER.lock().unwrap();

            if let Some(interned) = interner.get(&ws_normalized[..]) {
                return Ok(interned.clone());
            }
        }

        let mut regex = String::new();

        regex.push_str(&escape_and_insert_regex_sections(&normalize_whitespace(
            source,
        ))?);

        let key: Arc<str> = ws_normalized.into_owned().into();

        let regex_check = Self {
            regex: Arc::new(Regex::new(&regex)?),
            source: key.clone(),
        };

        {
            let mut interner = INTERNER.lock().unwrap();
            interner.insert(key, regex_check.clone());
        }

        Ok(regex_check)
    }

    pub fn check(&self, text: &str) -> bool {
        self.regex.is_match(&normalize_whitespace(text))
    }
}

fn normalize_whitespace(s: &str) -> Cow<str> {
    let s = s.trim();

    lazy_static! {
        static ref WHITESPACE: Regex = Regex::new(r"\s\s+").unwrap();
    }

    let ranges_to_replace: Vec<_> = WHITESPACE.find_iter(s).map(|m| m.range()).collect();

    if ranges_to_replace.is_empty() {
        return Cow::from(s);
    }

    let mut normalized = s.to_owned();

    for range in ranges_to_replace.into_iter().rev() {
        normalized.replace_range(range, " ");
    }

    normalized.into()
}

fn escape_and_insert_regex_sections(mut s: &str) -> anyhow::Result<String> {
    let mut result = String::new();

    loop {
        s = if let Some(regex_section_begin) = s.find("@{") {
            // Escape the none-regex part
            result.push_str(&regex::escape(s[..regex_section_begin].trim_end()));

            if let Some(regex_section_end) = s.find("}@") {
                result.push_str(s[regex_section_begin + 2..regex_section_end].trim());

                s[regex_section_end + 2..].trim_start()
            } else {
                bail!("Unclosed regex section in check statement")
            }
        } else {
            result.push_str(&regex::escape(s));
            ""
        };

        if s.is_empty() {
            break;
        }
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::RegexCheck;

    #[test]
    fn check_simple_string() {
        let check = RegexCheck::new("abc").unwrap();
        let text1 = "abc";
        let text2 = "abcd";

        let text3 = "";
        let text4 = "ab";
        let text5 = "abd";

        assert!(check.check(text1));
        assert!(check.check(text2));
        assert!(!check.check(text3));
        assert!(!check.check(text4));
        assert!(!check.check(text5));
    }

    #[test]
    fn normalize_whitespace() {
        let expected = "abc def";
        assert_eq!(super::normalize_whitespace(" abc def"), expected);
        assert_eq!(super::normalize_whitespace("abc def "), expected);
        assert_eq!(super::normalize_whitespace("abc  def"), expected);
        assert_eq!(super::normalize_whitespace("  abc   def    "), expected);
    }

    #[test]
    fn check_whitespace_normalized() {
        let check = RegexCheck::new("abc def").unwrap();
        let text1 = " abc def";
        let text2 = "abc def ";
        let text3 = "abc  def";
        let text4 = "  abc   def    ";

        let text5 = "abc de f";
        let text6 = "abcdef";

        assert!(check.check(text1));
        assert!(check.check(text2));
        assert!(check.check(text3));
        assert!(check.check(text4));

        assert!(!check.check(text5));
        assert!(!check.check(text6));
    }

    #[test]
    fn insert_regex() {
        let expected = "ab.*def";
        assert_eq!(
            super::escape_and_insert_regex_sections("ab@{ .* }@def").unwrap(),
            expected
        );
        assert_eq!(
            super::escape_and_insert_regex_sections("ab @{ .* }@def").unwrap(),
            expected
        );
        assert_eq!(
            super::escape_and_insert_regex_sections("ab @{ .* }@ def").unwrap(),
            expected
        );
        assert_eq!(
            super::escape_and_insert_regex_sections("ab@{.*}@def").unwrap(),
            expected
        );
        assert_eq!(
            super::escape_and_insert_regex_sections("ab @{ .* }@  def").unwrap(),
            expected
        );

        assert_eq!(
            super::escape_and_insert_regex_sections("a[b").unwrap(),
            "a\\[b"
        );
        assert_eq!(
            super::escape_and_insert_regex_sections("a*b").unwrap(),
            "a\\*b"
        );
        assert_eq!(
            super::escape_and_insert_regex_sections("a{b").unwrap(),
            "a\\{b"
        );
    }

    #[test]
    fn check_regex_with_escape() {
        let check = RegexCheck::new("a[bc d @{ .* }@ f}").unwrap();
        let text1 = "a[bc def}";
        let text2 = "a[bc df}";
        let text3 = "a[bc  dxxxxf}";
        let text4 = "  a[bc   d   x  f}    ";

        let text5 = "a[bc xef}";
        let text6 = "a[bc dex}";

        assert!(check.check(text1));
        assert!(check.check(text2));
        assert!(check.check(text3));
        assert!(check.check(text4));

        assert!(!check.check(text5));
        assert!(!check.check(text6));
    }
}
