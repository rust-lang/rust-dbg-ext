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

        let pre_expanded = expand_predefined_regexes(ws_normalized.clone());
        let regex_str = escape_and_insert_regex_sections(&pre_expanded)?;

        let key: Arc<str> = ws_normalized.into_owned().into();

        let regex_check = Self {
            regex: Arc::new(Regex::new(&regex_str)?),
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

const PRE_ADDR: &str = "@addr@";
const PRE_ADDR_REGEX: &str = "@{ (0x)?[0-9a-fA-F]{3,16} }@";

const PRE_ANY: &str = "@any@";
const PRE_ANY_REGEX: &str = "@{.*}@";

const PRE_U8: &str = "@u8@";
const PRE_U8_REGEX: &str = "@{ (u8)|(unsigned char) }@";

const PRE_U16: &str = "@u16@";
const PRE_U16_REGEX: &str = "@{ (u16)|(unsigned short) }@";

const PRE_U32: &str = "@u32@";
const PRE_U32_REGEX: &str = "@{ (u32)|(unsigned int) }@";

const PRE_U64: &str = "@u64@";
const PRE_U64_REGEX: &str = "@{ (u64)|(unsigned __int64) }@";

const PRE_I8: &str = "@i8@";
const PRE_I8_REGEX: &str = "@{ (i8)|(char) }@";

const PRE_I16: &str = "@i16@";
const PRE_I16_REGEX: &str = "@{ (i16)|(short) }@";

const PRE_I32: &str = "@i32@";
const PRE_I32_REGEX: &str = "@{ (i32)|(int) }@";

const PRE_I64: &str = "@i64@";
const PRE_I64_REGEX: &str = "@{ (i64)|(__int64) }@";

const PRE_UNIT: &str = "@unit@";
const PRE_UNIT_REGEX: &str = "@{ (\\(\\))|(tuple$<>) }@";

fn expand_predefined_regexes<'a>(s: Cow<'a, str>) -> Cow<'a, str> {
    let at_signs: Vec<usize> = memchr::memrchr_iter(b'@', s.as_bytes()).collect();

    if at_signs.len() < 2 {
        return s;
    }

    lazy_static! {
        static ref PREDEFINED: HashMap<&'static str, &'static str> = {
            let mut map = HashMap::default();
            map.insert(PRE_ANY, PRE_ANY_REGEX);
            map.insert(PRE_ADDR, PRE_ADDR_REGEX);
            map.insert(PRE_UNIT, PRE_UNIT_REGEX);
            map.insert(PRE_U8, PRE_U8_REGEX);
            map.insert(PRE_U16, PRE_U16_REGEX);
            map.insert(PRE_U32, PRE_U32_REGEX);
            map.insert(PRE_U64, PRE_U64_REGEX);
            map.insert(PRE_I8, PRE_I8_REGEX);
            map.insert(PRE_I16, PRE_I16_REGEX);
            map.insert(PRE_I32, PRE_I32_REGEX);
            map.insert(PRE_I64, PRE_I64_REGEX);

            map
        };
    }

    let mut result = s.into_owned();

    for window in at_signs.windows(2) {
        if let &[end, start] = window {
            if let Some(replacement) = PREDEFINED.get(&result[start..=end]) {
                result.replace_range(start..=end, replacement);
            }
        }
    }

    Cow::from(result)
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
                result.push_str("\\s*");
                result.push_str(s[regex_section_begin + 2..regex_section_end].trim());
                result.push_str("\\s*");

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
    use crate::regex_check::{
        PRE_ADDR, PRE_ADDR_REGEX, PRE_ANY, PRE_ANY_REGEX, PRE_U32, PRE_U32_REGEX,
    };

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

        assert!(check.check(" abc def"));
        assert!(check.check("abc def "));
        assert!(check.check("abc  def"));
        assert!(check.check("  abc   def    "));

        assert!(!check.check("abc de f"));
        assert!(!check.check("abcdef"));
    }

    #[test]
    fn insert_regex() {
        let expected = "ab\\s*[abc]\\s*def";
        assert_eq!(
            super::escape_and_insert_regex_sections("ab@{ [abc] }@def").unwrap(),
            expected
        );
        assert_eq!(
            super::escape_and_insert_regex_sections("ab @{ [abc] }@def").unwrap(),
            expected
        );
        assert_eq!(
            super::escape_and_insert_regex_sections("ab @{ [abc] }@ def").unwrap(),
            expected
        );
        assert_eq!(
            super::escape_and_insert_regex_sections("ab@{[abc]}@def").unwrap(),
            expected
        );
        assert_eq!(
            super::escape_and_insert_regex_sections("ab @{ [abc] }@  def").unwrap(),
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
        let check = RegexCheck::new("a[bc d @{ [[:alpha:]]* }@ f}").unwrap();

        assert!(check.check("a[bc def}"));
        assert!(check.check("a[bc df}"));
        assert!(check.check("a[bc  dxxxxf}"));
        assert!(check.check("  a[bc   d   x  f}    "));

        assert!(!check.check("a[bc xef}"));
        assert!(!check.check("a[bc dex}"));
    }

    #[test]
    fn replace_predefined() {
        assert_eq!(super::expand_predefined_regexes("@".into()), "@");
        assert_eq!(
            super::expand_predefined_regexes("abc def".into()),
            "abc def"
        );
        assert_eq!(super::expand_predefined_regexes("@any @".into()), "@any @");

        assert_eq!(
            super::expand_predefined_regexes(
                format!("{PRE_ADDR} abc {PRE_ANY} xyz {PRE_U32}").into()
            ),
            format!("{PRE_ADDR_REGEX} abc {PRE_ANY_REGEX} xyz {PRE_U32_REGEX}")
        );
    }
}
