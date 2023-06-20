use std::{borrow::Cow, path::Path};

pub mod breakpoints;
pub mod cargo_test_directory;
pub mod debugger;
pub mod import_export;
pub mod regex_check;
pub mod script;
pub mod test_result;
pub mod workflow;

pub fn prettify_path(path: &Path) -> Cow<'_, str> {
    let string = path.to_string_lossy();
    const PREFIX: &str = r"\\?\";
    if string.starts_with(PREFIX) {
        if Path::new(&string[PREFIX.len()..]).exists() {
            return Cow::from(string[PREFIX.len()..].to_string());
        }
    }

    string
}
