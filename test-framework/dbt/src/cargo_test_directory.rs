use regex::Regex;
use serde::Deserialize;
use std::{
    collections::HashSet,
    ffi::OsString,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::bail;
use log::{info, warn};

use crate::{
    breakpoints::{self, BreakPoint},
    script::{parse_script, Script},
};

#[derive(Debug, PartialEq, Eq)]
pub struct CargoWorkspace {
    pub root_path: PathBuf,
    pub cargo_packages: Vec<CargoPackage>,
}

/// A single Cargo package within a workspace. Each ex
#[derive(Debug, PartialEq, Eq)]
pub struct CargoPackage {
    pub root_path: PathBuf,

    // There might be multiple test definitions
    pub test_definitions: Vec<TestDefinition>,
}

impl CargoPackage {
    pub fn pretty_root_path(root_path: &Path) -> String {
        format!(
            "{}",
            root_path
                // Make path start with the top-level test directory
                .strip_prefix(root_path.parent().unwrap().parent().unwrap())
                .unwrap()
                .display()
        )
        .replace('\\', "/")
    }
}

/// A [TestDefinition] that has been successfully parsed from an `.rs` file. Each
/// TestDefinition corresponds to a single executable produced by compiling the
/// containing Cargo workspace.
#[derive(Debug, PartialEq, Eq)]
pub struct TestDefinition {
    /// The ID of the test, uniquely identifying the test definition in the entire session
    pub name: Arc<str>,

    /// Name of the executable (including .exe suffix on Windows)
    pub executable_name: OsString,

    /// Path to the source file containing the script.
    pub absolute_source_path: PathBuf,

    /// The debugger/check script of the test
    pub script: Script,

    /// Breakpoints created via #break annotations
    pub breakpoints: Vec<BreakPoint>,
}

impl TestDefinition {
    pub fn new(
        path_within_project: &Path,
        absolute_source_path: &Path,
        pretty_project_path: &str,
        executable_name: OsString,
        script: Script,
        breakpoints: Vec<BreakPoint>,
    ) -> TestDefinition {
        assert!(absolute_source_path.is_absolute());

        let mut name = String::with_capacity(
            pretty_project_path.len() + path_within_project.as_os_str().len() + 1,
        );
        name.push_str(pretty_project_path);
        for component in path_within_project.iter() {
            name.push('/');
            name.push_str(&component.to_string_lossy());
        }

        TestDefinition {
            name: name.into(),
            executable_name,
            absolute_source_path: absolute_source_path.to_path_buf(),
            script,
            breakpoints,
        }
    }

    pub fn flat_name(&self) -> String {
        self.name.replace(|c| c == '/' || c == '\\', "~")
    }

    pub fn matches(&self, regex: Option<&Regex>) -> bool {
        regex
            .map(|regex| regex.is_match(&self.name[..]))
            .unwrap_or(true)
    }
}

/// Finds and parses all [TestDefinitions]s in a Cargo package. Does not actually look
/// at the package's Cargo.toml, just assumes that `src/main.rs` and each file in `src/bin`
/// specifies a test case.
// TODO: also support tests specified in `tests` directory
fn analyze_cargo_package(project_directory: &Path) -> anyhow::Result<Vec<TestDefinition>> {
    info!("Analyzing Cargo package `{}`", project_directory.display());

    assert!(project_directory.is_dir());
    assert!(project_directory.exists());
    assert!(project_directory.is_absolute());

    let pretty_project_path = CargoPackage::pretty_root_path(project_directory);

    let cargo_toml_name = project_directory.join("Cargo.toml");

    if !cargo_toml_name.exists() {
        bail!("No Cargo.toml in {}", project_directory.display());
    }

    let mut test_defs = vec![];

    let src_directory = project_directory.join("src");

    let mut collect_test_def =
        |source_path: PathBuf, executable_name: OsString| -> anyhow::Result<()> {
            if source_path.exists() {
                let contents = std::fs::read_to_string(&source_path)?;
                let script = parse_script(&contents, Some(source_path.as_path()))?;
                let breakpoints = breakpoints::find(&contents);

                let test_definition = TestDefinition::new(
                    source_path.strip_prefix(project_directory)?,
                    source_path.as_path(),
                    &pretty_project_path,
                    executable_name,
                    script,
                    breakpoints,
                );

                info!(" - Found test case `{}`", test_definition.name);

                test_defs.push(test_definition);
            }

            Ok(())
        };

    // Take care of main.rs
    collect_test_def(
        src_directory.join("main.rs"),
        executable_name(project_directory.file_name().unwrap()),
    )?;

    // Collect executables from the src/bin directory
    let bin_directory = src_directory.join("bin");
    if bin_directory.exists() {
        let rs_ext = OsString::from("rs");

        for dir_entry in std::fs::read_dir(&bin_directory)? {
            let dir_entry = dir_entry?;
            let source_path = dir_entry.path();

            if source_path.extension() == Some(rs_ext.as_os_str()) {
                let executable_name =
                    executable_name(PathBuf::from(dir_entry.file_name()).with_extension(""));

                collect_test_def(source_path, executable_name)?;
            }
        }
    }

    test_defs.sort_by_cached_key(|td| td.executable_name.clone());

    Ok(test_defs)
}

impl CargoWorkspace {
    pub fn load(directory: &Path) -> anyhow::Result<CargoWorkspace> {
        info!("Loading Cargo workspace `{}`", directory.display());

        let directory = directory.canonicalize()?;

        let (mut members, exclude) = {
            let toml_text = std::fs::read_to_string(directory.join("Cargo.toml"))?;
            let mut ws_toml = toml::from_str::<WorkspaceToml>(&toml_text)?;

            // Always exclude `target` directory
            ws_toml.workspace.exclude.insert("target".to_string());

            (ws_toml.workspace.members, ws_toml.workspace.exclude)
        };

        // Read all the test files
        let mut files = Vec::new();
        for dir_entry in std::fs::read_dir(&directory)? {
            let dir_entry = dir_entry?;

            if dir_entry.file_type()?.is_dir() {
                let test_directory = dir_entry.path();

                let directory_name = test_directory.file_name().unwrap().to_string_lossy();

                if exclude.contains(&directory_name[..]) {
                    // Skip this directory if it is in the "exlude" list of the workspace Cargo.toml
                    continue;
                }

                let mut cargo_toml_path = test_directory.clone();
                cargo_toml_path.push("Cargo.toml");

                if cargo_toml_path.exists() {
                    info!(" - Found Cargo package `{}`", test_directory.display());
                    members.remove(&directory_name[..]);
                    files.push(test_directory);
                } else {
                    warn!(" - {} has no Cargo.toml", test_directory.display());
                }
            }
        }

        for member_not_found in members.iter().filter(|s| !s.contains('*')) {
            warn!(
                " - `{}` is listed as workspace member but the package could not be found",
                member_not_found
            )
        }

        files.sort();

        let mut test_project_defs = Vec::with_capacity(files.len());

        for cargo_project_directory in files {
            let test_definitions = analyze_cargo_package(&cargo_project_directory)?;
            test_project_defs.push(CargoPackage {
                root_path: cargo_project_directory,
                test_definitions,
            });
        }

        Ok(CargoWorkspace {
            root_path: directory,
            cargo_packages: test_project_defs,
        })
    }
}

fn executable_name(name: impl Into<OsString>) -> OsString {
    let mut name: OsString = name.into();

    if cfg!(target_os = "windows") {
        name.push(".exe");
    }

    name
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
struct WorkspaceToml {
    workspace: WorkspaceTomlInner,
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
struct WorkspaceTomlInner {
    #[serde(default)]
    members: HashSet<String>,
    #[serde(default)]
    exclude: HashSet<String>,
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::cargo_test_directory::WorkspaceTomlInner;

    use super::WorkspaceToml;

    fn hashset(items: &[&str]) -> HashSet<String> {
        items.into_iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn workspace_toml() {
        assert_eq!(
            toml::from_str::<WorkspaceToml>(
                r#"
            [workspace]
            members = ["foo", "bar"]
            exclude = ["target", "temp"]
            "#
            )
            .unwrap(),
            WorkspaceToml {
                workspace: WorkspaceTomlInner {
                    members: hashset(&["foo", "bar"]),
                    exclude: hashset(&["target", "temp"]),
                }
            }
        );
    }

    #[test]
    fn workspace_toml_empty() {
        assert_eq!(
            toml::from_str::<WorkspaceToml>(
                r#"
            [workspace]
            "#
            )
            .unwrap(),
            WorkspaceToml {
                workspace: WorkspaceTomlInner {
                    members: hashset(&[]),
                    exclude: hashset(&[]),
                }
            }
        );
    }
}
