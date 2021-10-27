use std::{
    ffi::OsString,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::bail;
use log::info;

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
        .replace("\\", "/")
    }
}

/// A [TestDefinition] that has been successfully parsed from an `.rs` file. Each
/// TestDefinition corresponds to a single executable produced by compiling the
/// containing Cargo workspace.
#[derive(Debug, PartialEq, Eq)]
pub struct TestDefinition {
    /// The ID of the test, uniquely identifying the test definition in the entire session
    pub name: Arc<str>,

    /// Name of the executable
    pub executable_name: OsString,

    /// The debugger/check script of the test
    pub script: Script,

    /// Breakpoints created via #break annotations
    pub breakpoints: Vec<BreakPoint>,
}

impl TestDefinition {
    pub fn new(
        path_within_project: &Path,
        pretty_project_path: &str,
        executable_name: OsString,
        script: Script,
        breakpoints: Vec<BreakPoint>,
    ) -> TestDefinition {
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
            script,
            breakpoints,
        }
    }

    pub fn flat_name(&self) -> String {
        self.name.replace(|c| c == '/' || c == '\\', "_")
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
                let script = parse_script(&contents)?;
                let breakpoints = breakpoints::find(&contents);

                let test_definition = TestDefinition::new(
                    source_path.strip_prefix(&project_directory)?,
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

        // Read all the test files
        let mut files = Vec::new();
        for dir_entry in std::fs::read_dir(&directory)? {
            let dir_entry = dir_entry?;

            if dir_entry.file_type()?.is_dir() {
                let test_directory = dir_entry.path();
                let mut cargo_toml_path = test_directory.clone();
                cargo_toml_path.push("Cargo.toml");

                if cargo_toml_path.exists() {
                    info!(" - Found Cargo package `{}`", test_directory.display());
                    files.push(test_directory);
                } else {
                    bail!("{} has no Cargo.toml", test_directory.display());
                }
            }
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
