use std::{ffi::OsString, path::PathBuf};

use dbt::cargo_test_directory::*;
use dbt::script::Script;

fn executable_name(name: &str) -> OsString {
    let mut name: OsString = name.into();

    if cfg!(target_os = "windows") {
        name.push(".exe");
    }

    name
}

#[test]
fn find_cargo_test_directories() {
    let root_path = PathBuf::from("tests/cargo-test-discovery-sample");

    let cargo_test_directory = CargoWorkspace::load(&root_path).unwrap();
    let root_path = root_path.canonicalize().unwrap();

    assert_eq!(
        cargo_test_directory,
        CargoWorkspace {
            root_path: root_path.clone(),
            cargo_packages: vec![
                CargoPackage {
                    root_path: root_path.join("testcase1"),
                    test_definitions: vec![TestDefinition {
                        executable_name: executable_name("testcase1"),
                        name: "cargo-test-discovery-sample/testcase1/src/main.rs".into(),
                        script: Script::new_empty(),
                        breakpoints: vec![],
                    }]
                },
                CargoPackage {
                    root_path: root_path.join("testcase2"),
                    test_definitions: vec![TestDefinition {
                        executable_name: executable_name("some_exe"),
                        name: "cargo-test-discovery-sample/testcase2/src/bin/some_exe.rs".into(),
                        script: Script::new_empty(),
                        breakpoints: vec![],
                    }]
                }
            ],
        }
    );
}
