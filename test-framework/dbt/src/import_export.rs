use std::{
    fs::File,
    path::{Path, PathBuf},
};

use flate2::{read::GzDecoder, write::GzEncoder, Compression};
use log::debug;
use tar;

pub fn import_crashdumps(output_dir: &Path, tar_path: &Path) -> anyhow::Result<()> {
    debug!("Importing crashdumps from `{}`", tar_path.display());
    let file = File::open(tar_path)?;
    let reader = GzDecoder::new(file);
    let mut archive = tar::Archive::new(reader);
    archive.unpack(output_dir)?;
    Ok(())
}

// Paths are absolute
#[derive(Debug)]
pub struct GeneratedCrashDump {
    pub crashdump_path: PathBuf,
    pub debuggee_path: PathBuf,
    pub extra_symbols: Option<PathBuf>,
}

impl GeneratedCrashDump {
    pub fn new(
        crashdump_path: PathBuf,
        debuggee_path: PathBuf,
        extra_symbols: Option<PathBuf>,
    ) -> anyhow::Result<GeneratedCrashDump> {
        assert!(crashdump_path.is_absolute());
        assert_eq!(crashdump_path, crashdump_path.canonicalize()?);

        assert!(debuggee_path.is_absolute());
        assert_eq!(debuggee_path, debuggee_path.canonicalize()?);

        if let Some(extra_symbols) = extra_symbols.as_ref() {
            assert!(extra_symbols.is_absolute());
            assert_eq!(extra_symbols, extra_symbols.canonicalize()?.as_path());
        }

        Ok(GeneratedCrashDump {
            crashdump_path,
            debuggee_path,
            extra_symbols,
        })
    }
}

pub struct CrashDumpExporter {
    root_dir: PathBuf,
    tar_builder: tar::Builder<GzEncoder<File>>,
}

impl CrashDumpExporter {
    pub fn new(root_dir: PathBuf, tar_file_path: &Path) -> anyhow::Result<CrashDumpExporter> {
        assert_eq!(root_dir, root_dir.canonicalize()?);

        let file = File::create(tar_file_path)?;
        let writer = GzEncoder::new(file, Compression::default());
        let tar_builder = tar::Builder::new(writer);

        Ok(CrashDumpExporter {
            root_dir,
            tar_builder,
        })
    }

    pub fn add_crashdump(&mut self, crashdump: GeneratedCrashDump) -> anyhow::Result<()> {
        let tar_paths = to_tar_paths(self.root_dir.as_path(), &crashdump)?;

        for (path_on_disk, path_in_tar) in tar_paths {
            self.tar_builder
                .append_path_with_name(path_on_disk, path_in_tar)?;
        }

        Ok(())
    }
}

fn to_tar_paths<'a>(
    root_dir: &Path,
    crashdump: &'a GeneratedCrashDump,
) -> anyhow::Result<Vec<(&'a Path, PathBuf)>> {
    let mut tar_paths = vec![];

    let crashdump_relative_path = crashdump.crashdump_path.strip_prefix(root_dir)?;

    tar_paths.push((
        crashdump.crashdump_path.as_path(),
        crashdump_relative_path.to_path_buf(),
    ));

    for file_from_target_dir in
        std::iter::once(crashdump.debuggee_path.as_path()).chain(crashdump.extra_symbols.as_deref())
    {
        anyhow::ensure!(file_from_target_dir.file_name().is_some());
        let relative_path =
            crashdump_relative_path.with_file_name(file_from_target_dir.file_name().unwrap());

        tar_paths.push((file_from_target_dir, relative_path));
    }

    Ok(tar_paths)
}

#[cfg(test)]
mod tests {
    use super::GeneratedCrashDump;
    use std::path::Path;

    #[test]
    fn to_tar_paths() {
        let root = Path::new("/xyz");

        let crashdump = GeneratedCrashDump {
            crashdump_path: root.join("crashdumps/dump.dmp"),
            debuggee_path: root.join("build/debug/foo.exe"),
            extra_symbols: Some(root.join("build/debug/foo.pdb")),
        };

        let tar_paths = super::to_tar_paths(root, &crashdump).unwrap();

        assert_eq!(
            tar_paths,
            vec![
                (
                    crashdump.crashdump_path.as_path(),
                    "crashdumps/dump.dmp".into(),
                ),
                (
                    crashdump.debuggee_path.as_path(),
                    "crashdumps/foo.exe".into(),
                ),
                (
                    crashdump.extra_symbols.as_ref().unwrap().as_path(),
                    "crashdumps/foo.pdb".into(),
                ),
            ]
        )
    }
}
