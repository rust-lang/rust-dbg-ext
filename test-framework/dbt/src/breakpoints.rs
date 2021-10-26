#[derive(Debug, PartialEq, Eq, Hash)]
pub struct BreakPoint {
    pub line_index: usize,
}

/// Return 0-based line numbers of all lines containing the string "#break"
pub fn find(file_contents: &str) -> Vec<BreakPoint> {
    file_contents
        .lines()
        .enumerate()
        .filter_map(|(line_index, line)| {
            if line.contains("#break") {
                Some(BreakPoint { line_index })
            } else {
                None
            }
        })
        .collect()
}
