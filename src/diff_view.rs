#[derive(Debug, Clone, PartialEq)]
pub struct ParsedFileDiff {
    pub path: String,
    pub additions: usize,
    pub deletions: usize,
    pub rows: Vec<DiffRow>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DiffRow {
    HunkHeader(HunkHeaderRow),
    Line(DiffLineRow),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HunkHeaderRow {
    pub raw: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiffLineKind {
    Context,
    Added,
    Removed,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DiffLineRow {
    pub kind: DiffLineKind,
    pub old_lineno: Option<u32>,
    pub new_lineno: Option<u32>,
    pub text: String,
}

fn parse_hunk_header(line: &str) -> Option<(u32, u32)> {
    let after_at = line.strip_prefix("@@")?;
    let end = after_at.find("@@")?;
    let middle = &after_at[..end].trim();

    let parts: Vec<&str> = middle.split_whitespace().collect();
    let mut old_start = 1u32;
    let mut new_start = 1u32;

    for part in parts {
        if let Some(rest) = part.strip_prefix('-') {
            if let Some((s, _)) = rest.split_once(',') {
                old_start = s.parse().unwrap_or(1);
            } else {
                old_start = rest.parse().unwrap_or(1);
            }
        } else if let Some(rest) = part.strip_prefix('+') {
            if let Some((s, _)) = rest.split_once(',') {
                new_start = s.parse().unwrap_or(1);
            } else {
                new_start = rest.parse().unwrap_or(1);
            }
        }
    }

    Some((old_start, new_start))
}

pub fn parse_unified_diff(path: String, diff: &str) -> ParsedFileDiff {
    let mut additions = 0usize;
    let mut deletions = 0usize;
    let mut rows = Vec::new();
    let mut old_ln: Option<u32> = None;
    let mut new_ln: Option<u32> = None;

    for raw in diff.lines() {
        if raw.starts_with("@@") {
            if let Some((old_start, new_start)) = parse_hunk_header(raw) {
                old_ln = Some(old_start);
                new_ln = Some(new_start);
            }
            rows.push(DiffRow::HunkHeader(HunkHeaderRow {
                raw: raw.to_string(),
            }));
            continue;
        }

        if raw.starts_with("diff --git ")
            || raw.starts_with("index ")
            || raw.starts_with("--- ")
            || raw.starts_with("+++ ")
        {
            continue;
        }

        if raw.starts_with("\\ No newline") {
            continue;
        }

        if old_ln.is_none() && new_ln.is_none() {
            continue;
        }

        let prefix = raw.as_bytes().first().copied().unwrap_or(b' ');
        let text = if raw.len() > 1 { &raw[1..] } else { "" };

        match prefix {
            b' ' => {
                rows.push(DiffRow::Line(DiffLineRow {
                    kind: DiffLineKind::Context,
                    old_lineno: old_ln,
                    new_lineno: new_ln,
                    text: text.to_string(),
                }));
                old_ln = old_ln.map(|n| n + 1);
                new_ln = new_ln.map(|n| n + 1);
            }
            b'+' => {
                additions += 1;
                rows.push(DiffRow::Line(DiffLineRow {
                    kind: DiffLineKind::Added,
                    old_lineno: None,
                    new_lineno: new_ln,
                    text: text.to_string(),
                }));
                new_ln = new_ln.map(|n| n + 1);
            }
            b'-' => {
                deletions += 1;
                rows.push(DiffRow::Line(DiffLineRow {
                    kind: DiffLineKind::Removed,
                    old_lineno: old_ln,
                    new_lineno: None,
                    text: text.to_string(),
                }));
                old_ln = old_ln.map(|n| n + 1);
            }
            _ => {
                rows.push(DiffRow::Line(DiffLineRow {
                    kind: DiffLineKind::Context,
                    old_lineno: old_ln,
                    new_lineno: new_ln,
                    text: raw.to_string(),
                }));
                old_ln = old_ln.map(|n| n + 1);
                new_ln = new_ln.map(|n| n + 1);
            }
        }
    }

    ParsedFileDiff {
        path,
        additions,
        deletions,
        rows,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_addition() {
        let diff = "@@ -1,3 +1,4 @@\n context\n+added\n context2\n context3\n";
        let parsed = parse_unified_diff("test.rs".into(), diff);
        assert_eq!(parsed.additions, 1);
        assert_eq!(parsed.deletions, 0);
        assert_eq!(parsed.rows.len(), 5);
    }

    #[test]
    fn parse_mixed_changes() {
        let diff = "@@ -1 +1 @@\n-old line\n+new line\n";
        let parsed = parse_unified_diff("test.rs".into(), diff);
        assert_eq!(parsed.additions, 1);
        assert_eq!(parsed.deletions, 1);
        assert_eq!(parsed.rows.len(), 3);

        if let DiffRow::Line(ref line) = parsed.rows[1] {
            assert_eq!(line.kind, DiffLineKind::Removed);
            assert_eq!(line.old_lineno, Some(1));
            assert_eq!(line.new_lineno, None);
            assert_eq!(line.text, "old line");
        } else {
            panic!("expected line row");
        }

        if let DiffRow::Line(ref line) = parsed.rows[2] {
            assert_eq!(line.kind, DiffLineKind::Added);
            assert_eq!(line.old_lineno, None);
            assert_eq!(line.new_lineno, Some(1));
            assert_eq!(line.text, "new line");
        } else {
            panic!("expected line row");
        }
    }
}
