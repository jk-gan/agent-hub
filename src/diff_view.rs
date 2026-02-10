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

fn split_diff_header_tokens(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    let mut in_quotes = false;
    let mut escaped = false;

    for ch in input.chars() {
        if in_quotes {
            if escaped {
                current.push(ch);
                escaped = false;
                continue;
            }

            match ch {
                '\\' => escaped = true,
                '"' => in_quotes = false,
                _ => current.push(ch),
            }
            continue;
        }

        match ch {
            '"' => in_quotes = true,
            c if c.is_whitespace() => {
                if !current.is_empty() {
                    tokens.push(std::mem::take(&mut current));
                }
            }
            _ => current.push(ch),
        }
    }

    if !current.is_empty() {
        tokens.push(current);
    }

    tokens
}

fn normalize_diff_path(raw: &str) -> String {
    let path = raw.trim();
    path.strip_prefix("a/")
        .or_else(|| path.strip_prefix("b/"))
        .unwrap_or(path)
        .to_string()
}

fn parse_path_from_diff_header(line: &str) -> Option<String> {
    let rest = line.strip_prefix("diff --git ")?;
    let tokens = split_diff_header_tokens(rest);
    let raw_path = tokens.get(1).or_else(|| tokens.first())?;
    let path = normalize_diff_path(raw_path);
    if path.trim().is_empty() {
        return None;
    }
    Some(path)
}

fn parse_path_from_plus_plus_plus(line: &str) -> Option<String> {
    let rest = line.strip_prefix("+++ ")?;
    if rest.trim() == "/dev/null" {
        return None;
    }

    let trimmed = rest.trim();
    let raw = if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2 {
        &trimmed[1..trimmed.len() - 1]
    } else {
        trimmed
    };
    let path = normalize_diff_path(raw);
    if path.trim().is_empty() {
        return None;
    }
    Some(path)
}

pub fn parse_multi_file_unified_diff(diff: &str) -> Vec<ParsedFileDiff> {
    let mut chunks: Vec<(Option<String>, Vec<String>)> = Vec::new();

    for line in diff.lines() {
        if line.starts_with("diff --git ") {
            chunks.push((parse_path_from_diff_header(line), vec![line.to_string()]));
            continue;
        }

        if let Some((_path, lines)) = chunks.last_mut() {
            lines.push(line.to_string());
        }
    }

    chunks
        .into_iter()
        .filter_map(|(path, lines)| {
            let path = path.or_else(|| {
                lines
                    .iter()
                    .find_map(|line| parse_path_from_plus_plus_plus(line))
            })?;
            let chunk_text = lines.join("\n");
            Some(parse_unified_diff(path, &chunk_text))
        })
        .collect()
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

    #[test]
    fn parse_multi_file_unified_diff_parses_each_file() {
        let diff = "\
diff --git a/src/lib.rs b/src/lib.rs
index 1111111..2222222 100644
--- a/src/lib.rs
+++ b/src/lib.rs
@@ -1 +1 @@
-fn old() {}
+fn new() {}
diff --git a/README.md b/README.md
index 3333333..4444444 100644
--- a/README.md
+++ b/README.md
@@ -1 +1,2 @@
 # Title
+More
";

        let parsed = parse_multi_file_unified_diff(diff);
        assert_eq!(parsed.len(), 2);
        assert_eq!(parsed[0].path, "src/lib.rs");
        assert_eq!(parsed[0].additions, 1);
        assert_eq!(parsed[0].deletions, 1);
        assert_eq!(parsed[1].path, "README.md");
        assert_eq!(parsed[1].additions, 1);
        assert_eq!(parsed[1].deletions, 0);
    }

    #[test]
    fn parse_multi_file_unified_diff_handles_quoted_paths() {
        let diff = "\
diff --git \"a/docs/with space.md\" \"b/docs/with space.md\"
index 3333333..4444444 100644
--- \"a/docs/with space.md\"
+++ \"b/docs/with space.md\"
@@ -1 +1 @@
-old
+new
";

        let parsed = parse_multi_file_unified_diff(diff);
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].path, "docs/with space.md");
        assert_eq!(parsed[0].additions, 1);
        assert_eq!(parsed[0].deletions, 1);
    }
}
