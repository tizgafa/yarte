//! Adapted from [`proc-macro2`](https://github.com/alexcrichton/proc-macro2).
use std::borrow::{Borrow, BorrowMut};
use std::fmt::{self, Debug};
use std::path::{Path, PathBuf};

use parking_lot::RwLock;

use crate::error::{KiError, Result};
use crate::strnom::Cursor;
static SOURCE_MAP: RwLock<SourceMap> = RwLock::new(new_sm());

/// Add file to source map and return lower bound
///
/// Use in the same thread
pub fn get_cursor<'a>(p: &Path, rest: &'a str) -> Cursor<'a> {
    let mut x = SOURCE_MAP.write();
    Cursor {
        rest,
        off: x.borrow_mut().add_file(p, rest).lo,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
struct FileInfo {
    name: PathBuf,
    span: Span,
    lines: Vec<usize>,
}

impl FileInfo {
    fn offset_line_column(&self, offset: usize) -> LineColumn {
        assert!(self.span_within(Span {
            lo: offset as u32,
            hi: offset as u32,
        }));
        let offset = offset - self.span.lo as usize;
        match self.lines.binary_search(&offset) {
            Ok(found) => LineColumn {
                line: found + 1,
                column: 0,
            },
            Err(idx) => LineColumn {
                line: idx,
                column: offset - self.lines[idx - 1],
            },
        }
    }

    fn get_ranges(&self, span: Span) -> ((usize, usize), (usize, usize)) {
        assert!(self.span_within(span));
        let lo = (span.lo - self.span.lo) as usize;
        let hi = (span.hi - self.span.lo) as usize;
        let lo_line = match self.lines.binary_search(&lo) {
            Ok(_) => lo,
            Err(idx) => self.lines[idx - 1],
        };
        let hi_line = match self.lines.binary_search(&hi) {
            Ok(_) => hi,
            Err(idx) => self
                .lines
                .get(idx)
                .copied()
                .unwrap_or((self.span.hi - self.span.lo) as usize),
        };
        ((lo_line, hi_line), (lo - lo_line, hi - lo_line))
    }

    fn span_within(&self, span: Span) -> bool {
        span.lo >= self.span.lo && span.hi <= self.span.hi
    }
}

/// Computes the offsets of each line in the given source string.
fn lines_offsets(s: &str) -> Vec<usize> {
    let mut lines = vec![0];
    let mut prev = 0;
    while let Some(len) = s.bytes().skip(prev).position(|x| x == b'\n') {
        prev += len + 1;
        lines.push(prev);
    }
    lines
}

struct SourceMap {
    files: Vec<FileInfo>,
}

const fn new_sm() -> SourceMap {
    SourceMap { files: vec![] }
}

impl SourceMap {
    fn next_start_pos(&self) -> u32 {
        // Add 1 so there's always space between files.
        self.files.last().map(|f| f.span.hi + 1).unwrap_or(0)
    }

    fn add_file(&mut self, name: &Path, src: &str) -> Span {
        assert!(
            !self.files.iter().any(|x| x.name == name),
            "File is already registered"
        );
        let lines = lines_offsets(src);
        let lo = self.next_start_pos();
        let span = Span {
            lo,
            hi: lo + (src.len() as u32),
        };

        self.files.push(FileInfo {
            name: name.to_owned(),
            span,
            lines,
        });

        span
    }

    fn file_info(&self, span: Span) -> &FileInfo {
        for file in &self.files {
            if file.span_within(span) {
                return file;
            }
        }
        panic!("Invalid span with no related FileInfo!");
    }
}

#[derive(Clone, Copy, PartialEq, Eq, serde::Deserialize)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

// Don't allow `Span` to transfer between thread
// impl !Send for Span {}
// impl !Sync for Span {}

impl Span {
    pub const fn new(lo: u32, hi: u32) -> Self {
        Span { lo, hi }
    }

    /// Assume a <= b
    #[inline]
    pub fn from_cursor(a: Cursor, b: Cursor) -> Self {
        debug_assert!(a.off <= b.off);
        Self {
            lo: a.off,
            hi: b.off,
        }
    }

    pub fn from_len(i: Cursor, len: usize) -> Self {
        Self {
            lo: i.off,
            hi: i.off + (len as u32),
        }
    }

    pub fn from_range(i: Cursor, (lo, hi): (usize, usize)) -> Self {
        Self {
            lo: i.off + (lo as u32),
            hi: i.off + (hi as u32),
        }
    }

    pub fn join_proc(self, proc: proc_macro2::Span) -> Self {
        let start = self.start();
        let p_start = proc.start();
        let p_end = proc.end();
        let lo = if p_start.line == 1 {
            self.lo + p_start.column as u32
        } else {
            let cm = SOURCE_MAP.read();
            let cm = cm.borrow();
            let fi = cm.file_info(self);
            fi.lines[start.line + p_start.line - 2] as u32 + p_start.column as u32
        };
        let hi = if p_end.line == 1 {
            self.lo + p_end.column as u32
        } else {
            let cm = SOURCE_MAP.read();
            let cm = cm.borrow();
            let fi = cm.file_info(self);
            fi.lines[start.line + p_end.line - 2] as u32 + p_end.column as u32
        };

        Self { lo, hi }
    }

    /// Returns line bounds and range in bounds
    pub fn range_in_file(self) -> ((usize, usize), (usize, usize)) {
        let cm = SOURCE_MAP.read();
        let cm = cm.borrow();
        let fi = cm.file_info(self);
        fi.get_ranges(self)
    }

    pub fn file_path(self) -> PathBuf {
        let cm = SOURCE_MAP.read();
        let cm = cm.borrow();
        let fi = cm.file_info(self);
        fi.name.clone()
    }

    pub fn start(self) -> LineColumn {
        let cm = SOURCE_MAP.read();
        let cm = cm.borrow();
        let fi = cm.file_info(self);
        fi.offset_line_column(self.lo as usize)
    }
}

impl<'a> From<Cursor<'a>> for Span {
    fn from(c: Cursor) -> Self {
        Self::from_cursor(c, c)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "bytes({}..{})", self.lo, self.hi)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, serde::Deserialize)]
pub struct S<T: Debug + PartialEq + Clone>(pub T, pub Span);

impl<T: Debug + PartialEq + Clone> S<T> {
    pub fn t(&self) -> &T {
        &self.0
    }
    pub fn span(&self) -> Span {
        self.1
    }
}

pub fn spanned<'a, T: Debug + PartialEq + Clone, E: KiError>(
    input: Cursor<'a>,
    f: fn(Cursor<'a>) -> Result<'a, T, E>,
) -> Result<'a, S<T>, E> {
    let lo = input.off;
    let (a, b) = f(input)?;
    let hi = a.off;
    let span = Span { lo, hi };
    Ok((a, S(b, span)))
}
