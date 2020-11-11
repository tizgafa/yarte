use std::fmt::Debug;

use crate::error::{ErrorMessage, KiError, LexError, PResult};
use crate::source_map::{Span, S};
use crate::strnom::{get_chars, is_ws, Cursor};
use crate::{Kinder, SToken, Token};

pub trait Ki<'a>: Kinder<'a> + Debug + PartialEq + Clone {}

impl<'a, T: Kinder<'a> + Debug + PartialEq + Clone> Ki<'a> for T {}

pub fn parse<'a, K: Ki<'a>>(i: Cursor<'a>) -> Result<Vec<SToken<'a, K>>, ErrorMessage<K::Error>> {
    let (c, res) = eat(i)?;
    if c.is_empty() {
        Ok(res)
    } else {
        Err(ErrorMessage {
            message: K::Error::UNCOMPLETED,
            span: Span::from_len(c, 1),
        })
    }
}

macro_rules! comment {
    ($K:ty, $cur:expr, $i:ident, $at:ident, $j:ident, $nodes:ident) => {
        match <$K>::comment($cur) {
            Ok((c, s)) => {
                eat_lit($i, $at + $j, &mut $nodes);
                $nodes.push(S(Token::Comment(s), Span::from_cursor($i.adv($at + $j), c)));
                $i = c;
                $at = 0;
                continue;
            }
            Err(LexError::Next(..)) => (),
            Err(e) => break Err(e),
        }
    };
}

fn eat<'a, K: Ki<'a>>(mut i: Cursor<'a>) -> PResult<Vec<SToken<'a, K>>, K::Error> {
    let mut nodes = vec![];
    let mut at = 0;
    loop {
        if let Some(j) = i.adv_find(at, K::OPEN) {
            let cur = i.adv(j + at);
            let next = cur.chars().next();
            let cur = cur.adv(2);
            if let Some(next) = next {
                if K::OPEN_BLOCK == K::OPEN_EXPR && next == K::OPEN_EXPR {
                    comment!(K, cur, i, at, j, nodes);
                } else if next == K::OPEN_EXPR {
                    unimplemented!()
                } else if next == K::OPEN_BLOCK {
                    comment!(K, cur, i, at, j, nodes);
                } else {
                    at += j + 1;
                }
            } else {
                at += j + 1;
            }
        } else {
            eat_lit(i, i.len(), &mut nodes);
            break Ok((i.adv(i.len()), nodes));
        }
    }
}

/// Push literal at cursor with length
fn eat_lit<'a, K: Ki<'a>>(i: Cursor<'a>, len: usize, nodes: &mut Vec<SToken<'a, K>>) {
    let lit = get_chars(i.rest, 0, len);
    if !lit.is_empty() {
        let (l, lit, r) = trim(lit);
        let ins = Span {
            lo: i.off + (l.len() as u32),
            hi: i.off + ((len - r.len()) as u32),
        };
        let out = Span {
            lo: i.off,
            hi: i.off + (len as u32),
        };
        nodes.push(S(Token::Lit(l, S(lit, ins), r), out));
    }
}

#[inline]
pub fn close_block<'a, K: Ki<'a>>(i: Cursor<'a>) -> PResult<(), K::Error> {
    if i.next_is(K::CLOSE_BLOCK) {
        if i.adv_next_is(1, K::CLOSE) {
            Ok((i.adv(2), ()))
        } else {
            Err(LexError::Next(K::Error::CLOSE_BLOCK, Span::from(i.adv(1))))
        }
    } else {
        Err(LexError::Next(K::Error::CLOSE_BLOCK, Span::from(i)))
    }
}

/// TODO: Define chars in path
/// Eat path at partial
/// Next white space close path
fn path<E: KiError>(i: Cursor) -> PResult<&str, E> {
    take_while!(i, |i| !is_ws(i)).and_then(|(c, s)| {
        if s.is_empty() {
            Err(LexError::Fail(E::PATH, Span::from(c)))
        } else {
            Ok((c, s))
        }
    })
}

pub fn trim(i: &str) -> (&str, &str, &str) {
    if i.is_empty() {
        return ("", "", "");
    }

    let b = i.as_bytes();

    if let Some(ln) = b.iter().position(|x| !is_ws((*x).into())) {
        let rn = b.iter().rposition(|x| !is_ws((*x).into())).unwrap();
        (
            safe_utf8(&b[..ln]),
            safe_utf8(&b[ln..=rn]),
            safe_utf8(&b[rn + 1..]),
        )
    } else {
        (i, "", "")
    }
}

/// Convert from bytes to str
/// Use when previous check bytes it's valid utf8
fn safe_utf8(s: &[u8]) -> &str {
    unsafe { ::std::str::from_utf8_unchecked(s) }
}
