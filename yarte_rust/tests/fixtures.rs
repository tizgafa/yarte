use std::fs::read_to_string;

use glob::glob;
use serde::Deserialize;

use yarte_rust::lexer::token_stream;
use yarte_rust::sink::{SResult, Sink, State};
use yarte_rust::token_types::{Delimiter, Ident, Literal, Punct};
use yarte_strnom::Cursor;

#[derive(Deserialize, Clone, PartialEq, Eq, Debug)]
pub enum Token<'a> {
    OpenGroup(Delimiter),
    CloseGroup(Delimiter),
    #[serde(borrow)]
    Ident(Ident<'a>),
    Punct(Punct),
    #[serde(borrow)]
    Literal(Literal<'a>),
}

#[derive(Debug)]
struct VecSink<'a>(Vec<Token<'a>>);

impl<'a> Sink<'a> for VecSink<'a> {
    fn open_group(&mut self, del: Delimiter) -> SResult {
        self.0.push(Token::OpenGroup(del));
        Ok(State::Continue)
    }
    fn close_group(&mut self, del: Delimiter) -> SResult {
        self.0.push(Token::CloseGroup(del));
        Ok(State::Continue)
    }
    fn ident(&mut self, ident: Ident<'a>) -> SResult {
        self.0.push(Token::Ident(ident));
        Ok(State::Continue)
    }
    fn punct(&mut self, punct: Punct) -> SResult {
        self.0.push(Token::Punct(punct));
        Ok(State::Continue)
    }
    fn literal(&mut self, literal: Literal<'a>) -> SResult {
        self.0.push(Token::Literal(literal));
        Ok(State::Continue)
    }
    fn end(&mut self) -> SResult {
        Ok(State::Continue)
    }
}

#[derive(Debug, Deserialize)]
struct Fixture<'a> {
    #[serde(borrow)]
    src: &'a str,
    #[serde(borrow)]
    exp: Vec<Token<'a>>,
}

macro_rules! features {
    ($name:ident: $path:literal) => {
        #[test]
        fn $name() {
            for entry in glob($path).expect("Failed to read glob pattern") {
                let name = entry.expect("File name");
                eprintln!("\n{:?}\n", name);
                let src = read_to_string(name).expect("Valid file");
                let fixtures: Vec<Fixture> =
                    ron::from_str(&src).expect("Valid Fixtures");

                for (i, Fixture { src, exp }) in fixtures.into_iter().enumerate() {
                    let cur = Cursor{ rest: src, off: 0 };
                    let mut vs = VecSink(Vec::new());
                    let _ = token_stream(cur, &mut vs).unwrap();
                    eprintln!("{:2}:\nBASE {:?} \nEXPR {:?}", i, exp, vs.0);
                    assert_eq!(vs.0, exp);
                }
            }
        }
    };
    ($name:ident: $path:literal, $($t:tt)*) => {
        features!($name: $path);
        features!($($t)*);
    };
    () => {}
}

features!(
    test_literals: "./tests/fixtures/literals/**/*.ron",
    test_expr: "./tests/fixtures/expr/**/*.ron"
);
