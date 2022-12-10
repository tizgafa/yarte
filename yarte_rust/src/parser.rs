use serde::Deserialize;

use crate::token_types::*;
use crate::sink::{Sink, SResult, State};
use crate::expr::*;

use yarte_strnom::{
    Cursor,
    // LexError,
    // Span
};
use crate::lexer::token_stream;
use crate::eater::try_eat_expr;

#[derive(Deserialize, Clone, PartialEq, Eq, Debug)]
pub enum Token<'a> {
    OpenGroup(Delimiter),
    CloseGroup(Delimiter),
    #[serde(borrow)]
    Ident(Ident<'a>),
    Punct(Punct),
    #[serde(borrow)]
    Literal(Literal<'a>),
    Reserve(ReserveIdent),
}

pub fn parse<'a>(input: Cursor<'static>) -> Result<Vec<Expr>,()> {
    let mut sink = ParserSink::default();
    let _ = token_stream(input, &mut sink);
    Ok(sink.stack)
}

#[derive(Deserialize, Clone, PartialEq, Eq, Debug)]
pub enum ReserveIdent {
    Let,
    If,
    For,
    While,
    Loop,
    Const,
}

fn read_ident<'a>(ident: &'a str) -> Option<ReserveIdent> {
    match ident {
        "let" => Some(ReserveIdent::Let),
        "if" => Some(ReserveIdent::If),
        "for" => Some(ReserveIdent::For),
        "while" => Some(ReserveIdent::While),
        "loop" => Some(ReserveIdent::Loop),
        "const" => Some(ReserveIdent::Const),
        _ => None
    }
}

pub struct ParserSink<'a> {
    pub stack: Vec<Expr>,
    pub stack_token: Vec<Token<'a>>,
}

impl<'a> Default for ParserSink<'a> {
    fn default() -> Self {
        Self {
            stack: Vec::new(),
            stack_token: Vec::new(),
        }
    }
}

impl<'a> Sink<'a> for ParserSink<'a> {
    fn open_group(&mut self, del: Delimiter) -> SResult {
        self.stack_token.push(Token::OpenGroup(del));
        // if let Ok(e) = try_eat_expr(self.stack_token) {
        //     self.stack.push(e);
        // }
        Ok(State::Continue)
    }

    fn close_group(&mut self, del: Delimiter) -> SResult {

        if self.stack_token.len() == 0 {
            // If expression starts with close group is an error
            // TODO: Add LexError
            return Ok(State::Stop)
        }
        self.stack_token.push(Token::CloseGroup(del));
        
        if let Ok(e) = try_eat_expr(&self.stack_token) {
            self.stack.push(e);
            self.stack_token = Vec::new();
        }
        Ok(State::Continue)
    }

    fn ident(&mut self, ident: Ident<'a>) -> SResult {
        if let Some(i) = read_ident(ident.inner) {
            self.stack_token.push(Token::Reserve(i));
        } else {
            self.stack_token.push(Token::Ident(ident));
        }

        if let Ok(e) = try_eat_expr(&self.stack_token) {
            self.stack.push(e);
            self.stack_token = Vec::new();
        }
        Ok(State::Continue)
    }

    fn punct(&mut self, punct: Punct) -> SResult {
        if self.stack_token.len() == 0 {
            // TODO: Send error
            return Ok(State::Stop)
        }
        self.stack_token.push(Token::Punct(punct));
        Ok(State::Continue)
    }

    fn literal(&mut self, literal: Literal<'a>) -> SResult {
        self.stack_token.push(Token::Literal(literal));
        Ok(State::Continue)
    }

    fn end(&mut self) -> SResult {
        if let Ok(e) = try_eat_expr(&self.stack_token) {
            self.stack.push(e);
            self.stack_token = Vec::new();
        }
        Ok(State::Stop)
    }
}
