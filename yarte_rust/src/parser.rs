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

fn eat_call_expr(group: &Vec<Token>) -> Result<Expr,()> {
    // name(arg1, arg2,...)
    if group.len() < 5 {
        return Err(())
    }
    let mut call_expr = ExprCall{ // TODO: Add default to all Expr???
        args: Vec::new(),
        func: "".to_string(),
    };
    if let Token::Ident(i) =  &group[0] {
        call_expr.func = i.inner.to_string();
    }else{
        return Err(())
    }
    match group[1] {
        Token::OpenGroup(Delimiter::Parenthesis) => (),
        _ => return Err(())
    }
    match group[group.len()-1] {
        Token::CloseGroup(Delimiter::Parenthesis) => (),
        _ => return Err(())
    }
    // Check pattern: expr1 punc.ch=="," .....
    if group.len() > 4 { // TODO: Remove this control is already passed by first `if` 
        for x in (2..(group.len()-3)).step_by(2) {
            // TODO: Change Ident for expression (try_eat_expr)
            if let Token::Ident(i) = &group[x] {
                call_expr.args.push(Expr::Variable(ExprVariable { inner: i.inner.to_string() })) 
            } else {
                return Err(())
            }
            // Checl Expr is followed by `,` 
            if let Token::Punct(p) = &group[x+1] {
                match p.ch {
                    ',' => (),
                    _ => return Err(())
                }
            } else {
                return Err(())
            }
        }
    }
    
    // Check last arg
    // TODO: Change Ident for expression (try_eat_expr)
    if let Token::Ident(i) = &group[group.len()-2] {
       call_expr.args.push(Expr::Variable(ExprVariable { inner: i.inner.to_string() })) 
    } else {
        return Err(())
    }
    Ok(Expr::Call(call_expr))
}

fn try_eat_expr(group: &Vec<Token>) -> Result<Expr,()> {
    if let Ok(expr) =  eat_call_expr(group){
        return Ok(expr)
    } else {
        return Err(())
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
        Ok(State::Stop)
    }
    fn literal(&mut self, literal: Literal<'a>) -> SResult {
        println!("{:?}",literal);
        unimplemented!();
    }
    fn end(&mut self) -> SResult {
        if let Ok(e) = try_eat_expr(&self.stack_token) {
            self.stack.push(e);
            self.stack_token = Vec::new();
        }
        Ok(State::Stop)
    }
}

#[cfg(test)]
mod tests {
    // use crate::expr::*;
    use super::*;
    use yarte_strnom::Cursor;

    #[test]
    fn function() {
        let cursor = Cursor {
            rest: "call(a,b)",
            off: 0
        };
        let mut func_res = Vec::new();
        func_res.push(Expr::Call(ExprCall{
            args: vec![
                Expr::Variable(ExprVariable{inner: "a".to_string()}),
                Expr::Variable(ExprVariable{inner: "b".to_string()})
            ],
            func: "call".to_string()
        }));
        let res = parse(cursor).unwrap();
        for (i, r) in res.iter().enumerate() {
            assert_eq!(r, &func_res[i])
        }
    }
}