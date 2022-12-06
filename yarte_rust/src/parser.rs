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

macro_rules! read_until_comma_or_end {
    ($group:ident, $s: literal, $end_del: path, $array:  expr)=> {
        let mut start = $s;
        for mut end in start..$group.len() {
            match &$group[end] {
                Token::Punct(p) => {
                    match p.ch {
                        ',' => {
                            if end-start == 1 {
                                match &$group[start] {
                                    Token::Ident(i) => {
                                        $array.push(Expr::Variable(ExprVariable { inner: i.inner.to_string() }));
                                        start = end+1;
                                        end = start; // For will increment one more
                                        continue;
                                    },
                                    _ => continue,
                                }
                            } 
                            if let Ok(expr) = try_eat_expr(&$group[start..end].to_vec()) {
                                $array.push(expr);
                                start = end+1;
                                end = start; // For will increment one more
                            } else { continue }
                        },
                        _ => continue,
                    }
                },
                Token::CloseGroup($end_del) => {
                    if end-start == 1 {
                        match &$group[start] {
                            Token::Ident(i) => {
                                $array.push(Expr::Variable(ExprVariable { inner: i.inner.to_string() }));
                                start = end+1;
                                end = start; // For will increment one more
                                continue;
                            },
                            Token::Literal(l) => {
                                $array.push(Expr::Lit(ExprLit { lit: l.inner.to_string() }));
                                start = end+1;
                                end = start; // For will increment one more
                                continue;
                            },
                            _ => continue,
                        }
                    } 
                    if let Ok(expr) = try_eat_expr(&$group[start..end].to_vec()) {
                        $array.push(expr);
                        start = end+1;
                        end = start + 1;
                    } else { continue }
                }
                _ => continue,
            }
        }
    };
}

fn eat_array_expr(group: &Vec<Token>) -> Result<Expr, ()> {
    // [ lit1 ] or [ lit1, lit2, ..., litN ]

    if group.len() < 3 { return Err(()) }
    match (&group[0], &group[group.len()-1]) {
        (Token::OpenGroup(Delimiter::Bracket), Token::CloseGroup(Delimiter::Bracket)) => (),
        _ => return Err(())
    }
    // println!("GROUP {:?}: {:?}", group.len(), group);
    let mut expr_array = ExprArray {elems: Vec::new()};
    if group.len() > 2 {
        read_until_comma_or_end!(group, 1, Delimiter::Bracket, expr_array.elems);
    }
    Ok(Expr::Array(expr_array))
}

fn eat_assign_expr(group: &Vec<Token>) -> Result<Expr,()> {
    // a = Compute()
    if group.len() < 3 { return Err(())}
    // let mut left: Option<Expr> = None;
    let left = match (&group[0], &group[1]) {
        // TODO: use read_ident to check is not a reserved word
        (Token::Ident(i), Token::Punct(p)) => {
            // println!("C {:?}. {:?}", group, i.inner);
            if p.ch == '=' {
                Expr::Variable(ExprVariable { inner: i.inner.to_string() })
            } else {
                return Err(())
            }
        },
        _ => return Err(())
    };
    let right = if let Ok(expr) = try_eat_expr(&group[2..].to_vec()) {
        expr
    } else{ return Err(())};

    Ok(Expr::Assign(ExprAssign { left: Box::new(left), right: Box::new(right) }))
}

fn eat_call_expr(group: &Vec<Token>) -> Result<Expr,()> {
    // name(arg1, arg2,...) or name() or name(arg1)
    if group.len() < 3 {
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
    match (&group[1], &group[group.len()-1]) {
        (Token::OpenGroup(Delimiter::Parenthesis), Token::CloseGroup(Delimiter::Parenthesis)) => (),
        _ => return Err(())
    }
    // Check pattern: expr1 punc.ch=="," .....
    if group.len() > 3 { // TODO: Remove this control is already passed by first `if` 
        read_until_comma_or_end!(group, 2, Delimiter::Parenthesis, call_expr.args);
    }

    Ok(Expr::Call(call_expr))
}

fn try_eat_expr(group: &Vec<Token>) -> Result<Expr,()> {
    if let Ok(expr) =  eat_call_expr(group){
        return Ok(expr)
    } else if let Ok(expr) = eat_array_expr(group) {
        return Ok(expr)
    } else if let Ok(expr) = eat_assign_expr(group) {
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

#[cfg(test)]
mod tests {
    // use crate::expr::*;
    use super::*;
    use yarte_strnom::Cursor;

    #[test]
    fn test_call_expr() {
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
            println!("Parsed: {:?}", r);
            println!("Should be: {:?}", func_res[i]);
            assert_eq!(r, &func_res[i])
        }

        // Call with no arguments
        let cursor = Cursor {
            rest: "call()",
            off: 0
        };
        let mut func_res = Vec::new();
        func_res.push(Expr::Call(ExprCall{
            args: Vec::new(),
            func: "call".to_string()
        }));
        let res = parse(cursor).unwrap();
        for (i, r) in res.iter().enumerate() {
            println!("Parsed: {:?}", r);
            println!("Should be: {:?}", func_res[i]);
            assert_eq!(r, &func_res[i])
        }

        // Cal with one argument
        let cursor = Cursor {
            rest: "call(a)",
            off: 0
        };
        let mut func_res = Vec::new();
        func_res.push(Expr::Call(ExprCall{
            args: vec![
                Expr::Variable(ExprVariable{inner: "a".to_string()}),
            ],
            func: "call".to_string()
        }));
        let res = parse(cursor).unwrap();
        for (i, r) in res.iter().enumerate() {
            println!("Parsed: {:?}", r);
            println!("Should be: {:?}", func_res[i]);
            assert_eq!(r, &func_res[i])
        }
    }

    #[test]
    fn test_array_expr() {
        let cursor = Cursor {
            rest: "[a, 'b']",
            off: 0
        };
        let mut arr_res = Vec::new();
        arr_res.push(Expr::Array(ExprArray{
            elems: vec![
                Expr::Variable(ExprVariable{inner: "a".to_string()}),
                Expr::Lit(ExprLit {lit: "'b'".to_string()})
            ]
        }));
        let res = parse(cursor).unwrap();
        for (i, r) in res.iter().enumerate() {
            println!("Parsed: {:?}", r);
            println!("Should be: {:?}", arr_res[i]);
            assert_eq!(r, &arr_res[i])
        }
    }

    #[test]
    fn test_assign_expr() {
        let cursor = Cursor {
            rest: "a = compute()",
            off: 0
        };
        let mut assign_res = Vec::new();
        assign_res.push(Expr::Assign(ExprAssign {
            left: Box::new(Expr::Variable(ExprVariable{ inner: "a".to_string() })),
            right: Box::new(Expr::Call(ExprCall { args: Vec::new(), func: "compute".to_string() })),
        }));
        let res = parse(cursor).unwrap();
        for (i, r) in res.iter().enumerate() {
            println!("Parsed: {:?}", r);
            println!("Should be: {:?}", assign_res[i]);
            assert_eq!(r, &assign_res[i])
        }

        // Combine assign array and call
        let cursor = Cursor {
            rest: "a = compute([b,'b'])",
            off: 0
        };
        let mut assign_res = Vec::new();
        assign_res.push(Expr::Assign(ExprAssign {
            left: Box::new(Expr::Variable(ExprVariable{ inner: "a".to_string() })),
            right: Box::new(Expr::Call(ExprCall {
                args: vec![
                    Expr::Array(ExprArray{
                        elems: vec![
                            Expr::Variable(ExprVariable{inner:"b".to_string()}),
                            Expr::Lit(ExprLit{lit:"'b'".to_string()})
                        ]
                    })
                ],
                func: "compute".to_string()
            })),
        }));
        println!("-----------------------");
        let res = parse(cursor).unwrap();
        println!("Length {:?}", res.len());
        for (i, r) in res.iter().enumerate() {
            println!("Parsed: {:?}", r);
            println!("Should be: {:?}", assign_res[i]);
            assert_eq!(r, &assign_res[i])
        }
    }
}