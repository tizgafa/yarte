use crate::parser::Token;
use crate::expr::*;
use crate::token_types::*;

pub fn try_eat_expr(group: & [Token]) -> Result<Expr,()> {
    if let Ok(expr) =  eat_call_expr(group){
        return Ok(expr)
    } else if let Ok(expr) = eat_array_expr(group) {
        return Ok(expr)
    } else if let Ok(expr) = eat_assign_expr(group) {
        return Ok(expr)
    } else if let Ok(expr) = eat_binary_expr(group) {
        return Ok(expr)
    } else {
        return Err(())
    } 
}

fn eat_operator(ops: & [Token]) -> Result<(BinOp, usize), ()> {
    let (binop, len) = match &ops[0] {
        Token::Punct(Punct{ch: '+'}) => match &ops[1] {
            Token::Punct(Punct{ch: '='}) => (BinOp::AddEq, 2),
            _ => (BinOp::Add, 1)
        }
        Token::Punct(Punct{ch: '-'}) => match &ops[1] {
            Token::Punct(Punct{ch: '='}) => (BinOp::SubEq, 2),
            _ => (BinOp::Sub, 1)
        }
        Token::Punct(Punct{ch: '*'}) => match &ops[1] {
            Token::Punct(Punct{ch: '='}) => (BinOp::MulEq, 2),
            _ => (BinOp::Mul, 1)
        }
        Token::Punct(Punct{ch: '/'}) => match &ops[1] {
            Token::Punct(Punct{ch: '='}) => (BinOp::DivEq, 2),
            _ => (BinOp::Div, 1)
        }
        Token::Punct(Punct{ch: '%'}) => match &ops[1] {
            Token::Punct(Punct{ch: '='}) => (BinOp::RemEq, 2),
            _ => (BinOp::Rem, 1)
        }
        Token::Punct(Punct{ch: '&'}) => match &ops[1] {
            Token::Punct(Punct{ch: '='}) => (BinOp::BitAndEq, 2),
            Token::Punct(Punct{ch: '&'}) => (BinOp::And, 2),
            _ => (BinOp::BitAnd, 1)
        }
        Token::Punct(Punct{ch: '^'}) => match &ops[1] {
            Token::Punct(Punct{ch: '='}) => (BinOp::BitXorEq, 2),
            _ => (BinOp::BitXor, 1)
        }
        Token::Punct(Punct{ch: '<'}) => match &ops[1] {
            Token::Punct(Punct{ch: '<'}) => match &ops[2] {
                Token::Punct(Punct{ch: '='}) => (BinOp::ShlEq, 3),
                _ => (BinOp::Shl, 2)
            }
            Token::Punct(Punct{ch: '='}) => (BinOp::Le, 2),
            _ => (BinOp::Lt, 1)
        }
        Token::Punct(Punct{ch: '>'}) => match &ops[1] {
            Token::Punct(Punct{ch: '>'}) => match &ops[2] {
                Token::Punct(Punct{ch: '='}) => (BinOp::ShrEq, 3),
                _ => (BinOp::Shr, 2)
            }
            Token::Punct(Punct{ch: '='}) => (BinOp::Ge, 2),
            _ => (BinOp::Gt, 1)
        }

        // Token::Punct(Punct{ch: '!'}) => match &ops[1] {
        //     Token::Punct(Punct{ch: '='}) => (BinOp::Ne, 2),
        //     _ => return Err(())
        // }
        // Token::Punct(Punct{ch: '='}) => (BinOp::Eq, 1),
        _ => return Err(())
    };
    return Ok((binop, len))
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

fn eat_array_expr(group: & [Token]) -> Result<Expr, ()> {
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

fn eat_assign_expr(group: &[Token]) -> Result<Expr,()> {
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

fn eat_call_expr(group: &[Token]) -> Result<Expr,()> {
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

fn eat_binary_expr(group: &[Token]) -> Result<Expr, ()> {
    // a + b
    let mut left = None;
    let mut op = None;
    let mut end_left = 0;
    if group.len() < 3 { return Err(())}
    for end in 1..group.len() {
        if let Ok((o, len)) = eat_operator(&group[end..]) {
            if end == 1 {
                match &group[0] {
                    Token::Ident(i) => {
                        left = Some(Expr::Variable(ExprVariable { inner: i.inner.to_string() }));
                        op = Some(o);
                        end_left = end + len;
                        break;
                    },
                    Token::Literal(l) => {
                        left = Some(Expr::Lit(ExprLit { lit: l.inner.to_string() }));
                        op = Some(o);
                        end_left = end + len;
                        break;
                    },
                    _ => continue,
                }
            }
            match try_eat_expr(&group[..end]) {
                Ok(expr) => {
                    left = Some(expr);
                    op = Some(o);
                    end_left = end + len;
                    break;
                }
                _ => continue
            }
        }
    }

    if left==None || op == None {return Err(())}

    for end in end_left..group.len() {
        println!("JJJJ\n\t {:?}", group[end_left]);
        if let Token::Punct(Punct { ch:';' }) = &group[end]{
            if end - end_left == 1 {
                match &group[end_left] {
                    Token::Ident(i) => return Ok(Expr::Binary(ExprBinary{
                        left: Box::new(left.unwrap()),
                        op: op.unwrap(),
                        right: Box::new(Expr::Variable(ExprVariable { inner: i.inner.to_string() }))
                    })),
                    Token::Literal(l) => return Ok(Expr::Binary(ExprBinary{
                        left: Box::new(left.unwrap()),
                        op: op.unwrap(),
                        right: Box::new(Expr::Lit(ExprLit { lit: l.inner.to_string() }))
                    })),
                    _ => return Err(())
                }
            } else{
                if let Ok(expr) = try_eat_expr(&group[end_left..]) {
                    return Ok(Expr::Binary(ExprBinary{
                        left: Box::new(left.unwrap()),
                        op: op.unwrap(),
                        right: Box::new(expr)
                    }))
                }else{ return Err(())}
            }
        }
    }
    return Err(())
}