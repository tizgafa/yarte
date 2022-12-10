use yarte_rust::parser::parse;
use yarte_rust::expr::*;
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
        // eprintln!("Parsed: {:?}", r);
        // eprintln!("Should be: {:?}", func_res[i]);
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
        // eprintln!("Parsed: {:?}", r);
        // eprintln!("Should be: {:?}", func_res[i]);
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
        // eprintln!("Parsed: {:?}", r);
        // eprintln!("Should be: {:?}", func_res[i]);
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
        // eprintln!("Parsed: {:?}", r);
        // eprintln!("Should be: {:?}", arr_res[i]);
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
        // eprintln!("Parsed: {:?}", r);
        // eprintln!("Should be: {:?}", assign_res[i]);
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
    // eprintln!("-----------------------");
    let res = parse(cursor).unwrap();
    // eprintln!("Length {:?}", res.len());
    for (i, r) in res.iter().enumerate() {
        // eprintln!("Parsed: {:?}", r);
        // eprintln!("Should be: {:?}", assign_res[i]);
        assert_eq!(r, &assign_res[i])
    }
}

#[test]
fn test_bin_expr() {
    let cursor = Cursor {
        rest: "a + b;",
        off: 0
    };
    let mut bin_res = Vec::new();
    bin_res.push(Expr::Binary(ExprBinary {
        left: Box::new(Expr::Variable(ExprVariable { inner: "a".to_string() })),
        op: BinOp::Add,
        right: Box::new(Expr::Variable(ExprVariable { inner: "b".to_string() }))
     }));
    let res = parse(cursor).unwrap();
    assert_eq!(res.len() , 1);
    for (i, r) in res.iter().enumerate() {
        // eprintln!("Parsed: {:?}", r);
        // eprintln!("Should be: {:?}", bin_res[i]);
        assert_eq!(r, &bin_res[i])
    }
    let cursor = Cursor {
        rest: "a + compute([b,'b']);",
        off: 0
    };
    let mut bin_res = Vec::new();
    bin_res.push(Expr::Binary(ExprBinary {
        left: Box::new(Expr::Variable(ExprVariable { inner: "a".to_string() })),
        op: BinOp::Add,
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
    let res = parse(cursor).unwrap();
    assert_eq!(res.len() , 1);
    for (i, r) in res.iter().enumerate() {
        eprintln!("Parsed: {:?}", r);
        eprintln!("Should be: {:?}", bin_res[i]);
        assert_eq!(r, &bin_res[i])
    }
}