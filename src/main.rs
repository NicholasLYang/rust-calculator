use std::io;
use std::io::Write;
#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub parser);

mod lexer;
mod ast;

use ast::{Expr, Value, Op};

fn eval(expr: &Expr) -> Option<Value> {
    match expr {
        Expr::BinOp(op, lhs, rhs) => bind_op(eval(lhs), eval(rhs), op),
        Expr::Value(val) => Some(*val),
    }
}

fn optimize(expr: Expr) -> Expr {
    if let Expr::BinOp(op, lhs, rhs) = expr {
        let optimized_lhs = optimize(*lhs);
        let optimized_rhs = optimize(*rhs);
        match (op, optimized_lhs, optimized_rhs) {
            (Op::Plus, Expr::Value(0), rhs) => rhs,
            (Op::Plus, lhs, Expr::Value(0)) => lhs,
            (Op::Times, _, Expr::Value(0)) => Expr::Value(0),
            (Op::Times, Expr::Value(0), _) => Expr::Value(0),
            (Op::Minus, lhs, Expr::Value(0)) => lhs,
            (op, lhs, rhs) => Expr::BinOp(op, Box::new(lhs), Box::new(rhs)),
        }
    } else {
        expr
    }
}

fn bind_op(lhs: Option<Value>, rhs: Option<Value>, op: &Op) -> Option<Value> {
    match (lhs, rhs) {
        (Some(lval), Some(rval)) => match *op {
            Op::Plus => Some(lval + rval),
            Op::Minus => Some(lval - rval),
            Op::Times => Some(lval * rval),
            Op::Div => {
                if rval == 0 {
                    return None;
                }
                Some(lval / rval)
            }
        },
        (_, _) => None,
    }
}

#[test]
fn test_optimize() {
    let exprs = vec![
        Expr::BinOp(
            Op::Times,
            Box::new(Expr::Value(0)),
            Box::new(Expr::Value(10)),
        ),
        Expr::BinOp(
            Op::Plus,
            Box::new(Expr::BinOp(
                Op::Minus,
                Box::new(Expr::Value(10)),
                Box::new(Expr::Value(20)),
            )),
            Box::new(Expr::Value(30)),
        ),
    ];
    for expr in exprs {
        let optimized_expr = optimize(expr.clone());
        assert_eq!(eval(&optimized_expr), eval(&expr));
    }
}

fn run_calculator() {
    print!("> ");
    io::stdout().flush().unwrap();
    let mut input = String::new();

    io::stdin().read_line(&mut input)
        .ok()
        .expect("Couldn't read line");

    let lexer = lexer::Lexer::new(&input);

    let parser_out = parser::ExprParser::new().parse(lexer);
    if let Ok(expr) = parser_out {
        println!("Parsed into: {:?}", expr);
        let optimized_expr = optimize(expr);
        println!("Optimized: {:?}", optimized_expr);
        match eval(&optimized_expr) {
            Some(val) => println!("{}", val),
            None => println!("Could not evaluate")
        }
    } else {
        println!("Could not parse input: {:?}", parser_out);
    }
}

fn main() {
    loop {
        run_calculator();
    }
}
