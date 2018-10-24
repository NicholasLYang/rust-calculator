#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    Plus,
    Minus,
    Times,
    Div,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    BinOp(Op, Box<Expr>, Box<Expr>),
    Value(Value),
}

type Value = i64;

fn eval(expr: Expr) -> Option<Value> {
    match expr {
        Expr::BinOp(op, lhs, rhs) => bind_op(eval(*lhs), eval(*rhs), op),
        Expr::Value(val) => Some(val),
    }
}

fn optimize(expr: Expr) -> Expr {
    if let Expr::BinOp(op, lhs, rhs) = expr {
        match (op, *lhs, rhs) {
            (Op::Plus, Expr::Value(0), rhs) => *rhs,
            (op, lhs, rhs) => Expr::BinOp(op, Box::new(lhs), rhs),
        }
    } else {
        expr
    }
}

fn bind_op(lhs: Option<Value>, rhs: Option<Value>, op: Op) -> Option<Value> {
    match (lhs, rhs) {
        (Some(lval), Some(rval)) => match op {
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
        assert_eq!(eval(optimized_expr), eval(expr));
    }
}

fn main() {
    let mut expr = Expr::BinOp(
        Op::Plus,
        Box::new(Expr::Value(0)),
        Box::new(Expr::Value(20)),
    );
    println!("{:?}", optimize(expr));
}
