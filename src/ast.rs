#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    BinOp(Op, Box<Expr>, Box<Expr>),
    Value(Value),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Op {
    Plus,
    Minus,
    Times,
    Div
}

pub type Value = i64;
