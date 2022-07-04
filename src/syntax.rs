#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    TInt,
    TBool,
    TVar(String),
    TTimes(Box<Type>, Box<Type>),
    TArrow(Box<Type>, Box<Type>),
    TList(Box<Type>),
}

impl Type {
    pub fn show(&self) -> String {
        match self {
            Type::TInt => "int".to_string(),
            Type::TBool => "bool".to_string(),
            Type::TVar(name) => name.clone(),
            Type::TTimes(left, right) => format!("{} * {}", left.show(), right.show()),
            Type::TArrow(left, right) => format!("{} -> {}", left.show(), right.show()),
            Type::TList(inner) => format!("[{}]", inner.show()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Var(String),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Equal(Box<Expr>, Box<Expr>),
    Lesser(Box<Expr>, Box<Expr>),
    Greater(Box<Expr>, Box<Expr>),
    Cons(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
    Func(String, Type, Box<Expr>),
    Rec(String, Type, Box<Expr>),
    Match(Box<Expr>, Type, Box<Expr>, String, String, Box<Expr>),
    Pair(Box<Expr>, Box<Expr>),
    Fst(Box<Expr>),
    Snd(Box<Expr>),
    List(Type),
}

pub enum TopLevel {
    Expr(Expr),
    Def(String, Expr),
    Comment,
}
