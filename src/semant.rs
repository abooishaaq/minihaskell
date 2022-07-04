use crate::syntax::*;
use std::collections::HashMap;

pub enum TyErr {
    TyUnbound(String),
    TyMismatch(Type, Type),
}

impl std::fmt::Display for TyErr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TyErr::TyUnbound(s) => write!(f, "Variable {} is unbound", s),
            TyErr::TyMismatch(t1, t2) => write!(f, "Type mismatch: {} vs {}", t1.show(), t2.show()),
        }
    }
}

fn check(env: &HashMap<String, Type>, t1: Type, expr: &Expr) -> Option<TyErr> {
    let res = type_of(env, expr);
    match res {
        Ok(t2) => {
            if t1 == t2 {
                None
            } else {
                Some(TyErr::TyMismatch(t1.clone(), t2))
            }
        }
        Err(e) => Some(e),
    }
}

pub fn type_of(env: &HashMap<String, Type>, expr: &Expr) -> Result<Type, TyErr> {
    match expr {
        Expr::Int(_) => Ok(Type::TInt),
        Expr::Bool(_) => Ok(Type::TBool),
        Expr::Var(v) => {
            if env.contains_key(v) {
                Ok(env[v].clone())
            } else {
                Err(TyErr::TyUnbound(v.to_string()))
            }
        }
        Expr::List(t) => Ok(Type::TList(Box::new(t.clone()))),
        Expr::Neg(ex) => {
            let t = type_of(env, ex)?;
            if t == Type::TInt {
                Ok(Type::TInt)
            } else {
                Err(TyErr::TyMismatch(Type::TInt, t))
            }
        }
        Expr::Add(ex1, ex2) => {
            let t1 = type_of(env, ex1)?;
            let t2 = type_of(env, ex2)?;
            if t1 == Type::TInt && t2 == Type::TInt {
                Ok(Type::TInt)
            } else {
                Err(TyErr::TyMismatch(Type::TInt, t1))
            }
        }
        Expr::Sub(ex1, ex2) => {
            let t1 = type_of(env, ex1)?;
            let t2 = type_of(env, ex2)?;
            if t1 == Type::TInt && t2 == Type::TInt {
                Ok(Type::TInt)
            } else {
                Err(TyErr::TyMismatch(Type::TInt, t1))
            }
        }
        Expr::Mul(ex1, ex2) => {
            let t1 = type_of(env, ex1)?;
            let t2 = type_of(env, ex2)?;
            if t1 == Type::TInt && t2 == Type::TInt {
                Ok(Type::TInt)
            } else {
                Err(TyErr::TyMismatch(Type::TInt, t1))
            }
        }
        Expr::Div(ex1, ex2) => {
            let t1 = type_of(env, ex1)?;
            let t2 = type_of(env, ex2)?;
            if t1 == Type::TInt && t2 == Type::TInt {
                Ok(Type::TInt)
            } else {
                Err(TyErr::TyMismatch(Type::TInt, t1))
            }
        }
        Expr::Mod(ex1, ex2) => {
            let t1 = type_of(env, ex1)?;
            let t2 = type_of(env, ex2)?;
            if t1 == Type::TInt && t2 == Type::TInt {
                Ok(Type::TInt)
            } else {
                Err(TyErr::TyMismatch(Type::TInt, t1))
            }
        }
        Expr::Equal(ex1, ex2) => {
            let t1 = type_of(env, ex1)?;
            let t2 = type_of(env, ex2)?;
            if (t1 == Type::TInt && t2 == Type::TInt) || (t1 == Type::TBool && t2 == Type::TBool) {
                Ok(Type::TBool)
            } else {
                Err(TyErr::TyMismatch(t1, t2))
            }
        }
        Expr::Lesser(ex1, ex2) => {
            let t1 = type_of(env, ex1)?;
            let t2 = type_of(env, ex2)?;
            if t1 == Type::TInt && t2 == Type::TInt {
                Ok(Type::TBool)
            } else {
                Err(TyErr::TyMismatch(Type::TInt, t1))
            }
        }
        Expr::Greater(ex1, ex2) => {
            let t1 = type_of(env, ex1)?;
            let t2 = type_of(env, ex2)?;
            if t1 == Type::TInt && t2 == Type::TInt {
                Ok(Type::TBool)
            } else {
                Err(TyErr::TyMismatch(Type::TInt, t1))
            }
        }
        Expr::Pair(ex1, ex2) => {
            let t1 = type_of(env, ex1)?;
            let t2 = type_of(env, ex2)?;
            Ok(Type::TTimes(Box::new(t1), Box::new(t2)))
        }
        Expr::Cons(ex1, ex2) => {
            let t1 = type_of(env, ex1)?;
            let t2 = Type::TList(Box::new(t1));
            match check(env, t2.clone(), ex2) {
                Some(e) => Err(e),
                None => Ok(t2.clone()),
            }
        }
        Expr::Fst(ex) => {
            let t = type_of(env, ex)?;
            if let Type::TTimes(t1, _) = t {
                Ok(*t1)
            } else {
                Err(TyErr::TyMismatch(Type::TList(Box::new(Type::TInt)), t))
            }
        }
        Expr::Snd(ex) => {
            let t = type_of(env, ex)?;
            if let Type::TTimes(_, t2) = t {
                Ok(*t2)
            } else {
                Err(TyErr::TyMismatch(Type::TList(Box::new(Type::TInt)), t))
            }
        }
        Expr::If(ex1, ex2, ex3) => {
            let t1 = type_of(env, ex1)?;
            let t2 = type_of(env, ex2)?;
            let t3 = type_of(env, ex3)?;
            if t1 == Type::TBool {
                if t2 == t3 {
                    Ok(t2)
                } else {
                    Err(TyErr::TyMismatch(t2, t3))
                }
            } else {
                Err(TyErr::TyMismatch(Type::TBool, t1))
            }
        }
        Expr::Func(v, t1, ex1) => {
            let mut env2 = env.clone();
            env2.insert(v.clone(), t1.clone());
            let t2 = type_of(&env2, ex1)?;
            let res = Type::TArrow(Box::new(t1.clone()), Box::new(t2));
            Ok(res)
        }
        Expr::Rec(v, t1, ex2) => {
            let mut env2 = env.clone();
            env2.insert(v.clone(), t1.clone());
            match check(&env2, t1.clone(), ex2) {
                Some(tyerr) => Err(tyerr),
                None => Ok(t1.clone()),
            }
        }
        Expr::Match(ex1, t1, ex2, v1, v2, ex3) => {
            let t2 = type_of(env, ex1)?;
            if let Type::TList(_) = t2 {
                let t3 = type_of(env, ex2)?;
                let mut env2 = env.clone();
                let t4 = Type::TList(Box::new(t1.clone()));
                env2.extend(vec![(v1.clone(), t1.clone()), (v2.clone(), t4.clone())]);
                let t5 = t3.clone();
                match check(&env2, t3, ex3) {
                    Some(tyerr) => Err(tyerr),
                    None => Ok(t5),
                }
            } else {
                Err(TyErr::TyMismatch(
                    Type::TList(Box::new(Type::TVar("a".to_string()))),
                    t1.clone(),
                ))
            }
        }
        Expr::Apply(ex1, ex2) => {
            let t1 = type_of(env, ex1)?;
            if let Type::TArrow(t2, t3) = t1 {
                match check(env, *t2, ex2) {
                    Some(tyerr) => Err(tyerr),
                    None => Ok(*t3),
                }
            } else {
                Err(TyErr::TyMismatch(
                    Type::TArrow(
                        Box::new(Type::TVar("a".to_string())),
                        Box::new(Type::TVar("b".to_string())),
                    ),
                    t1,
                ))
            }
        }
    }
}
