use crate::syntax::*;
use std::str;

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while_m_n},
    character::complete::space1,
    combinator::map,
    error::ErrorKind,
    multi::many0,
    sequence::{delimited, tuple},
    Err, IResult,
};

fn whitespace(input: &str) -> IResult<&str, Vec<&str>> {
    many0(alt((space1, tag("\n"))))(input)
}

fn isreserved(s: &str) -> bool {
    match s {
        "let" | "in" | "if" | "then" | "else" | "true" | "false" | "fun" | "rec" | "match"
        | "with" | "pair" | "fst" | "snd" | "list" => true,
        _ => false,
    }
}

fn identifier(input: &str) -> IResult<&str, String> {
    let (input, id) = take_while_m_n(1, 32, |c: char| c.is_alphanumeric() || c == '_')(input)?;
    if isreserved(id) {
        Err(Err::Error(nom::error::Error {
            input,
            code: ErrorKind::Fail,
        }))
    } else {
        Ok((input, id.to_string()))
    }
}

fn variable(input: &str) -> IResult<&str, Expr> {
    let (input, id) = identifier(input)?;
    Ok((input, Expr::Var(id)))
}

fn natural(input: &str) -> IResult<&str, Expr> {
    let (input, nat) = take_while_m_n(1, 19, |c: char| c.is_digit(10))(input)?;
    Ok((input, Expr::Int(nat.parse::<i64>().unwrap())))
}

fn boolean(input: &str) -> IResult<&str, Expr> {
    alt((
        map(tag("true"), |_| Expr::Bool(true)),
        map(tag("false"), |_| Expr::Bool(false)),
    ))(input)
}

fn tysimple(input: &str) -> IResult<&str, Type> {
    alt((
        map(tag("int"), |_| Type::TInt),
        map(tag("bool"), |_| Type::TBool),
        map(identifier, |id| Type::TVar(id)),
        delimited(tag("("), ty, tag(")")),
    ))(input)
}

fn tylist(input: &str) -> IResult<&str, Type> {
    let (input, mut t1) = tysimple(input)?;
    let (input, ls) = many0(delimited(whitespace, tag("list"), whitespace))(input)?;
    for _ in ls {
        t1 = Type::TList(Box::new(t1));
    }
    Ok((input, t1))
}

fn tytimes(input: &str) -> IResult<&str, Type> {
    let (input, mut t1) = tylist(input)?;
    let (input, res) = many0(tuple((whitespace, tag("*"), whitespace, tylist)))(input)?;
    for (_, _, _, t2) in res {
        t1 = Type::TTimes(Box::new(t1), Box::new(t2));
    }
    Ok((input, t1))
}

fn ty(input: &str) -> IResult<&str, Type> {
    let (input, mut t1) = tytimes(input)?;
    let (input, res) = many0(tuple((whitespace, tag("->"), whitespace, ty)))(input)?;
    for (_, _, _, t2) in res {
        t1 = Type::TArrow(Box::new(t1), Box::new(t2));
    }
    Ok((input, t1))
}

fn pair(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("(")(input)?;
    let (input, ex1) = delimited(whitespace, expr, whitespace)(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, ex2) = delimited(whitespace, expr, whitespace)(input)?;
    let (input, _) = tag(")")(input)?;
    Ok((input, Expr::Pair(Box::new(ex1), Box::new(ex2))))
}

fn ifelse(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("if")(input)?;
    let (input, ex1) = delimited(whitespace, expr, whitespace)(input)?;
    let (input, _) = tag("then")(input)?;
    let (input, ex2) = delimited(whitespace, expr, whitespace)(input)?;
    let (input, _) = tag("else")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, ex3) = expr(input)?;
    Ok((input, Expr::If(Box::new(ex1), Box::new(ex2), Box::new(ex3))))
}

fn list(input: &str) -> IResult<&str, Type> {
    let (input, _) = tag("[")(input)?;
    let (input, typ) = delimited(whitespace, ty, whitespace)(input)?;
    let (input, _) = tag("]")(input)?;
    Ok((input, typ))
}

fn fun(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("fun")(input)?;
    let (input, id) = delimited(whitespace, identifier, whitespace)(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, typ) = delimited(whitespace, ty, whitespace)(input)?;
    let (input, _) = delimited(whitespace, tag("=>"), whitespace)(input)?;
    let (input, ex) = expr(input)?;
    Ok((input, Expr::Func(id, typ, Box::new(ex))))
}

fn recis(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("rec")(input)?;
    let (input, id) = delimited(whitespace, identifier, whitespace)(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, typ) = delimited(whitespace, ty, whitespace)(input)?;
    let (input, _) = delimited(whitespace, tag("is"), whitespace)(input)?;
    let (input, ex) = expr(input)?;
    Ok((input, Expr::Rec(id, typ, Box::new(ex))))
}

fn matchh(input: &str) -> IResult<&str, Expr> {
    let (input, _) = delimited(whitespace, tag("match"), whitespace)(input)?;
    let (input, ex) = expr(input)?;
    let (input, _) = delimited(whitespace, tag("with"), whitespace)(input)?;
    let (input, t) = list(input)?;
    let (input, _) = delimited(whitespace, tag("=>"), whitespace)(input)?;
    let (input, ex2) = expr(input)?;
    let (input, _) = delimited(whitespace, tag("|"), whitespace)(input)?;
    let (input, v1) = identifier(input)?;
    let (input, _) = delimited(whitespace, tag("::"), whitespace)(input)?;
    let (input, v2) = identifier(input)?;
    let (input, _) = delimited(whitespace, tag("=>"), whitespace)(input)?;
    let (input, ex3) = expr(input)?;
    Ok((
        input,
        Expr::Match(Box::new(ex), t, Box::new(ex2), v1, v2, Box::new(ex3)),
    ))
}

fn fst(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("fst")(input)?;
    let (input, ex) = delimited(whitespace, expr, whitespace)(input)?;
    Ok((input, Expr::Fst(Box::new(ex))))
}

fn snd(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("snd")(input)?;
    let (input, ex) = delimited(whitespace, expr, whitespace)(input)?;
    Ok((input, Expr::Snd(Box::new(ex))))
}

fn aexp(input: &str) -> IResult<&str, Expr> {
    alt((
        natural,
        boolean,
        variable,
        pair,
        fst,
        snd,
        fun,
        recis,
        ifelse,
        matchh,
        map(list, Expr::List),
        delimited(tag("("), expr, tag(")")),
    ))(input)
}

fn cons(input: &str) -> IResult<&str, Expr> {
    let (input, mut ex1) = aexp(input)?;
    let (input, res) = many0(tuple((whitespace, tag("::"), whitespace, aexp)))(input)?;
    for (_, _, _, ex2) in res {
        ex1 = Expr::Cons(Box::new(ex1), Box::new(ex2));
    }
    Ok((input, ex1))
}

fn apply(input: &str) -> IResult<&str, Expr> {
    let (input, mut ex1) = cons(input)?;
    let (input, res) = many0(tuple((space1, cons)))(input)?;
    for (_, ex2) in res {
        ex1 = Expr::Apply(Box::new(ex1), Box::new(ex2));
    }
    Ok((input, ex1))
}

fn negate(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("-")(input)?;
    let (input, ex) = delimited(whitespace, apply, whitespace)(input)?;
    Ok((input, Expr::Neg(Box::new(ex))))
}

fn factor(input: &str) -> IResult<&str, Expr> {
    let (input, mut ex1) = alt((apply, negate))(input)?;
    let (input, mut res) = many0(tuple((
        whitespace,
        alt((tag("*"), tag("%"), tag("/"))),
        whitespace,
        apply,
    )))(input)?;
    while res.len() > 0 {
        let (_, op, _, ex2) = res.pop().unwrap();
        match op {
            "*" => ex1 = Expr::Mul(Box::new(ex1), Box::new(ex2)),
            "%" => ex1 = Expr::Mod(Box::new(ex1), Box::new(ex2)),
            "/" => ex1 = Expr::Div(Box::new(ex1), Box::new(ex2)),
            _ => panic!("unexpected operator"),
        }
    }
    Ok((input, ex1))
}

fn term(input: &str) -> IResult<&str, Expr> {
    let (input, mut ex1) = factor(input)?;
    let (input, mut res) = many0(tuple((
        whitespace,
        alt((tag("+"), tag("-"))),
        whitespace,
        factor,
    )))(input)?;
    while res.len() > 0 {
        let (_, op, _, ex2) = res.pop().unwrap();
        match op {
            "+" => ex1 = Expr::Add(Box::new(ex1), Box::new(ex2)),
            "-" => ex1 = Expr::Sub(Box::new(ex1), Box::new(ex2)),
            _ => panic!("unexpected operator"),
        }
    }
    Ok((input, ex1))
}

fn compare(input: &str) -> IResult<&str, Expr> {
    let (input, mut ex1) = term(input)?;
    let (input, mut res) = many0(tuple((
        whitespace,
        alt((tag("<"), tag(">"))),
        whitespace,
        term,
    )))(input)?;
    while res.len() > 0 {
        let (_, op, _, ex2) = res.pop().unwrap();
        match op {
            "<" => ex1 = Expr::Lesser(Box::new(ex1), Box::new(ex2)),
            ">" => ex1 = Expr::Greater(Box::new(ex1), Box::new(ex2)),
            _ => panic!("unexpected operator"),
        }
    }
    Ok((input, ex1))
}

pub fn expr(input: &str) -> IResult<&str, Expr> {
    let (input, mut ex1) = compare(input)?;
    let (input, mut res) = many0(tuple((whitespace, tag("="), whitespace, compare)))(input)?;
    while res.len() > 0 {
        let ex2 = res.pop().unwrap().3;
        ex1 = Expr::Equal(Box::new(ex1), Box::new(ex2));
    }
    Ok((input, ex1))
}

fn extop(input: &str) -> IResult<&str, TopLevel> {
    map(
        tuple((delimited(whitespace, expr, whitespace), tag(";;"))),
        |res| TopLevel::Expr(res.0),
    )(input)
}

fn def(input: &str) -> IResult<&str, TopLevel> {
    let (input, _) = delimited(whitespace, tag("let"), whitespace)(input)?;
    let (input, v) = identifier(input)?;
    let (input, _) = delimited(whitespace, tag("="), whitespace)(input)?;
    let (input, ex) = expr(input)?;
    let (input, _) = delimited(whitespace, tag(";;"), whitespace)(input)?;
    Ok((input, TopLevel::Def(v, ex)))
}

fn comment(input: &str) -> IResult<&str, TopLevel> {
    let (input, _) = tag("--")(input)?;
    let (input, _) = take_while(|c: char| c != '\n')(input)?;
    Ok((input, TopLevel::Comment))
}

pub fn toplevel(input: &str) -> IResult<&str, Vec<TopLevel>> {
    many0(delimited(
        whitespace,
        alt((def, extop, comment)),
        whitespace,
    ))(input)
}
