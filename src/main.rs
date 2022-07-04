mod parser;
mod semant;
mod syntax;

use parser::toplevel;
use semant::type_of;
use std::collections::HashMap;
use std::env;
use std::io::Read;
use syntax::{TopLevel, Type};

fn eval(tyenv: &mut HashMap<String, Type>, source: &str) {
    let result = toplevel(source);
    match result {
        Ok((_, res)) => {
            for stmt in res {
                match stmt {
                    TopLevel::Expr(ref ex) => {
                        // println!("{:?}", ex);
                        let res = type_of(tyenv, ex);
                        match res {
                            Ok(t) => println!("{}", t.show()),
                            Err(e) => println!("{}", e),
                        }
                    }
                    TopLevel::Def(ref v, ref ex) => {
                        // println!("{} = {:?}", v, ex);
                        let res = type_of(tyenv, ex);
                        match res {
                            Ok(t) => {
                                tyenv.insert(v.clone(), t.clone());
                                println!("{} : {}", v, t.show())
                            }
                            Err(e) => println!("{}", e),
                        }
                    }
                    TopLevel::Comment => (),
                }
            }
        }
        Err(e) => println!("Error: {}", e),
    }
}

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let mut env = HashMap::new();
    if args.len() > 1 {
        let mut file = std::fs::File::open(args[1].clone()).unwrap();
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();
        eval(&mut env, &contents);
    } else {
        loop {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            let input = input.trim();
            if input == "exit" {
                break;
            }
            eval(&mut env, &input);
        }
    }
}
