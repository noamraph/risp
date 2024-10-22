use std::{collections::HashMap, fmt};

use rustyline::DefaultEditor;

#[derive(Clone)]
enum RispExp {
    Bool(bool),
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
    Func(&'static str, fn(&[RispExp]) -> Result<RispExp, RispErr>),
    Lambda(RispLambda),
}

struct RispErr(String);

#[derive(Clone)]
struct RispLambda {
    params: Vec<String>,
    body_exp: Box<RispExp>,
}

#[derive(Clone)]
struct RispEnv<'a> {
    data: HashMap<String, RispExp>,
    outer: Option<&'a RispEnv<'a>>,
}

fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn parse(tokens: &[String]) -> Result<(RispExp, &[String]), RispErr> {
    let (token, rest) = tokens
        .split_first()
        .ok_or(RispErr("could not get token".to_string()))?;
    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(RispErr("Unexpected `)`".to_string())),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq(tokens: &[String]) -> Result<(RispExp, &[String]), RispErr> {
    let mut tokens = tokens;
    let mut list = Vec::<RispExp>::new();
    loop {
        let (token, rest) = tokens
            .split_first()
            .ok_or(RispErr("could not find closing `)`".to_string()))?;
        if token == ")" {
            return Ok((RispExp::List(list), rest));
        }
        let (exp, rest) = parse(tokens)?;
        list.push(exp);
        tokens = rest;
    }
}

fn parse_atom(token: &str) -> RispExp {
    match token {
        "true" => RispExp::Bool(true),
        "false" => RispExp::Bool(false),
        _ => {
            let maybe_float = token.parse::<f64>();
            match maybe_float {
                Ok(v) => RispExp::Number(v),
                Err(_) => RispExp::Symbol(token.to_string().clone()),
            }
        }
    }
}

fn parse_single_float(exp: &RispExp) -> Result<f64, RispErr> {
    match *exp {
        RispExp::Number(num) => Ok(num),
        _ => Err(RispErr("Expected a number".to_string())),
    }
}

fn parse_list_of_floats(args: &[RispExp]) -> Result<Vec<f64>, RispErr> {
    args.iter().map(parse_single_float).collect()
}

fn add(args: &[RispExp]) -> Result<RispExp, RispErr> {
    Ok(RispExp::Number(
        parse_list_of_floats(args)?
            .iter()
            .fold(0.0, |sum, a| sum + a),
    ))
}

fn sub(args: &[RispExp]) -> Result<RispExp, RispErr> {
    let nums = parse_list_of_floats(args)?;
    let (first, rest) = nums
        .split_first()
        .ok_or(RispErr("`-` expects at least one argument".into()))?;
    Ok(RispExp::Number(
        first - rest.iter().fold(0.0, |sum, a| sum + a),
    ))
}

macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse_list_of_floats(args)?;
            let (first, rest) = floats
                .split_first()
                .ok_or(RispErr("expected at least one number".into()))?;
            fn f(prev: &f64, xs: &[f64]) -> bool {
                match xs.first() {
                    Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                    None => true,
                }
            }
            Ok(RispExp::Bool(f(first, rest)))
        }
    }};
}

fn default_env<'a>() -> RispEnv<'a> {
    type Func = fn(&[RispExp]) -> Result<RispExp, RispErr>;
    let funcs: &[(&str, Func)] = &[
        ("+", add),
        ("-", sub),
        ("=", ensure_tonicity!(|a, b| a == b)),
        (">", ensure_tonicity!(|a, b| a > b)),
        (">=", ensure_tonicity!(|a, b| a >= b)),
        ("<", ensure_tonicity!(|a, b| a < b)),
        ("<=", ensure_tonicity!(|a, b| a <= b)),
    ];
    RispEnv {
        data: funcs
            .iter()
            .map(|(k, f)| (k.to_string(), RispExp::Func(k, *f)))
            .collect(),
        outer: None,
    }
}

fn eval_if_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let [test_form, if_form, else_form] = arg_forms else {
        return Err(RispErr("expected if to have exactly 3 args".into()));
    };
    let test_eval = eval(&arg_forms[0], env)?;
    match test_eval {
        RispExp::Bool(b) => {
            let res_form = if b { if_form } else { else_form };
            eval(res_form, env)
        }
        _ => Err(RispErr(format!("unexpected test form='{}'", test_form))),
    }
}

fn eval_def_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let [first_form, second_form] = arg_forms else {
        return Err(RispErr("expected def to have exactly 2 args".into()));
    };
    let name = match first_form {
        RispExp::Symbol(s) => s,
        _ => return Err(RispErr("expected first form to be a symbol".into())),
    };
    let second_eval = eval(second_form, env)?;
    env.data.insert(name.clone(), second_eval);
    Ok(first_form.clone())
}

fn eval_lambda_args(arg_forms: &[RispExp]) -> Result<RispExp, RispErr> {
    let [params_exp, body_exp] = arg_forms else {
        return Err(RispErr("expected fn to have exactly 2 forms".into()));
    };
    let RispExp::List(list) = params_exp else {
        return Err(RispErr("Expecting fn first form to be a list".into()));
    };
    let params = list
        .iter()
        .map(|x| match x {
            RispExp::Symbol(s) => Ok(s.clone()),
            _ => Err(RispErr("expected symbols in the argument list".into())),
        })
        .collect::<Result<Vec<String>, RispErr>>()?;
    Ok(RispExp::Lambda(RispLambda {
        params,
        body_exp: Box::new(body_exp.clone()),
    }))
}

fn eval_built_in_form(
    exp: &RispExp,
    arg_forms: &[RispExp],
    env: &mut RispEnv,
) -> Option<Result<RispExp, RispErr>> {
    match exp {
        RispExp::Symbol(s) => match s.as_str() {
            "if" => Some(eval_if_args(arg_forms, env)),
            "def" => Some(eval_def_args(arg_forms, env)),
            "fn" => Some(eval_lambda_args(arg_forms)),
            _ => None,
        },
        _ => None,
    }
}

fn env_get(k: &str, env: &RispEnv) -> Option<RispExp> {
    match env.data.get(k) {
        Some(exp) => Some(exp.clone()),
        None => match env.outer {
            Some(outer_env) => env_get(k, outer_env),
            None => None,
        },
    }
}

fn eval_forms(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<Vec<RispExp>, RispErr> {
    arg_forms.iter().map(|x| eval(x, env)).collect()
}

fn env_for_lambda<'a>(
    params: &[String],
    arg_forms: &[RispExp],
    outer_env: &'a mut RispEnv,
) -> Result<RispEnv<'a>, RispErr> {
    if arg_forms.len() != params.len() {
        return Err(RispErr(format!(
            "expected {} arguments, got {}",
            params.len(),
            arg_forms.len()
        )));
    }
    let vs = eval_forms(arg_forms, outer_env)?;
    let data = params
        .iter()
        .zip(vs.iter())
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect::<HashMap<String, RispExp>>();
    Ok(RispEnv {
        data,
        outer: Some(outer_env),
    })
}

fn eval(exp: &RispExp, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    match exp {
        RispExp::Symbol(k) => Ok(env_get(k, env)
            .ok_or(RispErr(format!("Couldn't find symbol `{}`", k)))?
            .clone()),
        RispExp::List(list) => {
            let (first_form, arg_forms) = list
                .split_first()
                .ok_or(RispErr("expected a non-empty list".into()))?;
            if let Some(res) = eval_built_in_form(first_form, arg_forms, env) {
                res
            } else {
                let first_eval = eval(first_form, env)?;
                match first_eval {
                    RispExp::Func(_, f) => f(&eval_forms(arg_forms, env)?),
                    RispExp::Lambda(lambda) => {
                        let new_env = &mut env_for_lambda(&lambda.params, arg_forms, env)?;
                        eval(&lambda.body_exp, new_env)
                    }
                    _ => Err(RispErr(format!(
                        "first form must be a function: {}",
                        first_eval
                    ))),
                }
            }
        }
        _ => Ok(exp.clone()),
    }
}

impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            RispExp::Bool(b) => b.to_string(),
            RispExp::Symbol(s) => s.clone(),
            RispExp::Number(n) => n.to_string(),
            RispExp::List(list) => {
                format!(
                    "({})",
                    list.iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join(" ")
                )
            }
            RispExp::Func(name, _) => name.to_string(),
            RispExp::Lambda(lambda) => {
                format!("(fn ({}) {})", lambda.params.join(" "), lambda.body_exp)
            }
        };
        write!(f, "{}", str)
    }
}

fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let tokens = tokenize(expr);
    let (parsed_exp, rest) = parse(&tokens)?;
    if !rest.is_empty() {
        return Err(RispErr("Unexpected tokens".into()));
    }
    let evaled_exp = eval(&parsed_exp, env)?;
    Ok(evaled_exp)
}

fn main() {
    let mut rl = DefaultEditor::new().unwrap();

    let env = &mut default_env();
    loop {
        let readline = rl.readline("risp>> ");
        match readline {
            Ok(expr) => {
                rl.add_history_entry(&expr).unwrap();
                match parse_eval(expr, env) {
                    Ok(res) => println!("{}", res),
                    Err(e) => match e {
                        RispErr(msg) => println!("// 🙀 => {}", msg),
                    },
                }
            }
            Err(_) => break,
        }
    }
}
