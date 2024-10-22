use std::{collections::HashMap, fmt, io};

#[derive(Clone)]
enum RispExp {
    Bool(bool),
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
    Func(fn(&[RispExp]) -> Result<RispExp, RispErr>),
    Lambda(RispLambda),
}

#[derive(Debug)]
enum RispErr {
    Reason(String),
}

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
        .ok_or(RispErr::Reason("could not get token".to_string()))?;
    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(RispErr::Reason("Unexpected `)`".to_string())),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq(tokens: &[String]) -> Result<(RispExp, &[String]), RispErr> {
    let mut tokens = tokens;
    let mut list = Vec::<RispExp>::new();
    loop {
        let (token, rest) = tokens
            .split_first()
            .ok_or(RispErr::Reason("could not find closing `)`".to_string()))?;
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
        _ => Err(RispErr::Reason("Expected a number".to_string())),
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
        .ok_or(RispErr::Reason("`-` expects at least one argument".into()))?;
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
                .ok_or(RispErr::Reason("expected at least one number".into()))?;
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
    RispEnv {
        data: HashMap::from([
            ("+".into(), RispExp::Func(add)),
            ("-".into(), RispExp::Func(sub)),
            ("=".into(), RispExp::Func(ensure_tonicity!(|a, b| a == b))),
            (">".into(), RispExp::Func(ensure_tonicity!(|a, b| a > b))),
            (">=".into(), RispExp::Func(ensure_tonicity!(|a, b| a >= b))),
            ("<".into(), RispExp::Func(ensure_tonicity!(|a, b| a < b))),
            ("<=".into(), RispExp::Func(ensure_tonicity!(|a, b| a <= b))),
        ]),
        outer: None,
    }
}

fn eval_if_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let [test_form, if_form, else_form] = arg_forms else {
        return Err(RispErr::Reason("expected if to have exactly 3 args".into()));
    };
    let test_eval = eval(&arg_forms[0], env)?;
    match test_eval {
        RispExp::Bool(b) => {
            let res_form = if b { if_form } else { else_form };
            eval(res_form, env)
        }
        _ => Err(RispErr::Reason(format!(
            "unexpected test form='{}'",
            test_form
        ))),
    }
}

fn eval_def_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let [first_form, second_form] = arg_forms else {
        return Err(RispErr::Reason(
            "expected def to have exactly 2 args".into(),
        ));
    };
    let name = match first_form {
        RispExp::Symbol(s) => s,
        _ => return Err(RispErr::Reason("expected first form to be a symbol".into())),
    };
    let second_eval = eval(second_form, env)?;
    env.data.insert(name.clone(), second_eval);
    Ok(first_form.clone())
}

fn eval_lambda_args(arg_forms: &[RispExp]) -> Result<RispExp, RispErr> {
    let [params_exp, body_exp] = arg_forms else {
        return Err(RispErr::Reason(
            "expected fn to have exactly 2 forms".into(),
        ));
    };
    let RispExp::List(list) = params_exp else {
        return Err(RispErr::Reason(
            "Expecting fn first form to be a list".into(),
        ));
    };
    let params = list
        .iter()
        .map(|x| match x {
            RispExp::Symbol(s) => Ok(s.clone()),
            _ => Err(RispErr::Reason(
                "expected symbols in the argument list".into(),
            )),
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
        return Err(RispErr::Reason(format!(
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
            .ok_or(RispErr::Reason(format!("unexpected symbol k='{}'", k)))?
            .clone()),
        RispExp::Number(_) | RispExp::Bool(_) => Ok(exp.clone()),
        RispExp::List(list) => {
            let (first_form, arg_forms) = list
                .split_first()
                .ok_or(RispErr::Reason("expected a non-empty list".into()))?;
            if let Some(res) = eval_built_in_form(first_form, arg_forms, env) {
                res
            } else {
                let first_eval = eval(first_form, env)?;
                match first_eval {
                    RispExp::Func(f) => f(&eval_forms(arg_forms, env)?),
                    RispExp::Lambda(lambda) => {
                        let new_env = &mut env_for_lambda(&lambda.params, arg_forms, env)?;
                        eval(&lambda.body_exp, new_env)
                    }
                    _ => Err(RispErr::Reason("first form must be a function".into())),
                }
            }
        }
        RispExp::Func(_) => Err(RispErr::Reason("Can't eval functions".into())),
        RispExp::Lambda(_) => Err(RispErr::Reason("Can't eval Lambda".into())),
    }
}

impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            RispExp::Bool(b) => b.to_string(),
            RispExp::Symbol(s) => s.clone(),
            RispExp::Number(n) => n.to_string(),
            RispExp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            RispExp::Func(_) => "Function {}".to_string(),
            RispExp::Lambda(_) => "Lambda {}".to_string(),
        };
        write!(f, "{}", str)
    }
}

fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let tokens = tokenize(expr);
    let (parsed_exp, rest) = parse(&tokens)?;
    if !rest.is_empty() {
        return Err(RispErr::Reason("Unexpected tokens".into()));
    }
    let evaled_exp = eval(&parsed_exp, env)?;
    Ok(evaled_exp)
}

fn slurp_expr() -> String {
    let mut expr = String::new();
    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");
    expr
}

fn main() {
    let env = &mut default_env();
    loop {
        println!("risp >");
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(res) => println!("// 🔥 => {}", res),
            Err(e) => match e {
                RispErr::Reason(msg) => println!("// 🙀 => {}", msg),
            },
        }
    }
}
