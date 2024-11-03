use core::panic;
use std::{collections::HashMap, io::Read, ops::ControlFlow};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0, none_of},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded, terminated},
    Finish, IResult, Parser,
};

fn main() {
    let mut buf = String::new();
    if !std::io::stdin().read_to_string(&mut buf).is_ok() {
        panic!("Failed to read stdin");
    }
    let parsed_statements = match statements_finish(&buf) {
        Ok(parsed_statements) => parsed_statements,
        Err(e) => {
            eprintln!("Parsed error: {:?}", e);
            return;
        }
    };
    let mut frame = StackFrame::new();
    eval_statements(&parsed_statements, &mut frame);
}

fn statements_finish(i: &str) -> Result<Statements, nom::error::Error<&str>> {
    let (_, res) = statements(i).finish()?;
    Ok(res)
}

fn eval_statements<'src>(stmts: &Statements<'src>, frame: &mut StackFrame<'src>) -> EvalResult {
    let mut result = EvalResult::Continue(Value::I64(0));
    for stmt in stmts {
        match stmt {
            Statement::Expression(expr) => {
                result = EvalResult::Continue(eval(expr, frame)?);
            }
            Statement::VarDef(name, expr) => {
                let value = eval(expr, frame)?;
                frame.vars.insert(name.to_string(), value);
            }
            Statement::VarAssign(name, expr) => {
                if !frame.vars.contains_key(*name) {
                    println!("variable not found: {:?}", name);
                }
                let value = eval(expr, frame)?;
                frame.vars.insert(name.to_string(), value);
            }
            Statement::For {
                loop_var,
                start,
                end,
                stmts,
            } => {
                let start = eval(start, frame)?
                    .as_i64()
                    .expect("iterator should be i64") as isize;
                let end = eval(end, frame)?.as_i64().expect("iterator should be i64") as isize;
                for i in start..end {
                    frame
                        .vars
                        .insert(loop_var.to_string(), Value::I64(i as i64));
                    match eval_statements(stmts, frame) {
                        EvalResult::Continue(val) => {
                            result = EvalResult::Continue(val);
                        }
                        EvalResult::Break(BreakResult::Return(val)) => {
                            return EvalResult::Break(BreakResult::Return(val))
                        }
                        EvalResult::Break(BreakResult::Break) => break,
                        EvalResult::Break(BreakResult::Continue) => continue,
                    }
                }
            }
            Statement::FnDef { name, args, stmts } => {
                frame.funcs.insert(
                    name.to_string(),
                    FnDef::User(UserFn {
                        args: args.clone(),
                        stmts: stmts.clone(),
                    }),
                );
            }
            Statement::Return(expr) => {
                return EvalResult::Break(BreakResult::Return(eval(expr, frame)?));
            }
            Statement::Break => return EvalResult::Break(BreakResult::Break),
            Statement::Continue => return EvalResult::Break(BreakResult::Continue),
        }
    }
    result
}

enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn),
}

impl<'src> FnDef<'src> {
    fn call(&self, args: &[Value], frame: &StackFrame<'src>) -> Value {
        match self {
            Self::User(user_fn) => {
                let mut new_frame = StackFrame::push_stack(frame);
                new_frame.vars = user_fn
                    .args
                    .iter()
                    .zip(args.iter())
                    .map(|(arg, val)| (arg.to_string(), val.clone()))
                    .collect();
                match eval_statements(&user_fn.stmts, &mut new_frame) {
                    EvalResult::Continue(val) | EvalResult::Break(BreakResult::Return(val)) => val,
                    EvalResult::Break(BreakResult::Break) => panic!("break outside loop"),
                    EvalResult::Break(BreakResult::Continue) => panic!("continue outside loop"),
                }
            }
            Self::Native(native_fn) => (native_fn.code)(args),
        }
    }
}

struct UserFn<'src> {
    args: Vec<&'src str>,
    stmts: Statements<'src>,
}

struct NativeFn {
    code: Box<dyn Fn(&[Value]) -> Value>,
}

type Variables = HashMap<String, Value>;
type Functions<'src> = HashMap<String, FnDef<'src>>;

#[derive(Default)]
struct StackFrame<'src> {
    vars: Variables,
    funcs: Functions<'src>,
    uplevel: Option<&'src StackFrame<'src>>,
}

impl<'src> StackFrame<'src> {
    fn new() -> Self {
        let mut funcs = HashMap::new();
        funcs.insert("sqrt".to_string(), unary_fn(f64::sqrt));
        funcs.insert("sin".to_string(), unary_fn(f64::sin));
        funcs.insert("cos".to_string(), unary_fn(f64::cos));
        funcs.insert("tan".to_string(), unary_fn(f64::tan));
        funcs.insert("asin".to_string(), unary_fn(f64::asin));
        funcs.insert("acos".to_string(), unary_fn(f64::acos));
        funcs.insert("atan".to_string(), unary_fn(f64::atan));
        funcs.insert("atan2".to_string(), binary_fn(f64::atan2));
        funcs.insert("pow".to_string(), binary_fn(f64::powf));
        funcs.insert("exp".to_string(), unary_fn(f64::exp));
        funcs.insert("log".to_string(), binary_fn(f64::log));
        funcs.insert("log10".to_string(), unary_fn(f64::log10));
        funcs.insert(
            "print".to_string(),
            FnDef::Native(NativeFn {
                code: Box::new(print),
            }),
        );
        funcs.insert(
            "dbg".to_string(),
            FnDef::Native(NativeFn {
                code: Box::new(p_dbg),
            }),
        );
        funcs.insert(
            "i64".to_string(),
            FnDef::Native(NativeFn {
                code: Box::new(|args| {
                    Value::I64(coerce_i64(
                        args.first().expect("functions missing argument"),
                    ))
                }),
            }),
        );
        funcs.insert(
            "f64".to_string(),
            FnDef::Native(NativeFn {
                code: Box::new(|args| {
                    Value::F64(coerce_f64(
                        args.first().expect("functions missing argument"),
                    ))
                }),
            }),
        );
        funcs.insert(
            "str".to_string(),
            FnDef::Native(NativeFn {
                code: Box::new(|args| {
                    Value::Str(coerce_str(
                        args.first().expect("functions missing argument"),
                    ))
                }),
            }),
        );

        Self {
            vars: Variables::new(),
            funcs,
            uplevel: None,
        }
    }

    fn get_fn(&self, name: &str) -> Option<&FnDef> {
        let mut next_frame = Some(self);
        while let Some(frame) = next_frame {
            if let Some(func) = frame.funcs.get(name) {
                return Some(func);
            }
            next_frame = frame.uplevel;
        }
        None
    }

    fn push_stack(uplevel: &'src Self) -> Self {
        Self {
            vars: Variables::new(),
            funcs: Functions::new(),
            uplevel: Some(uplevel),
        }
    }
}

fn print(args: &[Value]) -> Value {
    println!("print: {}", args[0]);
    Value::I64(0)
}

fn p_dbg(args: &[Value]) -> Value {
    println!("dbg: {:?}", args[0]);
    Value::I64(0)
}

#[derive(Debug, PartialEq, Clone)]
enum Value {
    F64(f64),
    I64(i64),
    Str(String),
}

impl Value {
    fn as_i64(&self) -> Option<i64> {
        match self {
            Self::F64(v) => Some(*v as i64),
            Self::I64(v) => Some(*v),
            Self::Str(v) => v.parse().ok(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::F64(v) => write!(f, "{v}"),
            Self::I64(v) => write!(f, "{v}"),
            Self::Str(v) => write!(f, "{v}"),
        }
    }
}

fn binary_op_str(
    lhs: &Value,
    rhs: &Value,
    d: impl Fn(f64, f64) -> f64,
    i: impl Fn(i64, i64) -> i64,
    s: impl Fn(&str, &str) -> String,
) -> Value {
    use Value::*;
    match (lhs, rhs) {
        (F64(lhs), rhs) => F64(d(*lhs, coerce_f64(rhs))),
        (lhs, F64(rhs)) => F64(d(coerce_f64(lhs), *rhs)),
        (I64(lhs), I64(rhs)) => I64(i(*lhs, *rhs)),
        (Str(lhs), Str(rhs)) => Str(s(lhs, rhs)),
        _ => panic!(
            "Can't operate on different types on {:?} and {:?}",
            lhs, rhs
        ),
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Value::*;
        match (self, other) {
            (F64(lhs), F64(rhs)) => lhs.partial_cmp(rhs),
            (I64(lhs), I64(rhs)) => lhs.partial_cmp(rhs),
            (Str(lhs), Str(rhs)) => lhs.partial_cmp(rhs),
            (I64(lhs), F64(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (F64(lhs), I64(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            _ => panic!("Can't compare different types"),
        }
    }
}

fn coerce_f64(a: &Value) -> f64 {
    match a {
        Value::F64(v) => *v,
        Value::I64(v) => *v as f64,
        Value::Str(_) => panic!("Can't coerce string to f64"),
    }
}

fn coerce_i64(a: &Value) -> i64 {
    match a {
        Value::F64(v) => *v as i64,
        Value::I64(v) => *v,
        Value::Str(_) => panic!("Can't coerce string to i64"),
    }
}

fn coerce_str(a: &Value) -> String {
    match a {
        Value::F64(v) => v.to_string(),
        Value::I64(v) => v.to_string(),
        Value::Str(v) => v.clone(),
    }
}

impl std::ops::Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        binary_op_str(&self, &rhs, f64::add, i64::add, |lhs, rhs| {
            lhs.to_owned() + rhs
        })
    }
}
impl std::ops::Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        binary_op_str(&self, &rhs, f64::sub, i64::sub, |_lhs, _rhs| {
            panic!("Can't subtract strings")
        })
    }
}
impl std::ops::Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        binary_op_str(&self, &rhs, f64::mul, i64::mul, |_lhs, _rhs| {
            panic!("Can't multiple strings")
        })
    }
}
impl std::ops::Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        binary_op_str(&self, &rhs, f64::div, i64::div, |_lhs, _rhs| {
            panic!("Can't divide strings")
        })
    }
}

type Statements<'a> = Vec<Statement<'a>>;

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    StrLiteral(String),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Gt(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    FnInvoke(&'src str, Vec<Expression<'src>>),
    If(
        Box<Expression<'src>>,
        Box<Statements<'src>>,
        Option<Box<Statements<'src>>>,
    ),
}

#[derive(Debug, PartialEq, Clone)]
enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
    For {
        loop_var: &'src str,
        start: Expression<'src>,
        end: Expression<'src>,
        stmts: Statements<'src>,
    },
    FnDef {
        name: &'src str,
        args: Vec<&'src str>,
        stmts: Statements<'src>,
    },
    Return(Expression<'src>),
    Break,
    Continue,
}

#[derive(Debug)]
enum BreakResult {
    Return(Value),
    Break,
    Continue,
}
type EvalResult = ControlFlow<BreakResult, Value>;

fn eval<'src>(expr: &Expression<'src>, frame: &mut StackFrame<'src>) -> EvalResult {
    use Expression::*;
    let res = match expr {
        Ident("pi") => Value::F64(std::f64::consts::PI),
        Ident(id) => frame.vars.get(*id).cloned().expect("variable not found"),
        NumLiteral(n) => Value::F64(*n),
        StrLiteral(s) => Value::Str(s.clone()),
        FnInvoke(name, args) => {
            let mut arg_vals = vec![];
            for arg in args {
                arg_vals.push(eval(arg, frame)?);
            }
            if let Some(func) = frame.get_fn(*name) {
                func.call(&arg_vals, frame)
            } else {
                panic!("Unknown function: {:?}", name);
            }
        }
        Add(lhs, rhs) => eval(lhs, frame)? + eval(rhs, frame)?,
        Sub(lhs, rhs) => eval(lhs, frame)? - eval(rhs, frame)?,
        Mul(lhs, rhs) => eval(lhs, frame)? * eval(rhs, frame)?,
        Div(lhs, rhs) => eval(lhs, frame)? / eval(rhs, frame)?,
        Gt(lhs, rhs) => {
            if eval(lhs, frame)? > eval(rhs, frame)? {
                Value::I64(1)
            } else {
                Value::I64(0)
            }
        }
        Lt(lhs, rhs) => {
            if eval(lhs, frame)? < eval(rhs, frame)? {
                Value::I64(1)
            } else {
                Value::I64(0)
            }
        }
        If(cond, t_case, f_case) => {
            if eval(cond, frame)? != Value::I64(0) {
                eval_statements(t_case, frame)?
            } else if let Some(f_case) = f_case {
                eval_statements(f_case, frame)?
            } else {
                Value::I64(0)
            }
        }
    };
    EvalResult::Continue(res)
}

fn statements(i: &str) -> IResult<&str, Statements> {
    let (i, stmts) = many0(statement)(i)?;
    Ok((i, stmts))
}

fn statement(i: &str) -> IResult<&str, Statement> {
    let terminator = move |x| char(';')(x);
    alt((
        var_def,
        var_assign,
        for_statement,
        fn_def_statement,
        terminated(return_statement, terminator),
        terminated(break_statement, terminator),
        terminated(continue_statement, terminator),
        terminated(expr_statement, terminator),
    ))(i)
}

fn for_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("for"))(i)?;
    let (i, loop_var) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("in"))(i)?;
    let (i, start) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(tag("to"))(i)?;
    let (i, end) = space_delimited(expr)(i)?;
    let (i, stmts) = delimited(open_brace, statements, close_brace)(i)?;
    Ok((
        i,
        Statement::For {
            loop_var,
            start,
            end,
            stmts,
        },
    ))
}

fn fn_def_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("fn"))(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, args) = delimited(
        tag("("),
        separated_list0(tag(","), space_delimited(identifier)),
        tag(")"),
    )(i)?;
    let (i, stmts) = delimited(open_brace, statements, close_brace)(i)?;
    println!("fn_def_statement: {:?}", i);
    Ok((i, Statement::FnDef { name, args, stmts }))
}

fn var_def(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("var"))(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("="))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(tag(";"))(i)?;
    Ok((i, Statement::VarDef(name, expr)))
}

fn var_assign(i: &str) -> IResult<&str, Statement> {
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("="))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(tag(";"))(i)?;
    Ok((i, Statement::VarAssign(name, expr)))
}

fn return_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("return"))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    Ok((i, Statement::Return(expr)))
}

fn break_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("break"))(i)?;
    Ok((i, Statement::Break))
}

fn continue_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("continue"))(i)?;
    Ok((i, Statement::Continue))
}

fn expr_statement(i: &str) -> IResult<&str, Statement> {
    let (i, expr) = expr(i)?;
    Ok((i, Statement::Expression(expr)))
}

fn expr(i: &str) -> IResult<&str, Expression> {
    alt((if_expr, cond_expr, num_expr))(i)
}

fn if_expr(i: &str) -> IResult<&str, Expression> {
    let (i, _) = space_delimited(tag("if"))(i)?;
    let (i, cond) = expr(i)?;
    let (i, t_case) = delimited(open_brace, statements, close_brace)(i)?;
    let (i, f_case) = opt(preceded(
        space_delimited(tag("else")),
        delimited(open_brace, statements, close_brace),
    ))(i)?;

    Ok((
        i,
        Expression::If(Box::new(cond), Box::new(t_case), f_case.map(Box::new)),
    ))
}

fn num_expr(i: &str) -> IResult<&str, Expression> {
    let (i, init) = term(i)?;

    fold_many0(
        pair(space_delimited(alt((char('+'), char('-')))), term),
        move || init.clone(),
        |acc, (_op, val): (char, Expression)| match _op {
            '+' => Expression::Add(Box::new(acc), Box::new(val)),
            '-' => Expression::Sub(Box::new(acc), Box::new(val)),
            _ => panic!(
                "sub or add expression should be here, '+' or '-' but got {:?}",
                _op
            ),
        },
    )(i)
}

fn term(input: &str) -> IResult<&str, Expression> {
    let (i, init) = factor(input)?;

    fold_many0(
        pair(space_delimited(alt((char('*'), char('/')))), factor),
        move || init.clone(),
        |acc, (_op, val): (char, Expression)| match _op {
            '*' => Expression::Mul(Box::new(acc), Box::new(val)),
            '/' => Expression::Div(Box::new(acc), Box::new(val)),
            _ => panic!(
                "Multiplicative Expression should be here, '*' or '/' but got {:?}",
                _op
            ),
        },
    )(i)
}

fn cond_expr(i: &str) -> IResult<&str, Expression> {
    let (i, lhs) = num_expr(i)?;
    let (i, op) = alt((tag(">"), tag("<")))(i)?;
    let (i, rhs) = num_expr(i)?;
    Ok((
        i,
        match op {
            ">" => Expression::Gt(Box::new(lhs), Box::new(rhs)),
            "<" => Expression::Lt(Box::new(lhs), Box::new(rhs)),
            _ => panic!("unexpected operator: {:?}", op),
        },
    ))
}

fn factor(input: &str) -> IResult<&str, Expression> {
    alt((str_literal, number_literal, func_call, ident, parens))(input)
}

fn func_call(input: &str) -> IResult<&str, Expression> {
    let (i, ident) = space_delimited(identifier)(input)?;
    let (i, args) = space_delimited(delimited(
        tag("("),
        many0(delimited(multispace0, expr, opt(tag(",")))),
        tag(")"),
    ))(i)?;
    Ok((i, Expression::FnInvoke(ident, args)))
}

fn open_brace(i: &str) -> IResult<&str, &str> {
    space_delimited(tag("{"))(i)
}

fn close_brace(i: &str) -> IResult<&str, &str> {
    space_delimited(tag("}"))(i)
}

fn parens(input: &str) -> IResult<&str, Expression> {
    delimited(tag("("), expr, tag(")"))(input)
}

fn number_literal(i: &str) -> IResult<&str, Expression> {
    let (r, v) = space_delimited(recognize_float)(i)?;
    Ok((
        r,
        Expression::NumLiteral(v.parse().map_err(|_| {
            nom::Err::Error(nom::error::Error {
                input: i,
                code: nom::error::ErrorKind::Digit,
            })
        })?),
    ))
}

fn str_literal(i: &str) -> IResult<&str, Expression> {
    let (r, _) = preceded(multispace0, char('\"'))(i)?;
    let (r, val) = many0(none_of("\""))(r)?;
    let (r, _) = terminated(char('"'), multispace0)(r)?;
    Ok((
        r,
        Expression::StrLiteral(
            val.iter()
                .collect::<String>()
                .replace("\\n", "\n")
                .replace("\\\\", "\\"),
        ),
    ))
}

fn ident(input: &str) -> IResult<&str, Expression> {
    let (r, v) = space_delimited(identifier)(input)?;
    Ok((r, Expression::Ident(v)))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn space_delimited<'src, O, E>(
    f: impl Parser<&'src str, O, E>,
) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
where
    E: ParseError<&'src str>,
{
    delimited(multispace0, f, multispace0)
}

fn unary_fn<'a>(f: fn(f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
        code: Box::new(move |args| {
            Value::F64(f(coerce_f64(
                args.into_iter().next().expect("functions missing argument"),
            )))
        }),
    })
}

fn binary_fn<'a>(f: fn(f64, f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
        code: Box::new(move |args| {
            let mut args = args.into_iter();
            let lhs = args.next().expect("function missing first argument");
            let rhs = args.next().expect("function missing second argument");
            Value::F64(f(coerce_f64(lhs), coerce_f64(rhs)))
        }),
    })
}
