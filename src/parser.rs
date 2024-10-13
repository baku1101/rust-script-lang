use std::{collections::HashMap, io::Read};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded, terminated},
    IResult, Parser,
};

fn main() {
    let mut buf = String::new();
    if !std::io::stdin().read_to_string(&mut buf).is_ok() {
        panic!("Failed to read stdin");
    }
    let (_, parsed_statements) = match statements(&buf) {
        Ok(parsed_statements) => parsed_statements,
        Err(e) => {
            println!("Parsed error: {:?}", e);
            return;
        }
    };
    let mut frame = StackFrame::new();
    eval_statements(&parsed_statements, &mut frame);
}

fn eval_statements<'src>(stmts: &Statements<'src>, frame: &mut StackFrame<'src>) -> f64 {
    let mut result = 0.;
    for stmt in stmts {
        match stmt {
            Statement::Expression(expr) => {
                result = eval(expr, frame);
            }
            Statement::VarDef(name, expr) => {
                frame.vars.insert(name.to_string(), eval(expr, frame));
            }
            Statement::VarAssign(name, expr) => {
                if !frame.vars.contains_key(*name) {
                    println!("variable not found: {:?}", name);
                }
                frame.vars.insert(name.to_string(), eval(expr, frame));
            }
            Statement::For {
                loop_var,
                start,
                end,
                stmts,
            } => {
                let start = eval(start, frame) as isize;
                let end = eval(end, frame) as isize;
                for i in start..end {
                    frame.vars.insert(loop_var.to_string(), i as f64);
                    eval_statements(stmts, frame);
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
        }
    }
    result
}

enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn),
}

impl<'src> FnDef<'src> {
    fn call(&self, args: &[f64], frame: &StackFrame<'src>) -> f64 {
        match self {
            Self::User(user_fn) => {
                let mut new_frame = StackFrame::push_stack(frame);
                new_frame.vars = user_fn
                    .args
                    .iter()
                    .zip(args.iter())
                    .map(|(arg, val)| (arg.to_string(), *val))
                    .collect();
                eval_statements(&user_fn.stmts, &mut new_frame)
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
    code: Box<dyn Fn(&[f64]) -> f64>,
}

type Variables = HashMap<String, f64>;
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
        funcs.insert("print".to_string(), unary_fn(print));
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

fn print(arg: f64) -> f64 {
    println!("print: {:?}", arg);
    0.
}

type Statements<'a> = Vec<Statement<'a>>;

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    FnInvoke(&'src str, Vec<Expression<'src>>),
    If(
        Box<Expression<'src>>,
        Box<Expression<'src>>,
        Option<Box<Expression<'src>>>,
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
}

fn eval(expr: &Expression, frame: &StackFrame) -> f64 {
    use Expression::*;
    match expr {
        Ident("pi") => std::f64::consts::PI,
        Ident(id) => *frame.vars.get(*id).expect("variable not found"),
        NumLiteral(n) => *n,
        FnInvoke(name, args) => {
            if let Some(func) = frame.get_fn(*name) {
                let args: Vec<_> = args.iter().map(|arg| eval(arg, frame)).collect();
                func.call(&args, frame)
            } else {
                panic!("Unknown function: {:?}", name);
            }
        }
        Add(lhs, rhs) => eval(lhs, frame) + eval(rhs, frame),
        Sub(lhs, rhs) => eval(lhs, frame) - eval(rhs, frame),
        Mul(lhs, rhs) => eval(lhs, frame) * eval(rhs, frame),
        Div(lhs, rhs) => eval(lhs, frame) / eval(rhs, frame),
        If(cond, t_case, f_case) => {
            if eval(cond, frame) != 0. {
                eval(t_case, frame)
            } else if let Some(f_case) = f_case {
                eval(f_case, frame)
            } else {
                0.
            }
        }
    }
}

fn statements(i: &str) -> IResult<&str, Statements> {
    let (i, stmts) = many0(statement)(i)?;
    Ok((i, stmts))
}

fn statement(i: &str) -> IResult<&str, Statement> {
    alt((
        for_statement,
        fn_def_statement,
        terminated(alt((var_def, var_assign, expr_statement)), char(';')),
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
    Ok((i, Statement::FnDef { name, args, stmts }))
}

fn var_def(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("var"))(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("="))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    Ok((i, Statement::VarDef(name, expr)))
}

fn var_assign(i: &str) -> IResult<&str, Statement> {
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("="))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    Ok((i, Statement::VarAssign(name, expr)))
}

fn expr_statement(i: &str) -> IResult<&str, Statement> {
    let (i, expr) = expr(i)?;
    Ok((i, Statement::Expression(expr)))
}

fn expr(i: &str) -> IResult<&str, Expression> {
    alt((if_expr, num_expr))(i)
}

fn if_expr(i: &str) -> IResult<&str, Expression> {
    let (i, _) = space_delimited(tag("if"))(i)?;
    let (i, cond) = expr(i)?;
    let (i, t_case) = delimited(open_brace, expr, close_brace)(i)?;
    let (i, f_case) = opt(preceded(
        space_delimited(tag("else")),
        delimited(open_brace, expr, close_brace),
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

fn factor(input: &str) -> IResult<&str, Expression> {
    alt((number, func_call, ident, parens))(input)
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

fn number(input: &str) -> IResult<&str, Expression> {
    let (r, v) = space_delimited(recognize_float)(input)?;
    Ok((
        r,
        Expression::NumLiteral(v.parse().map_err(|_| {
            nom::Err::Error(nom::error::Error {
                input,
                code: nom::error::ErrorKind::Digit,
            })
        })?),
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
            f(*args.into_iter().next().expect("functions missing argument"))
        }),
    })
}

fn binary_fn<'a>(f: fn(f64, f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
        code: Box::new(move |args| {
            let mut args = args.into_iter();
            let lhs = *args.next().expect("function missing first argument");
            let rhs = *args.next().expect("function missing second argument");
            f(lhs, rhs)
        }),
    })
}
