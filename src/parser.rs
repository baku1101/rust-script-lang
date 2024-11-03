use std::{collections::HashMap, ops::ControlFlow};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, none_of},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded, terminated},
    IResult, Parser,
};

fn space_delimited<'src, O, E>(
    f: impl Parser<&'src str, O, E>,
) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
where
    E: ParseError<&'src str>,
{
    delimited(multispace0, f, multispace0)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, TypeDecl, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
    Return(Expression<'src>),
    Break,
    Continue,
    For {
        loop_var: &'src str,
        start: Expression<'src>,
        end: Expression<'src>,
        stmts: Statements<'src>,
    },
    FnDef {
        name: &'src str,
        args: Vec<(&'src str, TypeDecl)>,
        ret_type: TypeDecl,
        stmts: Statements<'src>,
    },
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeDecl {
    Any,
    F64,
    I64,
    Str,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    F64(f64),
    I64(i64),
    Str(String),
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

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::F64(a), Self::F64(b)) => a.partial_cmp(b),
            (Self::I64(a), Self::I64(b)) => a.partial_cmp(b),
            (Self::Str(a), Self::Str(b)) => a.partial_cmp(b),
            (Self::F64(a), Self::I64(b)) => a.partial_cmp(&(*b as f64)),
            (Self::I64(a), Self::F64(b)) => (*a as f64).partial_cmp(b),
            _ => None,
        }
    }
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

fn binary_op_str(
    lhs: &Value,
    rhs: &Value,
    d: impl Fn(f64, f64) -> f64,
    i: impl Fn(i64, i64) -> i64,
    s: impl Fn(&str, &str) -> String,
) -> Value {
    use Value::*;
    match (lhs, rhs) {
        (F64(a), b) => F64(d(*a, coerce_f64(b))),
        (a, F64(b)) => F64(d(coerce_f64(a), *b)),
        (I64(a), I64(b)) => I64(i(*a, *b)),
        (Str(a), Str(b)) => Str(s(a, b)),
        _ => {
            panic!("Unsupported operator between {:?} and {:?}", lhs, rhs)
        }
    }
}

fn coerce_f64(a: &Value) -> f64 {
    match a {
        Value::F64(v) => *v as f64,
        Value::I64(v) => *v as f64,
        _ => panic!("The string could not be parsed as f64"),
    }
}

fn coerce_i64(a: &Value) -> i64 {
    match a {
        Value::F64(v) => *v as i64,
        Value::I64(v) => *v as i64,
        _ => panic!("The string could not be parsed as i64"),
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
        binary_op_str(&self, &rhs, f64::sub, i64::sub, |_, _| {
            panic!("Unsupported operator - between strings")
        })
    }
}

impl std::ops::Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        binary_op_str(&self, &rhs, f64::mul, i64::mul, |_, _| {
            panic!("Unsupported operator * between strings")
        })
    }
}

impl std::ops::Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        binary_op_str(&self, &rhs, f64::div, i64::div, |_, _| {
            panic!("Unsupported operator / between strings")
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'src> {
    Ident(&'src str),
    If(
        Box<Expression<'src>>,
        Box<Statements<'src>>,
        Option<Box<Statements<'src>>>,
    ),
    Number(f64),
    Str(String),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Eq(Box<Expression<'src>>, Box<Expression<'src>>),
    Ne(Box<Expression<'src>>, Box<Expression<'src>>),
    Gt(Box<Expression<'src>>, Box<Expression<'src>>),
    Gte(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    Lte(Box<Expression<'src>>, Box<Expression<'src>>),
    FnInvoke(&'src str, Vec<Expression<'src>>),
}

pub enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn<'src>),
}

struct UserFn<'src> {
    args: Vec<(&'src str, TypeDecl)>,
    ret_type: TypeDecl,
    stmts: Statements<'src>,
}

struct NativeFn<'src> {
    args: Vec<(&'src str, TypeDecl)>,
    ret_type: TypeDecl,
    code: Box<dyn Fn(&[Value]) -> Value>,
}

impl<'src> FnDef<'src> {
    fn call(&self, args: &[Value], frame: &StackFrame) -> Value {
        match self {
            FnDef::User(user_fn) => {
                let mut new_frame = StackFrame {
                    vars: Variables::new(),
                    funcs: Functions::new(),
                    uplevel: Some(frame),
                };
                new_frame.vars = user_fn
                    .args
                    .iter()
                    .zip(args)
                    .map(|(name, value)| (name.0.to_string(), value.to_owned()))
                    .collect::<Variables>();
                match eval_statements(&user_fn.stmts, &mut new_frame) {
                    EvalResult::Continue(val) | EvalResult::Break(BreakResult::Return(val)) => val,
                    EvalResult::Break(BreakResult::Break) => panic!("break outside loop"),
                    EvalResult::Break(BreakResult::Continue) => panic!("continue outside loop"),
                }
            }
            FnDef::Native(native_fn) => (native_fn.code)(args),
        }
    }

    fn args(&self) -> &Vec<(&'src str, TypeDecl)> {
        match self {
            Self::User(user) => &user.args,
            Self::Native(code) => &code.args,
        }
    }

    fn ret_type(&self) -> &TypeDecl {
        match self {
            Self::User(user) => &user.ret_type,
            Self::Native(native) => &native.ret_type,
        }
    }
}

type Variables = HashMap<String, Value>;
type Functions<'src> = HashMap<String, FnDef<'src>>;
type Statements<'a> = Vec<Statement<'a>>;
type EvalResult = ControlFlow<BreakResult, Value>;

#[derive(Debug)]
pub enum BreakResult {
    Return(Value),
    Break,
    Continue,
}

pub struct StackFrame<'src> {
    vars: Variables,
    funcs: Functions<'src>,
    uplevel: Option<&'src StackFrame<'src>>,
}

impl<'src> StackFrame<'src> {
    pub fn new() -> Self {
        let mut funcs = Functions::new();
        funcs.insert("sqrt".to_string(), unary_fn(f64::sqrt));
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
                args: vec![("arg", TypeDecl::Any)],
                ret_type: TypeDecl::Any,
                code: Box::new(print),
            }),
        );
        funcs.insert(
            "puts".to_string(),
            FnDef::Native(NativeFn {
                args: vec![("arg", TypeDecl::Any)],
                ret_type: TypeDecl::Any,
                code: Box::new(puts_fn),
            }),
        );
        funcs.insert(
            "dbg".to_string(),
            FnDef::Native(NativeFn {
                args: vec![("arg", TypeDecl::Any)],
                ret_type: TypeDecl::Any,
                code: Box::new(p_dbg),
            }),
        );
        funcs.insert(
            "i64".to_string(),
            FnDef::Native(NativeFn {
                args: vec![("arg", TypeDecl::Any)],
                ret_type: TypeDecl::I64,
                code: Box::new(move |args| {
                    Value::I64(coerce_i64(args.first().expect("function missing argument")))
                }),
            }),
        );
        funcs.insert(
            "f64".to_string(),
            FnDef::Native(NativeFn {
                args: vec![("arg", TypeDecl::Any)],
                ret_type: TypeDecl::F64,
                code: Box::new(move |args| {
                    Value::F64(coerce_f64(args.first().expect("function missing argument")))
                }),
            }),
        );
        funcs.insert(
            "str".to_string(),
            FnDef::Native(NativeFn {
                args: vec![("arg", TypeDecl::Any)],
                ret_type: TypeDecl::Str,
                code: Box::new(move |args| {
                    Value::Str(coerce_str(args.first().expect("function missing argument")))
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
        self.funcs
            .get(name)
            .or_else(|| self.uplevel.and_then(|up| up.get_fn(name)))
    }
}

pub fn statements(i: &str) -> IResult<&str, Statements> {
    let (i, stmts) = many0(statement)(i)?;
    Ok((i, stmts))
}

fn statement(i: &str) -> IResult<&str, Statement> {
    alt((
        var_def,
        var_assign,
        fn_def_statement,
        for_statement,
        terminated(return_statement, char(';')),
        terminated(break_statement, char(';')),
        terminated(continue_statement, char(';')),
        terminated(expr_statement, char(';')),
    ))(i)
}

fn break_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("break"))(i)?;
    Ok((i, Statement::Break))
}

fn continue_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("continue"))(i)?;
    Ok((i, Statement::Continue))
}

fn return_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("return"))(i)?;
    let (i, expr) = expr(i)?;
    Ok((i, Statement::Return(expr)))
}

fn fn_def_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("fn"))(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("("))(i)?;
    let (i, args) = separated_list0(char(','), space_delimited(argument))(i)?;
    let (i, _) = space_delimited(tag(")"))(i)?;
    let (i, _) = space_delimited(tag("->"))(i)?;
    let (i, ret_type) = type_decl(i)?;
    let (i, stmts) = delimited(open_brace, statements, close_brace)(i)?;
    Ok((
        i,
        Statement::FnDef {
            name,
            args,
            ret_type,
            stmts,
        },
    ))
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

fn type_decl(i: &str) -> IResult<&str, TypeDecl> {
    let (i, td) = space_delimited(identifier)(i)?;
    Ok((
        i,
        match td {
            "i64" => TypeDecl::I64,
            "f64" => TypeDecl::F64,
            "str" => TypeDecl::Str,
            _ => {
                panic!("Type annotation has unknown type: {td}")
            }
        },
    ))
}

fn argument(i: &str) -> IResult<&str, (&str, TypeDecl)> {
    let (i, ident) = space_delimited(identifier)(i)?;
    let (i, _) = char(':')(i)?;
    let (i, td) = type_decl(i)?;

    Ok((i, (ident, td)))
}

fn var_def(i: &str) -> IResult<&str, Statement> {
    let (i, _) = delimited(multispace0, tag("var"), multispace1)(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char(':'))(i)?;
    let (i, td) = type_decl(i)?;
    let (i, _) = space_delimited(char('='))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(char(';'))(i)?;
    Ok((i, Statement::VarDef(name, td, expr)))
}

fn var_assign(i: &str) -> IResult<&str, Statement> {
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("="))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(tag(";"))(i)?;
    Ok((i, Statement::VarAssign(name, expr)))
}

fn expr_statement(i: &str) -> IResult<&str, Statement> {
    let (i, expr) = expr(i)?;
    Ok((i, Statement::Expression(expr)))
}

fn eval<'a>(expr: &Expression<'a>, frame: &mut StackFrame<'a>) -> EvalResult {
    use Expression::*;
    let res = match expr {
        Number(n) => Value::F64(*n),
        Ident("pi") => Value::F64(std::f64::consts::PI),
        Ident(id) => frame
            .vars
            .get(&id.to_string())
            .expect("unknown variables")
            .to_owned(),
        Str(s) => Value::Str(s.clone()),
        If(cond, then, els) => {
            if eval(cond, frame)? != Value::I64(0) {
                eval_statements(then, frame)?
            } else {
                els.as_ref()
                    .map_or(EvalResult::Continue(Value::I64(0)), |els| {
                        eval_statements(els, frame)
                    })?
            }
        }
        FnInvoke(name, args) => {
            let args: Vec<_> = args
                .iter()
                .map(|arg| eval(arg, frame))
                .map(|res| match res {
                    EvalResult::Continue(val) => val,
                    EvalResult::Break(BreakResult::Return(val)) => val,
                    EvalResult::Break(BreakResult::Break) => panic!("break in argument"),
                    EvalResult::Break(BreakResult::Continue) => panic!("continue in argument"),
                })
                .collect();
            if let Some(func) = frame.get_fn(*name) {
                func.call(&args, frame)
            } else {
                panic!("Unknown function {:?}", name);
            }
        }
        Add(a, b) => eval(a, frame)? + eval(b, frame)?,
        Mul(a, b) => eval(a, frame)? * eval(b, frame)?,
        Sub(a, b) => eval(a, frame)? - eval(b, frame)?,
        Div(a, b) => eval(a, frame)? / eval(b, frame)?,
        Eq(a, b) => Value::I64((eval(a, frame)? == eval(b, frame)?) as i64),
        Ne(a, b) => Value::I64((eval(a, frame)? != eval(b, frame)?) as i64),
        Gt(a, b) => Value::I64((eval(a, frame)? > eval(b, frame)?) as i64),
        Gte(a, b) => Value::I64((eval(a, frame)? >= eval(b, frame)?) as i64),
        Lt(a, b) => Value::I64((eval(a, frame)? < eval(b, frame)?) as i64),
        Lte(a, b) => Value::I64((eval(a, frame)? <= eval(b, frame)?) as i64),
    };
    EvalResult::Continue(res)
}

fn unary_fn<'a>(f: fn(f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
        args: vec![("lhs", TypeDecl::F64), ("rhs", TypeDecl::F64)],
        ret_type: TypeDecl::F64,
        code: Box::new(move |args| {
            Value::F64(f(coerce_f64(
                args.into_iter().next().expect("function missing argument"),
            )))
        }),
    })
}
fn binary_fn<'a>(f: fn(f64, f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
        args: vec![("lhs", TypeDecl::F64), ("rhs", TypeDecl::F64)],
        ret_type: TypeDecl::F64,
        code: Box::new(move |args| {
            let mut args = args.into_iter();
            let lhs = coerce_f64(args.next().expect("function missing the first argument"));
            let rhs = coerce_f64(args.next().expect("function missing the first argument"));
            Value::F64(f(lhs, rhs))
        }),
    })
}

fn expr(input: &str) -> IResult<&str, Expression> {
    alt((if_expr, cond_expr, num_expr))(input)
}

fn if_expr(i: &str) -> IResult<&str, Expression> {
    let (i, _) = space_delimited(tag("if"))(i)?;
    let (i, cond) = space_delimited(expr)(i)?;
    let (i, then) = delimited(open_brace, statements, close_brace)(i)?;
    let (i, els) = opt(preceded(
        space_delimited(tag("else")),
        delimited(open_brace, statements, close_brace),
    ))(i)?;
    Ok((
        i,
        Expression::If(Box::new(cond), Box::new(then), els.map(Box::new)),
    ))
}

fn open_brace(i: &str) -> IResult<&str, ()> {
    let (i, _) = space_delimited(tag("{"))(i)?;
    Ok((i, ()))
}

fn close_brace(i: &str) -> IResult<&str, ()> {
    let (i, _) = space_delimited(tag("}"))(i)?;
    Ok((i, ()))
}

fn cond_expr(i: &str) -> IResult<&str, Expression> {
    let (i, lhs) = space_delimited(term)(i)?;
    let (i, op) = alt((
        space_delimited(tag("==")),
        space_delimited(tag("!=")),
        space_delimited(tag(">=")),
        space_delimited(tag("<=")),
        space_delimited(tag(">")),
        space_delimited(tag("<")),
    ))(i)?;
    let (i, rhs) = space_delimited(term)(i)?;

    Ok((
        i,
        match op {
            "==" => Expression::Eq(Box::new(lhs), Box::new(rhs)),
            "!=" => Expression::Ne(Box::new(lhs), Box::new(rhs)),
            ">=" => Expression::Gte(Box::new(lhs), Box::new(rhs)),
            "<=" => Expression::Lte(Box::new(lhs), Box::new(rhs)),
            ">" => Expression::Gt(Box::new(lhs), Box::new(rhs)),
            "<" => Expression::Lt(Box::new(lhs), Box::new(rhs)),
            _ => unreachable!(),
        },
    ))
}

fn num_expr(input: &str) -> IResult<&str, Expression> {
    let (i, init) = expr_muldiv(input)?;
    fold_many0(
        pair(space_delimited(alt((char('+'), char('-')))), expr_muldiv),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| match op {
            '+' => Expression::Add(Box::new(acc), Box::new(val)),
            '-' => Expression::Sub(Box::new(acc), Box::new(val)),
            _ => unreachable!(),
        },
    )(i)
}

fn expr_muldiv(input: &str) -> IResult<&str, Expression> {
    let (i, init) = term(input)?;

    fold_many0(
        pair(space_delimited(alt((char('*'), char('/')))), term),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| match op {
            '*' => Expression::Mul(Box::new(acc), Box::new(val)),
            '/' => Expression::Div(Box::new(acc), Box::new(val)),
            _ => unreachable!(),
        },
    )(i)
}

fn term(input: &str) -> IResult<&str, Expression> {
    alt((paren, func_call, token))(input)
}

fn paren(input: &str) -> IResult<&str, Expression> {
    delimited(
        multispace0,
        delimited(tag("("), expr, tag(")")),
        multispace0,
    )(input)
}

fn token(i: &str) -> IResult<&str, Expression> {
    alt((ident, number, str))(i)
}

fn number(input: &str) -> IResult<&str, Expression> {
    let (r, v) = delimited(multispace0, recognize_float, multispace0)(input)?;
    Ok((
        r,
        Expression::Number(v.parse().map_err(|_| {
            nom::Err::Error(nom::error::Error {
                input,
                code: nom::error::ErrorKind::Digit,
            })
        })?),
    ))
}

fn str(i: &str) -> IResult<&str, Expression> {
    let (r, _) = preceded(multispace0, char('\"'))(i)?;
    let (r, val) = many0(none_of("\""))(r)?;
    let (r, _) = terminated(char('"'), multispace0)(r)?;
    Ok((
        r,
        Expression::Str(
            val.iter()
                .collect::<String>()
                .replace("\\\\", "\\")
                .replace("\\n", "\n"),
        ),
    ))
}

fn ident(input: &str) -> IResult<&str, Expression> {
    let (r, v) = delimited(multispace0, identifier, multispace0)(input)?;
    Ok((r, Expression::Ident(v)))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn func_call(input: &str) -> IResult<&str, Expression> {
    let (r, ident) = space_delimited(identifier)(input)?;
    let (r, args) = space_delimited(delimited(
        tag("("),
        many0(delimited(multispace0, expr, space_delimited(opt(tag(","))))),
        tag(")"),
    ))(r)?;
    Ok((r, Expression::FnInvoke(ident, args)))
}

pub fn eval_statements<'short, 'long: 'short>(
    statements: &[Statement<'long>],
    frame: &mut StackFrame<'short>,
) -> EvalResult {
    let mut last = EvalResult::Continue(Value::I64(0));
    for statement in statements {
        match statement {
            Statement::Expression(expr) => {
                last = EvalResult::Continue(eval(expr, frame)?);
            }
            Statement::VarDef(name, _, expr) => {
                let eval_res = eval(expr, frame)?;
                frame.vars.insert(name.to_string(), eval_res);
            }
            Statement::VarAssign(name, expr) => {
                let name = name.to_string();
                if !frame.vars.contains_key(&name) {
                    panic!("Variable not defined: {:?}", name);
                }
                let eval_res = eval(expr, frame)?;
                frame.vars.insert(name, eval_res);
            }
            Statement::For {
                loop_var,
                start,
                end,
                stmts,
            } => {
                let start = eval(start, frame)?
                    .as_i64()
                    .expect("start must be an integer");
                let end = eval(end, frame)?.as_i64().expect("end must be an integer");
                for i in start..end {
                    frame.vars.insert(loop_var.to_string(), Value::I64(i));
                    match eval_statements(stmts, frame) {
                        EvalResult::Break(BreakResult::Break) => break,
                        EvalResult::Break(BreakResult::Continue) => continue,
                        EvalResult::Break(BreakResult::Return(val)) => {
                            return EvalResult::Break(BreakResult::Return(val));
                        }
                        EvalResult::Continue(_) => {}
                    }
                }
            }
            Statement::FnDef {
                name,
                args,
                ret_type,
                stmts,
            } => {
                frame.funcs.insert(
                    name.to_string(),
                    FnDef::User(UserFn {
                        args: args.to_vec(),
                        ret_type: *ret_type,
                        stmts: stmts.to_vec(),
                    }),
                );
            }
            Statement::Return(expr) => {
                return EvalResult::Break(BreakResult::Return(eval(expr, frame)?));
            }
            Statement::Break => return EvalResult::Break(BreakResult::Break),
            Statement::Continue => return EvalResult::Break(BreakResult::Continue),
        };
    }
    last
}

fn print(args: &[Value]) -> Value {
    println!("print: {}", args[0]);
    Value::I64(0)
}

fn puts_fn(args: &[Value]) -> Value {
    for arg in args {
        print!("{}", arg);
    }
    Value::F64(0.)
}

fn p_dbg(values: &[Value]) -> Value {
    println!("dbg: {:?}", values[0]);
    Value::I64(0)
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_whtitespace() {
        assert_eq!(whitespace("  hello"), "hello");
    }

    #[test]
    fn test_number() {
        assert_eq!(number("+123 "), (" ", Some(Token::Number)));
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident("hello00"), ("", Some(Token::Ident)));
    }
}
