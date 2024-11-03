use crate::ast::{Expression, Statement, Statements, TypeDecl};
use crate::value::Value;
use std::collections::HashMap;
use std::ops::ControlFlow;
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

type Functions<'src> = HashMap<String, FnDef<'src>>;
type Variables = HashMap<String, Value>;
type EvalResult = ControlFlow<BreakResult, Value>;

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

pub enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn<'src>),
}

pub struct UserFn<'src> {
    args: Vec<(&'src str, TypeDecl)>,
    ret_type: TypeDecl,
    stmts: Statements<'src>,
}

pub struct NativeFn<'src> {
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

#[derive(Debug)]
pub enum BreakResult {
    Return(Value),
    Break,
    Continue,
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
