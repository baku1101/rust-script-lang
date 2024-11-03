use crate::{
    ast::{Expression, Statement, Statements, TypeDecl},
    value::{coerce_f64, coerce_i64, coerce_str, Value},
};
use std::{collections::HashMap, ops::ControlFlow, vec};

type Variables = HashMap<String, Value>;
type Functions<'src> = HashMap<String, FnDef<'src>>;

pub enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn<'src>),
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
                    .map(|(arg, val)| (arg.0.to_string(), val.clone()))
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
    args: Vec<(&'src str, TypeDecl)>,
    ret_type: TypeDecl,
    stmts: Statements<'src>,
}

struct NativeFn<'src> {
    args: Vec<(&'src str, TypeDecl)>,
    ret_type: TypeDecl,
    code: Box<dyn Fn(&[Value]) -> Value>,
}

fn unary_fn<'a>(f: fn(f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
        args: vec![("arg", TypeDecl::F64)],
        ret_type: TypeDecl::F64,
        code: Box::new(move |args| {
            Value::F64(f(coerce_f64(
                args.into_iter().next().expect("functions missing argument"),
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
            let lhs = args.next().expect("function missing first argument");
            let rhs = args.next().expect("function missing second argument");
            Value::F64(f(coerce_f64(lhs), coerce_f64(rhs)))
        }),
    })
}

#[derive(Default)]
pub struct StackFrame<'src> {
    vars: Variables,
    funcs: Functions<'src>,
    uplevel: Option<&'src StackFrame<'src>>,
}

impl<'src> StackFrame<'src> {
    pub fn new() -> Self {
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
                args: vec![("arg", TypeDecl::Any)],
                ret_type: TypeDecl::I64,
                code: Box::new(print),
            }),
        );
        funcs.insert(
            "dbg".to_string(),
            FnDef::Native(NativeFn {
                args: vec![("arg", TypeDecl::Any)],
                ret_type: TypeDecl::I64,
                code: Box::new(p_dbg),
            }),
        );
        funcs.insert(
            "i64".to_string(),
            FnDef::Native(NativeFn {
                args: vec![("arg", TypeDecl::Any)],
                ret_type: TypeDecl::I64,
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
                args: vec![("arg", TypeDecl::Any)],
                ret_type: TypeDecl::F64,
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
                args: vec![("arg", TypeDecl::Any)],
                ret_type: TypeDecl::Str,
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

#[derive(Debug)]
pub enum BreakResult {
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

pub fn eval_statements<'src>(stmts: &Statements<'src>, frame: &mut StackFrame<'src>) -> EvalResult {
    let mut result = EvalResult::Continue(Value::I64(0));
    for stmt in stmts {
        match stmt {
            Statement::Expression(expr) => {
                result = EvalResult::Continue(eval(expr, frame)?);
            }
            Statement::VarDef(name, _, expr) => {
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
            Statement::FnDef {
                name,
                args,
                ret_type,
                stmts,
            } => {
                frame.funcs.insert(
                    name.to_string(),
                    FnDef::User(UserFn {
                        args: args.clone(),
                        ret_type: ret_type.clone(),
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
