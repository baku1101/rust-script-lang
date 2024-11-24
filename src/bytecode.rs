use crate::value::Value;
use std::collections::HashMap;

use crate::ast::{Statements, TypeDecl};
use crate::typechecker::Span;

pub type Functions<'src> = HashMap<String, FnDef<'src>>;

pub fn standard_functions<'src>() -> Functions<'src> {
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
            args: vec![(Span::new("arg"), TypeDecl::Any)],
            ret_type: TypeDecl::Any,
            code: Box::new(print),
        }),
    );
    funcs.insert(
        "puts".to_string(),
        FnDef::Native(NativeFn {
            args: vec![(Span::new("arg"), TypeDecl::Any)],
            ret_type: TypeDecl::Any,
            code: Box::new(puts_fn),
        }),
    );
    funcs.insert(
        "dbg".to_string(),
        FnDef::Native(NativeFn {
            args: vec![(Span::new("arg"), TypeDecl::Any)],
            ret_type: TypeDecl::Any,
            code: Box::new(p_dbg),
        }),
    );
    funcs.insert(
        "i64".to_string(),
        FnDef::Native(NativeFn {
            args: vec![(Span::new("arg"), TypeDecl::Any)],
            ret_type: TypeDecl::I64,
            code: Box::new(move |args| {
                Value::I64(coerce_i64(args.first().expect("function missing argument")))
            }),
        }),
    );
    funcs.insert(
        "f64".to_string(),
        FnDef::Native(NativeFn {
            args: vec![(Span::new("arg"), TypeDecl::Any)],
            ret_type: TypeDecl::F64,
            code: Box::new(move |args| {
                Value::F64(coerce_f64(args.first().expect("function missing argument")))
            }),
        }),
    );
    funcs.insert(
        "str".to_string(),
        FnDef::Native(NativeFn {
            args: vec![(Span::new("arg"), TypeDecl::Any)],
            ret_type: TypeDecl::Str,
            code: Box::new(move |args| {
                Value::Str(coerce_str(args.first().expect("function missing argument")))
            }),
        }),
    );
    funcs
}

pub fn unary_fn<'a>(f: fn(f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
        args: vec![
            (Span::new("lhs"), TypeDecl::F64),
            (Span::new("rhs"), TypeDecl::F64),
        ],
        ret_type: TypeDecl::F64,
        code: Box::new(move |args| {
            Value::F64(f(coerce_f64(
                args.into_iter().next().expect("function missing argument"),
            )))
        }),
    })
}
pub fn binary_fn<'a>(f: fn(f64, f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
        args: vec![
            (Span::new("lhs"), TypeDecl::F64),
            (Span::new("rhs"), TypeDecl::F64),
        ],
        ret_type: TypeDecl::F64,
        code: Box::new(move |args| {
            let mut args = args.into_iter();
            let lhs = coerce_f64(args.next().expect("function missing the first argument"));
            let rhs = coerce_f64(args.next().expect("function missing the first argument"));
            Value::F64(f(lhs, rhs))
        }),
    })
}

pub fn print(args: &[Value]) -> Value {
    println!("print: {}", args[0]);
    Value::I64(0)
}

pub fn puts_fn(args: &[Value]) -> Value {
    for arg in args {
        print!("{}", arg);
    }
    Value::F64(0.)
}

pub fn p_dbg(values: &[Value]) -> Value {
    println!("dbg: {:?}", values[0]);
    Value::I64(0)
}

pub fn coerce_f64(a: &Value) -> f64 {
    match a {
        Value::F64(v) => *v as f64,
        Value::I64(v) => *v as f64,
        _ => panic!("The string could not be parsed as f64"),
    }
}

pub fn coerce_i64(a: &Value) -> i64 {
    match a {
        Value::F64(v) => *v as i64,
        Value::I64(v) => *v as i64,
        _ => panic!("The string could not be parsed as i64"),
    }
}

pub fn coerce_str(a: &Value) -> String {
    match a {
        Value::F64(v) => v.to_string(),
        Value::I64(v) => v.to_string(),
        Value::Str(v) => v.clone(),
    }
}

pub enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn<'src>),
}

pub struct UserFn<'src> {
    pub args: Vec<(Span<'src>, TypeDecl)>,
    pub ret_type: TypeDecl,
    pub stmts: Statements<'src>,
}

pub struct NativeFn<'src> {
    pub args: Vec<(Span<'src>, TypeDecl)>,
    pub ret_type: TypeDecl,
    pub code: Box<dyn Fn(&[Value]) -> Value>,
}
