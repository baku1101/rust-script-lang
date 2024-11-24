use std::collections::HashMap;

use crate::{
    ast::{Span, Statements, TypeDecl},
    value::{coerce_f64, coerce_i64, coerce_str, Value},
};

pub type Functions<'src> = HashMap<String, FnDef<'src>>;

pub struct NativeFn<'src> {
    pub args: Vec<(&'src str, TypeDecl)>,
    pub ret_type: TypeDecl,
    pub(crate) code: Box<dyn Fn(&[Value]) -> Value>,
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

impl<'src> UserFn<'src> {
    pub fn new(
        args: Vec<(Span<'src>, TypeDecl)>,
        ret_type: TypeDecl,
        stmts: Statements<'src>,
    ) -> Self {
        Self {
            args,
            ret_type,
            stmts,
        }
    }
}

pub fn standard_functions<'src>() -> Functions<'src> {
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
    funcs
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

fn print(args: &[Value]) -> Value {
    println!("print: {}", args[0]);
    Value::I64(0)
}

fn p_dbg(args: &[Value]) -> Value {
    println!("dbg: {:?}", args[0]);
    Value::I64(0)
}
