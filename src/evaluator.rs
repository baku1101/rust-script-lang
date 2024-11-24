use crate::ast::{ExprEnum, Expression, Statement, Statements, TypeDecl};
use crate::bytecode::{coerce_f64, standard_functions, FnDef, Functions, UserFn};
use crate::typechecker::Span;
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
        Self {
            vars: Variables::new(),
            funcs: standard_functions(),
            uplevel: None,
        }
    }
    fn get_fn(&self, name: &str) -> Option<&FnDef> {
        self.funcs
            .get(name)
            .or_else(|| self.uplevel.and_then(|up| up.get_fn(name)))
    }
}

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

    pub fn args(&self) -> Vec<(Span<'src>, TypeDecl)> {
        match self {
            Self::User(user) => user.args.clone(),
            Self::Native(native) => native.args.clone(),
        }
    }

    pub fn ret_type(&self) -> TypeDecl {
        match self {
            Self::User(user) => user.ret_type.clone(),
            Self::Native(native) => native.ret_type.clone(),
        }
    }
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

#[derive(Debug)]
pub enum BreakResult {
    Return(Value),
    Break,
    Continue,
}

fn eval<'a>(expr: &Expression<'a>, frame: &mut StackFrame<'a>) -> EvalResult {
    use ExprEnum::*;
    let res = match &expr.expr {
        Number(n) => Value::F64(*n),
        Ident("pi") => Value::F64(std::f64::consts::PI),
        Ident(id) => frame
            .vars
            .get(&id.to_string())
            .expect("unknown variables")
            .to_owned(),
        Str(s) => Value::Str(s.clone()),
        If(cond, then, els) => {
            if eval(&cond, frame)? != Value::I64(0) {
                eval_statements(&then, frame)?
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
            if let Some(func) = frame.get_fn(name.into_fragment()) {
                func.call(&args, frame)
            } else {
                panic!("Unknown function {:?}", name);
            }
        }
        Add(a, b) => eval(&a, frame)? + eval(&b, frame)?,
        Mul(a, b) => eval(&a, frame)? * eval(&b, frame)?,
        Sub(a, b) => eval(&a, frame)? - eval(&b, frame)?,
        Div(a, b) => eval(&a, frame)? / eval(&b, frame)?,
        Eq(a, b) => Value::I64((eval(&a, frame)? == eval(&b, frame)?) as i64),
        Ne(a, b) => Value::I64((eval(&a, frame)? != eval(&b, frame)?) as i64),
        Gt(a, b) => Value::I64((eval(&a, frame)? > eval(&b, frame)?) as i64),
        Gte(a, b) => Value::I64((eval(&a, frame)? >= eval(&b, frame)?) as i64),
        Lt(a, b) => Value::I64((eval(&a, frame)? < eval(&b, frame)?) as i64),
        Lte(a, b) => Value::I64((eval(&a, frame)? <= eval(&b, frame)?) as i64),
    };
    EvalResult::Continue(res)
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
            Statement::VarDef { name, expr, .. } => {
                let eval_res = eval(expr, frame)?;
                frame.vars.insert(name.to_string(), eval_res);
            }
            Statement::VarAssign { name, expr, .. } => {
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
                ..
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
                        ret_type: ret_type.clone(),
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
