use crate::{
    ast::{Expression, Statement, Statements, TypeDecl},
    bytecode::{standard_functions, FnDef, Functions, UserFn},
    value::Value,
};
use std::{collections::HashMap, ops::ControlFlow, vec};

type Variables = HashMap<String, Value>;

#[derive(Default)]
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

    pub fn args(&self) -> Vec<(&'src str, TypeDecl)> {
        match self {
            Self::User(user_fn) => user_fn.args.clone(),
            Self::Native(native_fn) => native_fn.args.clone(),
        }
    }

    pub fn ret_type(&self) -> TypeDecl {
        match self {
            Self::User(user_fn) => user_fn.ret_type,
            Self::Native(native_fn) => native_fn.ret_type,
        }
    }
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
