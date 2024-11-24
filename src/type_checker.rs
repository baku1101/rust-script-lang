use std::collections::HashMap;

use crate::ast::{Expression, Statement, TypeDecl};
use crate::bytecode::{standard_functions, FnDef, UserFn};

pub fn type_check<'src>(
    stmts: &Vec<Statement<'src>>,
    ctx: &mut TypeCheckContext<'src>,
) -> Result<TypeDecl, TypeCheckError> {
    let mut res = TypeDecl::Any;
    for stmt in stmts {
        match stmt {
            Statement::Expression(expr) => {
                res = tc_expr(&expr, ctx)?;
            }
            Statement::VarDef(var, ty, expr) => {
                let expr_ty = tc_expr(expr, ctx)?;
                let coerce_ty = tc_coerce_type(&expr_ty, ty)?;
                ctx.vars.insert(*var, coerce_ty);
            }
            Statement::VarAssign(var, expr) => {
                let var_ty = ctx.get_var(*var).expect("Variable not found in scope");
                let expr_ty = tc_expr(expr, ctx)?;
                tc_coerce_type(&expr_ty, &var_ty)?;
            }
            Statement::FnDef {
                name,
                args,
                ret_type,
                stmts,
            } => {
                ctx.funcs.insert(
                    name.to_string(),
                    FnDef::User(UserFn::new(args.clone(), *ret_type, stmts.clone())),
                );
                let mut sub_ctx = TypeCheckContext::push_stack(ctx);
                for (arg, ty) in args.iter() {
                    sub_ctx.vars.insert(arg, *ty);
                }
                let last_stmt = type_check(stmts, &mut sub_ctx)?;
                tc_coerce_type(&last_stmt, ret_type)?;
            }
            Statement::For {
                loop_var,
                start,
                end,
                stmts,
            } => {
                tc_coerce_type(&tc_expr(start, ctx)?, &TypeDecl::I64)?;
                tc_coerce_type(&tc_expr(end, ctx)?, &TypeDecl::I64)?;
                ctx.vars.insert(loop_var, TypeDecl::I64);
                res = type_check(stmts, ctx)?;
            }
            Statement::Return(expr) => {
                return tc_expr(expr, ctx);
            }
            Statement::Break | Statement::Continue => (),
        };
    }

    Ok(res)
}

fn tc_expr<'src>(
    e: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src>,
) -> Result<TypeDecl, TypeCheckError> {
    use Expression::*;
    Ok(match &e {
        NumLiteral(_val) => TypeDecl::F64,
        StrLiteral(_val) => TypeDecl::Str,
        Ident(name) => ctx.get_var(name).ok_or_else(|| {
            TypeCheckError::new(format!("Variable {:?} not found in scope", name))
        })?,
        FnInvoke(fn_name, args) => {
            let args_ty = args
                .iter()
                .map(|arg| tc_expr(arg, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            let func = ctx.get_fn(fn_name).ok_or_else(|| {
                TypeCheckError::new(format!("Function {:?} not found in scope", fn_name))
            })?;
            let args_decl = func.args();
            for (arg_ty, decl) in args_ty.iter().zip(args_decl.iter()) {
                tc_coerce_type(arg_ty, &decl.1)?;
            }
            func.ret_type()
        }
        Add(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Add")?,
        Sub(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Sub")?,
        Mul(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Mul")?,
        Div(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Div")?,
        Lt(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "Lt")?,
        Gt(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "Gt")?,
        If(cond, true_branch, false_branch) => {
            tc_coerce_type(&tc_expr(cond, ctx)?, &TypeDecl::I64)?;
            let true_type = type_check(&true_branch, ctx)?;
            if let Some(false_type) = false_branch {
                let false_type = type_check(&false_type, ctx)?;
                tc_op_type(&true_type, &false_type).map_err(|_| {
                    TypeCheckError::new(format!(
                        "Incompatible types in if branches: {:?} and {:?}",
                        true_type, false_type
                    ))
                })?
            } else {
                true_type
            }
        }
    })
}

pub struct TypeCheckContext<'src> {
    vars: HashMap<&'src str, TypeDecl>,
    // NativeFnの名前は'srcによらないためStringで持つ
    funcs: HashMap<String, FnDef<'src>>,
    super_context: Option<&'src TypeCheckContext<'src>>,
}

impl<'src> TypeCheckContext<'src> {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: standard_functions(),
            super_context: None,
        }
    }

    fn get_var(&self, name: &str) -> Option<TypeDecl> {
        if let Some(val) = self.vars.get(name) {
            Some(val.clone())
        } else {
            None
        }
    }

    fn get_fn(&self, name: &str) -> Option<&FnDef<'src>> {
        if let Some(val) = self.funcs.get(name) {
            Some(val)
        } else if let Some(super_ctx) = self.super_context {
            super_ctx.get_fn(name)
        } else {
            None
        }
    }

    fn push_stack(super_ctx: &'src Self) -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            super_context: Some(super_ctx),
        }
    }
}

#[derive(Debug)]
pub struct TypeCheckError {
    msg: String,
}

impl<'src> std::fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg,)
    }
}

impl TypeCheckError {
    fn new(msg: String) -> Self {
        Self { msg }
    }
}

fn tc_coerce_type<'src>(value: &TypeDecl, target: &TypeDecl) -> Result<TypeDecl, TypeCheckError> {
    use TypeDecl::*;
    Ok(match (value, target) {
        (_, Any) => value.clone(),
        (Any, _) => target.clone(),
        (F64 | I64, F64) => F64,
        (F64, I64) => F64,
        (I64, I64) => I64,
        (Str, Str) => Str,
        _ => {
            return Err(TypeCheckError::new(format!(
                "{:?} cannot be assigned to {:?}",
                value, target
            )))
        }
    })
}

fn tc_binary_op<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src>,
    op: &str,
) -> Result<TypeDecl, TypeCheckError> {
    let lhs_ty = tc_expr(lhs, ctx)?;
    let rhs_ty = tc_expr(rhs, ctx)?;
    tc_op_type(&lhs_ty, &rhs_ty).map_err(|_| {
        TypeCheckError::new(format!(
            "Operation {op} between incompatible type: {:?} and type: {:?}",
            lhs_ty, rhs_ty
        ))
    })
}

fn tc_op_type(lhst: &TypeDecl, rhst: &TypeDecl) -> Result<TypeDecl, ()> {
    use TypeDecl::*;
    Ok(match (lhst, rhst) {
        (Any, _) => Any,
        (_, Any) => Any,
        (I64, I64) => I64,
        (I64 | F64, I64 | F64) => F64,
        (Str, Str) => Str,
        _ => return Err(()),
    })
}

fn tc_binary_cmp<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src>,
    op: &str,
) -> Result<TypeDecl, TypeCheckError> {
    let lhs_ty = tc_expr(lhs, ctx)?;
    let rhs_ty = tc_expr(rhs, ctx)?;
    tc_cmp_type(&lhs_ty, &rhs_ty).map_err(|_| {
        TypeCheckError::new(format!(
            "Operation {op} between incompatible type: {:?} and type: {:?}",
            lhs_ty, rhs_ty
        ))
    })
}

fn tc_cmp_type(lhst: &TypeDecl, rhst: &TypeDecl) -> Result<TypeDecl, ()> {
    use TypeDecl::*;
    Ok(match (lhst, rhst) {
        (Any, _) => Any,
        (_, Any) => Any,
        (I64, I64) => I64,
        (F64, F64) => I64,
        (Str, Str) => Str,
        _ => return Err(()),
    })
}
