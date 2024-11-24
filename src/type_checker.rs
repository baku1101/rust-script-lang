use std::collections::HashMap;

use crate::ast::{calc_offset, ExprEnum, Expression, GetSpan, Span, Statement, TypeDecl};
use crate::bytecode::{standard_functions, FnDef, UserFn};

pub fn type_check<'src>(
    stmts: &Vec<Statement<'src>>,
    ctx: &mut TypeCheckContext<'src, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let mut res = TypeDecl::Any;
    for stmt in stmts {
        match stmt {
            Statement::Expression(expr) => {
                res = tc_expr(expr, ctx)?;
            }
            Statement::VarDef { name, td, expr, .. } => {
                let expr_ty = tc_expr(expr, ctx)?;
                let coerce_ty = tc_coerce_type(&expr_ty, td, expr.span)?;
                ctx.vars.insert(name, coerce_ty);
            }
            Statement::VarAssign { name, expr, .. } => {
                let var_ty = ctx.get_var(*name).expect("Variable not found in scope");
                let expr_ty = tc_expr(expr, ctx)?;
                tc_coerce_type(&expr_ty, &var_ty, expr.span)?;
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
                tc_coerce_type(&last_stmt, ret_type, stmts.span())?;
            }
            Statement::For {
                loop_var,
                start,
                end,
                stmts,
                ..
            } => {
                tc_coerce_type(&tc_expr(start, ctx)?, &TypeDecl::I64, start.span)?;
                tc_coerce_type(&tc_expr(end, ctx)?, &TypeDecl::I64, end.span)?;
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
    ctx: &mut TypeCheckContext<'src, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    use ExprEnum::*;
    Ok(match &e.expr {
        NumLiteral(_val) => TypeDecl::F64,
        StrLiteral(_val) => TypeDecl::Str,
        Ident(name) => ctx.get_var(*name).ok_or_else(|| {
            TypeCheckError::new(format!("Variable {:?} not found in scope", name), e.span)
        })?,
        FnInvoke(fn_name, args) => {
            let args_ty = args
                .iter()
                .map(|arg| tc_expr(arg, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            let func = ctx.get_fn(fn_name).ok_or_else(|| {
                TypeCheckError::new(format!("Function {:?} not found in scope", fn_name), e.span)
            })?;
            let args_decl = func.args();
            for ((arg, arg_ty), decl) in args.iter().zip(args_ty.iter()).zip(args_decl.iter()) {
                tc_coerce_type(arg_ty, &decl.1, arg.span)?;
            }
            func.ret_type()
        }
        Add(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Add", e.span)?,
        Sub(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Sub", e.span)?,
        Mul(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Mul", e.span)?,
        Div(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Div", e.span)?,
        Lt(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "Lt", e.span)?,
        Gt(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "Gt", e.span)?,
        If(cond, true_branch, false_branch) => {
            tc_coerce_type(&tc_expr(cond, ctx)?, &TypeDecl::I64, cond.span)?;
            let true_type = type_check(true_branch, ctx)?;
            if let Some(false_branch) = false_branch {
                let false_type = type_check(false_branch, ctx)?;
                tc_op_type(&true_type, &false_type).map_err(|_| {
                    let true_span = true_branch.span();
                    let false_span = false_branch.span();
                    TypeCheckError::new(
                        format!(
                            "Conditional expression doesn't have the \
                      compatible types in true and false branch: \
                      {:?} and {:?}",
                            true_type, false_type
                        ),
                        calc_offset(true_span, false_span),
                    )
                })?
            } else {
                true_type
            }
        }
    })
}

pub struct TypeCheckContext<'src, 'ctx> {
    vars: HashMap<&'src str, TypeDecl>,
    // NativeFnの名前は'srcによらないためStringで持つ
    funcs: HashMap<String, FnDef<'src>>,
    super_context: Option<&'ctx TypeCheckContext<'src, 'ctx>>,
}

impl<'src, 'ctx> TypeCheckContext<'src, 'ctx> {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: standard_functions(),
            super_context: None,
        }
    }

    fn get_var(&self, name: Span) -> Option<TypeDecl> {
        if let Some(val) = self.vars.get(*name) {
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

    fn push_stack(super_ctx: &'ctx Self) -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            super_context: Some(super_ctx),
        }
    }
}

#[derive(Debug)]
pub struct TypeCheckError<'src> {
    msg: String,
    span: Span<'src>,
}

impl<'src> std::fmt::Display for TypeCheckError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n location: {}:{}: {}",
            self.msg,
            self.span.location_line(),
            self.span.get_utf8_column(),
            self.span.fragment()
        )
    }
}

impl<'src> TypeCheckError<'src> {
    fn new(msg: String, span: Span<'src>) -> Self {
        Self { msg, span }
    }
}

fn tc_coerce_type<'src>(
    value: &TypeDecl,
    target: &TypeDecl,
    span: Span<'src>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    use TypeDecl::*;
    Ok(match (value, target) {
        (_, Any) => value.clone(),
        (Any, _) => target.clone(),
        (F64 | I64, F64) => F64,
        (F64, I64) => F64,
        (I64, I64) => I64,
        (Str, Str) => Str,
        _ => {
            return Err(TypeCheckError::new(
                format!("{:?} cannot be assigned to {:?}", value, target),
                span,
            ))
        }
    })
}

fn tc_binary_op<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src, '_>,
    op: &str,
    span: Span<'src>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let lhs_ty = tc_expr(lhs, ctx)?;
    let rhs_ty = tc_expr(rhs, ctx)?;
    tc_op_type(&lhs_ty, &rhs_ty).map_err(|_| {
        TypeCheckError::new(
            format!(
                "Operation {op} between incompatible type: {:?} and type: {:?}",
                lhs_ty, rhs_ty
            ),
            span,
        )
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
    ctx: &mut TypeCheckContext<'src, '_>,
    op: &str,
    span: Span<'src>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let lhs_ty = tc_expr(lhs, ctx)?;
    let rhs_ty = tc_expr(rhs, ctx)?;
    tc_cmp_type(&lhs_ty, &rhs_ty).map_err(|_| {
        TypeCheckError::new(
            format!(
                "Operation {op} between incompatible type: {:?} and type: {:?}",
                lhs_ty, rhs_ty
            ),
            span,
        )
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
