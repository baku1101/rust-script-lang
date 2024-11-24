use crate::ast::{ExprEnum, Expression, GetSpan, Statement, TypeDecl};
use crate::bytecode::{standard_functions, FnDef, UserFn};
use nom_locate::LocatedSpan;
use std::collections::HashMap;

pub type Span<'src> = LocatedSpan<&'src str>;

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

pub struct TypeCheckContext<'src, 'ctx> {
    vars: HashMap<&'src str, TypeDecl>,
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

    pub fn get_var(&self, name: &'src str) -> Option<TypeDecl> {
        if let Some(val) = self.vars.get(&name) {
            Some(val.clone())
        } else {
            None
        }
    }

    pub fn get_fn(&self, name: &str) -> Option<&FnDef<'src>> {
        if let Some(val) = self.funcs.get(name) {
            Some(val)
        } else if let Some(super_ctx) = self.super_context {
            super_ctx.get_fn(name)
        } else {
            None
        }
    }

    pub fn push_stack(super_ctx: &'ctx Self) -> Self {
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
            "{}\nlocation: {}:{} :{}",
            self.msg,
            self.span.location_line(),
            self.span.get_utf8_column(),
            self.span.fragment()
        )
    }
}

impl<'src> TypeCheckError<'src> {
    pub fn new(msg: String, span: Span<'src>) -> Self {
        Self { msg, span }
    }
}

pub fn type_check<'src>(
    stmts: &Vec<Statement<'src>>,
    ctx: &mut TypeCheckContext<'src, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let mut res_ty = TypeDecl::Any;
    for stmt in stmts {
        res_ty = match stmt {
            Statement::VarDef {
                name: var,
                td: def_type,
                expr: expression,
                ..
            } => {
                let expr_ty = tc_expr(expression, ctx)?;
                let coerced_ty = tc_coerce_type(&expr_ty, def_type, expression.span)?;
                ctx.vars.insert(var.into_fragment(), coerced_ty.clone());
                TypeDecl::Any
            }
            Statement::VarAssign {
                span,
                name: var,
                expr: expression,
            } => {
                let expr_ty = tc_expr(expression, ctx)?;
                let var_ty = ctx.get_var(var.into_fragment()).ok_or_else(|| {
                    TypeCheckError::new(format!("{:?} not found in scope", var), expression.span)
                })?;
                tc_coerce_type(&expr_ty, &var_ty, *span)?;
                TypeDecl::Any
            }
            Statement::Expression(expr) => tc_expr(expr, ctx)?,
            Statement::FnDef {
                name,
                args,
                ret_type,
                stmts,
            } => {
                ctx.funcs.insert(
                    name.to_string(),
                    FnDef::User(UserFn {
                        args: args.clone(),
                        ret_type: ret_type.clone(),
                        stmts: stmts.clone(),
                    }),
                );
                let mut subctx = TypeCheckContext::push_stack(ctx);
                for (arg, ty) in args.iter() {
                    subctx.vars.insert(arg.into_fragment(), ty.clone());
                }
                let last_stmt = type_check(stmts, &mut subctx)?;
                tc_coerce_type(&last_stmt, &ret_type, stmts.span())?;
                TypeDecl::Any
            }
            Statement::For {
                loop_var,
                start,
                end,
                stmts,
                ..
            } => {
                let _ = tc_coerce_type(&tc_expr(start, ctx)?, &TypeDecl::I64, start.span);
                let _ = tc_coerce_type(&tc_expr(end, ctx)?, &TypeDecl::I64, end.span);
                ctx.vars.insert(loop_var.into_fragment(), TypeDecl::I64);
                type_check(stmts, ctx)?
            }
            Statement::Return(expr) => return Ok(tc_expr(expr, ctx)?),
            Statement::Break | Statement::Continue => TypeDecl::Any,
        }
    }
    Ok(res_ty)
}

fn tc_expr<'src>(
    e: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    use ExprEnum::*;
    Ok(match &e.expr {
        Number(_val) => TypeDecl::F64,
        Str(_val) => TypeDecl::Str,
        Ident(str) => ctx
            .get_var(*str)
            .ok_or_else(|| TypeCheckError::new(format!("{:?} not found in scope", str), e.span))?,
        FnInvoke(str, args) => {
            let args_ty = args
                .iter()
                .map(|v| tc_expr(v, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            let func = ctx.get_fn(str.into_fragment()).ok_or_else(|| {
                TypeCheckError::new(format!("Function {:?} is not defined", str), e.span)
            })?;
            let args_decl = func.args();
            for (arg_ty, decl) in args_ty.iter().zip(args_decl.iter()) {
                tc_coerce_type(arg_ty, &decl.1, e.span)?;
            }
            func.ret_type().clone()
        }
        Add(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Add")?,
        Sub(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Sub")?,
        Mul(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Mul")?,
        Div(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Div")?,
        Eq(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "Eq")?,
        Ne(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "Ne")?,
        Gt(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "Gt")?,
        Gte(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "Gte")?,
        Lt(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "Lt")?,
        Lte(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "Lte")?,
        If(cond, then, els) => {
            tc_coerce_type(&tc_expr(cond, ctx)?, &TypeDecl::I64, cond.span)?;
            let then_ty = type_check(then, ctx)?;
            if let Some(els_ty) = els {
                let els_ty = type_check(els_ty, ctx)?;
                tc_coerce_type(&then_ty, &els_ty, e.span)?
            } else {
                then_ty
            }
        }
    })
}

fn tc_binary_op<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src, '_>,
    op: &str,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let lhs_ty = tc_expr(lhs, ctx)?;
    let rhs_ty = tc_expr(rhs, ctx)?;
    binary_op_type(&lhs_ty, &rhs_ty).map_err(|_e| {
        TypeCheckError::new(
            format!(
                "Operation {op} between incompatible type: {:?} and {:?}",
                lhs_ty, rhs_ty
            ),
            rhs.span,
        )
    })
}

fn binary_op_type(lhs: &TypeDecl, rhs: &TypeDecl) -> Result<TypeDecl, ()> {
    use TypeDecl::*;
    Ok(match (lhs, rhs) {
        (Any, _) => Any,
        (_, Any) => Any,
        (F64, F64) => F64,
        (I64, I64) => I64,
        (Str, Str) => Str,
        (F64, I64) => F64,
        (I64, F64) => F64,
        _ => return Err(()),
    })
}

fn tc_binary_cmp<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src, '_>,
    op: &str,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let lhs_ty = tc_expr(lhs, ctx)?;
    let rhs_ty = tc_expr(rhs, ctx)?;
    binary_cmp_type(&lhs_ty, &rhs_ty).map_err(|_e| {
        TypeCheckError::new(
            format!(
                "Operation {op} between incompatible type: {:?} and {:?}",
                lhs_ty, rhs_ty
            ),
            rhs.span,
        )
    })
}

fn binary_cmp_type(lhs: &TypeDecl, rhs: &TypeDecl) -> Result<TypeDecl, ()> {
    use TypeDecl::*;
    Ok(match (lhs, rhs) {
        (Any, _) => Any,
        (_, Any) => Any,
        (F64, F64) => I64,
        (I64, I64) => I64,
        (Str, Str) => I64,
        _ => return Err(()),
    })
}
