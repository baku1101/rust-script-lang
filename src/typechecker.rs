use crate::parser::{Expression, FnDef, TypeDecl};
use std::collections::HashMap;

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

struct TypeCheckContext<'src> {
    vars: HashMap<&'src str, TypeDecl>,
    funcs: HashMap<String, FnDef<'src>>,
    super_context: Option<&'src TypeCheckContext<'src>>,
}

impl<'src> TypeCheckContext<'src> {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
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
}

#[derive(Debug)]
struct TypeCheckError {
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

fn tc_expr<'src, 'b>(
    e: &'b Expression<'src>,
    ctx: &mut TypeCheckContext<'src>,
) -> Result<TypeDecl, TypeCheckError> {
    use Expression::*;
    Ok(match &e {
        Number(_val) => TypeDecl::F64,
        Str(_val) => TypeDecl::Str,
        Ident(str) => ctx
            .get_var(str)
            .ok_or_else(|| TypeCheckError::new(format!("Variable {} not found in scope", str)))?,
        _ => todo!(),
    })
}
