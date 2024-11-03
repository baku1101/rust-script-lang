use std::collections::HashMap;

use crate::parser::{Expression, FnDef, TypeDecl};

// fn tc_expr<'src>(
//     e: &Expression<'src>,
//     ctx: &mut TypeCheckerContext<'src>,
// ) -> Result<TypeDecl, TypeCheckError> {
// }

pub struct TypeCheckerContext<'src> {
    vars: HashMap<&'src str, TypeDecl>,
    funcs: HashMap<&'src str, FnDef<'src>>,
    super_context: Option<&'src TypeCheckerContext<'src>>,
}

impl<'src> TypeCheckerContext<'src> {
    pub fn new(source_file: Option<&'src str>) -> Self {
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
