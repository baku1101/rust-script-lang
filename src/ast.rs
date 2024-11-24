use crate::parser::calc_offset;
use crate::typechecker::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDecl {
    Any,
    F64,
    I64,
    Str,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef {
        span: Span<'src>,
        name: Span<'src>,
        td: TypeDecl,
        expr: Expression<'src>,
    },
    VarAssign {
        span: Span<'src>,
        name: Span<'src>,
        expr: Expression<'src>,
    },
    Return(Expression<'src>),
    Break,
    Continue,
    For {
        span: Span<'src>,
        loop_var: Span<'src>,
        start: Expression<'src>,
        end: Expression<'src>,
        stmts: Statements<'src>,
    },
    FnDef {
        name: Span<'src>,
        args: Vec<(Span<'src>, TypeDecl)>,
        ret_type: TypeDecl,
        stmts: Statements<'src>,
    },
}

impl<'src> Statement<'src> {
    pub fn span(&self) -> Option<Span<'src>> {
        Some(match self {
            Statement::Expression(expr) => expr.span,
            Statement::VarDef { span, .. } => *span,
            Statement::VarAssign { span, .. } => *span,
            Statement::Return(expr) => expr.span,
            Statement::Break => return None,
            Statement::Continue => return None,
            Statement::For { span, .. } => *span,
            Statement::FnDef { name, stmts, .. } => calc_offset(*name, stmts.span()),
        })
    }
}

pub trait GetSpan<'a> {
    fn span(&self) -> Span<'a>;
}

pub type Statements<'a> = Vec<Statement<'a>>;

impl<'a> GetSpan<'a> for Statements<'a> {
    fn span(&self) -> Span<'a> {
        self.iter().find_map(|stmt| stmt.span()).unwrap()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprEnum<'src> {
    Ident(&'src str),
    If(
        Box<Expression<'src>>,
        Box<Statements<'src>>,
        Option<Box<Statements<'src>>>,
    ),
    Number(f64),
    Str(String),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Eq(Box<Expression<'src>>, Box<Expression<'src>>),
    Ne(Box<Expression<'src>>, Box<Expression<'src>>),
    Gt(Box<Expression<'src>>, Box<Expression<'src>>),
    Gte(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    Lte(Box<Expression<'src>>, Box<Expression<'src>>),
    FnInvoke(Span<'src>, Vec<Expression<'src>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression<'src> {
    pub(crate) expr: ExprEnum<'src>,
    pub(crate) span: Span<'src>,
}

impl<'src> Expression<'src> {
    pub fn new(expr: ExprEnum<'src>, span: Span<'src>) -> Self {
        Expression { expr, span }
    }
}
