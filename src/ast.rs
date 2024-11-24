use nom::{InputTake, Offset};
use nom_locate::LocatedSpan;
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TypeDecl {
    Any,
    F64,
    I64,
    Str,
}

pub type Statements<'a> = Vec<Statement<'a>>;
pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Clone)]
pub struct Expression<'src> {
    pub(crate) expr: ExprEnum<'src>,
    pub(crate) span: Span<'src>,
}

impl<'src> Expression<'src> {
    pub fn new(expr: ExprEnum<'src>, span: Span<'src>) -> Self {
        Self { expr, span }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprEnum<'src> {
    Ident(Span<'src>),
    NumLiteral(f64),
    StrLiteral(String),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Gt(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    FnInvoke(Span<'src>, Vec<Expression<'src>>),
    If(
        Box<Expression<'src>>,
        Box<Statements<'src>>,
        Option<Box<Statements<'src>>>,
    ),
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
    Return(Expression<'src>),
    Break,
    Continue,
}

impl<'src> Statement<'src> {
    pub fn span(&self) -> Option<Span<'src>> {
        use Statement::*;
        Some(match self {
            Expression(expr) => expr.span,
            VarDef { span, .. } => *span,
            VarAssign { span, .. } => *span,
            For { loop_var, .. } => *loop_var,
            FnDef { name, stmts, .. } => calc_offset(*name, stmts.span()),
            Return(expr) => expr.span,
            Break | Continue => return None,
        })
    }
}
pub trait GetSpan<'a> {
    fn span(&self) -> Span<'a>;
}

impl<'a> GetSpan<'a> for Statements<'a> {
    fn span(&self) -> Span<'a> {
        self.iter().find_map(|stmt| stmt.span()).unwrap()
    }
}

pub fn calc_offset<'a>(i: Span<'a>, r: Span<'a>) -> Span<'a> {
    i.take(i.offset(&r))
}
