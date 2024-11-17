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
    VarDef(&'src str, TypeDecl, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
    Return(Expression<'src>),
    Break,
    Continue,
    For {
        loop_var: &'src str,
        start: Expression<'src>,
        end: Expression<'src>,
        stmts: Statements<'src>,
    },
    FnDef {
        name: &'src str,
        args: Vec<(&'src str, TypeDecl)>,
        ret_type: TypeDecl,
        stmts: Statements<'src>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'src> {
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
    FnInvoke(&'src str, Vec<Expression<'src>>),
}

pub type Statements<'a> = Vec<Statement<'a>>;
