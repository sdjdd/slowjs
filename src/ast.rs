#[derive(Debug)]
pub struct Program {
    pub body: Vec<StatementOrDirective>,
}

#[derive(Debug, Clone)]
pub enum StatementOrDirective {
    Statement(Statement),
    Directive(Directive),
}

#[derive(Debug, Clone)]
pub struct Directive {
    pub expression: Literal,
    pub directive: String,
}

#[derive(Debug, Clone)]
pub enum Statement {
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
    EmptyStatement,
    Declaration(Declaration),
    IfStatement(IfStatement),
    ReturnStatement(ReturnStatement),
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub declarations: Vec<VariableDeclarator>,
    pub kind: VariableDeclarationKind,
}

#[derive(Debug, Clone)]
pub struct VariableDeclarator {
    pub id: Pattern,
    pub init: Option<Expression>,
}

#[derive(Debug, Clone)]
pub enum VariableDeclarationKind {
    Var,
}

pub type ProgramBody = Vec<StatementOrDirective>;

pub type FunctionBody = Vec<StatementOrDirective>;

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    BinaryExpression(BinaryExpression),
    ObjectExpression(ObjectExpression),
    FunctionExpression(FunctionExpression),
    CallExpression(CallExpression),
}

impl Expression {
    pub fn new_binary(operator: BinaryOperator, left: Expression, right: Expression) -> Self {
        Expression::BinaryExpression(BinaryExpression {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        })
    }
}

#[derive(Debug, Clone)]
pub struct ObjectExpression {
    pub properties: Vec<Property>,
}

#[derive(Debug, Clone)]
pub struct Property {
    pub key: PropertyKey,
    pub value: Expression,
    pub kind: PropertyKind,
}

#[derive(Debug, Clone)]
pub enum PropertyKey {
    Literal(Literal),
    Identifier(Identifier),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PropertyKind {
    Init,
    Get,
    Set,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub test: Box<Expression>,
    pub consequent: Box<Statement>,
    pub alternate: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub id: Identifier,
    pub params: Vec<Pattern>,
    pub body: Vec<StatementOrDirective>,
}

#[derive(Debug, Clone)]
pub struct FunctionExpression {
    pub id: Option<Identifier>,
    pub params: Vec<Pattern>,
    pub body: Vec<StatementOrDirective>,
}

impl From<FunctionDeclaration> for FunctionExpression {
    fn from(decl: FunctionDeclaration) -> Self {
        FunctionExpression {
            id: Some(decl.id),
            params: decl.params,
            body: decl.body,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Identifier(Identifier),
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub argument: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}
