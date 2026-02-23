pub struct Program {
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
    EmptyStatement,
    Declaration(Declaration),
    IfStatement(IfStatement),
    ReturnStatement(ReturnStatement),
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

#[derive(Debug)]
pub enum Declaration {
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub declarations: Vec<VariableDeclarator>,
    pub kind: VariableDeclarationKind,
}

#[derive(Debug)]
pub struct VariableDeclarator {
    pub id: Identifier,
    pub init: Option<Expression>,
}

#[derive(Debug)]
pub enum VariableDeclarationKind {
    Var,
}

#[derive(Debug)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct ObjectExpression {
    pub properties: Vec<Property>,
}

#[derive(Debug)]
pub struct Property {
    pub key: PropertyKey,
    pub value: Expression,
    pub kind: PropertyKind,
}

#[derive(Debug)]
pub enum PropertyKey {
    Literal(Literal),
    Identifier(Identifier),
}

#[derive(Debug, PartialEq)]
pub enum PropertyKind {
    Init,
    Get,
    Set,
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub enum Literal {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
}

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug)]
pub struct IfStatement {
    pub test: Box<Expression>,
    pub consequent: Box<Statement>,
    pub alternate: Option<Box<Statement>>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub id: Identifier,
    pub params: Vec<Pattern>,
    pub body: BlockStatement,
}

#[derive(Debug)]
pub struct FunctionExpression {
    pub id: Option<Identifier>,
    pub params: Vec<Pattern>,
    pub body: BlockStatement,
}

#[derive(Debug)]
pub enum Pattern {
    Identifier(Identifier),
}

pub type FunctionBody = BlockStatement;

#[derive(Debug)]
pub struct ReturnStatement {
    pub argument: Option<Expression>,
}

#[derive(Debug)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}
