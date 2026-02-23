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
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

#[derive(Debug)]
pub enum Declaration {
    VariableDeclaration(VariableDeclaration),
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
