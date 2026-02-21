pub struct Program {
    pub body: Vec<Statement>,
}

pub enum Statement {
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
    EmptyStatement,
}

impl Statement {
    pub fn new_expression(expression: Expression) -> Self {
        Statement::ExpressionStatement(ExpressionStatement { expression })
    }

    pub fn new_block(body: Vec<Statement>) -> Self {
        Statement::BlockStatement(BlockStatement { body })
    }
}

pub struct ExpressionStatement {
    pub expression: Expression,
}

pub struct BlockStatement {
    pub body: Vec<Statement>,
}

pub enum Expression {
    Literal(Literal),
    BinaryExpression(BinaryExpression),
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

pub enum BinaryOperator {
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
}
