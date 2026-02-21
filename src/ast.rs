pub struct Program {
    pub body: Vec<Statement>,
}

pub enum Statement {
    ExpressionStatement { expression: Expression },
}

pub enum Expression {
    Literal(Literal),
    BinaryExpression {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
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
