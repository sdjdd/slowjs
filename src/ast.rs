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

pub enum Expression {
    Literal(Literal),
    // TODO: Replace Box with Rc?
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
}

pub enum Statement {
    Expression(Expression),
}

pub struct Program {
    pub body: Vec<Statement>,
}
