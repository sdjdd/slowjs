pub enum Value {
    Null,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug)]
pub enum Literal {
    Null,
}

pub enum Expression {
    Literal(Literal),
}

pub enum Statement {
    Expression(Expression),
}

pub struct Program {
    pub body: Vec<Statement>,
}
