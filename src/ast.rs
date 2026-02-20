pub enum Value {
    Null,
    String(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::String(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug)]
pub enum Literal {
    Null,
    String(String),
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
