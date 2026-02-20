pub enum Value {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Boolean(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Value::Number(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{:.0}", n)
                } else {
                    write!(f, "{}", n)
                }
            }
            Value::String(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug)]
pub enum Literal {
    Null,
    Boolean(bool),
    Number(f64),
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
