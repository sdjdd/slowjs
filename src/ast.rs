#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SourceLocation {
    pub start: Position,
    pub end: Position,
}

impl SourceLocation {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

#[derive(Debug)]
pub struct Program {
    pub body: Vec<StatementOrDirective>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementOrDirective {
    Statement(Statement),
    Directive(Directive),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Directive {
    pub expression: Literal,
    pub directive: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
    EmptyStatement,
    Declaration(Declaration),
    IfStatement(IfStatement),
    ReturnStatement(ReturnStatement),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression,
    pub loc: Option<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub declarations: Vec<VariableDeclarator>,
    pub kind: VariableDeclarationKind,
    pub loc: Option<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclarator {
    pub id: Pattern,
    pub init: Option<Expression>,
    pub loc: Option<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableDeclarationKind {
    Var,
}

pub type ProgramBody = Vec<StatementOrDirective>;

pub type FunctionBody = Vec<StatementOrDirective>;

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
    pub loc: Option<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    BinaryExpression(BinaryExpression),
    ObjectExpression(ObjectExpression),
    FunctionExpression(FunctionExpression),
    CallExpression(CallExpression),
}

impl Expression {
    pub fn loc(&self) -> Option<SourceLocation> {
        match self {
            Expression::Literal(lit) => lit.loc,
            Expression::Identifier(id) => id.loc,
            Expression::BinaryExpression(bin) => bin.loc,
            Expression::ObjectExpression(obj) => obj.loc,
            Expression::FunctionExpression(func) => func.loc,
            Expression::CallExpression(call) => call.loc,
        }
    }

    pub fn new_binary(operator: BinaryOperator, left: Expression, right: Expression) -> Self {
        Expression::BinaryExpression(BinaryExpression {
            operator,
            left: Box::new(left),
            right: Box::new(right),
            loc: None,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectExpression {
    pub properties: Vec<Property>,
    pub loc: Option<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub key: PropertyKey,
    pub value: Expression,
    pub kind: PropertyKind,
    pub loc: Option<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub loc: Option<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub value: LiteralValue,
    pub loc: Option<SourceLocation>,
}

impl Literal {
    pub fn new(value: LiteralValue, loc: Option<SourceLocation>) -> Self {
        Self { value, loc }
    }
}

impl ToString for Literal {
    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

impl ToString for LiteralValue {
    fn to_string(&self) -> String {
        match self {
            LiteralValue::Null => "null".to_string(),
            LiteralValue::Boolean(b) => b.to_string(),
            LiteralValue::Number(n) => n.to_string(),
            LiteralValue::String(s) => s.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /

    // Comparison
    Equal,         // ==
    NotEqual,      // !=
    LessThan,      // <
    LessThanEq,    // <=
    GreaterThan,   // >
    GreaterThanEq, // >=
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub loc: Option<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub test: Box<Expression>,
    pub consequent: Box<Statement>,
    pub alternate: Option<Box<Statement>>,
    pub loc: Option<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub id: Identifier,
    pub params: Vec<Pattern>,
    pub body: Vec<StatementOrDirective>,
    pub loc: Option<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionExpression {
    pub id: Option<Identifier>,
    pub params: Vec<Pattern>,
    pub body: Vec<StatementOrDirective>,
    pub loc: Option<SourceLocation>,
}

impl From<FunctionDeclaration> for FunctionExpression {
    fn from(decl: FunctionDeclaration) -> Self {
        FunctionExpression {
            id: Some(decl.id),
            params: decl.params,
            body: decl.body,
            loc: decl.loc,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Identifier(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub argument: Option<Expression>,
    pub loc: Option<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub loc: Option<SourceLocation>,
}
