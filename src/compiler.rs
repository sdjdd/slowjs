use crate::ast::*;
use crate::vm::{JsValue, OpCode};

pub struct Compiler {
    constants: Vec<JsValue>,
    bytecode: Vec<OpCode>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            bytecode: Vec::new(),
        }
    }

    pub fn compile(&mut self, program: &Program) -> CompileResult {
        self.compile_program(program);
        self.bytecode.push(OpCode::Halt);

        let result = CompileResult {
            constants: self.constants.clone(),
            bytecode: self.bytecode.clone(),
        };

        self.bytecode.clear();
        result
    }

    fn compile_program(&mut self, program: &Program) {
        for stmt in &program.body {
            self.compile_statement(stmt);
        }
    }

    fn compile_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::ExpressionStatement(expr_stmt) => {
                self.compile_expression(&expr_stmt.expression);
            }
            _ => unimplemented!(),
        }
    }

    fn compile_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::BinaryExpression(bin_expr) => self.compile_binary_expression(bin_expr),
            _ => unimplemented!(),
        }
    }

    fn compile_literal(&mut self, literal: &Literal) {
        let value = match literal {
            Literal::Number(n) => JsValue::Number(*n),
            Literal::Null => JsValue::Null,
            Literal::Boolean(b) => JsValue::Boolean(*b),
            Literal::String(s) => JsValue::String(s.clone()),
        };
        let idx = self.add_constant(value);
        self.bytecode.push(OpCode::PushConst(idx));
    }

    fn compile_binary_expression(&mut self, bin_expr: &BinaryExpression) {
        self.compile_expression(&bin_expr.left);
        self.compile_expression(&bin_expr.right);

        match bin_expr.operator {
            BinaryOperator::Add => {
                self.bytecode.push(OpCode::Add);
            }
            BinaryOperator::Subtract => {
                self.bytecode.push(OpCode::Sub);
            }
            BinaryOperator::Multiply => {
                unimplemented!("multiply operator");
            }
            BinaryOperator::Divide => {
                unimplemented!("divide operator");
            }
        }
    }

    fn add_constant(&mut self, value: JsValue) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}

pub struct CompileResult {
    pub constants: Vec<JsValue>,
    pub bytecode: Vec<OpCode>,
}
