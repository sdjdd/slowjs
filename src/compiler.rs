use std::collections::{BTreeMap, HashMap};

use thiserror::Error;

use crate::ast::*;
use crate::vm::{JsValue, OpCode, VmConstant};

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("{0}")]
    SyntaxError(String),
}

pub struct Compiler {
    constants: HashMap<VmConstant, usize>,
    next_const_idx: usize,
    bytecode: Vec<OpCode>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
            next_const_idx: 0,
            bytecode: Vec::new(),
        }
    }

    fn emit(&mut self, op: OpCode) {
        self.bytecode.push(op);
    }

    pub fn compile(&mut self, program: &Program) -> Result<CompileResult, CompilerError> {
        self.compile_program(program)?;
        self.bytecode.push(OpCode::Halt);

        let result = CompileResult {
            constants: self
                .constants
                .iter()
                .map(|(vc, idx)| (*idx, (*vc).clone()))
                .collect::<BTreeMap<_, _>>()
                .values()
                .map(|s| s.clone())
                .collect(),
            bytecode: self.bytecode.clone(),
        };

        self.bytecode.clear();
        Ok(result)
    }

    fn compile_program(&mut self, program: &Program) -> Result<(), CompilerError> {
        for stmt in &program.body {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement) -> Result<(), CompilerError> {
        match stmt {
            Statement::ExpressionStatement(expr_stmt) => {
                self.compile_expression(&expr_stmt.expression);
            }
            Statement::Declaration(decl) => match decl {
                Declaration::VariableDeclaration(decl) => {
                    self.compile_variable_declaration(decl)?
                }
            },
            Statement::EmptyStatement => {}
            _ => unimplemented!(),
        };
        Ok(())
    }

    fn compile_variable_declaration(
        &mut self,
        decl: &VariableDeclaration,
    ) -> Result<(), CompilerError> {
        for VariableDeclarator { id, init } in &decl.declarations {
            self.emit(OpCode::SlowPushValue(JsValue::String(id.name.to_string())));
            self.emit(OpCode::SlowDeclareVar);
            if let Some(init) = init {
                self.compile_expression(init);
                self.emit(OpCode::SlowPushValue(JsValue::String(id.name.to_string())));
                self.emit(OpCode::SlowLoadVar);
            }
        }

        Ok(())
    }

    fn compile_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::BinaryExpression(bin_expr) => self.compile_binary_expression(bin_expr),
            Expression::Identifier(ident) => self.compile_identifier(ident),
            _ => unimplemented!(),
        }
    }

    fn compile_identifier(&mut self, id: &Identifier) {
        self.emit(OpCode::SlowPushValue(JsValue::String(id.name.to_string())));
        self.emit(OpCode::SlowPushVar);
    }

    fn compile_literal(&mut self, literal: &Literal) {
        self.emit(match literal {
            Literal::Null => OpCode::PushNull,
            Literal::Boolean(b) => {
                if *b {
                    OpCode::PushTrue
                } else {
                    OpCode::PushFalse
                }
            }
            Literal::Number(n) => OpCode::SlowPushValue(JsValue::Number(*n)),
            Literal::String(s) => OpCode::SlowPushValue(JsValue::String(s.clone())),
        });
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

    pub fn add_constant(&mut self, value: VmConstant) -> usize {
        match self.constants.get(&value) {
            Some(idx) => *idx,
            None => {
                let idx = self.next_const_idx;
                self.constants.insert(value, idx);
                self.next_const_idx += 1;
                idx
            }
        }
    }
}

#[derive(Debug)]
pub struct CompileResult {
    pub constants: Vec<VmConstant>,
    pub bytecode: Vec<OpCode>,
}
