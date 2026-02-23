use std::rc::Rc;

use thiserror::Error;

use crate::ast::*;
use crate::vm::{JsFunction, JsValue, OpCode};

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("{0}")]
    SyntaxError(String),
}

pub struct Compiler {
    bytecode: Vec<OpCode>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
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
                self.compile_expression(&expr_stmt.expression)?;
            }
            Statement::Declaration(decl) => match decl {
                Declaration::VariableDeclaration(decl) => {
                    self.compile_variable_declaration(decl)?
                }
                Declaration::FunctionDeclaration(decl) => {
                    self.compile_function_declaration(decl)?
                }
            },
            Statement::EmptyStatement => {}
            Statement::ReturnStatement(stmt) => self.compile_return_statement(stmt)?,
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
                if let Expression::FunctionExpression(func) = init {
                    // Prepare a name for anonymous function
                    self.compile_function_expression(func, Some(&id.name))?;
                } else {
                    self.compile_expression(init)?;
                }
                self.emit(OpCode::SlowPushValue(JsValue::String(id.name.to_string())));
                self.emit(OpCode::SlowSetVar);
            }
        }

        Ok(())
    }

    fn compile_expression(&mut self, expr: &Expression) -> Result<(), CompilerError> {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal)?,
            Expression::BinaryExpression(bin_expr) => self.compile_binary_expression(bin_expr)?,
            Expression::Identifier(ident) => self.compile_identifier(ident)?,
            Expression::CallExpression(call) => self.compile_call_expression(call)?,
            Expression::FunctionExpression(func) => self.compile_function_expression(func, None)?,
            _ => unimplemented!(),
        }
        Ok(())
    }

    fn compile_identifier(&mut self, id: &Identifier) -> Result<(), CompilerError> {
        self.emit(OpCode::SlowPushValue(JsValue::String(id.name.to_string())));
        self.emit(OpCode::SlowGetVar);
        Ok(())
    }

    fn compile_literal(&mut self, literal: &Literal) -> Result<(), CompilerError> {
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
        Ok(())
    }

    fn compile_binary_expression(
        &mut self,
        bin_expr: &BinaryExpression,
    ) -> Result<(), CompilerError> {
        self.compile_expression(&bin_expr.left)?;
        self.compile_expression(&bin_expr.right)?;

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
        Ok(())
    }

    fn compile_function_declaration(
        &mut self,
        func: &FunctionDeclaration,
    ) -> Result<(), CompilerError> {
        let mut func_compiler = Compiler::new();
        for stmt in &func.body.body {
            func_compiler.compile_statement(stmt)?;
        }
        if !matches!(func_compiler.bytecode.last(), Some(OpCode::Return)) {
            func_compiler.emit(OpCode::PushUndefined);
            func_compiler.emit(OpCode::Return);
        }

        let func_val = JsValue::Function(Rc::new(JsFunction {
            name: func.id.name.clone(),
            params: func
                .params
                .iter()
                .map(|p| match p {
                    Pattern::Identifier(id) => id.name.clone(),
                })
                .collect(),
            bytecode: func_compiler.bytecode,
        }));

        self.emit(OpCode::SlowPushValue(JsValue::String(func.id.name.clone())));
        self.emit(OpCode::SlowDeclareVar);

        self.emit(OpCode::SlowPushValue(func_val));
        self.emit(OpCode::SlowPushValue(JsValue::String(func.id.name.clone())));
        self.emit(OpCode::SlowSetVar);

        Ok(())
    }

    fn compile_function_expression(
        &mut self,
        func: &FunctionExpression,
        inferred_name: Option<&String>,
    ) -> Result<(), CompilerError> {
        let mut func_compiler = Compiler::new();
        for stmt in &func.body.body {
            func_compiler.compile_statement(stmt)?;
        }
        if !matches!(func_compiler.bytecode.last(), Some(OpCode::Return)) {
            func_compiler.emit(OpCode::PushUndefined);
            func_compiler.emit(OpCode::Return);
        }

        let func_name = func
            .id
            .as_ref()
            .map(|i| i.name.clone())
            .or_else(|| inferred_name.cloned())
            .unwrap_or_default();

        let func_val = JsValue::Function(Rc::new(JsFunction {
            name: func_name,
            params: func
                .params
                .iter()
                .map(|p| match p {
                    Pattern::Identifier(id) => id.name.clone(),
                })
                .collect(),
            bytecode: func_compiler.bytecode,
        }));

        self.emit(OpCode::SlowPushValue(func_val));
        Ok(())
    }

    fn compile_call_expression(&mut self, call: &CallExpression) -> Result<(), CompilerError> {
        self.compile_expression(&call.callee)?;

        for arg in &call.arguments {
            self.compile_expression(arg)?;
        }

        self.emit(OpCode::Call(call.arguments.len()));
        Ok(())
    }

    fn compile_return_statement(&mut self, stmt: &ReturnStatement) -> Result<(), CompilerError> {
        match &stmt.argument {
            Some(expr) => self.compile_expression(expr)?,
            None => self.emit(OpCode::PushUndefined),
        }
        self.emit(OpCode::Return);
        Ok(())
    }
}

#[derive(Debug)]
pub struct CompileResult {
    pub bytecode: Vec<OpCode>,
}
