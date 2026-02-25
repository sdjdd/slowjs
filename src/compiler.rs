use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::rc::Rc;

use thiserror::Error;

use crate::ast::*;
use crate::vm::{ConstantPool, JsFunction, JsValue, OpCode};

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("{0} is not defined")]
    ReferenceError(String),
}

#[derive(Debug)]
struct Scope {
    variables: HashMap<String, usize>,
    slot_count: usize,
    lexical: bool,
}

impl Scope {
    fn new(lexical: bool, start: usize) -> Self {
        Self {
            lexical,
            slot_count: start,
            variables: HashMap::new(),
        }
    }
}

pub struct Compiler {
    bytecode: Vec<OpCode>,
    constants: ConstantPool,
    scopes: Vec<Scope>,
}

impl Compiler {
    pub fn new() -> Self {
        let mut compiler = Self {
            bytecode: Vec::new(),
            constants: Vec::new(),
            scopes: Vec::new(),
        };

        compiler.begin_scope();

        compiler
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Scope::new(false, 0));
    }

    fn begin_lexical_scope(&mut self) {
        let current_scope = self.scopes.last().unwrap();
        self.scopes.push(Scope::new(true, current_scope.slot_count));
    }

    fn end_scope(&mut self) -> usize {
        if self.scopes.len() > 1 {
            self.scopes.pop().map(|s| s.slot_count).unwrap_or(0)
        } else {
            self.scopes.last().map(|s| s.slot_count).unwrap_or(0)
        }
    }

    /// Create a `var` binding, no lexical scope
    fn declare_variable(&mut self, name: &str) -> usize {
        let (idx, scope) = 'a: loop {
            for (idx, scope) in self.scopes.iter_mut().rev().enumerate() {
                if scope.lexical {
                    continue;
                }
                break 'a (idx, scope);
            }
            unreachable!()
        };

        match scope.variables.entry(name.to_string()) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let slot = scope.slot_count;
                scope.slot_count += 1;
                entry.insert(slot);

                // Fix inner lexical scope
                for scope in self.scopes.iter_mut().skip(idx) {
                    if scope.lexical {
                        scope.slot_count += 1;
                        for slot in scope.variables.values_mut() {
                            *slot += 1;
                        }
                    } else {
                        break;
                    }
                }

                slot
            }
        }
    }

    fn add_constant(&mut self, value: JsValue) -> usize {
        if matches!(value, JsValue::Function(_)) {
            let index = self.constants.len();
            self.constants.push(value);
            return index;
        }

        for (i, existing) in self.constants.iter().enumerate() {
            match (&value, existing) {
                (JsValue::String(a), JsValue::String(b)) if a == b => return i,
                (JsValue::Number(a), JsValue::Number(b)) if a == b => return i,
                (JsValue::Boolean(a), JsValue::Boolean(b)) if a == b => return i,
                (JsValue::Null, JsValue::Null) => return i,
                (JsValue::Undefined, JsValue::Undefined) => return i,
                _ => continue,
            }
        }
        let index = self.constants.len();
        self.constants.push(value);
        index
    }

    fn emit(&mut self, op: OpCode) {
        self.bytecode.push(op);
    }

    pub fn compile(&mut self, program: &Program) -> Result<CompileResult, CompilerError> {
        // Remove tailing Halt
        while let Some(OpCode::Halt) = self.bytecode.last() {
            self.bytecode.pop();
        }

        let mut rest_stmts = Vec::new();

        // Hoist [function | var] declarations
        for item in &program.body {
            match item {
                StatementOrDirective::Statement(stmt) => match stmt {
                    Statement::Declaration(decl) => match decl {
                        Declaration::FunctionDeclaration(decl) => {
                            self.compile_function_declaration(decl)?
                        }
                        Declaration::VariableDeclaration(decl) => {
                            self.compile_variable_declaration(decl)?
                        }
                    },
                    _ => rest_stmts.push(stmt),
                },
                _ => {} // Ignore directives now
            }
        }

        for stmt in rest_stmts {
            self.compile_statement(stmt)?;
        }

        self.bytecode.push(OpCode::Halt);

        let result = CompileResult {
            bytecode: self.bytecode.clone(),
            constants: self.constants.clone(),
        };

        Ok(result)
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
            Statement::BlockStatement(stmt) => self.compile_block_statement(stmt)?,
            _ => unimplemented!(),
        };
        Ok(())
    }

    fn compile_block_statement(&mut self, stmt: &BlockStatement) -> Result<(), CompilerError> {
        self.begin_lexical_scope();
        for stmt in &stmt.body {
            self.compile_statement(stmt)?;
        }
        self.end_scope();
        Ok(())
    }

    fn compile_variable_declaration(
        &mut self,
        decl: &VariableDeclaration,
    ) -> Result<(), CompilerError> {
        for VariableDeclarator { id, init } in &decl.declarations {
            let var_name = match id {
                Pattern::Identifier(ident) => &ident.name,
            };

            let slot = self.declare_variable(&var_name);
            if let Some(init) = init {
                if let Expression::FunctionExpression(func) = init {
                    self.compile_function_expression(func, Some(var_name))?;
                } else {
                    self.compile_expression(init)?;
                }
                self.emit(OpCode::SetLocal(slot));
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
        for scope in self.scopes.iter().rev() {
            if let Some(&slot) = scope.variables.get(&id.name) {
                self.emit(OpCode::GetLocal(slot));
                return Ok(());
            }
        }

        Err(CompilerError::ReferenceError(id.name.clone()))
    }

    fn compile_literal(&mut self, literal: &Literal) -> Result<(), CompilerError> {
        match literal {
            Literal::Null => self.emit(OpCode::PushNull),
            Literal::Boolean(b) => {
                if *b {
                    self.emit(OpCode::PushTrue);
                } else {
                    self.emit(OpCode::PushFalse);
                }
            }
            Literal::Number(n) => {
                let index = self.add_constant(JsValue::Number(*n));
                self.emit(OpCode::PushConstant(index));
            }
            Literal::String(s) => {
                let index = self.add_constant(JsValue::String(s.clone()));
                self.emit(OpCode::PushConstant(index));
            }
        }
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
        let slot = self.declare_variable(&func.id.name);
        self.compile_function_expression(&((*func).clone().into()), Some(&func.id.name))?;
        self.emit(OpCode::SetLocal(slot));
        Ok(())
    }

    fn compile_function_expression(
        &mut self,
        func: &FunctionExpression,
        func_name: Option<&String>,
    ) -> Result<(), CompilerError> {
        let mut func_compiler = Compiler::new();
        func_compiler.begin_scope();

        for param in &func.params {
            let param_name = match param {
                Pattern::Identifier(id) => id.name.clone(),
            };
            func_compiler.declare_variable(&param_name);
        }

        for stmt in &func.body.body {
            func_compiler.compile_statement(stmt)?;
        }
        if !matches!(func_compiler.bytecode.last(), Some(OpCode::Return)) {
            func_compiler.emit(OpCode::PushUndefined);
            func_compiler.emit(OpCode::Return);
        }

        let slot_count = func_compiler.end_scope();

        let mut final_bytecode = Vec::new();
        final_bytecode.push(OpCode::DeclareLocal(slot_count));
        final_bytecode.extend(func_compiler.bytecode);

        let func_name = func
            .id
            .as_ref()
            .map(|id| id.name.clone())
            .or_else(|| func_name.cloned())
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
            bytecode: final_bytecode,
            constants: func_compiler.constants,
        }));

        let func_index = self.add_constant(func_val);
        self.emit(OpCode::PushConstant(func_index));
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
    pub constants: ConstantPool,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;
    use crate::parser::parse;

    fn compile(program: &str) -> Compiler {
        let tokens = tokenize(program).unwrap();
        let program = parse(tokens).unwrap();
        let mut complier = Compiler::new();
        complier.compile(&program).unwrap();
        complier
    }

    #[test]
    fn test_variable_hoisting() {
        let program = r#"
            var a = 1;
            var a = 2;
        "#;

        let complier = compile(program);
        let scope = &complier.scopes[0];

        assert_eq!(scope.slot_count, 1);
        assert_eq!(scope.variables["a"], 0);
    }
}
