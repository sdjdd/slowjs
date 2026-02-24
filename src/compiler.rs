use std::collections::HashMap;
use std::rc::Rc;

use thiserror::Error;

use crate::ast::*;
use crate::vm::{ConstantPool, JsFunction, JsValue, OpCode};

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("{0}")]
    SyntaxError(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ScopeKind {
    Global,
    Function,
}

#[derive(Debug)]
struct Scope {
    variables: HashMap<String, usize>,
    slot_count: usize,
    kind: ScopeKind,
}

impl Scope {
    fn new(kind: ScopeKind) -> Self {
        Self {
            variables: HashMap::new(),
            slot_count: 0,
            kind,
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
        Self {
            bytecode: Vec::new(),
            constants: Vec::new(),
            scopes: Vec::new(),
        }
    }

    fn begin_scope(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope::new(kind));
    }

    fn end_scope(&mut self) -> usize {
        self.scopes.pop().map(|s| s.slot_count).unwrap_or(0)
    }

    fn declare_variable(&mut self, name: String) -> usize {
        let scope = self.scopes.last_mut().expect("No scope");
        let slot = scope.slot_count;
        scope.slot_count += 1;
        scope.variables.insert(name, slot);
        slot
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
        self.begin_scope(ScopeKind::Global);
        self.compile_program(program)?;
        self.end_scope();
        self.bytecode.push(OpCode::Halt);

        let result = CompileResult {
            bytecode: self.bytecode.clone(),
            constants: self.constants.clone(),
        };

        self.bytecode.clear();
        self.constants.clear();
        Ok(result)
    }

    fn compile_program(&mut self, program: &Program) -> Result<(), CompilerError> {
        for item in &program.body {
            match item {
                StatementOrDirective::Statement(stmt) => self.compile_statement(stmt)?,
                StatementOrDirective::Directive(_) => {}
            }
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
            let var_name = match id {
                Pattern::Identifier(ident) => &ident.name,
            };

            let scope_kind = self
                .scopes
                .last()
                .map(|s| s.kind)
                .unwrap_or(ScopeKind::Global);

            match scope_kind {
                ScopeKind::Global => {
                    let name_index = self.add_constant(JsValue::String(var_name.to_string()));
                    self.emit(OpCode::DeclareGlobal(name_index));
                    if let Some(init) = init {
                        if let Expression::FunctionExpression(func) = init {
                            self.compile_function_expression(func, Some(var_name))?;
                        } else {
                            self.compile_expression(init)?;
                        }
                        self.emit(OpCode::SetGlobal(name_index));
                    }
                }
                ScopeKind::Function => {
                    let slot = self.declare_variable(var_name.clone());
                    if let Some(init) = init {
                        if let Expression::FunctionExpression(func) = init {
                            self.compile_function_expression(func, Some(var_name))?;
                        } else {
                            self.compile_expression(init)?;
                        }
                        self.emit(OpCode::SetLocal(slot));
                    }
                }
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
        for (_depth, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(&slot) = scope.variables.get(&id.name) {
                match scope.kind {
                    ScopeKind::Global => {
                        break;
                    }
                    ScopeKind::Function => {
                        self.emit(OpCode::GetLocal(slot));
                    }
                }
                return Ok(());
            }
        }

        let index = self.add_constant(JsValue::String(id.name.to_string()));
        self.emit(OpCode::GetGlobal(index));
        Ok(())
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
        let name_index = self.add_constant(JsValue::String(func.id.name.clone()));
        self.emit(OpCode::DeclareGlobal(name_index));
        self.compile_function_expression(&((*func).clone().into()), None)?;
        self.emit(OpCode::SetGlobal(name_index));
        Ok(())
    }

    fn compile_function_expression(
        &mut self,
        func: &FunctionExpression,
        func_name: Option<&String>,
    ) -> Result<(), CompilerError> {
        let mut func_compiler = Compiler::new();
        func_compiler.begin_scope(ScopeKind::Function);

        for param in &func.params {
            let param_name = match param {
                Pattern::Identifier(id) => id.name.clone(),
            };
            func_compiler.declare_variable(param_name);
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
