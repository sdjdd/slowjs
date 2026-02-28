use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::rc::Rc;

use thiserror::Error;

use crate::ast::*;
use crate::vm::{CodeBlock, ConstantPool, JsFunction, JsValue, OpCode};

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("{0} is not defined")]
    ReferenceError(String),
}

#[derive(Debug)]
struct Scope {
    variables: HashMap<String, usize>,
    next_slot: usize,
    lexical: bool,
}

impl Scope {
    fn new(lexical: bool, start: usize) -> Self {
        Self {
            lexical,
            next_slot: start,
            variables: HashMap::new(),
        }
    }
}

enum Variable {
    Global(usize),
    Local(usize),
}

pub struct Compiler {
    bytecode: Vec<OpCode>,
    constants: ConstantPool,
    scopes: Vec<Scope>,

    pub handle_directives: bool,
}

impl Compiler {
    pub fn new() -> Self {
        let mut compiler = Self {
            bytecode: Vec::new(),
            constants: Vec::new(),
            scopes: Vec::new(),
            handle_directives: true,
        };

        compiler.begin_scope();

        // Register builtin functions
        compiler.declare_variable("print");

        compiler
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Scope::new(false, 0));
    }

    fn begin_lexical_scope(&mut self) {
        let current_scope = self.scopes.last().unwrap();
        self.scopes.push(Scope::new(true, current_scope.next_slot));
    }

    fn end_scope(&mut self) -> usize {
        if self.scopes.len() > 1 {
            self.scopes.pop().map(|s| s.next_slot).unwrap_or(0)
        } else {
            self.scopes.last().map(|s| s.next_slot).unwrap_or(0)
        }
    }

    /// Create a `var` binding, no lexical scope
    fn declare_variable(&mut self, name: &str) -> Variable {
        let (idx, scope) = 'a: {
            for (idx, scope) in self.scopes.iter_mut().rev().enumerate() {
                if scope.lexical {
                    continue;
                }
                break 'a (idx, scope);
            }
            unreachable!("scope[0] should be not lexical")
        };

        let slot = match scope.variables.entry(name.to_string()) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let slot = scope.next_slot;
                scope.next_slot += 1;
                entry.insert(slot);

                // Update inner lexical scope
                for scope in self.scopes.iter_mut().skip(idx) {
                    if scope.lexical {
                        scope.next_slot += 1;
                        for slot in scope.variables.values_mut() {
                            *slot += 1;
                        }
                    } else {
                        break;
                    }
                }

                slot
            }
        };

        if idx == 0 {
            Variable::Global(slot)
        } else {
            Variable::Local(slot)
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
        self.bytecode.clear();

        let rest_stmts = self.compile_hoisted_statements(&program.body)?;
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
            Statement::IfStatement(stmt) => self.compile_if_statement(stmt)?,
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

    fn compile_if_statement(&mut self, stmt: &IfStatement) -> Result<(), CompilerError> {
        self.compile_expression(&stmt.test)?;

        let else_jump_offset = self.bytecode.len();
        self.bytecode.push(OpCode::JumpIfFalse(0));

        self.compile_statement(&stmt.consequent)?;

        let end_jump_offset = self.bytecode.len();
        self.bytecode.push(OpCode::Jump(0));

        let else_addr = self.bytecode.len();
        self.bytecode[else_jump_offset] = OpCode::JumpIfFalse(else_addr);

        if let Some(alternate) = &stmt.alternate {
            self.compile_statement(alternate)?;
        }

        let end_addr = self.bytecode.len();
        self.bytecode[end_jump_offset] = OpCode::Jump(end_addr);

        Ok(())
    }

    fn compile_variable_declaration(
        &mut self,
        decl: &VariableDeclaration,
    ) -> Result<(), CompilerError> {
        for VariableDeclarator { id, init, .. } in &decl.declarations {
            let var_name = match id {
                Pattern::Identifier(ident) => &ident.name,
            };

            let var = self.declare_variable(var_name);
            if let Some(init) = init {
                if let Expression::FunctionExpression(func) = init {
                    self.compile_function_expression(func, Some(var_name))?;
                } else {
                    self.compile_expression(init)?;
                }

                match var {
                    Variable::Global(slot) => {
                        self.emit(OpCode::SetGlobal(slot));
                    }
                    Variable::Local(slot) => {
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
            Expression::ObjectExpression(obj) => self.compile_object_expression(obj)?,
        }
        Ok(())
    }

    fn compile_identifier(&mut self, id: &Identifier) -> Result<(), CompilerError> {
        for (idx, scope) in self.scopes.iter().rev().enumerate() {
            if let Some(&slot) = scope.variables.get(&id.name) {
                if idx == 0 {
                    self.emit(OpCode::GetGlobal(slot));
                } else {
                    self.emit(OpCode::GetLocal(slot));
                }
                return Ok(());
            }
        }

        Err(CompilerError::ReferenceError(id.name.clone()))
    }

    fn compile_object_expression(&mut self, obj: &ObjectExpression) -> Result<(), CompilerError> {
        self.emit(OpCode::NewObject);

        for property in &obj.properties {
            let key = match &property.key {
                PropertyKey::Literal(lit) => JsValue::String(lit.to_string()),
                PropertyKey::Identifier(id) => JsValue::String(id.name.clone()),
            };
            let key_index = self.add_constant(key);

            self.compile_expression(&property.value)?;

            self.emit(OpCode::SetProperty(key_index));
        }

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
                self.bytecode.push(OpCode::Mul);
            }
            BinaryOperator::Divide => {
                self.bytecode.push(OpCode::Div);
            }
            BinaryOperator::Equal => {
                self.bytecode.push(OpCode::Eq);
            }
            BinaryOperator::NotEqual => {
                self.bytecode.push(OpCode::NotEq);
            }
            BinaryOperator::LessThan => {
                self.bytecode.push(OpCode::Less);
            }
            BinaryOperator::LessThanEq => {
                self.bytecode.push(OpCode::LessEq);
            }
            BinaryOperator::GreaterThan => {
                self.bytecode.push(OpCode::Greater);
            }
            BinaryOperator::GreaterThanEq => {
                self.bytecode.push(OpCode::GreaterEq);
            }
        }
        Ok(())
    }

    fn compile_function_declaration(
        &mut self,
        func: &FunctionDeclaration,
    ) -> Result<(), CompilerError> {
        let var = self.declare_variable(&func.id.name);
        self.compile_function_expression(&((*func).clone().into()), Some(&func.id.name))?;
        match var {
            Variable::Global(slot) => {
                self.emit(OpCode::SetGlobal(slot));
            }
            Variable::Local(slot) => {
                self.emit(OpCode::SetLocal(slot));
            }
        }
        Ok(())
    }

    fn compile_function_expression(
        &mut self,
        func: &FunctionExpression,
        func_name: Option<&String>,
    ) -> Result<(), CompilerError> {
        let mut compiler = Compiler::new();
        compiler.begin_lexical_scope();

        for param in &func.params {
            let param_name = match param {
                Pattern::Identifier(id) => id.name.clone(),
            };
            compiler.declare_variable(&param_name);
        }

        let rest_stmts = compiler.compile_hoisted_statements(&func.body)?;
        for stmt in &rest_stmts {
            compiler.compile_statement(stmt)?;
        }

        let bytecode = compiler.bytecode;

        let func_name = func
            .id
            .as_ref()
            .map(|id| id.name.clone())
            .or_else(|| func_name.cloned())
            .unwrap_or_default();

        let func_val = JsValue::Function(Rc::new(JsFunction {
            name: func_name,
            arity: func.params.len(),
            code_block: CodeBlock {
                code: bytecode,
                constants: compiler.constants,
            },
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

    fn compile_hoisted_statements<'a>(
        &mut self,
        body: &'a [StatementOrDirective],
    ) -> Result<Vec<&'a Statement>, CompilerError> {
        let mut rest_stmts = Vec::new();

        // Hoist [function | var] declarations
        for item in body {
            match item {
                StatementOrDirective::Statement(stmt) => {
                    self.handle_directives = false;
                    match stmt {
                        Statement::Declaration(decl) => match decl {
                            Declaration::FunctionDeclaration(decl) => {
                                self.compile_function_declaration(decl)?;
                            }
                            Declaration::VariableDeclaration(decl) => {
                                self.compile_variable_declaration(decl)?;
                            }
                        },
                        _ => rest_stmts.push(stmt),
                    }
                }
                StatementOrDirective::Directive(d) => {
                    if self.handle_directives {
                        // TODO
                        continue;
                    }
                    eprintln!("handle directive: {}", self.handle_directives);
                    self.compile_literal(&d.expression)?;
                }
            }
        }

        Ok(rest_stmts)
    }
}

#[derive(Debug)]
pub struct CompileResult {
    pub bytecode: Vec<OpCode>,
    pub constants: ConstantPool,
}
