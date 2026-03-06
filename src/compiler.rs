use std::rc::Rc;

use thiserror::Error;

use crate::ast::*;
use crate::runtime::{CodeBlock, ConstantTable};
use crate::vm::{Constant, ConstantPool, FunctionTemplate, OpCode};

#[derive(Debug, Error)]
pub enum CompilerError {}

pub struct Compiler {
    bytecode: Vec<u8>,
    constants: ConstantPool,

    handle_directives: bool,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            bytecode: Vec::new(),
            constants: Vec::new(),
            handle_directives: true,
        }
    }

    pub fn reset(&mut self) {
        self.bytecode.clear();
    }

    fn add_constant(&mut self, value: Constant) -> usize {
        for (i, existing) in self.constants.iter().enumerate() {
            if *existing == value {
                return i;
            }
        }
        let index = self.constants.len();
        self.constants.push(value);
        index
    }

    fn emit(&mut self, op: OpCode) -> usize {
        self.bytecode.push(op.into());
        self.bytecode.len() - 1
    }

    fn emit_u8(&mut self, value: u8) -> usize {
        self.bytecode.push(value);
        self.bytecode.len() - 1
    }

    fn emit_u16(&mut self, value: u16) -> usize {
        let bytes = u16::to_be_bytes(value);
        self.emit_u8(bytes[0]);
        self.emit_u8(bytes[1]) - 1
    }

    fn write_u16(&mut self, offset: usize, value: u16) {
        let bytes = u16::to_be_bytes(value);
        self.bytecode[offset] = bytes[0];
        self.bytecode[offset + 1] = bytes[1];
    }

    pub fn get_result(&mut self) -> CompileResult {
        if let Some(op) = self.bytecode.last()
            && *op != OpCode::Halt as u8
        {
            self.emit(OpCode::Halt);
        }

        CompileResult {
            bytecode: self.bytecode.clone(),
            constants: self.constants.clone(),
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<CompileResult, CompilerError> {
        self.bytecode.clear();

        let mut rest_stmts = Vec::new();

        for item in &program.body {
            match item {
                ProgramBodyItem::Statement(stmt) => {
                    self.handle_directives = false;
                    match stmt {
                        Statement::Declaration(decl) => match decl {
                            Declaration::FunctionDeclaration(decl) => {
                                self.compile_function_declaration(decl)?;
                            }
                            Declaration::VariableDeclaration(decl) => {
                                self.compile_variable_declaration(decl)?;
                                // TODO: omit declaration, emit assignment only
                                rest_stmts.push(stmt);
                            }
                        },
                        _ => rest_stmts.push(stmt),
                    }
                }
                ProgramBodyItem::Directive(_) => {}
            }
        }

        for stmt in rest_stmts {
            self.compile_statement(stmt)?;
        }

        Ok(self.get_result())
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
        for stmt in &stmt.body {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }

    fn compile_if_statement(&mut self, stmt: &IfStatement) -> Result<(), CompilerError> {
        self.compile_expression(&stmt.test)?;

        self.emit(OpCode::JumpIfFalse);
        let else_jump_offset = self.emit_u16(0);

        self.compile_statement(&stmt.consequent)?;

        self.emit(OpCode::Jump);
        let end_jump_offset = self.emit_u16(0);

        let else_addr = self.bytecode.len();
        self.write_u16(else_jump_offset, else_addr as u16);

        if let Some(alternate) = &stmt.alternate {
            self.compile_statement(alternate)?;
        }

        let end_addr = self.bytecode.len();
        self.write_u16(end_jump_offset, end_addr as u16);

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

            let name_index = self.add_constant(Constant::String(var_name.clone()));
            self.emit(OpCode::DeclareVar);
            self.emit_u16(name_index as u16);

            if let Some(init) = init {
                if let Expression::FunctionExpression(func) = init {
                    self.compile_function_expression(func, Some(var_name))?;
                } else {
                    self.compile_expression(init)?;
                }

                self.emit(OpCode::SetVar);
                self.emit_u16(name_index as u16);
            }
        }

        Ok(())
    }

    pub fn compile_expression(&mut self, expr: &Expression) -> Result<(), CompilerError> {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal)?,
            Expression::BinaryExpression(bin_expr) => self.compile_binary_expression(bin_expr)?,
            Expression::Identifier(ident) => self.compile_identifier(ident)?,
            Expression::CallExpression(call) => self.compile_call_expression(call)?,
            Expression::FunctionExpression(func) => self.compile_function_expression(func, None)?,
            Expression::ObjectExpression(obj) => self.compile_object_expression(obj)?,
            Expression::AssignmentExpression(assignment) => {
                self.compile_assignment_expression(assignment)?
            }
            Expression::MemberExpression(member) => self.compile_member_expression(member)?,
            Expression::UnaryExpression(unary) => self.compile_unary_expression(unary)?,
            Expression::LogicalExpression(logical) => self.compile_logical_expression(logical)?,
            Expression::NewExpression(new_expr) => self.compile_new_expression(new_expr)?,
            Expression::ThisExpression(_) => {
                self.emit(OpCode::PushThis);
            }
        }
        Ok(())
    }

    fn compile_unary_expression(&mut self, unary: &UnaryExpression) -> Result<(), CompilerError> {
        self.compile_expression(&unary.argument)?;
        match unary.operator {
            UnaryOperator::Not => {
                self.emit(OpCode::LogicalNot);
            }
        }
        Ok(())
    }

    fn compile_logical_expression(
        &mut self,
        logical: &LogicalExpression,
    ) -> Result<(), CompilerError> {
        self.compile_expression(&logical.left)?;

        match logical.operator {
            LogicalOperator::And => {
                self.emit(OpCode::Dup);
                self.emit(OpCode::JumpIfFalse);
                let jump_to_end_offset = self.emit_u16(0);
                self.emit(OpCode::Pop);
                self.compile_expression(&logical.right)?;
                let end_addr = self.bytecode.len();
                self.write_u16(jump_to_end_offset, end_addr as u16);
            }
            LogicalOperator::Or => {
                self.emit(OpCode::Dup);
                self.emit(OpCode::JumpIfTrue);
                let jump_to_end_offset = self.emit_u16(0);
                self.emit(OpCode::Pop);
                self.compile_expression(&logical.right)?;
                let end_addr = self.bytecode.len();
                self.write_u16(jump_to_end_offset, end_addr as u16);
            }
        }

        Ok(())
    }

    fn compile_identifier(&mut self, id: &Identifier) -> Result<(), CompilerError> {
        let name_index = self.add_constant(Constant::String(id.name.clone()));
        self.emit(OpCode::GetVar);
        self.emit_u16(name_index as u16);
        Ok(())
    }

    fn compile_object_expression(&mut self, obj: &ObjectExpression) -> Result<(), CompilerError> {
        self.emit(OpCode::NewObject);

        for property in &obj.properties {
            let key = match &property.key {
                PropertyKey::Literal(lit) => Constant::String(lit.value.to_string()),
                PropertyKey::Identifier(id) => Constant::String(id.name.clone()),
            };
            let key_index = self.add_constant(key);

            self.compile_expression(&property.value)?;

            self.emit(OpCode::InitProperty);
            self.emit_u16(key_index as u16);
        }

        Ok(())
    }

    fn compile_member_expression(
        &mut self,
        member: &MemberExpression,
    ) -> Result<(), CompilerError> {
        self.compile_expression(&member.object)?;

        if member.computed {
            // obj[expr]
            self.compile_expression(&member.property)?;
            self.emit(OpCode::GetElement);
        } else {
            // obj.property
            let property_name = match &*member.property {
                Expression::Identifier(id) => &id.name,
                _ => unimplemented!(),
            };
            let key_index = self.add_constant(Constant::String(property_name.clone()));
            self.emit(OpCode::GetProperty);
            self.emit_u16(key_index as u16);
        }

        Ok(())
    }

    fn compile_assignment_expression(
        &mut self,
        assignment: &AssignmentExpression,
    ) -> Result<(), CompilerError> {
        match &assignment.operator {
            AssignmentOperator::Assign => {
                if let AssignmentTarget::Expression(Expression::MemberExpression(member)) =
                    &*assignment.left
                {
                    self.compile_expression(&member.object)?;

                    if member.computed {
                        self.compile_expression(&member.property)?;
                        self.compile_expression(&assignment.right)?;
                        self.emit(OpCode::SetElement);
                    } else {
                        self.compile_expression(&assignment.right)?;
                        let property_name = match &*member.property {
                            Expression::Identifier(id) => &id.name,
                            _ => unimplemented!(),
                        };
                        let key_index = self.add_constant(Constant::String(property_name.clone()));
                        self.emit(OpCode::SetProperty);
                        self.emit_u16(key_index as u16);
                    }
                    return Ok(());
                }

                // Simple assignment: var = value
                self.compile_expression(&assignment.right)?;
            }
            op => {
                // Compound assignment: get current value, then right side, then apply op
                self.compile_assignment_target(&assignment.left)?;
                self.compile_expression(&assignment.right)?;

                let opcode = match op {
                    AssignmentOperator::AddAssign => OpCode::Add,
                    AssignmentOperator::SubtractAssign => OpCode::Sub,
                    AssignmentOperator::MultiplyAssign => OpCode::Mul,
                    AssignmentOperator::DivideAssign => OpCode::Div,
                    _ => unreachable!(),
                };
                self.emit(opcode);
            }
        }

        match &*assignment.left {
            AssignmentTarget::Pattern(pattern) => match pattern {
                Pattern::Identifier(id) => {
                    let name_index = self.add_constant(Constant::String(id.name.clone()));
                    self.emit(OpCode::SetVar);
                    self.emit_u16(name_index as u16);
                }
            },
            _ => unreachable!(),
        }

        Ok(())
    }

    fn compile_assignment_target(
        &mut self,
        target: &AssignmentTarget,
    ) -> Result<(), CompilerError> {
        match target {
            AssignmentTarget::Pattern(pattern) => match pattern {
                Pattern::Identifier(id) => self.compile_identifier(id)?,
            },
            AssignmentTarget::Expression(expr) => {
                self.compile_expression(expr)?;
            }
        }
        Ok(())
    }

    fn compile_literal(&mut self, literal: &Literal) -> Result<(), CompilerError> {
        match &literal.value {
            LiteralValue::Null => {
                self.emit(OpCode::PushNull);
            }
            LiteralValue::Boolean(b) => {
                if *b {
                    self.emit(OpCode::PushTrue);
                } else {
                    self.emit(OpCode::PushFalse);
                }
            }
            LiteralValue::Number(n) => {
                let index = self.add_constant(Constant::Number(n.to_bits()));
                self.emit(OpCode::PushConstant);
                self.emit_u16(index as u16);
            }
            LiteralValue::String(s) => {
                let index = self.add_constant(Constant::String(s.clone()));
                self.emit(OpCode::PushConstant);
                self.emit_u16(index as u16);
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

        let opcode = match bin_expr.operator {
            BinaryOperator::Add => OpCode::Add,
            BinaryOperator::Subtract => OpCode::Sub,
            BinaryOperator::Multiply => OpCode::Mul,
            BinaryOperator::Divide => OpCode::Div,
            BinaryOperator::Equal => OpCode::Eq,
            BinaryOperator::NotEqual => OpCode::NotEq,
            BinaryOperator::LessThan => OpCode::Less,
            BinaryOperator::LessThanEq => OpCode::LessEq,
            BinaryOperator::GreaterThan => OpCode::Greater,
            BinaryOperator::GreaterThanEq => OpCode::GreaterEq,
            BinaryOperator::Instanceof => OpCode::InstanceOf,
        };
        self.emit(opcode);
        Ok(())
    }

    fn compile_function_declaration(
        &mut self,
        func: &FunctionDeclaration,
    ) -> Result<(), CompilerError> {
        let name_index = self.add_constant(Constant::String(func.id.name.clone()));
        self.emit(OpCode::DeclareVar);
        self.emit_u16(name_index as u16);
        self.compile_function_expression(&((*func).clone().into()), Some(&func.id.name))?;
        self.emit(OpCode::SetVar);
        self.emit_u16(name_index as u16);
        Ok(())
    }

    fn compile_function_expression(
        &mut self,
        func: &FunctionExpression,
        func_name: Option<&String>,
    ) -> Result<(), CompilerError> {
        let mut compiler = Compiler::new();

        let mut params = Vec::new();
        for param in &func.params {
            let param_name = match param {
                Pattern::Identifier(id) => id.name.clone(),
            };
            params.push(param_name.clone());
            let name_index = self.add_constant(Constant::String(param_name.clone()));
            self.emit(OpCode::DeclareVar);
            self.emit_u16(name_index as u16);
        }

        let mut rest_stmts = Vec::new();

        for item in &func.body {
            match item {
                FunctionBodyItem::Statement(stmt) => {
                    self.handle_directives = false;
                    match stmt {
                        Statement::Declaration(decl) => match decl {
                            Declaration::FunctionDeclaration(decl) => {
                                self.compile_function_declaration(decl)?;
                            }
                            Declaration::VariableDeclaration(decl) => {
                                self.compile_variable_declaration(decl)?;
                                // TODO: omit declaration, emit assignment only
                                rest_stmts.push(stmt);
                            }
                        },
                        _ => rest_stmts.push(stmt),
                    }
                }
                FunctionBodyItem::Directive(_) => {}
            }
        }

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

        let func = FunctionTemplate {
            name: func_name,
            params,
            arity: func.params.len(),
            code_block: Rc::new(CodeBlock {
                code: bytecode,
                constants: ConstantTable::new(compiler.constants),
            }),
        };

        let index = self.add_constant(Constant::Function(func));
        self.emit(OpCode::NewFunc);
        self.emit_u16(index as u16);
        Ok(())
    }

    fn compile_call_expression(&mut self, call: &CallExpression) -> Result<(), CompilerError> {
        self.compile_expression(&call.callee)?;

        if let Expression::MemberExpression(member) = call.callee.as_ref() {
            self.compile_expression(&member.object)?;
        } else {
            self.emit(OpCode::PushThis);
        }

        for arg in &call.arguments {
            self.compile_expression(arg)?;
        }

        self.emit(OpCode::Call);
        self.emit_u8(call.arguments.len() as u8);

        Ok(())
    }

    fn compile_new_expression(&mut self, new_expr: &NewExpression) -> Result<(), CompilerError> {
        self.compile_expression(&new_expr.callee)?;

        for arg in &new_expr.arguments {
            self.compile_expression(arg)?;
        }

        self.emit(OpCode::Construct);
        self.emit_u8(new_expr.arguments.len() as u8);

        Ok(())
    }

    fn compile_return_statement(&mut self, stmt: &ReturnStatement) -> Result<(), CompilerError> {
        match &stmt.argument {
            Some(expr) => self.compile_expression(expr)?,
            None => {
                self.emit(OpCode::PushUndefined);
            }
        }
        self.emit(OpCode::Return);
        Ok(())
    }
}

pub struct CompileResult {
    pub bytecode: Vec<u8>,
    pub constants: ConstantPool,
}
