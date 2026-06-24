use std::{
    cell::Cell,
    collections::{HashSet, VecDeque},
    fmt::{Debug, Display},
    ops::Index,
};

use crate::{
    atom::{Atom, Atoms},
    expr::ExprKind::IfThenElse,
    lexer::SourceSpan,
};

pub struct Expressions {
    exprs: Vec<Expr>,
    atoms: Atoms,
    id_buffer: Cell<VecDeque<ExprId>>,
}

impl Expressions {
    pub fn new() -> Self {
        Self {
            exprs: Vec::new(),
            atoms: Atoms::new(),
            id_buffer: Cell::new(VecDeque::new()),
        }
    }

    pub fn push_expr(&mut self, expr: Expr) -> ExprId {
        let id = self.exprs.len();
        self.exprs.push(expr);
        ExprId(id)
    }

    pub fn get_expr(&self, id: ExprId) -> Option<&Expr> {
        self.exprs.get(id.0)
    }

    pub fn view(&self, id: ExprId) -> ExprView<'_> {
        ExprView { id, exprs: &self }
    }

    pub fn get_atom(&self, atom: Atom) -> &str {
        self.atoms.resolve(&atom)
    }

    pub fn get_or_intern(&mut self, s: &str) -> Atom {
        self.atoms.get_or_intern(s)
    }

    pub fn find_captures(&self, id: ExprId, f: &mut impl FnMut(Atom)) {
        let mut rem = self.id_buffer.take();
        rem.clear();
        rem.push_back(id);
        let mut locals = HashSet::new();
        while let Some(id) = rem.pop_front() {
            let e = &self.exprs[id.0];
            match &e.kind {
                ExprKind::Identifier(atom) => {
                    if !locals.contains(atom) {
                        f(*atom)
                    }
                }
                ExprKind::Let { id, value, .. } => {
                    locals.insert(id);
                    rem.push_back(*value);
                }
                ExprKind::Number(_) | ExprKind::Bool(_) => {}
                ExprKind::Array(children) => rem.extend(children.iter().map(|e| e.expr_id)),
                ExprKind::FunctionCall { args, .. } => rem.extend(args.iter()),
                ExprKind::FunctionDef { captures, .. } => {
                    for c in captures.iter() {
                        if !locals.contains(c) {
                            f(*c)
                        }
                    }
                }
                ExprKind::Unary { operand, .. } => rem.push_back(*operand),
                ExprKind::Binary { lhs, rhs, .. } => {
                    rem.push_back(*lhs);
                    rem.push_back(*rhs);
                }
                ExprKind::Block { children } => rem.extend(children.iter()),
                IfThenElse { cond, lhs, rhs } => {
                    rem.push_back(*cond);
                    rem.push_back(*lhs);
                    rem.push_back(*rhs);
                }
            }
        }
        self.id_buffer.set(rem);
    }
}

impl Index<ExprId> for Expressions {
    type Output = Expr;

    fn index(&self, index: ExprId) -> &Self::Output {
        &self.exprs[index.0]
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprId(usize);

impl Debug for ExprId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: SourceSpan,
}

impl Expr {
    pub fn new(kind: ExprKind, span: SourceSpan) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Identifier(Atom),
    Number(f64),
    Bool(bool),
    Array(Vec<ArrayElem>),
    FunctionCall {
        func: ExprId,
        args: Vec<ExprId>,
    },
    Let {
        id: Atom,
        value: ExprId,
        type_annotation: Option<TypeAnnotation>,
    },
    IfThenElse {
        cond: ExprId,
        lhs: ExprId,
        rhs: ExprId,
    },
    FunctionDef {
        args: Vec<FnArg>,
        captures: HashSet<Atom>,
        body: ExprId,
        ret_type: Option<TypeAnnotation>,
    },
    Unary {
        op: UnaryOp,
        operand: ExprId,
    },
    Binary {
        lhs: ExprId,
        op: BinaryOp,
        rhs: ExprId,
    },
    Block {
        children: Vec<ExprId>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAnnotation {
    Never,
    Unit,
    Bool,
    Float,
    Array(Box<TypeAnnotation>),
    Fn(Vec<TypeAnnotation>, Box<TypeAnnotation>),
    Enum(Vec<TypeAnnotation>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnArg {
    pub id: Atom,
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayElem {
    pub expr_id: ExprId,
    pub flatten: bool,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

impl BinaryOp {
    pub const fn as_str(&self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Eq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::Less => "<",
            BinaryOp::LessEq => "<=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEq => ">=",
            BinaryOp::Assign => "=",
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    pub const fn as_str(&self) -> &'static str {
        match self {
            UnaryOp::Neg => "Neg",
            UnaryOp::Not => "Not",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PostfixOp {
    FunctionCall,
    Index,
}

#[derive(Clone, Copy)]
pub struct ExprView<'a> {
    id: ExprId,
    exprs: &'a Expressions,
}

impl<'a> ExprView<'a> {
    pub fn with_id(&self, id: ExprId) -> Self {
        Self {
            id,
            exprs: self.exprs,
        }
    }

    pub fn expr(&self) -> &ExprKind {
        &self.exprs.get_expr(self.id).unwrap().kind
    }
}

impl<'a> Debug for ExprView<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.expr() {
            ExprKind::Identifier(atom) => f
                .debug_tuple("Identifier")
                .field(&self.exprs.atoms.resolve(atom))
                .finish(),
            ExprKind::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            ExprKind::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            ExprKind::Unary { op, operand } => f
                .debug_tuple(op.as_str())
                .field(&self.with_id(*operand))
                .finish(),
            ExprKind::Binary { op, lhs, rhs } => f
                .debug_tuple(op.as_str())
                .field(&self.with_id(*lhs))
                .field(&self.with_id(*rhs))
                .finish(),
            ExprKind::Block { .. } => f.debug_tuple("Block").finish(),
            ExprKind::FunctionCall { func, args } => f
                .debug_struct("FunctionCall")
                .field("func", &self.with_id(*func))
                .finish(),
            ExprKind::IfThenElse { .. } => todo!(),
            ExprKind::FunctionDef { .. } => todo!(),
            ExprKind::Let { .. } => todo!(),
            ExprKind::Array(..) => todo!(),
        }
    }
}
