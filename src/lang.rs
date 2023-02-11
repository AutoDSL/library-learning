//! The language of AutoBio ISA.

use babble::{
    ast_node::{Arity, AstNode, Expr, Precedence, Printable, Printer},
    learn::{LibId, ParseLibIdError},
    teachable::{BindingExpr, DeBruijnIndex, Teachable},
};
use egg::Symbol;

use std::{
    convert::Infallible,
    fmt::{self, Display, Formatter, Write},
    str::FromStr,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AutoBioOp {
    Op(String),

    /// Add an element to the front of a list
    Cons,
    /// A boolean literal
    Bool(bool),
    /// A conditional expression
    If,
    /// An integer literal
    Int(i32),
    /// A function application
    Apply,
    /// A de Bruijn-indexed variable
    Var(DeBruijnIndex),
    /// An identifier
    Ident(Symbol),
    /// An anonymous function
    Lambda,
    /// A library function binding
    Lib(LibId),
    /// A reference to a lib var
    LibVar(LibId),
    /// A list
    List,
    /// A shift
    Shift,
}

impl Arity for AutoBioOp {
    fn min_arity(&self) -> usize {
        match self {
            Self::Bool(_)
            | Self::Int(_)
            | Self::Var(_)
            | Self::Ident(_)
            | Self::LibVar(_)
            | Self::List => 0,
            Self::Lambda | Self::Shift => 1,
            Self::Cons | Self::Apply | Self::Lib(_) => 2,
            Self::If => 3,
            Self::Op(_) => 0
        }
    }

    fn max_arity(&self) -> Option<usize> {
        match self {
            Self::List => None,
            Self::Op(_) => None,
            other => Some(other.min_arity()),
        }
    }
}

impl Display for AutoBioOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Cons => "cons",
            Self::If => "if",
            Self::Apply => "@",
            Self::Lambda => "λ",
            Self::Shift => "shift",
            Self::List => "list",
            Self::Op(name) => {
                name
            }
            Self::Lib(ix) => {
                return write!(f, "lib {}", ix);
            }
            Self::LibVar(ix) => {
                return write!(f, "{}", ix);
            }
            Self::Bool(b) => {
                return write!(f, "{}", b);
            }
            Self::Int(i) => {
                return write!(f, "{}", i);
            }
            Self::Var(index) => {
                return write!(f, "{}", index);
            }
            Self::Ident(ident) => {
                return write!(f, "{}", ident);
            }

        };
        f.write_str(s)
    }
}

impl FromStr for AutoBioOp {
    type Err = Infallible;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let op = match input {
            "cons" => Self::Cons,
            "if" => Self::If,
            "shift" => Self::Shift,
            "apply" | "@" => Self::Apply,
            "lambda" | "λ" => Self::Lambda,
            "list" => Self::List,
            op if op.len() > 2 => Self::Op(op.to_string()),
            input => input
                .parse()
                .map(Self::Bool)
                .or_else(|_| input.parse().map(Self::Var))
                .or_else(|_| input.parse().map(Self::Int))
                .or_else(|_| input.parse().map(Self::LibVar))
                .or_else(|_| {
                    input
                        .strip_prefix("lib ")
                        .ok_or(ParseLibIdError::NoLeadingL)
                        .and_then(|x| x.parse().map(Self::Lib))
                })
                .unwrap_or_else(|_| Self::Ident(input.into())),
        };
        Ok(op)
    }
}

impl Teachable for AutoBioOp {
    fn from_binding_expr<T>(binding_expr: BindingExpr<T>) -> AstNode<Self, T> {
        match binding_expr {
            BindingExpr::Lambda(body) => AstNode::new(Self::Lambda, [body]),
            BindingExpr::Apply(fun, arg) => AstNode::new(Self::Apply, [fun, arg]),
            BindingExpr::Var(index) => AstNode::leaf(Self::Var(index)),
            BindingExpr::Lib(ix, bound_value, body) => {
                AstNode::new(Self::Lib(ix), [bound_value, body])
            }
            BindingExpr::LibVar(ix) => AstNode::leaf(Self::LibVar(ix)),
            BindingExpr::Shift(body) => AstNode::new(Self::Shift, [body]),
        }
    }

    fn as_binding_expr<T>(node: &AstNode<Self, T>) -> Option<BindingExpr<&T>> {
        let binding_expr = match node.as_parts() {
            (Self::Lambda, [body]) => BindingExpr::Lambda(body),
            (Self::Apply, [fun, arg]) => BindingExpr::Apply(fun, arg),
            (&Self::Var(index), []) => BindingExpr::Var(index),
            (Self::Lib(ix), [bound_value, body]) => BindingExpr::Lib(*ix, bound_value, body),
            (Self::LibVar(ix), []) => BindingExpr::LibVar(*ix),
            (Self::Shift, [body]) => BindingExpr::Shift(body),
            _ => return None,
        };
        Some(binding_expr)
    }

    fn list() -> Self {
        Self::List
    }
}

impl Printable for AutoBioOp {
    fn precedence(&self) -> Precedence {
        match self {
            Self::Bool(_) | Self::Int(_) | Self::Var(_) | Self::Ident(_) | Self::LibVar(_) => 60,
            Self::List => 50,
            Self::Apply | Self::Shift => 40,
            Self::Op(_) => 40,
            Self::Cons => 30,
            Self::If => 20,
            Self::Lambda | AutoBioOp::Lib(_) => 10,
        }
    }

    fn print_naked<W: Write>(expr: &Expr<Self>, printer: &mut Printer<W>) -> fmt::Result {
        match (expr.0.operation(), expr.0.args()) {
            (&Self::Int(i), []) => {
                write!(printer.writer, "{}", i)
            }
            (&Self::Bool(b), []) => {
                write!(printer.writer, "{}", b)
            }
            (&Self::Ident(ident), []) => {
                let name: &str = ident.into();
                if name == "empty" {
                    printer.writer.write_str("[]")
                } else {
                    printer.writer.write_str(ident.into())
                }
            }
            (&Self::Cons, [head, tail]) => {
                printer.print(head)?;
                printer.writer.write_str(" : ")?;
                printer.print_in_context(tail, printer.ctx_precedence - 1) // cons is right-associative
            }
            (&Self::If, [cond, then, els]) => {
                printer.writer.write_str("if ")?;
                printer.print_in_context(cond, 0)?; // children do not need parens
                printer.writer.write_str(" then ")?;
                printer.print_in_context(then, 0)?;
                printer.writer.write_str(" else ")?;
                printer.print_in_context(els, 0)
            }
            (&Self::List, ts) => {
                let elem = |p: &mut Printer<W>, i: usize| {
                    p.print_in_context(&ts[i], 0) // children do not need parens
                };
                printer.in_brackets(|p| p.indented(|p| p.vsep(elem, ts.len(), ",")))
            }
            (Self::Op(name), ts) => {

                printer.writer.write_str(name)?;
                printer.writer.write_str(" ")?;
                for (i, e) in ts.iter().enumerate() {
                    if i > 0 {
                        printer.writer.write_str(" ")?;
                    }
                    printer.print_in_context(e, 0)?;
                }
                Ok(())
            }
            (op, _) => write!(printer.writer, "{} ???", op),
        }
    }
}

