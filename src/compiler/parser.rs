#![allow(dead_code)]
use compiler::scanner::{Token, TokenPosition};

/* Grammar
 S := Assign (Assign | Struct)*
 Struct := name (::) LeftCurl S RightCurl
 Assign := [const | mut]? Sign name (=) Stmt
 Sign := LeftPar Type RightArrow Type LeftPar
 Type := LeftPar Type (Comma Type)* RightPar | name
 Stmt := LeftCurl (Assign)* Expr RightCurl | Expr | (if) Predicate Stmt ((else) (if) Stmt)* ((else) Stmt)? | (match) LeftCurl (Type (=>) Stmt)* (_) (=) Stmt RightCurl
 Expr := Predicate | AddExpr
 Predicate := AndExpr | AndExpr (||) AndExpr
 AndExpr := CmpExpr | CmpExpr (&&) CmpExpr
 CmpExpr := AddExpr Cmp AddExpr | true | false
 Cmp := (==) | (!=) | (>) | (<) | (>=) | (<=)
 AddExpr := MulExpr ([(+) | (-)] MulExpr)*
 MulExpr := Unary ([(*) | (/)] Unary)*
 Unary := Primary | [(~) | (-)] Primary
 Primary := (module)* [function | name method] Expr RightPar| (module)* name | LeftPar Expr RightPar | Value
 Value := integer | float | string
*/

mod ast {

    pub enum Top {
        Assign(Vec<Box<Assignment>>),
        Module(terminal::Name, Vec<Box<Top>>),
    }

    pub enum Assignment {
        Function(
            terminal::Constness,
            Box<Signature>,
            terminal::Name,
            Box<Statement>,
        ),
        Field(
            terminal::Constness,
            Box<Signature>,
            terminal::Name,
            Box<Statement>,
        ),
        Struct(terminal::Name, Box<Top>),
        Enum(terminal::Name, Vec<Box<(terminal::Name)>>),
    }

    pub enum Signature {
        Function(Box<Signature>, Box<Signature>),
        Tuple(Vec<Box<Signature>>),
        Type(String),
    }

    pub enum Statement {
        FunctionBody(Vec<Box<Top>>, Box<Expression>),
        Expression(Box<Predicate>, Box<Expression>),
        If(
            Box<Predicate>,
            Box<Statement>,
            Vec<(Box<Predicate>, Box<Statement>)>, // Else if
            Option<Box<Statement>>, // Else
        ),
        Match(Vec<Box<(terminal::Name, Statement)>>, Box<Statement>),
    }

    pub enum Predicate {
        Compare(terminal::Compare, Box<Expression>, Box<Expression>),
        Logic(terminal::LogicOperator, Box<Predicate>, Box<Predicate>),
        Unary(terminal::UnaryLogicOperator, Box<Predicate>),
        Primary(Box<Binding>),
    }

    pub enum Expression {
        Binary(terminal::Operator, Box<Expression>, Box<Expression>),
        Unary(terminal::UnaryOperator, Box<Expression>),
        Primary(Box<Binding>),
    }

    pub enum Binding {
        Literal(Box<terminal::Literal>),
        Field(Vec<terminal::Module>, terminal::Name),
        Function(Vec<terminal::Module>, terminal::Name, Vec<Box<Expression>>),
    }

    pub mod terminal {
        pub enum Constness {
            Const,
            Mut,
            None,
        }

        pub enum Literal {
            Float(f64),
            Integer(i64),
            String(String),
            Bool(bool),
        }

        pub enum Operator {
            Add,
            Sub,
            Mul,
            Div,
        }

        pub enum UnaryOperator {
            Identity,
            Neg,
        }

        pub enum LogicOperator {
            And,
            Or,
            Neg,
        }

        pub enum UnaryLogicOperator {
            Idendity,
            Neg,
        }

        pub enum Compare {
            Equal,
            NotEqual,
            Greater,
            Smaller,
            GreaterEqual,
            SmallerEqual,
        }

        pub type Module = String;

        pub type Name = String;
    }
}

pub fn parse(mut tokens: Vec<(TokenPosition, Token)>) -> ast::Top {
    unimplemented!()
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::io::Write;
    use tempdir::TempDir;

}
