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

    use super::*;

    pub enum ParseError {
        UnexpectedToken(Token, TokenPosition),
        UnexpectedEOF,
    }

    pub enum Top {
        Assign(Vec<Box<Assignment>>),     // type a = b
        Module(terminal::Name, Box<Top>), // name::{...}
        Error(ParseError),
    }

    pub enum Assignment {
        Function(
            terminal::Constness,      // const | mut | None
            Box<Signature>,           // (type -> type)
            terminal::Name,           // name
            Vec<Box<terminal::Name>>, // (x, y, ...)
            Box<Statement>,           // = {...} | = ... ;
        ),
        Field(
            terminal::Constness, // const | mut | None
            Box<Signature>,      // type
            terminal::Name,      // name
            Box<Statement>,      // = {...} | = ... ;
        ),
        Reassign(
            terminal::Name, // name
            Box<Statement>, // = {...} | = ... ;
        ),
        Struct(terminal::Name, Box<Top>), // struct name = {top}
        Enum(terminal::Name, Vec<Box<(terminal::Name)>>), // enum name = {(name,)*}
        Error(ParseError),
    }

    pub enum Signature {
        Function(Box<Signature>, Box<Signature>), // sign -> sign
        Tuple(Vec<Box<Signature>>),
        Type(String), // Typename
        Error(ParseError),
    }

    pub enum Statement {
        Assign(Box<Assignment>, Box<Statement> /* next */),
        Call(Box<FunctionCall>, Box<Statement> /* next */),
        If(
            Box<Expression>,                        // If expression
            Box<Statement>,                         // Then statement
            Vec<(Box<Expression>, Box<Statement>)>, // (else if expression then statement )*
            Option<Box<Statement>>,                 // Else
            Box<Statement>,                         /* next */
        ),
        Match(
            Vec<Box<(terminal::Name, Statement)>>,
            Option<Box<Statement>>, /* _ => stmt */
            Box<Statement>,         /* next */
        ),
        Return(Box<Expression> /* return expression */),
        Error(ParseError),
    }

    pub enum Expression {
        Binary(terminal::Operator, Box<Expression>, Box<Expression>),
        Unary(terminal::UnaryOperator, Box<Expression>),
        Primary(Box<Binding>),
        Error(ParseError),
    }

    pub enum Binding {
        Literal(Box<terminal::Literal>),
        Field(Vec<terminal::Module>, terminal::Name),
        Function(Vec<FunctionCall>), // fun(..).fun(..).fun(..)
        Error(ParseError),
    }

    pub enum FunctionCall {
        Function(Vec<terminal::Module>, terminal::Name, Vec<Box<Expression>>),
        Error(ParseError),
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
            BitAnd,
            BitOr,
            BitXor,
            BitShiftLeft,
            BitShiftRight,
            And,
            Or,
            Neg,
            Mod,
        }

        pub enum UnaryOperator {
            Identity,
            Neg,
            Increment,
            Decrement,
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
