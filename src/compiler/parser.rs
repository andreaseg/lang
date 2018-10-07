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

    #[derive(PartialEq, Debug, Clone)]
    pub enum ParseError {
        UnexpectedToken(Token, TokenPosition),
        UnexpectedEOF,
    }

    pub type S = Vec<Top>;

    #[derive(PartialEq, Debug, Clone)]
    pub enum Top {
        Assign(Vec<Box<Assignment>>), // type a = b
        Module(ModuleSymbol, Box<S>), // name::{...}
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum Assignment {
        Function(
            terminal::Constness, // const | mut | None
            Symbol,              // name
            Vec<Box<Symbol>>,    // (x, y, ...)
            Box<Statement>,      // = {...} | = ... ;
        ),
        Field(
            terminal::Constness, // const | mut | None
            Symbol,              // name
            Box<Statement>,      // = {...} | = ... ;
        ),
        Reassign(
            Symbol,         // name
            Box<Statement>, // = {...} | = ... ;
        ),
        Struct(Symbol, Box<Top>),         // struct name = {top}
        Enum(Symbol, Vec<Box<(Symbol)>>), // enum name = {(name,)*}
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
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
            Vec<Box<(Symbol, Statement)>>,
            Option<Box<Statement>>, /* _ => stmt */
            Box<Statement>,         /* next */
        ),
        Return(Box<Expression> /* return expression */),
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum Expression {
        Binary(terminal::Operator, Box<Expression>, Box<Expression>),
        Unary(terminal::UnaryOperator, Box<Expression>),
        Primary(Box<Binding>),
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum Binding {
        Literal(Box<terminal::Literal>),
        Field(ModuleSymbol, Symbol),
        Function(Vec<FunctionCall>), // fun(..).fun(..).fun(..)
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum FunctionCall {
        Function(ModuleSymbol, Symbol, Vec<Box<Expression>>),
        Error(ParseError),
    }

    pub mod terminal {
        #[derive(PartialEq, Debug, Clone)]
        pub enum Constness {
            Const,
            Mut,
            None,
        }

        #[derive(PartialEq, Debug, Clone)]
        pub enum Literal {
            Float(f64),
            Integer(i64),
            String(String),
            Bool(bool),
        }

        #[derive(PartialEq, Debug, Clone)]
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

        #[derive(PartialEq, Debug, Clone)]
        pub enum UnaryOperator {
            Identity,
            Neg,
            Increment,
            Decrement,
        }

        #[derive(PartialEq, Debug, Clone)]
        pub enum Compare {
            Equal,
            NotEqual,
            Greater,
            Smaller,
            GreaterEqual,
            SmallerEqual,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Int,
    Float,
    Double,
    Uint,
    Bool,
    String,
    Ptr(Box<Type>),
    V1,
    V2,
    V4,
    V8,
    V16,
    V32,
    V64,
    List(Box<Type>),
    Function(Box<Type>, Box<Type>),
    Tuple(Vec<Box<Type>>),
    Declaration(String),
    Undefined,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Symbol {
    name: String,
    ty: Type,
}

pub type ModuleSymbol = Vec<String>;

pub fn parse(mut tokens: Vec<(TokenPosition, Token)>) -> ast::S {
    unimplemented!()
}

#[cfg(test)]
mod tests {

    use super::*;
    use compiler::scanner;
    use std::fs::File;
    use std::io::Write;
    use tempdir::TempDir;

    fn test_parser(name: &str, input: &str, expected_result: ast::S) {
        let dir = TempDir::new("scanner_test").unwrap();
        let file_path = dir.path().join(format!("{}.txt", name));
        println!("Test single token \"{}\"", name);
        {
            let mut file = File::create(file_path.clone()).expect("Unable to create file");
            file.write(input.as_bytes())
                .expect("Unable to write to file");
        }
        let file = File::open(file_path).expect("Unable to open file");

        println!("{}", input);

        let tokens = scanner::tokenize(file);
        {
            let errors = scanner::get_errors(&tokens);
            if !errors.is_empty() {
                let mut error_message = String::new();
                for (pos, message) in errors {
                    error_message += format!("Error at {} \"{}\"\n", pos, message).as_str();
                }
                panic!(error_message);
            }
        }

        let ast = parse(tokens);

        println!("{:?}", ast);

        assert_eq!(ast, expected_result);
    }

    #[test]
    fn assign() {
        test_parser(
            "assign",
            "int foo = 1;",
            vec![ast::Top::Assign(vec![Box::new(ast::Assignment::Field(
                ast::terminal::Constness::None,
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Int,
                },
                Box::new(ast::Statement::Return(Box::new(ast::Expression::Primary(
                    Box::new(ast::Binding::Literal(Box::new(
                        ast::terminal::Literal::Integer(1),
                    ))),
                )))),
            ))])],
        );
    }

}
