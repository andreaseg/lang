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
        UnexpectedToken((TokenPosition, Token), /* message */String),
        UnexpectedEOF,
    }

    pub type S = Vec<Top>;

    #[derive(PartialEq, Debug, Clone)]
    pub enum Top {
        Assign(Box<Assignment>), // type a = b
        Module(Module, Box<S>), // name::{...}
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
        Field(Symbol, Vec<Box<Binding>> /* chained function */),
        Function(FunctionCall, Vec<Box<Binding>> /* chained function */),
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum FunctionCall {
        Function(Symbol, Vec<Box<Expression>>),
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

pub type Module = String;


pub fn parse(mut tokens: Vec<(TokenPosition, Token)>) -> Result<ast::S, Vec<ast::ParseError>> {
    tokens.reverse();
    let mut ast = parse_s(&mut tokens);
    resolve_types(&mut ast);
    let errors = get_errors(&ast);
    if !errors.is_empty() {
        return Err(errors);
    }
    Ok(ast)
}

//type Tokens = std::iter::Peekable<std::vec::IntoIter<(TokenPosition, Token)>>;
type Tokens = Vec<(TokenPosition, Token)>;

fn parse_s(tokens: &mut Tokens) -> ast::S {
    unimplemented!()
}

fn parse_top(tokens: &mut Tokens) -> ast::Top {
    unimplemented!()
}

fn parse_assignment(tokens: &mut Tokens) -> ast::Assignment {
    unimplemented!()
}

fn parse_statement(tokens: &mut Tokens) -> ast::Statement {
    unimplemented!()
}

/// Precedence order is C order: https://en.cppreference.com/w/c/language/operator_precedence
fn parse_expression(tokens: &mut Tokens) -> ast::Expression {
    unimplemented!()
}

fn parse_or_expr(tokens: &mut Tokens) -> ast::Expression {
    unimplemented!()
}

fn parse_and_expr(tokens: &mut Tokens) -> ast::Expression {
    unimplemented!()
}

fn parse_bor_expr(tokens: &mut Tokens) -> ast::Expression {
    unimplemented!()
}

fn parse_xor_expr(tokens: &mut Tokens) -> ast::Expression {
    unimplemented!()
}

fn parse_band_expr(tokens: &mut Tokens) -> ast::Expression {
    unimplemented!()
}

fn parse_cmp_expr(tokens: &mut Tokens) -> ast::Expression {
    unimplemented!()
}

fn parse_add_expr(tokens: &mut Tokens) -> ast::Expression {
    unimplemented!()
}

fn parse_mul_expr(tokens: &mut Tokens) -> ast::Expression {
    unimplemented!()
}

fn parse_binding(tokens: &mut Tokens) -> ast::Binding {

    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Binding::Error(ast::ParseError::UnexpectedEOF)
    };

    println!("{:?}", token);

    return match token {
        (_, Token::Function(_)) => {
            tokens.push(token);
            let fun = parse_function_call(tokens);
            let chain = parse_chained_binding(tokens);
            ast::Binding::Function(fun, chain)
        },
        (_, Token::Name(name)) => {
            ast::Binding::Field(Symbol{name: name.clone(), ty: Type::Undefined},
            parse_chained_binding(tokens))
        },
        (_, Token::Float(x)) => {ast::Binding::Literal(Box::new(ast::terminal::Literal::Float(x.clone())))},
        (_, Token::Integer(n)) => {ast::Binding::Literal(Box::new(ast::terminal::Literal::Integer(n.clone())))},
        (_, Token::Truthy(b)) => {ast::Binding::Literal(Box::new(ast::terminal::Literal::Bool(b.clone())))},
        (_, Token::String(s)) => {ast::Binding::Literal(Box::new(ast::terminal::Literal::String(s.clone())))},
        (pos, token) => ast::Binding::Error(ast::ParseError::UnexpectedToken((pos, token), "Expected function, name, or literal binding".to_string()))
    }
}

fn parse_chained_binding(tokens: &mut Tokens) -> Vec<Box<ast::Binding>> {
    // TODO
    Vec::new()
}

fn parse_function_call(tokens: &mut Tokens) -> ast::FunctionCall {
    let symbol = match tokens.pop() {
        Some(val) => match val {
            (_, Token::Function(name)) => Symbol{name: name, ty: Type::Undefined},
            (pos, token) => return ast::FunctionCall::Error(ast::ParseError::UnexpectedToken((pos.clone(), token.clone()), "Expected function".to_string()))
        },
        None => return ast::FunctionCall::Error(ast::ParseError::UnexpectedEOF)
    };

    let mut args: Vec<Box<ast::Expression>> = Vec::new();

    // Guard against empty function call
    match tokens.pop() {
        Some(val) => match val {
            (_, Token::RightPar) => return ast::FunctionCall::Function(symbol, vec![]),
            other => {
                tokens.push(other);
            }
        },
        None => return ast::FunctionCall::Error(ast::ParseError::UnexpectedEOF)
    }

    loop {
        args.push(Box::new(parse_expression(tokens)));

        match tokens.pop() {
            Some(val) => match val {
                (_, Token::Comma) => continue,
                (_, Token::RightPar) => break,
                (pos, token) => return ast::FunctionCall::Error(ast::ParseError::UnexpectedToken((pos.clone(), token.clone()), "Expected comma or right par".to_string()))
            },
            None => return ast::FunctionCall::Error(ast::ParseError::UnexpectedEOF)
        };
    }

    ast::FunctionCall::Function(symbol, args)
}

fn resolve_types(ast: &mut ast::S) {
    unimplemented!()
}

fn get_errors(ast: &ast::S) -> Vec<ast::ParseError> {
    unimplemented!()
}

#[cfg(test)]
mod tests {

    use super::*;
    use compiler::scanner;
    use std::fs::File;
    use std::io::Write;
    use tempdir::TempDir;

    fn generate_test_tokens(name: &str, input: &str) -> Vec<(TokenPosition, Token)> {
        let dir = TempDir::new("parser_test").unwrap();
        let file_path = dir.path().join(format!("{}.txt", name));
        {
            let mut file = File::create(file_path.clone()).expect("Unable to create file");
            file.write(input.as_bytes())
                .expect("Unable to write to file");
        }
        let file = File::open(file_path).expect("Unable to open file");

        println!("{}", input);

        let mut tokens = match scanner::tokenize(file) {
            Ok(ok) => ok,
            Err(errors) => {
                let mut error_message = String::new();
                for (pos, message) in errors {
                    error_message += format!("Error at {} \"{}\"\n", pos, message).as_str();
                }
                panic!(error_message);
            }
        };

        tokens.reverse();

        tokens
    }

    fn test_parser(name: &str, input: &str, expected_result: ast::S) {
        let tokens = generate_test_tokens(name, input);

        let ast = parse(tokens).expect("Unable to parse expression");

        println!("{:?}", ast);

        assert_eq!(ast, expected_result);
    }

    #[test]
    fn assign() {
        test_parser(
            "assign",
            "int foo = 1;",
            vec![ast::Top::Assign(Box::new(ast::Assignment::Field(
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
            )))],
        );
    }



    #[test]
    fn integer_binding() {
        let mut tokens = generate_test_tokens("integer", "1");
        let ast = parse_binding(&mut tokens);
        assert_eq!(ast, ast::Binding::Literal(
            Box::new(
                ast::terminal::Literal::Integer(1)
            )
        ))
    }

    #[test]
    fn float_binding() {
        let mut tokens = generate_test_tokens("float", "1.0");
        let ast = parse_binding(&mut tokens);
        assert_eq!(ast, ast::Binding::Literal(
            Box::new(
                ast::terminal::Literal::Float(1.0)
            )
        ))
    }

    #[test]
    fn string_binding() {
        let mut tokens = generate_test_tokens("string", "\"string\"");
        let ast = parse_binding(&mut tokens);
        assert_eq!(ast, ast::Binding::Literal(
            Box::new(
                ast::terminal::Literal::String("string".to_string())
            )
        ))
    }

    #[test]
    fn bool_binding() {
        {
            let mut tokens = generate_test_tokens("bool_true", "true");
            let ast = parse_binding(&mut tokens);
            assert_eq!(ast, ast::Binding::Literal(
                Box::new(
                    ast::terminal::Literal::Bool(true)
                )
            ))
        }
        {
            let mut tokens = generate_test_tokens("bool_false", "false");
            let ast = parse_binding(&mut tokens);
            assert_eq!(ast, ast::Binding::Literal(
                Box::new(
                    ast::terminal::Literal::Bool(false)
                )
            ))
        }
    }

    #[test]
    fn field_binding() {
        let mut tokens = generate_test_tokens("field_binding", "foo");
        let ast = parse_binding(&mut tokens);
        assert_eq!(ast, 
            ast::Binding::Field(
                Symbol {name: "foo".to_string(), ty: Type::Undefined},
                vec![]
            )
        );
    }

    #[test]
    fn empty_function_binding() {
        let mut tokens = generate_test_tokens("empty_function_binding", "foo()");
        let ast = parse_binding(&mut tokens);
        assert_eq!(ast,
            ast::Binding::Function(
                ast::FunctionCall::Function(
                    Symbol {name: "foo".to_string(), ty: Type::Undefined},
                    vec![]
                ),
                vec![]
            )
        )
    }

}
