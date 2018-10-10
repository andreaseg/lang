#![allow(dead_code)]
#![allow(unused_variables)]
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
        UnexpectedToken((TokenPosition, Token), /* message */ String),
        UnexpectedEOF,
    }

    #[derive(PartialEq, Debug, Clone)]
    pub struct S {
        pub code: Vec<Top>,
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum Top {
        Assign(Assignment),     // type a = b
        Module(Module, Box<S>), // name::{...}
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum Assignment {
        Function(
            terminal::Constness, // const | mut | None
            Symbol,              // name
            Vec<Symbol>,         // (x, y, ...)
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
        Field(Symbol, Option<Box<Binding>> /* chained function */),
        Function(
            FunctionCall,
            Option<Box<Binding>>, /* chained function */
        ),
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
            Mod,
            Equal,
            NotEqual,
            Greater,
            Lesser,
            GreaterEqual,
            LesserEqual,
        }

        #[derive(PartialEq, Debug, Clone)]
        pub enum UnaryOperator {
            Identity,
            Neg,
            Minus,
            Increment,
            Decrement,
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
    Struct,
    Enum,
    EnumMember,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Symbol {
    name: String,
    ty: Type,
}

pub type Module = String;

pub fn parse(mut tokens: Vec<(TokenPosition, Token)>) -> Result<ast::S, Vec<ast::ParseError>> {
    tokens.reverse();
    let ast = parse_s(&mut tokens);
    let errors = get_errors(&ast);
    if !errors.is_empty() {
        return Err(errors);
    }
    Ok(ast)
}

type Tokens = Vec<(TokenPosition, Token)>;

macro_rules! expect_tokens {
    ($tokens: ident, $error_type: path, $error_message: expr, $(($type: pat => $result: expr),)*) => {
        match $tokens.pop() {
            Some(val) => match val {
                $((_, $type) => $result,)*
                other => {
                    use $error_type as path;
                    return path::Error(ast::ParseError::UnexpectedToken(
                        other,
                        $error_message.to_string(),
                    ))
                }
            },
            None => {
                use $error_type as path;
                return path::Error(ast::ParseError::UnexpectedEOF)
            },
        };
    }
}

fn parse_s(tokens: &mut Tokens) -> ast::S {
    let mut code: Vec<ast::Top> = Vec::new();
    while !tokens.is_empty() {
        code.push(parse_top(tokens));
    }
    ast::S { code }
}

fn parse_top(tokens: &mut Tokens) -> ast::Top {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Top::Error(ast::ParseError::UnexpectedEOF),
    };

    match token {
        (_, Token::Module(name)) => {
            expect_tokens!(tokens, self::ast::Top, "Expected {",
                (Token::LeftCurl => {}),);

            let s = parse_s(tokens);

            expect_tokens!(tokens, self::ast::Top, "Expected }",
                (Token::RightCurl => {}),);

            ast::Top::Module(name, Box::new(s))
        }
        other => {
            tokens.push(other);
            ast::Top::Assign(parse_assignment(tokens))
        }
    }
}

fn parse_assignment(tokens: &mut Tokens) -> ast::Assignment {
    let mut token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Assignment::Error(ast::ParseError::UnexpectedEOF),
    };

    let constness = match token {
        (_, Token::Const) => ast::terminal::Constness::Const,
        (_, Token::Mut) => ast::terminal::Constness::Mut,
        _ => {
            tokens.push(token);
            ast::terminal::Constness::None
        }
    };

    token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Assignment::Error(ast::ParseError::UnexpectedEOF),
    };

    let ty = match token {
        (_, Token::LeftPar) => {
            let sign = parse_signature(tokens);
            let token = match tokens.pop() {
                Some(val) => val,
                None => return ast::Assignment::Error(ast::ParseError::UnexpectedEOF),
            };
            match token {
                (_, Token::RightPar) => Some(sign),
                other => {
                    return ast::Assignment::Error(ast::ParseError::UnexpectedToken(
                        other,
                        "Expected end of type signature".to_string(),
                    ))
                }
            }
        }
        _ => {
            tokens.push(token);
            None
        }
    };

    token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Assignment::Error(ast::ParseError::UnexpectedEOF),
    };

    if ty.is_some() {
        match token {
            (_, Token::Function(name)) => {
                parse_function_assignment(tokens, constness, name, ty.unwrap())
            }
            (_, Token::Name(name)) => parse_field_assignment(tokens, constness, name, ty.unwrap()),
            other => ast::Assignment::Error(ast::ParseError::UnexpectedToken(
                other,
                "Expected declaration".to_string(),
            )),
        }
    } else {
        match token {
            (_, Token::Name(name)) | (_, Token::Function(name)) => parse_reassignment(tokens, name),
            (_, Token::Struct) => parse_struct_assignment(tokens),
            (_, Token::Enum) => parse_enum_assignment(tokens),
            other => ast::Assignment::Error(ast::ParseError::UnexpectedToken(
                other,
                "Expected struct, enum, or reassignment".to_string(),
            )),
        }
    }
}

fn parse_function_assignment(
    tokens: &mut Tokens,
    constness: ast::terminal::Constness,
    name: String,
    ty: Type,
) -> ast::Assignment {
    expect_tokens!(tokens, self::ast::Assignment, "Expected (",
                (Token::LeftPar => {}),);

    let mut params = Vec::new();

    loop {
        expect_tokens!(tokens, self::ast::Assignment, "Expected {",
                    (Token::Comma => continue),
                    (Token::RightPar => break),
                    (Token::Name(name) => params.push(Symbol {name, ty: Type::Undefined})),
                    );
    }

    expect_tokens!(tokens, self::ast::Assignment, "Expected assignment (=)",
                (Token::Assign => {}),);

    expect_tokens!(tokens, self::ast::Assignment, "Expected {",
                (Token::LeftCurl => {}),);

    let stmt = parse_statement(tokens);

    expect_tokens!(tokens, self::ast::Assignment, "Expected }",
                (Token::RightCurl => {}),);

    ast::Assignment::Function(constness, Symbol { name, ty }, params, Box::new(stmt))
}

fn parse_field_assignment(
    tokens: &mut Tokens,
    constness: ast::terminal::Constness,
    name: String,
    ty: Type,
) -> ast::Assignment {
    expect_tokens!(tokens, self::ast::Assignment, "Expected assignment (=)",
                (Token::Assign => {}),);

    expect_tokens!(tokens, self::ast::Assignment, "Expected {",
                (Token::LeftCurl => {}),);

    let stmt = parse_statement(tokens);

    expect_tokens!(tokens, self::ast::Assignment, "Expected }",
                (Token::RightCurl => {}),);

    ast::Assignment::Field(constness, Symbol { name, ty }, Box::new(stmt))
}

fn parse_struct_assignment(tokens: &mut Tokens) -> ast::Assignment {
    let name = expect_tokens!(tokens, 
                    self::ast::Assignment, "Expected struct name",
                    (Token::Name(name) => name.to_string()),);

    expect_tokens!(tokens, self::ast::Assignment, "Expected assignment (=)",
                (Token::Assign => {}),);

    expect_tokens!(tokens, self::ast::Assignment, "Expected {",
                (Token::LeftCurl => {}),);

    let struct_body = parse_top(tokens);

    expect_tokens!(tokens, self::ast::Assignment, "Expected }",
                (Token::RightCurl => {}),);

    ast::Assignment::Struct(
        Symbol {
            name,
            ty: Type::Struct,
        },
        Box::new(struct_body),
    )
}

fn parse_enum_assignment(tokens: &mut Tokens) -> ast::Assignment {
    let name = expect_tokens!(tokens, 
                    self::ast::Assignment, "Expected enum name",
                    (Token::Name(name) => name.to_string()),);

    expect_tokens!(tokens, self::ast::Assignment, "Expected assignment (=)",
                (Token::Assign => {}),);

    expect_tokens!(tokens, self::ast::Assignment, "Expected {",
                (Token::LeftCurl => {}),);

    let symbols = match parse_enum_symbols(tokens) {
        Ok(ok) => ok,
        Err(e) => return ast::Assignment::Error(e),
    };

    expect_tokens!(tokens, self::ast::Assignment, "Expected }",
                (Token::RightCurl => {}),);

    ast::Assignment::Enum(
        Symbol {
            name,
            ty: Type::Enum,
        },
        symbols,
    )
}

fn parse_reassignment(tokens: &mut Tokens, name: String) -> ast::Assignment {
    expect_tokens!(tokens, self::ast::Assignment, "Expected assignment (=)",
                (Token::Assign => {}),);

    expect_tokens!(tokens, self::ast::Assignment, "Expected {",
                (Token::LeftCurl => {}),);

    let stmt = parse_statement(tokens);

    expect_tokens!(tokens, self::ast::Assignment, "Expected }",
                (Token::RightCurl => {}),);

    ast::Assignment::Reassign(
        Symbol {
            name,
            ty: Type::Undefined,
        },
        Box::new(stmt),
    )
}

fn parse_enum_symbols(tokens: &mut Tokens) -> Result<Vec<Box<Symbol>>, ast::ParseError> {
    let mut symbols = Vec::new();

    loop {
        let token = match tokens.pop() {
            Some(val) => val,
            None => return Err(ast::ParseError::UnexpectedEOF),
        };

        match token {
            (_, Token::Comma) => continue,
            (_, Token::RightCurl) => {
                tokens.push(token);
                break;
            }
            (_, Token::Name(name)) => symbols.push(Box::new(Symbol {
                name,
                ty: Type::EnumMember,
            })),
            other => {
                return Err(ast::ParseError::UnexpectedToken(
                    other,
                    "Expected enum entry".to_string(),
                ))
            }
        }
    }

    Ok(symbols)
}

fn parse_signature(tokens: &mut Tokens) -> Type {
    // TODO
    unimplemented!()
}

fn parse_statement(tokens: &mut Tokens) -> ast::Statement {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Statement::Error(ast::ParseError::UnexpectedEOF),
    };

    // TODO
    unimplemented!()
}

/// Precedence order is C order: https://en.cppreference.com/w/c/language/operator_precedence

macro_rules! parse_binary_expr {
    ($tokens: ident, $next_parse: ident, $(($left: expr => $op: expr),)*) => ({
        let mut node = $next_parse($tokens);
        while let Some(token) = $tokens.pop() {
        node = match token {
            (pos, Token::Operator(op)) => match op.as_ref() {
                $(
                    $left => ast::Expression::Binary($op, Box::new(node), Box::new($next_parse($tokens))),
                )+
                other => {
                    $tokens.push((pos, Token::Operator(op.clone())));
                    break;
                }
            },
            other => {
                $tokens.push(other);
                break;
            }
        }
    }
    node
    })
}

fn parse_expression(tokens: &mut Tokens) -> ast::Expression {
    parse_or_expr(tokens)
}

fn parse_or_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_and_expr, ("||" => ast::terminal::Operator::Or),)
}

fn parse_and_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_bor_expr, ("&&" => ast::terminal::Operator::And),)
}

fn parse_bor_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_xor_expr, ("|" => ast::terminal::Operator::BitOr),)
}

fn parse_xor_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_band_expr, ("^" => ast::terminal::Operator::BitXor),)
}

fn parse_band_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_cmp_expr, ("&" => ast::terminal::Operator::BitAnd),)
}

fn parse_cmp_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_bitshift_expr, ("==" => ast::terminal::Operator::Equal),
    ("!=" => ast::terminal::Operator::NotEqual),
    ("<" => ast::terminal::Operator::Lesser),
    (">" => ast::terminal::Operator::Greater),
    ("<=" => ast::terminal::Operator::LesserEqual),
    (">=" => ast::terminal::Operator::GreaterEqual),)
}

fn parse_bitshift_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_add_expr, ("<<" => ast::terminal::Operator::BitShiftLeft),
    (">>" => ast::terminal::Operator::BitShiftRight),)
}

fn parse_add_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_mul_expr, ("+" => ast::terminal::Operator::Add),
    ("-" => ast::terminal::Operator::Sub),)
}

fn parse_mul_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_unary_expr, ("*" => ast::terminal::Operator::Mul),
    ("/" => ast::terminal::Operator::Div),)
}

fn parse_unary_expr(tokens: &mut Tokens) -> ast::Expression {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Expression::Error(ast::ParseError::UnexpectedEOF),
    };

    match token {
        (_, Token::Operator(op)) => match op.as_ref() {
            "-" => ast::Expression::Unary(
                ast::terminal::UnaryOperator::Minus,
                Box::new(parse_primary_expr(tokens)),
            ),
            "~" => ast::Expression::Unary(
                ast::terminal::UnaryOperator::Neg,
                Box::new(parse_primary_expr(tokens)),
            ),
            other => panic!("Unexpected token {:?}, should be - or +", other),
        },
        other => {
            tokens.push(other);
            let mut node = parse_primary_expr(tokens);
            if let Some(token) = tokens.pop() {
                match token {
                    (pos, Token::Operator(op)) => match op.as_ref() {
                        "++" => {
                            node = ast::Expression::Unary(
                                ast::terminal::UnaryOperator::Increment,
                                Box::new(node),
                            )
                        }
                        "--" => {
                            node = ast::Expression::Unary(
                                ast::terminal::UnaryOperator::Decrement,
                                Box::new(node),
                            )
                        }
                        _ => tokens.push((pos, Token::Operator(op.clone()))),
                    },
                    _ => tokens.push(token),
                }
            }

            node
        }
    }
}

fn parse_primary_expr(tokens: &mut Tokens) -> ast::Expression {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Expression::Error(ast::ParseError::UnexpectedEOF),
    };

    match token {
        (_, Token::LeftPar) => {
            let node = parse_expression(tokens);
            let token = match tokens.pop() {
                Some(val) => val,
                None => return ast::Expression::Error(ast::ParseError::UnexpectedEOF),
            };
            match token {
                (_, Token::RightPar) => node,
                other => ast::Expression::Error(ast::ParseError::UnexpectedToken(
                    other,
                    "Expected expression".to_string(),
                )),
            }
        }
        other => {
            tokens.push(other);
            ast::Expression::Primary(Box::new(parse_binding(tokens)))
        }
    }
}

fn parse_binding(tokens: &mut Tokens) -> ast::Binding {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Binding::Error(ast::ParseError::UnexpectedEOF),
    };

    println!("{:?}", token);

    match token {
        (_, Token::Function(_)) => {
            tokens.push(token);
            let fun = parse_function_call(tokens);
            let chain = parse_chained_binding(tokens);
            ast::Binding::Function(fun, chain)
        }
        (_, Token::Name(name)) => ast::Binding::Field(
            Symbol {
                name: name.clone(),
                ty: Type::Undefined,
            },
            parse_chained_binding(tokens),
        ),
        (_, Token::Float(x)) => ast::Binding::Literal(Box::new(ast::terminal::Literal::Float(x))),
        (_, Token::Integer(n)) => {
            ast::Binding::Literal(Box::new(ast::terminal::Literal::Integer(n)))
        }
        (_, Token::Truthy(b)) => ast::Binding::Literal(Box::new(ast::terminal::Literal::Bool(b))),
        (_, Token::String(s)) => {
            ast::Binding::Literal(Box::new(ast::terminal::Literal::String(s.clone())))
        }
        (pos, token) => ast::Binding::Error(ast::ParseError::UnexpectedToken(
            (pos, token),
            "Expected function, name, or literal binding".to_string(),
        )),
    }
}

fn parse_chained_binding(tokens: &mut Tokens) -> Option<Box<ast::Binding>> {
    // TODO
    None
}

fn parse_function_call(tokens: &mut Tokens) -> ast::FunctionCall {
    let symbol = match tokens.pop() {
        Some(val) => match val {
            (_, Token::Function(name)) => Symbol {
                name,
                ty: Type::Undefined,
            },
            (pos, token) => {
                return ast::FunctionCall::Error(ast::ParseError::UnexpectedToken(
                    (pos.clone(), token.clone()),
                    "Expected function".to_string(),
                ))
            }
        },
        None => return ast::FunctionCall::Error(ast::ParseError::UnexpectedEOF),
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
        None => return ast::FunctionCall::Error(ast::ParseError::UnexpectedEOF),
    }

    loop {
        args.push(Box::new(parse_expression(tokens)));

        match tokens.pop() {
            Some(val) => match val {
                (_, Token::Comma) => continue,
                (_, Token::RightPar) => break,
                (pos, token) => {
                    return ast::FunctionCall::Error(ast::ParseError::UnexpectedToken(
                        (pos.clone(), token.clone()),
                        "Expected comma or right par".to_string(),
                    ))
                }
            },
            None => return ast::FunctionCall::Error(ast::ParseError::UnexpectedEOF),
        };
    }

    ast::FunctionCall::Function(symbol, args)
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

    #[test]
    fn integer_binding() {
        let mut tokens = generate_test_tokens("integer", "1");
        let ast = parse_binding(&mut tokens);
        assert_eq!(
            ast,
            ast::Binding::Literal(Box::new(ast::terminal::Literal::Integer(1)))
        )
    }

    #[test]
    fn float_binding() {
        let mut tokens = generate_test_tokens("float", "1.0");
        let ast = parse_binding(&mut tokens);
        assert_eq!(
            ast,
            ast::Binding::Literal(Box::new(ast::terminal::Literal::Float(1.0)))
        )
    }

    #[test]
    fn string_binding() {
        let mut tokens = generate_test_tokens("string", "\"string\"");
        let ast = parse_binding(&mut tokens);
        assert_eq!(
            ast,
            ast::Binding::Literal(Box::new(ast::terminal::Literal::String(
                "string".to_string()
            )))
        )
    }

    #[test]
    fn bool_binding() {
        {
            let mut tokens = generate_test_tokens("bool_true", "true");
            let ast = parse_binding(&mut tokens);
            assert_eq!(
                ast,
                ast::Binding::Literal(Box::new(ast::terminal::Literal::Bool(true)))
            )
        }
        {
            let mut tokens = generate_test_tokens("bool_false", "false");
            let ast = parse_binding(&mut tokens);
            assert_eq!(
                ast,
                ast::Binding::Literal(Box::new(ast::terminal::Literal::Bool(false)))
            )
        }
    }

    #[test]
    fn field_binding() {
        let mut tokens = generate_test_tokens("field_binding", "foo");
        let ast = parse_binding(&mut tokens);
        assert_eq!(
            ast,
            ast::Binding::Field(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined
                },
                None
            )
        );
    }

    #[test]
    fn empty_function_binding() {
        let mut tokens = generate_test_tokens("empty_function_binding", "foo()");
        let ast = parse_binding(&mut tokens);
        assert_eq!(
            ast,
            ast::Binding::Function(
                ast::FunctionCall::Function(
                    Symbol {
                        name: "foo".to_string(),
                        ty: Type::Undefined
                    },
                    vec![]
                ),
                None
            )
        )
    }

    #[test]
    fn member_binding() {
        let mut tokens = generate_test_tokens("empty_function_binding", "foo.bar");
        let ast = parse_binding(&mut tokens);
        assert_eq!(
            ast,
            ast::Binding::Field(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined
                },
                Some(Box::new(ast::Binding::Field(
                    Symbol {
                        name: "bar".to_string(),
                        ty: Type::Undefined
                    },
                    None
                )))
            )
        )
    }

    #[test]
    fn method_binding() {
        let mut tokens = generate_test_tokens("empty_function_binding", "foo.bar()");
        let ast = parse_binding(&mut tokens);
        assert_eq!(
            ast,
            ast::Binding::Field(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined
                },
                Some(Box::new(ast::Binding::Function(
                    ast::FunctionCall::Function(
                        Symbol {
                            name: "bar".to_string(),
                            ty: Type::Undefined
                        },
                        vec![]
                    ),
                    None
                )))
            )
        )
    }

    #[test]
    fn chain_method_binding() {
        let mut tokens = generate_test_tokens("empty_function_binding", "foo().bar()");
        let ast = parse_binding(&mut tokens);
        assert_eq!(
            ast,
            ast::Binding::Function(
                ast::FunctionCall::Function(
                    Symbol {
                        name: "foo".to_string(),
                        ty: Type::Undefined
                    },
                    vec![]
                ),
                Some(Box::new(ast::Binding::Function(
                    ast::FunctionCall::Function(
                        Symbol {
                            name: "bar".to_string(),
                            ty: Type::Undefined
                        },
                        vec![]
                    ),
                    None
                )))
            )
        )
    }

    #[test]
    fn chain_member_binding() {
        let mut tokens = generate_test_tokens("empty_function_binding", "foo().bar");
        let ast = parse_binding(&mut tokens);
        assert_eq!(
            ast,
            ast::Binding::Function(
                ast::FunctionCall::Function(
                    Symbol {
                        name: "foo".to_string(),
                        ty: Type::Undefined
                    },
                    vec![]
                ),
                Some(Box::new(ast::Binding::Field(
                    Symbol {
                        name: "bar".to_string(),
                        ty: Type::Undefined
                    },
                    None
                )))
            )
        )
    }

    #[test]
    fn expr_add() {
        let mut tokens = generate_test_tokens("add", "1 + 2");
        let ast = parse_expression(&mut tokens);
        assert_eq!(
            ast,
            ast::Expression::Binary(
                ast::terminal::Operator::Add,
                Box::new(ast::Expression::Primary(Box::new(ast::Binding::Literal(
                    Box::new(ast::terminal::Literal::Integer(1))
                )))),
                Box::new(ast::Expression::Primary(Box::new(ast::Binding::Literal(
                    Box::new(ast::terminal::Literal::Integer(2))
                ))))
            )
        )
    }

    #[test]
    fn expr_and() {
        let mut tokens = generate_test_tokens("add", "foo && bar()");
        let ast = parse_expression(&mut tokens);
        assert_eq!(
            ast,
            ast::Expression::Binary(
                ast::terminal::Operator::And,
                Box::new(ast::Expression::Primary(Box::new(ast::Binding::Field(
                    Symbol {
                        name: "foo".to_string(),
                        ty: Type::Undefined
                    },
                    None
                )))),
                Box::new(ast::Expression::Primary(Box::new(ast::Binding::Function(
                    ast::FunctionCall::Function(
                        Symbol {
                            name: "bar".to_string(),
                            ty: Type::Undefined
                        },
                        vec![]
                    ),
                    None
                ))))
            )
        )
    }

    #[test]
    fn expr_incr() {
        let mut tokens = generate_test_tokens("add", "foo++");
        let ast = parse_expression(&mut tokens);
        assert_eq!(
            ast,
            ast::Expression::Unary(
                ast::terminal::UnaryOperator::Increment,
                Box::new(ast::Expression::Primary(Box::new(ast::Binding::Field(
                    Symbol {
                        name: "foo".to_string(),
                        ty: Type::Undefined
                    },
                    None
                )))),
            )
        );
    }

}
