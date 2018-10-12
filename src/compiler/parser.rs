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
            Scope,               // = {...} | = ... ;
        ),
        Field(
            terminal::Constness, // const | mut | None
            Symbol,              // name
            Scope,               // = {...} | = ... ;
        ),
        Reassign(
            Symbol, // name
            Scope,  // = {...} | = ... ;
        ),
        Struct(Symbol, Vec<Box<(Symbol)>>), // struct name = {(name,)*}
        Enum(Symbol, Vec<Box<(Symbol)>>),   // enum name = {(name,)*}
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum Scope {
        Closed(Vec<Statement>),
        Open(Box<Statement>),
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum Statement {
        Assign(Box<Assignment>),
        Call(Box<FunctionCall>),
        If(
            Box<Expression>,               // If expression
            Scope,                         // Then statement
            Vec<(Box<Expression>, Scope)>, // (else if expression then statement )*
            Option<Scope>,                 // Else
        ),
        Match(
            Symbol,
            Vec<(Symbol, Scope)>,
            Option<Scope>, /* _ => stmt */
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
    Tuple(Vec<Type>),
    Declaration(String),
    Generic(String, Box<Type>),
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

macro_rules! expect_tokens_or_err {
    ($tokens: ident, $error_message: expr, $(($type: pat => $result: expr),)*) => {
        match $tokens.pop() {
            Some(val) => match val {
                $((_, $type) => $result,)*
                other => {
                    return Err(ast::ParseError::UnexpectedToken(
                        other,
                        $error_message.to_string(),
                    ))
                }
            },
            None => {
                return Err(ast::ParseError::UnexpectedEOF)
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

fn peek_assign(tokens: &mut Tokens) -> Result<bool, ast::ParseError> {
    let l1 = match tokens.pop() {
        Some(val) => val,
        None => return Err(ast::ParseError::UnexpectedEOF),
    };

    let l2 = match tokens.pop() {
        Some(val) => val,
        None => return Err(ast::ParseError::UnexpectedEOF),
    };

    match l2 {
        (_, Token::Assign) => {
            tokens.push(l2);
            tokens.push(l1);
            Ok(true)
        }
        _ => {
            tokens.push(l2);
            tokens.push(l1);
            Ok(false)
        }
    }
}

fn parse_assignment(tokens: &mut Tokens) -> ast::Assignment {
    let token = match tokens.pop() {
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

    let has_type = match peek_assign(tokens) {
        Ok(ok) => !ok,
        Err(e) => return ast::Assignment::Error(e),
    };

    let ty = if has_type {
        match parse_signature(tokens) {
            Ok(ok) => Some(ok),
            Err(e) => return ast::Assignment::Error(e),
        }
    } else {
        None
    };

    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Assignment::Error(ast::ParseError::UnexpectedEOF),
    };

    if has_type {
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

fn parse_scope(tokens: &mut Tokens) -> ast::Scope {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Scope::Error(ast::ParseError::UnexpectedEOF),
    };

    match token {
        (_, Token::LeftCurl) => {
            let mut stmt = Vec::new();

            loop {
                stmt.push(parse_statement(tokens));

                match tokens.pop() {
                    Some(token) => match token {
                        (_, Token::RightCurl) => break,
                        _ => tokens.push(token),
                    },
                    None => return ast::Scope::Error(ast::ParseError::UnexpectedEOF),
                }
            }

            ast::Scope::Closed(stmt)
        }
        _ => {
            tokens.push(token);
            let expr = parse_expression(tokens);
            expect_tokens!(tokens, self::ast::Scope, "Expected ;",
                    (Token::End => {}),);
            ast::Scope::Open(Box::new(ast::Statement::Return(Box::new(expr))))
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

    expect_tokens!(tokens, self::ast::Assignment, "Expected function assignment (=)",
                (Token::Assign => {}),);

    let stmt = parse_scope(tokens);

    ast::Assignment::Function(constness, Symbol { name, ty }, params, stmt)
}

fn parse_field_assignment(
    tokens: &mut Tokens,
    constness: ast::terminal::Constness,
    name: String,
    ty: Type,
) -> ast::Assignment {
    expect_tokens!(tokens, self::ast::Assignment, "Expected field assignment (=)",
                (Token::Assign => {}),);

    let stmt = parse_scope(tokens);

    ast::Assignment::Field(constness, Symbol { name, ty }, stmt)
}

fn parse_struct_assignment(tokens: &mut Tokens) -> ast::Assignment {
    let name = expect_tokens!(tokens,
                    self::ast::Assignment, "Expected enum name",
                    (Token::Name(name) => name.to_string()),);

    expect_tokens!(tokens, self::ast::Assignment, "Expected struct assignment (=)",
                (Token::Assign => {}),);

    expect_tokens!(tokens, self::ast::Assignment, "Expected {",
                (Token::LeftCurl => {}),);

    let symbols = match parse_struct_symbols(tokens) {
        Ok(ok) => ok,
        Err(e) => return ast::Assignment::Error(e),
    };

    expect_tokens!(tokens, self::ast::Assignment, "Expected }",
                (Token::RightCurl => {}),);

    ast::Assignment::Struct(
        Symbol {
            name,
            ty: Type::Struct,
        },
        symbols,
    )
}

fn parse_enum_assignment(tokens: &mut Tokens) -> ast::Assignment {
    let name = expect_tokens!(tokens,
                    self::ast::Assignment, "Expected enum name",
                    (Token::Name(name) => name.to_string()),);

    expect_tokens!(tokens, self::ast::Assignment, "Expected enum assignment (=)",
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
    expect_tokens!(tokens, self::ast::Assignment, "Expected reassignment (=)",
                (Token::Assign => {}),);

    let stmt = parse_scope(tokens);

    ast::Assignment::Reassign(
        Symbol {
            name,
            ty: Type::Undefined,
        },
        stmt,
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

fn parse_struct_symbols(tokens: &mut Tokens) -> Result<Vec<Box<Symbol>>, ast::ParseError> {
    let mut symbols = Vec::new();

    loop {
        let ty = match parse_signature(tokens) {
            Ok(ok) => ok,
            Err(e) => return Err(e),
        };

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
            (_, Token::Name(name)) => symbols.push(Box::new(Symbol { name, ty })),
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

fn parse_signature(tokens: &mut Tokens) -> Result<Type, ast::ParseError> {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return Err(ast::ParseError::UnexpectedEOF),
    };

    match token {
        (_, Token::LeftPar) => {
            let mut types = Vec::new();

            match parse_signature(tokens) {
                Ok(ok) => types.push(ok),
                Err(e) => return Err(e),
            }

            let token = match tokens.pop() {
                Some(val) => val,
                None => return Err(ast::ParseError::UnexpectedEOF),
            };

            match token {
                (_, Token::RightArrow) => {
                    let left = types.remove(0);
                    let right = match parse_signature(tokens) {
                        Ok(ok) => ok,
                        Err(e) => return Err(e),
                    };
                    return Ok(Type::Function(Box::new(left), Box::new(right)));
                }
                _ => tokens.push(token),
            }

            loop {
                let token = match tokens.pop() {
                    Some(val) => val,
                    None => return Err(ast::ParseError::UnexpectedEOF),
                };
                match token {
                    (_, Token::Comma) => continue,
                    (_, Token::RightPar) => break,
                    _ => {
                        tokens.push(token);
                        match parse_signature(tokens) {
                            Ok(ok) => types.push(ok),
                            Err(e) => return Err(e),
                        }
                    }
                }
            }
            Ok(Type::Tuple(types))
        }
        (_, Token::Name(name)) => match name.as_ref() {
            "int" => Ok(Type::Int),
            "float" => Ok(Type::Float),
            "uint" => Ok(Type::Uint),
            "bool" => Ok(Type::Bool),
            "string" => Ok(Type::String),
            "v1" => Ok(Type::V1),
            "v2" => Ok(Type::V2),
            "v4" => Ok(Type::V4),
            "v8" => Ok(Type::V8),
            "v16" => Ok(Type::V16),
            "v32" => Ok(Type::V32),
            "v64" => Ok(Type::V64),
            other => Ok(Type::Declaration(other.to_string())),
        },
        (_, Token::Function(name)) => match name.as_ref() {
            "ptr" => {
                let t = match parse_signature(tokens) {
                    Ok(ok) => ok,
                    Err(e) => return Err(e),
                };
                Ok(Type::Ptr(Box::new(t)))
            }
            "list" => {
                let t = match parse_signature(tokens) {
                    Ok(ok) => ok,
                    Err(e) => return Err(e),
                };
                Ok(Type::List(Box::new(t)))
            }
            other => {
                let t = match parse_signature(tokens) {
                    Ok(ok) => ok,
                    Err(e) => return Err(e),
                };
                Ok(Type::Generic(other.to_string(), Box::new(t)))
            }
        },
        other => Err(ast::ParseError::UnexpectedToken(
            other,
            "Expected type signature".to_string(),
        )),
    }
}

fn parse_statement(tokens: &mut Tokens) -> ast::Statement {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Statement::Error(ast::ParseError::UnexpectedEOF),
    };

    match token {
        (_, Token::If) => return parse_if_statement(tokens),
        (_, Token::Match) => return parse_match_statement(tokens),
        (_, Token::Function(_)) => {
            tokens.push(token);
            return parse_call_statement(tokens);
        }
        (_, Token::Return) => {
            let expr = parse_expression(tokens);
            expect_tokens!(tokens, self::ast::Statement, "Expected ;",
                (Token::End => {}),);
            return ast::Statement::Return(Box::new(expr));
        }
        _ => tokens.push(token),
    }

    ast::Statement::Assign(Box::new(parse_assignment(tokens)))
}

fn parse_if_statement(tokens: &mut Tokens) -> ast::Statement {
    let condition = parse_expression(tokens);

    let stmt = parse_scope(tokens);

    let mut else_if_blocks = Vec::new();

    loop {
        let else_token = match tokens.pop() {
            Some(val) => val,
            None => break,
        };

        match else_token {
            (_, Token::Else) => {}
            _ => {
                tokens.push(else_token);
                break;
            }
        }

        let if_token = match tokens.pop() {
            Some(val) => val,
            None => return ast::Statement::Error(ast::ParseError::UnexpectedEOF),
        };

        match if_token {
            (_, Token::If) => {}
            _ => {
                tokens.push(if_token);
                tokens.push(else_token);
                break;
            }
        }

        let condition = parse_expression(tokens);

        let stmt = parse_scope(tokens);

        else_if_blocks.push((Box::new(condition), stmt));
    }

    let else_block = match tokens.pop() {
        Some(token) => match token {
            (_, Token::Else) => {
                let stmt = parse_scope(tokens);

                Some(stmt)
            }
            _ => {
                tokens.push(token);
                None
            }
        },
        None => None,
    };

    ast::Statement::If(Box::new(condition), stmt, else_if_blocks, else_block)
}

fn parse_match_statement(tokens: &mut Tokens) -> ast::Statement {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Statement::Error(ast::ParseError::UnexpectedEOF),
    };

    let symbol = match token {
        (_, Token::Name(name)) => Symbol {
            name,
            ty: Type::Undefined,
        },
        _ => {
            return ast::Statement::Error(ast::ParseError::UnexpectedToken(
                token,
                "Expected match name".to_string(),
            ))
        }
    };

    expect_tokens!(tokens, self::ast::Statement, "Expected {",
                (Token::LeftCurl => {}),);

    let mut arms = Vec::new();
    let mut wildcard_arm = None;

    loop {
        let token = match tokens.pop() {
            Some(val) => val,
            None => return ast::Statement::Error(ast::ParseError::UnexpectedEOF),
        };

        match token {
            (_, Token::Wildcard) => {
                expect_tokens!(tokens, self::ast::Statement, "Expected =>",
                        (Token::FatRightArrow => {}),);
                let stmt = parse_scope(tokens);
                wildcard_arm = Some(stmt);
            }
            (_, Token::RightCurl) => break,
            (_, Token::Name(name)) => {
                expect_tokens!(tokens, self::ast::Statement, "Expected =>",
                        (Token::FatRightArrow => {}),);
                let stmt = parse_scope(tokens);
                arms.push((
                    Symbol {
                        name,
                        ty: Type::Undefined,
                    },
                    stmt,
                ))
            }
            other => {
                return ast::Statement::Error(ast::ParseError::UnexpectedToken(
                    other,
                    "Expected match token".to_string(),
                ))
            }
        }
    }

    ast::Statement::Match(symbol, arms, wildcard_arm)
}

fn parse_call_statement(tokens: &mut Tokens) -> ast::Statement {
    let call = parse_function_call(tokens);
    expect_tokens!(tokens, self::ast::Statement, "Expected ;",
        (Token::End => {}),);
    ast::Statement::Call(Box::new(call))
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
    let token = match tokens.pop() {
        Some(val) => val,
        None => return None,
    };

    match token {
        (pos, Token::Method(name)) => {
            tokens.push((pos, Token::Function(name)));
            let fun = parse_function_call(tokens);
            let chain = parse_chained_binding(tokens);
            Some(Box::new(ast::Binding::Function(fun, chain)))
        }
        (_, Token::Member(name)) => Some(Box::new(ast::Binding::Field(
            Symbol {
                name: name.clone(),
                ty: Type::Undefined,
            },
            parse_chained_binding(tokens),
        ))),
        other => {
            tokens.push(other);
            None
        }
    }
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
        assert!(tokens.is_empty());

        assert_eq!(
            ast,
            ast::Binding::Literal(Box::new(ast::terminal::Literal::Integer(1)))
        )
    }

    #[test]
    fn float_binding() {
        let mut tokens = generate_test_tokens("float", "1.0");
        let ast = parse_binding(&mut tokens);
        assert!(tokens.is_empty());

        assert_eq!(
            ast,
            ast::Binding::Literal(Box::new(ast::terminal::Literal::Float(1.0)))
        )
    }

    #[test]
    fn string_binding() {
        let mut tokens = generate_test_tokens("string", "\"string\"");
        let ast = parse_binding(&mut tokens);
        assert!(tokens.is_empty());

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
            assert!(tokens.is_empty());

            assert_eq!(
                ast,
                ast::Binding::Literal(Box::new(ast::terminal::Literal::Bool(true)))
            )
        }
        {
            let mut tokens = generate_test_tokens("bool_false", "false");
            let ast = parse_binding(&mut tokens);
            assert!(tokens.is_empty());

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
        assert!(tokens.is_empty());

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
        assert!(tokens.is_empty());

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
        assert!(tokens.is_empty());

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
        assert!(tokens.is_empty());

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
        assert!(tokens.is_empty());

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
        assert!(tokens.is_empty());

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
        assert!(tokens.is_empty());

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
        assert!(tokens.is_empty());

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
        assert!(tokens.is_empty());

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

    #[test]
    fn stmt_assign() {
        let mut tokens = generate_test_tokens("stmt_assign", "int foo = bar;");
        let ast = parse_statement(&mut tokens);
        assert!(tokens.is_empty());

        assert_eq!(
            ast,
            ast::Statement::Assign(Box::new(ast::Assignment::Field(
                ast::terminal::Constness::None,
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Int
                },
                ast::Scope::Open(Box::new(ast::Statement::Return(Box::new(
                    ast::Expression::Primary(Box::new(ast::Binding::Field(
                        Symbol {
                            name: "bar".to_string(),
                            ty: Type::Undefined
                        },
                        None
                    )))
                ))))
            )))
        );
    }

    #[test]
    fn stmt_call() {
        let mut tokens = generate_test_tokens("stmt_call", "foo(bar, baz);");
        let ast = parse_statement(&mut tokens);
        assert!(tokens.is_empty());

        assert_eq!(
            ast,
            ast::Statement::Call(Box::new(ast::FunctionCall::Function(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined
                },
                vec![
                    Box::new(ast::Expression::Primary(Box::new(ast::Binding::Field(
                        Symbol {
                            name: "bar".to_string(),
                            ty: Type::Undefined
                        },
                        None
                    )))),
                    Box::new(ast::Expression::Primary(Box::new(ast::Binding::Field(
                        Symbol {
                            name: "baz".to_string(),
                            ty: Type::Undefined
                        },
                        None
                    ))))
                ]
            )))
        );
    }

    #[test]
    fn stmt_if() {
        let mut tokens = generate_test_tokens("stmt_if", "if a==b {foo();}");
        let ast = parse_statement(&mut tokens);
        assert!(tokens.is_empty());

        let cmp = ast::Expression::Binary(
            ast::terminal::Operator::Equal,
            Box::new(ast::Expression::Primary(Box::new(ast::Binding::Field(
                Symbol {
                    name: "a".to_string(),
                    ty: Type::Undefined,
                },
                None,
            )))),
            Box::new(ast::Expression::Primary(Box::new(ast::Binding::Field(
                Symbol {
                    name: "b".to_string(),
                    ty: Type::Undefined,
                },
                None,
            )))),
        );

        let stmt = ast::Statement::Call(Box::new(ast::FunctionCall::Function(
            Symbol {
                name: "foo".to_string(),
                ty: Type::Undefined,
            },
            vec![],
        )));

        assert_eq!(
            ast,
            ast::Statement::If(Box::new(cmp), 
            ast::Scope::Closed(vec![stmt]), vec![], None)
        );
    }

    #[test]
    fn stmt_if_else() {
        let mut tokens = generate_test_tokens("stmt_if_else", "if a==b {foo();} else {bar();}");
        let ast = parse_statement(&mut tokens);
        println!("AST: {:?},", ast);
        tokens.reverse();
        println!("Leftover tokens: {:?}", tokens);
        assert!(tokens.is_empty());

        let cmp = ast::Expression::Binary(
            ast::terminal::Operator::Equal,
            Box::new(ast::Expression::Primary(Box::new(ast::Binding::Field(
                Symbol {
                    name: "a".to_string(),
                    ty: Type::Undefined,
                },
                None,
            )))),
            Box::new(ast::Expression::Primary(Box::new(ast::Binding::Field(
                Symbol {
                    name: "b".to_string(),
                    ty: Type::Undefined,
                },
                None,
            )))),
        );

        let stmt = ast::Statement::Call(Box::new(ast::FunctionCall::Function(
            Symbol {
                name: "foo".to_string(),
                ty: Type::Undefined,
            },
            vec![],
        )));

        let stmt2 = ast::Statement::Call(Box::new(ast::FunctionCall::Function(
            Symbol {
                name: "bar".to_string(),
                ty: Type::Undefined,
            },
            vec![],
        )));

        assert_eq!(
            ast,
            ast::Statement::If(Box::new(cmp), 
            ast::Scope::Closed(vec![stmt]), 
            vec![], 
            Some(ast::Scope::Closed(vec![stmt2])))
        );
    }

    #[test]
    fn stmt_if_else_if() {
        let mut tokens = generate_test_tokens(
            "stmt_if_else_if",
            "if a==b {foo();} else if c+d>0 {bar();} else {baz();}",
        );
        let ast = parse_statement(&mut tokens);
        println!("AST: {:?},", ast);
        tokens.reverse();
        println!("Leftover tokens: {:?}", tokens);
        assert!(tokens.is_empty());

        let cmp = ast::Expression::Binary(
            ast::terminal::Operator::Equal,
            Box::new(ast::Expression::Primary(Box::new(ast::Binding::Field(
                Symbol {
                    name: "a".to_string(),
                    ty: Type::Undefined,
                },
                None,
            )))),
            Box::new(ast::Expression::Primary(Box::new(ast::Binding::Field(
                Symbol {
                    name: "b".to_string(),
                    ty: Type::Undefined,
                },
                None,
            )))),
        );

        let stmt = ast::Statement::Call(Box::new(ast::FunctionCall::Function(
            Symbol {
                name: "foo".to_string(),
                ty: Type::Undefined,
            },
            vec![],
        )));

        let cmp2 = ast::Expression::Binary(
            ast::terminal::Operator::Greater,
            Box::new(ast::Expression::Binary(
                ast::terminal::Operator::Add,
                Box::new(ast::Expression::Primary(Box::new(ast::Binding::Field(
                    Symbol {
                        name: "c".to_string(),
                        ty: Type::Undefined,
                    },
                    None,
                )))),
                Box::new(ast::Expression::Primary(Box::new(ast::Binding::Field(
                    Symbol {
                        name: "d".to_string(),
                        ty: Type::Undefined,
                    },
                    None,
                )))),
            )),
            Box::new(ast::Expression::Primary(Box::new(ast::Binding::Literal(
                Box::new(ast::terminal::Literal::Integer(0)),
            )))),
        );

        let stmt2 = ast::Statement::Call(Box::new(ast::FunctionCall::Function(
            Symbol {
                name: "bar".to_string(),
                ty: Type::Undefined,
            },
            vec![],
        )));

        let stmt3 = ast::Statement::Call(Box::new(ast::FunctionCall::Function(
            Symbol {
                name: "baz".to_string(),
                ty: Type::Undefined,
            },
            vec![],
        )));

        assert_eq!(
            ast,
            ast::Statement::If(
                Box::new(cmp),
                ast::Scope::Closed(vec![stmt]),
                vec![(Box::new(cmp2), ast::Scope::Closed(vec![stmt2]))],
                Some(ast::Scope::Closed(vec![stmt3]))
            )
        );
    }

    #[test]
    fn stmt_match() {
        let mut tokens = generate_test_tokens(
            "stmt_match",
            "match foo {bar => {a = 1;} baz => {a = 2;} _ => {a = 3;}}",
        );
        let ast = parse_statement(&mut tokens);
        println!("AST: {:?},", ast);
        tokens.reverse();
        println!("Leftover tokens: {:?}", tokens);
        assert!(tokens.is_empty());

        let mut stmt = vec![1, 2, 3].into_iter().map(|value| {
            ast::Statement::Assign(Box::new(ast::Assignment::Reassign(
                Symbol {
                    name: "a".to_string(),
                    ty: Type::Undefined,
                },
                ast::Scope::Open(Box::new(ast::Statement::Return(Box::new(ast::Expression::Primary(
                    Box::new(ast::Binding::Literal(Box::new(
                        ast::terminal::Literal::Integer(value),
                    ))),
                ))))),
            )))
        });

        assert_eq!(
            ast,
            ast::Statement::Match(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined
                },
                vec![
                    (
                        Symbol {
                            name: "bar".to_string(),
                            ty: Type::Undefined
                        },
                        ast::Scope::Closed(vec![stmt.next().unwrap()])
                    ),
                    (
                        Symbol {
                            name: "baz".to_string(),
                            ty: Type::Undefined
                        },
                        ast::Scope::Closed(vec![stmt.next().unwrap()])
                    )
                ],
                Some(ast::Scope::Closed(vec![stmt.next().unwrap()]))
            )
        )
    }

    #[test]
    fn stmt_return() {
        let mut tokens = generate_test_tokens("stmt_return", "return foo;");
        let ast = parse_statement(&mut tokens);
        assert!(tokens.is_empty());

        assert_eq!(
            ast,
            ast::Statement::Return(Box::new(ast::Expression::Primary(Box::new(
                ast::Binding::Field(
                    Symbol {
                        name: "foo".to_string(),
                        ty: Type::Undefined
                    },
                    None
                )
            ))))
        );
    }

    #[test]
    fn assign_field() {
        let mut tokens = generate_test_tokens("assign_field", "mut int foo = 1;");
        let ast = parse_assignment(&mut tokens);
        assert!(tokens.is_empty());

        assert_eq!(
            ast,
            ast::Assignment::Field(
                ast::terminal::Constness::Mut,
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Int
                },
                ast::Scope::Open(Box::new(ast::Statement::Return(Box::new(ast::Expression::Primary(
                    Box::new(ast::Binding::Literal(Box::new(
                        ast::terminal::Literal::Integer(1)
                    )))
                )))))
            )
        );
    }

    #[test]
    fn reassign_field() {
        let mut tokens = generate_test_tokens("reassign_field", "foo = 1;");
        let ast = parse_assignment(&mut tokens);
        assert!(tokens.is_empty());

        assert_eq!(
            ast,
            ast::Assignment::Reassign(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined
                },
                ast::Scope::Open(Box::new(ast::Statement::Return(Box::new(ast::Expression::Primary(
                    Box::new(ast::Binding::Literal(Box::new(
                        ast::terminal::Literal::Integer(1)
                    )))
                )))))
            )
        );
    }

    #[test]
    fn stmt_body() {
        let mut tokens = generate_test_tokens(
            "stmt_block",
            "{mut float foo = 1.0; \n bar(foo); \n return foo;}",
        );
        let ast = parse_scope(&mut tokens);
        assert!(tokens.is_empty());

        let stmt1 = ast::Statement::Assign(Box::new(ast::Assignment::Field(
            ast::terminal::Constness::Mut,
            Symbol {
                name: "foo".to_string(),
                ty: Type::Float,
            },
            ast::Scope::Open(Box::new(ast::Statement::Return(Box::new(ast::Expression::Primary(
                Box::new(ast::Binding::Literal(Box::new(
                    ast::terminal::Literal::Float(1.0),
                ))),
            ))))),
        )));

        let stmt2 = ast::Statement::Call(Box::new(ast::FunctionCall::Function(
            Symbol {
                name: "bar".to_string(),
                ty: Type::Undefined,
            },
            vec![Box::new(ast::Expression::Primary(Box::new(
                ast::Binding::Field(
                    Symbol {
                        name: "foo".to_string(),
                        ty: Type::Undefined,
                    },
                    None,
                ),
            )))],
        )));

        let stmt3 = ast::Statement::Return(Box::new(ast::Expression::Primary(Box::new(
            ast::Binding::Field(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined,
                },
                None,
            ),
        ))));

        assert_eq!(ast, ast::Scope::Closed(vec![stmt1, stmt2, stmt3]));
    }

}
