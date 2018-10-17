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

pub mod ast {

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
        Assign(Assignment), // type a = b
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
        Call(Box<Expression>),
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum Expression {
        Literal(
            Box<terminal::Literal>,
            Option<Box<Expression>>, /* chained function */
        ),
        Field(Symbol, Option<Box<Expression>> /* chained function */),
        Function(
            Symbol,
            Vec<Expression>,         /* args */
            Option<Box<Expression>>, /* chained function */
        ),
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

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use self::Type as T;
        match &self {
            T::Int => write!(f, "int"),
            T::Float => write!(f, "float"),
            T::Double => write!(f, "double"),
            T::Uint => write!(f, "uint"),
            T::Bool => write!(f, "bool"),
            T::String => write!(f, "string"),
            T::Ptr(t) => write!(f, "ptr({})", t),
            T::V1 => write!(f, "v1"),
            T::V2 => write!(f, "v2"),
            T::V4 => write!(f, "v4"),
            T::V8 => write!(f, "v8"),
            T::V16 => write!(f, "v16"),
            T::V32 => write!(f, "v32"),
            T::V64 => write!(f, "v64"),
            T::List(t) => write!(f, "[{}]", t),
            T::Function(l, r) => write!(f, "({} -> {})", l, r),
            T::Tuple(v) => {
                let s = v
                    .iter()
                    .fold("".to_owned(), |acc, t| acc + t.to_string().as_str() + ", ");
                write!(f, "({})", s)
            }
            T::Declaration(s) => write!(f, "{}", s),
            T::Generic(s, t) => write!(f, "{}({})", s, t),
            T::Undefined => write!(f, "<?>"),
            T::Struct => write!(f, "struct"),
            T::Enum => write!(f, "enum"),
            T::EnumMember => write!(f, "enum member"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Symbol {
    name: String,
    ty: Type,
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} {}", self.ty, self.name)
    }
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
    ast::Top::Assign(parse_assignment(tokens))
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
            ast::Scope::Open(Box::new(ast::Statement::Call(Box::new(expr))))
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

fn lookahead_for_assign(tokens: &mut Tokens) -> Result<bool, ast::ParseError> {
    let mut lookahead = Vec::new();
    loop {
        match tokens.pop() {
            Some(token) => {
                match token {
                    (pos, Token::End) => {
                        lookahead.push((pos, Token::End));
                        while let Some(t) = lookahead.pop() {
                            tokens.push(t);
                        }
                        return Ok(false);
                    },
                    (pos, Token::Assign) => {
                        lookahead.push((pos, Token::Assign));
                        while let Some(t) = lookahead.pop() {
                            tokens.push(t);
                        }
                        return Ok(true);
                    },
                    other => {
                        lookahead.push(other);
                    }
                }
            },
            None => return Err(ast::ParseError::UnexpectedEOF)
        }
    }

}

fn parse_statement(tokens: &mut Tokens) -> ast::Statement {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Statement::Error(ast::ParseError::UnexpectedEOF),
    };

    let is_assign = match lookahead_for_assign(tokens) {
        Ok(val) => val,
        Err(e) => return ast::Statement::Error(e)
    };

    match token {
        (_, Token::Function(_)) => {
            tokens.push(token);
            let call = parse_function_call(tokens);
            expect_tokens!(tokens, self::ast::Statement, "Expected ;",
                (Token::End => {}),);
            ast::Statement::Call(Box::new(call))
        }
        _ => {
            tokens.push(token);
            if is_assign {
                let assign = parse_assignment(tokens);
                ast::Statement::Assign(Box::new(assign))
            } else {
                let call = parse_expression(tokens);
                expect_tokens!(tokens, self::ast::Statement, "Expected ;",
                (Token::End => {}),);
                ast::Statement::Call(Box::new(call))
            }
            
        }
    }
}

/// Precedence order is C order: https://en.cppreference.com/w/c/language/operator_precedence

macro_rules! parse_binary_expr {
    ($tokens: ident, $next_parse: ident, $(($left: expr => $op: expr),)*) => ({
        let mut node = $next_parse($tokens);
        while let Some(token) = $tokens.pop() {
        node = match token {
            (pos, Token::Operator(op)) => match op.as_ref() {
                $(
                    $left => ast::Expression::Function(
                        Symbol {name: $op.to_string(), ty: Type::Undefined},
                        vec![node, $next_parse($tokens)],
                        None
                    ),
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
    parse_binary_expr!(tokens, parse_and_expr, ("||" => "$or"),)
}

fn parse_and_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_bor_expr, ("&&" => "$and"),)
}

fn parse_bor_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_xor_expr, ("|" => "$bor"),)
}

fn parse_xor_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_band_expr, ("^" => "$bxor"),)
}

fn parse_band_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_cmp_expr, ("&" => "$band"),)
}

fn parse_cmp_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_bitshift_expr, ("==" => "$eq"),
    ("!=" => "$neq"),
    ("<" => "$le"),
    (">" => "$ge"),
    ("<=" => "$geq"),
    (">=" => "$leq"),)
}

fn parse_bitshift_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_add_expr, ("<<" => "$bsleft"),
    (">>" => "$bsright"),)
}

fn parse_add_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_mul_expr, ("+" => "$add"),
    ("-" => "$sub"),)
}

fn parse_mul_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_unary_expr, ("*" => "$mul"),
    ("/" => "$div"),)
}

fn parse_unary_expr(tokens: &mut Tokens) -> ast::Expression {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Expression::Error(ast::ParseError::UnexpectedEOF),
    };

    match token {
        (_, Token::Operator(op)) => match op.as_ref() {
            "-" => ast::Expression::Function(
                Symbol {
                    name: "$neg".to_string(),
                    ty: Type::Undefined,
                },
                vec![parse_primary_expr(tokens)],
                None,
            ),
            "~" => ast::Expression::Function(
                Symbol {
                    name: "$bneg".to_string(),
                    ty: Type::Undefined,
                },
                vec![parse_primary_expr(tokens)],
                None,
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
                            node = ast::Expression::Function(
                                Symbol {
                                    name: "$incr".to_string(),
                                    ty: Type::Undefined,
                                },
                                vec![node],
                                None,
                            )
                        }
                        "--" => {
                            node = ast::Expression::Function(
                                Symbol {
                                    name: "$decr".to_string(),
                                    ty: Type::Undefined,
                                },
                                vec![node],
                                None,
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
            (parse_binding(tokens))
        }
    }
}

fn parse_binding(tokens: &mut Tokens) -> ast::Expression {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Expression::Error(ast::ParseError::UnexpectedEOF),
    };

    match token {
        (_, Token::Function(_)) => {
            tokens.push(token);
            let fun = parse_function_call(tokens);
            let chain = parse_chained_binding(tokens);
            match fun {
                ast::Expression::Function(symbol, args, chain) => {
                    ast::Expression::Function(symbol, args, chain)
                }
                _ => unreachable!(),
            }
        }
        (_, Token::Name(name)) => ast::Expression::Field(
            Symbol {
                name: name.clone(),
                ty: Type::Undefined,
            },
            parse_chained_binding(tokens),
        ),
        (_, Token::Float(x)) => ast::Expression::Literal(
            Box::new(ast::terminal::Literal::Float(x)),
            parse_chained_binding(tokens),
        ),
        (_, Token::Integer(n)) => ast::Expression::Literal(
            Box::new(ast::terminal::Literal::Integer(n)),
            parse_chained_binding(tokens),
        ),
        (_, Token::Truthy(b)) => ast::Expression::Literal(
            Box::new(ast::terminal::Literal::Bool(b)),
            parse_chained_binding(tokens),
        ),
        (_, Token::String(s)) => ast::Expression::Literal(
            Box::new(ast::terminal::Literal::String(s.clone())),
            parse_chained_binding(tokens),
        ),
        (pos, token) => ast::Expression::Error(ast::ParseError::UnexpectedToken(
            (pos, token),
            "Expected function, name, or literal binding".to_string(),
        )),
    }
}

fn chained_binding_append(
    binding: Option<Box<ast::Expression>>,
    append: Option<Box<ast::Expression>>,
) -> Option<Box<ast::Expression>> {
    match binding {
        Some(binding) => match (*binding).clone() {
            ast::Expression::Literal(sym, chain) => {
                let chain = if chain.is_some() {
                    chained_binding_append(chain, append)
                } else {
                    append
                };
                Some(Box::new(ast::Expression::Literal(sym, chain)))
            },
            ast::Expression::Function(sym, args, chain) => {
                let chain = if chain.is_some() {
                    chained_binding_append(chain, append)
                } else {
                    append
                };
                Some(Box::new(ast::Expression::Function(sym, args, chain)))
            },
            ast::Expression::Field(sym, mut chain) => {
                let chain = if chain.is_some() {
                    chained_binding_append(chain, append)
                } else {
                    append
                };
                Some(Box::new(ast::Expression::Field(sym, chain)))
            }
            ast::Expression::Error(_) => panic!("Error descending chained binding"),
        },
        None => None
    }
}

fn parse_chained_binding(tokens: &mut Tokens) -> Option<Box<ast::Expression>> {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return None,
    };

    match token {
        (pos, Token::Method(name)) => {
            tokens.push((pos, Token::Function(name)));
            let fun = parse_function_call(tokens);
            let chain = parse_chained_binding(tokens);
            match fun {
                ast::Expression::Function(symbol, args, mut next) => {
                    next = chained_binding_append(next, chain);

                    Some(Box::new(ast::Expression::Function(symbol, args, next)))
                }
                _ => panic!("Error parsing chained binding"),
            }
        }
        (_, Token::Member(name)) => Some(Box::new(ast::Expression::Field(
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

fn parse_function_call(tokens: &mut Tokens) -> ast::Expression {
    let symbol = match tokens.pop() {
        Some(val) => match val {
            (_, Token::Function(name)) => Symbol {
                name,
                ty: Type::Undefined,
            },
            (pos, token) => {
                return ast::Expression::Error(ast::ParseError::UnexpectedToken(
                    (pos.clone(), token.clone()),
                    "Expected function".to_string(),
                ))
            }
        },
        None => return ast::Expression::Error(ast::ParseError::UnexpectedEOF),
    };

    let mut args: Vec<ast::Expression> = Vec::new();

    // Guard against empty function call
    match tokens.pop() {
        Some(val) => match val {
            (_, Token::RightPar) => return ast::Expression::Function(symbol, vec![], None),
            other => {
                tokens.push(other);
            }
        },
        None => return ast::Expression::Error(ast::ParseError::UnexpectedEOF),
    }

    loop {
        args.push(parse_expression(tokens));

        match tokens.pop() {
            Some(val) => match val {
                (_, Token::Comma) => continue,
                (_, Token::RightPar) => break,
                (pos, token) => {
                    return ast::Expression::Error(ast::ParseError::UnexpectedToken(
                        (pos.clone(), token.clone()),
                        "Expected comma or right par".to_string(),
                    ))
                }
            },
            None => return ast::Expression::Error(ast::ParseError::UnexpectedEOF),
        };
    }

    ast::Expression::Function(symbol, args, None)
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
            ast::Expression::Literal(Box::new(ast::terminal::Literal::Integer(1)), None)
        )
    }

    #[test]
    fn float_binding() {
        let mut tokens = generate_test_tokens("float", "1.0");
        let ast = parse_binding(&mut tokens);
        assert!(tokens.is_empty());

        assert_eq!(
            ast,
            ast::Expression::Literal(Box::new(ast::terminal::Literal::Float(1.0)), None)
        )
    }

    #[test]
    fn string_binding() {
        let mut tokens = generate_test_tokens("string", "\"string\"");
        let ast = parse_binding(&mut tokens);
        assert!(tokens.is_empty());

        assert_eq!(
            ast,
            ast::Expression::Literal(
                Box::new(ast::terminal::Literal::String("string".to_string())),
                None
            )
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
                ast::Expression::Literal(Box::new(ast::terminal::Literal::Bool(true)), None)
            )
        }
        {
            let mut tokens = generate_test_tokens("bool_false", "false");
            let ast = parse_binding(&mut tokens);
            assert!(tokens.is_empty());

            assert_eq!(
                ast,
                ast::Expression::Literal(Box::new(ast::terminal::Literal::Bool(false)), None)
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
            ast::Expression::Field(
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
            ast::Expression::Function(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined
                },
                vec![],
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
            ast::Expression::Field(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined
                },
                Some(Box::new(ast::Expression::Field(
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
            ast::Expression::Field(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined
                },
                Some(Box::new(ast::Expression::Function(
                    Symbol {
                        name: "bar".to_string(),
                        ty: Type::Undefined
                    },
                    vec![],
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
            ast::Expression::Function(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined
                },
                vec![],
                Some(Box::new(ast::Expression::Function(
                    Symbol {
                        name: "bar".to_string(),
                        ty: Type::Undefined
                    },
                    vec![],
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
            ast::Expression::Function(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined
                },
                vec![],
                Some(Box::new(ast::Expression::Field(
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
            ast::Expression::Function(
                Symbol {
                    name: "$add".to_string(),
                    ty: Type::Undefined
                },
                vec![
                    ast::Expression::Literal(Box::new(ast::terminal::Literal::Integer(1)), None),
                    ast::Expression::Literal(Box::new(ast::terminal::Literal::Integer(2)), None)
                ],
                None
            )
        )
    }

    #[test]
    fn expr_and() {
        let mut tokens = generate_test_tokens("and", "foo && bar()");
        let ast = parse_expression(&mut tokens);
        assert!(tokens.is_empty());

        assert_eq!(
            ast,
            ast::Expression::Function(
                Symbol {
                    name: "$and".to_string(),
                    ty: Type::Undefined
                },
                vec![
                    ast::Expression::Field(
                        Symbol {
                            name: "foo".to_string(),
                            ty: Type::Undefined
                        },
                        None
                    ),
                    ast::Expression::Function(
                        Symbol {
                            name: "bar".to_string(),
                            ty: Type::Undefined
                        },
                        vec![],
                        None
                    )
                ],
                None
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
            ast::Expression::Function(
                Symbol {
                    name: "$incr".to_string(),
                    ty: Type::Undefined
                },
                vec![ast::Expression::Field(
                    Symbol {
                        name: "foo".to_string(),
                        ty: Type::Undefined
                    },
                    None
                )],
                None
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
                ast::Scope::Open(Box::new(ast::Statement::Call(Box::new(
                    ast::Expression::Field(
                        Symbol {
                            name: "bar".to_string(),
                            ty: Type::Undefined
                        },
                        None
                    )
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
            ast::Statement::Call(Box::new(ast::Expression::Function(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined
                },
                vec![
                    ast::Expression::Field(
                        Symbol {
                            name: "bar".to_string(),
                            ty: Type::Undefined
                        },
                        None
                    ),
                    ast::Expression::Field(
                        Symbol {
                            name: "baz".to_string(),
                            ty: Type::Undefined
                        },
                        None
                    )
                ],
                None
            )))
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
                ast::Scope::Open(Box::new(ast::Statement::Call(Box::new(
                    ast::Expression::Literal(Box::new(ast::terminal::Literal::Integer(1),), None)
                ))))
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
                ast::Scope::Open(Box::new(ast::Statement::Call(Box::new(
                    ast::Expression::Literal(Box::new(ast::terminal::Literal::Integer(1)), None)
                ))))
            )
        );
    }

    #[test]
    fn stmt_body() {
        let mut tokens = generate_test_tokens(
            "stmt_block",
            "{mut float foo = 1.0; \n bar(foo); \n foo;}",
        );
        let ast = parse_scope(&mut tokens);
        assert!(tokens.is_empty());

        let stmt1 = ast::Statement::Assign(Box::new(ast::Assignment::Field(
            ast::terminal::Constness::Mut,
            Symbol {
                name: "foo".to_string(),
                ty: Type::Float,
            },
            ast::Scope::Open(Box::new(ast::Statement::Call(Box::new(
                ast::Expression::Literal(Box::new(ast::terminal::Literal::Float(1.0)), None),
            )))),
        )));

        let stmt2 = ast::Statement::Call(Box::new(ast::Expression::Function(
            Symbol {
                name: "bar".to_string(),
                ty: Type::Undefined,
            },
            vec![ast::Expression::Field(
                Symbol {
                    name: "foo".to_string(),
                    ty: Type::Undefined,
                },
                None,
            )],
            None,
        )));

        let stmt3 = ast::Statement::Call(Box::new(ast::Expression::Field(
            Symbol {
                name: "foo".to_string(),
                ty: Type::Undefined,
            },
            None,
        )));

        assert_eq!(ast, ast::Scope::Closed(vec![stmt1, stmt2, stmt3]));
    }

}
