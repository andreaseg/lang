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
        Other(String)
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
        Assign(
            Symbol,
            Closure,
        ),
        Reassign(
            Symbol,
            Closure,
        ),
        Type(Symbol),
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum Closure {
        Closed(Vec<Statement>),
        Function(
            Vec<Symbol>, // Type aliases and generics 
            Type, // Signature
            Vec<Statement>
        ),
        Open(Box<Statement>),
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum Statement {
        Assign(Box<Assignment>),
        Call(Box<Expression>),
        Return(Box<Expression>),
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum Expression {
        Literal(
            Box<Literal>,
        ),
        Symbol(Symbol),
        Function(
            Symbol,
            Vec<Expression> /* args */
        ),
        Chain(Vec<Expression>),
        Error(ParseError),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum Literal {
        Float(f64),
        Integer(i64),
        String(String),
        Bool(bool),
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
    Variant(Vec<Type>),
    Declaration(String),
    Undefined,
    Error(ast::ParseError)
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
            T::Undefined => write!(f, "<?>"),
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

                    /*
                    Remove tokens until expected token is found. 
                    Allows continued parsing in a legal state to 
                    collect as many parse errors as possible in
                    a single pass.
                    */
                    while let Some(token) = $tokens.pop() {
                        match token {
                             $((_, $type) => break,)*
                             _ => continue
                        }
                    }

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

    match token {
        (_, Token::Let) => parse_field_assignment(tokens),
        (_, Token::Type) => parse_type_assignment(tokens),
        (_, Token::Name(_)) => {
            tokens.push(token);
            parse_field_reassignment(tokens)},
        other => ast::Assignment::Error(ast::ParseError::UnexpectedToken(
            other,
            "Expected declaration".to_string(),
        )),
    }
}



fn parse_field_assignment(tokens: &mut Tokens) -> ast::Assignment {

    let name;

    expect_tokens!(tokens, self::ast::Assignment, "Expected name",
        (Token::Name(n) => name = n),);

    expect_tokens!(tokens, self::ast::Assignment, "Expected assignment (=)",
        (Token::Assign => {}),);

    let closure = parse_closure(tokens);

    ast::Assignment::Assign(Symbol { name, ty: Type::Undefined }, closure)
}

fn parse_field_reassignment(tokens: &mut Tokens) -> ast::Assignment {

    let name;

    expect_tokens!(tokens, self::ast::Assignment, "Expected name",
        (Token::Name(n) => name = n),);

    expect_tokens!(tokens, self::ast::Assignment, "Expected assignment (=)",
        (Token::Assign => {}),);

    let closure = parse_closure(tokens);

    ast::Assignment::Reassign(Symbol { name, ty: Type::Undefined }, closure)
}

fn parse_type_assignment(tokens: &mut Tokens) -> ast::Assignment {
    let name;

    expect_tokens!(tokens, self::ast::Assignment, "Expected name",
        (Token::Name(n) => name = n),);

    expect_tokens!(tokens, self::ast::Assignment, "Expected assignment (=)",
        (Token::Assign => {}),);

    let ty = parse_signature(tokens);

    ast::Assignment::Type(Symbol{name, ty})
}

fn parse_closure(tokens: &mut Tokens) -> ast::Closure {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Closure::Error(ast::ParseError::UnexpectedEOF),
    };

    match token {
        (_, Token::LeftCurl) => {

            let aliases = parse_aliases(tokens);

            let sign = parse_signature(tokens);

            expect_tokens!(tokens, self::ast::Assignment, "Expected name",
        (Token::FatRightArrow =>{}),);
            
            let mut stmt = Vec::new();

            loop {
                stmt.push(parse_statement(tokens));

                match tokens.pop() {
                    Some(token) => match token {
                        (_, Token::RightCurl) => break,
                        _ => tokens.push(token),
                    },
                    None => return ast::Closure::Error(ast::ParseError::UnexpectedEOF),
                }
            }

            ast::Closure::Function(aliases, sign, stmt)
        },
        (_, Token::LeftPar) => {
            let mut stmt = Vec::new();

            loop {
                stmt.push(parse_statement(tokens));

                match tokens.pop() {
                    Some(token) => match token {
                        (_, Token::RightPar) => break,
                        _ => tokens.push(token),
                    },
                    None => return ast::Closure::Error(ast::ParseError::UnexpectedEOF),
                }
            }

            ast::Closure::Closed(stmt)
        },
        _ => {
            tokens.push(token);
            let expr = parse_expression(tokens);
            expect_tokens!(tokens, self::ast::Scope, "Expected ;",
                    (Token::Semicolon => {}),);
            ast::Closure::Open(Box::new(ast::Statement::Return(Box::new(expr))))
        }
    }
}

fn parse_aliases(tokens: &mut Tokens) -> Vec<Symbol> {
    unimplemented!()
}

fn parse_signature(tokens: &mut Tokens) -> Type {
    parse_variant_signature(tokens)
}

fn parse_variant_signature(tokens: &mut Tokens) -> Type {
    let mut sign = vec![parse_tuple_signature(tokens)];
    loop {
        let token = match tokens.pop() {
            Some(val) => val,
            None => return Type::Error(ast::ParseError::UnexpectedEOF),
        };

        match token {
            (_, Token::Operator(n)) => {
                match n.as_ref() {
                    "|" => {},
                    _ => {
                        tokens.push(token);
                        break
                    }
                }
            },
            (_, _) => {
                tokens.push(token);
                break
            }
        }

        sign.push(parse_tuple_signature(tokens));
    }

    if sign.len() > 1 {
        Type::Variant(sign)
    } else {
        sign.pop().unwrap()
    }
}

fn parse_tuple_signature(tokens: &mut Tokens) -> Type {
    let mut sign = vec![parse_function_signature(tokens)];
    loop {
        let token = match tokens.pop() {
            Some(val) => val,
            None => return Type::Error(ast::ParseError::UnexpectedEOF),
        };

        match token {
            (_, Token::Operator(n)) => {
                match n.as_ref() {
                    "," => {},
                    _ => {
                        tokens.push(token);
                        break
                    }
                }
            },
            (_, Token::RightPar) => {
                break
            }
            (_, _) => {
                tokens.push(token);
                break
            }
        }

        sign.push(parse_tuple_signature(tokens));
    }

    if sign.len() > 1 {
        Type::Tuple(sign)
    } else {
        sign.pop().unwrap()
    }
}

fn parse_function_signature(tokens: &mut Tokens) -> Type {
    let left = parse_primary_signature(tokens);

    let token = match tokens.pop() {
        Some(val) => val,
        None => return Type::Error(ast::ParseError::UnexpectedEOF),
    };

    match token {
        (_, Token::RightArrow) => {},
        (_, _) => {
            tokens.push(token);
            return left;
        }
    }

    let right = parse_primary_signature(tokens);

    Type::Function(Box::new(left), Box::new(right))
}

fn parse_primary_signature(tokens: &mut Tokens) -> Type {
    let token = match tokens.pop() {
        Some(val) => val,
        None => return Type::Error(ast::ParseError::UnexpectedEOF),
    };

    match token {
        (_, Token::LeftBrace) => {
            let sign = parse_signature(tokens);
            expect_tokens!(tokens, self::Type, "Expected ]",
                    (Token::RightBrace => {}),);
            Type::List(Box::new(sign))
        }
        (_, Token::Name(name)) => match name.as_ref() {
            "int" => Type::Int,
            "float" => Type::Float,
            "uint" => Type::Uint,
            "bool" => Type::Bool,
            "string" => Type::String,
            "v1" => Type::V1,
            "v2" => Type::V2,
            "v4" => Type::V4,
            "v8" => Type::V8,
            "v16" => Type::V16,
            "v32" => Type::V32,
            "v64" => Type::V64,
            other => Type::Declaration(other.to_string()),
        },
        other => Type::Error(ast::ParseError::UnexpectedToken(
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
        (_, Token::Let) 
        | (_, Token::Type) => {
            tokens.push(token);
            return ast::Statement::Assign(Box::new(parse_assignment(tokens)))
        },
        _ => {}
    }

    let token2 = match tokens.pop() {
        Some(val) => val,
        None => return ast::Statement::Error(ast::ParseError::UnexpectedEOF),
    };

    match token2 {
        (_, Token::Assign) => {
            tokens.push(token2);
            tokens.push(token);
            return ast::Statement::Assign(Box::new(parse_assignment(tokens)))
        },
        _ => {
            tokens.push(token2);
            tokens.push(token);
        }
    }

    let expr = parse_expression(tokens);

    let token = match tokens.pop() {
        Some(val) => val,
        None => return ast::Statement::Error(ast::ParseError::UnexpectedEOF),
    };

    match token {
        (_, Token::Semicolon) => ast::Statement::Call(Box::new(expr)),
        (_,_) => {
            tokens.push(token);
            ast::Statement::Return(Box::new(expr))
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
                        vec![node, $next_parse($tokens)]
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
    parse_binary_expr!(tokens, parse_and_expr, ("||" => "(||)"),)
}

fn parse_and_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_bor_expr, ("&&" => "(&&)"),)
}

fn parse_bor_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_xor_expr, ("|" => "(|)"),)
}

fn parse_xor_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_band_expr, ("^" => "(^)"),)
}

fn parse_band_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_cmp_expr, ("&" => "(&)"),)
}

fn parse_cmp_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_bitshift_expr, ("==" => "(==)"),
    ("!=" => "(!=)"),
    ("<" => "(<)"),
    (">" => "(>)"),
    ("<=" => "(<=)"),
    (">=" => "(>=)"),)
}

fn parse_bitshift_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_add_expr, ("<<" => "(<<)"),
    (">>" => "(>>)"),)
}

fn parse_add_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_mul_expr, ("+" => "(+)"),
    ("-" => "(-)"),)
}

fn parse_mul_expr(tokens: &mut Tokens) -> ast::Expression {
    parse_binary_expr!(tokens, parse_unary_expr, ("*" => "(*)"),
    ("/" => "(/)"),)
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
                    name: "(-)".to_string(),
                    ty: Type::Undefined,
                },
                vec![parse_primary_expr(tokens)]
            ),
            "~" => ast::Expression::Function(
                Symbol {
                    name: "(~)".to_string(),
                    ty: Type::Undefined,
                },
                vec![parse_primary_expr(tokens)]
            ),
            "!" => ast::Expression::Function(
                Symbol {
                    name: "(!)".to_string(),
                    ty: Type::Undefined,
                },
                vec![parse_primary_expr(tokens)]
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
                                    name: "(++)".to_string(),
                                    ty: Type::Undefined,
                                },
                                vec![node]
                            )
                        }
                        "--" => {
                            node = ast::Expression::Function(
                                Symbol {
                                    name: "(--)".to_string(),
                                    ty: Type::Undefined,
                                },
                                vec![node]
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

    let mut expr_chain = Vec::new();

    let expr = match token {
        (_, Token::Name(name)) => ast::Expression::Symbol(
            Symbol {
                name: name.clone(),
                ty: Type::Undefined,
            }
        ),
        (_, Token::Float(x)) => ast::Expression::Literal(
            Box::new(ast::Literal::Float(x))
        ),
        (_, Token::Integer(n)) => ast::Expression::Literal(
            Box::new(ast::Literal::Integer(n))
        ),
        (_, Token::Truthy(b)) => ast::Expression::Literal(
            Box::new(ast::Literal::Bool(b))
        ),
        (_, Token::String(s)) => ast::Expression::Literal(
            Box::new(ast::Literal::String(s.clone()))
        ),
        (pos, token) => ast::Expression::Error(ast::ParseError::UnexpectedToken(
            (pos, token),
            "Expected function, name, or literal binding".to_string(),
        ))
    };

    expr_chain.push(expr);

    loop {
        let token = match tokens.pop() {
            Some(val) => val,
            None => return ast::Expression::Error(ast::ParseError::UnexpectedEOF),
        };

        match token {
            (_, Token::Method(_)) => {
                tokens.push(token);
                let expr = parse_function_call(tokens);
                expr_chain.push(expr);
            },
            (_, Token::Member(name)) => {
                let expr = ast::Expression::Field(
                    Symbol {
                        name: name.clone(),
                    ty: Type::Undefined,
                    }
                );
                expr_chain.push(expr);
            },
            _ => {
                tokens.push(token);
                break
            }
        }

    }

    expr_chain.reverse();

    let mut chain_end = expr_chain.pop().unwrap();

    while let Some(expr) = expr_chain.pop() {
        chain_end = match (expr, chain_end) {
            (ast::Expression::Literal(left_terminal),
                ast::Expression::Field(right_symbol)) => {
                    ast::Expression::Function(
                        Symbol {name: "get".to_string(), ty: Type::Undefined},
                        vec![
                            ast::Expression::Literal(left_terminal),
                            ast::Expression::Field(right_symbol)
                        ]
                    )
                },
            (ast::Expression::Literal(left_terminal),
                ast::Expression::Function(right_symbol, right_args)) => {
                    let mut args = right_args;
                    args.insert(0, ast::Expression::Literal(left_terminal));
                    ast::Expression::Function(
                        right_symbol,
                        args
                    )
                },
            (ast::Expression::Field(left_symbol),
                ast::Expression::Function(right_symbol, right_args)) => {
                    let mut args = right_args;
                    args.insert(0, ast::Expression::Field(left_symbol));
                    ast::Expression::Function(
                        right_symbol,
                        args
                    )
                },
            (ast::Expression::Field(left_symbol),
                ast::Expression::Field(right_symbol)) => {
                    ast::Expression::Function(
                        Symbol {name: "get".to_string(), ty: Type::Undefined},
                        vec![
                            ast::Expression::Field(left_symbol),
                            ast::Expression::Field(right_symbol)
                        ]
                    )
                },
            (ast::Expression::Function(left_symbol,
                left_args), ast::Expression::Field(right_symbol)) => {
                    ast::Expression::Function(
                        Symbol {name: "get".to_string(), ty: Type::Undefined},
                        vec![
                            ast::Expression::Function(left_symbol, left_args),
                            ast::Expression::Field(right_symbol)
                        ]
                    )
                },
            (ast::Expression::Function(left_symbol,
                left_args), ast::Expression::Function(right_symbol, right_args)) => {
                    let mut args = right_args;
                    args.insert(0, ast::Expression::Function(left_symbol, left_args));
                    ast::Expression::Function(
                        right_symbol,
                        args
                    )
                }
            (_, right) => {
                match right {
                    ast::Expression::Literal(_) => ast::Expression::Error(ast::ParseError::Other("Unexpected literal".to_string())),
                    ast::Expression::Function(_, _) => ast::Expression::Error(ast::ParseError::Other("Unexpected function".to_string())),
                    ast::Expression::Field(_) => ast::Expression::Error(ast::ParseError::Other("Unexpected field".to_string())),
                    ast::Expression::Error(_) => ast::Expression::Error(ast::ParseError::Other("Unexpected error".to_string()))
                }
            }
        }
    }

    chain_end
}

fn parse_function_call(tokens: &mut Tokens) -> ast::Expression {
    let symbol = match tokens.pop() {
        Some(val) => match val {
            (_, Token::Function(name)) 
                | (_, Token::Method(name)) => Symbol {
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
            (_, Token::RightPar) => return ast::Expression::Function(symbol, vec![]),
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

    ast::Expression::Function(symbol, args)
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
            ast::Expression::Literal(Box::new(ast::terminal::Literal::Integer(1)))
        )
    }

    #[test]
    fn float_binding() {
        let mut tokens = generate_test_tokens("float", "1.0");
        let ast = parse_binding(&mut tokens);
        assert!(tokens.is_empty());

        assert_eq!(
            ast,
            ast::Expression::Literal(Box::new(ast::terminal::Literal::Float(1.0)))
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
                Box::new(ast::terminal::Literal::String("string".to_string()))
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
                ast::Expression::Literal(Box::new(ast::terminal::Literal::Bool(true)))
            )
        }
        {
            let mut tokens = generate_test_tokens("bool_false", "false");
            let ast = parse_binding(&mut tokens);
            assert!(tokens.is_empty());

            assert_eq!(
                ast,
                ast::Expression::Literal(Box::new(ast::terminal::Literal::Bool(false)))
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
                }
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
                vec![]
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
            ast::Expression::Function(
                Symbol {
                    name: "get".to_string(),
                    ty: Type::Undefined
                },
                vec![
                    ast::Expression::Field(
                        Symbol {
                            name: "foo".to_string(),
                            ty: Type::Undefined
                        }
                    ),
                    ast::Expression::Field(
                        Symbol {
                            name: "bar".to_string(),
                            ty: Type::Undefined
                        }
                    )
                ]
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
            ast::Expression::Function(
                Symbol {
                    name: "bar".to_string(),
                    ty: Type::Undefined
                },
                vec![
                    ast::Expression::Field(
                        Symbol {
                            name: "foo".to_string(),
                            ty: Type::Undefined
                        }
                    )
                ]
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
                    name: "bar".to_string(),
                    ty: Type::Undefined
                },
                vec![
                    ast::Expression::Function(
                    Symbol {
                        name: "bar".to_string(),
                        ty: Type::Undefined
                    },
                    vec![],
                    )
                ]
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
                    name: "get".to_string(),
                    ty: Type::Undefined
                },
                vec![
                    ast::Expression::Function(
                        Symbol {
                            name: "foo".to_string(),
                            ty: Type::Undefined
                        },
                        vec![]
                    ),
                    ast::Expression::Field(
                        Symbol {
                            name: "bar".to_string(),
                            ty: Type::Undefined
                        }
                    )
                ]
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
                    name: "(+)".to_string(),
                    ty: Type::Undefined
                },
                vec![
                    ast::Expression::Literal(Box::new(ast::terminal::Literal::Integer(1))),
                    ast::Expression::Literal(Box::new(ast::terminal::Literal::Integer(2)))
                ]
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
                    name: "(&&)".to_string(),
                    ty: Type::Undefined
                },
                vec![
                    ast::Expression::Field(
                        Symbol {
                            name: "foo".to_string(),
                            ty: Type::Undefined
                        }
                    ),
                    ast::Expression::Function(
                        Symbol {
                            name: "bar".to_string(),
                            ty: Type::Undefined
                        },
                        vec![]
                    )
                ]
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
                    name: "(++)".to_string(),
                    ty: Type::Undefined
                },
                vec![ast::Expression::Field(
                    Symbol {
                        name: "(--)".to_string(),
                        ty: Type::Undefined
                    }
                )]
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
                        }
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
                        }
                    ),
                    ast::Expression::Field(
                        Symbol {
                            name: "baz".to_string(),
                            ty: Type::Undefined
                        }
                    )
                ]
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
                    ast::Expression::Literal(Box::new(ast::terminal::Literal::Integer(1),))
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
                    ast::Expression::Literal(Box::new(ast::terminal::Literal::Integer(1)))
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
        let ast = parse_closure(&mut tokens);
        assert!(tokens.is_empty());

        let stmt1 = ast::Statement::Assign(Box::new(ast::Assignment::Field(
            ast::terminal::Constness::Mut,
            Symbol {
                name: "foo".to_string(),
                ty: Type::Float,
            },
            ast::Scope::Open(Box::new(ast::Statement::Call(Box::new(
                ast::Expression::Literal(Box::new(ast::terminal::Literal::Float(1.0))),
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
                }
            )]
        )));

        let stmt3 = ast::Statement::Call(Box::new(ast::Expression::Field(
            Symbol {
                name: "foo".to_string(),
                ty: Type::Undefined,
            }
        )));

        assert_eq!(ast, ast::Scope::Closed(vec![stmt1, stmt2, stmt3]));
    }

}
