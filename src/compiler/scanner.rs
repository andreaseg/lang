#![allow(dead_code)]
use regex::Regex;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(PartialEq, Debug, Clone)]
pub struct TokenPosition {
    line: usize,
    symbol: usize,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Float(f64),
    Integer(i64),
    LeftPar,
    RightPar,
    LeftCurl,
    RightCurl,
    Function(String),
    Module(String),
    Method(String),
    Member(String),
    Name(String),
    Comma,
    Operator(String),
    Assign,
    String(String),
    RightArrow,
    FatRightArrow,
    Truthy(bool),
    If,
    Else,
    Match,
    Const,
    Mut,
    Wildcard,
    Error(String),
    Struct,
    Enum,
    End
}

pub fn tokenize(file: File) -> Vec<(TokenPosition, Token)> {
    let buf_reader = BufReader::new(file);

    let mut tokens = Vec::new();

    let re = Regex::new(concat!(
        r"(?P<float>\d*\.\d+)|",
        r"(?P<integer>\d+)|",
        r"(?P<end>;)|",
        r"(?P<lpar>\()|",
        r"(?P<rpar>\))|",
        r"(?P<lcurl>\{)|",
        r"(?P<rcurl>\})|",
        r"(?P<true>true)|",
        r"(?P<false>false)|",
        r"(?P<if>if)|",
        r"(?P<else>else)|",
        r"(?P<match>match)|",
        r"(?P<const>const)|",
        r"(?P<mut>mut)|",
        r"(?P<wildcard>_)|",
        r"(?P<struct>struct)|",
        r"(?P<enum>enum)|",
        r"(?P<method>\.[[:alpha:]][[:alnum:]]*\()|",
        r"(?P<member>\.[[:alpha:]][[:alnum:]]*)|",
        r"(?P<function>[[:alpha:]][[:alnum:]]*\()|",
        r"(?P<module>[[:alpha:]][[:alnum:]]*::)|",
        r"(?P<name>[[:alpha:]][[:alnum:]]*)|",
        r"(?P<comma>,)|",
        r"(?P<rightarrow>->)|",
        r"(?P<fatrightarrow>=>)|",
        r"(?P<operator><<|>>|&&|\|\||\+\+|\-\-|\+|\-|\*|/|\^|\||&|<=|>=|<|>|!=|==|%|:|~|\?)|",
        r"(?P<assign>=)|",
        r#"(?P<string>"[^"]*")|"#,
        r"(?P<whitespace>\s*|\t*|\n*|\r*)"
    )).unwrap();

    for (num, line) in buf_reader.lines().enumerate() {
        if line.as_ref().unwrap().starts_with("//") {
            continue;
        }

        let mut last_symbol = 0;

        for cap in re.captures_iter(line.as_ref().unwrap()) {
            macro_rules! safe_unwrap {
                ($name:expr) => {{
                    let m = cap.name($name).unwrap();
                    if last_symbol != m.start() {
                        tokens.push((
                            TokenPosition {
                                line: num,
                                symbol: last_symbol,
                            },
                            Token::Error(line.as_ref().unwrap()[last_symbol..m.start()].to_string()),
                        ))
                    }
                    last_symbol = m.end();
                    m
                }};
            }

            macro_rules! ret_tok {
                ($cap:ident, $token:expr) => {
                    (
                        TokenPosition {
                            line: num,
                            symbol: $cap.start(),
                        },
                        $token,
                    )
                };
            }

            let token = if cap.name("float").is_some() {
                let m = safe_unwrap!("float");
                ret_tok!(m, Token::Float(m.as_str().parse().unwrap()))
            } else if cap.name("integer").is_some() {
                let m = safe_unwrap!("integer");
                ret_tok!(m, Token::Integer(m.as_str().parse().unwrap()))
            } else if cap.name("end").is_some() {
                let m = safe_unwrap!("end");
                ret_tok!(m, Token::End)
            } else if cap.name("lpar").is_some() {
                let m = safe_unwrap!("lpar");
                ret_tok!(m, Token::LeftPar)
            } else if cap.name("rpar").is_some() {
                let m = safe_unwrap!("rpar");
                ret_tok!(m, Token::RightPar)
            } else if cap.name("lcurl").is_some() {
                let m = safe_unwrap!("lcurl");
                ret_tok!(m, Token::LeftCurl)
            } else if cap.name("rcurl").is_some() {
                let m = safe_unwrap!("rcurl");
                ret_tok!(m, Token::RightCurl)
            } else if cap.name("true").is_some() {
                let m = safe_unwrap!("true");
                ret_tok!(m, Token::Truthy(true))
            } else if cap.name("false").is_some() {
                let m = safe_unwrap!("false");
                ret_tok!(m, Token::Truthy(false))
            } else if cap.name("if").is_some() {
                let m = safe_unwrap!("if");
                ret_tok!(m, Token::If)
            } else if cap.name("else").is_some() {
                let m = safe_unwrap!("else");
                ret_tok!(m, Token::Else)
            } else if cap.name("match").is_some() {
                let m = safe_unwrap!("match");
                ret_tok!(m, Token::Match)
            } else if cap.name("const").is_some() {
                let m = safe_unwrap!("const");
                ret_tok!(m, Token::Const)
            } else if cap.name("mut").is_some() {
                let m = safe_unwrap!("mut");
                ret_tok!(m, Token::Mut)
            } else if cap.name("wildcard").is_some() {
                let m = safe_unwrap!("wildcard");
                ret_tok!(m, Token::Wildcard)
            } else if cap.name("struct").is_some() {
                let m = safe_unwrap!("struct");
                ret_tok!(m, Token::Struct)
            } else if cap.name("enum").is_some() {
                let m = safe_unwrap!("enum");
                ret_tok!(m, Token::Enum)
            } else if cap.name("function").is_some() {
                let m = safe_unwrap!("function");
                let mut s = m.as_str().to_string();
                let l = s.len();
                s.truncate(l - 1);
                ret_tok!(m, Token::Function(s))
            } else if cap.name("module").is_some() {
                let m = safe_unwrap!("module");
                let mut s = m.as_str().to_string();
                let l = s.len();
                s.truncate(l - 2);
                ret_tok!(m, Token::Module(s))
            } else if cap.name("method").is_some() {
                let m = safe_unwrap!("method");
                let mut s = m.as_str().to_string();
                s.drain(..1);
                let l = s.len();
                s.truncate(l - 1);
                ret_tok!(m, Token::Method(s))
            } else if cap.name("member").is_some() {
                let m = safe_unwrap!("member");
                let mut s = m.as_str().to_string();
                s.drain(..1);
                ret_tok!(m, Token::Member(s))
            } else if cap.name("operator").is_some() {
                let m = safe_unwrap!("operator");
                ret_tok!(m, Token::Operator(m.as_str().to_string()))
            } else if cap.name("name").is_some() {
                let m = safe_unwrap!("name");
                ret_tok!(m, Token::Name(m.as_str().to_string()))
            } else if cap.name("comma").is_some() {
                let m = safe_unwrap!("comma");
                ret_tok!(m, Token::Comma)
            } else if cap.name("rightarrow").is_some() {
                let m = safe_unwrap!("rightarrow");
                ret_tok!(m, Token::RightArrow)
            } else if cap.name("fatrightarrow").is_some() {
                let m = safe_unwrap!("fatrightarrow");
                ret_tok!(m, Token::FatRightArrow)
            } else if cap.name("assign").is_some() {
                let m = safe_unwrap!("assign");
                ret_tok!(m, Token::Assign)
            } else if cap.name("whitespace").is_some() {
                safe_unwrap!("whitespace");
                continue;
            } else if cap.name("string").is_some() {
                let m = safe_unwrap!("string");
                ret_tok!(m, Token::String(m.as_str().to_string()))
            } else {
                panic!("Unable to parse expression");
            };

            tokens.push(token);
        }
    }

    tokens
}

pub type ScanError = (TokenPosition, String);

pub fn get_errors(tokens: &[(TokenPosition, Token)]) -> Vec<ScanError> {
    let mut errors = Vec::new();

    for tok in tokens {
        if let (pos, Token::Error(s)) = tok {
            errors.push((pos.clone(), s.clone()))
        }
    }

    errors
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::io::Write;
    use tempdir::TempDir;

    #[test]
    fn single_token() {
        let dir = TempDir::new("scanner_test").unwrap();

        macro_rules! test_token {
            ($name:expr, $str_token:expr, $result_token:expr) => {
                let file_path = dir.path().join(format!("{}.txt", $name));
                println!("Test single token \"{}\"", $name);
                {
                    let mut file =
                        File::create(file_path.clone()).expect("Unable to create file");
                    file.write($str_token.as_bytes())
                        .expect("Unable to write to file");
                }
                let file = File::open(file_path).expect("Unable to open file");

                let tokens = tokenize(file).pop().expect("Missing token in file");
                assert_eq!(
                    tokens,
                    (TokenPosition { line: 0, symbol: 0 }, $result_token)
                );
            };
        }

        test_token!("float", "1.0", Token::Float(1.0));
        test_token!("integer", "1", Token::Integer(1));
        test_token!("left_par", "(", Token::LeftPar);
        test_token!("right_par", ")", Token::RightPar);
        test_token!("left_curl", "{", Token::LeftCurl);
        test_token!("right_curl", "}", Token::RightCurl);
        test_token!("function", "fun(", Token::Function("fun".to_string()));
        test_token!("module", "mod::", Token::Module("mod".to_string()));
        test_token!("method", ".map(", Token::Method("map".to_string()));
        test_token!("member", ".val", Token::Member("val".to_string()));
        test_token!("name", "val", Token::Name("val".to_string()));
        test_token!("assign", "=", Token::Assign);
        test_token!(
            "string",
            "\"$string!\"",
            Token::String("\"$string!\"".to_string())
        );
        test_token!("right_arrow", "->", Token::RightArrow);
        test_token!("true", "true", Token::Truthy(true));
        test_token!("false", "false", Token::Truthy(false));
        test_token!("if", "if", Token::If);
        test_token!("else", "else", Token::Else);
        test_token!("match", "match", Token::Match);
        test_token!("const", "const", Token::Const);
        test_token!("mut", "mut", Token::Mut);
        test_token!("wildcard", "_", Token::Wildcard);
        test_token!("struct", "struct", Token::Struct);
        test_token!("enum", "enum", Token::Enum);

        test_token!("shiftleft", "<<", Token::Operator("<<".to_string()));
        test_token!("shiftright", ">>", Token::Operator(">>".to_string()));
        test_token!("and", "&&", Token::Operator("&&".to_string()));
        test_token!("or", "||", Token::Operator("||".to_string()));
        test_token!("incr", "++", Token::Operator("++".to_string()));
        test_token!("decr", "--", Token::Operator("--".to_string()));
        test_token!("add", "+", Token::Operator("+".to_string()));
        test_token!("sub", "-", Token::Operator("-".to_string()));
        test_token!("mul", "*", Token::Operator("*".to_string()));
        test_token!("div", "/", Token::Operator("/".to_string()));
        test_token!("bxor", "^", Token::Operator("^".to_string()));
        test_token!("bor", "|", Token::Operator("|".to_string()));
        test_token!("band", "&", Token::Operator("&".to_string()));
        test_token!("geq", "<=", Token::Operator("<=".to_string()));
        test_token!("leq", ">=", Token::Operator(">=".to_string()));
        test_token!("less", "<", Token::Operator("<".to_string()));
        test_token!("greater", ">", Token::Operator(">".to_string()));
        test_token!("neq", "!=", Token::Operator("!=".to_string()));
        test_token!("eq", "==", Token::Operator("==".to_string()));
        test_token!("mod", "%", Token::Operator("%".to_string()));
        test_token!("colon", ":", Token::Operator(":".to_string()));
        test_token!("not", "~", Token::Operator("~".to_string()));
        test_token!("question", "?", Token::Operator("?".to_string()));
    }

    macro_rules! test_scanner {
        ($name: expr, $str_token: expr, $(($line: expr, $sym: expr, $res: expr),)*) => {
            let dir = TempDir::new("scanner_test").unwrap();
            let file_path = dir.path().join(format!("{}.txt", $name));
            println!("Test single token \"{}\"", $name);
            {
                let mut file = File::create(file_path.clone()).expect("Unable to create file");
                file.write($str_token.as_bytes()).expect("Unable to write to file");
            }
            let file = File::open(file_path).expect("Unable to open file");

            let left_tokens = tokenize(file);
            let right_tokens = vec!($((TokenPosition{line: $line, symbol: $sym}, $res) ,)*);
            println!("{:?}", left_tokens);
            assert_eq!(left_tokens.len(),right_tokens.len());
            for i in 0..left_tokens.len() {
                assert_eq!(left_tokens[i], right_tokens[i]);
            }
        }
    }

    #[test]
    fn assignment() {
        test_scanner!(
            "assignment",
            "int x = 1",
            (0, 0, Token::Name("int".to_string())),
            (0, 4, Token::Name("x".to_string())),
            (0, 6, Token::Assign),
            (0, 8, Token::Integer(1)),
        );
    }

    #[test]
    fn function() {
        test_scanner!(
            "call",
            "fn fun(int x) = x + 1",
            (0, 0, Token::Name("fn".to_string())),
            (0, 3, Token::Function("fun".to_string())),
            (0, 7, Token::Name("int".to_string())),
            (0, 11, Token::Name("x".to_string())),
            (0, 12, Token::RightPar),
            (0, 14, Token::Assign),
            (0, 16, Token::Name("x".to_string())),
            (0, 18, Token::Operator("+".to_string())),
            (0, 20, Token::Integer(1)),
        );
    }

    #[test]
    fn multiline() {
        test_scanner!(
            "multiline",
            "Foo bar = Foo::new()\nbar.run()",
            (0, 0, Token::Name("Foo".to_string())),
            (0, 4, Token::Name("bar".to_string())),
            (0, 8, Token::Assign),
            (0, 10, Token::Module("Foo".to_string())),
            (0, 15, Token::Function("new".to_string())),
            (0, 19, Token::RightPar),
            (1, 0, Token::Name("bar".to_string())),
            (1, 3, Token::Method("run".to_string())),
            (1, 8, Token::RightPar),
        );
    }

    #[test]
    fn comment() {
        test_scanner!(
            "comment",
            "// Comment\nx",
            (1, 0, Token::Name("x".to_string())),
        );
    }

    #[test]
    fn error() {
        test_scanner!(
            "error",
            "foo¤bar",
            (0, 0, Token::Name("foo".to_string())),
            (0, 3, Token::Error("¤".to_string())),
            (0, 5, Token::Name("bar".to_string())),
        );
    }

    macro_rules! test_scanner_error {
        ($name: expr, $str_token: expr, $(($line: expr, $sym: expr, $error: expr),)*) => {
            let dir = TempDir::new("scanner_test").unwrap();
            let file_path = dir.path().join(format!("{}.txt", $name));
            println!("Test single token \"{}\"", $name);
            {
                let mut file = File::create(file_path.clone()).expect("Unable to create file");
                file.write($str_token.as_bytes()).expect("Unable to write to file");
            }
            let file = File::open(file_path).expect("Unable to open file");

            let tokens = tokenize(file);
            let left_errors = get_errors(&tokens);
            let right_errors = vec!($((TokenPosition{line: $line, symbol: $sym}, $error ),)*);
            println!("{:?}", tokens);
            assert_eq!(left_errors, right_errors);
        }
    }

    #[test]
    fn extract_error() {
        test_scanner_error!("scanner_error", "foo¤bar", (0, 3, "¤".to_string()),);
    }
}
