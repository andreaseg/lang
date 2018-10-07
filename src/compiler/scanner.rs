#![allow(dead_code)]
#![allow(clippy::all)] // Clippy crashes on #[derive(FromPrimitive)]: Issue https://github.com/rust-lang-nursery/rust-clippy/issues/2910
use num::FromPrimitive;
use regex::{Match, Regex};
use std::fmt;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(PartialEq, Debug, Clone)]
pub struct TokenPosition {
    line: usize,
    symbol: usize,
}

impl fmt::Display for TokenPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "line {} and symbol {}", self.line, self.symbol)
    }
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
    End,
}

macro_rules! make_regex {
    ($(($name: ident, $regex: expr),)*) => {
        fn get_regex() -> Regex {
            Regex::new(concat!($($regex,)*)).unwrap()
        }

        #[derive(FromPrimitive)]
        enum Re {
            None,
            $($name, )*
        }
    }
}

make_regex!(
    (Float, r"(?P<float>\d*\.\d+)|"),
    (Integer, r"(?P<integer>\d+)|"),
    (End, r"(?P<end>;)|"),
    (LPar, r"(?P<lpar>\()|"),
    (RPar, r"(?P<rpar>\))|"),
    (LCurl, r"(?P<lcurl>\{)|"),
    (RCurl, r"(?P<rcurl>\})|"),
    (True, r"(?P<true>true)|"),
    (False, r"(?P<false>false)|"),
    (If, r"(?P<if>if)|"),
    (Else, r"(?P<else>else)|"),
    (Match, r"(?P<match>match)|"),
    (Const, r"(?P<const>const)|"),
    (Mut, r"(?P<mut>mut)|"),
    (Wildcard, r"(?P<wildcard>_)|"),
    (Struct, r"(?P<struct>struct)|"),
    (Enum, r"(?P<enum>enum)|"),
    (Method, r"(?P<method>\.[[:alpha:]][[:alnum:]]*\()|"),
    (Member, r"(?P<member>\.[[:alpha:]][[:alnum:]]*)|"),
    (Function, r"(?P<function>[[:alpha:]][[:alnum:]]*\()|"),
    (Module, r"(?P<module>[[:alpha:]][[:alnum:]]*::)|"),
    (Name, r"(?P<name>[[:alpha:]][[:alnum:]]*)|"),
    (Comma, r"(?P<comma>,)|"),
    (Rightarrow, r"(?P<rightarrow>->)|"),
    (FatRightArrow, r"(?P<fatrightarrow>=>)|"),
    (
        Operator,
        r"(?P<operator><<|>>|&&|\|\||\+\+|\-\-|\+|\-|\*|/|\^|\||&|<=|>=|<|>|!=|==|%|:|~|\?)|"
    ),
    (Assign, r"(?P<assign>=)|"),
    (String, r#"(?P<string>"[^"]*")|"#),
    (Whitespace, r"(?P<whitespace>\s*|\t*|\n*|\r*)"),
);

fn find_matches<'a>(re: &Regex, line: &'a str) -> Vec<(usize, Match<'a>)> {
    let names = re
        .capture_names()
        .enumerate()
        .skip(1)
        .map(|(i, name)| (i, name.unwrap()))
        .collect::<Vec<(usize, &str)>>();

    let mut matches: Vec<(usize, Match)> = Vec::new();

    for cap in re.captures_iter(line) {
        matches.extend(
            names
                .iter()
                .find(|(_, name)| cap.name(name).is_some())
                .map(|(i, name)| (*i, cap.name(name).unwrap())),
        );
    }

    matches
}

pub fn trunc_cap(cap: &Match, start: usize, end: usize) -> String {
    let mut s = cap.as_str().to_string();
    s.drain(..start);
    let l = s.len();
    s.truncate(l - end);
    s
}

pub fn tokenize(file: File) -> Vec<(TokenPosition, Token)> {
    let buf_reader = BufReader::new(file);

    let mut tokens = Vec::new();

    let re = get_regex();

    for (num, line) in buf_reader.lines().enumerate() {
        if line.as_ref().unwrap().starts_with("//") {
            continue;
        }

        let mut last_symbol = 0;

        for (i, cap) in find_matches(&re, line.as_ref().unwrap()) {
            macro_rules! ret_tok {
                ($token:expr) => {
                    (
                        TokenPosition {
                            line: num,
                            symbol: cap.start(),
                        },
                        $token,
                    )
                };
            }

            if last_symbol != cap.start() {
                tokens.push((
                    TokenPosition {
                        line: num,
                        symbol: last_symbol,
                    },
                    Token::Error(line.as_ref().unwrap()[last_symbol..cap.start()].to_string()),
                ))
            }
            last_symbol = cap.end();

            let token = match Re::from_usize(i).unwrap() {
                Re::Float => ret_tok!(Token::Float(cap.as_str().parse().unwrap())),
                Re::Integer => ret_tok!(Token::Integer(cap.as_str().parse().unwrap())),
                Re::End => ret_tok!(Token::End),
                Re::LPar => ret_tok!(Token::LeftPar),
                Re::RPar => ret_tok!(Token::RightPar),
                Re::LCurl => ret_tok!(Token::LeftCurl),
                Re::RCurl => ret_tok!(Token::RightCurl),
                Re::True => ret_tok!(Token::Truthy(true)),
                Re::False => ret_tok!(Token::Truthy(false)),
                Re::If => ret_tok!(Token::If),
                Re::Else => ret_tok!(Token::Else),
                Re::Match => ret_tok!(Token::Match),
                Re::Const => ret_tok!(Token::Const),
                Re::Mut => ret_tok!(Token::Mut),
                Re::Wildcard => ret_tok!(Token::Wildcard),
                Re::Struct => ret_tok!(Token::Struct),
                Re::Enum => ret_tok!(Token::Enum),
                Re::Method => ret_tok!(Token::Method(trunc_cap(&cap, 1, 1))),
                Re::Member => ret_tok!(Token::Member(trunc_cap(&cap, 1, 0))),
                Re::Function => ret_tok!(Token::Function(trunc_cap(&cap, 0, 1))),
                Re::Module => ret_tok!(Token::Module(trunc_cap(&cap, 0, 2))),
                Re::Name => ret_tok!(Token::Name(cap.as_str().to_string())),
                Re::Comma => ret_tok!(Token::Comma),
                Re::Rightarrow => ret_tok!(Token::RightArrow),
                Re::FatRightArrow => ret_tok!(Token::FatRightArrow),
                Re::Operator => ret_tok!(Token::Operator(cap.as_str().to_string())),
                Re::Assign => ret_tok!(Token::Assign),
                Re::String => ret_tok!(Token::String(cap.as_str().to_string())),
                Re::Whitespace => {
                    continue;
                }
                Re::None => unreachable!(),
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
