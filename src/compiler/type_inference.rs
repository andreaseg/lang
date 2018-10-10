pub fn resolve_types(ast: &mut ast::S) {
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
            ast::S {
                code: vec![ast::Top::Assign(Box::new(ast::Assignment::Field(
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
            },
        );
    }
}