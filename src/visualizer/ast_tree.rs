#![allow(non_upper_case_globals)]
use compiler::parser::ast::*;

// https://codepen.io/philippkuehn/pen/QbrOaN

static UL: &str = "<ul>";
static EndUL: &str = "</ul>";
static EndLI: &str = "</li>";
static A: &str = "<a href=\"#\">";
static EndA: &str = "</a>";
static DIV: &str = "<div class=\"tree\">";
static EndDIV: &str = "</div>";

#[allow(non_snake_case)]
fn LI(class: &str) -> String {
    ["<li class=\"", class, ">"].concat()
}

#[allow(dead_code)]
pub fn generate_html(ast: &S) -> String {
    [DIV, ast.to_html().as_ref(), EndDIV].concat()
}

trait ToHtml {
    fn to_html(&self) -> String;
}

impl ToHtml for ParseError {
    fn to_html(&self) -> String {
        match &self {
            ParseError::UnexpectedToken((pos, token), message) => [
                LI("error").as_ref(),
                A,
                pos.to_string().as_str(),
                token.to_string().as_str(),
                message.as_str(),
                EndA,
                EndLI,
            ]
                .concat(),
            ParseError::UnexpectedEOF => {
                [LI("error").as_ref(), A, "Error: EOF", EndA, EndLI].concat()
            },
            ParseError::Other(message) => {
                [LI("error").as_ref(), A, message, EndA, EndLI].concat()
            }
        }
    }
}

impl ToHtml for S {
    fn to_html(&self) -> String {
        let members = &self
            .code
            .iter()
            .map(|t| t.to_html())
            .collect::<Vec<String>>()
            .concat();

        [UL, members, EndUL].concat()
    }
}

impl ToHtml for Top {
    fn to_html(&self) -> String {
        match &self {
            Top::Assign(a) => a.to_html(),
            Top::Error(e) => e.to_html(),
        }
    }
}

impl ToHtml for Assignment {
    fn to_html(&self) -> String {
        match &self {
            Assignment::Function(c, symbol, args, scope) => {
                let a = args
                    .iter()
                    .fold("".to_owned(), |acc, t| acc + t.to_string().as_str() + ", ");
                [
                    LI("assign").as_ref(),
                    A,
                    c.to_html().as_ref(),
                    symbol.to_string().as_str(),
                    "(",
                    a.as_str(),
                    ")",
                    EndA,
                    UL,
                    scope.to_html().as_ref(),
                    EndUL,
                    EndLI,
                ]
                    .concat()
            }
            Assignment::Field(c, symbol, scope) => [
                LI("assign").as_ref(),
                A,
                c.to_html().as_ref(),
                symbol.to_string().as_str(),
                EndA,
                UL,
                scope.to_html().as_ref(),
                EndUL,
                EndLI,
            ]
                .concat(),
            Assignment::Reassign(symbol, scope) => [
                LI("assign").as_ref(),
                A,
                symbol.to_string().as_str(),
                EndA,
                UL,
                scope.to_html().as_ref(),
                EndUL,
                EndLI,
            ]
                .concat(),
            Assignment::Struct(symbol, members) => {
                let m = members
                    .iter()
                    .map(|m| m.to_string())
                    .collect::<Vec<String>>()
                    .join([EndLI, LI("member").as_ref()].concat().as_str());

                [
                    LI("assign").as_ref(),
                    A,
                    symbol.to_string().as_str(),
                    EndA,
                    UL,
                    LI("member").as_ref(),
                    m.as_str(),
                    EndLI,
                    EndUL,
                    EndLI,
                ]
                    .concat()
            }
            Assignment::Enum(symbol, members) => {
                let m = members
                    .iter()
                    .map(|m| m.to_string())
                    .collect::<Vec<String>>()
                    .join([EndLI, LI("member").as_ref()].concat().as_str());

                [
                    LI("assign").as_ref(),
                    A,
                    symbol.to_string().as_str(),
                    EndA,
                    UL,
                    LI("member").as_ref(),
                    m.as_str(),
                    EndLI,
                    EndUL,
                    EndLI,
                ]
                    .concat()
            }
            Assignment::Error(e) => e.to_html(),
        }
    }
}

impl ToHtml for terminal::Constness {
    fn to_html(&self) -> String {
        use self::terminal::Constness as C;
        match &self {
            C::Const => "const ",
            C::Mut => "mut ",
            C::None => "",
        }.to_string()
    }
}

impl ToHtml for Scope {
    fn to_html(&self) -> String {
        match &self {
            Scope::Closed(v) => {
                let s = v
                    .iter()
                    .fold("".to_owned(), |acc, s| acc + s.to_html().as_ref());
                [LI("scope").as_ref(), UL, s.as_str(), EndUL, EndLI].concat()
            }
            Scope::Open(s) => (*s).to_html(),
            Scope::Error(e) => e.to_html(),
        }
    }
}

impl ToHtml for Statement {
    fn to_html(&self) -> String {
        match &self {
            Statement::Assign(a) => (*a).to_html(),
            Statement::Call(c) => (*c).to_html(),
            Statement::Error(e) => e.to_html(),
        }
    }
}

impl ToHtml for Expression {
    fn to_html(&self) -> String {
        match &self {
            Expression::Literal(lit) => [
                LI("expression").as_ref(),
                A,
                lit.to_html().as_ref(),
                EndA,
                EndLI,
            ]
                .concat(),
            Expression::Field(sym) => [
                LI("expression").as_ref(),
                A,
                sym.to_string().as_ref(),
                EndA,
                EndLI,
            ]
                .concat(),
            Expression::Function(sym, _args) => [
                LI("expression").as_ref(),
                A,
                sym.to_string().as_ref(),
                EndA,
                EndLI,
            ]
                .concat(),
            Expression::Error(e) => e.to_html(),
        }
    }
}

impl ToHtml for terminal::Literal {
    fn to_html(&self) -> String {
        match &self {
            terminal::Literal::Float(n) => [
                LI("terminal").as_ref(),
                A,
                n.to_string().as_str(),
                EndA,
                EndLI,
            ]
                .concat(),
            terminal::Literal::Integer(n) => [
                LI("terminal").as_ref(),
                A,
                n.to_string().as_str(),
                EndA,
                EndLI,
            ]
                .concat(),
            terminal::Literal::String(s) => {
                [LI("terminal").as_ref(), A, s.as_ref(), EndA, EndLI].concat()
            }
            terminal::Literal::Bool(b) => [
                LI("terminal").as_ref(),
                A,
                b.to_string().as_str(),
                EndA,
                EndLI,
            ]
                .concat(),
        }
    }
}
