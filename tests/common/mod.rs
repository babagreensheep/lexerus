pub use lexerus::TestBuild;
pub use lexerus::*;

#[derive(Lexer, Token, Debug)]
pub struct Char<'code>(Buffer<'code>);

#[derive(Lexer, Token, Debug)]
pub struct Quote<'code>(#[pattern = "\""] Buffer<'code>);

#[derive(Lexer, Token, Debug)]
pub struct Escape<'code>(#[pattern = "\\"] Buffer<'code>);

#[derive(Lexer, Token, Debug)]
pub enum StringChars<'code> {
    Escaped(Escape<'code>, Quote<'code>),
    Char(Char<'code>),
}
