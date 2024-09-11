#![allow(dead_code)]
//! Mark EOF
use crate::*;

#[derive(Debug, PartialEq, Clone)]
pub struct EoF;

impl<'code> Lexer<'code> for EoF
where
    Self: Token<'code>,
{
    fn lex(
        buffer: &mut Buffer<'code>,
    ) -> Result<Self, Error<'code>> {
        if buffer.search_len() == 0 {
            Ok(Self)
        }
        else {
            Err(Error {
                buffer: buffer.clone(),
                matched: 0,
                kind: Kind::NotFound(<Self as Token>::NAME),
            })
        }
    }
}

impl<'code> Token<'code> for EoF {
    const NAME: &'static str = "EoF";

    fn buffer(&self) -> Option<Buffer<'code>> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eof() {
        let mut buffer = Buffer::from("hello");
        let search = buffer.eat_word("hello").unwrap();
        println!("Search: {search}");
        let eof = EoF::lex(&mut buffer).unwrap();
        assert_eq!(eof, EoF);
    }

    #[test]
    fn eof_complex() {
        let buffer0 = Buffer::from("hello");
        let buffer1 = Buffer::from("world");
        let mut buffer = buffer0 + buffer1;
        let search = buffer.eat_word("helloworld").unwrap();
        println!("Search: {search}");
        let eof = EoF::lex(&mut buffer).unwrap();
        assert_eq!(eof, EoF);
    }
}
