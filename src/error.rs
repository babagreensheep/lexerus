use std::fmt::Debug;

use super::*;

#[derive(Debug)]
pub struct Error<'code> {
    pub buffer: Buffer<'code>,
    pub matched: usize,
    pub kind: Kind,
}

#[derive(Debug)]
pub enum Kind {
    NotFound(&'static str),
    UnexpectedToken(&'static str),
}
