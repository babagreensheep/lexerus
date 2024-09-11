use super::*;

pub trait Token<'code>
where
    Self: Sized,
{
    const NAME: &'static str;
    fn buffer(&self) -> Option<Buffer<'code>>;
}
