//! [Infix] Operators are used for creating an
//! infix operation.

// App imports
use crate::*;

#[derive(Clone)]
/// [Infix] operator used to create an infix
/// operation, e.g. `1+1`. This helper is useful
/// because it does not discard the `LHS` result
/// but rather returns it as a `None` value if the
/// `Operator` cannot be found.
pub enum Infix<LHS, Operator, RHS> {
    Found {
        lhs: LHS,
        operator: Operator,
        rhs: RHS,
    },
    None(LHS),
}

impl<LHS, Operator, RHS> ::std::fmt::Debug
    for Infix<LHS, Operator, RHS>
where
    LHS: ::std::fmt::Debug,
    RHS: ::std::fmt::Debug,
    Operator: ::std::fmt::Debug,
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Found { lhs, operator, rhs } => f
                .debug_map()
                .entry(&"lhs", lhs)
                .entry(&"operator", operator)
                .entry(&"rhs", rhs)
                .finish(),
            Self::None(lhs) => std::fmt::Debug::fmt(lhs, f),
        }
    }
}

impl<'code, LHS, Operator, RHS> Lexer<'code>
    for Infix<LHS, Operator, RHS>
where
    LHS: Lexer<'code>,
    Operator: Lexer<'code>,
    RHS: Lexer<'code>,
{
    fn lex(
        buffer: &mut Buffer<'code>,
    ) -> Result<Self, Error<'code>> {
        // Clone local buffer
        let mut local_buffer = buffer.clone();

        // Count matched
        let matched = 0usize;

        // Get LHS and update count
        let lhs = <LHS as Lexer>::lex(&mut local_buffer)?;
        let matched = matched + 1;

        match <Operator as Lexer>::lex(&mut local_buffer) {
            Ok(operator) => {
                let matched = matched + 1;

                let rhs =
                    <RHS as Lexer>::lex(&mut local_buffer)
                        .map_err(|mut err| {
                            err.matched = matched;
                            err
                        })?;

                // Write to local buffer
                *buffer = local_buffer;

                Ok(Self::Found { lhs, operator, rhs })
            }
            Err(_none) => {
                // Write back to buffer
                *buffer = local_buffer;
                Ok(Self::None(lhs))
            }
        }
    }
}

impl<'code, LHS, Operator, RHS> Token<'code>
    for Infix<LHS, Operator, RHS>
where
    LHS: Lexer<'code>,
    Operator: Lexer<'code>,
    RHS: Lexer<'code>,
{
    fn buffer(&self) -> Option<Buffer<'code>> {
        match self {
            Infix::Found { lhs, rhs, operator } => {
                let buffers = [
                    <LHS as Token>::buffer(lhs),
                    <Operator as Token>::buffer(operator),
                    <RHS as Token>::buffer(rhs),
                ]
                .into_iter();

                buffers
                    .flatten()
                    .reduce(|acc, current| acc + current)
            }
            Infix::None(lhs) => <LHS as Token>::buffer(lhs),
        }
    }

    const NAME: &'static str = "Infix";
}

#[cfg(test)]
mod tests;
