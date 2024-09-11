//! Various helper groups meant to capture
//! groups of things.

// App imports
use crate::*;
use ::std::marker::PhantomData;

#[derive(Clone)]
/// Captures a group of _contiguous_ `Capture`. Unlike [GroupUntil], this structure does not check
/// for an end token.
///
/// **Fails*:
/// - Requisite `NUM` of `Capture` not met.
///
/// The `Capture` is _not allocated_ which means that the [Buffer] is not split out into individual [Buffer]s. Use only if `Capture` does not need to be individually parsed, for example in a string of characters.
///
/// **Warning**: Caller is responsible for checking for escaped `Capture`
pub struct Group<'code, Capture, const NUM: usize> {
    buffer: Option<Buffer<'code>>,
    _captured: PhantomData<Capture>,
}

impl<'code, Capture, const NUM: usize> std::fmt::Debug
    for Group<'code, Capture, NUM>
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        f.debug_tuple("Group").field(&self.buffer).finish()
    }
}

impl<'code, Capture, const NUM: usize> Token<'code>
    for Group<'code, Capture, NUM>
where
    Capture: Token<'code>,
{
    const NAME: &'static str = <Capture as Token>::NAME;

    fn buffer(&self) -> Option<Buffer<'code>> {
        self.buffer.clone()
    }
}

impl<'code, Capture, const NUM: usize> Lexer<'code>
    for Group<'code, Capture, NUM>
where
    Capture: Lexer<'code>,
{
    fn lex(
        buffer: &mut Buffer<'code>,
    ) -> Result<Self, Error<'code>> {
        // Local
        let mut local_buffer = buffer.clone();

        let mut return_buffer = None::<Buffer<'code>>;
        let mut matched = 0usize;

        // Attempt to eat from buffer
        loop {
            match <Capture as Lexer>::lex(&mut local_buffer)
            {
                Ok(captured) => {
                    return_buffer = match return_buffer {
                        Some(return_buffer) => Some(
                            match <Capture as Token>::buffer(
                                &captured,
                            ) {
                                Some(captured_buffer) => {
                                    return_buffer
                                        + captured_buffer
                                }
                                None => return_buffer,
                            },
                        ),
                        None => <Capture as Token>::buffer(
                            &captured,
                        )
                        .clone(),
                    };
                    matched += 1;
                }
                Err(mut err) => {
                    // Check if one
                    if NUM > 0 && matched < NUM {
                        // Update error count and call error
                        err.matched = matched;
                        Err(err)?
                    }
                    // return
                    break;
                }
            }
        }

        *buffer = local_buffer;

        Ok(Self {
            buffer: return_buffer,
            _captured: PhantomData,
        })
    }
}

#[derive(Clone)]
/// Captures a group of _contiguous_ `Capture` until  `End` is found.
///
/// **Fails*:
/// - Buffer is unable to match any more `Capture` but is also unable to match `End`; **or**
/// - Requisite `NUM` of `Capture` not met.
///
/// The `Capture` is _not allocated_ which means that the [Buffer] is not split out into individual [Buffer]s. Use only if `Capture` does not need to be individually parsed, for example in a string of characters.
pub struct GroupUntil<'code, Capture, End, const NUM: usize>
{
    buffer: Option<Buffer<'code>>,
    _captured: PhantomData<Capture>,
    _end: PhantomData<End>,
}

impl<'code, Capture, End, const NUM: usize> std::fmt::Debug
    for GroupUntil<'code, Capture, End, NUM>
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        f.debug_tuple("Group").field(&self.buffer).finish()
    }
}

impl<'code, Capture, End, const NUM: usize> Token<'code>
    for GroupUntil<'code, Capture, End, NUM>
where
    Capture: Token<'code>,
    End: Token<'code>,
{
    const NAME: &'static str = <Capture as Token>::NAME;

    fn buffer(&self) -> Option<Buffer<'code>> {
        self.buffer.clone()
    }
}

impl<'code, Capture, End, const NUM: usize> Lexer<'code>
    for GroupUntil<'code, Capture, End, NUM>
where
    Capture: Lexer<'code>,
    End: Lexer<'code>,
{
    fn lex(
        buffer: &mut Buffer<'code>,
    ) -> Result<Self, Error<'code>> {
        // Local
        let mut local_buffer = buffer.clone();

        let mut return_buffer = None::<Buffer<'code>>;
        let mut matched = 0usize;

        // Local buffer is cloned agained to prevent
        // eating the end token since the end token is not
        // treated as part of the capture group
        while <End as Lexer>::lex(&mut local_buffer.clone())
            .is_err()
        {
            // Capture
            let captured =
                <Capture as Lexer>::lex(&mut local_buffer)
                    .map_err(|mut err| {
                        err.matched = matched;
                        if let Kind::NotFound(syntax) =
                            &mut err.kind
                        {
                            *syntax = <End as Token>::NAME
                        }
                        err
                    })?;

            // Return
            return_buffer = match return_buffer {
                Some(return_buffer) => Some(
                    match <Capture as Token>::buffer(
                        &captured,
                    ) {
                        Some(captured_buffer) => {
                            return_buffer + captured_buffer
                        }
                        None => return_buffer,
                    },
                ),
                None => {
                    <Capture as Token>::buffer(&captured)
                        .clone()
                }
            };
            matched += 1;
        }

        // Check if one and throw if not
        if NUM > 0 && matched < NUM {
            Err(Error {
                buffer: local_buffer.clone(),
                matched,
                kind: Kind::NotFound(<Self as Token>::NAME),
            })?
        }

        *buffer = local_buffer;

        Ok(Self {
            buffer: return_buffer,
            _captured: PhantomData,
            _end: PhantomData,
        })
    }
}

#[derive(Debug, Clone)]
/// Captures a group of _contiguous_ `Capture` bookended by the `Start` and `End` types.
///
/// **Fails*:
/// - No `Start` is found; **or**
/// - Buffer is unable to match any more `Capture` but is also unable to match `End`; **or**
/// - Requisite `NUM` of `Capture` not met.
///
/// The `Capture` is _not allocated_ which means that the [Buffer] is not split out into individual [Buffer]s. Use only if `Capture` does not need to be individually parsed, for example in a string of characters.
pub struct GroupBookEnd<
    'code,
    Start,
    Capture,
    End,
    const NUM: usize,
> {
    start: Start,
    group: GroupUntil<'code, Capture, End, NUM>,
    end: End,
}

impl<'code, Start, Capture, End, const NUM: usize>
    Token<'code>
    for GroupBookEnd<'code, Start, Capture, End, NUM>
where
    Start: Token<'code>,
    Capture: Token<'code>,
    End: Token<'code>,
{
    const NAME: &'static str = <Capture as Token>::NAME;

    fn buffer(&self) -> Option<Buffer<'code>> {
        let buffers = [
            <Start as Token>::buffer(&self.start),
            <GroupUntil<'code, Capture, End, NUM> as Token>::buffer(&self.group),
            <End as Token>::buffer(&self.end),
        ]
        .into_iter();

        buffers
            .flatten()
            .reduce(|acc, current| acc + current)
    }
}

impl<'code, Start, Capture, End, const NUM: usize>
    Lexer<'code>
    for GroupBookEnd<'code, Start, Capture, End, NUM>
where
    Start: Lexer<'code>,
    Capture: Lexer<'code>,
    End: Lexer<'code>,
{
    fn lex(
        buffer: &mut Buffer<'code>,
    ) -> Result<Self, Error<'code>> {
        let mut local_buffer = buffer.clone();
        let mut matched = 0usize;

        // Consume start
        let start =
            <Start as Lexer>::lex(&mut local_buffer)?;
        matched += 1;

        // Get group buffer
        let group =
            <GroupUntil<Capture, End, NUM> as Lexer>::lex(
                &mut local_buffer,
            )
            .map_err(|mut err| {
                matched += err.matched;
                err.matched = matched;
                err
            })?;

        // iterate
        let end = <End as Lexer>::lex(&mut local_buffer)
            .map_err(|mut err| {
                matched += err.matched;
                err.matched = matched;
                err
            })?;

        // Reset buffer
        *buffer = local_buffer;

        Ok(Self { start, group, end })
    }
}
