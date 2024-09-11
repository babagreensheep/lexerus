//! [Buffer] is a container for either (depending on the context) the parsed string or the unparsed
//! string.
use std::ops::Range;

#[cfg(test)]
mod tests;

/// Marks a [Buffer] with the core traits that [lexerus_derive::Lexer] uses when decorating a
/// lexer.
pub trait Searchable
where
    Self: Sized,
{
    fn eat_word(&mut self, word: &str) -> Option<Self>;
    fn advance_char(&mut self, by: usize) -> Option<Self>;
    fn search_len(&self) -> usize;
}

#[derive(PartialEq, Clone)]
/// [Buffer] is a container for source code. It is represented as an `enum` because there are two
/// forms of [Buffer] which can be created:
/// - A _contiguous_ chunk of [str] will always be allocated as a [Buffer::Cont] and make use of the simple chunk container.
/// - A _fragmented_ chunk will always result in a heap allocation ([Box]) of chunk to join two non-adjacent chunks to each other.
pub enum Buffer<'code> {
    Cont { chunk: Chunk<'code> },
    Frag(Box<Buffer<'code>>, Box<Buffer<'code>>),
}

impl<'code> std::fmt::Debug for Buffer<'code> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Cont { chunk } => {
                std::fmt::Debug::fmt(chunk, f)
            }
            Self::Frag(arg0, arg1) => f
                .debug_set()
                .entries([arg0, arg1].iter())
                .finish(),
        }
    }
}

impl<'code> std::fmt::Display for Buffer<'code> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Buffer::Cont { chunk } => f.write_str(chunk),
            Buffer::Frag(lhs, rhs) => {
                write!(f, "{}{}", lhs, rhs)
            }
        }
    }
}

impl<'code> Buffer<'code> {
    fn search_char_len(&self) -> usize {
        match self {
            Buffer::Cont { chunk } => chunk
                .search_str()
                .unwrap_or("")
                .chars()
                .count(),
            Buffer::Frag(lhs, rhs) => {
                lhs.search_char_len()
                    + rhs.search_char_len()
            }
        }
    }
}

impl<'code> std::ops::Add for Buffer<'code> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (
                Buffer::Cont { chunk: lhs },
                Buffer::Cont { chunk: rhs },
            ) => lhs + rhs,
            (
                Buffer::Cont { chunk: lhs },
                Buffer::Frag(r_lhs, r_rhs),
            ) => Self::Frag(
                Box::new(Buffer::from(lhs) + *r_lhs),
                r_rhs,
            ),
            (
                Buffer::Frag(l_lhs, l_rhs),
                Buffer::Cont { chunk: rhs },
            ) => Self::Frag(
                l_lhs,
                Box::new(*l_rhs + Buffer::from(rhs)),
            ),
            (
                Buffer::Frag(l_lhs, l_rhs),
                Buffer::Frag(r_lhs, r_rhs),
            ) => *l_lhs + *l_rhs + *r_lhs + *r_rhs,
        }
    }
}

impl<'code> std::ops::Add for Chunk<'code> {
    type Output = Buffer<'code>;

    fn add(mut self, rhs: Self) -> Self::Output {
        match self.search_range() {
            Some(lhs_range) => match rhs.search_range() {
                Some(rhs_range) => {
                    // Check if code same and boundaries
                    // align
                    if self.code == rhs.code
                        && (lhs_range.start
                            == rhs_range.end
                            || lhs_range.end
                                == rhs_range.start)
                    {
                        let start = lhs_range
                            .start
                            .min(rhs_range.start);
                        let end = lhs_range
                            .end
                            .max(rhs_range.end);
                        self.range = start..end;
                        Buffer::from(self)
                    }
                    else {
                        let mut return_value = Buffer::Frag(
                            Box::new(Buffer::from(self)),
                            Box::new(Buffer::from(rhs)),
                        );
                        return_value.defrag();
                        return_value
                    }
                }
                None => Buffer::from(self),
            },
            None => Buffer::from(rhs),
        }
    }
}

impl<'code> Buffer<'code> {
    fn defrag(&mut self) {
        // Remove blank nodes
        if let Self::Frag(lhs, rhs) = &self {
            if let Self::Cont { chunk } = lhs.as_ref() {
                if chunk.search_range().is_none() {
                    *self = *rhs.clone();
                }
            }
            else if let Self::Cont { chunk } =
                rhs.as_ref()
            {
                if chunk.search_range().is_none() {
                    *self = *lhs.clone();
                }
            }
        }
    }
}

impl<'code> Searchable for Buffer<'code> {
    fn eat_word(&mut self, word: &str) -> Option<Self> {
        match self {
            Buffer::Cont { chunk } => chunk
                .eat_word(word)
                .map(|chunk| Self::Cont { chunk }),

            Buffer::Frag(lhs, rhs) => {
                // Construct LHS word
                let lhs_word_end =
                    word.len().min(lhs.search_len());
                let lhs_word = &word[0..lhs_word_end];

                // Attempt to eat on LHS
                let mut lhs_temp = *lhs.clone();
                let lhs_new =
                    lhs_temp.eat_word(lhs_word)?;

                // Construct RHS word
                let rhs_word = &word[lhs_word_end..];
                let rhs_word = (!rhs_word.is_empty())
                    .then_some(rhs_word);

                // Attempt to eat on RHS only if RHS has
                // words to eat
                let return_value = match rhs_word {
                    Some(rhs_word) => {
                        // Attempt to eat on RHS
                        let mut rhs_temp = *rhs.clone();
                        let rhs_new =
                            rhs_temp.eat_word(rhs_word)?;

                        // Reset self
                        // LHS can be disregarded because it
                        // has been consumed
                        *self = rhs_temp;

                        // Create return value and defrag
                        let mut return_value =
                            lhs_new + rhs_new;
                        return_value.defrag();
                        return_value
                    }
                    None => {
                        *lhs = Box::new(lhs_temp);
                        self.defrag();
                        lhs_new
                    }
                };

                // Defrag and return
                Some(return_value)
            }
        }
    }

    fn advance_char(&mut self, by: usize) -> Option<Self> {
        match self {
            Buffer::Cont { chunk } => chunk
                .advance_char(by)
                .map(|chunk| Self::Cont { chunk }),

            Buffer::Frag(lhs, rhs) => {
                // Construct LHS search length
                let by_lhs = lhs.search_char_len().min(by);

                // Attempt to advance on LHS
                let mut lhs_temp = *lhs.clone();
                let lhs_new =
                    lhs_temp.advance_char(by_lhs)?;

                // Construct RHS search length
                let by_rhs = by
                    .checked_sub(by_lhs)
                    .and_then(|value| {
                        (!value.eq(&0)).then_some(value)
                    });

                // Get return value
                let return_value = match by_rhs {
                    // If RHS word exists
                    Some(by_rhs) => {
                        // Construct RHS
                        let mut rhs_temp = *rhs.clone();
                        let rhs_new = rhs_temp
                            .advance_char(by_rhs)?;

                        // Reset self
                        // LHS can be disregarded because it
                        // has been consumed
                        *self = rhs_temp;

                        // Create return value and defrag
                        let mut return_value =
                            lhs_new + rhs_new;
                        return_value.defrag();
                        return_value
                    }
                    None => {
                        *lhs = Box::new(lhs_temp);
                        self.defrag();
                        lhs_new
                    }
                };

                // Defrag and return
                Some(return_value)
            }
        }
    }

    fn search_len(&self) -> usize {
        match self {
            Self::Cont { chunk } => chunk.search_len(),
            Self::Frag(lhs, rhs) => {
                lhs.search_len() + rhs.search_len()
            }
        }
    }
}

impl<'code> From<&'code str> for Buffer<'code> {
    fn from(code: &'code str) -> Self {
        Self::Cont {
            chunk: Chunk::from(code),
        }
    }
}

impl<'code> From<Chunk<'code>> for Buffer<'code> {
    fn from(chunk: Chunk<'code>) -> Self {
        Self::Cont { chunk }
    }
}

#[derive(PartialEq)]
/// Basic container for [str]. This should never be called by itself.
pub struct Chunk<'code> {
    code: &'code str,
    range: Range<usize>,
}

impl<'code> ::std::ops::Deref for Chunk<'code> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        let range = self.search_range().unwrap_or(0..0);
        &self.code[range]
    }
}

impl<'code> From<&'code str> for Chunk<'code> {
    fn from(code: &'code str) -> Self {
        Self {
            code,
            range: 0..code.len(),
        }
    }
}

impl<'code> std::fmt::Debug for Chunk<'code> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self.search_range() {
            Some(range) => {
                write!(
                    f,
                    "[{:?}] {}",
                    range,
                    &self.code[range.clone()],
                )
            }
            None => f.write_str("None"),
        }
    }
}

impl<'code> std::fmt::Display for Chunk<'code> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        f.write_str(self.search_str().unwrap_or(""))
    }
}

impl<'code> Searchable for Chunk<'code> {
    fn eat_word(&mut self, word: &str) -> Option<Self> {
        self.search_str()?
            .starts_with(word)
            .then_some(())?;
        self.advance_by(word.len())
    }

    fn advance_char(&mut self, by: usize) -> Option<Self> {
        let search_str = self.search_str()?;
        let by = by.checked_sub(1)?;
        let mut len = None::<usize>;
        for (count, character) in
            search_str.chars().enumerate()
        {
            // Break if count exceeded
            if count > by {
                break;
            }
            let char_len = character.len_utf8();
            len = match len {
                Some(len) => Some(char_len + len),
                None => Some(char_len),
            }
        }
        let len = len?;
        self.advance_by(len)
    }

    fn search_len(&self) -> usize {
        let search_range =
            self.search_range().unwrap_or(0..0);
        search_range.end - search_range.start
    }
}

impl<'code> Chunk<'code> {
    pub fn range_end(&self) -> usize {
        self.range.end.min(self.code.len())
    }

    pub fn search_range(&self) -> Option<Range<usize>> {
        // Check for overflow
        let search_start = (self.range.start
            < self.code.len())
        .then_some(self.range.start)?;

        // Get range end
        let search_end = self.range_end();

        // Check if the end is larger than start
        (search_end > search_start)
            .then_some(search_start..search_end)
    }

    pub fn search_str(&self) -> Option<&str> {
        // Basic sanity check
        let search_range = self.search_range()?;
        Some(&self.code[search_range])
    }

    pub fn advance_by(
        &mut self,
        by: usize,
    ) -> Option<Self> {
        // Recalculate by
        let by = self.range.start + by;

        // Return one
        if by > self.range_end() {
            None
        }
        // Return none if past code end
        else {
            // Create eaten token
            let mut eaten = self.clone();
            eaten.range.end = by;

            // Move self
            self.range.start = by;

            // Return eaten token
            Some(eaten)
        }
    }
}

impl<'code> Clone for Chunk<'code> {
    fn clone(&self) -> Self {
        Self {
            code: self.code,
            range: self.range.clone(),
        }
    }
}
