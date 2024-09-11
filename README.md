**Note**: This readme is auto generated. Please refer to the [docs](https://docs.rs/crate/lexerus/latesthttps://docs.rs/lexerus/latest/lexerus/).

# Lexerus
Lexerus is a **lexer** dinosaur that consumes a [Buffer]
constructed from [str] and spits out a structure through
the [lexer::Lexer::lex] call.

This library uses the [lexerus_derive::Token] and
[lexerus_derive::Lexer] macros to decorate a structure
for automatic parsing. See those macros for additional
options.

This library was developed in conjunction with [SPEW](https://github.com/babagreensheep/spew/tree/dev/elves/winky/src) and examples on actual implementation can be found there.

## Example
```rust

// Create and decorate a struct
#[derive(Lexer, Token, Debug)]
struct Trex<'code>(#[pattern = "trex::"] Buffer<'code>);

#[derive(Lexer, Token, Debug)]
struct TrexCall<'code>(
    #[pattern = "RAWR"] Buffer<'code>,
);

#[derive(Lexer, Token, Debug)]
struct Call<'code> {
    rex: Trex<'code>,
    call: TrexCall<'code>,
}

// Create a raw buffe
let mut buffer = Buffer::from("trex::RAWR");

// Attempt to parse the trex
let trex_calling = Call::lex(&mut buffer).unwrap();

// Extract the buffer from trex
let trex = trex_calling.rex.buffer().unwrap();
let trex_calling = trex_calling.buffer().unwrap();

// Buffer should contain the exact matched string
assert_eq!(trex_calling.to_string(), "trex::RAWR");
assert_eq!(trex.to_string(), "trex::");
```

## Goals
- No heap allocations when _parsing_. However there are
  some exceptions:
  - When using helpers such as [GroupUntil], a [Vec] is
    allocated to store the parsed [Buffer] in individual
    units. Contrast this with [Group] which only
    captures the [Buffer] output   without individual
    segregation.
- Heap allocations only occur when calling
  [Token::buffer] on _non-contigous_ sections of text or
  _repeated_ sections of text. This is inevitable beause
  different sections [str] have to be stitched together
  and teh only way to do so is with a heap allocation.
- Proper debuggable information, i.e. the [Buffer]
  retains information about its source and the
exact range on the source.
