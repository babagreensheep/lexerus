**Note**: This readme is auto generated. Please refer to the [docs](https://docs.rs/crate/lexerus/latest).

# Lexerus
Lexerus is a **lexer** dinosaur that consumes a [Buffer]
constructed from [str] and spits out a structure through
the [lexer::Lexer::lex] call.

This library uses the [lexerus_derive::Token] and
[lexerus_derive::Lexer] macros to decorate a structure
for automatic parsing. See those macros for additional
options.

This library was developed in conjunction with [SPEW](https://github.com/babagreensheep/spew/tree/dev/elves/winky/src) and examples on actual implementation can be found there.

An annotated `struct` will act as an AND and all tokens must be matched before
[Lexer::lex] returns a valid [Result::Ok]
An annotated `enum` acts as an OR and any of the match arms must be met in order for the
[Lexer::lex] to return a valid [Result::Ok]


## Example
```rust

// Create and decorate a struct
#[derive(Lexer, Token, Debug)]
enum Trex<'code> {
    Trex(#[pattern = "rawr"] Buffer<'code>),
    Other(#[pattern = "meow"] Buffer<'code>),
};

// Create a raw buffe
let mut buffer = Buffer::from("rawr");

// Attempt to parse the trex
let trex_calling = Trex::lex(&mut buffer).unwrap();

if let Trex::Trex(trex_calling) = trex_calling {
    assert_eq!(trex_calling.to_string(), "rawr");
}
else {
    panic!("expected trex");
}
```

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
- No heap allocations when _parsing_. However be aware that certain [helpers] may use heap allocations if required.
- Heap allocations only occur when calling [Token::buffer] on _non-contigous_ sections of text or_repeated_ sections of text. This is inevitable beause different sections of  [str] have to be stitched together and the only way to do so is with a heap allocation.
- Proper debuggable information, i.e. the [Buffer] retains information about its source and theexact range on the source. The [Error] which [Lexer::lex] generates contains a clone of the unparsed [Buffer] so that the program can debug where the [Lexer::lex] failed.
