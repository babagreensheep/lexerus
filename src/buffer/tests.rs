use core::panic;

use super::*;

#[test]
fn search_len() {
    let buf0 = Chunk {
        code: "hello",
        range: 1..4,
    };
    let buf1 = Chunk {
        code: "from",
        range: 3..4,
    };
    let buf2 = Chunk {
        code: "the other",
        range: 2..4,
    };
    let lhs = buf1 + buf2;
    let buf =
        Buffer::Frag(lhs.into(), Buffer::from(buf0).into());
    let search_len = buf.search_len();
    assert_eq!(search_len, 6);
}

#[test]
fn add_same_cont_seq() {
    let buf0 = Chunk {
        code: "hello from",
        range: 0..2,
    };
    let buf1 = Chunk {
        code: "hello from",
        range: 2..4,
    };
    let buf = buf0 + buf1;
    println!("Product: {buf:#?}");
    assert_eq!("hell", buf.to_string());
    let Buffer::Cont { .. } = buf
    else {
        panic!("expected contiguous buffer")
    };
}

#[test]
fn add_same_cont_seq_rev() {
    let buf0 = Chunk {
        code: "hello from",
        range: 2..4,
    };
    let buf1 = Chunk {
        code: "hello from",
        range: 0..2,
    };
    let buf = buf0 + buf1;
    println!("Product: {buf:#?}");
    assert_eq!("hell", buf.to_string());
    let Buffer::Cont { .. } = buf
    else {
        panic!("expected contiguous buffer")
    };
}

#[test]
fn add_same_cont_unseq() {
    let buf0 = Chunk {
        code: "hello from",
        range: 0..2,
    };
    let buf1 = Chunk {
        code: "hello from",
        range: 3..5,
    };
    let buf = buf0 + buf1;
    println!("Product: {buf:#?}");
    assert_eq!("helo", buf.to_string());
    let Buffer::Frag { .. } = buf
    else {
        panic!("expected contiguous buffer")
    };
}

#[test]
fn add_blank_lhs() {
    let buf1 = Chunk {
        code: "hello from",
        range: 1..1,
    };
    let buf0 = Chunk {
        code: "the other side",
        range: 2..4,
    };
    let buf = buf0 + buf1;
    println!("Product: {buf:#?}");
    assert_eq!("e ", buf.to_string());
    let Buffer::Cont { .. } = buf
    else {
        panic!("expected contiguous buffer")
    };
}

#[test]
fn add_blank_rhs() {
    let buf1 = Chunk {
        code: "hello from",
        range: 0..5,
    };
    let buf0 = Chunk {
        code: "the other side",
        range: 4..4,
    };
    let buf = buf0 + buf1;
    println!("Product: {buf:#?}");
    assert_eq!("hello", buf.to_string());
    let Buffer::Cont { .. } = buf
    else {
        panic!("expected contiguous buffer")
    };
}

#[test]
fn add_blank_lhs_rhs() {
    let buf1 = Chunk {
        code: "hello from",
        range: 0..0,
    };
    let buf0 = Chunk {
        code: "the other side",
        range: 4..4,
    };
    let buf = buf0 + buf1;
    println!("Product: {buf:#?}");
    assert_eq!("", buf.to_string());
    let Buffer::Cont { .. } = buf
    else {
        panic!("expected contiguous buffer")
    };
}

fn fragments() -> Buffer<'static> {
    let buf0 = Chunk::from("hello");
    let buf1 = Chunk::from("from");
    let buf2 = Chunk::from("the");
    Buffer::from(buf0) + (buf1 + buf2)
}

#[test]
fn add_fragmented_rhs() {
    let buf = fragments();
    println!("Product: {buf:#?}");
    assert_eq!("hellofromthe", buf.to_string());
}

#[test]
fn add_multiple_frag() {
    let buf0 = fragments();
    let buf1 = fragments();
    let buf2 = fragments();
    let buf3 = fragments();
    let buf = buf0 + (buf1 + buf2) + buf3;
    println!("Product: {buf:#?}");
    assert_eq!(
        "hellofromthehellofromthehellofromthehellofromthe",
        buf.to_string()
    );
}

#[test]
fn eat_word() {
    let mut buf = fragments();
    let search = buf.eat_word("hello").unwrap();
    println!("Buffer: {buf:#?}");
    assert_eq!("fromthe", buf.to_string());
    println!("Result: {search:#?}");
    assert_eq!("hello", search.to_string());
}

#[test]
fn eat_word_half() {
    let mut buf = fragments();
    let search = buf.eat_word("hel").unwrap();
    println!("Buffer: {buf:#?}");
    assert_eq!("lofromthe", buf.to_string());
    println!("Result: {search:#?}");
    assert_eq!("hel", search.to_string());
}

#[test]
fn eat_word_fail() {
    let mut buf = fragments();
    let search = buf.eat_word("hellothe");
    println!("Buffer: {buf:#?}");
    assert_eq!("hellofromthe", buf.to_string());
    if let Some(search) = search {
        panic!("expected none, found {search}")
    }
}

#[test]
fn advance_char_simple() {
    let mut buf = fragments();
    let advanced = buf.advance_char(5).unwrap();
    println!("Buffer: {buf:#?}");
    println!("Advanced: {advanced}");
    assert_eq!("fromthe", buf.to_string());
    assert_eq!("hello", advanced.to_string());
}

#[test]
fn advance_char_fail() {
    let mut buf = Buffer::from("h");
    let _ = buf.advance_char(1).unwrap();
    let advanced = buf.advance_char(1);
    println!("Buffer: {buf:#?}");
    assert_eq!(buf.search_len(), 0);
    if let Some(advanced) = advanced {
        panic!("expected none, found {advanced}")
    }
}
