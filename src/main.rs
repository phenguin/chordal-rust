#![feature(try_from)]
use std::convert::{From, TryFrom};

#[allow(dead_code)]
#[derive(Clone, PartialOrd, PartialEq, Hash, Debug, Ord, Eq)]
enum Note {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    Flat(Box<Note>),
    Sharp(Box<Note>),
}

fn sharp(note: Note) -> Note {
    Note::Sharp(Box::new(note))
}

fn flat(note: Note) -> Note {
    Note::Flat(Box::new(note))
}

impl From<usize> for Note {
    fn from(n: usize) -> Note {
        match Note::try_from(n) {
            Ok(note) => note,
            Err(_) => panic!("Invalid note index."),
        }
    }
}

impl TryFrom<usize> for Note {
    type Error = ();
    fn try_from(n: usize) -> Result<Note, ()> {
        use Note::*;
        match n {
            0 => Ok(A),
            1 => Ok(flat(B)),
            2 => Ok(B),
            3 => Ok(C),
            4 => Ok(flat(D)),
            5 => Ok(D),
            6 => Ok(flat(E)),
            7 => Ok(E),
            8 => Ok(F),
            9 => Ok(flat(G)),
            10 => Ok(G),
            11 => Ok(flat(A)),
            _ => Err(()),
        }
    }
}



fn main() {
    println!("Hello, world!");
}
