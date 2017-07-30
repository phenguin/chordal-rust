#![feature(try_from)]
#[macro_use] extern crate maplit;
#[macro_use] extern crate lazy_static;

use std::convert::{From, TryFrom};
use std::collections::HashSet;

use Interval::*;


trait HasAccidnetals
    where Self: Sized
{
    fn flat(self) -> Self;
    fn sharp(self) -> Self;
    fn modify(self, &acc: &Accidental) -> Self {
        use Accidental::*;
        match acc {
            Natural => self,
            Flat => self.flat(),
            Sharp => self.sharp(),
        }
    }
}

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
    Flattened(Box<Note>),
    Sharpened(Box<Note>),
}

impl Note {
    fn up(&self, interval: &Interval) -> Self {
        let interval_num = usize::from(interval.clone());
        let note_num = usize::from(self.clone());
        Self::from((note_num + interval_num) % 12)
    }

    fn down(&self, interval: &Interval) -> Self {
        let interval_num = usize::from(interval.clone());
        let note_num = usize::from(self.clone());
        Self::from((note_num - interval_num) % 12)
    }
}
#[allow(dead_code)]
#[derive(Clone, Copy, PartialOrd, PartialEq, Hash, Debug, Ord, Eq)]
enum Accidental {
    Natural,
    Flat,
    Sharp,
}

impl HasAccidnetals for Note {
    fn sharp(self) -> Self {
        Note::Sharpened(Box::new(self))
    }

    fn flat(self) -> Self {
        Note::Flattened(Box::new(self))
    }
}

impl From<usize> for Note {
    fn from(n: usize) -> Note {
        match Note::try_from(n) {
            Ok(note) => note,
            Err(_) => panic!("Invalid note index."),
        }
    }
}

trait EnharmonicEquiv<Rhs: ?Sized = Self> {
    fn equiv(&self, other: &Rhs) -> bool;
}

impl EnharmonicEquiv<Note> for Note {
    fn equiv(&self, other: &Note) -> bool {
        usize::from(self.clone()) == usize::from(other.clone())
    }
}

impl From<Note> for usize {
    fn from(note: Note) -> usize {
        use Note::*;
        match note {
            A => 0,
            B => 2,
            C => 3,
            D => 5,
            E => 7,
            F => 8,
            G => 10,
            Flattened(n) => (usize::from(*n) - 1) % 12,
            Sharpened(n) => (usize::from(*n) + 1) % 12,
        }
    }
}

impl TryFrom<usize> for Note {
    type Error = ();
    fn try_from(n: usize) -> Result<Note, ()> {
        use Note::*;
        match n {
            0 => Ok(A),
            1 => Ok(B.flat()),
            2 => Ok(B),
            3 => Ok(C),
            4 => Ok(D.flat()),
            5 => Ok(D),
            6 => Ok(E.flat()),
            7 => Ok(E),
            8 => Ok(F),
            9 => Ok(G.flat()),
            10 => Ok(G),
            11 => Ok(A.flat()),
            _ => Err(()),
        }
    }
}

#[derive(Clone, PartialOrd, PartialEq, Hash, Debug, Ord, Eq)]
enum Interval {
    Unison,
    Second,
    Third,
    Fourth,
    Fifth,
    Sixth,
    Seventh,
    Flattened(Box<Interval>),
    Sharpened(Box<Interval>),
}

impl HasAccidnetals for Interval {
    fn sharp(self) -> Self {
        Interval::Sharpened(Box::new(self))
    }

    fn flat(self) -> Self {
        Interval::Flattened(Box::new(self))
    }
}

impl TryFrom<usize> for Interval {
    type Error = ();
    fn try_from(n: usize) -> Result<Interval, ()> {
        use Interval::*;
        match n {
            0 => Ok(Unison),
            1 => Ok(Second.flat()),
            2 => Ok(Second),
            3 => Ok(Third.flat()),
            4 => Ok(Third),
            5 => Ok(Fourth),
            6 => Ok(Fifth.flat()),
            7 => Ok(Fifth),
            8 => Ok(Sixth.flat()),
            9 => Ok(Sixth),
            10 => Ok(Seventh.flat()),
            11 => Ok(Seventh),
            _ => Err(()),
        }
    }
}

impl From<Interval> for usize {
    fn from(interval: Interval) -> usize {
        use Interval::*;
        match interval {
            Unison => 0,
            Second => 2,
            Third => 4,
            Fourth => 5,
            Fifth => 7,
            Sixth => 9,
            Seventh => 11,
            Flattened(i) => (usize::from(*i) - 1) % 12,
            Sharpened(i) => (usize::from(*i) + 1) % 12,
        }
    }
}

struct Chord {
    intervals: HashSet<Interval>,
}

fn chord(ints: HashSet<Interval>) -> Chord {
    Chord {
        intervals: ints,
    }
}

impl Chord {
    fn based_on(&self, base: &Note) -> HashSet<Note> {
        self.intervals.iter().map(|interval| {
            base.up(interval)
        }).collect()
    }
}

lazy_static! {
    static ref MAJOR: Chord = chord({
        let mut m = HashSet::new();
        m.insert(Unison);
        m.insert(Third);
        m.insert(Fifth);
        m
    });
}


fn main() {
    use Note::*;
    use Accidental::*;
    use Interval::*;
    println!("{:?}", MAJOR.based_on(&F));
}
