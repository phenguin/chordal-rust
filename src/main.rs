#![feature(try_from)]
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate lazy_static;

use std::iter::FromIterator;
use std::convert::{From, TryFrom};
use std::collections::HashSet;
use std::ops::{Sub, Add};
use std::cmp::{Ordering, PartialOrd};
use std::fmt;

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
#[derive(Clone, PartialEq, Hash, Debug, Eq)]
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
        Self::try_from((note_num + interval_num) % 12).unwrap()
    }

    // fn down(&self, interval: &Interval) -> Self {
    //     let interval_num = usize::from(interval.clone());
    //     let note_num = usize::from(self.clone());
    //     Self::try_from((note_num - interval_num) % 12).unwrap()
    // }
}
#[allow(dead_code)]
#[derive(Clone, Copy, PartialOrd, PartialEq, Hash, Debug, Ord, Eq)]
enum Accidental {
    Flat,
    Natural,
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

#[derive(Clone, PartialEq, Hash, Debug, Ord, Eq)]
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

impl PartialOrd for Interval {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        usize::from(self.clone()).partial_cmp(&usize::from(other.clone()))
    }
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

impl Interval {
    fn up_semitones(self, n: usize) -> Interval {
        let rep: usize = usize::from(self);
        Interval::try_from((rep + n) % 12).unwrap()
    }
}

#[derive(Debug)]
struct Chord {
    intervals: HashSet<Interval>,
}

impl fmt::Display for Chord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut intervals = Vec::from_iter(self.intervals.iter().cloned());
        intervals.sort();
        write!(f, "[")?;
        for i in &intervals {
            write!(
                f,
                "{:?} ",
                i,
            )?;
        }
        write!(f, "]")
    }
}

#[derive(Debug)]
struct Scale {
    intervals: Vec<Interval>,
}



fn chord(ints: HashSet<Interval>) -> Chord {
    Chord { intervals: ints }
}

fn scale(ints: Vec<Interval>) -> Scale {
    Scale { intervals: ints }
}

impl Scale {
    fn diatonic_chords(&self) -> Vec<Chord> {
        let mut chords = Vec::new();
        let intervals = &self.intervals;
        let n = intervals.len();
        for (i, interval) in intervals.iter().enumerate() {
            let mut chord_intervals: HashSet<Interval> = HashSet::new();
            chord_intervals.insert(intervals[i].clone() - interval.clone());
            chord_intervals.insert(intervals[(i+2) % n].clone() - interval.clone());
            chord_intervals.insert(intervals[(i+4) % n].clone() - interval.clone());
            chords.push(chord(chord_intervals));
        }
        chords
    }
}

impl Sub for Interval {
    type Output = Interval;
    fn sub(self, other: Self) -> Self {
        let first = usize::from(self);
        let second = usize::from(other);
        let neg_second = (12 as usize).checked_sub(second % 12).unwrap() % 12;
        Interval::try_from((first + neg_second) % 12).unwrap()
     }
}

impl Add for Interval {
    type Output = Interval;
    fn add(self, other: Self) -> Self {
        let first = usize::from(self);
        let second = usize::from(other);
        Interval::try_from((first + second) % 12).unwrap()
    }
}

impl Chord {
    fn based_on(&self, base: &Note) -> HashSet<Note> {
        self.intervals
            .iter()
            .map(|interval| base.up(interval))
            .collect()
    }
}

lazy_static! {
    static ref MAJOR_TRIAD: Chord = chord(hashset!{Unison, Third, Fifth});

    static ref MAJOR_SCALE: Scale = scale(
        vec![Unison, Second, Third, Fourth, Fifth, Sixth, Seventh]
    );
}


fn main() {
    use Note::*;
    use Accidental::*;
    use Interval::*;
    for chord in MAJOR_SCALE.diatonic_chords() {
        println!("{}", chord);
    }
}
