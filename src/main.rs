#![allow(unused)]
#![feature(try_from)]
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate dump;

use std::iter::FromIterator;
use std::convert::{From, TryFrom, TryInto};
use std::collections::HashSet;
use std::ops::{Sub, Add};
use std::cmp::{Ordering, PartialOrd, Ord};
use std::fmt;

use Interval::*;

// Basically integers modulo 12.
#[derive(Clone, Copy, PartialEq, Hash, Debug, Eq)]
struct PitchClass {
    rep: i8,
}

fn mod12(n: i8) -> i8 {
    ((n % 12) + 12) % 12
}

// Convenience method for unsafe construction. Keep private.
fn pc<T: Into<i8>>(it: T) -> PitchClass {
    PitchClass { rep: it.into() }
}

impl Shiftable for PitchClass {
    fn shift<T: Into<i8>>(self, amt: T) -> Self {
        pc(mod12(self.rep + amt.into()))
    }
}

impl Add for PitchClass {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        pc(mod12(self.rep + other.rep))
    }
}

impl Sub for PitchClass {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        pc(mod12(self.rep - other.rep))
    }
}

trait Shiftable {
    fn shift<T: Into<i8>>(self, amt: T) -> Self;
}

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
        let semitones = interval.semitones();
        let note_num = PitchClass::from(self.clone());
        Note::from(note_num.shift(semitones))
    }

    fn down(&self, interval: &Interval) -> Self {
        let semitones = interval.semitones();
        let note_num = PitchClass::from(self.clone());
        Note::from(note_num.shift(-1 * semitones))
    }
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
        PitchClass::from(self.clone()) == PitchClass::from(other.clone())
    }
}

impl From<Note> for PitchClass {
    fn from(note: Note) -> PitchClass {
        use Note::*;
        match note {
            A => pc(0),
            B => pc(2),
            C => pc(3),
            D => pc(5),
            E => pc(7),
            F => pc(8),
            G => pc(10),
            Flattened(n) => PitchClass::from(*n).shift(-1),
            Sharpened(n) => PitchClass::from(*n).shift(1),
        }
    }
}

impl From<PitchClass> for Note {
    fn from(pitch: PitchClass) -> Note {
        use Note::*;
        let n = pitch.rep;
        match n {
            0 => A,
            1 => B.flat(),
            2 => B,
            3 => C,
            4 => D.flat(),
            5 => D,
            6 => E.flat(),
            7 => E,
            8 => F,
            9 => G.flat(),
            10 => G,
            11 => A.flat(),
            _ => unreachable!(),
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
        self.semitones().partial_cmp(&other.semitones())
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

impl From<PitchClass> for Interval {
    fn from(pitch: PitchClass) -> Interval {
        use Interval::*;
        let n = pitch.rep;
        match n {
            0 => Unison,
            1 => Second.flat(),
            2 => Second,
            3 => Third.flat(),
            4 => Third,
            5 => Fourth,
            6 => Fifth.flat(),
            7 => Fifth,
            8 => Sixth.flat(),
            9 => Sixth,
            10 => Seventh.flat(),
            11 => Seventh,
            _ => unreachable!(),
        }
    }
}

impl From<Interval> for PitchClass {
    fn from(interval: Interval) -> PitchClass {
        use Interval::*;
        match interval {
            Unison => pc(0),
            Second => pc(2),
            Third => pc(4),
            Fourth => pc(5),
            Fifth => pc(7),
            Sixth => pc(9),
            Seventh => pc(11),
            Flattened(i) => PitchClass::from(*i).shift(-1),
            Sharpened(i) => PitchClass::from(*i).shift(1),
        }
    }
}

impl Interval {
    fn semitones(&self) -> i8 {
        use Interval::*;
        match self {
            &Unison => 0,
            &Second => 2,
            &Third => 4,
            &Fourth => 5,
            &Fifth => 7,
            &Sixth => 9,
            &Seventh => 11,
            &Flattened(ref i) => i.semitones() - 1,
            &Sharpened(ref i) => i.semitones() + 1,
        }
    }

    fn equiv_with_base(self, other: &Self) -> Self {
        let my_pitch = PitchClass::from(self.clone());
        let their_pitch = PitchClass::from(other.clone());
        let up_diff = (their_pitch - my_pitch).rep;
        dump!(up_diff);
        let down_diff = (my_pitch - their_pitch).rep;
        dump!(down_diff);
        match up_diff.cmp(&down_diff) {
            Ordering::Greater => self.flat().equiv_with_base(other),
            Ordering::Less => self.sharp().equiv_with_base(other),
            Ordering::Equal => self,
        }
    }
}

impl Shiftable for Interval {
    fn shift<T: Into<i8>>(self, amt: T) -> Self {
        PitchClass::from(self).shift(amt).into()
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
        dump!(intervals);
        let n = intervals.len();
        for (i, interval) in intervals.iter().enumerate() {
            let mut chord_intervals: HashSet<Interval> = HashSet::new();
            let semitones = interval.semitones();
            chord_intervals.insert(intervals[i].clone().shift(-semitones));
            chord_intervals.insert(intervals[(i + 2) % n].clone().shift(-semitones));
            chord_intervals.insert(intervals[(i + 4) % n].clone().shift(-semitones));
            chords.push(chord(chord_intervals));
        }
        chords
    }
}

impl Sub for Interval {
    type Output = Interval;
    fn sub(self, other: Self) -> Self {
        let first = PitchClass::from(self);
        let second = PitchClass::from(other);
        Interval::from(second - first)
    }
}

impl Add for Interval {
    type Output = Interval;
    fn add(self, other: Self) -> Self {
        let first = PitchClass::from(self);
        let second = PitchClass::from(other);
        Interval::from(second - first)
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

macro_rules! chord {
    ( $( $x:expr ),* ) => {
        {
        let mut ret = HashSet::new();
        $(
            ret.insert($x);
        )*
        chord(ret)
        }
    }
}

macro_rules! scale {
    ( $( $x:expr ),* ) => {
        {
            let mut ret = Vec::new();
            $(
                ret.push($x);
            )*
                scale(ret)
        }
    }
}
lazy_static! {
    static ref MAJOR_TRIAD: Chord = chord![Unison,Third,Fifth];

    static ref MAJOR_SCALE: Scale = scale![Unison,Second,Third,Fourth,Fifth,Sixth,Seventh];
    static ref MELODIC_MINOR: Scale = scale![Unison,Second,Third.flat(),Fourth,Fifth,Sixth.flat(),Seventh]
;
}

fn main() {
    use Note::*;
    use Accidental::*;
    use Interval::*;
    dump!(Fifth.equiv_with_base(&Fourth));
    for chord in MAJOR_SCALE.diatonic_chords() {
        println!("{}", chord);
    }
}
