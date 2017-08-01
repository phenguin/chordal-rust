#![feature(const_fn)]

use std::iter::FromIterator;
use std::convert::{From, TryFrom, TryInto};
use std::collections::HashSet;
use std::ops::{Sub, Add};
use std::cmp::{Ordering, PartialOrd, Ord};
use std::fmt;
use std::ops::Deref;

use self::Accidental::*;

// Basically integers modulo 12.
#[derive(Clone, Copy, PartialEq, Hash, Debug, Eq)]
pub struct PitchClass {
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

pub trait Shiftable {
    fn shift<T: Into<i8>>(self, amt: T) -> Self;
}

pub trait HasAccidentals
    where Self: Sized
{
    type Output: HasAccidentals;
    fn flat(self) -> Self::Output;
    fn sharp(self) -> Self::Output;
    fn natural(self) -> Self::Output;
    fn modify(self, &acc: &Accidental) -> Self::Output {
        match acc {
            Natural => self.natural(),
            Flat => self.flat(),
            Sharp => self.sharp(),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Hash, Debug, Eq)]
enum NoteBase {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
}

#[derive(Copy, Clone, PartialEq, Hash, Debug, Eq)]
pub struct Note(NoteBase, i8);

impl Note {
    pub fn up(&self, interval: Interval) -> Self {
        let semitones = interval.semitones();
        let note_num = PitchClass::from(self.clone());
        Note::from(note_num.shift(semitones))
    }

    pub fn down(&self, interval: Interval) -> Self {
        let semitones = interval.semitones();
        let note_num = PitchClass::from(self.clone());
        Note::from(note_num.shift(-1 * semitones))
    }
}
#[allow(dead_code)]
#[derive(Clone, Copy, PartialOrd, PartialEq, Hash, Debug, Ord, Eq)]
pub enum Accidental {
    Flat,
    Natural,
    Sharp,
}

impl HasAccidentals for NoteBase {
    type Output = Note;
    fn sharp(self) -> Note {
        self.natural().sharp()
    }

    fn natural(self) -> Note {
        Note(self, 0)
    }

    fn flat(self) -> Note {
        self.natural().flat()
    }
}

impl HasAccidentals for Note {
    type Output = Self;
    fn sharp(self) -> Self {
        Note(self.0, self.1 + 1)
    }

    fn natural(self) -> Self {
        self.0.natural()
    }

    fn flat(self) -> Self {
        Note(self.0, self.1 - 1)
    }
}

pub trait EnharmonicEquiv<Rhs: ?Sized = Self> {
    fn equiv(&self, other: &Rhs) -> bool;
}

impl EnharmonicEquiv<Note> for Note {
    fn equiv(&self, other: &Note) -> bool {
        PitchClass::from(self.clone()) == PitchClass::from(other.clone())
    }
}

impl From<Note> for PitchClass {
    fn from(note: Note) -> PitchClass {
        use self::NoteBase::*;
        let Note(base, accs) = note;
        let base_val = match base {
            A => 0,
            B => 2,
            C => 3,
            D => 5,
            E => 7,
            F => 8,
            G => 10,
        };
        pc(base_val + accs)
    }
}

impl From<NoteBase> for Note {
    fn from(base: NoteBase) -> Note {
        Note(base, 0)
    }
}

impl From<IntervalBase> for Interval {
    fn from(base: IntervalBase) -> Interval {
        Interval(base, 0)
    }
}

impl From<PitchClass> for Note {
    fn from(pitch: PitchClass) -> Note {
        let n = pitch.rep;
        use self::NoteBase::*;
        match n {
            0 => A.natural(),
            1 => B.flat(),
            2 => B.natural(),
            3 => C.natural(),
            4 => D.flat(),
            5 => D.natural(),
            6 => E.flat(),
            7 => E.natural(),
            8 => F.natural(),
            9 => G.flat(),
            10 => G.natural(),
            11 => A.flat(),
            _ => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, PartialOrd, PartialEq, Hash, Debug, Ord, Eq)]
enum IntervalBase {
    Unison,
    Second,
    Third,
    Fourth,
    Fifth,
    Sixth,
    Seventh,
}

impl IntervalBase {
    fn nat(self) -> Interval {
        Interval(self, 0)
    }
}

#[derive(Copy, Clone, PartialEq, Hash, Debug, Ord, Eq)]
pub struct Interval(IntervalBase, i8);

impl PartialOrd for Interval {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.semitones().partial_cmp(&other.semitones())
    }
}

impl HasAccidentals for IntervalBase {
    type Output = Interval;
    fn sharp(self) -> Interval {
        self.natural().sharp()
    }

    fn natural(self) -> Interval {
        Interval(self, 0)
    }

    fn flat(self) -> Interval {
        self.natural().flat()
    }
}

impl HasAccidentals for Interval {
    type Output = Self;
    fn sharp(self) -> Self {
        Interval(self.0, self.1 + 1)
    }

    fn natural(self) -> Self {
        self.0.natural()
    }

    fn flat(self) -> Self {
        Interval(self.0, self.1 - 1)
    }
}

impl From<PitchClass> for Interval {
    fn from(pitch: PitchClass) -> Interval {
        let n = pitch.rep;
        use self::IntervalBase::*;
        match n {
            0 => Unison.natural(),
            1 => Second.flat(),
            2 => Second.natural(),
            3 => Third.flat(),
            4 => Third.natural(),
            5 => Fourth.natural(),
            6 => Fifth.flat(),
            7 => Fifth.natural(),
            8 => Sixth.flat(),
            9 => Sixth.natural(),
            10 => Seventh.flat(),
            11 => Seventh.natural(),
            _ => unreachable!(),
        }
    }
}

impl From<Interval> for PitchClass {
    fn from(interval: Interval) -> PitchClass {
        use self::IntervalBase::*;
        let Interval(base, acc) = interval;
        let base_val = match base {
            Unison => 0,
            Second => 2,
            Third => 4,
            Fourth => 5,
            Fifth => 7,
            Sixth => 9,
            Seventh => 11,
        };
        pc(base_val + acc)
    }
}

impl Interval {
    pub fn semitones(self) -> i8 {
        use self::IntervalBase::*;
        let Interval(base, acc) = self;
        acc + (match base {
            Unison => 0,
            Second => 2,
            Third => 4,
            Fourth => 5,
            Fifth => 7,
            Sixth => 9,
            Seventh => 11,
        })
    }

    pub fn equiv_with_base(self, other: &Self) -> Self {
        let my_pitch = PitchClass::from(self.clone());
        let their_pitch = PitchClass::from(other.clone());
        let up_diff = (their_pitch - my_pitch).rep;
        let down_diff = (my_pitch - their_pitch).rep;
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
pub struct Chord {
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
                "{} ",
                i,
            )?;
        }
        write!(f, "]")
    }
}


impl fmt::Display for IntervalBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::IntervalBase::*;
        let it = match *self {
            Unison => "R",
            Second => "2",
            Third => "3",
            Fourth => "4",
            Fifth => "5",
            Sixth => "6",
            Seventh => "7",
        };
        write!(f, "{}", it)

    }
}

impl fmt::Display for NoteBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <Self as fmt::Debug>::fmt(self, f)
    }
}

impl fmt::Display for Note {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Note(base, acc) = *self;
        write!(f, "{}", base)?;
        let rest: String = match acc.cmp(&0i8) {
            Ordering::Equal => "".into(),
            Ordering::Greater => (0..acc).map(|_| "#").collect(),
            Ordering::Less => (0..-acc).map(|_| "b").collect(),
        };
        write!(f, "{}", rest)
    }
}

impl fmt::Display for Interval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Interval(base, acc) = *self;
        let rest: String = match acc.cmp(&0i8) {
            Ordering::Equal => "".into(),
            Ordering::Greater => (0..acc).map(|_| "#").collect(),
            Ordering::Less => (0..-acc).map(|_| "b").collect(),
        };
        write!(f, "{}", rest)?;
        write!(f, "{}", base)
    }
}

#[derive(Debug)]
pub struct Scale {
    intervals: Vec<Interval>,
}



fn chord(ints: HashSet<Interval>) -> Chord {
    Chord { intervals: ints }
}

fn scale(ints: Vec<Interval>) -> Scale {
    Scale { intervals: ints }
}

impl Scale {
    pub fn diatonic_chords(&self) -> Vec<Chord> {
        let mut chords = Vec::new();
        let intervals = &self.intervals;
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
    pub fn based_on(&self, base: Note) -> HashSet<Note> {
        self.intervals
            .iter()
            .map(|interval| base.up(*interval))
            .collect()
    }
}

macro_rules! chord {
    ( $( $x:expr ),* ) => {
        {
            use self::intervals::*;
            let mut ret: HashSet<Interval> = HashSet::new();
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
            use self::intervals::*;
            let mut ret: Vec<Interval> = Vec::new();
            $(
                ret.push($x);
            )*
                scale(ret)
        }
    }
}

lazy_static! {
    pub static ref MAJOR_TRIAD: Chord = chord![UNISON,THIRD,FIFTH];
    pub static ref MAJOR_SCALE: Scale = scale![UNISON,SECOND,THIRD,FOURTH,FIFTH,SIXTH,SEVENTH];
    pub static ref MELODIC_MINOR: Scale =
        scale![UNISON,SECOND,THIRD.flat(),FOURTH,FIFTH,SIXTH.flat(),SEVENTH];
}

pub mod notes {
    use super::*;
    pub const A: Note = Note(NoteBase::A,0);
    pub const B: Note = Note(NoteBase::B,0);
    pub const C: Note = Note(NoteBase::C,0);
    pub const D: Note = Note(NoteBase::D,0);
    pub const E: Note = Note(NoteBase::E,0);
    pub const F: Note = Note(NoteBase::F,0);
    pub const G: Note = Note(NoteBase::G,0);

}

pub mod intervals {
    use super::*;
    pub use super::Shiftable;
    pub use super::HasAccidentals;
    pub use super::EnharmonicEquiv;

    pub const UNISON: Interval = Interval(IntervalBase::Unison,0);
    pub const SECOND: Interval = Interval(IntervalBase::Second,0);
    pub const THIRD: Interval = Interval(IntervalBase::Third,0);
    pub const FOURTH: Interval = Interval(IntervalBase::Fourth,0);
    pub const FIFTH: Interval = Interval(IntervalBase::Fifth,0);
    pub const SIXTH: Interval = Interval(IntervalBase::Sixth,0);
    pub const SEVENTH: Interval = Interval(IntervalBase::Seventh,0);
}
