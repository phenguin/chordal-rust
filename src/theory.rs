use std::iter::FromIterator;
use std::convert::{From, TryFrom, TryInto};
use std::collections::HashSet;
use std::ops::{Sub, Add};
use std::cmp::{Ordering, PartialOrd, Ord};
use std::fmt;

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

trait Shiftable {
    fn shift<T: Into<i8>>(self, amt: T) -> Self;
}

trait HasAccidnetals
    where Self: Sized
{
    fn flat(self) -> Self;
    fn sharp(self) -> Self;
    fn modify(self, &acc: &Accidental) -> Self {
        match acc {
            Natural => self,
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
    pub fn up(&self, interval: &Interval) -> Self {
        let semitones = interval.semitones();
        let note_num = PitchClass::from(self.clone());
        Note::from(note_num.shift(semitones))
    }

    pub fn down(&self, interval: &Interval) -> Self {
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

impl HasAccidnetals for Note {
    fn sharp(self) -> Self {
        Note(self.0, self.1 + 1)
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

impl From<PitchClass> for Note {
    fn from(pitch: PitchClass) -> Note {
        fn note(it: NoteBase) -> Note {
            Note::from(it)
        }

        let n = pitch.rep;
        use self::NoteBase::*;
        match n {
            0 => note(A),
            1 => note(B).flat(),
            2 => note(B),
            3 => note(C),
            4 => note(D).flat(),
            5 => note(D),
            6 => note(E).flat(),
            7 => note(E),
            8 => note(F),
            9 => note(G).flat(),
            10 => note(G),
            11 => note(A).flat(),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, PartialEq, Hash, Debug, Ord, Eq)]
pub enum Interval {
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
        let n = pitch.rep;
        use self::Interval::*;
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
        use self::Interval::*;
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
    pub fn semitones(&self) -> i8 {
        use self::Interval::*;
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

    pub fn equiv_with_base(self, other: &Self) -> Self {
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
                "{:?} ",
                i,
            )?;
        }
        write!(f, "]")
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
    pub fn based_on(&self, base: &Note) -> HashSet<Note> {
        self.intervals
            .iter()
            .map(|interval| base.up(interval))
            .collect()
    }
}

macro_rules! chord {
    ( $( $x:expr ),* ) => {
        {
            use self::Interval::*;
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
            use self::Interval::*;
            let mut ret = Vec::new();
            $(
                ret.push($x);
            )*
                scale(ret)
        }
    }
}

lazy_static! {
    pub static ref MAJOR_TRIAD: Chord = chord![Unison,Third,Fifth];

    pub static ref MAJOR_SCALE: Scale = scale![Unison,Second,Third,Fourth,Fifth,Sixth,Seventh];
    pub static ref MELODIC_MINOR: Scale =
        scale![Unison,Second,Third.flat(),Fourth,Fifth,Sixth.flat(),Seventh];
}

pub mod notes {
    use super::*;
    lazy_static! {
        pub static ref A: Note = NoteBase::A.into();
        pub static ref B: Note = NoteBase::B.into();
        pub static ref C: Note = NoteBase::C.into();
        pub static ref D: Note = NoteBase::D.into();
        pub static ref E: Note = NoteBase::E.into();
        pub static ref F: Note = NoteBase::F.into();
        pub static ref G: Note = NoteBase::G.into();
    }
}

pub mod intervals {
    use super::*;
    lazy_static! {
        pub static ref UNISON: Interval = Interval::Unison;
        pub static ref SECOND: Interval = Interval::Second;
        pub static ref THIRD: Interval = Interval::Third;
        pub static ref FOURTH: Interval = Interval::Fourth;
        pub static ref FIFTH: Interval = Interval::Fifth;
        pub static ref SIXTH: Interval = Interval::Sixth;
        pub static ref SEVENTH: Interval = Interval::Seventh;
    }
}
