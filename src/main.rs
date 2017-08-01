#![allow(unused)]
#![feature(try_from)]
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate dump;

mod theory;

fn main() {
    use theory::*;
    use theory::MAJOR_SCALE;
    use theory::notes::*;
    use theory::intervals::*;
    dump!(FIFTH.clone().equiv_with_base(&FOURTH));
    for chord in MAJOR_SCALE.diatonic_chords() {
        println!("{}", chord);
        for note in chord.based_on(F.flat()) {
            println!("Note: {}", note);
        }
    }
}
