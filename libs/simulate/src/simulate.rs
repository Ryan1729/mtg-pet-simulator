use card::Card::{self, *};
use mana::ManaType::*;

pub enum Spec {
    //AllDraws,
    NthDraw(usize),
}

#[derive(Debug)]
pub struct Outcome {
    // TODO useful fields
}

pub fn calculate(spec: Spec, right: &[Card]) -> Outcome {
    // TODO actual impl
    Outcome {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        todo!();
    }
}
