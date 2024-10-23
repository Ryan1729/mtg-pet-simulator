use card::Card::{self, *};
use mana::ManaType::*;

type PermutationNumber = usize;

#[derive(Debug)]
pub enum Spec {
    //AllDraws,
    NthDraw(PermutationNumber),
}
use Spec::*;

#[derive(Debug)]
pub enum Outcome {
    Lose,
    Win,
}
use Outcome::*;

#[derive(Debug)]
pub enum CalculateError {
    DeckTooLarge,
}
use CalculateError::*;

const MAX_DECK_SIZE: u8 = 60;

pub fn calculate(spec: Spec, deck: &[Card]) -> Result<Outcome, CalculateError> {
    if deck.len() > MAX_DECK_SIZE as _ {
        return Err(DeckTooLarge);
    }

    let ordered_deck = match spec {
        NthDraw(n) => {
            nth_ordered(deck, n)
        }
    }?;
    dbg!(&ordered_deck);

    let mut hand = Vec::with_capacity(16);

    let mut deck = ordered_deck;

    while hand.len() < 7 {
        match draw(deck) {
            None => {
                return Ok(Lose);
            },
            Some((d, card)) => {
                hand.push(card);
                deck = d;
            }
        }
    }

    todo!();
}

type Deck = Box<[Card]>;

fn draw(deck: Deck) -> Option<(Deck, Card)> {
    todo!();
}

fn nth_ordered(
    deck: &[Card],
    n: PermutationNumber
) -> Result<Deck, CalculateError> {
   todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        todo!();
    }
}
