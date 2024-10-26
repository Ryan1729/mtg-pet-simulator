use card::Card::{self, *};
use mana::ManaType::*;

type PermutationNumber = u128;

#[derive(Debug)]
pub enum Spec {
    //AllDraws,
    NthDraw(PermutationNumber),
}
use Spec::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Outcome {
    Lose,
    Win,
}
use Outcome::*;

#[derive(Debug, PartialEq, Eq)]
pub enum CalculateError {
    DeckTooLarge,
    PermutationNumberTooHigh,
    UsizeTooBig,
    FactorialDigitTooBig,
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

    while let Some((d, card)) = draw(deck) {
        hand.push(card);
        deck = d;

        
    }

    return Ok(Lose);
}

#[cfg(test)]
mod calculate_works {
    use super::*;

    const _0: Card = InsatiableAvarice;
    const _1: Card = SchemingSymmetry;
    const _2: Card = FeedTheSwarm;

    #[test]
    fn on_all_swamps() {
        let _60_swamps = [Swamp; 60];

        const specs: [Spec; 7] = [
            NthDraw(0),
            NthDraw(1),
            NthDraw(10),
            NthDraw(100),
            NthDraw(1_000),
            NthDraw(10_000),
            NthDraw(100_000),
        ];

        for spec in specs {
            assert_eq!(
                Ok(Lose),
                calculate(spec, &_60_swamps)
            );
        }
    }
}

type Deck = Box<[Card]>;

fn draw(deck: Deck) -> Option<(Deck, Card)> {
    todo!();
}

fn nth_ordered(
    deck: &[Card],
    mut n: PermutationNumber
) -> Result<Deck, CalculateError> {
    let len = deck.len();
    // Based on https://stackoverflow.com/a/7919887

    let mut fact = Vec::with_capacity(len);
    let mut perm: Vec<usize> = Vec::with_capacity(len);

    // compute factorial numbers
    fact.push(1u128);
    for i in 1..len {
        dbg!(fact[i - 1], i);
        fact.push(fact[i - 1] * PermutationNumber::try_from(i).map_err(|_| UsizeTooBig)?);
    }
    assert_eq!(fact.len(), len);

    // compute factorial code
    for i in 0..len {
        let div = n / fact[len - 1 - i];
        if div >= PermutationNumber::try_from(len).map_err(|_| UsizeTooBig)? {
            return Err(PermutationNumberTooHigh);
        }
        perm.push(div.try_into().map_err(|_| FactorialDigitTooBig)?);
        n = n % fact[len - 1 - i];
    }

    // readjust values to obtain the permutation
    // start from the end and check if preceding values are lower
    for i in (1..=(len - 1)).rev() {
        for j in (0..=(i - 1)).rev() {
            if perm[j] <= perm[i] {
                perm[i] += 1;
            }
        }
    }

    // Apply the permutation to the input deck
    let mut output = Vec::with_capacity(len);

    for i in 0..len {
        output.push(deck[perm[i]])
    }

    Ok(output.into_boxed_slice())
}

#[cfg(test)]
mod nth_ordered_works {
    use super::*;

    const _0: Card = InsatiableAvarice;
    const _1: Card = SchemingSymmetry;
    const _2: Card = FeedTheSwarm;

    #[test]
    fn on_all_the_three_element_permutations() {
        const decks: [[Card; 3]; 6] = [
            [_0, _1, _2],
            [_0, _2, _1],
            [_1, _0, _2],
            [_1, _2, _0],
            [_2, _0, _1],
            [_2, _1, _0],
        ];

        for i in 0..decks.len() {
            let actual = nth_ordered(&decks[0], i.try_into().unwrap()).expect("");
            assert_eq!(*actual, decks[i], "mismatch at {i}");
        }

        let result = nth_ordered(&decks[0], decks.len().try_into().unwrap());
        assert_eq!(result, Err(PermutationNumberTooHigh), "Mismatch at one past the last valid permutation number");
    }
}
