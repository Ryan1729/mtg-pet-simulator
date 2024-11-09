use card::Card::{self, *};
use mana::ManaType::*;

use::std::collections::HashSet;

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
pub struct OutcomeAt {
    outcome: Outcome,
    at: TurnNumber,
}

// TODO non-empty vec
pub type Outcomes = Vec<OutcomeAt>;

pub type Hand = Vec<Card>;

#[derive(Debug, PartialEq, Eq)]
pub enum CalculateError {
    DeckTooSmall,
    DeckTooLarge,
    PermutationNumberTooHigh,
    UsizeTooBig,
    FactorialDigitTooBig,
}
use CalculateError::*;

const MAX_DECK_SIZE: u8 = 60;

pub fn calculate(spec: Spec, deck: &[Card]) -> Result<Outcomes, CalculateError> {
    if deck.len() > MAX_DECK_SIZE as _ {
        return Err(DeckTooLarge);
    }

    let ordered_deck = match spec {
        NthDraw(n) => {
            nth_ordered(deck, n)
        }
    }?;
    dbg!(&ordered_deck);

    let mut states = Vec::with_capacity(64);

    let mut hand = Vec::with_capacity(16);

    let mut deck = ordered_deck;

    while hand.len() < 7 {
        match draw(deck) {
            None => {
                return Err(DeckTooSmall);
            },
            Some((card, d)) => {
                hand.push(card);
                deck = d;
            }
        }
    }

    states.push(
        State {
            hand,
            board: Board::default(),
            deck,
            turn_number: 0,
            step: Step::default(),
            land_plays: INITIAL_LAND_PLAYS,
        }
    );

    let mut outcomes = Vec::with_capacity(64);

    while let Some(state) = states.pop() {
        dbg!(states.len());
        let mut results = calculate_step(state);
        dbg!(&results);
        for result in results.into_vec().into_iter() {
            let result: Result<State, OutcomeAt> = result;
            match result {
                Ok(s) => {
                    states.push(s);
                }
                Err(outcome) => {
                    outcomes.push(outcome);
                }
            }
        }
    }

    return Ok(outcomes);
}

fn calculate_step(mut state: State) -> Box<[Result<State, OutcomeAt>]> {
    macro_rules! one_path_forward {
        () => {
            return Box::from([Ok(state)])
        }
    }

    match state.step {
        Draw => {
            if let Some((card, d)) = draw(state.deck) {
                state.deck = d;
                state.hand.push(card);
                state.step = MainPhase1;

                one_path_forward!()
            } else {
                return Box::from([
                    Err(OutcomeAt {
                        outcome: Lose,
                        at: state.turn_number,
                    })
                ]);
            }
        },
        MainPhase1 => {
            if state.land_plays > 0 {
                let mut seen_land = HashSet::with_capacity(state.hand.len());
                let mut land_indexes = Vec::with_capacity(state.hand.len());

                for (i, card) in state.hand.iter().enumerate() {
                    if card.is_land() && !seen_land.contains(card) {
                        land_indexes.push(i);
                        seen_land.insert(card);
                    }
                }
    
                // TODO? Is it ever worth doing to not play a land if you can?
                match land_indexes.len() {
                    0 => {},
                    1 => {
                        let card = state.hand.remove(land_indexes[0]);
                        state.board.lands.push(card);
                        state.land_plays -= 1;

                        one_path_forward!()
                    }
                    _ => {
                        let mut output = Vec::with_capacity(land_indexes.len());

                        for i in land_indexes.into_iter().rev() {
                            let (h, card) = remove(&state.hand, i).expect("land index to be valid");

                            output.push(Ok(State {
                                hand: h.to_vec(),
                                board: Board {
                                    lands: push(&state.board.lands, card),
                                    ..state.board.clone()
                                },
                                land_plays: state.land_plays - 1,
                                ..state.clone()
                            }));
                        }

                        return Box::from(output);
                    }
                }
            }

            let mut output = Vec::with_capacity(state.hand.len());

            // TODO add more possible plays when there are any

            state.step = End;
            output.push(Ok(state));
            return Box::from(output);
        }
        End => {
            state.turn_number += 1;
            state.step = Step::default();
            state.land_plays = INITIAL_LAND_PLAYS;
            one_path_forward!()
        }
    }
}

// TODO consider making Deck into an opaque type that we will be easily able to replace with a fast(er) 
// immutable library type later on
fn remove(slice: &[Card], index: usize) -> Option<(Box<[Card]>, Card)> {
    if let Some(element) = slice.get(index).cloned() {
        let mut output = Vec::with_capacity(slice.len() - 1);

        for (i, e) in slice.iter().enumerate() {
            if i == index { continue }

            output.push(e.clone());
        }

        Some((output.into(), element))
    } else {
        None
    }
}

#[cfg(test)]
mod remove_works {
    use super::*;

    #[test]
    fn on_these_examples() {
        let empty = &[];
        a_eq!(remove(empty, 0), None);
        a_eq!(remove(&[Swamp], 0), Some((Box::from([]), Swamp)));
        a_eq!(remove(&[InsatiableAvarice, Swamp], 0), Some((Box::from([Swamp]), InsatiableAvarice)));
        a_eq!(remove(&[InsatiableAvarice, Swamp], 1), Some((Box::from([InsatiableAvarice]), Swamp)));
    }
}

fn push(slice: &[Card], element: Card) -> Vec<Card> {
    let mut output = Vec::with_capacity(slice.len() + 1);

    for e in slice.iter() {
        output.push(e.clone());
    }
    output.push(element);

    output
}

#[cfg(test)]
mod push_works {
    use super::*;

    #[test]
    fn on_these_examples() {
        let empty = &[];
        a_eq!(push(empty, Swamp), vec![Swamp]);
        a_eq!(push(&[Swamp], Swamp), vec![Swamp, Swamp]);
    }
}

/// 64k turns ought to be enough for anybody!
type TurnNumber = u16;

#[derive(Clone, Debug, Default)]
struct Board {
    lands: Vec<Card>,
}

#[derive(Copy, Clone, Debug, Default)]
enum Step {
    #[default]
    //Untap,
    //Upkeep,
    Draw,
    MainPhase1,
    //Combat, // TODO? Split this up if needed
    //MainPhase2,
    End,
}
use Step::*;

type LandPlays = u8;

const INITIAL_LAND_PLAYS: LandPlays = 1;

#[derive(Clone, Debug)]
struct State {
    hand: Hand,
    board: Board,
    deck: Deck,
    land_plays: LandPlays,
    step: Step,
    turn_number: TurnNumber,
}

#[cfg(test)]
mod test_coverage_utils {
    #[cfg(not(tarpaulin_include))]
    pub fn uncovered_assert_eq<A: PartialEq<A> + core::fmt::Debug>(
        actual: A,
        expected: A,
        func: impl FnOnce() -> String,
    ) {
        assert_eq!(actual, expected, "{}", func());
    }

    #[macro_export]
    macro_rules! _a_eq {
        ($left:expr, $right:expr $(,)?) => {
            uncovered_assert_eq($left, $right, || { format!("") })
        };
        ($left:expr, $right:expr, $($arg:tt)+) => {
            uncovered_assert_eq($left, $right, || { format!($($arg)+)} )
        };
    }
    pub use _a_eq as a_eq;
}
#[cfg(test)]
use test_coverage_utils::*;

#[cfg(test)]
mod calculate_works {
    use super::*;

    const _0: Card = InsatiableAvarice;
    const _1: Card = SchemingSymmetry;
    const _2: Card = FeedTheSwarm;

    #[test]
    fn on_empty_deck() {
        assert!(
            matches!(calculate(NthDraw(0), &[]), Err(_),)
        );
    }

    #[test]
    fn on_too_small_but_non_empty_deck() {
        assert!(
            matches!(calculate(NthDraw(0), &[Swamp; 1]), Err(_),)
        );
    }

    #[test]
    fn on_too_large_deck() {
        assert!(
            matches!(calculate(NthDraw(0), &[Swamp; MAX_DECK_SIZE as usize + 1]), Err(_),)
        );
    }

    #[test]
    fn on_8_swamps() {
        let _8_swamps = [Swamp; 8];

        const specs: [Spec; 5] = [
            NthDraw(0),
            NthDraw(1),
            NthDraw(10),
            NthDraw(100),
            NthDraw(1_000),
        ];

        for spec in specs {
            let outcomes = calculate(spec, &_8_swamps).unwrap();

            for outcome in outcomes {
                let does_match = matches!(outcome, OutcomeAt{ outcome: Lose, ..});

                assert!(does_match);
            }
        }
    }

    #[test]
    fn on_60_swamps() {
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
            let outcomes = calculate(spec, &_60_swamps).unwrap();

            for outcome in outcomes {
                let does_match = matches!(
                    outcome,
                    OutcomeAt{ outcome: Lose, ..},
                );

                assert!(
                    does_match,
                );
            }
        }
    }

    #[test]
    fn on_8_non_lands() {
        let _8_non_lands = [
            InsatiableAvarice,
            InsatiableAvarice,
            InsatiableAvarice,
            InsatiableAvarice,
            SchemingSymmetry,
            SchemingSymmetry,
            SchemingSymmetry,
            SchemingSymmetry,
        ];

        const specs: [Spec; 5] = [
            NthDraw(0),
            NthDraw(1),
            NthDraw(10),
            NthDraw(100),
            NthDraw(1_000),
        ];

        for spec in specs {
            let outcomes = calculate(spec, &_8_non_lands).unwrap();

            for outcome in outcomes {
                let does_match = matches!(outcome, OutcomeAt{ outcome: Lose, ..});

                assert!(does_match);
            }
        }
    }

    #[test]
    fn on_8_swamps_and_non_basic_lands() {
        let _8_swamps_and_non_basic_lands = [
            MemorialToFolly,
            PhyrexianTower,
            TheDrossPits,
            BlastZone,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
        ];

        const specs: [Spec; 5] = [
            NthDraw(0),
            NthDraw(1),
            NthDraw(10),
            NthDraw(100),
            NthDraw(1_000),
        ];

        for spec in specs {
            let outcomes = calculate(spec, &_8_swamps_and_non_basic_lands).unwrap();

            for outcome in outcomes {
                let does_match = matches!(outcome, OutcomeAt{ outcome: Lose, ..});

                assert!(does_match);
            }
        }
    }
}

type Deck = Box<[Card]>;

fn draw(deck: Deck) -> Option<(Card, Deck)> {
    deck.split_first().map(|(c, d)| (c.clone(), d.into()))
}

fn nth_ordered(
    deck: &[Card],
    mut n: PermutationNumber
) -> Result<Deck, CalculateError> {
    let len = deck.len();
    let mut perm = nth_factorial_number(len, n)?;

    // re-adjust values to obtain the permutation
    // start from the end and check if preceding values are lower
    for i in (1..len).rev() {
        for j in (0..i).rev() {
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

fn nth_factorial_number(len: usize, mut n: PermutationNumber) -> Result<Box<[usize]>, CalculateError> {
    let mut num: Vec<usize> = Vec::with_capacity(len);

    if n > 0 {
        let mut place_value = 1;
        let mut place = 1;

        while place_value < n {
            place += 1;
            place_value *= place;
        }

        if place_value > n {
            place_value /= place;
            place -= 1;
        }

        while place > 0 {
            let mut digit = 0;
            while n >= place_value {
                digit += 1;
                n -= place_value;
            }
            num.push(digit);

            place_value /= place;
            place -= 1;
        }
    }

    // At this point we have just the non-zero digits.

    // All factorial numbers end with 0
    num.push(0);

    while num.len() < len {
        num.insert(0, 0);
    }

    if num.len() > len {
        return Err(PermutationNumberTooHigh)
    }

    Ok(num.into_boxed_slice())
}

#[cfg(test)]
mod nth_factorial_number_works {
    use super::*;

    macro_rules! b {
        ($e : expr) => ({
            let slice: &[_] = $e;
            Box::from(slice)
        })
    }

    #[test]
    fn on_these_explict_examples() {
        let EXPECTEDS: [Box<[usize]>; 6] = [
            b!(&[0, 0, 0]),
            b!(&[0, 1, 0]),
            b!(&[1, 0, 0]),
            b!(&[1, 1, 0]),
            b!(&[2, 0, 0]),
            b!(&[2, 1, 0]),
        ];

        assert_eq!(
            &[
                nth_factorial_number(3, 0).unwrap(),
                nth_factorial_number(3, 1).unwrap(),
                nth_factorial_number(3, 2).unwrap(),
                nth_factorial_number(3, 3).unwrap(),
                nth_factorial_number(3, 4).unwrap(),
                nth_factorial_number(3, 5).unwrap(),
            ],
            &EXPECTEDS,
        );
    }

    #[test]
    fn on_these_examples() {
        for len in 1..30 {
            for n in 0..30 {
                let actual = nth_factorial_number(len, n);
                let expected = nth_factorial_number_limited(len, n);

                a_eq!(actual, expected, "mismatch on {len}, {n}");
            }
        }
    }

    fn nth_factorial_number_limited(len: usize, mut n: PermutationNumber) -> Result<Box<[usize]>, CalculateError> {
        // https://stackoverflow.com/a/7919887
        let mut fact: Vec<_> = Vec::with_capacity(len);
        let mut perm: Vec<_> = Vec::with_capacity(len);

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

        dbg!(&perm);

        Ok(perm.into_boxed_slice())
    }
}