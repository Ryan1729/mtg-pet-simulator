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
        }
    );

    let mut outcomes = Vec::with_capacity(64);

    while let Some(state) = states.pop() {
        let mut results = calculate_step(state);

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
    state.turn_number += 1;

    while let Some((card, d)) = draw(state.deck) {
        state.hand.push(card);
        state.deck = d;

        todo!()

        // We'll need to choose between different decisions here.
        // Or we could try all of them!
        /* Given a hand, deck and board, we can come up with a list of all the options.
        Then each of the options will produce a new hand, deck and board, and thus another list of options
        eventually we get to an empty list of options, for each case, leaving us with a list of possible future board states.
        If any of them win, return that.
        The ones that lose don't create further options.
        The rest get to draw a card
        Gotta track turn number too

        So we should mkae this iterative I think, since combinatoric numbers are large enough we might want to paralellize
        Call `(Hand, Board, Deck, Turn Number)` `State`.
        We basically have State -> [Result<State, Outcome>]
        
        */
        // 
    }

    Box::from([
        Err(OutcomeAt {
            outcome: Lose,
            at: state.turn_number,
        })
    ])
}

/// 64k turns ought to be enough for anybody!
type TurnNumber = u16;

#[derive(Default)]
struct Board {
    // TODO
}

struct State {
    hand: Hand,
    board: Board,
    deck: Deck,
    turn_number: TurnNumber,
}

#[cfg(test)]
mod calculate_works {
    use super::*;

    const _0: Card = InsatiableAvarice;
    const _1: Card = SchemingSymmetry;
    const _2: Card = FeedTheSwarm;

    #[test]
    fn on_all_swamps() {
        let _60_swamps = [Swamp; 30];

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
    
    dbg!(&perm);

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
    let mut perm: Vec<usize> = Vec::with_capacity(len);
    
    if (n <= 0) {
        perm.push(0);
    } else {
        let mut placeValue = 1;
        let mut place = 1;
    
        while placeValue < n {
            place += 1;
            placeValue *= place;
        }
    
        if placeValue > n {
            placeValue /= place;
            place -= 1;
        }
    
        while place > 0 {
            let mut digit = 0;
            while n >= placeValue {
                digit += 1;
                n -= placeValue;
            }
            perm.push(digit);
    
            placeValue /= place;
            place -= 1;
        }
    }

    dbg!("short", &perm);

    while perm.len() < len {
        perm.push(0);
    }
    perm.reverse();

    dbg!("long", &perm);

    Ok(perm.into_boxed_slice())
}

#[cfg(test)]
mod nth_factorial_number_works {
    use super::*;

    #[test]
    fn on_these_examples() {
        for len in 1..30 {
            for n in 0..30 {
                assert_eq!(
                    nth_factorial_number(len, n),
                    nth_factorial_number_limited(len, n),
                    "mismatch on {len}, {n}"
                );
            }
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