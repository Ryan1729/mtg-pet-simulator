#![deny(unreachable_patterns)]
#![deny(unused_must_use)]
// #![deny(unused_variables)] // This makes todo! use annoying
#![allow(non_snake_case)]

#[cfg(test)]
use ntest::timeout;

use arena::Arena;
use card::Card::{self, *};
use equiv::{Equiv, LooseCmp};
use fast_hash::{HashSet, HashSetExt};
use mana::{ManaCost, ManaPool};
use permanent::{Permanent, TurnNumber, INITIAL_TURN_NUMBER};

use std::cmp::Ordering;
use std::collections::{BTreeSet, BTreeMap};
use std::iter::ExactSizeIterator;

type PermutationNumber = u128;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DrawSpec {
    //AllDraws,
    NthDraw(PermutationNumber),
}
use DrawSpec::*;

const NO_SHUFFLING: DrawSpec = NthDraw(0);

impl Default for DrawSpec {
    fn default() -> Self {
        NO_SHUFFLING
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum PetSpec {
    #[default]
    Goldfish,
}
use PetSpec::*;

impl PetSpec {
    #[cfg(test)]
    const ALL: [PetSpec; 1] = [
        Goldfish,
    ];
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TurnBoundsSpec {
    StopAt(TurnNumber),
}
use TurnBoundsSpec::*;

impl Default for TurnBoundsSpec {
    fn default() -> Self {
        TurnBoundsSpec::StopAt(10)
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Spec {
    pub draw: DrawSpec,
    pub pet: PetSpec,
    pub turn_bounds: TurnBoundsSpec,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Outcome {
    Lose,
    Win,
}
use Outcome::*;

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Debug)]
struct HeapWrapper(State);

impl Ord for HeapWrapper {
    fn cmp(&self, other: &Self) -> Ordering {
        // The BinaryHeap is a max-heap by default so reverse the order to put low opponents_life first
        other.0.opponents_life.cmp(&self.0.opponents_life)
            // reverse the order to put low turns first
            .then_with(|| other.0.turn_number.cmp(&self.0.turn_number))
    }
}

impl PartialOrd for HeapWrapper {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for HeapWrapper {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for HeapWrapper {}

#[cfg(test)]
mod heap_wrapper_works {
    use super::*;

    #[test]
    fn when_checking_equality() {
        let hand = Hand::default();
        let deck = Deck::default();

        assert_eq!(
            HeapWrapper(State::new(hand.clone(), deck.clone())),
            HeapWrapper(State::new(hand.clone(), deck.clone())),
        );
    }
}

pub fn calculate(spec: Spec, deck: &[Card]) -> Result<Outcomes, CalculateError> {
    if deck.len() > MAX_DECK_SIZE as _ {
        return Err(DeckTooLarge);
    }

    let ordered_deck = match spec.draw {
        NthDraw(n) => {
            nth_ordered(deck, n)
        }
    }?;

    let mut states = std::collections::BinaryHeap::with_capacity(64);

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

    let mut seen = HashSet::with_capacity_and_seed(64, 0xca55e77e);
    macro_rules! push_state {
        ($state: expr) => ({
            let s = $state;
            if !seen.contains(&s) {
                // Could probably just store the hash here
                // and avoid the clone.
                // Something like this maybe?
                // https://www.somethingsimilar.com/2012/05/21/the-opposite-of-a-bloom-filter/
                seen.insert(s.clone());
                states.push(HeapWrapper(s));
            }
        })
    }

    push_state!(State::new(hand, deck));

    match spec.pet {
        Goldfish => {
            let mut outcomes = Vec::with_capacity(64);

            while let Some(HeapWrapper(state)) = states.pop() {
                let results = calculate_step(state);

                for result in results.into_vec().into_iter() {
                    let result: Result<State, OutcomeAt> = result;
                    match result {
                        Ok(s) => {
                            match spec.turn_bounds {
                                StopAt(max_turn) => {
                                    if s.turn_number <= max_turn {
                                        push_state!(s);
                                    }
                                }
                            }
                        }
                        Err(outcome) => {
                            outcomes.push(outcome);
                        }
                    }
                }
            }

            return Ok(outcomes);
        }
    }
}

mod non_empty {
    #[derive(Debug)]
    pub struct OwnedSlice<A>(
        Box<[A]>
    );

    impl <A> OwnedSlice<A> {
        pub fn into_vec(self) -> Vec<A> {
            self.0.into_vec()
        }
    }

    impl <A> From<A> for OwnedSlice<A> {
        fn from(a: A) -> Self {
            Self(Box::from([a]))
        }
    }

    impl <A> From<(A, Vec<A>)> for OwnedSlice<A> {
        fn from((a, mut rest): (A, Vec<A>)) -> Self {
            rest.push(a);
            Self(rest.into())
        }
    }
}

type StepOutput = non_empty::OwnedSlice<Result<State, OutcomeAt>>;

fn calculate_step(mut state: State) -> StepOutput {
    macro_rules! one_path_forward {
        () => {
            return StepOutput::from(Ok(state))
        }
    }

    macro_rules! set_step {
        ($state: expr, $step: expr) => {
            $state.step = $step;
            $state.with_mana_pool(<_>::default());
        }
    }

    macro_rules! check_state_based_actions {
        () => {
            // TODO probably some fiddly rules about who the active player is?
            if state.opponents_life <= 0 {
                return StepOutput::from(
                    Err(OutcomeAt {
                        outcome: Win,
                        at: state.turn_number,
                    })
                );
            }
        }
    }

    macro_rules! main_phase {
        ($next_step: ident) => ({
            check_state_based_actions!();

            if state.land_plays > 0 {
                let mut land_indexes = BTreeMap::new();

                for (i, card) in state.hand.iter().enumerate() {
                    if card.is_land() && !land_indexes.contains_key(card) {
                        land_indexes.insert(card, i);
                    }
                }

                // TODO? Is it ever worth doing to not play a land if you can?
                let mut output = Vec::with_capacity(land_indexes.len());

                for (_key, i) in land_indexes.into_iter().rev() {
                    let (h, card) = remove(&state.hand, i).expect("land index to be valid");

                    output.push(Ok(State {
                        hand: h.to_vec(),
                        board: state.board.enter(Permanent::card(card, state.turn_number)),
                        land_plays: state.land_plays - 1,
                        ..state.clone()
                    }));
                }

                if let Some(s) = output.pop() {
                    return StepOutput::from((s, output));
                }
            }

            let mut output = Vec::with_capacity(state.hand.len());

            state.add_casting_states(&mut output);

            // TODO add more possible plays when there are any

            set_step!(state, $next_step);
            return StepOutput::from((Ok(state), output));
        })
    }

    match state.step {
        Untap => {
            check_state_based_actions!();

            for permanent in state.board.permanents_mut() {
                *permanent = permanent.untapped();
            }

            set_step!(state, Draw);

            one_path_forward!()
        }
        Draw => {
            check_state_based_actions!();

            if let Some((card, d)) = draw(state.deck) {
                state.deck = d;
                state.hand.push(card);
                set_step!(state, MainPhase1);

                one_path_forward!()
            } else {
                return StepOutput::from(
                    Err(OutcomeAt {
                        outcome: Lose,
                        at: state.turn_number,
                    })
                );
            }
        },
        MainPhase1 => {
            main_phase!(CombatDamage)
        }
        CombatDamage => {
            check_state_based_actions!();

            // For now, since we're only supporting Goldfish, we can get away with just always attacking, and not
            // implementing blocking. Thus, we can tap stuff here, even though it should be done in the declare
            // attackers step.
            let attackers = state.board.possible_attackers(state.turn_number);

//dbg!(&state.board, &attackers);
            let mut total_power = 0;

            for &index in attackers.into_iter() {
                let attacker = state.board.permanent_mut(index).expect("attacker index was bad");
                *attacker = attacker.tapped();

                let power = attacker.power().expect("attacker had no power");
                total_power += power;
            }

            state.opponents_life = state.opponents_life.saturating_sub(total_power.into());

            check_state_based_actions!();

            state.step = MainPhase2;

            one_path_forward!()
        }
        MainPhase2 => {
            main_phase!(End)
        }
        End => {
            check_state_based_actions!();

            state.turn_number += 1;
            set_step!(state, Step::default());
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

type IndexSet = u128;
const INDEX_SET_MAX_ELEMENTS: usize = IndexSet::BITS as _;

mod board {
    use arena::{Arena, ArenaVec};
    use card::Card;
    use equiv::{Equiv, LooseCmp};
    use fast_hash::{HashSet, HashSetExt};
    use mana::{ManaCost, ManaPool, SpendError};
    use permanent::{Permanent, PermanentKind, TurnNumber};

    use super::{Ability, Abilities, Effect, IndexSet, INDEX_SET_MAX_ELEMENTS, SpreeIter};

    use std::collections::{BTreeMap, BTreeSet};

    // Keep things private because we suspect we'll want lots of different
    // ways to query what is on the board, and that we will care about
    // making them fast. So, we expect to want to change the internals
    // later on, without needing to change all the usages of those
    // internals.
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub struct Board<'arena> {
        mana_pool: ManaPool,
        // We suspect we'll want like lands, creatures, etc. as ready to go collections
        permanents: ArenaVec<'arena, Permanent>,
    }

    impl Board<'arena> {
        pub fn in(arena: &'arena Arena) -> Self {
            let permanents = ArenaVec::with_capacity_in(0, arena);

            Self {
                mana_pool,
                permanents,
            }
        }
    }

    use std::cmp::Ordering;

    impl LooseCmp for Board<'_> {
        fn loose_cmp(&self, other: &Self) -> Ordering {
            let mut permanents = self.permanents.clone();
            permanents.sort();
            let mut other_permanents = other.permanents.clone();
            other_permanents.sort();

            let mut mana_pool = self.mana_pool;
            let mut other_mana_pool = other.mana_pool;

            // For each mana in the pool, untap any corresponding
            // basic lands.
            // TODO actually handle all mana colours
            'black: while mana_pool.black > 0 {
                for permanent in &mut permanents {
                    if permanent.kind().card() == card::Card::Swamp
                    && permanent.is_tapped() {
                        *permanent = permanent.untapped();
                        mana_pool.black -= 1;
                        continue 'black;
                    }
                }

                break
            }

            'other_black: while other_mana_pool.black > 0 {
                for permanent in &mut other_permanents {
                    if permanent.kind().card() == card::Card::Swamp
                    && permanent.is_tapped() {
                        *permanent = permanent.untapped();
                        other_mana_pool.black -= 1;
                        continue 'other_black;
                    }
                }

                break
            }

            mana_pool.cmp(&other_mana_pool).then_with(
                || {
                    Ord::cmp(&Equiv(permanents.as_slice()), &Equiv(other_permanents.as_slice()))
                }
            )
        }
    }

    #[cfg(test)]
    mod ord_for_equiv_board_works {
        use super::*;
        use card::Card::*;
        use permanent::{INITIAL_TURN_NUMBER};

        #[test]
        fn on_this_should_match_example() {
            let mut arena = Arena::with_capacity(128);

             let extra_tapped_swamp = Board {
                mana_pool: ManaPool {
                    black: 1,
                    colorless: 0,
                },
                permanents: arena::vec![
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(StarscapeCleric, INITIAL_TURN_NUMBER),
                    => &arena
                ],
            };

            let minimal_tapping = Board {
                mana_pool: ManaPool {
                    black: 0,
                    colorless: 0,
                },
                permanents: arena::vec![
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER),
                    Permanent::card(StarscapeCleric, INITIAL_TURN_NUMBER),
                    => &arena
                ],
            };

            assert_eq!(
                Equiv(&extra_tapped_swamp).cmp(&Equiv(&minimal_tapping)),
                Ordering::Equal
            );
        }

        #[test]
        fn on_these_permuted_examples() {
             let mut arena = Arena::with_capacity(128);

             let base = Board::default();

             let extra_tapped_swamp = Board {
                mana_pool: ManaPool {
                    black: 1,
                    colorless: 0,
                },
                permanents: arena::vec![
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(StarscapeCleric, INITIAL_TURN_NUMBER),
                    => &arena
                ],
            };

            let extra_tapped_swamp_permuted = Board {
                mana_pool: ManaPool {
                    black: 1,
                    colorless: 0,
                },
                permanents: arena::vec![
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(StarscapeCleric, INITIAL_TURN_NUMBER),
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    => &arena
                ],
            };

            let minimal_tapping = Board {
                mana_pool: ManaPool {
                    black: 0,
                    colorless: 0,
                },
                permanents: arena::vec![
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER),
                    Permanent::card(StarscapeCleric, INITIAL_TURN_NUMBER),
                    => &arena
                ],
            };

            let minimal_tapping_permuted = Board {
                mana_pool: ManaPool {
                    black: 0,
                    colorless: 0,
                },
                permanents: arena::vec![
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped(),
                    Permanent::card(StarscapeCleric, INITIAL_TURN_NUMBER),
                    Permanent::card(Swamp, INITIAL_TURN_NUMBER),
                    => &arena
                ],
            };

            assert_ne!(
                Equiv(&extra_tapped_swamp).cmp(&Equiv(&base)),
                Ordering::Equal
            );

            assert_ne!(
                Equiv(&extra_tapped_swamp_permuted).cmp(&Equiv(&base)),
                Ordering::Equal
            );

            assert_ne!(
                Equiv(&minimal_tapping).cmp(&Equiv(&base)),
                Ordering::Equal
            );

            assert_ne!(
                Equiv(&minimal_tapping_permuted).cmp(&Equiv(&base)),
                Ordering::Equal
            );

            // Equal section

            assert_eq!(
                Equiv(&extra_tapped_swamp).cmp(&Equiv(&extra_tapped_swamp_permuted)),
                Ordering::Equal
            );

            assert_eq!(
                Equiv(&minimal_tapping).cmp(&Equiv(&minimal_tapping_permuted)),
                Ordering::Equal
            );

            assert_eq!(
                Equiv(&extra_tapped_swamp).cmp(&Equiv(&minimal_tapping_permuted)),
                Ordering::Equal
            );

            assert_eq!(
                Equiv(&minimal_tapping).cmp(&Equiv(&extra_tapped_swamp_permuted)),
                Ordering::Equal
            );
        }
    }

    /// Returns `true` if `b1` is a preferred board over `b2`.
    pub fn preferred(b1: &Board, b2: &Board) -> bool {
        b1.mana_pool.is_empty() && !b2.mana_pool.is_empty()
    }

    fn push(arena: &Arena, slice: &[Permanent], element: Permanent) -> ArenaVec<'arena, Permanent> {
        let mut output = ArenaVec::with_capacity_in(slice.len() + 1, arena);

        for e in slice.iter() {
            output.push(e.clone());
        }
        output.push(element);

        output
    }

    fn set_at(slice: &[Permanent], index: PermanentIndex, element: Permanent) -> Vec<Permanent> {
        let mut output = slice.to_vec();

        if index < output.len() {
            output[index] = element;
        }

        output
    }

    impl Board {
        pub fn permanent(&self, index: PermanentIndex) -> Option<&Permanent> {
            self.permanents.get(index)
        }

        pub fn permanent_mut(&mut self, index: PermanentIndex) -> Option<&mut Permanent> {
            self.permanents.get_mut(index)
        }

        pub fn permanents_mut(&mut self) -> impl Iterator<Item = &mut Permanent> {
            self.permanents.iter_mut()
        }

        pub fn with_mana_pool(&self, mana_pool: ManaPool) -> Self {
            Self {
                mana_pool,
                ..self.clone()
            }
        }

        pub fn with_additional_mana(&self, additional_mana_pool: ManaPool) -> Result<Self, mana::AddError> {
            Ok(Self {
                mana_pool: self.mana_pool.add(additional_mana_pool)?,
                ..self.clone()
            })
        }

        #[must_use]
        pub fn enter(&self, arena: &Arena, permanent: Permanent) -> Self {
            Board {
                permanents: push(
                    arena,
                    self.permanents.slice(),
                    permanent,
                ),
                ..self.clone()
            }
        }
    }

    /// The index of the permanent on the board
    // TODO? Make this a generational index?
    pub type PermanentIndex = usize;

    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
    enum ManaAbilityKindSpec {
        //TapForWhite,
        //TapForBlue,
        TapForBlack,
        //TapForRed,
        //TapForGreen,
        TapForColorless,
        SacrificeCreatureForTwoBlack,
    }
    use ManaAbilityKindSpec::*;

    pub type CreatureIndex = usize;

    #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
    pub enum ManaAbilityKind {
        //TapForWhite,
        //TapForBlue,
        TapForBlack,
        //TapForRed,
        //TapForGreen,
        TapForColorless,
        SacrificeCreatureForTwoBlack(CreatureIndex),
    }

    static NO_MANA_ABILITIES: &[ManaAbilityKindSpec] = &[];
    static TAP_FOR_BLACK: &[ManaAbilityKindSpec] = &[TapForBlack];
    static PHYREXIAN_TOWER_ABILITIES: &[ManaAbilityKindSpec] = &[TapForColorless, SacrificeCreatureForTwoBlack];
    static TAP_FOR_COLORLESS: &[ManaAbilityKindSpec] = &[TapForColorless];

    fn mana_ability_kind_specs_for_card(card: Card) -> impl Iterator<Item = &'static ManaAbilityKindSpec> {
        use Card::*;
        match card {
            // Plains
            // Island
            Swamp
            // Mountian
            // Forest
            | HagraMauling
            | MemorialToFolly
            | TheDrossPits => TAP_FOR_BLACK.into_iter(),
            PhyrexianTower => PHYREXIAN_TOWER_ABILITIES.into_iter(),
            BlastZone
            | SceneOfTheCrime => TAP_FOR_COLORLESS.into_iter(),
            InsatiableAvarice
            | SchemingSymmetry
            | FeedTheSwarm
            | SignInBlood
            | StarscapeCleric
            | WishclawTalisman
            | CeaseAndDesist
            | HowlingMine
            | JetMedallion
            | MindStone
            | GrimTutor
            | HoodedBlightfang
            | NighthawkScavenger
            | ToxicDeluge
            | VitoThornOfTheDuskRose
            | BakeIntoAPie
            | EnduringTenacity
            | SheoldredTheApocalypse
            | ExquisiteBlood => NO_MANA_ABILITIES.into_iter(),
        }
    }

    /// This is exceeded in certain scenarios, but almost always this will be enough .
    const MANA_ABILITIES_COUNT_ESTIMATE: usize = 2;

    fn mana_abilities<'board>(
        board: &'board Board,
        permanent_index: PermanentIndex,
        permanent: &Permanent
    ) -> impl Iterator<Item = ManaAbility> + 'board {
        let permanent_kind = permanent.kind();

        match permanent_kind {
            PermanentKind::Card(card)
            | PermanentKind::Token(card) =>
                mana_ability_kind_specs_for_card(card)
                    .enumerate()
                    .flat_map(move |(ability_index, kind_spec)|
                        match kind_spec {
                            //TapForWhite => ManaAbilityKind::TapForWhite,
                            //TapForBlue => ManaAbilityKind::TapForBlue,
                            TapForBlack => vec![
                                ManaAbility{
                                    kind: ManaAbilityKind::TapForBlack,
                                    permanent_index,
                                    permanent_kind: permanent_kind.clone(),
                                    ability_index: ability_index.try_into().unwrap(),
                                }
                            ].into_iter(),
                            //TapForRed => ManaAbilityKind::TapForRed,
                            //TapForGreen => ManaAbilityKind::TapForGreen,
                            TapForColorless => vec![
                                ManaAbility{
                                    kind: ManaAbilityKind::TapForColorless,
                                    permanent_index,
                                    permanent_kind: permanent_kind.clone(),
                                    ability_index: ability_index.try_into().unwrap(),
                                }
                            ].into_iter(),
                            SacrificeCreatureForTwoBlack => {
                                board.sacrificeable_creatures()
                                    .iter()
                                    .map(|index| {
                                        ManaAbility{
                                            kind: ManaAbilityKind::SacrificeCreatureForTwoBlack(*index),
                                            permanent_index,
                                            permanent_kind: permanent_kind.clone(),
                                            ability_index: ability_index.try_into().unwrap(),
                                        }
                                    })
                                    // TODO? Avoid this allocation that's here to avoid dealing with
                                    // the closure type?
                                    .collect::<Vec<_>>()
                                    .into_iter()
                            },
                        }
                    ),
        }
    }

    /// The index of the mana abilty on the given permanent
    type ManaAbilityIndex = u8;

    /// A Mana Ability that can be played on the battlefield, including the costs.
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ManaAbility {
        kind: ManaAbilityKind,
        ability_index: ManaAbilityIndex,
        permanent_kind: PermanentKind,
        permanent_index: PermanentIndex,
    }

    type ApplyManaAbilityError = ();

    pub fn apply_mana_ability(board: &Board, mana_ability: &ManaAbility) -> Result<Board, ApplyManaAbilityError> {
        macro_rules! tap_for {
            ($board: ident $(,)? $pool: expr) => {
                $board.tap_permanent_at(mana_ability.permanent_index)
                    .and_then(|b| b.with_additional_mana($pool))
            }
        }

        match mana_ability.kind {
            //ManaAbilityKind::TapForWhite => tap_for!(board, ManaPool { white: 1, ..ManaPool::default() }),
            //ManaAbilityKind::TapForBlue => tap_for!(board, ManaPool { blue: 1, ..ManaPool::default() }),
            ManaAbilityKind::TapForBlack => tap_for!(board, ManaPool { black: 1, ..ManaPool::default() }),
            //ManaAbilityKind::TapForRed => tap_for!(board, ManaPool { red: 1, ..ManaPool::default() }),
            //ManaAbilityKind::TapForGreen => tap_for!(board, ManaPool { green: 1, ..ManaPool::default() }),
            ManaAbilityKind::TapForColorless => tap_for!(board, ManaPool { colorless: 1, ..ManaPool::default() }),
            ManaAbilityKind::SacrificeCreatureForTwoBlack(creature_index) => {
                board.sacrifice_creature_at(creature_index)
                    .and_then(|b| {
                        tap_for!(b, ManaPool { black: 2, ..ManaPool::default() })
                    })
            },
        }
    }

    mod mana_abilities_set {
        use super::*;

        type ManaAbilitiesSetInner = HashSet<ManaAbility>;

        #[derive(Debug, Default)]
        pub struct ManaAbilitiesSet {
            set: ManaAbilitiesSetInner,
        }

        impl PartialEq for ManaAbilitiesSet {
            fn eq(&self, other: &Self) -> bool {
                let mut self_vec: Vec<_> = self.set.iter().collect();
                self_vec.sort();

                let mut other_vec: Vec<_> = self.set.iter().collect();
                other_vec.sort();

                self_vec == other_vec
            }
        }

        impl <const N: usize> From<[ManaAbility; N]> for ManaAbilitiesSet {
            fn from(arr: [ManaAbility; N]) -> Self {
                let mut set = ManaAbilitiesSetInner::with_capacity_and_seed(arr.len(), Self::SEED);

                for e in arr {
                    set.insert(e);
                }

                Self { set }
            }
        }

        impl ManaAbilitiesSet {
            const SEED: usize = 0xba5ed;

            pub fn new() -> Self {
                Self{
                    set: ManaAbilitiesSetInner::with_capacity_and_seed(16, Self::SEED),
                }
            }

            pub fn with_capacity_and_seed(capacity: usize, seed: usize) -> Self {
                Self{
                    set: ManaAbilitiesSetInner::with_capacity_and_seed(16, Self::SEED),
                }
            }

            pub fn unordered_iter(&self) -> impl Iterator<Item = &ManaAbility> {
                self.set.iter()
            }

            pub fn ordered_iter(&self) -> impl Iterator<Item = ManaAbility> {
                let mut vec: Vec<_> = self.set.iter().cloned().collect();

                vec.sort();

                vec.into_iter()
            }

            pub fn len(&self) -> usize {
                self.set.len()
            }

            pub fn insert(&mut self, value: ManaAbility) {
                self.set.insert(value);
            }

            pub fn clear(&mut self) {
                self.set.clear();
            }
        }
    }
    use mana_abilities_set::ManaAbilitiesSet;

    type ManaAbilitiesKey = (ManaAbilityKind, PermanentKind);

    type AllManaAbilities = Box<[ManaAbility]>;

    type ManaAbilitiesKeysHash = u64;
    type ManaAbilitiesSubsetsSeen = HashSet<Vec<ManaAbilitiesKey>>;

    // Making this streaming iterator instead of a flat list, to avoid using 2^n memory, seems worth it.
    #[derive(Debug)]
    pub struct ManaAbilitiesSubsets {
        current_set: ManaAbilitiesSet,
        index_set: IndexSet,
        all: AllManaAbilities,
        last_advance_was_skipped: bool,
        // This bit uses 2^n memory again worst case, but currently seems like the best way to
        // eliminate some duplicates we want to eliminate.
        seen: ManaAbilitiesSubsetsSeen,
    }

    impl From<AllManaAbilities> for ManaAbilitiesSubsets {
        fn from(all: AllManaAbilities) -> Self {
            let len = all.len();

            Self {
                current_set: ManaAbilitiesSet::with_capacity_and_seed(len, 0x5ca1ab1e),
                index_set: <_>::default(),
                all,
                last_advance_was_skipped: <_>::default(),
                seen: ManaAbilitiesSubsetsSeen::with_capacity_and_seed(len, 0xb0a710ad),
            }
        }
    }

    // Written aiming to be compatible with streaming_iterator but currently
    // not bothering to actually depend on that crate until we have
    // a reason to.
    impl ManaAbilitiesSubsets {
        fn fully_advanced(&self) -> bool {
            if self.all.is_empty() {
                return self.index_set > 0;
            }

            let log_2_or_0 =
                if self.index_set == 0 {
                    0
                } else {
                    self.index_set.ilog2() as usize
                };

            log_2_or_0 >= self.all.len()
        }

        fn advance(&mut self) {
            loop {
                if self.fully_advanced() {
                    self.last_advance_was_skipped = true;
                    return
                }

                self.current_set.clear();

                let mut used = self.index_set;

                let mut index = 0;

                while used > 0 {
                    if used & 1 != 0 {
                        self.current_set.insert(self.all[index].clone());
                    }
                    index += 1;
                    used >>= 1;
                }

                self.index_set += 1;

                let mut seen_buffer = Vec::with_capacity(self.current_set.len());

                for el in self.current_set.unordered_iter(){
                    seen_buffer.push((el.kind, el.permanent_kind));
                }

                seen_buffer.sort();

                let was_newly_inserted = self.seen.insert(seen_buffer.clone());

                if was_newly_inserted {
                    return
                }
                // else loop around and try the next one
            }
        }

        fn get(&self) -> Option<&ManaAbilitiesSet> {
            if self.last_advance_was_skipped {
                None
            } else {
                Some(&self.current_set)
            }
        }

        pub fn next(&mut self) -> Option<&ManaAbilitiesSet> {
            self.advance();
            self.get()
        }
    }

    #[cfg(test)]
    mod mana_ability_subsets_works {
        use super::*;

        const ABILITY: ManaAbility = ManaAbility {
            kind: ManaAbilityKind::TapForBlack,
            ability_index: 0,
            permanent_kind: PermanentKind::Card(
                card::Card::Swamp,
            ),
            permanent_index: 0,
        };

        const TWO_ABILITIES: [ManaAbility; 2] =
            [
                ManaAbility {
                    kind: ManaAbilityKind::TapForColorless,
                    ability_index: 0,
                    permanent_kind: PermanentKind::Card(
                        card::Card::PhyrexianTower,
                    ),
                    permanent_index: 0,
                },
                ManaAbility {
                    kind: ManaAbilityKind::SacrificeCreatureForTwoBlack(0),
                    ability_index: 1,
                    permanent_kind: PermanentKind::Card(
                        card::Card::PhyrexianTower,
                    ),
                    permanent_index: 0,
                },
            ];

        #[test]
        fn on_a_zero_element_all() {
            let mut iter = ManaAbilitiesSubsets::from(
                AllManaAbilities::from([])
            );

            let empty = iter.next();

            assert_eq!(empty, Some(&ManaAbilitiesSet::new()));

            let none = iter.next();

            assert_eq!(none, None);
        }

        #[test]
        fn on_a_one_element_all() {
            let mut iter = ManaAbilitiesSubsets::from(
                AllManaAbilities::from([ABILITY])
            );

            // Subsets of a one element set with
            // an element called A include just
            // [], and [A]

            let empty = iter.next();

            assert_eq!(empty, Some(&ManaAbilitiesSet::new()));

            let full = iter.next();

            assert_eq!(full, Some(&([ABILITY].into())));

            let none = iter.next();

            assert_eq!(none, None);
        }

        #[test]
        fn on_a_two_element_all() {
            let mut iter = ManaAbilitiesSubsets::from(
                AllManaAbilities::from(TWO_ABILITIES)
            );

            // Subsets of a two element set with
            // elements called A and B include
            // [], [A], [B], [A, B]

            let empty = iter.next();

            assert_eq!(empty, Some(&ManaAbilitiesSet::new()));

            let first = iter.next();

            assert_eq!(first, Some(&([TWO_ABILITIES[0].clone()].into())));

            let second = iter.next();

            assert_eq!(second, Some(&([TWO_ABILITIES[1].clone()].into())));

            let both = iter.next();

            assert_eq!(both, Some(&(TWO_ABILITIES.clone().into())));

            let none = iter.next();

            assert_eq!(none, None);
        }

        #[test]
        fn on_a_case_where_we_should_eliminate_some_redundant_elements() {
            const ALL: [ManaAbility; 4] =
                [
                    ManaAbility {
                        kind: ManaAbilityKind::TapForBlack,
                        ability_index: 0,
                        permanent_kind: PermanentKind::Card(
                            card::Card::Swamp,
                        ),
                        permanent_index: 0,
                    },
                    ManaAbility {
                        kind: ManaAbilityKind::TapForBlack,
                        ability_index: 0,
                        permanent_kind: PermanentKind::Card(
                            card::Card::HagraMauling,
                        ),
                        permanent_index: 1,
                    },
                    ManaAbility {
                        kind: ManaAbilityKind::TapForBlack,
                        ability_index: 0,
                        permanent_kind: PermanentKind::Card(
                            card::Card::HagraMauling,
                        ),
                        permanent_index: 2,
                    },
                    ManaAbility {
                        kind: ManaAbilityKind::TapForBlack,
                        ability_index: 0,
                        permanent_kind: PermanentKind::Card(
                            card::Card::Swamp,
                        ),
                        permanent_index: 3,
                    },
                ];

            const ALL_SUBSET_COUNT: usize = 1 << ALL.len();

            let mut iter = ManaAbilitiesSubsets::from(
                AllManaAbilities::from(ALL)
            );

            let mut count = 0;
            while let Some(_) = iter.next() {
                count += 1;
            }

            // We should skip at least some in this case
            assert_ne!(count, ALL_SUBSET_COUNT);

            // What it turned out to be
            assert_eq!(count, 9);
        }

        #[test]
        fn on_this_large_case() {
            const K: usize = 16;
            let mut all = Vec::with_capacity(K);

            for i in 0..K {
                all.push(
                    ManaAbility {
                        kind: ManaAbilityKind::TapForBlack,
                        ability_index: 0,
                        permanent_kind: PermanentKind::Card(
                            card::Card::Swamp,
                        ),
                        permanent_index: i,
                    }
                )
            }

            let all_subset_count: usize = 1 << all.len();

            let mut iter = ManaAbilitiesSubsets::from(
                AllManaAbilities::from(all)
            );

            let mut count = 0;
            while let Some(_) = iter.next() {
                count += 1;
            }

            // We should skip at least some in this case
            assert_ne!(count, all_subset_count);

            // What it turned out to be
            assert_eq!(count, 17);
        }
    }

    pub fn mana_ability_subsets(board: &Board) -> ManaAbilitiesSubsets {
        let capacity_estimate = board.permanents.len() * MANA_ABILITIES_COUNT_ESTIMATE;
        let mut all_mana_abilities = Vec::with_capacity(capacity_estimate);

        for (i, p) in board.permanents.iter().enumerate() {
            for a in mana_abilities(board, i, p) {
                all_mana_abilities.push(a);
            }
        }

        let len = all_mana_abilities.len();

        // If this limit ever gets reached, we can instead use an arbitrary length bitset
        // with the same overall interface, including addition.
        assert!(len <= INDEX_SET_MAX_ELEMENTS, "Too many mana abilities to fit subset indexes in 128 bits");

        ManaAbilitiesSubsets::from(
            AllManaAbilities::from(all_mana_abilities)
        )
    }

    /// A `ManaAbilityKey` represents everything about a given mana ability that
    /// would be relevant when making a decision. So two different land types
    /// should have different keys, two identical untapped Swamps should have
    /// the same key, but a Swamp enchanted with a Kudzu should have a different key.
    /// Two different mana abilities on the same permanent should also have different keys
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ManaAbilityKey {
        kind: PermanentKind,
        index: ManaAbilityIndex,
        // TODO other fields as needed
    }

    impl From<&ManaAbility> for ManaAbilityKey {
        fn from(ManaAbility{ permanent_kind, ability_index, .. }: &ManaAbility) -> Self {
            Self {
                kind: permanent_kind.clone(),
                index: *ability_index,
            }
        }
    }

    impl From<ManaAbility> for ManaAbilityKey {
        fn from(ability: ManaAbility) -> Self {
            From::from(&ability)
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    enum ManaAbilityKeyKind {
        Card(Card),
        Token(Card),
    }

    impl From<&PermanentKind> for ManaAbilityKeyKind{
        fn from(kind: &PermanentKind) -> Self {
            match kind {
                PermanentKind::Card(card) => Self::Card(*card),
                PermanentKind::Token(card) => Self::Token(*card),
            }
        }
    }

    impl From<PermanentKind> for ManaAbilityKeyKind {
        fn from(kind: PermanentKind) -> Self {
            From::from(&kind)
        }
    }

    type ManaAbilityCount = u8;
    pub type ManaAbilityKeySet = BTreeMap<ManaAbilityKey, ManaAbilityCount>;

    pub fn to_key_set(mana_abilities: &ManaAbilitiesSet) -> ManaAbilityKeySet {
        let mut output = ManaAbilityKeySet::new();

        for ability in mana_abilities.unordered_iter() {
            let key = ManaAbilityKey {
                kind: ability.permanent_kind,
                index: ability.ability_index,
            };

            *output.entry(key).or_insert(0) += 1;
        }

        output
    }

    #[cfg(test)]
    mod to_key_set_works {
        use super::*;
        use card::Card::*;

        const SWAMPS: [ManaAbility; 2] =
        [
            ManaAbility {
                kind: ManaAbilityKind::TapForBlack,
                ability_index: 0,
                permanent_kind: PermanentKind::Card(
                    Swamp,
                ),
                permanent_index: 0,
            },
            ManaAbility {
                kind: ManaAbilityKind::TapForBlack,
                ability_index: 0,
                permanent_kind: PermanentKind::Card(
                    Swamp,
                ),
                permanent_index: 1,
            },
        ];

        #[test]
        fn on_these_examples() {
            let set_a = ManaAbilitiesSet::from([SWAMPS[0].clone()]);
            let set_b = ManaAbilitiesSet::from([SWAMPS[1].clone()]);
            let set_ab = ManaAbilitiesSet::from([SWAMPS[0].clone(), SWAMPS[1].clone()]);

            let key_a = to_key_set(&set_a);
            let key_b = to_key_set(&set_b);
            let key_ab = to_key_set(&set_ab);

            // Two instances of the same mana ability should be the same
            assert_eq!(key_b, key_a);
            // Two mana abilities should be different than just one.
            assert_ne!(key_ab, key_a);
        }
    }

    fn remove(slice: &[Permanent], index: usize) -> Option<(Box<[Permanent]>, Permanent)> {
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

    type TapPermanentError = ();

    impl Board {
        pub fn cast_options(&self, card: Card) -> impl ExactSizeIterator<Item = Ability> {
            use Card::*;
            // Note: only provide the options that are castable via the mana_pool on self
            match card {
                TheDrossPits
                | Swamp
                | BlastZone
                | MemorialToFolly
                | PhyrexianTower => Abilities::Empty,
                SignInBlood => {
                    Abilities::FixedLength(
                        vec![
                            Ability {
                                mana_cost: ManaCost {
                                    black: 2,
                                    ..ManaCost::default()
                                },
                                creature_cost: 0,
                                effects: vec![Effect::TargetPlayerDrawsTwoCardsLosesTwoLife],
                            },
                        ].into_iter()
                    )
                },
                SchemingSymmetry => {
                    Abilities::FixedLength(
                        vec![
                            Ability {
                                mana_cost: ManaCost {
                                    black: 1,
                                    ..ManaCost::default()
                                },
                                creature_cost: 0,
                                effects: vec![Effect::SchemingSymmetry],
                            },
                        ].into_iter()
                    )
                },
                InsatiableAvarice => Abilities::Spree(
                    SpreeIter::new(
                        self.mana_pool.clone(),
                        ManaCost {
                            black: 1,
                            ..ManaCost::default()
                        },
                        &[
                            Ability {
                                mana_cost: ManaCost {
                                    colorless: 2,
                                    ..ManaCost::default()
                                },
                                creature_cost: 0,
                                effects: vec![Effect::SearchForCardTopOfDeckShuffle],
                            },
                            Ability {
                                mana_cost: ManaCost {
                                    black: 2,
                                    ..ManaCost::default()
                                },
                                creature_cost: 0,
                                effects: vec![Effect::TargetPlayerDrawsThreeCardsLosesThreeLife],
                            },
                        ]
                    )
                ),
                StarshapeCleric => {
                    Abilities::FixedLength(
                        vec![
                            Ability {
                                mana_cost: ManaCost {
                                    generic: 1,
                                    black: 1,
                                    ..ManaCost::default()
                                },
                                creature_cost: 0,
                                effects: vec![Effect::HandToBattlefield(StarshapeCleric)],
                            },
                            Ability {
                                mana_cost: ManaCost {
                                    generic: 3,
                                    black: 2,
                                    ..ManaCost::default()
                                },
                                creature_cost: 0,
                                effects: vec![Effect::Offspring(StarshapeCleric)],
                            },
                        ].into_iter()
                    )
                }
                //_ => {
                    //todo!("cast_options for {card:?}");
                //},
            }
        }

        pub fn sacrifice_creatures(&self, creature_count: super::CreatureCount) -> Result<impl Iterator<Item = Self>, super::SacrificeCreaturesError> {
            let sacrificeable_creatures = self.sacrificeable_creatures();
            if (creature_count as usize) > sacrificeable_creatures.len() {
                return Err(())
            }

            let mut subsets = super::length_n_subsets(&sacrificeable_creatures, creature_count);

            let mut output = Vec::with_capacity(creature_count as usize * 3 /* probably not a good estimate! */);

            while let Some(subset) = subsets.next() {
                let mut state = self.clone();

                for index in subset.into_iter().rev() {
                    state = state.sacrifice_creature_at(*index)?;
                }

                output.push(state);
            }

            Ok(output.into_iter())
        }

        fn sacrifice_creature_at(&self, permanent_index: PermanentIndex) -> Result<Self, super::SacrificeCreaturesError> {
            if let Some(to_sac) = self.permanents.get(permanent_index) {
                if to_sac.is_a_creature() {
                    // TODO: Tracking the graveyard
                    // TODO: On-sacrifice triggers if we ever need them
                    let Some((permanents, _)) = remove(
                        &self.permanents,
                        permanent_index,
                    ) else {
                        return Err(())
                    };

                    Ok(Self {
                        permanents: permanents.to_vec(),
                        ..self.clone()
                    })
                } else {
                    Err(())
                }
            } else {
                Err(())
            }
        }

        /// Taps the given permanent as a cost. Returns an `Err` if it is already tapped, or if there is
        /// no permanent at that index.
        fn tap_permanent_at(&self, permanent_index: PermanentIndex) -> Result<Self, TapPermanentError> {
            if let Some(old) = self.permanents.get(permanent_index) {
                if old.is_tapped() {
                    Err(())
                } else {
                    Ok(Self {
                        permanents: set_at(
                            &self.permanents,
                            permanent_index,
                            old.tapped(),
                        ),
                        ..self.clone()
                    })
                }
            } else {
                Err(())
            }
        }

        pub fn creatures(&self) -> Box<[PermanentIndex]> {
            let len = self.permanents.len();

            let mut output = Vec::with_capacity(len / 2);

            for i in 0..len {
                if self.permanents[i].is_a_creature() {
                    output.push(i);
                }
            }

            output.into()
        }

        pub fn possible_attackers(&self, current: TurnNumber) -> Box<[PermanentIndex]> {
            let len = self.permanents.len();

            let mut output = Vec::with_capacity(len / 2);

            for i in 0..len {
                if self.permanents[i].can_attack(current) {
                    output.push(i);
                }
            }

            output.into()
        }

        pub fn sacrificeable_creatures(&self) -> Box<[PermanentIndex]> {
            // There are creatures that can't be sacrificed, and effects like "can't cause you to sacrifice permanents".
            // So this won't necessarily always be just all creatures
            self.creatures()
        }

        pub fn spend(&self, mana_cost: ManaCost) -> Result<impl Iterator<Item = Self> + '_, SpendError> {
            let mana_pools = self.mana_pool.spend(mana_cost)?;

            Ok(mana_pools.map(|mana_pool| self.with_mana_pool(mana_pool)))
        }
    }
}
use board::Board;


#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum Step {
    #[default]
    Untap,
    //Upkeep,
    Draw,
    MainPhase1,
    //BeginningOfCombat,
    //DeclareAttackers,
    //DeclareBlockers,
    CombatDamage,
    //EndOfCombat
    MainPhase2,
    End,
}
use Step::*;

type LandPlays = u8;

const INITIAL_LAND_PLAYS: LandPlays = 1;

type CardIndex = usize;
// Maybe something like this later, with additional variants for
// things like flashback.
//type HandIndex = usize;
//enum CardIndex {
    //Hand(HandIndex)
//}


#[derive(Debug)]
enum AttemptToCastError {
    NoCard,
    NotEnoughMana,
}

type Life = i16;

const INITIAL_LIFE: Life = 20;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct State {
    hand: Hand,
    board: Board,
    deck: Deck,
    land_plays: LandPlays,
    step: Step,
    turn_number: TurnNumber,
    opponents_life: Life,
}

impl State {
    fn new(hand: Hand, deck: Deck) -> Self {
        State {
            hand,
            board: Board::default(),
            deck,
            turn_number: INITIAL_TURN_NUMBER,
            step: Step::default(),
            land_plays: INITIAL_LAND_PLAYS,
            opponents_life: INITIAL_LIFE,
        }
    }

    fn with_board(&self, board: Board) -> Self {
        Self {
            board,
            ..self.clone()
        }
    }

    fn with_mana_pool(&self, mana_pool: ManaPool) -> Self {
        self.with_board(self.board.with_mana_pool(mana_pool))
    }

    fn with_additional_mana(&self, additional_mana_pool: ManaPool) -> Result<Self, mana::AddError> {
        self.board.with_additional_mana(additional_mana_pool)
            .map(|b| self.with_board(b))
    }

    fn attempt_to_cast(
        &self,
        arena: &Arena,
        card_index: CardIndex,
    ) -> Result<impl Iterator<Item = Self>, AttemptToCastError> {
        use AttemptToCastError::*;
        use arena::{Arena, ArenaVec};

        let card = *self.hand.get(card_index).ok_or(NoCard)?;

        let cast_options = self.cast_options(card);

        let mut output: Vec<Self> = Vec::with_capacity(/* Not a realy great bound */ cast_options.len());

        for cast_option in cast_options {
            let Ok(new_boards) = self.board.spend(cast_option.mana_cost) else {
                continue
            };

            for new_board in new_boards {
                let new_state = self.with_board(new_board);

                let Ok(new_states) =
                    new_state
                        .sacrifice_creatures(cast_option.creature_cost) else {
                    continue
                };

                for unmapped_state in new_states {
                    // We don't want all the states between the individual effects!
                    // But, we do want to allow multiple states to be returned from each
                    // stage, in case there are choices as part of them. To summarize,
                    // we only want the possible end states after the effect list is done,
                    // but we do want all of them!
                    // Thinking through Insatiable Avarice is probably a good example for this
                    let effect_count = cast_option.effects.len();

                    let mut mapped_states = ArenaVec::with_capacity_in(effect_count, &arena);
                    mapped_states.push(unmapped_state);
                    for effect in cast_option.effects.iter() {
                        let mut temp = ArenaVec::with_capacity_in(effect_count, &arena);
                        for s in mapped_states.into_iter() {
                            let applied: Result<std::vec::IntoIter<Self>, ()> = s.apply_effect(*effect);
                            temp.extend(applied.unwrap_or_else(|_| vec![s].into_iter()));
                        }
                        // Temp will get cleaned up when the arena is
                        mapped_states = temp;
                    }

                    output.extend(mapped_states.into_iter());
                }
            }
        }

        if output.is_empty() {
            Err(NotEnoughMana)
        } else {
            Ok(output.into_iter())
        }
    }
}

type CreatureCount = u8;

#[derive(Copy, Clone, Debug)]
enum Effect {
    // TODO? Make a NonEmptyManaPool? Is Add 0 mana something we want to represent?
    AddMana(ManaPool),
    SearchForCardTopOfDeckShuffle,
    SchemingSymmetry,
    TargetPlayerDrawsTwoCardsLosesTwoLife,
    TargetPlayerDrawsThreeCardsLosesThreeLife,
    HandToBattlefield(Card),
    Offspring(Card),
}

type EffectError = ();

type CardSet = BTreeSet<Card>;

impl LooseCmp for State {
    fn loose_cmp(&self, other: &Self) -> Ordering {
        macro_rules! cmp_ret {
            ($field: ident) => {
                match self.$field.cmp(&other.$field) {
                    Ordering::Equal => {},
                    other => return other,
                }
            }
        }
        cmp_ret!(hand);
        cmp_ret!(land_plays);
        cmp_ret!(step);
        cmp_ret!(turn_number);
        cmp_ret!(opponents_life);
        match self.deck.len().cmp(&other.deck.len()) {
            Ordering::Equal => {},
            other => return other,
        }

        Equiv(&self.board).cmp(&Equiv(&other.board))
    }
}

mod equiv_state_works {
    use super::*;

    #[test]
    fn on_this_found_pair() {
        let hand: Hand = vec![
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            Swamp,
            Swamp,
            Swamp,
        ].into();

        let deck: Deck = vec![
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
        ].into();

        let mut arena = Arena::with_capacity(128);

        let board1 = Board::default()
            .enter(&arena, Permanent::card(Swamp, 0).tapped())
            .enter(&arena, Permanent::card(Swamp, 1).tapped())
            .enter(&arena, Permanent::card(Swamp, 2).tapped())
            .enter(&arena, Permanent::card(StarscapeCleric, 2))
            .with_mana_pool(
                ManaPool {
                    black: 1,
                    colorless: 0,
                }
            );

        let board2 = Board::default()
            .enter(&arena, Permanent::card(Swamp, 0).tapped())
            .enter(&arena, Permanent::card(Swamp, 1).tapped())
            .enter(&arena, Permanent::card(Swamp, 2))
            .enter(&arena, Permanent::card(StarscapeCleric, 2));

        let s1 = Equiv(State {
            hand: hand.clone(),
            board: board1,
            deck: deck.clone(),
            land_plays: 0,
            step: MainPhase1,
            turn_number: 2,
            opponents_life: 20,
        });

        let s2 = Equiv(State {
            hand,
            board: board2,
            deck,
            land_plays: 0,
            step: MainPhase1,
            turn_number: 2,
            opponents_life: 20,
        });

        assert_eq!(s1, s2);
    }

    #[test]
    fn when_binary_searching() {
        let mut arena = Arena::with_capacity(128);

        let needle = Equiv(
            Board::default()
            .with_mana_pool(ManaPool {
                black: 1,
                colorless: 0,
            })
            .enter(&arena, Permanent::card(Swamp, 0).tapped())
            .enter(&arena, Permanent::card(Swamp, 1).tapped())
            .enter(&arena, Permanent::card(Swamp, 2).tapped())
            .enter(&arena, Permanent::card(StarscapeCleric, 2))
        );

        let haystack = vec![
            Equiv(
                Board::default()
                .enter(&arena, Permanent::card(Swamp, 0).tapped())
                .enter(&arena, Permanent::card(Swamp, 1).tapped())
                .enter(&arena, Permanent::card(Swamp, 2))
                .enter(&arena, Permanent::card(StarscapeCleric, 2))
            )
        ];

        assert_eq!(
            needle.binary_search_in(&haystack),
            Ok(0)
        );
    }
}

impl State {
    fn add_casting_states(&self, output: &mut Vec<Result<Self, OutcomeAt>>) {
        let mut castable_card_indexes = Vec::with_capacity(self.hand.len());
        for (card_index, card) in self.hand.iter().enumerate() {
            if card.is_castable() {
                castable_card_indexes.push(card_index);
            }
        }

        /// Returns `true` if `s1` is a preferred game state over `s2`.
        fn preferred(s1: &State, s2: &State) -> bool {
            board::preferred(&s1.board, &s2.board)
        }

        let mut new_states = Vec::with_capacity(16);

        // TODO: We could filter out duplicates, and also use the set
        // of cards to restrict the mana_spends we consider, though
        // that may or may not be faster than trying them all quickly
        if !castable_card_indexes.is_empty() {
            let mut all_mana_ability_subsets = board::mana_ability_subsets(&self.board);

            let mut seen_mana_abilities: HashSet<_> = HashSet::default();

            while let Some(mana_abilities) = all_mana_ability_subsets.next() {
                let key = board::to_key_set(&mana_abilities);

                if seen_mana_abilities.contains(&key) {
                    // Avoid bothering to track identical options
                    // like 6 different orders for 3 Swamps.
                    continue
                }

                seen_mana_abilities.insert(key);

                let mut current_board = self.board.clone();

                // TODO? is it worth it to avoid doing all the work
                // of calling apply_mana_ability and doing all these loops up front by making this a custom iterator?
                for mana_ability in mana_abilities.ordered_iter() {
                    if let Ok(board) = board::apply_mana_ability(&current_board, &mana_ability) {
                        current_board = board;
                    }
                }

                let spend_state = State {
                    board: current_board,
                    ..self.clone()
                };

                let arena = Arena::with_capacity(/* Not a realy great bound */ 1024);

                for card_index in &castable_card_indexes {
                    match spend_state.attempt_to_cast(&arena, *card_index) {
                        Ok(cast_states) => {
                            for unwrapped_cast_state in cast_states {
                                let cast_state = Equiv(unwrapped_cast_state);

                                match cast_state.binary_search_in(&new_states) {
                                    Ok(replace_index) => {
                                        if preferred(&cast_state.0, &new_states[replace_index].0) {
                                            new_states[replace_index] = cast_state;
                                        }
                                    }
                                    Err(insert_index) => {
                                        new_states.insert(insert_index, cast_state);
                                    }
                                }
                            }
                        },
                        Err(_) => {}
                    }
                }
            }
        }

        for new_state in new_states {
            output.push(Ok(new_state.0));
        }
    }
}

#[cfg(test)]
mod add_casting_states_works {
    use super::*;

    #[test]
    fn on_these_examples_where_multiple_redundant_paths_are_possible() {
        {
            // We want a state where there are 3 swamps and a starscape cleric is in hand
            // And we want to only get one state back out
            let hand = vec![StarscapeCleric];
            let deck = vec![Swamp];

            let board = Board::default()
                .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER))
                .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER))
                .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER))
                ;

            let state = State::new(hand, deck.into())
                .with_board(board);

            let mut output = Vec::with_capacity(1);

            state.add_casting_states(&mut output);

            assert_eq!(output.len(), 1, "{output:#?}");
        }

        {
            // We want a state where there are 3 swamps and two starscape clerics in hand
            // And we want to only get one state back out
            let hand = vec![StarscapeCleric, StarscapeCleric];
            let deck = vec![Swamp];

            let board = Board::default()
                .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER))
                .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER))
                .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER))
                ;

            let state = State::new(hand, deck.into())
                .with_board(board);

            let mut output = Vec::with_capacity(1);

            state.add_casting_states(&mut output);

            assert_eq!(output.len(), 1, "{output:#?}");
        }
    }

    #[test]
    fn on_this_found_example_where_multiple_redundant_paths_are_possible() {
        // We want a state like the one we found in practice,
        // And we want to only get one state back out
        let hand = vec![
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            Swamp,
            Swamp,
            Swamp,
        ].into();

        let deck = vec![
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
        ].into();

        let mut state = State::new(hand, deck);
        state.step = MainPhase1;
        state.board =
            Board::default()
            .enter(
                Permanent::card(Swamp, INITIAL_TURN_NUMBER)
            )
            .enter(
                Permanent::card(Swamp, INITIAL_TURN_NUMBER + 1)
            )
            .enter(
                Permanent::card(Swamp, INITIAL_TURN_NUMBER + 2)
            )
            ;
        state.turn_number = INITIAL_TURN_NUMBER + 2;
        state.land_plays = 0;

        let mut output = Vec::with_capacity(1);

        state.add_casting_states(&mut output);

        assert_eq!(output.len(), 1, "{output:#?}");
    }

    #[test]
    fn on_these_examples_where_we_care_about_order() {
        // We want an example state where we have multiple options to test
        // we get the state where we did play something out first
        let hand = vec![Swamp, Swamp, StarscapeCleric, StarscapeCleric];
        let deck = vec![Swamp];

        let board = Board::default()
            .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER))
            .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER))
            .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER))
            .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER))
            ;

        let state = State::new(hand, deck.into())
            .with_board(board);

        let mut output = Vec::with_capacity(1);

        state.add_casting_states(&mut output);

        assert_eq!(
            output.clone().into_iter().map(|s| s.unwrap().board).collect::<Vec<_>>(),
            vec![
                // We specifically want the 0 mana pool
                Board::default()
                    .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped())
                    .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER).tapped())
                    .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER))
                    .enter(Permanent::card(Swamp, INITIAL_TURN_NUMBER))
                    .enter(Permanent::card(StarscapeCleric, INITIAL_TURN_NUMBER))
            ],
        );
    }
}

impl State {
    fn apply_effect(&self, effect: Effect) -> Result<std::vec::IntoIter<Self>, EffectError> {
        use Effect::*;
        match effect {
            AddMana(pool) => {
                self.with_additional_mana(pool)
                    .map(|s| vec![s].into_iter())
            },
            SearchForCardTopOfDeckShuffle
            // SchemingSymmetry isn't meaningfully different agaist a goldfish
            | SchemingSymmetry => {
                let mut card_set = CardSet::new();

                for c in self.deck.iter() {
                    card_set.insert(*c);
                }

                Ok(
                    card_set.iter()
                    .map(|card| {
                        // In some sense, we should produce states for each possible after doing this.
                        // But that's intractable, and it seems plausible that doing this will produce
                        // the same overall answer.
                        // Until we have evidence against that, we'll proceed with this.
                        let Some(index) =
                            self.deck.iter()
                            .position(|c| c == card) else {
                                return self.clone();
                            };

                        let mut deck = self.deck.clone();

                        deck.swap(0, index);

                        Self {
                            deck,
                            ..self.clone()
                        }
                    })
                    .collect::<Vec<_>>()
                    .into_iter()
                )
            },
            TargetPlayerDrawsTwoCardsLosesTwoLife => todo!("TargetPlayerDrawsTwoCardsLosesTwoLife"),
            TargetPlayerDrawsThreeCardsLosesThreeLife => todo!("TargetPlayerDrawsThreeCardsLosesThreeLife"),
            HandToBattlefield(card) => {
                let index = self.hand.iter().position(|c| c == &card).ok_or(())?;

                let (hand, removed) = remove(&self.hand, index).ok_or(())?;

                Ok(vec![Self {
                    hand: hand.to_vec(),
                    board: self.board.enter(Permanent::card(removed, self.turn_number)),
                    ..self.clone()
                }].into_iter())
            },
            Offspring(card) => {
                let index = self.hand.iter().position(|c| c == &card).ok_or(())?;

                let (hand, removed) = remove(&self.hand, index).ok_or(())?;

                let token = Permanent::token_of(card, self.turn_number).with_p_t(1, 1);

                Ok(vec![Self {
                    hand: hand.to_vec(),
                    board: self.board.enter(Permanent::card(removed, self.turn_number)).enter(token),
                    ..self.clone()
                }].into_iter())
            },
        }
    }
}

// Per CR 113.3 the text of spells are Spell Abilities
#[derive(Clone, Debug)]
struct Ability {
    mana_cost: ManaCost,
    creature_cost: CreatureCount,
    effects: Vec<Effect>,
}

impl State {
    #[allow(unused)] // We expect this to be used eventually
    fn activated_abilities(&self, index: board::PermanentIndex) -> impl ExactSizeIterator<Item = Ability> {
        let Some(card) = todo!() else {
            return Abilities::Empty
        };

        match card {
            PhyrexianTower => {
                Abilities::FixedLength(
                    vec![
                        Ability {
                            mana_cost: ManaCost::default(),
                            creature_cost: 0,
                            effects: vec![Effect::AddMana(ManaPool {
                                colorless: 1,
                                ..ManaPool::default()
                            })],
                        },
                        Ability {
                            mana_cost: ManaCost::default(),
                            creature_cost: 1,
                            effects: vec![Effect::AddMana(ManaPool {
                                black: 2,
                                ..ManaPool::default()
                            })],
                        },
                    ].into_iter()
                )
            },
            _ => {
                todo!("activated_abilities for {card:?}");
            },
        }
    }

    fn cast_options(&self, card: Card) -> impl ExactSizeIterator<Item = Ability> {
        self.board.cast_options(card)
    }
}

struct SpreeIter {

}

impl SpreeIter {
    fn new(pool: ManaPool, base_cost: ManaCost, options: &[Ability]) -> Self {
        Self {

        }
    }
}

impl Iterator for SpreeIter {
    type Item = Ability;

    fn next(&mut self) -> Option<Self::Item> {
        // FIXME and the size_hint!
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // FIXME!
        (0, Some(0))
    }
}

impl ExactSizeIterator for SpreeIter {}

enum Abilities {
    Empty,
    FixedLength(std::vec::IntoIter<Ability>),
    Spree(SpreeIter)
}

impl Iterator for Abilities {
    type Item = Ability;

    fn next(&mut self) -> Option<Self::Item> {
        use Abilities::*;
        match self {
            Empty => None,
            FixedLength(ref mut iter) => iter.next(),
            Spree(ref mut iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        use Abilities::*;
        match self {
            Empty => (0, Some(0)),
            FixedLength(ref iter) => iter.size_hint(),
            Spree(ref iter) => iter.size_hint(),
        }
    }
}

impl ExactSizeIterator for Abilities {}

type Index = usize;

type MaterializedIndexSet = BTreeSet<Index>;

// Making this streaming iterator instead of a flat list, to avoid using 2^n memory, seems worth it.
struct LengthNIndexSubsets {
    target_length: u8,
    current_set: MaterializedIndexSet,
    index_set: IndexSet,
    all: Box<[Index]>,
    last_advance_was_skipped: bool,
}

// Written aiming to be compatible with streaming_iterator but currently
// not bothering to actually depend on that crate until we have
// a reason to.
impl LengthNIndexSubsets {
    fn fully_advanced(&self) -> bool {
        if self.all.is_empty() {
            return self.index_set > 0;
        }

        let log_2_or_0 =
            if self.index_set == 0 {
                0
            } else {
                self.index_set.ilog2() as usize
            };

        log_2_or_0 >= self.all.len()
    }

    fn advance(&mut self) {
        loop {
            if self.fully_advanced() {
                self.last_advance_was_skipped = true;
                return
            }

            if self.index_set.count_ones() == self.target_length as u32 {
                self.current_set.clear();

                let mut used = self.index_set;

                let mut index = 0;

                while used > 0 {
                    if used & 1 != 0 {
                        self.current_set.insert(self.all[index].clone());
                    }
                    index += 1;
                    used >>= 1;
                }

                self.index_set += 1;

                break
            }

            self.index_set += 1;
        }
    }

    fn get(&self) -> Option<&MaterializedIndexSet> {
        if self.last_advance_was_skipped {
            None
        } else {
            Some(&self.current_set)
        }
    }

    fn next(&mut self) -> Option<&MaterializedIndexSet> {
        self.advance();
        self.get()
    }
}

#[cfg(test)]
mod length_n_index_subsets_works {
    use super::*;

    #[test]
    fn on_a_zero_element_all() {
        let mut iter = LengthNIndexSubsets {
            target_length: <_>::default(),
            current_set: MaterializedIndexSet::new(),
            index_set: <_>::default(),
            all: (*(&[])).into(),
            last_advance_was_skipped: <_>::default(),
        };

        let empty = iter.next();

        assert_eq!(empty, Some(&MaterializedIndexSet::new()));

        let none = iter.next();

        assert_eq!(none, None);
    }

    #[test]
    fn on_a_one_element_all() {
        {
            let mut iter = LengthNIndexSubsets {
                target_length: 0,
                current_set: MaterializedIndexSet::new(),
                index_set: <_>::default(),
                all: (*(&[0])).into(),
                last_advance_was_skipped: <_>::default(),
            };

            let empty = iter.next();

            assert_eq!(empty, Some(&MaterializedIndexSet::new()));

            let none = iter.next();

            assert_eq!(none, None);
        }

        let mut iter = LengthNIndexSubsets {
            target_length: 1,
            current_set: MaterializedIndexSet::new(),
            index_set: <_>::default(),
            all: (*(&[0])).into(),
            last_advance_was_skipped: <_>::default(),
        };

        // Subsets of a one element set with
        // an element called A include just
        // [], and [A]

        let full = iter.next();

        assert_eq!(full, Some(&([0].into())));

        let none = iter.next();

        assert_eq!(none, None);
    }

    #[test]
    fn on_a_two_element_all() {
        {
            let mut iter = LengthNIndexSubsets {
                target_length: 0,
                current_set: MaterializedIndexSet::new(),
                index_set: <_>::default(),
                all: (*(&[0, 1])).into(),
                last_advance_was_skipped: <_>::default(),
            };

            let empty = iter.next();

            assert_eq!(empty, Some(&MaterializedIndexSet::new()));

            let none = iter.next();

            assert_eq!(none, None);
        }

        let mut iter = LengthNIndexSubsets {
            target_length: 1,
            current_set: MaterializedIndexSet::new(),
            index_set: <_>::default(),
            all: (*(&[0, 1])).into(),
            last_advance_was_skipped: <_>::default(),
        };

        // Subsets of a two element set with
        // elements called A and B include
        // [], [A], [B], [A, B]

        let first = iter.next();

        assert_eq!(first, Some(&([0].into())));

        let second = iter.next();

        assert_eq!(second, Some(&([1].into())));

        let none = iter.next();

        assert_eq!(none, None);
    }
}

fn length_n_subsets(slice: &[Index], target_length: u8) -> LengthNIndexSubsets {
    LengthNIndexSubsets {
        target_length,
        current_set: MaterializedIndexSet::new(),
        index_set: <_>::default(),
        all: slice.into(),
        last_advance_was_skipped: <_>::default(),
    }
}

type SacrificeCreaturesError = ();

impl State {
    fn sacrifice_creatures(&self, creature_count: CreatureCount) -> Result<impl Iterator<Item = Self> + '_, SacrificeCreaturesError> {
        self.board.sacrifice_creatures(creature_count).map(|boards| boards.map(|board| Self { board, ..self.clone() }))
    }
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
    #[timeout(1)]
    fn on_empty_deck() {
        for pet in PetSpec::ALL {
            let spec = Spec { draw: NthDraw(0), pet, ..<_>::default() };
            assert!(calculate(spec, &[]).is_err());
        }
    }

    #[test]
    #[timeout(1)]
    fn on_too_small_but_non_empty_deck() {
        for pet in PetSpec::ALL {
            let spec = Spec { draw: NthDraw(0), pet, ..<_>::default() };
            assert!(calculate(spec, &[Swamp; 1]).is_err());
        }
    }

    #[test]
    #[timeout(1)]
    fn on_too_large_deck() {
        for pet in PetSpec::ALL {
            let spec = Spec { draw: NthDraw(0), pet, ..<_>::default() };
            assert!(calculate(spec, &[Swamp; MAX_DECK_SIZE as usize + 1]).is_err());
        }
    }

    #[test]
    #[timeout(1)]
    fn on_8_swamps() {
        let _8_swamps = [Swamp; 8];

        const DRAW_SPECS: [DrawSpec; 5] = [
            NthDraw(0),
            NthDraw(1),
            NthDraw(10),
            NthDraw(100),
            NthDraw(1_000),
        ];

        for draw in DRAW_SPECS {
            for pet in PetSpec::ALL {
                let outcomes = calculate(Spec { draw, pet, ..<_>::default() }, &_8_swamps).unwrap();

                for outcome in outcomes {
                    let does_match = matches!(outcome, OutcomeAt{ outcome: Lose, ..});

                    assert!(does_match);
                }
            }
        }
    }

    #[test]
    #[timeout(100)]
    fn on_60_swamps() {
        let _60_swamps = [Swamp; 60];

        const DRAW_SPECS: [DrawSpec; 7] = [
            NthDraw(0),
            NthDraw(1),
            NthDraw(10),
            NthDraw(100),
            NthDraw(1_000),
            NthDraw(10_000),
            NthDraw(100_000),
        ];

        for draw in DRAW_SPECS {
            for pet in PetSpec::ALL {
                let outcomes = calculate(Spec { draw, pet, ..<_>::default() }, &_60_swamps).unwrap();

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
    }

    #[test]
    #[timeout(100)]
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

        const DRAW_SPECS: [DrawSpec; 5] = [
            NthDraw(0),
            NthDraw(1),
            NthDraw(10),
            NthDraw(100),
            NthDraw(1_000),
        ];

        for draw in DRAW_SPECS {
            for pet in PetSpec::ALL {
                let outcomes = calculate(Spec { draw, pet, ..<_>::default() }, &_8_non_lands).unwrap();

                for outcome in outcomes {
                    let does_match = matches!(outcome, OutcomeAt{ outcome: Lose, ..});

                    assert!(does_match);
                }
            }
        }
    }

    #[test]
    #[timeout(1)]
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

        const DRAW_SPECS: [DrawSpec; 5] = [
            NthDraw(0),
            NthDraw(1),
            NthDraw(10),
            NthDraw(100),
            NthDraw(1_000),
        ];

        for draw in DRAW_SPECS {
            for pet in PetSpec::ALL {
                let outcomes = calculate(Spec{ draw, pet, ..<_>::default() }, &_8_swamps_and_non_basic_lands).unwrap();

                for outcome in outcomes {
                    let does_match = matches!(outcome, OutcomeAt{ outcome: Lose, ..});

                    assert!(does_match);
                }
            }
        }
    }

    #[test]
    #[timeout(1000)]
    fn on_stacked_swamps_and_cleric() {
        let _deck: [Card; 60] = [
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
        ];

        let outcomes = calculate(Spec{ draw: NO_SHUFFLING, pet: Goldfish, turn_bounds: StopAt(7) }, &_deck).unwrap();

        for outcome in outcomes {
            let does_match = matches!(outcome, OutcomeAt{ outcome: Win, ..});

            assert!(does_match);
        }
    }

    #[test]
    #[timeout(10000)]
    fn on_selected_non_basic_lands_and_cleric() {
        let _deck: [Card; 28] = [
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            HagraMauling,
            MemorialToFolly,
            TheDrossPits,
            PhyrexianTower,
            BlastZone,
            SceneOfTheCrime,
            HagraMauling,
            MemorialToFolly,
            TheDrossPits,
            PhyrexianTower,
            BlastZone,
            SceneOfTheCrime,
            HagraMauling,
            MemorialToFolly,
            TheDrossPits,
            PhyrexianTower,
            BlastZone,
            SceneOfTheCrime,
            HagraMauling,
            MemorialToFolly,
            TheDrossPits,
            PhyrexianTower,
            BlastZone,
            SceneOfTheCrime,
        ];

        let outcomes = calculate(Spec{ draw: NO_SHUFFLING, pet: Goldfish, turn_bounds: StopAt(7) }, &_deck).unwrap();

        for outcome in outcomes {
            let does_match = matches!(outcome, OutcomeAt{ outcome: Win, ..});

            assert!(does_match);
        }
    }

    #[test]
    #[timeout(5000)]
    fn on_fewer_non_basic_lands_and_cleric_stop_at_3() {
        let _deck: [Card; 11] = [
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            Swamp,
            MemorialToFolly,
            TheDrossPits,
            Swamp,
            BlastZone,
            SceneOfTheCrime,
            HagraMauling,
        ];

        let outcomes = calculate(Spec{ draw: NO_SHUFFLING, pet: Goldfish, turn_bounds: StopAt(3) }, &_deck).unwrap();

        for outcome in outcomes {
            let does_match = matches!(outcome, OutcomeAt{ outcome: Win, ..});

            assert!(does_match);
        }
    }

    #[test]
    #[timeout(1000)]
    fn on_fewer_non_basic_lands_and_cleric() {
        let _deck: [Card; 14] = [
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            HagraMauling,
            MemorialToFolly,
            TheDrossPits,
            PhyrexianTower,
            BlastZone,
            SceneOfTheCrime,
            HagraMauling,
            MemorialToFolly,
            TheDrossPits,
            PhyrexianTower,
        ];

        let outcomes = calculate(Spec{ draw: NO_SHUFFLING, pet: Goldfish, turn_bounds: StopAt(2) }, &_deck).unwrap();

        for outcome in outcomes {
            let does_match = matches!(outcome, OutcomeAt{ outcome: Win, ..});

            assert!(does_match);
        }
    }

    #[test]
    #[timeout(100)]
    fn on_a_small_stack_of_clerics_and_swamps_and_a_tower() {
        let _deck: [Card; 9] = [
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            PhyrexianTower,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
        ];

        let outcomes = calculate(Spec{ draw: NO_SHUFFLING, pet: Goldfish, turn_bounds: StopAt(7) }, &_deck).unwrap();

        for outcome in outcomes {
            let does_match = matches!(outcome, OutcomeAt{ outcome: Lose, ..});

            assert!(does_match);
        }
    }

    #[test]
    #[timeout(10000)]
    fn on_a_small_stack_of_clerics_and_hagra_maulings_and_swamps() {
        let _deck: [Card; 12] = [
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            HagraMauling,
            HagraMauling,
            HagraMauling,
            HagraMauling,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
        ];

        let outcomes = calculate(Spec{ draw: NO_SHUFFLING, pet: Goldfish, turn_bounds: StopAt(7) }, &_deck).unwrap();

        for outcome in outcomes {
            let does_match = matches!(outcome, OutcomeAt{ outcome: Lose, ..});

            assert!(does_match);
        }
    }
}

#[cfg(test)]
mod calculate_step_works {
    use super::*;

    #[test]
    fn when_you_can_swing_for_lethal_first_combat() {
        let mut arena = Arena::with_capacity(128);

        let deck: [Card; 6] = [
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
        ];

        let mut state = State::new(<_>::default(), deck.into());
        state.step = CombatDamage;
        state.board =
            Board::in(&arena)
            .enter(
                &arena,
                Permanent::card(StarscapeCleric, INITIAL_TURN_NUMBER)
                    .with_p_t(INITIAL_LIFE.try_into().unwrap(), INITIAL_LIFE.try_into().unwrap())
            )
            ;
        // So we can attack with the big creature
        state.turn_number = INITIAL_TURN_NUMBER + 1;
        let outcomes = calculate_step(state);

        for outcome in outcomes.into_vec().into_iter() {
            let does_match = matches!(outcome.as_ref().unwrap_err(), OutcomeAt{ outcome: Win, ..});

            assert!(does_match);
        }
    }

    #[test]
    fn when_you_can_swing_for_lethal_next_combat() {
        let mut arena = Arena::with_capacity(128);

        let deck: [Card; 6] = [
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
        ];

        let mut state = State::new(<_>::default(), deck.into());
        state.step = MainPhase2;
        state.board =
            Board::in(&arena)
            .enter(
                &arena,
                Permanent::card(StarscapeCleric, INITIAL_TURN_NUMBER)
                    .with_p_t(INITIAL_LIFE.try_into().unwrap(), INITIAL_LIFE.try_into().unwrap())
            )
            ;

        let mut states = vec![state];

        while !states.iter().any(|s| s.step == CombatDamage) {
            let state = states.pop().expect("ran out of states");

            let outcomes = calculate_step(state);
            for outcome in outcomes.into_vec().into_iter() {
                states.push(outcome.clone().expect("ended too soon"));
            }
        }

        for state in states {
            let outcomes = calculate_step(state);

            for outcome in outcomes.into_vec().into_iter() {
                let does_match = matches!(outcome.as_ref().unwrap_err(), OutcomeAt{ outcome: Win, ..});

                assert!(does_match);
            }
        }
    }

    #[test]
    fn in_this_case_that_should_not_produce_multiple_outputs_where_starscape_cleric_is_played() {
        let mut arena = Arena::with_capacity(128);

        let hand = vec![
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            StarscapeCleric,
            Swamp,
            Swamp,
            Swamp,
        ].into();

        let deck = vec![
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
            Swamp,
        ].into();

        let mut state = State::new(hand, deck);
        state.step = MainPhase1;
        state.board =
            // TODO make a convenience macro that looks something like
            // let (_arena, board) = board!(
            //     Permanent::card(Swamp, INITIAL_TURN_NUMBER),
            //     Permanent::card(Swamp, INITIAL_TURN_NUMBER + 1),
            //     Permanent::card(Swamp, INITIAL_TURN_NUMBER + 2),
            // );
            // because that will be insulated against any futher changes, and clearer to read to boot!
            Board::in(&arena)
            .enter(
                &arena,
                Permanent::card(Swamp, INITIAL_TURN_NUMBER)
            )
            .enter(
                &arena,
                Permanent::card(Swamp, INITIAL_TURN_NUMBER + 1)
            )
            .enter(
                &arena,
                Permanent::card(Swamp, INITIAL_TURN_NUMBER + 2)
            )
            ;
        state.turn_number = INITIAL_TURN_NUMBER + 2;
        state.land_plays = 0;

        let outcomes = calculate_step(state);

        let mut has_cleric_count = 0;
        for outcome in outcomes.into_vec().into_iter() {
            let mut state = outcome.unwrap();

            if state.board.permanents_mut().any(|p| p.kind().card() == StarscapeCleric) {
                has_cleric_count += 1;
            }
        }

        assert_eq!(has_cleric_count, 1);
    }
}

type Deck = Box<[Card]>;

fn draw(deck: Deck) -> Option<(Card, Deck)> {
    deck.split_first().map(|(c, d)| (c.clone(), d.into()))
}

fn nth_ordered(
    deck: &[Card],
    n: PermutationNumber
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
        const DECKS: [[Card; 3]; 6] = [
            [_0, _1, _2],
            [_0, _2, _1],
            [_1, _0, _2],
            [_1, _2, _0],
            [_2, _0, _1],
            [_2, _1, _0],
        ];

        for i in 0..DECKS.len() {
            let actual = nth_ordered(&DECKS[0], i.try_into().unwrap()).expect("");
            assert_eq!(*actual, DECKS[i], "mismatch at {i}");
        }

        let result = nth_ordered(&DECKS[0], DECKS.len().try_into().unwrap());
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

        Ok(perm.into_boxed_slice())
    }
}