#![deny(unreachable_patterns)]
#![deny(unused_must_use)]
// #![deny(unused_variables)] // This makes todo! use annoying
#![allow(non_snake_case)]

use card::Card::{self, *};
use mana::{ManaCost, ManaPool};

use std::collections::{BTreeSet, HashSet};
use std::iter::ExactSizeIterator;

type PermutationNumber = u128;

#[derive(Clone, Copy, Debug)]
pub enum DrawSpec {
    //AllDraws,
    NthDraw(PermutationNumber),
}
use DrawSpec::*;

#[cfg(test)]
const NO_SHUFFLING: DrawSpec = NthDraw(0);

#[derive(Clone, Copy, Debug)]
pub enum PetSpec {
    Goldfish,
}
use PetSpec::*;

impl PetSpec {
    #[cfg(test)]
    const ALL: [PetSpec; 1] = [
        Goldfish,
    ];
}

#[derive(Debug)]
pub struct Spec {
    pub draw: DrawSpec,
    pub pet: PetSpec,
}

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

    let ordered_deck = match spec.draw {
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

    match spec.pet {
        Goldfish => {
            let mut outcomes = Vec::with_capacity(64);

            while let Some(state) = states.pop() {
                let results = calculate_step(state);

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
    }
}

fn calculate_step(mut state: State) -> Box<[Result<State, OutcomeAt>]> {
    macro_rules! one_path_forward {
        () => {
            return Box::from([Ok(state)])
        }
    }

    match state.step {
        Untap => {
            for permanent in state.board.permanents_mut() {
                permanent.untap();
            }

            state.step = Draw;

            one_path_forward!()
        }
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
                        state.board = state.board.enter(card);
                        state.land_plays -= 1;

                        one_path_forward!()
                    }
                    _ => {
                        let mut output = Vec::with_capacity(land_indexes.len());

                        for i in land_indexes.into_iter().rev() {
                            let (h, card) = remove(&state.hand, i).expect("land index to be valid");

                            output.push(Ok(State {
                                hand: h.to_vec(),
                                board: state.board.enter(card),
                                land_plays: state.land_plays - 1,
                                ..state.clone()
                            }));
                        }

                        return Box::from(output);
                    }
                }
            }

            let mut output = Vec::with_capacity(state.hand.len());

            for spend_state in state.mana_spends() {
dbg!(&state, &spend_state);
                for card_index in 0..spend_state.hand.len() {
dbg!("attempt_to_cast", spend_state.hand.get(card_index));
                    match spend_state.attempt_to_cast(card_index) {
                        Ok(new_states) => {
                            for new_state in new_states {
                                output.push(Ok(new_state));
                            }
                        },
                        Err(_) => {}
                    }
                }

            }

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

type IndexSet = u128;
const INDEX_SET_MAX_ELEMENTS: usize = IndexSet::BITS as _;

mod board {
    use card::Card;
    use mana::{ManaCost, ManaPool, SpendError};

    use super::{Ability, Abilities, Effect, IndexSet, INDEX_SET_MAX_ELEMENTS, SpreeIter};

    use std::collections::{BTreeMap, BTreeSet};


    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
    enum PermanentKind {
        Card(Card),
    }

    type IsTapped = bool;
    //type IsFlipped = bool;
    //type IsFaceDown = bool;
    //type IsPhasedOut = bool;

    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Permanent {
        kind: PermanentKind,
        // "CR 110.6. A permanent’s status is its physical state. There are four status categories, each of which has two
        // possible values: tapped/untapped, flipped/unflipped, face up/face down, and phased in/phased out.
        // Each permanent always has one of these values for each of these categories."
        is_tapped: IsTapped,
        //is_flipped: IsFlipped,
        //is_face_down: IsFaceDown,
        //is_phased_out: IsPhasedOut,
    }

    impl Permanent {
        pub fn is_a_creature(&self) -> bool {
            use PermanentKind::*;
            match self.kind {
                Card(card) => card.is_a_creature(),
            }
        }

        pub fn untap(&mut self) {
            self.is_tapped = true;
        }
    }

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

    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
    enum ManaAbilityKind {
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

    impl Permanent {
        fn mana_abilities<'board>(&self, board: &'board Board, permanent_index: PermanentIndex) -> impl Iterator<Item = ManaAbility> + 'board {
            let permanent_kind = self.kind.clone();

            match self.kind {
                PermanentKind::Card(card) =>
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
    }

    // Keep things private because we suspect we'll want lots of different
    // ways to query what is on the board, and that we will care about
    // making them fast. So, we expect to want to change the internals
    // later on, without needing to change all the usages of those
    // internals.
    #[derive(Clone, Debug, Default)]
    pub struct Board {
        mana_pool: ManaPool,
        // We suspect we'll want like lands, creatures, etc. as ready to go collections
        permanents: Vec<Permanent>,
    }

    fn push(slice: &[Permanent], element: Permanent) -> Vec<Permanent> {
        let mut output = Vec::with_capacity(slice.len() + 1);

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
        pub fn permanent(&mut self, index: PermanentIndex) -> Option<&Permanent> {
            self.permanents.get(index)
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
        pub fn enter(&self, card: Card) -> Self {
            Board {
                permanents: push(
                    &self.permanents,
                    Permanent{
                        kind: PermanentKind::Card(card),
                        is_tapped: Card::enters_tapped(card),
                    }
                ),
                ..self.clone()
            }
        }
    }

    /// The index of the permanent on the board
    // TODO? Make this a generational index?
    pub type PermanentIndex = usize;

    /// The index of the mana abilty on the given permanent
    type ManaAbilityIndex = u8;

    /// A Mana Ability that can be played on the battlefield, including the costs.
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    struct ManaAbility {
        kind: ManaAbilityKind,
        ability_index: ManaAbilityIndex,
        permanent_kind: PermanentKind,
        permanent_index: PermanentIndex,
    }

    type ApplyManaAbilityError = ();

    fn apply_mana_ability(board: &Board, mana_ability: &ManaAbility) -> Result<Board, ApplyManaAbilityError> {
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

    type ManaAbilitiesSet = BTreeSet<ManaAbility>;

    // Making this streaming iterator instead of a flat list, to avoid using 2^n memory, seems worth it.
    #[derive(Debug)]
    struct ManaAbilitiesSubsets {
        current_set: ManaAbilitiesSet,
        index_set: IndexSet,
        all: Box<[ManaAbility]>,
        last_advance_was_skipped: bool,
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
        }

        fn get(&self) -> Option<&ManaAbilitiesSet> {
            if self.last_advance_was_skipped {
                None
            } else {
                Some(&self.current_set)
            }
        }

        fn next(&mut self) -> Option<&ManaAbilitiesSet> {
            self.advance();
            self.get()
        }
    }

    #[cfg(test)]
    mod mana_abilty_subsets_works {
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
            let mut iter = ManaAbilitiesSubsets {
                current_set: <_>::default(),
                index_set: <_>::default(),
                all: [].into(),
                last_advance_was_skipped: <_>::default(),
            };

            let empty = iter.next();

            assert_eq!(empty, Some(&ManaAbilitiesSet::new()));

            let none = iter.next();

            assert_eq!(none, None);
        }

        #[test]
        fn on_a_one_element_all() {
            let mut iter = ManaAbilitiesSubsets {
                current_set: <_>::default(),
                index_set: <_>::default(),
                all: [ABILITY].into(),
                last_advance_was_skipped: <_>::default(),
            };

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
            let mut iter = ManaAbilitiesSubsets {
                current_set: <_>::default(),
                index_set: <_>::default(),
                all: TWO_ABILITIES.into(),
                last_advance_was_skipped: <_>::default(),
            };

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
    }

    fn mana_abilty_subsets(board: &Board) -> ManaAbilitiesSubsets {
        let all_mana_abilities =
            dbg!(&board.permanents)
                .iter()
                .enumerate()
                .flat_map(|(i, p)| p.mana_abilities(board, i))
                .collect::<Vec<_>>();
dbg!(&all_mana_abilities);
        let len = all_mana_abilities.len();

        // If this limit ever gets reached, we can instead use an arbitrary length bitset
        // with the same overall interface, including addition.
        assert!(len <= INDEX_SET_MAX_ELEMENTS, "Too many mana abilities to fit subset indexes in 128 bits");

        ManaAbilitiesSubsets {
            current_set: <_>::default(),
            index_set: <_>::default(),
            all: all_mana_abilities.into(),
            last_advance_was_skipped: <_>::default(),
        }
    }

    /// A `ManaAbilityKey` represents everything about a given mana ability that
    /// would be relevant when making a decision. So two different land types
    /// should have different keys, two identical untapped Swamps should have
    /// the same key, but a Swamp enchanted with a Kudzu should have a different key.
    /// Two different mana abilities on the same permanent should also have different keys
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    struct ManaAbilityKey {
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
        Card(Card)
    }

    impl From<&PermanentKind> for ManaAbilityKeyKind{
        fn from(kind: &PermanentKind) -> Self {
            match kind {
                PermanentKind::Card(card) => Self::Card(*card),
            }
        }
    }

    impl From<PermanentKind> for ManaAbilityKeyKind {
        fn from(kind: PermanentKind) -> Self {
            From::from(&kind)
        }
    }

    type ManaAbilityKeySet = BTreeSet<ManaAbilityKey>;

    fn to_key_set(mana_abilities: &ManaAbilitiesSet) -> ManaAbilityKeySet {
        mana_abilities.iter()
            .map(|ability| {
                ManaAbilityKey {
                    kind: ability.permanent_kind,
                    index: ability.ability_index,
                }
            })
            .collect()
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
                //SignInBlood => {
                    //Abilities::FixedLength(
                        //vec![
                            //Ability {
                                //mana_cost: ManaCost::default(),
                                //creature_cost: 0,
                                //effects: vec![Effect::TargetPlayerDrawsTwoCardsLosesTwoLife],
                            //},
                        //].into_iter()
                    //)
                //},
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
                _ => {
                    todo!("cast_options for {card:?}");
                },
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
            todo!("sacrifice_creature_at")
        }

        /// Taps the given permanent as a cost. Returns an `Err` if it is already tapped, or if there is
        /// no permanent at that index.
        fn tap_permanent_at(&self, permanent_index: PermanentIndex) -> Result<Self, TapPermanentError> {
            if let Some(old) = self.permanents.get(permanent_index) {
                if old.is_tapped {
                    Err(())
                } else {
                    Ok(Self {
                        permanents: set_at(
                            &self.permanents,
                            permanent_index,
                            Permanent {
                                is_tapped: true,
                                ..old.clone()
                            },
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

        pub fn sacrificeable_creatures(&self) -> Box<[PermanentIndex]> {
            // There are creatures that can't be sacrificed, and effects like "can't cause you to sacrifice permanents".
            // So this won't necessarily always be just all creatures
            self.creatures()
        }

        pub fn spend(&self, mana_cost: ManaCost) -> Result<Self, SpendError> {
            let mana_pool = self.mana_pool.spend(mana_cost)?;

            Ok(self.with_mana_pool(mana_pool))
        }

        pub fn mana_spends(&self) -> impl Iterator<Item = Self> {
            let mut all_mana_abilty_subsets = mana_abilty_subsets(self);

            let mut output = BTreeMap::new();

            while let Some(mana_abilities) = all_mana_abilty_subsets.next() {
                let key = to_key_set(&mana_abilities);
                if output.contains_key(&key) {
                    // Avoid bothering to track identical options
                    // like 6 different orders for 3 Swamps.
                    continue
                }

                let mut current_board = self.clone();

                // TODO? is it worth it to avoid doing all the work
                // up front by making this a custom iterator?
                for mana_ability in mana_abilities {
                    if let Ok(board) = apply_mana_ability(&current_board, mana_ability) {
                        current_board = board;
                    }
                }

                output.insert(key, current_board);
            }

            output.into_values()
        }
    }

    #[cfg(test)]
    mod mana_spends_works {
        use super::*;
        use card::Card::*;

        #[test]
        fn on_a_default_board() {
            let board = Board::default();

            let vec = board.mana_spends().collect::<Vec<_>>();

            // Not spending any mana counts as an option.
            assert_eq!(vec.len(), 1);
        }

        #[test]
        fn on_a_single_swamp() {
            let mut board = Board::default();

            board = board.enter(Swamp);

            assert!(board.permanents.len() > 0, "pre-condition failure");

            let vec = board.mana_spends().collect::<Vec<_>>();

            // We should have the option to not tap the swamp
            // and the option to tap the swamp.
            // This is because we call mana_spends in cases where we might have a 1 drop in hand.
            assert!(vec.len() >= 2);
        }
    }
}
use board::Board;


#[derive(Copy, Clone, Debug, Default)]
enum Step {
    #[default]
    Untap,
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

#[derive(Clone, Debug)]
struct State {
    hand: Hand,
    board: Board,
    deck: Deck,
    land_plays: LandPlays,
    step: Step,
    turn_number: TurnNumber,
}

impl State {
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

    fn mana_spends(&self) -> impl Iterator<Item = Self> + '_ {
        // Theoretically there could be mana spends involving the
        // hand or library.
        self.board
            .mana_spends()
            .map(|board| {
                State {
                    board,
                    ..self.clone()
                }
            })
    }

    fn attempt_to_cast(
        &self,
        card_index: CardIndex,
    ) -> Result<impl Iterator<Item = Self>, AttemptToCastError> {
        use AttemptToCastError::*;
        let card = *self.hand.get(card_index).ok_or(NoCard)?;

        let cast_options = self.cast_options(card);

        let mut output: Vec<Self> = Vec::with_capacity(cast_options.len());

        for cast_option in cast_options {
            let Ok(new_board) = self.board.spend(cast_option.mana_cost) else {
                continue
            };

            let new_state = self.with_board(new_board);

            let Ok(new_states) =
                new_state
                    .sacrifice_creatures(cast_option.creature_cost) else {
                continue
            };

            let mapped_states = new_states
                .flat_map(|unmapped_state| {
                    // We don't want all the states between the individual effects!
                    // But, we do want to allow multiple states to be returned from each 
                    // stage, in case there are choices as part of them. To summarize, 
                    // we only want the possible end states after the effect list is done, 
                    // but we do want all of them!
                    // Thinking through Insatiable Avarice is probably a good example for this
                    let mut output = vec![unmapped_state];
                    for effect in cast_option.effects.iter() {
                        output = output.into_iter().map(move |s: State| {
                            let applied: Result<State, ()> = s.apply_effect(*effect);
                            applied.unwrap_or(s)
                        }).collect();
                    }

                    output
                });

            output.extend(mapped_states);
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
    TargetPlayerDrawsThreeCardsLosesThreeLife,
}

type EffectError = ();

impl State {
    fn apply_effect(&self, effect: Effect) -> Result<Self, EffectError> {
        use Effect::*;
        match effect {
            AddMana(pool) => {
                self.with_additional_mana(pool)
            },
            SearchForCardTopOfDeckShuffle => todo!("SearchForCardTopOfDeckShuffle"),
            TargetPlayerDrawsThreeCardsLosesThreeLife => todo!("TargetPlayerDrawsThreeCardsLosesThreeLife"),
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
    #[allow(unused)] // We expect this to be used event
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
            dbg!(self.fully_advanced(), self.index_set, self.index_set.count_ones() != self.target_length as u32);
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
    fn on_empty_deck() {
        for pet in PetSpec::ALL {
            let spec = Spec { draw: NthDraw(0), pet };
            assert!(calculate(spec, &[]).is_err());
        }
    }

    #[test]
    fn on_too_small_but_non_empty_deck() {
        for pet in PetSpec::ALL {
            let spec = Spec { draw: NthDraw(0), pet };
            assert!(calculate(spec, &[Swamp; 1]).is_err());
        }
    }

    #[test]
    fn on_too_large_deck() {
        for pet in PetSpec::ALL {
            let spec = Spec { draw: NthDraw(0), pet };
            assert!(calculate(spec, &[Swamp; MAX_DECK_SIZE as usize + 1]).is_err());
        }
    }

    #[test]
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
                let outcomes = calculate(Spec { draw, pet }, &_8_swamps).unwrap();

                for outcome in outcomes {
                    let does_match = matches!(outcome, OutcomeAt{ outcome: Lose, ..});

                    assert!(does_match);
                }
            }
        }
    }

    #[test]
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
                let outcomes = calculate(Spec { draw, pet }, &_60_swamps).unwrap();

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
                let outcomes = calculate(Spec { draw, pet }, &_8_non_lands).unwrap();

                for outcome in outcomes {
                    let does_match = matches!(outcome, OutcomeAt{ outcome: Lose, ..});

                    assert!(does_match);
                }
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

        const DRAW_SPECS: [DrawSpec; 5] = [
            NthDraw(0),
            NthDraw(1),
            NthDraw(10),
            NthDraw(100),
            NthDraw(1_000),
        ];

        for draw in DRAW_SPECS {
            for pet in PetSpec::ALL {
                let outcomes = calculate(Spec{ draw, pet }, &_8_swamps_and_non_basic_lands).unwrap();

                for outcome in outcomes {
                    let does_match = matches!(outcome, OutcomeAt{ outcome: Lose, ..});

                    assert!(does_match);
                }
            }
        }
    }

    #[test]
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

        let outcomes = calculate(Spec{ draw: NO_SHUFFLING, pet: Goldfish }, &_deck).unwrap();

        for outcome in outcomes {
            let does_match = matches!(outcome, OutcomeAt{ outcome: Win, ..});

            assert!(does_match);
        }
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