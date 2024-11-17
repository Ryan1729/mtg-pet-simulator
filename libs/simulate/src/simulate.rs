use card::Card::{self, *};
use mana::{ManaCost, ManaPool, ManaType::*, SpendError};

use std::collections::HashSet;
use std::iter::ExactSizeIterator;

type PermutationNumber = u128;

#[derive(Clone, Copy, Debug)]
pub enum DrawSpec {
    //AllDraws,
    NthDraw(PermutationNumber),
}
use DrawSpec::*;

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
                for card_index in 0..spend_state.hand.len() {
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

mod board {
    use card::Card;
    use mana::{ManaCost, ManaPool, ManaType::*, SpendError};

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
    struct Permanent {
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
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
    enum ManaAbilityKind {
        //TapForWhite,
        //TapForBlue,
        TapForBlack,
        //TapForRed,
        //TapForGreen,
        TapForColorless,
        SacrificeCreatureForTwoBlack,
    }
    use ManaAbilityKind::*;

    static NO_MANA_ABILITIES: &[ManaAbilityKind] = &[];
    static TAP_FOR_BLACK: &[ManaAbilityKind] = &[TapForBlack];
    static PHYREXIAN_TOWER_ABILITIES: &[ManaAbilityKind] = &[TapForColorless, SacrificeCreatureForTwoBlack];
    static TAP_FOR_COLORLESS: &[ManaAbilityKind] = &[TapForColorless];

    fn mana_ability_kinds_for_card(card: Card) -> impl Iterator<Item = &'static ManaAbilityKind> {
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
        fn mana_abilities(&self, permanent_index: PermanentIndex) -> impl Iterator<Item = ManaAbility> {
            let permanent_kind = self.kind.clone();

            match self.kind {
                PermanentKind::Card(card) =>
                    mana_ability_kinds_for_card(card)
                        .enumerate()
                        .map(move |(ability_index, kind)|
                            ManaAbility{
                                kind: *kind,
                                permanent_index,
                                permanent_kind: permanent_kind.clone(),
                                ability_index: ability_index.try_into().unwrap(),
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

    impl Board {
        pub fn with_mana_pool(&self, mana_pool: ManaPool) -> Self {
            Self {
                mana_pool,
                ..self.clone()
            }
        }

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

    fn apply_mana_ability(board: &mut Board, mana_ability: &ManaAbility) -> Board {
        todo!("apply_mana_ability")
    }

    type ManaAbilitiesSet = BTreeSet<ManaAbility>;

    type IndexSet = u128;
    const INDEX_SET_MAX_ELEMENTS: usize = IndexSet::BITS as _;

    // Making this streaming iterator instead of a flat list, to avoid using 2^n memory, seems worth it.
    struct ManaAbilitiesSubsets {
        current_set: ManaAbilitiesSet,
        index_set: IndexSet,
        all: Box<[ManaAbility]>,
    }

    // Written aiming to be compatible with streaming_iterator but currently
    // not bothering to actually depend on that crate until we have
    // a reason to.
    impl ManaAbilitiesSubsets {
        fn fully_advanced(&self) -> bool {
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
            if self.fully_advanced() {
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

    fn mana_abilty_subsets(board: &Board) -> ManaAbilitiesSubsets {
        let all_mana_abilities =
            board.permanents
                .iter()
                .enumerate()
                .flat_map(|(i, p)| p.mana_abilities(i))
                .collect::<Vec<_>>();

        let len = all_mana_abilities.len();

        // If this limit ever gets reached, we can instead use an arbitrary length bitset
        // with the same overall interface, including addition.
        assert!(len <= INDEX_SET_MAX_ELEMENTS, "Too many mana abilities to fit subset indexes in 128 bits");

        ManaAbilitiesSubsets {
            current_set: <_>::default(),
            index_set: <_>::default(),
            all: all_mana_abilities.into(),
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

    fn to_key_set(board: &Board, mana_abilities: &ManaAbilitiesSet) -> ManaAbilityKeySet {
        mana_abilities.iter()
            .map(|ability| {
                ManaAbilityKey {
                    kind: ability.permanent_kind,
                    index: ability.ability_index,
                }
            })
            .collect()
    }

    impl Board {
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
                let key = to_key_set(self, &mana_abilities);
                if output.contains_key(&key) {
                    // Avoid bothering to track identical options
                    // like 6 different orders for 3 Swamps.
                    continue
                }

                let mut current_board = self.clone();

                // TODO? is it worth it to avoid doing all the work
                // up front by making this a custom iterator?
                for mana_ability in mana_abilities {
                    current_board = apply_mana_ability(&mut current_board, mana_ability);
                }

                output.insert(key, current_board);
            }

            output.into_values()
        }
    }
}
use board::Board;


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
        Self {
            board: self.board.with_mana_pool(mana_pool),
            ..self.clone()
        }
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

        let mut output = Vec::with_capacity(cast_options.len());

        for cast_option in cast_options {
            let Ok(new_board) = self.board.spend(cast_option.mana_cost) else {
                continue
            };

            let Ok(new_states) =
                self.with_board(new_board)
                    .sacrifice_creatures(cast_option.creature_cost) else {
                continue
            };

            output.extend(
                new_states
                    .flat_map(|s|
                        cast_option.effects.iter()
                            .map(move |effect|
                                s.apply_effect(*effect)
                            )
                    )
            );
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
    // TODO? Make a NonEmptyManaPool? Is Add 0 manan something we want to represent?
    AddMana(ManaPool),
}

struct CastOption {
    mana_cost: ManaCost,
    creature_cost: CreatureCount,
    effects: Vec<Effect>,
}

impl State {
    fn cast_options(&self, card: Card) -> impl ExactSizeIterator<Item = CastOption> {
        match card {
            PhyrexianTower => {
                vec![
                    CastOption {
                        mana_cost: ManaCost::default(),
                        creature_cost: 0,
                        effects: vec![Effect::AddMana(ManaPool {
                            colorless: 1,
                            ..ManaPool::default()
                        })],
                    },
                    CastOption {
                        mana_cost: ManaCost::default(),
                        creature_cost: 1,
                        effects: vec![Effect::AddMana(ManaPool {
                            black: 2,
                            ..ManaPool::default()
                        })],
                    },
                ].into_iter()
            },
            _ => {
                todo!("cast_options for {card:?}"); vec![].into_iter()
            },
        }
    }

    fn apply_effect(&self, effect: Effect) -> Self {
        todo!("apply_effect");
    }
}

fn length_n_subsets<A>(slice: &[A], n: u8) -> Vec<Vec<A>> {
    todo!("length_n_subsets");
}

type SacrificeCreaturesError = ();

use board::PermanentIndex;

impl State {
    fn sacrifice_creatures(&self, creature_count: CreatureCount) -> Result<impl Iterator<Item = Self>, SacrificeCreaturesError> {
        let sacrificeable_creatures = self.board.sacrificeable_creatures();
        if (creature_count as usize) > sacrificeable_creatures.len() {
            return Err(())
        }

        let subsets = length_n_subsets(&sacrificeable_creatures, creature_count);

        let mut output = Vec::with_capacity(subsets.len());

        for subset in subsets {
            let mut state = self.clone();

            for index in subset.into_iter().rev() {
                state = state.sacrifice_creature_at(index)?;
            }

            output.push(state);
        }

        Ok(output.into_iter())
    }

    fn sacrifice_creature_at(&self, permanent_index: PermanentIndex) -> Result<Self, SacrificeCreaturesError> {
        todo!("sacrifice_creature_at")
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

        const draw_specs: [DrawSpec; 5] = [
            NthDraw(0),
            NthDraw(1),
            NthDraw(10),
            NthDraw(100),
            NthDraw(1_000),
        ];

        for draw in draw_specs {
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

        const draw_specs: [DrawSpec; 7] = [
            NthDraw(0),
            NthDraw(1),
            NthDraw(10),
            NthDraw(100),
            NthDraw(1_000),
            NthDraw(10_000),
            NthDraw(100_000),
        ];

        for draw in draw_specs {
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

        const draw_specs: [DrawSpec; 5] = [
            NthDraw(0),
            NthDraw(1),
            NthDraw(10),
            NthDraw(100),
            NthDraw(1_000),
        ];

        for draw in draw_specs {
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

        const draw_specs: [DrawSpec; 5] = [
            NthDraw(0),
            NthDraw(1),
            NthDraw(10),
            NthDraw(100),
            NthDraw(1_000),
        ];

        for draw in draw_specs {
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