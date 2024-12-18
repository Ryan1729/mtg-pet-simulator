use card::{Card, Power, Toughness};
use equiv::LooseCmp;

/// 64k turns ought to be enough for anybody!
pub type TurnNumber = u16;

pub const INITIAL_TURN_NUMBER: TurnNumber = 0;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum PermanentKind {
    Card(Card),
    Token(Card),
}

impl PermanentKind {
    pub fn card(&self) -> Card {
        use PermanentKind::*;
        match self {
            Card(card) => *card,
            Token(card) => *card,
        }
    }
}

pub type IsTapped = bool;
//pub type IsFlipped = bool;
//pub type IsFaceDown = bool;
//pub type IsPhasedOut = bool;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Permanent {
    kind: PermanentKind,
    // "CR 110.6. A permanent’s status is its physical state. There are four status categories, each of which has two
    // possible values: tapped/untapped, flipped/unflipped, face up/face down, and phased in/phased out.
    // Each permanent always has one of these values for each of these categories."
    is_tapped: IsTapped,
    //is_flipped: IsFlipped,
    //is_face_down: IsFaceDown,
    //is_phased_out: IsPhasedOut,
    override_base_power: Option<Power>,
    override_base_toughness: Option<Toughness>,
    /// Used to calculate summoning sickness.
    entered: TurnNumber,
}

impl LooseCmp for Permanent {
    fn loose_cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.kind.cmp(&other.kind)
            .then_with(|| {
                self.is_tapped.cmp(&other.is_tapped)
                    .then_with(|| {
                        self.override_base_power.cmp(&other.override_base_power)
                            .then_with(|| {
                                self.override_base_toughness.cmp(&other.override_base_toughness)
                                // ignore turn number on purpose
                            })
                    })
            })
    }
}

impl Permanent {
    pub fn card(card: Card, entered: TurnNumber) -> Self {
        Self {
            kind: PermanentKind::Card(card),
            is_tapped: Card::enters_tapped(card),
            override_base_power: <_>::default(),
            override_base_toughness: <_>::default(),
            entered,
        }
    }

    pub fn token_of(card: Card, entered: TurnNumber) -> Self {
        Self {
            kind: PermanentKind::Token(card),
            is_tapped: Card::enters_tapped(card),
            override_base_power: <_>::default(),
            override_base_toughness: <_>::default(),
            entered,
        }
    }

    pub fn with_p_t(&self, power: Power, toughness: Toughness) -> Self {
        Self {
            override_base_power: Some(power),
            override_base_toughness: Some(toughness),
            ..self.clone()
        }
    }

    pub fn power(&self) -> Option<Power> {
        use PermanentKind::*;
        self.override_base_power.or_else(|| {
            match self.kind {
                Card(card) => card.raw_power(),
                Token(card) => card.raw_power(),
            }
        })
    }

    pub fn is_a_creature(&self) -> bool {
        use PermanentKind::*;
        // If we ever implement stuff like turning intoa treasure token we'll need to change this
        match self.kind {
            Card(card) => card.is_a_creature(),
            Token(card) => card.is_a_creature(),
        }
    }

    pub fn can_attack(&self, current: TurnNumber) -> bool {
        self.is_a_creature() && (/* self.has_haste() || */ self.entered < current)
    }

    pub fn kind(&self) -> PermanentKind {
        self.kind.clone()
    }

    pub fn is_tapped(&self) -> bool {
        self.is_tapped
    }

    pub fn untapped(&self) -> Self {
        Self {
            is_tapped: false,
            ..self.clone()
        }
    }

    pub fn tapped(&self) -> Self {
        Self {
            is_tapped: true,
            ..self.clone()
        }
    }
}

#[cfg(test)]
mod power_works {
    use super::*;

    #[test]
    fn on_a_non_modified_token() {
        // Say we had Rite of Replication implmented
        let token = Permanent::token_of(Card::HoodedBlightfang, INITIAL_TURN_NUMBER);

        assert_eq!(token.power(), Some(1));
    }
}