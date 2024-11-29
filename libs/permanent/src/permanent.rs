use card::{Card};

pub type Power = u8;
pub type Toughness = u8;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum PermanentKind {
    Card(Card),
    Token(Card),
}

pub type IsTapped = bool;
//pub type IsFlipped = bool;
//pub type IsFaceDown = bool;
//pub type IsPhasedOut = bool;

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
    override_base_power: Option<Power>,
    override_base_toughness: Option<Toughness>,
}

impl Permanent {
    pub fn card(card: Card) -> Self {
        Self {
            kind: PermanentKind::Card(card),
            is_tapped: Card::enters_tapped(card),
            override_base_power: <_>::default(),
            override_base_toughness: <_>::default(),
        }
    }

    pub fn token_of(card: Card) -> Self {
        Self {
            kind: PermanentKind::Token(card),
            is_tapped: Card::enters_tapped(card),
            override_base_power: <_>::default(),
            override_base_toughness: <_>::default(),
        }
    }

    pub fn with_p_t(&self, power: Power, toughness: Toughness) -> Self {
        Self {
            override_base_power: Some(power),
            override_base_toughness: Some(toughness),
            ..self.clone()
        }
    }

    pub fn is_a_creature(&self) -> bool {
        use PermanentKind::*;
        // If we ever implement stuff like turning intoa treasure token we'll need to change this
        match self.kind {
            Card(card) => card.is_a_creature(),
            Token(card) => card.is_a_creature(),
        }
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
