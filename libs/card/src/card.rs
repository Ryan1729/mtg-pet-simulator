#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Card {
    // Plains
    // Island
    Swamp,
    // Mountian
    // Forest
    InsatiableAvarice,
    SchemingSymmetry,
    FeedTheSwarm,
    SignInBlood,
    StarscapeCleric,
    WishclawTalisman,
    CeaseAndDesist,
    HowlingMine,
    JetMedallion,
    MindStone,
    GrimTutor,
    HoodedBlightfang,
    NighthawkScavenger,
    ToxicDeluge,
    VitoThornOfTheDuskRose,
    BakeIntoThePie,
    EnduringTenacity,
    HagraMauling,
    SheoldredTheApocalypse,
    ExquisiteBlood,
    MemorialToFolly,
    PhyrexianTower,
    TheDrossPits,
    BlastZone,
    SceneOfTheCrime,
}

impl Card {
    /// returns true for double faced cards that are lands
    pub fn is_land(self) -> bool {
        use Card::*;
        match self {
            // Plains
            // Island
            Swamp
            // Mountian
            // Forest
            | HagraMauling
            | ExquisiteBlood
            | MemorialToFolly
            | PhyrexianTower
            | TheDrossPits
            | BlastZone
            | SceneOfTheCrime => true,
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
            | BakeIntoThePie
            | EnduringTenacity
            | SheoldredTheApocalypse => false,
        }
    }

    /// returns false for non-permanents
    /// returns true for double faced cards that are permanents that enter tapped
    pub fn enters_tapped(self) -> bool {
        use Card::*;
        match self {
            HagraMauling
            | MemorialToFolly
            | PhyrexianTower
            | TheDrossPits
            | SceneOfTheCrime => true,
            // Plains
            // Island
            Swamp
            // Mountian
            // Forest
            | BlastZone
            | ExquisiteBlood
            | InsatiableAvarice
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
            | BakeIntoThePie
            | EnduringTenacity
            | SheoldredTheApocalypse => false,
        }
    }
}