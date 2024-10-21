use card::Card::*;
use simulate::Spec::*;

fn main() {
    // TODO accept as input
    let deck = vec![
        InsatiableAvarice,
        SchemingSymmetry,
        SchemingSymmetry,
        FeedTheSwarm,
        SignInBlood,
        StarscapeCleric,
        StarscapeCleric,
        WishclawTalisman,
        WishclawTalisman,
        WishclawTalisman,
        WishclawTalisman,
        CeaseAndDesist,
        CeaseAndDesist,
        CeaseAndDesist,
        CeaseAndDesist,
        HowlingMine,
        HowlingMine,
        JetMedallion,
        MindStone,
        MindStone,
        MindStone,
        MindStone,
        GrimTutor,
        HoodedBlightfang,
        NighthawkScavenger,
        ToxicDeluge,
        VitoThornOfTheDuskRose,
        VitoThornOfTheDuskRose,
        VitoThornOfTheDuskRose,
        BakeIntoThePie,
        EnduringTenacity,
        HagraMauling,
        SheoldredTheApocalypse,
        ExquisiteBlood,
        ExquisiteBlood,
        ExquisiteBlood,
        ExquisiteBlood,
        MemorialToFolly,
        MemorialToFolly,
        PhyrexianTower,
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
        TheDrossPits,
        TheDrossPits,
        TheDrossPits,
        TheDrossPits,
        BlastZone,
        SceneOfTheCrime,
    ];

    assert_eq!(deck.len(), 60);

    let outcome = simulate::calculate(NthDraw(0), &deck);

    println!("{outcome:?}");
}
