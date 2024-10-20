use card::Card::*;
use simulate::Spec::*;

fn main() {
    // TODO accept as input
    let deck = vec![
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
        // TODO rest of deck
    ];

    let outcome = simulate::calculate(NthDraw(0), &deck);

    println!("{outcome:?}");
}
