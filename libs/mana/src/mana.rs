pub enum ManaType {
    // White
    // Blue
    Black,
    // Red
    // Green
    Colorless
}

// 64k Mana ought to be enough for anybody!
pub type ManaAmount = u16;

// TODO? Make the empty pool unrepresentable? 
#[derive(Clone, Copy, Debug, Default)]
pub struct ManaPool {
    //pub white: ManaAmount,
    //pub blue: ManaAmount,
    pub black: ManaAmount,
    //pub red: ManaAmount,
    //pub green: ManaAmount,
    pub colorless: ManaAmount,
}