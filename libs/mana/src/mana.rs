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

#[derive(Clone, Copy, Debug, Default)]
pub struct ManaPool {
    //pub white: ManaAmount,
    //pub blue: ManaAmount,
    pub black: ManaAmount,
    //pub red: ManaAmount,
    //pub green: ManaAmount,
    pub colorless: ManaAmount,
}

// TODO will there be a reason to make these separate later? Just making operation sets separate?
pub type ManaCost = ManaPool;

pub type SpendError = ();

impl ManaPool {
    pub fn spend(self, cost: ManaCost) -> Result<Self, SpendError> {
        Ok(Self {
            //white: self.white.checked_sub(cost.white).ok_or(())?,
            //blue: self.blue.checked_sub(cost.blue).ok_or(())?,
            black: self.black.checked_sub(cost.black).ok_or(())?,
            //red: self.red.checked_sub(cost.red).ok_or(())?,
            //green: self.green.checked_sub(cost.green).ok_or(())?,
            colorless: self.colorless.checked_sub(cost.colorless).ok_or(())?,
        })
    }
}