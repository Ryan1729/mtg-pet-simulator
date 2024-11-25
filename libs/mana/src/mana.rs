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

pub type SpendError = ();

impl ManaPool {
    pub fn spend(self, cost: ManaCost) -> Result<Self, SpendError> {
        let besides_generic = Self {
            //white: self.white.checked_sub(cost.white).ok_or(())?,
            //blue: self.blue.checked_sub(cost.blue).ok_or(())?,
            black: self.black.checked_sub(cost.black).ok_or(())?,
            //red: self.red.checked_sub(cost.red).ok_or(())?,
            //green: self.green.checked_sub(cost.green).ok_or(())?,
            colorless: self.colorless.checked_sub(cost.colorless).ok_or(())?,
        };

        if cost.generic > 0 {
            todo!("spending generic costs");
        }

        Ok(besides_generic)
    }
}

pub type AddError = ();

impl ManaPool {
    pub fn add(self, pool: ManaPool) -> Result<Self, AddError> {
        Ok(Self {
            //white: self.white.checked_add(pool.white).ok_or(())?,
            //blue: self.blue.checked_add(pool.blue).ok_or(())?,
            black: self.black.checked_add(pool.black).ok_or(())?,
            //red: self.red.checked_add(pool.red).ok_or(())?,
            //green: self.green.checked_add(pool.green).ok_or(())?,
            colorless: self.colorless.checked_add(pool.colorless).ok_or(())?,
        })
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct ManaCost {
    //pub white: ManaAmount,
    //pub blue: ManaAmount,
    pub black: ManaAmount,
    //pub red: ManaAmount,
    //pub green: ManaAmount,
    pub colorless: ManaAmount,
    pub generic: ManaAmount,
}

impl ManaCost {
    pub fn add(self, cost: ManaCost) -> Result<Self, AddError> {
        Ok(Self {
            //white: self.white.checked_add(cost.white).ok_or(())?,
            //blue: self.blue.checked_add(cost.blue).ok_or(())?,
            black: self.black.checked_add(cost.black).ok_or(())?,
            //red: self.red.checked_add(cost.red).ok_or(())?,
            //green: self.green.checked_add(cost.green).ok_or(())?,
            colorless: self.colorless.checked_add(cost.colorless).ok_or(())?,
            generic: self.generic.checked_add(cost.generic).ok_or(())?,
        })
    }
}