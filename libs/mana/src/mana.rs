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

/// The number of fields `ManaPool` has.
const MANA_POOL_TYPE_COUNT: usize = 2;

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ManaPool {
    //pub white: ManaAmount,
    //pub blue: ManaAmount,
    pub black: ManaAmount,
    //pub red: ManaAmount,
    //pub green: ManaAmount,
    pub colorless: ManaAmount,
}

#[macro_export]
macro_rules! _mp {
    () => {
        $crate::ManaPool::default()
    };
    (W $($tokens: tt)*) => {
        $crate::ManaPool{ white: 1, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    (U $($tokens: tt)*) => {
        $crate::ManaPool{ blue: 1, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    (B $($tokens: tt)*) => {
        $crate::ManaPool{ black: 1, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    (R $($tokens: tt)*) => {
        $crate::ManaPool{ red: 1, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    (G $($tokens: tt)*) => {
        $crate::ManaPool{ green: 1, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    // Not really needed, but why add this case too?
    (C $($tokens: tt)*) => {
        $crate::ManaPool{ colorless: 1, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    (0 $($tokens: tt)*) => {
        mp!($($tokens)*)
    };
    (1 $($tokens: tt)*) => {
        $crate::ManaPool{ colorless: 1, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    (2 $($tokens: tt)*) => {
        $crate::ManaPool{ colorless: 2, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    (3 $($tokens: tt)*) => {
        $crate::ManaPool{ colorless: 3, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    (4 $($tokens: tt)*) => {
        $crate::ManaPool{ colorless: 4, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    (5 $($tokens: tt)*) => {
        $crate::ManaPool{ colorless: 5, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    (6 $($tokens: tt)*) => {
        $crate::ManaPool{ colorless: 6, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    (7 $($tokens: tt)*) => {
        $crate::ManaPool{ colorless: 7, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    (8 $($tokens: tt)*) => {
        $crate::ManaPool{ colorless: 8, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
    (9 $($tokens: tt)*) => {
        $crate::ManaPool{ colorless: 9, ..$crate::ManaPool::default() }.add(mp!($($tokens)*)).unwrap()
    };
}
// Trick to make this usable above the def
pub use _mp as mp;

pub type SpendError = ();

impl ManaPool {
    pub fn is_empty(&self) -> bool {
        self == &Self::default()
    }

    pub fn spend(self, mut cost: ManaCost) -> Result<impl Iterator<Item = Self>, SpendError> {
        let besides_generic = Self {
            //white: self.white.checked_sub(cost.white).ok_or(())?,
            //blue: self.blue.checked_sub(cost.blue).ok_or(())?,
            black: self.black.checked_sub(cost.black).ok_or(())?,
            //red: self.red.checked_sub(cost.red).ok_or(())?,
            //green: self.green.checked_sub(cost.green).ok_or(())?,
            colorless: self.colorless.checked_sub(cost.colorless).ok_or(())?,
        };

        let mut output: Vec<Self> = Vec::with_capacity(
            1 + cost.generic as usize * MANA_POOL_TYPE_COUNT,
        );
        output.push(besides_generic);

        let mut start_index = 0;

        while cost.generic > 0 {
            cost.generic = cost.generic.checked_sub(1).ok_or(())?;

            let previous_index = start_index;
            start_index = output.len();

            for i in previous_index..start_index {
                let current = output[i];

                if let Some(colorless) = current.colorless.checked_sub(1) {
                    output.push(
                        Self {
                            colorless,
                            ..current
                        }
                    );
                }

                if let Some(black) = current.black.checked_sub(1) {
                    output.push(
                        Self {
                            black,
                            ..current
                        }
                    );
                }
            }
        }

        // We'll see if this shows up on the profile. If so, consider a VecDeque, or custom deduping iterator.
        output.drain(..start_index);

        output.sort();
        output.dedup();

        Ok(output.into_iter())
    }
}

#[cfg(test)]
mod spend_works {
    use super::*;

    #[test]
    fn on_these_examples() {
        macro_rules! a {
            ($pool: ident - $cost: expr => $expected: expr) => {
                let actual_raw = $pool.spend($cost);

                let mut actual_owned = actual_raw
                    .map(|iter| {
                        let mut a = iter.collect::<Vec<_>>();
                        a.sort();
                        a
                    });

                let actual: Result<&[_], _> = match actual_owned {
                    Ok(ref ok) => Ok(ok),
                    Err(e) => Err(e),
                };

                let expected: &[_] = &$expected;

                assert_eq!(
                    actual,
                    Ok(expected),
                );
            }
        }

        {
            let base = mp!(1 B);

            a!(base - mc!() => [base]);
            a!(base - mc!(1) => [mp!(1), mp!(B)]);
        }
        {
            let base = mp!(1 B B);

            a!(base - mc!() => [base]);
            a!(base - mc!(1) => [mp!(1 B), mp!(B B)]);
            a!(base - mc!(2) => [mp!(1), mp!(B)]);
        }
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

macro_rules! _mc {
    () => {
        $crate::ManaCost::default()
    };
    (W $($tokens: tt)*) => {
        $crate::ManaCost{ white: 1, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (U $($tokens: tt)*) => {
        $crate::ManaCost{ blue: 1, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (B $($tokens: tt)*) => {
        $crate::ManaCost{ black: 1, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (R $($tokens: tt)*) => {
        $crate::ManaCost{ red: 1, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (G $($tokens: tt)*) => {
        $crate::ManaCost{ green: 1, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (C $($tokens: tt)*) => {
        $crate::ManaCost{ colorless: 1, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (0 $($tokens: tt)*) => {
        mc!($($tokens)*)
    };
    (1 $($tokens: tt)*) => {
        $crate::ManaCost{ generic: 1, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (2 $($tokens: tt)*) => {
        $crate::ManaCost{ generic: 2, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (3 $($tokens: tt)*) => {
        $crate::ManaCost{ generic: 3, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (4 $($tokens: tt)*) => {
        $crate::ManaCost{ generic: 4, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (5 $($tokens: tt)*) => {
        $crate::ManaCost{ generic: 5, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (6 $($tokens: tt)*) => {
        $crate::ManaCost{ generic: 6, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (7 $($tokens: tt)*) => {
        $crate::ManaCost{ generic: 7, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (8 $($tokens: tt)*) => {
        $crate::ManaCost{ generic: 8, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
    (9 $($tokens: tt)*) => {
        $crate::ManaCost{ generic: 9, ..$crate::ManaCost::default() }.add(mc!($($tokens)*)).unwrap()
    };
}
// Trick to make this usable above the def
use _mc as mc;

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