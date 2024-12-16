use core::cmp::Ordering;

#[macro_export]
macro_rules! _ord_subtrait_boilerplate {
    ($type: ty) => {
        impl PartialOrd for $type {
            fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }
        
        impl PartialEq for $type {
            fn eq(&self, other: &Self) -> bool {
                self.cmp(other) == core::cmp::Ordering::Equal
            }
        }
        
        impl Eq for $type {}
    }
}
pub use _ord_subtrait_boilerplate as ord_subtrait_boilerplate;

pub trait LooseCmp {
    fn loose_cmp(&self, other: &Self) -> Ordering;
}

impl <A> LooseCmp for &A
where A: LooseCmp {
    fn loose_cmp(&self, other: &Self) -> Ordering {
        (*self).loose_cmp(&*other)
    }
}

/// This is used to only retain one element of the given
/// equivalence class.
#[derive(Debug)]
pub struct Equiv<A>(pub A);

impl <A> Ord for Equiv<A>
where A: LooseCmp {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.loose_cmp(&other.0)
    }
}

impl <A> PartialOrd for Equiv<A>
where A: LooseCmp {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl <A> PartialEq for Equiv<A>
where A: LooseCmp {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl <A> Eq for Equiv<A> where A: LooseCmp {}

impl <A> Equiv<A>
where Equiv<A>: Ord
{
    pub fn binary_search_in(&self, haystack: &[Equiv<A>]) -> Result<usize, usize> {
        match haystack.binary_search(self) {
            Ok(i) => Ok(i),
            Err(i) => {
                if haystack.get(i) == Some(self) {
                    Ok(i)
                } else {
                    Err(i)
                }
            }
        }
    }
}

impl <A> Ord for Equiv<&[A]>
where A: LooseCmp {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.0.len() == other.0.len() {
            let len = self.0.len();
            for i in 0..len {
                let cmpped = Equiv(&self.0[i]).cmp(&Equiv(&other.0[i]));
                if cmpped != core::cmp::Ordering::Equal {
                    return cmpped
                }
            }
            core::cmp::Ordering::Equal
        } else {
            assert!(false, "Unexpected case hit in Ord for Equiv<&[A]>");
            // Is this good enough, or should we compare prefixes to get 
            // a better order?
            self.0.len().cmp(&other.0.len())
            // Maybe just
            // self.0.cmp(&other.0)
        }
    }
}

impl <A> PartialOrd for Equiv<&[A]> 
where A: LooseCmp {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl <A> PartialEq for Equiv<&[A]> 
where A: LooseCmp {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl <A> Eq for Equiv<&[A]> where A: LooseCmp {}
