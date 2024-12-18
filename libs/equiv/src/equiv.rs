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

#[cfg(test)]
mod loose_cmp_equiv_works {
    use super::*;

    struct Example;

    impl LooseCmp for Example {
        fn loose_cmp(&self, other: &Self) -> Ordering {
            Ordering::Equal
        }
    }

    #[test]
    fn on_partial_ord() {
        assert_eq!(Equiv(Example).partial_cmp(&Equiv(Example)), Some(Ordering::Equal))
    }
}

impl <A> Equiv<A>
where Equiv<A>: Ord
{
    pub fn binary_search_in(&self, haystack: &[Equiv<A>]) -> Result<usize, usize> {
        haystack.binary_search(self)
    }
}

#[cfg(test)]
mod binary_search_in_works {
    use super::*;

    struct Example(u8);

    impl LooseCmp for Example {
        fn loose_cmp(&self, other: &Self) -> Ordering {
            (self.0 & 0b1111_1110).cmp(&(other.0 & 0b1111_1110))
        }
    }

    #[test]
    fn on_partial_ord() {
        assert_eq!(Equiv(Example(1)).binary_search_in(&[Equiv(Example(0))]), Ok(0));
        assert_eq!(Equiv(Example(1)).binary_search_in(&[Equiv(Example(1))]), Ok(0));
        assert_eq!(Equiv(Example(1)).binary_search_in(&[Equiv(Example(2))]), Err(0));
    }
}

impl <A> Ord for Equiv<&[A]>
where A: LooseCmp + Ord {
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
            Ord::cmp(&self.0, &other.0)
        }
    }
}

impl <A> PartialOrd for Equiv<&[A]> 
where A: LooseCmp + Ord {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl <A> PartialEq for Equiv<&[A]> 
where A: LooseCmp + Ord {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl <A> Eq for Equiv<&[A]> where A: LooseCmp + Ord {}

#[cfg(test)]
mod slice_equiv_works {
    use super::*;

    impl LooseCmp for i32 {
        fn loose_cmp(&self, other: &Self) -> Ordering {
            self.cmp(other)
        }
    }

    #[test]
    fn on_partial_ord() {
        fn assert_partial_ord<T: PartialOrd>() {}
        assert_partial_ord::<Equiv<&[i32]>>();

        let example: Equiv<&[i32]> = Equiv(&[1]);

        assert_eq!(example.partial_cmp(&example), Some(Ordering::Equal))
    }

    #[test]
    fn on_partial_eq() {
        fn assert_partial_eq<T: PartialEq>(){}
        assert_partial_eq::<Equiv<&[i32]>>();

        let example: Equiv<&[i32]> = Equiv(&[1]);

        assert!(example.eq(&example))
    }

    #[test]
    fn on_ord() {
        fn assert_ord<T: Ord>() {}
        assert_ord::<Equiv<&[i32]>>();

        let example: Equiv<&[i32]> = Equiv(&[1]);
        let less: Equiv<&[i32]> = Equiv(&[0]);

        assert_eq!(example.cmp(&example), Ordering::Equal);
        assert_eq!(less.cmp(&example), Ordering::Less);
    }
}
