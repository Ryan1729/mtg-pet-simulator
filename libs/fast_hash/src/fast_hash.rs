pub use ahash::{HashSet};

pub trait HashSetExt : ahash:: HashSetExt {
    fn with_capacity_and_seed(capacity: usize, seed: usize) -> Self;
}

impl <K> HashSetExt for HashSet<K> {
    fn with_capacity_and_seed(capacity: usize, seed: usize) -> Self {
        HashSet::with_capacity_and_hasher(capacity, ahash::RandomState::with_seed(seed))
    }
}