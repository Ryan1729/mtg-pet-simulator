pub struct Arena {
    arena: bumpalo::Bump,
}

impl Arena {
    pub fn with_capacity(capacity: usize) -> Arena {
        Arena {
            arena: bumpalo::Bump::with_capacity(capacity),
        }
    }
}

pub struct ArenaVec<'arena, T> {
    vec: bumpalo::collections::vec::Vec<'arena, T>,
}

impl<'arena, T: 'arena> IntoIterator for ArenaVec<'arena, T> {
    type Item = T;
    type IntoIter = bumpalo::collections::vec::IntoIter<'arena, T>;

    #[inline]
    fn into_iter(mut self) -> Self::IntoIter {
        self.vec.into_iter()
    }
}

impl<'arena, T: 'arena> Extend<T> for ArenaVec<'arena, T> {
    #[inline]
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.vec.extend(iter)
    }
}

impl <'arena, T> ArenaVec<'arena, T> {
    pub fn with_capacity_in(capacity: usize, arena: &'arena Arena) -> Self {
        Self {
            vec: bumpalo::collections::vec::Vec::with_capacity_in(capacity, &arena.arena),
        }
    }

    pub fn push(&mut self, thing: T) {
        self.vec.push(thing);
    }
}