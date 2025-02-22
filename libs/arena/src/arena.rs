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

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ArenaVec<'arena, T> {
    vec: bumpalo::collections::vec::Vec<'arena, T>,
}

#[macro_export]
macro_rules! _vec {
    (=> $arena: expr) => {
        $crate::ArenaVec::with_capacity_in(0, $arena)
    };
    ($($exprs: expr),* $(,)? => $arena: expr) => ({
        // TODO Is there a way to get the count without evaluating the expressions twice?
        let count = 0;
        let mut output = $crate::ArenaVec::with_capacity_in(count, $arena);

        $(
            output.push($exprs);
        )*

        output
    })
}
pub use _vec as vec;

impl<'arena, T: 'arena> IntoIterator for ArenaVec<'arena, T> {
    type Item = T;
    type IntoIter = bumpalo::collections::vec::IntoIter<'arena, T>;

    #[inline]
    fn into_iter(mut self) -> Self::IntoIter {
        self.vec.into_iter()
    }
}

impl<'a, 'bump, T> IntoIterator for &'a mut ArenaVec<'bump, T> {
    type Item = &'a mut T;
    type IntoIter = std::slice::IterMut<'a, T>;

    fn into_iter(self) -> std::slice::IterMut<'a, T> {
        (&mut self.vec).into_iter()
    }
}

impl<'arena, T: 'arena> Extend<T> for ArenaVec<'arena, T> {
    #[inline]
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.vec.extend(iter)
    }
}

impl<'arena, T: 'arena> std::ops::Deref for ArenaVec<'arena, T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        self.vec.deref()
    }
}

impl<'arena, T: 'arena> std::ops::DerefMut for ArenaVec<'arena, T> {
    fn deref_mut(&mut self) -> &mut [T] {
        self.vec.deref_mut()
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

    pub fn as_slice(&self) -> &[T] {
        use std::ops::Deref;
        self.deref()
    }
}

impl <T> ArenaVec<'_, T>
where T: Copy {
    pub fn extend_from_slice_copy(&mut self, slice: &[T]) {
        self.vec.extend_from_slice_copy(slice);
    }
}