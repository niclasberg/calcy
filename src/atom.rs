use lasso::Rodeo;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[repr(transparent)]
pub struct Atom(usize);

unsafe impl lasso::Key for Atom {
    fn into_usize(self) -> usize {
        self.0
    }

    fn try_from_usize(int: usize) -> Option<Self> {
        Some(Self(int))
    }
}

pub type Atoms = Rodeo<Atom>;
