use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub struct Atom(usize);

pub struct AtomMap<'s> {
    atoms: Vec<&'s str>,
    str_lookup: HashMap<&'s str, usize>,
}

impl<'s> AtomMap<'s> {
    pub fn get_or_insert(&mut self, str: &'s str) -> Atom {
        let id = self.str_lookup.entry(str).or_insert_with(|| {
            let id = self.atoms.len();
            self.atoms.push(str);
            id
        });
        Atom(*id)
    }

    pub fn get(&self, atom: Atom) -> &str {
        &self.atoms[atom.0]
    }
}
