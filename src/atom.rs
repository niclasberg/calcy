use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub struct Atom(usize);

pub struct AtomMap {
    atoms: Vec<String>,
    str_lookup: HashMap<&'static str, usize>,
}

impl AtomMap {
    pub fn insert(&mut self, str: &str) -> Atom {
        let id = self.str_lookup.entry(str).or_insert_with(|| {
            let id = self.atoms.len();
            self.atoms.push(String::from(str));
            id
        });
        Atom(*id)
    }

    pub fn get_str(&self, atom: Atom) -> &str {
        &self.atoms[atom.0]
    }
}
