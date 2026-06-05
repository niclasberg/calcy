use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum Type {
    /// Bottom type, represents a type that could never be constructed, and which is a subtype of any type
    Never,
    /// Top type, all other types are a subtype of this
    Any,
    Unit,
    Bool,
    Number,
    // The set must have at least 2 members
    Enum(BTreeSet<Type>),
    Struct(BTreeMap<String, Type>),
}

impl Type {
    pub fn one_of(it: impl Iterator<Item = Self>) -> Self {
        let mut values: BTreeSet<_> = it.collect();
        if values.is_empty() {
            Type::Never
        } else if values.len() == 1 {
            values.pop_first().unwrap()
        } else {
            Type::Enum(values)
        }
    }

    pub fn is_subtype_of(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Type::Any) => true,
            (Type::Bool, Type::Bool) | (Type::Unit, Type::Unit) | (Type::Number, Type::Number) => {
                true
            }
            _ => false,
        }
    }

    pub fn intersect(&self, other: &Self) -> Self {
        match (self, other) {
            (Type::Any, other) | (other, Type::Any) => other.clone(),
            (Type::Bool, Type::Bool) => Self::Bool,
            (Type::Unit, Type::Unit) => Self::Unit,
            (Type::Number, Type::Number) => Self::Number,
            (Type::Enum(e1), Type::Enum(e2)) => Self::one_of(e1.intersection(e2).cloned()),
            (Type::Enum(e), Type::Never) | (Type::Never, Type::Enum(e)) => Type::Enum(e.clone()),
            (Type::Enum(e), other) | (other, Type::Enum(e)) => {
                let mut inner = e.clone();
                inner.insert(other.clone());
                Type::Enum(inner)
            }
            (Type::Struct(fields1), Type::Struct(fields2)) => {
                let mut fields = fields1.clone();
                for (name, t2) in fields2.iter() {
                    if let Some(t) = fields.get_mut(name) {
                        *t = t.intersect(t2);
                        if *t == Type::Never {
                            return Type::Never;
                        }
                    } else {
                        fields.insert(name.clone(), t2.clone());
                    }
                }
                Type::Struct(fields)
            }
            _ => Self::Never,
        }
    }

    pub fn union(&self, other: &Self) -> Self {
        match (self, other) {
            _ => todo!(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Never => f.write_str("!"),
            Type::Unit => f.write_str("Unit"),
            Type::Any => f.write_str("Any"),
            Type::Bool => f.write_str("Bool"),
            Type::Number => f.write_str("Number"),
            Type::Enum(alts) => {
                for alt in alts.iter().take(alts.len() - 1) {
                    alt.fmt(f)?;
                    f.write_str(" | ")?;
                }
                alts.last().map(|alt| alt.fmt(f)).unwrap_or(Ok(()))
            }
            Type::Struct(_) => todo!(),
        }
    }
}
