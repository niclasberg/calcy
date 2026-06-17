use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum Type {
    /// Bottom type, represents a type that could never be constructed, and which is a subtype of any type
    Never,
    Unit,
    Bool,
    BoolLiteral(bool),
    Number,
    Array,
    Fn,
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

    pub fn is_compatible_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Bool, Type::Bool) | (Type::Unit, Type::Unit) | (Type::Number, Type::Number) => {
                true
            }
            (Type::Bool, Type::BoolLiteral(_)) | (Type::BoolLiteral(_), Type::Bool) => true,
            (Type::BoolLiteral(lhs), Type::BoolLiteral(rhs)) => *lhs == *rhs,
            (_, Type::Never) => true,
            _ => false,
        }
    }

    pub fn intersect(&self, other: &Self) -> Self {
        match (self, other) {
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
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Never => f.write_str("!"),
            Type::Unit => f.write_str("Unit"),
            Type::Bool => f.write_str("Bool"),
            Type::BoolLiteral(value) => Display::fmt(value, f),
            Type::Number => f.write_str("Number"),
            Type::Array => f.write_str("Array"),
            Type::Enum(alts) => {
                for alt in alts.iter().take(alts.len() - 1) {
                    alt.fmt(f)?;
                    f.write_str(" | ")?;
                }
                alts.last().map(|alt| alt.fmt(f)).unwrap_or(Ok(()))
            }
            Type::Struct(_) => todo!(),
            Type::Fn => f.write_str("fn"),
        }
    }
}
