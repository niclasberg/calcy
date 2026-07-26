use std::{
    collections::BTreeMap,
    fmt::{Display, Write},
};

use intervalsets::{
    Interval, IntervalSet,
    ops::{Intersects, Union},
};

use crate::{atom::Atom, expr::TypeAnnotation};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Bottom type, represents a type that could never be constructed, and which is a subtype of any type
    Never,
    Unit,
    /// Bool type or a bool literal (true/false)
    Bool(BoolType),
    Float(FloatType),
    Int(IntType),
    Array(ArrayType),
    Fn(FnType),
    Union(UnionType),
    Record(RecordType),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum BoolType {
    Any,
    Literal(bool),
}

impl BoolType {
    pub fn from_values(has_true: bool, has_false: bool) -> Option<Self> {
        match (has_true, has_false) {
            (true, true) => Some(Self::Any),
            (true, false) => Some(Self::Literal(true)),
            (false, true) => Some(Self::Literal(false)),
            _ => None,
        }
    }

    pub fn into_values(&self) -> (bool, bool) {
        match self {
            BoolType::Any => (true, true),
            BoolType::Literal(value) => (*value, !*value),
        }
    }

    pub fn is_subtype_of(&self, other: &Self) -> bool {
        *self == *other || *other == BoolType::Any
    }

    pub fn union(&self, other: &Self) -> Self {
        let (lhs_has_true, lhs_has_false) = self.into_values();
        let (rhs_has_true, rhs_has_false) = other.into_values();
        Self::from_values(lhs_has_true || rhs_has_true, lhs_has_false || rhs_has_false)
            .expect("Union of booleans should be a boolean")
    }

    pub fn intersect(&self, other: &Self) -> Option<Self> {
        let (lhs_has_true, lhs_has_false) = self.into_values();
        let (rhs_has_true, rhs_has_false) = other.into_values();
        Self::from_values(lhs_has_true && rhs_has_true, lhs_has_false && rhs_has_false)
    }

    pub fn difference(&self, other: &Self) -> Option<Self> {
        let (lhs_has_true, lhs_has_false) = self.into_values();
        let (rhs_has_true, rhs_has_false) = other.into_values();
        Self::from_values(
            lhs_has_true && !rhs_has_true,
            lhs_has_false && !rhs_has_false,
        )
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            BoolType::Any => "Bool",
            BoolType::Literal(value) => {
                if *value {
                    "true"
                } else {
                    "false"
                }
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct IntType {
    bounds: Option<IntervalSet<i64>>,
}

impl IntType {
    pub fn is_subtype_of(&self, _other: &Self) -> bool {
        true
        //self.bounds.intersects(&other.bounds)
    }

    pub fn union(&self, _other: &Self) -> Self {
        self.clone()
        /*Self {
            bounds: self.bounds.union(&other.bounds),
        }*/
    }

    pub fn intersect(&self, _other: &Self) -> Option<Self> {
        Some(self.clone())
        /*let intersection = self.bounds.intersection(&other.bounds);
        if !intersection.is_empty() {
            Some(Self {
                bounds: intersection,
            })
        } else {
            None
        }*/
    }
}

impl Display for IntType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Int")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FloatType {
    bounds: Option<IntervalSet<f64>>,
}

impl FloatType {
    pub fn is_subtype_of(&self, other: &Self) -> bool {
        // This is not correct, I think...
        if let (Some(lhs), Some(rhs)) = (&self.bounds, &other.bounds) {
            lhs.intersects(rhs)
        } else {
            true
        }
    }

    pub fn union(&self, other: &Self) -> Self {
        self.clone()
        /*Self {
            bounds: self.bounds.union(&other.bounds),
        }*/
    }

    pub fn intersect(&self, other: &Self) -> Option<Self> {
        Some(self.clone())
        /*let intersection = self.bounds.intersection(&other.bounds);
        if !intersection.is_empty() {
            Some(Self {
                bounds: intersection,
            })
        } else {
            None
        }*/
    }

    pub fn difference(&self, other: &Self) -> Option<Self> {
        None
        /*let diff = self.bounds.difference(&other.bounds);
        if !diff.is_empty() {
            Some(Self { bounds: diff })
        } else {
            None
        }*/
    }
}

impl Display for FloatType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.bounds.is_some() {
            f.write_str("ConstFloat")
        } else {
            f.write_str("Float")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub elem: Box<Type>,
    pub size_bounds: IntervalSet<usize>,
}

impl ArrayType {
    pub fn new(elem: Type, size_bounds: Interval<usize>) -> Self {
        Self {
            elem: Box::new(elem),
            size_bounds: size_bounds.into(),
        }
    }

    pub fn is_subtype_of(&self, other: &Self) -> bool {
        self.elem.is_subtype_of(&other.elem) // && other.size_bounds.intersects(&self.size_bounds)
    }

    pub fn try_union(&self, other: &Self) -> Option<Self> {
        if self.elem.is_subtype_of(&other.elem) {
            let size_bounds = self.size_bounds.union(&other.size_bounds);
            Some(Self {
                elem: self.elem.clone(),
                size_bounds,
            })
        } else {
            None
        }
    }
}

impl Display for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", &self.elem)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnType {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
}

impl FnType {
    pub fn is_subtype_of(&self, other: &Self) -> bool {
        self.args.len() == other.args.len()
            && self
                .args
                .iter()
                .zip(other.args.iter())
                .all(|(lhs_arg, rhs_arg)| rhs_arg.is_subtype_of(lhs_arg))
            && self.ret.is_subtype_of(&other.ret)
    }
}

impl Display for FnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for arg in self.args.iter() {
            write!(f, "{}, ", arg)?;
        }
        write!(f, ") => {}", &self.ret)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordType(BTreeMap<Atom, Type>);

impl RecordType {
    pub fn is_subtype_of(&self, other: &Self) -> bool {
        todo!()
    }
}

impl Display for RecordType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('{')?;
        for elem in self.0.iter() {
            write!(f, "{:?}: {}, ", elem.0, elem.1)?;
        }
        f.write_char('}')
    }
}

/// Represents the union of types, e.g. Int | Float
/// The type should always contain at least 2 types,
/// but we also use this struct while building other types, so we don't
/// enforce this as a strict invariant (right now).
#[derive(Debug, Clone, PartialEq)]
pub struct UnionType {
    bool: Option<BoolType>,
    has_unit: bool,
    int: Option<IntType>,
    float: Option<FloatType>,
    functions: Vec<FnType>,
    arrays: Vec<ArrayType>,
    records: Vec<RecordType>,
}

impl UnionType {
    const EMPTY: Self = Self {
        bool: None,
        has_unit: false,
        float: None,
        int: None,
        functions: Vec::new(),
        arrays: Vec::new(),
        records: Vec::new(),
    };

    pub fn contains(&self, other: &Type) -> bool {
        match other {
            Type::Union(other) => {
                !(other.has_unit && !self.has_unit)
                    && other.bool.as_ref().is_some_and(|b| self.contains_bool(b))
                    && other.float.as_ref().is_some_and(|f| self.contains_float(f))
                    && other.arrays.iter().all(|f| self.contains_array(f))
                    && other.functions.iter().all(|f| self.contains_fn(f))
            }
            Type::Bool(lhs) => self.contains_bool(lhs),
            Type::Unit => self.has_unit,
            Type::Float(lhs) => self.contains_float(lhs),
            Type::Array(lhs) => self.contains_array(lhs),
            Type::Never => true,
            Type::Int(int_type) => self.contains_int(int_type),
            Type::Fn(fn_type) => self.contains_fn(fn_type),
            Type::Record(record) => self.contains_record(record),
        }
    }

    pub fn contains_bool(&self, other: &BoolType) -> bool {
        self.bool.is_some_and(|b| b.is_subtype_of(other))
    }

    pub fn contains_float(&self, other: &FloatType) -> bool {
        self.float.as_ref().is_some_and(|f| f.is_subtype_of(other))
    }

    pub fn contains_int(&self, other: &IntType) -> bool {
        self.int.as_ref().is_some_and(|i| i.is_subtype_of(other))
    }

    pub fn contains_fn(&self, other: &FnType) -> bool {
        self.functions.iter().any(|f| f.is_subtype_of(other))
    }

    pub fn contains_array(&self, other: &ArrayType) -> bool {
        self.arrays.iter().any(|a| a.is_subtype_of(other))
    }

    pub fn contains_record(&self, other: &RecordType) -> bool {
        self.records.iter().any(|r| r.is_subtype_of(other))
    }

    pub fn count_types(&self) -> usize {
        self.bool.iter().count()
            + self.float.iter().count()
            + self.int.iter().count()
            + if self.has_unit { 1 } else { 0 }
            + self.functions.len()
            + self.arrays.len()
            + self.records.len()
    }
}

impl Display for UnionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        let mut write_sep = |f: &mut std::fmt::Formatter<'_>| -> std::fmt::Result {
            if !first {
                f.write_str(" | ")?;
            };
            first = false;
            Ok(())
        };

        if self.has_unit {
            write_sep(f)?;
            f.write_str("Unit")?;
        }

        if let Some(b) = &self.bool {
            write_sep(f)?;
            f.write_str(b.as_str())?;
        }

        if let Some(fl) = &self.float {
            write_sep(f)?;
            Display::fmt(fl, f)?;
        }

        if let Some(int) = &self.int {
            write_sep(f)?;
            Display::fmt(int, f)?;
        }

        for arr in self.arrays.iter() {
            write_sep(f)?;
            Display::fmt(arr, f)?;
        }

        for func in self.functions.iter() {
            write_sep(f)?;
            Display::fmt(func, f)?;
        }

        Ok(())
    }
}

pub struct TypeBuilder(UnionType);

impl TypeBuilder {
    pub fn new(initial_type: Type) -> Self {
        let mut inner = UnionType::EMPTY;

        match initial_type {
            Type::Never => {}
            Type::Unit => inner.has_unit = true,
            Type::Bool(bool_type) => inner.bool = Some(bool_type),
            Type::Float(float_type) => inner.float = Some(float_type),
            Type::Int(int_type) => inner.int = Some(int_type),
            Type::Array(array_type) => inner.arrays.push(array_type),
            Type::Fn(fn_type) => inner.functions.push(fn_type),
            Type::Union(union_type) => inner = union_type,
            Type::Record(btree_map) => todo!(),
        }

        Self(inner)
    }

    pub fn union(mut self, other: Type) -> Self {
        match other {
            Type::Never => {}
            Type::Unit => self.0.has_unit = true,
            Type::Bool(bool_type) => self.union_bool(bool_type),
            Type::Int(int_type) => self.union_int(int_type),
            Type::Float(float_type) => self.union_float(float_type),
            Type::Array(array_type) => self.union_array(array_type),
            Type::Fn(fn_type) => todo!(),
            Type::Union(union_type) => {
                if let Some(b) = union_type.bool {
                    self.union_bool(b);
                }
                if let Some(f) = union_type.float {
                    self.union_float(f);
                }
                if let Some(i) = union_type.int {
                    self.union_int(i);
                }
                for array_type in union_type.arrays {
                    self.union_array(array_type);
                }
            }
            Type::Record(btree_map) => todo!(),
        }
        self
    }

    fn union_bool(&mut self, b: BoolType) {
        if let Some(b) = &mut self.0.bool {
            *b = b.union(&b);
        } else {
            self.0.bool = Some(b);
        }
    }

    fn union_float(&mut self, float_type: FloatType) {
        if let Some(f) = &mut self.0.float {
            *f = f.union(&float_type);
        } else {
            self.0.float = Some(float_type);
        }
    }

    fn union_int(&mut self, int_type: IntType) {
        if let Some(i) = &mut self.0.int {
            *i = i.union(&int_type);
        } else {
            self.0.int = Some(int_type);
        }
    }

    fn union_array(&mut self, arr: ArrayType) {
        let arrays = &mut self.0.arrays;
        if !arrays.iter().any(|existing| arr.is_subtype_of(&existing)) {
            arrays.retain(|existing| !existing.is_subtype_of(&arr));
            arrays.push(arr);
        }
    }

    pub fn intersect(mut self, other: Type) -> Self {
        self.0 = match (other) {
            Type::Never => UnionType::EMPTY,
            Type::Bool(other) => UnionType {
                bool: self.0.bool.and_then(|b| b.intersect(&other)),
                ..UnionType::EMPTY
            },
            Type::Unit => UnionType {
                has_unit: self.0.has_unit,
                ..UnionType::EMPTY
            },
            Type::Float(other) => UnionType {
                float: self.0.float.and_then(|f| f.intersect(&other)),
                ..UnionType::EMPTY
            },
            Type::Fn(other) => {
                /*if lhs.is_subtype_of(rhs) {
                    Self::Fn(rhs.clone())
                } else if rhs.is_subtype_of(lhs) {
                    Self::Fn(lhs.clone())
                } else {
                    Self::Never
                }*/
                todo!()
            }
            Type::Int(int_type) => todo!(),
            Type::Array(array_type) => todo!(),
            Type::Union(union_type) => todo!(),
            Type::Record(btree_map) => todo!(),
        };
        self
    }

    pub fn difference(mut self, other: Type) -> Self {
        match other {
            Type::Never => {}
            Type::Unit => self.0.has_unit = false,
            Type::Bool(other) => self.diff_bool(other),
            Type::Float(other) => {
                self.0.float = self.0.float.and_then(|f| f.difference(&other));
            }
            Type::Array(array_type) => todo!(),
            Type::Fn(fn_type) => todo!(),
            Type::Union(other) => {
                if other.has_unit {
                    self.0.has_unit = false;
                }

                if let Some(bool) = other.bool {
                    self.diff_bool(bool);
                }
            }
            Type::Record(btree_map) => todo!(),
            Type::Int(int_type) => todo!(),
        }
        self
    }

    fn diff_bool(&mut self, other: BoolType) {
        self.0.bool = self.0.bool.and_then(|b| b.difference(&other));
    }

    pub fn finish(mut self) -> Type {
        match self.0.count_types() {
            0 => Type::Never,
            1 => {
                if let Some(b) = self.0.bool {
                    Type::Bool(b)
                } else if self.0.has_unit {
                    Type::Unit
                } else if let Some(f) = self.0.float {
                    Type::Float(f)
                } else if let Some(i) = self.0.int {
                    Type::Int(i)
                } else if let Some(f) = self.0.functions.pop() {
                    Type::Fn(f)
                } else {
                    unreachable!()
                }
            }
            _ => Type::Union(self.0),
        }
    }
}

impl Type {
    pub const NEVER: Self = Self::Never;
    pub const UNIT: Self = Self::Unit;
    pub const BOOL: Self = Self::Bool(BoolType::Any);
    pub const FLOAT: Self = Self::Float(FloatType { bounds: None });
    pub const INT: Self = Self::Int(IntType { bounds: todo!() });

    pub const fn bool_lit(value: bool) -> Self {
        Self::Bool(BoolType::Literal(value))
    }

    pub fn array(element_type: Type) -> Self {
        Self::Array(ArrayType::new(element_type, Interval::empty()))
    }

    pub fn func(args: Vec<Self>, ret: Self) -> Self {
        Self::Fn(FnType {
            args,
            ret: Box::new(ret),
        })
    }

    pub fn record(elems: BTreeMap<Atom, Self>) -> Self {
        Self::Record(RecordType(elems))
    }

    pub fn join(it: impl IntoIterator<Item = Self>) -> Self {
        let mut builder = TypeBuilder::new(Self::Never);
        for t in it {
            builder = builder.union(t);
        }
        builder.finish()
    }

    pub fn is_subtype_of(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Self::Never) => true,
            (Self::Unit, Self::Unit) => true,
            (Self::Float(lhs), Self::Float(rhs)) => lhs.is_subtype_of(rhs),
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs.is_subtype_of(rhs),
            (Self::Fn(lhs), Self::Fn(rhs)) => lhs.is_subtype_of(rhs),
            (Self::Array(lhs), Self::Array(rhs)) => lhs.is_subtype_of(rhs),
            (lhs, Self::Union(rhs)) => rhs.contains(lhs),
            _ => false,
        }
    }

    pub fn widen_literals(self) -> Self {
        match self {
            Self::Bool(_) => Self::BOOL,
            Self::Float(_) => Self::FLOAT,
            s => s,
        }
    }
}

impl From<&TypeAnnotation> for Type {
    fn from(value: &TypeAnnotation) -> Self {
        match value {
            TypeAnnotation::Never => Type::NEVER,
            TypeAnnotation::Unit => Type::UNIT,
            TypeAnnotation::Bool => Type::BOOL,
            TypeAnnotation::BoolLiteral(value) => Type::bool_lit(*value),
            TypeAnnotation::Float => Type::FLOAT,
            TypeAnnotation::Int => Type::INT,
            TypeAnnotation::Enum(alts) => Type::join(alts.iter().map(Self::from)),
            TypeAnnotation::Array(elem) => Type::array(elem.as_ref().into()),
            TypeAnnotation::Fn(args, ret) => {
                Type::func(args.iter().map(Self::from).collect(), ret.as_ref().into())
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Never => f.write_str("!"),
            Self::Unit => f.write_str("Unit"),
            Self::Bool(b) => f.write_str(b.as_str()),
            Self::Float(float) => Display::fmt(float, f),
            Self::Int(int) => Display::fmt(int, f),
            Self::Array(t) => Display::fmt(t, f),
            Self::Record(_) => todo!(),
            Self::Fn(func) => Display::fmt(func, f),
            Self::Union(u) => Display::fmt(u, f),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_subtypes() {
        // Bools
        assert!(Type::bool_lit(true).is_subtype_of(&Type::BOOL));
        assert!(Type::bool_lit(true).is_subtype_of(&Type::bool_lit(true)));
        assert!(!Type::bool_lit(true).is_subtype_of(&Type::bool_lit(false)));

        // Floats
        assert!(Type::FLOAT.is_subtype_of(&Type::FLOAT));

        // Arrays
        assert!(!Type::array(Type::FLOAT).is_subtype_of(&Type::BOOL));
        assert!(
            Type::array(Type::FLOAT)
                .is_subtype_of(&Type::array(Type::join([Type::BOOL, Type::FLOAT])))
        );

        // Unions
        assert!(Type::FLOAT.is_subtype_of(&Type::join([Type::FLOAT, Type::BOOL])));
        assert!(
            Type::join([Type::FLOAT, Type::BOOL])
                .is_subtype_of(&Type::join([Type::BOOL, Type::FLOAT]))
        );
    }

    #[test]
    fn test_unions() {
        assert_eq!(
            Type::join([Type::FLOAT, Type::BOOL]),
            Type::join([Type::BOOL, Type::FLOAT])
        )
    }
}
