use std::fmt::Display;

pub enum IntervalSet<T> {
    Unbounded,
    Bounded(Vec<Interval<T>>),
}

impl<T> IntervalSet<T> {
    pub const fn unbounded() -> Self {
        Self::Unbounded
    }

    pub fn point(value: T) -> Self {
        Self::Bounded(vec![Interval::Point(value)])
    }
}

pub enum Interval<T> {
    Point(T),
    Below(Bound<T>),
    Above(Bound<T>),
    Finite(Bound<T>, Bound<T>),
}

impl<T: Display> Display for Interval<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_kind(kind: BoundKind) -> &'static str {
            match kind {
                BoundKind::Open => "",
                BoundKind::Closed => "=",
            }
        }

        match self {
            Interval::Point(value) => Display::fmt(value, f),
            Interval::Below(bound) => write!(f, "..{}{}", fmt_kind(bound.kind), bound.value),
            Interval::Above(bound) => write!(f, "{}{}..", bound.value, fmt_kind(bound.kind)),
            Interval::Finite(lower, upper) => write!(
                f,
                "{}{}..{}{}",
                lower.value,
                fmt_kind(lower.kind),
                fmt_kind(upper.kind),
                upper.value
            ),
        }
    }
}

pub struct Bound<T> {
    value: T,
    kind: BoundKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoundKind {
    Open,
    Closed,
}

/// Closed range of integers (inclusive)
pub struct IntRange {
    start: i64,
    end: i64,
}

pub struct SliceRange {
    pub start: SliceBound,
    pub end: SliceBound,
    pub step: usize,
}

impl Display for SliceRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

pub enum SliceBound {
    Start(usize),
    End(usize),
    Unbounded,
}

impl Display for SliceBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SliceBound::Start(i) => Display::fmt(i, f),
            SliceBound::End(i) => write!(f, "end - {}", *i),
            SliceBound::Unbounded => Ok(()),
        }
    }
}
