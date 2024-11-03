#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    F64(f64),
    I64(i64),
    Str(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::F64(v) => write!(f, "{v}"),
            Self::I64(v) => write!(f, "{v}"),
            Self::Str(v) => write!(f, "{v}"),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::F64(a), Self::F64(b)) => a.partial_cmp(b),
            (Self::I64(a), Self::I64(b)) => a.partial_cmp(b),
            (Self::Str(a), Self::Str(b)) => a.partial_cmp(b),
            (Self::F64(a), Self::I64(b)) => a.partial_cmp(&(*b as f64)),
            (Self::I64(a), Self::F64(b)) => (*a as f64).partial_cmp(b),
            _ => None,
        }
    }
}

impl Value {
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Self::F64(v) => Some(*v as i64),
            Self::I64(v) => Some(*v),
            Self::Str(v) => v.parse().ok(),
        }
    }
}
