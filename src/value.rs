#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    F64(f64),
    I64(i64),
    Str(String),
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
        use Value::*;
        match (self, other) {
            (F64(lhs), F64(rhs)) => lhs.partial_cmp(rhs),
            (I64(lhs), I64(rhs)) => lhs.partial_cmp(rhs),
            (Str(lhs), Str(rhs)) => lhs.partial_cmp(rhs),
            (I64(lhs), F64(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (F64(lhs), I64(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            _ => panic!("Can't compare different types"),
        }
    }
}

fn binary_op_str(
    lhs: &Value,
    rhs: &Value,
    d: impl Fn(f64, f64) -> f64,
    i: impl Fn(i64, i64) -> i64,
    s: impl Fn(&str, &str) -> String,
) -> Value {
    use Value::*;
    match (lhs, rhs) {
        (F64(lhs), rhs) => F64(d(*lhs, coerce_f64(rhs))),
        (lhs, F64(rhs)) => F64(d(coerce_f64(lhs), *rhs)),
        (I64(lhs), I64(rhs)) => I64(i(*lhs, *rhs)),
        (Str(lhs), Str(rhs)) => Str(s(lhs, rhs)),
        _ => panic!(
            "Can't operate on different types on {:?} and {:?}",
            lhs, rhs
        ),
    }
}

pub fn coerce_f64(a: &Value) -> f64 {
    match a {
        Value::F64(v) => *v,
        Value::I64(v) => *v as f64,
        Value::Str(_) => panic!("Can't coerce string to f64"),
    }
}

pub fn coerce_i64(a: &Value) -> i64 {
    match a {
        Value::F64(v) => *v as i64,
        Value::I64(v) => *v,
        Value::Str(_) => panic!("Can't coerce string to i64"),
    }
}

pub fn coerce_str(a: &Value) -> String {
    match a {
        Value::F64(v) => v.to_string(),
        Value::I64(v) => v.to_string(),
        Value::Str(v) => v.clone(),
    }
}

impl std::ops::Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        binary_op_str(&self, &rhs, f64::add, i64::add, |lhs, rhs| {
            lhs.to_owned() + rhs
        })
    }
}
impl std::ops::Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        binary_op_str(&self, &rhs, f64::sub, i64::sub, |_lhs, _rhs| {
            panic!("Can't subtract strings")
        })
    }
}
impl std::ops::Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        binary_op_str(&self, &rhs, f64::mul, i64::mul, |_lhs, _rhs| {
            panic!("Can't multiple strings")
        })
    }
}
impl std::ops::Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        binary_op_str(&self, &rhs, f64::div, i64::div, |_lhs, _rhs| {
            panic!("Can't divide strings")
        })
    }
}
