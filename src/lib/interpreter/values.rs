use crate::error::{LoxError, LoxResult};
use crate::lib::token::{Keyword, Numeric, TokenKind};
use std::cmp::PartialEq;
use std::convert::TryFrom;
use std::ops::{Add, Div, Mul, Neg, Sub};

/// Internal language types
pub(crate) enum LoxValue {
    String(String),
    Nil,
    Decimal(f64),
    Integer(isize),
    Boolean(bool),
}

impl LoxValue {
    /// Check if a value is `truthy` or not.
    /// `false` and `nil` are falsy, everything else is truthy.
    pub fn is_truthy(&self) -> bool {
        match &self {
            Self::Boolean(b) => *b,
            Self::Nil => false,
            _ => true,
        }
    }

    fn into_int(self) -> isize {
        match self {
            Self::Decimal(d) => d as isize,
            Self::Integer(i) => i,
            _ => unreachable!(),
        }
    }

    fn into_dec(&self) -> f64 {
        match self {
            Self::Decimal(d) => *d,
            Self::Integer(i) => *i as f64,
            _ => unreachable!(),
        }
    }

    fn is_num(&self) -> bool {
        matches!(self, Self::Integer(_) | Self::Decimal(_))
    }

    fn is_decimal(&self) -> bool {
        matches!(self, Self::Decimal(_))
    }

    fn is_string(&self) -> bool {
        matches!(self, Self::String(_))
    }
}

impl TryFrom<&TokenKind> for LoxValue {
    type Error = &'static str;
    fn try_from(t: &TokenKind) -> Result<Self, Self::Error> {
        match t {
            TokenKind::StringLiteral(s) => Ok(LoxValue::String(s.to_string())),
            TokenKind::BooleanLiteral(b) => Ok(LoxValue::Boolean(*b)),
            TokenKind::NumericLiteral(Numeric::Integer(i)) => Ok(LoxValue::Integer(*i)),
            TokenKind::NumericLiteral(Numeric::Decimal(d)) => Ok(LoxValue::Decimal(*d)),
            TokenKind::Keyword(Keyword::Nil) => Ok(LoxValue::Nil),
            _ => Err("could not convert token to LoxValue"),
        }
    }
}

impl std::fmt::Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self {
            LoxValue::Decimal(d) => write!(f, "{}", d),
            LoxValue::Integer(i) => write!(f, "{}", i),
            LoxValue::Boolean(b) => write!(f, "{}", b),
            LoxValue::String(s) => write!(f, "{}", s),
            LoxValue::Nil => write!(f, "nil"),
        }
    }
}

macro_rules! check {
        ($test:expr, $($val:expr),+) => {
            {
                let mut check = true;
                $(
                    if !$test($val) {
                        check = false;
                    }
                )+
                check
            }
        };
    }

/// Test each val, if the test returns false, returns an Error.
/// Otherwise, do nothing
macro_rules! check_or {
        ($test:expr, $($val:expr),+;$msg:literal) => {
            {
                $(
                    if !$test($val) {
                        return Err(LoxError::Generic($msg.to_string()));
                    }
                )*
            }
        };
    }

/// Performs a binary operation between two `[LoxValue]`s, using the given operator.
///
/// # Example
///
/// ```
/// let n = binop!(LoxValue::Decimal(10.0), LoxValue::Integer(1), -);
/// assert_eq!(n, Ok(LoxValue::Decimal(9.0)));
/// ```
macro_rules! binop {
        ($lhs:expr, $rhs:expr, $op:tt) => {
            {
                // if at least one of them is decimal, then we return a decimal
                if $lhs.is_decimal() || $rhs.is_decimal() {
                    return Ok(LoxValue::Decimal(($lhs.into_dec() $op $rhs.into_dec())));
                }

                // here we definitely have two integers
                Ok(LoxValue::Integer(($lhs.into_int() $op $rhs.into_int())))
            }
        }
    }

impl Sub for LoxValue {
    type Output = LoxResult<Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        // make sure both values are numerical values
        check_or!(LoxValue::is_num, &self, &rhs; "operands must be numbers");

        binop!(self, rhs, -)
    }
}

impl Div for LoxValue {
    type Output = LoxResult<Self>;

    fn div(self, rhs: Self) -> Self::Output {
        // make sure both values are numerical values
        check_or!(LoxValue::is_num, &self, &rhs; "operands must be numbers");

        binop!(self, rhs, /)
    }
}

impl Mul for LoxValue {
    type Output = LoxResult<Self>;

    fn mul(self, rhs: Self) -> Self::Output {
        // make sure both values are numerical values
        check_or!(LoxValue::is_num, &self, &rhs; "operands must be numbers");

        binop!(self, rhs, *)
    }
}

impl Add for LoxValue {
    type Output = LoxResult<Self>;

    fn add(self, rhs: Self) -> Self::Output {
        // int + int
        if check!(Self::is_num, &self, &rhs) {
            return binop!(self, rhs, +);
        }

        // concat strings (convert to string if necessary)
        if self.is_string() || rhs.is_string() {
            return Ok(LoxValue::String(format!("{}{}", self, rhs)));
        }

        Err(LoxError::Generic(
            "operands must be number or string".to_string(),
        ))
    }
}

impl Neg for LoxValue {
    type Output = LoxResult<Self>;

    fn neg(self) -> Self::Output {
        match self {
            Self::Decimal(d) => Ok(LoxValue::Decimal(-d)),
            Self::Integer(i) => Ok(LoxValue::Integer(-i)),
            _ => Err(LoxError::Generic(
                "unary operand must be a number".to_string(),
            )),
        }
    }
}

// Returns true if all the values match the pattern
macro_rules! multi_matches {
    ($expression:pat, $($val:expr),+) => {
        {
            let mut ret = true;
            $(ret &= matches!($val, $expression);)+
            ret
        }
    }
}

// Nil, Nil => true
// String, String => cmp.string,
// Int, int = cmp.int,
// bool, bool, => cmp.bool,
// _ => false
impl PartialEq for LoxValue {
    fn eq(&self, oth: &Self) -> bool {
        if multi_matches!(&LoxValue::Nil, &self, &oth) {
            return true;
        }

        match &self {
            LoxValue::Nil => false,
            LoxValue::String(s) => {
                if let LoxValue::String(oth) = oth {
                    return s.eq(oth);
                }
                false
            }
            LoxValue::Boolean(b) => {
                if let LoxValue::Boolean(oth) = oth {
                    return b.eq(oth);
                }
                false
            }
            _ => {
                if !oth.is_num() {
                    return false;
                }

                let lhs = self.into_dec();
                let rhs = oth.into_dec();

                lhs.eq(&rhs)
            }
        }
    }
}
