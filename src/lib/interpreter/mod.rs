pub(crate) use self::values::LoxValue;
use crate::error::LoxResult;

use super::parser::expression::Expr;

pub mod values {
    use crate::error::{LoxError, LoxResult};
    use crate::lib::token::{Keyword, Numeric, TokenKind};
    use std::convert::TryFrom;
    use std::ops::{Add, Div, Mul, Neg, Sub};

    pub(crate) enum LoxValue {
        String(String),
        Nil,
        Decimal(f64),
        Integer(isize),
        Boolean(bool),
    }

    impl LoxValue {
        pub fn is_truthy(&self) -> bool {
            match &self {
                Self::Boolean(b) => *b,
                Self::Nil => false,
                _ => true,
            }
        }

        pub fn as_int(self) -> isize {
            match self {
                Self::Decimal(d) => d as isize,
                Self::Integer(i) => i,
                _ => unreachable!(),
            }
        }

        pub fn as_dec(self) -> f64 {
            match self {
                Self::Decimal(d) => d,
                Self::Integer(i) => i as f64,
                _ => unreachable!(),
            }
        }

        pub fn is_num(&self) -> bool {
            matches!(self, Self::Integer(_) | Self::Decimal(_))
        }

        pub fn is_decimal(&self) -> bool {
            matches!(self, Self::Decimal(_))
        }

        pub fn is_string(&self) -> bool {
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

    impl Neg for LoxValue {
        type Output = Self;

        fn neg(self) -> Self::Output {
            match self {
                Self::Decimal(d) => LoxValue::Decimal(-d),
                Self::Integer(i) => LoxValue::Integer(-i),
                _ => self,
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

    macro_rules! binop {
        ($lhs:expr, $rhs:expr, $op:tt, $err:literal) => {
            {
                // make sure both values are numerical values
                check_or!(LoxValue::is_num, &$lhs, &$rhs; $err);

                // if at least one of them is decimal, then we return a decimal
                if $lhs.is_decimal() || $rhs.is_decimal() {
                    return Ok(LoxValue::Decimal(($lhs.as_dec() $op $rhs.as_dec())));
                }

                // here we definitely have two integers
                Ok(LoxValue::Integer(($lhs.as_int() $op $rhs.as_int())))
            }
        }
    }

    impl Sub for LoxValue {
        type Output = LoxResult<Self>;

        fn sub(self, rhs: Self) -> Self::Output {
            binop!(self, rhs, -, "attempt to subtract invalid integer")
        }
    }

    impl Div for LoxValue {
        type Output = LoxResult<Self>;

        fn div(self, rhs: Self) -> Self::Output {
            binop!(self, rhs, /, "attempt to divide invalid integer")
        }
    }

    impl Mul for LoxValue {
        type Output = LoxResult<Self>;

        fn mul(self, rhs: Self) -> Self::Output {
            binop!(self, rhs, *, "attempt to multiply invalid integer")
        }
    }

    impl Add for LoxValue {
        type Output = LoxResult<Self>;

        fn add(self, rhs: Self) -> Self::Output {
            // int + int
            if check!(Self::is_num, &self, &rhs) {
                return binop!(self, rhs, +, "this error should be unreachable. this is a bug.");
            }

            // concat strings (convert to string if necessary)
            if self.is_string() || rhs.is_string() {
                return Ok(LoxValue::String(format!("{}{}", self, rhs)));
            }

            Err(LoxError::Generic(
                "attempt to sum invalid types".to_string(),
            ))
        }
    }
}

pub(crate) struct Interpreter;

impl Interpreter {
    pub fn interpret(expression: Expr) -> LoxResult<()> {
        let val = expression.evaluate()?;
        println!("{}", val);
        Ok(())
    }
}
