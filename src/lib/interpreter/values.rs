use crate::lib::{
    error::{LoxError, LoxResult},
    parser::Expr,
    token::{Keyword, Numeric, TokenKind},
};
use std::{
    cell::RefCell,
    cmp::PartialEq,
    collections::HashMap,
    convert::TryFrom,
    ops::{Add, Div, Mul, Neg, Sub},
    rc::Rc,
};

use super::{class::LoxInstance, Environment, LoxClass};

/// Internal language types
#[derive(Clone)]
pub(crate) enum LoxValue {
    String(String),
    Nil,
    Decimal(f64),
    Integer(isize),
    Boolean(bool),
    Array(RefCell<Vec<Rc<LoxValue>>>),
    Callable(Rc<dyn LoxCallable>),
    Instance(LoxInstance),
}

pub(crate) trait LoxCallable {
    fn call(
        &self,
        env: Rc<Environment>,
        locals: &HashMap<Expr, usize>,
        args: &[Rc<LoxValue>],
    ) -> LoxResult<Rc<LoxValue>>;
    fn arity(&self) -> usize {
        0
    }

    fn to_string(&self) -> String {
        String::from("<native fn>")
    }

    fn as_any(&self) -> &dyn std::any::Any;
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

    pub fn ge(&self, oth: &Self) -> LoxResult<LoxValue> {
        check_or!(LoxValue::is_num, self, oth; "operands must be numbers");
        cmpop!(self, oth, >=)
    }

    pub fn gt(&self, oth: &Self) -> LoxResult<LoxValue> {
        check_or!(LoxValue::is_num, self, oth; "operands must be numbers");
        cmpop!(self, oth, >)
    }
    pub fn le(&self, oth: &Self) -> LoxResult<LoxValue> {
        check_or!(LoxValue::is_num, self, oth; "operands must be numbers");
        cmpop!(self, oth, <=)
    }
    pub fn lt(&self, oth: &Self) -> LoxResult<LoxValue> {
        check_or!(LoxValue::is_num, self, oth; "operands must be numbers");
        cmpop!(self, oth, <)
    }

    fn to_int(&self) -> isize {
        match self {
            Self::Decimal(d) => *d as isize,
            Self::Integer(i) => *i,
            _ => unreachable!(),
        }
    }

    fn to_dec(&self) -> f64 {
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

    pub fn as_class(&self) -> LoxResult<&LoxClass> {
        if let Self::Callable(c) = self {
            return c
                .as_any()
                .downcast_ref::<LoxClass>()
                .ok_or_else(|| LoxError::Generic(String::from("not a class")));
        }
        Err(LoxError::Generic(String::from("not callable")))
    }

    pub fn as_instance(&self) -> LoxResult<&LoxInstance> {
        if let Self::Instance(i) = self {
            return Ok(i);
        }
        Err(LoxError::Generic(String::from("not an instance")))
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
            LoxValue::Callable(callable) => write!(f, "{}", callable.to_string()),
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Instance(instance) => {
                write!(f, "{}", instance)
            }
            LoxValue::Array(values) => {
                let values = values.borrow();
                let mut counter = 0;
                let mut printable = Vec::with_capacity(values.len());
                for value in &*values {
                    match **value {
                        LoxValue::Nil => {
                            counter += 1;
                            continue;
                        }
                        _ => {
                            if counter != 0 {
                                printable.push(format!("<{} empty values>", counter));
                                counter = 0;
                            }
                            printable.push(value.to_string());
                        }
                    }
                }
                write!(f, "[{}]", printable.join(", "))
            }
        }
    }
}
impl std::fmt::Debug for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self {
            LoxValue::String(s) => write!(f, "\"{}\"", s),
            s => write!(f, "{}", s.to_string()),
        }
    }
}

impl Sub for LoxValue {
    type Output = LoxResult<Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        check_or!(LoxValue::is_num, &self, &rhs; "operands must be numbers");

        binop!(self, rhs, -)
    }
}

impl Div for LoxValue {
    type Output = LoxResult<Self>;

    fn div(self, rhs: Self) -> Self::Output {
        check_or!(LoxValue::is_num, &self, &rhs; "operands must be numbers");
        let zero = LoxValue::Decimal(0.0);
        if self.eq(&zero) || rhs.eq(&zero) {
            return Err(LoxError::Generic("attempt to divide by zero".to_string()));
        }
        binop!(self, rhs, /)
    }
}

impl Mul for LoxValue {
    type Output = LoxResult<Self>;

    fn mul(self, rhs: Self) -> Self::Output {
        if check!(Self::is_num, &self, &rhs) {
            return binop!(self, rhs, *);
        }
        if self.is_num() || rhs.is_num() {
            let (num, oth) = if self.is_num() {
                (self, rhs)
            } else {
                (rhs, self)
            };
            if !num.is_decimal() {
                match oth {
                    LoxValue::Array(ref vec) => {
                        let new = std::iter::repeat(&(*vec.borrow()))
                            .take(num.to_int() as usize)
                            .flatten()
                            .cloned()
                            .collect::<Vec<_>>();
                        return Ok(LoxValue::Array(RefCell::new(new)));
                    }
                    LoxValue::String(s) => {
                        return Ok(LoxValue::String(s.repeat(num.to_int() as usize)))
                    }
                    _ => todo!(),
                };
            }
        }

        Err(LoxError::Generic(
            "invalid operands to multiplication expression".to_string(),
        ))
    }
}

impl Add for LoxValue {
    type Output = LoxResult<Self>;

    fn add(self, rhs: Self) -> Self::Output {
        if check!(Self::is_num, &self, &rhs) {
            return binop!(self, rhs, +);
        }

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

                let lhs = self.to_dec();
                let rhs = oth.to_dec();

                lhs.eq(&rhs)
            }
        }
    }
}
