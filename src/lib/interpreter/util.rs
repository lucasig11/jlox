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
macro_rules! binop {
    ($lhs:expr, $rhs:expr, $op:tt) => {
        {
            // if at least one of them is decimal, then we return a decimal
            if $lhs.is_decimal() || $rhs.is_decimal() {
                return Ok(LoxValue::Decimal(($lhs.to_dec() $op $rhs.to_dec())));
            }

            // here we definitely have two integers
            Ok(LoxValue::Integer(($lhs.to_int() $op $rhs.to_int())))
        }
    }
}
macro_rules! cmpop {
    ($lhs:expr, $rhs:expr, $op:tt) => {
        {
            Ok(LoxValue::Boolean(($lhs.to_dec() $op $rhs.to_dec())))
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
