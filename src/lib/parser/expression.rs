use crate::lib::{
    error::*,
    interpreter::{Environment, LoxValue},
    position::Span,
    token::{Keyword, Punctuator, Token, TokenKind},
};
use std::{collections::HashMap, convert::TryInto, rc::Rc};

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
/// Language expressions
#[allow(dead_code)]
pub(crate) enum Expr {
    /// Binary expression (Expr, Operator, Expr)
    Binary(Box<Expr>, Token, Box<Expr>),
    /// Unary expression (op: Token, rhs: Expr)
    Unary(Token, Box<Expr>),
    /// Assign expression (name: Token, value: Expression)
    Assign(Token, Box<Expr>),
    /// Call expression (callee: Expr, Token: paren, args: Vec<Expr>)
    Call(Box<Expr>, Token, Vec<Expr>),
    /// Class `get` expression (object: Expr, name: Token)
    Get(Box<Expr>, Token),
    /// Class set expression (object: Expr, name: Token, value: Expr)
    Set(Box<Expr>, Token, Box<Expr>),
    /// Represents the parentheses groups
    Grouping(Box<Expr>),
    /// Literal values
    Literal(Token),
    /// Logical expression (lhs: Expr, op: Token, rhs: Expr)
    Logical(Box<Expr>, Token, Box<Expr>),
    /// Super expression (keyword: Token, method: Token)
    Super(Token, Token),
    /// Class `this` expression
    This(Token),
    /// Variable expression (name: Token)
    Variable(Token),
}

impl Expr {
    pub fn evaluate(
        &self,
        env: Rc<Environment>,
        locals: &HashMap<Expr, usize>,
    ) -> LoxResult<LoxValue> {
        let pos = &self.position();
        match self {
            Expr::Literal(tk) => tk
                .kind()
                .try_into()
                .map_err(|e: &str| InnerError::new(*pos, e).into()),
            Expr::Grouping(expr) => (*expr).evaluate(env, locals),
            Expr::Unary(op, rhs) => {
                let rhs = rhs.evaluate(env, locals)?;

                use Punctuator::*;

                match *op.kind() {
                    TokenKind::Punctuator(Sub) => {
                        Ok((-rhs).map_err(|e: LoxError| InnerError::new(*pos, &e.to_string()))?)
                    }
                    TokenKind::Punctuator(Not) => Ok(LoxValue::Boolean(!rhs.is_truthy())),
                    _ => Err(InnerError::new(
                        *pos,
                        "attempt to evaluate an invalid unary expression",
                    )
                    .into()),
                }
            }

            Expr::Binary(lhs, op, rhs) => {
                let lhs = lhs.evaluate(Rc::clone(&env), locals)?;
                let rhs = rhs.evaluate(env, locals)?;
                use Punctuator::*;
                let result = match *op.kind() {
                    TokenKind::Punctuator(Sub) => lhs - rhs,
                    TokenKind::Punctuator(Mul) => lhs * rhs,
                    TokenKind::Punctuator(Div) => lhs / rhs,
                    TokenKind::Punctuator(Add) => lhs + rhs,
                    TokenKind::Punctuator(GreaterThan) => lhs.gt(&rhs),
                    TokenKind::Punctuator(GreaterThanOrEq) => lhs.ge(&rhs),
                    TokenKind::Punctuator(LessThan) => lhs.lt(&rhs),
                    TokenKind::Punctuator(LessThanOrEq) => lhs.le(&rhs),
                    TokenKind::Punctuator(Eq) => Ok(LoxValue::Boolean(lhs == rhs)),
                    TokenKind::Punctuator(NotEq) => Ok(LoxValue::Boolean(lhs != rhs)),
                    _ => Err(InnerError::new(
                        *pos,
                        "attempt to evaluate an invalid binary expression. this is probably a bug.",
                    )
                    .into()),
                };
                result.map_err(|e: LoxError| InnerError::new(*pos, &e.to_string()).into())
            }

            Expr::Logical(lhs, op, rhs) => {
                let lhs = lhs.evaluate(Rc::clone(&env), locals)?;

                if let TokenKind::Keyword(Keyword::Or) = *op.kind() {
                    if lhs.is_truthy() {
                        return Ok(lhs);
                    }
                } else if !lhs.is_truthy() {
                    return Ok(lhs);
                }

                rhs.evaluate(env, locals)
            }
            Expr::Variable(ref name) => {
                if let Some(idx) = locals.get(self) {
                    return env.get_at(*idx, &name.to_string());
                }
                env.global()
                    .get(&name.to_string())
                    .map_err(|e| InnerError::new(*pos, &e.to_string()).into())
            }
            Expr::Assign(name, val) => {
                let val = val.evaluate(Rc::clone(&env), locals)?;

                if let Some(idx) = locals.get(self) {
                    env.assign_at(*idx, &name.to_string(), &val)?;
                    Ok(val)
                } else {
                    return env
                        .global()
                        .assign(&name.to_string(), &val)
                        .map_err(|e| InnerError::new(*pos, &e.to_string()).into());
                }
            }

            Expr::Call(callee, _, args) => {
                let callee = callee.evaluate(Rc::clone(&env), locals)?;
                let args: Vec<_> = args
                    .iter()
                    .map(|arg| arg.evaluate(Rc::clone(&env), locals))
                    .collect::<LoxResult<_>>()?;

                if let LoxValue::Callable(c) = callee {
                    if c.arity() != args.len() {
                        return Err(InnerError::new(
                            *pos,
                            &format!("expected {} arguments, got {}", c.arity(), args.len()),
                        )
                        .into());
                    }
                    return c.call(env, locals, &args);
                }
                Err(InnerError::new(*pos, "can only call functions or class constructors").into())
            }
            Expr::Get(object, name) => {
                let object = object.evaluate(env, locals)?;
                if let LoxValue::Instance(i) = object {
                    return i.get(name);
                }
                Err(InnerError::new(*pos, "only instances have properties").into())
            }
            Expr::Set(object, name, value) => {
                let var = &*object.to_string();
                let object = object.evaluate(Rc::clone(&env), locals)?;
                if let LoxValue::Instance(ref i) = object {
                    let value = value.evaluate(env.clone(), locals)?;
                    i.set(name, &value)?;
                    // reassign to env because we are cloners and i hate myself
                    return env.assign(var, &object);
                }
                Err(InnerError::new(*pos, "only instances have fields").into())
            }
            Expr::Super(_, _) => todo!(),
            Expr::This(_) => todo!(),
        }
    }

    pub fn position(&self) -> Span {
        match &self {
            Expr::This(tk) => *tk.span(),
            Expr::Literal(tk) => *tk.span(),
            Expr::Variable(tk) => *tk.span(),
            Expr::Grouping(expr) => expr.position(),
            Expr::Get(expr, tk) => Span::new(expr.position().start(), tk.span().end()),
            Expr::Unary(op, expr) => Span::new(op.span().start(), expr.position().end()),
            Expr::Super(ltk, rtk) => Span::new(ltk.span().start(), rtk.span().end()),
            Expr::Assign(tk, expr) => Span::new(tk.span().start(), expr.position().end()),
            Expr::Binary(lhs, _, rhs) => Span::new(lhs.position().start(), rhs.position().end()),
            Expr::Logical(lhs, _, rhs) => Span::new(lhs.position().start(), rhs.position().end()),
            Expr::Set(lhs, _, rhs) => Span::new(lhs.position().start(), rhs.position().end()),
            Expr::Call(expr, tk, args) => {
                if let Some(arg) = args.last() {
                    return Span::new(expr.position().start(), arg.position().end());
                }
                Span::new(expr.position().start(), tk.span().end())
            }
        }
    }

    pub fn is_nil_expr(&self) -> bool {
        matches!(&self, Expr::Literal(t) if *t.kind() == Keyword::Nil.into())
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Expr::Binary(lhs, tk, rhs) => write!(f, "({} {} {})", tk, *lhs, *rhs),
            Expr::Unary(tk, rhs) => write!(f, "({} {})", tk, *rhs),
            Expr::Assign(tk, expr) => write!(f, "({} {})", tk, *expr),
            Expr::Grouping(expr) => write!(f, "(group {})", *expr),
            Expr::Literal(tk) => write!(f, "{}", tk),
            Expr::Variable(tk) => write!(f, "{}", tk),
            _ => unimplemented!(),
        }
    }
}
