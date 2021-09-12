use crate::lib::{
    error::*,
    interpreter::{Environment, LoxValue},
    position::Span,
    token::{Keyword, Punctuator, Token, TokenKind},
};
use std::{cell::RefCell, collections::HashMap, convert::TryInto, rc::Rc};

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
/// Language expressions
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
    /// Array (start_token: Token, values: Vec<Expr>)
    Array(Token, Vec<Expr>),
    /// Index (name: Token, idx: Expr)
    Index(Token, Box<Expr>),
    /// ArrayAssing (name: Token, idx: Expr, val: Expr)
    IndexAssign(Token, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn evaluate(
        &self,
        env: Rc<Environment>,
        locals: &HashMap<Expr, usize>,
    ) -> LoxResult<Rc<LoxValue>> {
        let pos = &self.position();
        let var_lookup = |name, expr| {
            if let Some(idx) = locals.get(expr) {
                return env.get_at(*idx, name);
            }
            env.global()
                .get(name)
                .map_err(|e| InnerError::new(*pos, &e.to_string()).into())
        };

        match self {
            Expr::Literal(tk) => {
                let val = tk.kind().try_into().map_err(|e: &str| {
                    let err: LoxError = InnerError::new(*pos, e).into();
                    err
                })?;
                Ok(Rc::new(val))
            }
            Expr::Grouping(expr) => (*expr).evaluate(env, locals),
            Expr::Unary(op, rhs) => {
                let rhs = rhs.evaluate(env, locals)?;

                use Punctuator::*;

                match *op.kind() {
                    TokenKind::Punctuator(Sub) => {
                        Ok(Rc::new((-(*rhs).to_owned()).map_err(|e: LoxError| {
                            InnerError::new(*pos, &e.to_string())
                        })?))
                    }
                    TokenKind::Punctuator(Not) => Ok(Rc::new(LoxValue::Boolean(!rhs.is_truthy()))),
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
                let lhs = (*lhs).to_owned();
                let rhs = (*rhs).to_owned();

                let result = match *op.kind() {
                    TokenKind::Punctuator(Sub | AssignSub) => lhs - rhs,
                    TokenKind::Punctuator(Mul | AssignMul) => lhs * rhs,
                    TokenKind::Punctuator(Div | AssignDiv) => lhs / rhs,
                    TokenKind::Punctuator(Add | AssignAdd) => lhs + rhs,
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
                }
                .map_err(|e: LoxError| {
                    let err: LoxError = InnerError::new(*pos, &e.to_string()).into();
                    err
                })?;
                Ok(Rc::new(result))
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
            Expr::Variable(ref name) => var_lookup(&name.to_string(), self),
            Expr::Assign(name, val) => {
                let val = val.evaluate(Rc::clone(&env), locals)?;
                if let Some(idx) = locals.get(self) {
                    env.assign_at(*idx, &name.to_string(), &val)?;
                } else {
                    env.global()
                        .assign(&name.to_string(), &val)
                        .map_err(|e: LoxError| {
                            let err: LoxError = InnerError::new(*pos, &e.to_string()).into();
                            err
                        })?;
                }
                Ok(val)
            }

            Expr::Call(callee, _, args) => {
                let callee = callee.evaluate(Rc::clone(&env), locals)?;
                let args: Vec<_> = args
                    .iter()
                    .map(|arg| arg.evaluate(Rc::clone(&env), locals))
                    .collect::<LoxResult<_>>()?;

                if let LoxValue::Callable(c) = &*callee {
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
                let object = object.evaluate(Rc::clone(&env), locals)?;
                if let LoxValue::Instance(i) = &*object {
                    return i.get(name);
                }

                if let Ok(class) = object.as_class() {
                    return class
                        .find_static(&name.to_string())
                        .map_err(|e| InnerError::new(*pos, &e.to_string()).into());
                }

                Err(InnerError::new(
                    *pos,
                    &format!("cannot access property `{}` of `{}`", name, object),
                )
                .into())
            }
            Expr::Set(object, name, value) => {
                let object = &object.evaluate(Rc::clone(&env), locals)?;
                if let LoxValue::Instance(ref i) = **object {
                    let value = value.evaluate(Rc::clone(&env), locals)?;
                    (*i).set(name, &value)?;
                    return Ok(value);
                }
                Err(InnerError::new(*pos, "only instances have fields").into())
            }
            Expr::This(kw) => var_lookup(&kw.to_string(), self),
            Expr::Super(_, method) => {
                // Safe to unwrap here because we resolved the `super` expression already
                // so we know it exists
                let distance = locals.get(self).unwrap();
                let superclass = env.get_at(*distance, "super")?;
                let object = env.get_at(distance - 1, "this")?;
                let method = superclass
                    .as_class()
                    .map_err(|e| InnerError::new(*method.span(), &e.to_string()))?
                    .find_method(&method.to_string())
                    .ok_or_else(|| {
                        InnerError::new(
                            *pos,
                            &format!("undefined property `{}`", method.to_string()),
                        )
                    })?;
                method
                    .bind(object.as_instance()?)
                    .map(|f| Rc::new(LoxValue::Callable(Rc::new(f))))
            }
            Expr::Array(_, values) => {
                let values: Vec<_> = values
                    .iter()
                    .map(|val| val.evaluate(Rc::clone(&env), locals))
                    .collect::<LoxResult<_>>()?;
                Ok(Rc::new(LoxValue::Array(RefCell::new(values))))
            }
            Expr::Index(name, idx) => {
                let name = var_lookup(&name.to_string(), self)?;
                let idx = match *idx.evaluate(env, locals)? {
                    LoxValue::Integer(i) if i >= 0 => i as usize,
                    _ => return Ok(Rc::new(LoxValue::Nil)),
                };
                match *name {
                    LoxValue::Array(ref vec) => match vec.borrow().get(idx) {
                        Some(val) => Ok(Rc::clone(val)),
                        None => Ok(Rc::new(LoxValue::Nil)),
                    },
                    _ => Err(InnerError::new(*pos, "attempt to index unindexable type").into()),
                }
            }
            Expr::IndexAssign(name, idx, val) => {
                let name = var_lookup(&name.to_string(), self)?;
                let idx = match *idx.evaluate(Rc::clone(&env), locals)? {
                    LoxValue::Integer(i) if i >= 0 => i as usize,
                    _ => return Ok(Rc::new(LoxValue::Nil)),
                };
                let value = val.evaluate(env, locals)?;
                match *name {
                    LoxValue::Array(ref vec) => {
                        if vec.borrow().len() < idx {
                            vec.borrow_mut().resize(idx + 1, Rc::new(LoxValue::Nil));
                        }
                        match vec.borrow_mut().get_mut(idx) {
                            Some(curr) => {
                                *curr = Rc::clone(&value);
                                Ok(value)
                            }
                            None => Ok(Rc::new(LoxValue::Nil)),
                        }
                    }
                    _ => Err(InnerError::new(*pos, "attempt to index unindexable type").into()),
                }
            }
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
            Expr::Array(tk, values) => {
                if let Some(val) = values.last() {
                    return Span::new(tk.span().start(), val.position().end());
                }
                *tk.span()
            }
            Expr::Index(name, idx) => Span::new(name.span().start(), idx.position().end()),
            Expr::IndexAssign(name, _, val) => Span::new(name.span().start(), val.position().end()),
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
