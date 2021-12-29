use std::fmt;

use crate::sexpr::{Atom, SExpr};

pub enum Value {
    Symbol(String),
    Natural(u64),
    List(Vec<Value>),
    Expr(SExpr),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Symbol(s) => write!(f, "{}", s),
            Value::Natural(n) => write!(f, "{}", n),
            Value::List(l) => {
                let mut iter = l.iter();
                write!(f, "(")?;
                if let Some(head) = iter.next() {
                    write!(f, "{}", head)?;
                }
                for rest in iter {
                    write!(f, " {}", rest)?;
                }
                write!(f, ")")
            }
            Value::Expr(e) => write!(f, "`{}`", e),
        }
    }
}

#[test]
fn test_value_print() {
    let symbol = Value::Symbol("+".into());
    assert_eq!(symbol.to_string(), "+");

    let number = Value::Natural(1);
    assert_eq!(number.to_string(), "1");

    let list = Value::List(vec![
        Value::Symbol("+".into()),
        Value::Natural(1),
        Value::Natural(2),
        Value::Natural(3),
    ]);
    assert_eq!(list.to_string(), "(+ 1 2 3)");

    let expr = Value::Expr(SExpr::List(vec![
        SExpr::Atom(Atom::Symbol("+".into())),
        SExpr::Atom(Atom::Natural(1)),
        SExpr::List(vec![
            SExpr::Atom(Atom::Symbol("+".into())),
            SExpr::Atom(Atom::Natural(2)),
            SExpr::Atom(Atom::Natural(3)),
        ]),
    ]));
    assert_eq!(expr.to_string(), "`(+ 1 (+ 2 3))`");
}

#[derive(Debug, Clone)]
pub enum EvalError {
    NotFound(String),
    NotNatural(String),
    NotSymbol(String),
    NotList(String),
    NotExpr(String),
}

// Evaluate a S-expression.
pub fn eval(sexpr: SExpr) -> Result<Value, EvalError> {
    todo!()
}
