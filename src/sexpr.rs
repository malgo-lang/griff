use std::fmt;

#[derive(Debug, Clone)]
pub enum SExpr {
    Atom(Atom),
    List(Vec<SExpr>),
}

#[derive(Debug, Clone)]
pub enum Atom {
    Symbol(String),
    Natural(u64),
}

impl fmt::Display for SExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SExpr::Atom(s) => write!(f, "{}", s),
            SExpr::List(l) => {
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
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Symbol(s) => write!(f, "{}", s),
            Atom::Natural(n) => write!(f, "{}", n),
        }
    }
}

#[test]
fn test_sexpr_print() {
    let e = SExpr::List(vec![
        SExpr::Atom(Atom::Symbol("+".into())),
        SExpr::Atom(Atom::Natural(1)),
        SExpr::List(vec![
            SExpr::Atom(Atom::Symbol("+".into())),
            SExpr::Atom(Atom::Natural(2)),
            SExpr::Atom(Atom::Natural(3)),
        ]),
    ]);

    assert_eq!(e.to_string(), "(+ 1 (+ 2 3))");
}
