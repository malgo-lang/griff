#[derive(Debug, PartialEq)]
pub struct Id {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int32(i32),
    Int64(i64),
    Int(i64),
    Float32(f32),
    Float64(f64),
    Float(f64),
    Char(char),
    String(String),
}

#[derive(Debug, PartialEq)]
pub enum Type {
    App { fun: Box<Type>, args: Vec<Type> },
    Ident(Id),
    Arr { dom: Box<Type>, cod: Box<Type> },
    Tuple(Vec<Type>),
    Record(Vec<(Id, Type)>),
    Block(Box<Type>), // equivalent to () -> a
}

#[derive(Debug, PartialEq)]
pub enum Exp {
    Ident(Id),
    Unboxed(Literal),
    Boxed(Literal),
    App {
        fun: Box<Exp>,
        args: Vec<Exp>,
    },
    OpApp {
        op: Id,
        left: Box<Exp>,
        right: Box<Exp>,
    },
    Fun(Vec<Clause>),
    Tuple(Vec<Exp>),
    Record(Vec<(Id, Exp)>),
    List(Vec<Exp>),
    Ann {
        exp: Box<Exp>,
        ty: Type,
    },
    Seq(Vec<Stmt>),
    Parens(Box<Exp>),
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let { name: Id, exp: Box<Exp> },
    With { name: Option<Id>, exp: Box<Exp> },
    NoBind(Box<Exp>),
}

#[derive(Debug, PartialEq)]
pub struct Clause {
    patterns: Vec<Pat>,
    body: Box<Exp>,
}

#[derive(Debug, PartialEq)]
pub enum Pat {
    Ident(Id),
    Wildcard,
    App { fun: Id, args: Vec<Pat> },
    Tuple(Vec<Pat>),
    Record(Vec<(Id, Pat)>),
    Unboxed(Literal),
    Boxed(Literal),
}