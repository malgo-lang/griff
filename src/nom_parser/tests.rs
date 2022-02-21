use super::*;

#[test]
fn test_parse_type() {
    let data = "Int";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::Ident(Id {
                name: "Int".to_string()
            })
        ))
    );

    let data = "(Int, Int)";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::Tuple(vec![
                Type::Ident(Id {
                    name: "Int".to_string()
                }),
                Type::Ident(Id {
                    name: "Int".to_string()
                }),
            ])
        ))
    );

    let data = "List Int";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::App {
                fun: Box::new(Type::Ident(Id {
                    name: "List".to_string()
                })),
                args: vec![Type::Ident(Id {
                    name: "Int".to_string()
                })],
            }
        ))
    );

    let data = "Either Int String";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::App {
                fun: Box::new(Type::Ident(Id {
                    name: "Either".to_string()
                })),
                args: vec![
                    Type::Ident(Id {
                        name: "Int".to_string()
                    }),
                    Type::Ident(Id {
                        name: "String".to_string()
                    }),
                ],
            }
        ))
    );

    let data = "Either (List Int) (List String)";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::App {
                fun: Box::new(Type::Ident(Id {
                    name: "Either".to_string()
                })),
                args: vec![
                    Type::App {
                        fun: Box::new(Type::Ident(Id {
                            name: "List".to_string()
                        })),
                        args: vec![Type::Ident(Id {
                            name: "Int".to_string()
                        })],
                    },
                    Type::App {
                        fun: Box::new(Type::Ident(Id {
                            name: "List".to_string()
                        })),
                        args: vec![Type::Ident(Id {
                            name: "String".to_string()
                        })],
                    },
                ],
            }
        ))
    );

    let data = "Int -> Int";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::Arr {
                dom: Box::new(Type::Ident(Id {
                    name: "Int".to_string()
                })),
                cod: Box::new(Type::Ident(Id {
                    name: "Int".to_string()
                })),
            }
        ))
    );

    let data = "Int -> Int -> Int";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::Arr {
                dom: Box::new(Type::Ident(Id {
                    name: "Int".to_string()
                })),
                cod: Box::new(Type::Arr {
                    dom: Box::new(Type::Ident(Id {
                        name: "Int".to_string()
                    })),
                    cod: Box::new(Type::Ident(Id {
                        name: "Int".to_string()
                    })),
                }),
            }
        ))
    );

    let data = "{ x: Int, y: String }";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::Record(vec![
                (
                    Id {
                        name: "x".to_string()
                    },
                    Type::Ident(Id {
                        name: "Int".to_string()
                    })
                ),
                (
                    Id {
                        name: "y".to_string()
                    },
                    Type::Ident(Id {
                        name: "String".to_string()
                    })
                ),
            ])
        ))
    );

    let data = "{ a }";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::Block(Box::new(Type::Ident(Id {
                name: "a".to_string()
            })))
        ))
    );
}
#[test]
fn test_parse_exp() {
    let data = "f 1 2";
    let result = parse_exp(data);
    assert_eq!(
        result,
        Ok((
            "",
            Exp::App {
                fun: Box::new(Exp::Ident(Id {
                    name: "f".to_string()
                })),
                args: vec![Exp::Boxed(Literal::Int(1)), Exp::Boxed(Literal::Int(2))],
            }
        ))
    );
}

#[test]
fn test_parse_pat() {
    let data = "Nil";
    let result = parse_pat(data);
    assert_eq!(
        result,
        Ok((
            "",
            Pat::Ident(Id {
                name: "Nil".to_string()
            })
        ))
    );

    let data = "Cons x xs";
    let result = parse_pat(data);
    assert_eq!(
        result,
        Ok((
            "",
            Pat::App {
                fun: Id {
                    name: "Cons".to_string()
                },
                args: vec![
                    Pat::Ident(Id {
                        name: "x".to_string()
                    }),
                    Pat::Ident(Id {
                        name: "xs".to_string()
                    }),
                ],
            }
        ))
    );

    let data = "(x, y, z)";
    let result = parse_pat(data);
    assert_eq!(
        result,
        Ok((
            "",
            Pat::Tuple(vec![
                Pat::Ident(Id {
                    name: "x".to_string()
                }),
                Pat::Ident(Id {
                    name: "y".to_string()
                }),
                Pat::Ident(Id {
                    name: "z".to_string()
                }),
            ])
        ))
    );
}
#[test]
fn test_parse_ident() {
    let data = "x";
    let result = parse_exp(data);
    assert_eq!(
        result,
        Ok((
            "",
            Exp::Ident(Id {
                name: "x".to_string()
            })
        ))
    );
}

#[test]
fn test_parse_literal() {
    let data = "123i32";
    let result = parse_literal(data);
    assert_eq!(result, Ok(("", Literal::Int32(123))));

    let data = "3.14f32";
    let result = parse_literal(data);
    assert_eq!(result, Ok(("", Literal::Float32(3.14))));

    let data = "314E-2f64";
    let result = parse_literal(data);
    assert_eq!(result, Ok(("", Literal::Float64(314E-2))));

    let data = "'a'";
    let result = parse_literal(data);
    assert_eq!(result, Ok(("", Literal::Char('a'))));

    let data = "'\\n'";
    let result = parse_literal(data);
    assert_eq!(result, Ok(("", Literal::Char('\n'))));

    let data = "'\\u{1F602}'";
    let result = parse_literal(data);
    assert_eq!(result, Ok(("", Literal::Char('\u{1F602}'))));

    let data = "\"Hello, world!\"";
    let result = parse_literal(data);
    assert_eq!(
        result,
        Ok(("", Literal::String("Hello, world!".to_string())))
    );
}
