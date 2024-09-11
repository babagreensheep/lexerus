#[derive(PartialEq, Eq)]
pub struct Syntax {
    pub name: &'static str,
    pub values: Expr,
}

impl Syntax {
    fn as_rhs(&self) -> String {
        match &self.values {
            Expr::Symbol(_) => format!("{:?}", self.values),
            Expr::And(vec) => {
                if vec.len() == 1 {
                    format!("{:?}", vec[0])
                }
                else {
                    format!("<{}>", self.name)
                }
            }
            Expr::Or(vec) => {
                if vec.len() == 1 {
                    format!("{:?}", vec[0])
                }
                else {
                    format!("<{}>", self.name)
                }
            }
            Expr::Not(syntax) | Expr::Many(syntax) => {
                syntax.as_rhs()
            }
        }
    }
}

impl ::std::fmt::Debug for Syntax {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "{} ::= {:?}", self.name, self.values)?;
        Ok(())
    }
}

#[derive(PartialEq, Eq)]
pub enum Expr {
    Symbol(Option<&'static str>),
    And(Vec<Syntax>),
    Or(Vec<Syntax>),
    Not(Box<Syntax>),
    Many(Box<Syntax>),
}

impl ::std::fmt::Debug for Expr {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Symbol(symbol) => match symbol {
                Some(symbol) => {
                    ::std::fmt::Debug::fmt(symbol, f)
                }
                None => write!(f, "*"),
            },
            Self::And(syntaxes) => {
                // Create hashmap of keys and
                // representations
                for (count, syntax) in
                    syntaxes.iter().enumerate()
                {
                    if count > 0 {
                        write!(f, " && ")?;
                    }
                    write!(f, "[{}]", syntax.as_rhs())?;
                }
                Ok(())
            }
            Self::Or(syntaxes) => {
                // Create hashmap of keys and
                // representations
                for (count, syntax) in
                    syntaxes.iter().enumerate()
                {
                    if count > 0 {
                        write!(f, " || ")?;
                    }
                    write!(f, "[{}]", syntax.as_rhs())?;
                }
                Ok(())
            }
            Self::Not(syntax) => {
                write!(f, "![{}]", syntax.as_ref().as_rhs())
            }
            Self::Many(syntax) => {
                write!(f, "?[{}]", syntax.as_ref().as_rhs())
            }
        }
    }
}

#[test]
fn debug_syntax() {
    let test = Syntax {
        name: "Test",
        values: Expr::Symbol(Some("hi")),
    };
    let symbol = Syntax {
        name: "Array",
        values: Expr::Or(
            [
                Syntax {
                    name: "Test",
                    values: Expr::Symbol(Some("hi")),
                },
                Syntax {
                    name: "Any",
                    values: Expr::Symbol(None),
                },
            ]
            .into(),
        ),
    };
    let symbol = Syntax {
        name: "Compound",
        values: Expr::And(
            [
                Syntax {
                    name: "Test",
                    values: Expr::Symbol(Some(
                        "aagfasfasdfsdf",
                    )),
                },
                symbol,
                Syntax {
                    name: "Multiple",
                    values: Expr::Not(test.into()),
                },
            ]
            .into(),
        ),
    };
    println!("{symbol:#?}");
}
