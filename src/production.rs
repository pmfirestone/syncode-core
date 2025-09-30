use std::sync::Arc;

/// A single production of the grammar.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Production {
    /// The left hand side of the production.
    pub lhs: Arc<String>,
    /// The right hand side of the production.
    pub rhs: Arc<Vec<String>>,
    // The priority of this production. Used to resolve conflicts when constructing the table.
    // pub priority: i32,
}

impl Production {
    /// Convenience constructor for tests.
    pub fn new(lhs: &str, rhs: Vec<&str>) -> Production {
        Production {
            lhs: lhs.to_string().into(),
            rhs: rhs
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .into(),
        }
    }
}
