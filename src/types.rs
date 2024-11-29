use crate::{
    type_enum::Type,
    state::Statement
};

pub type Scope = std::collections::BTreeMap<String, Type>;
pub type Program = Vec<Statement>;
