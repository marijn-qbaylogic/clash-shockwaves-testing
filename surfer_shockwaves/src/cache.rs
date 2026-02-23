/*

A cache for structures.

*/


use std::collections::HashMap;

use crate::data::Structure;

pub type StructureMap = HashMap<String, Structure>;

#[derive(Debug, Default)]
pub struct Cache {
    pub structures: StructureMap,
}
