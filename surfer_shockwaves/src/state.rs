use crate::config::*;
use crate::data::*;
use crate::cache::*;

#[derive(Debug)]
pub struct State {
    pub data: Data,
    pub config: Config,
    pub cache: Cache,
}

impl State {
    pub fn new() -> Self {
        Self {
            data: Data::new(),
            config: Config::default(),
            cache: Cache::default(),
        }
    }

    pub fn set_data(&mut self, data: Data) {
        self.data = data;
        self.replace_wavestyles();
        self.cache = Cache::default();
    }
}