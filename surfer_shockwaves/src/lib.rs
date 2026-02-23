// use compile_time::datetime_str;

// use either::Either;
// use either::Either::*;
// use extism_pdk::host_fn;
// use extism_pdk::{FnResult, Json, plugin_fn};
// use extism_pdk::{error, info, warn};

// use surfer_translation_types::WaveSource;
// pub use surfer_translation_types::plugin_types::TranslateParams;
// use surfer_translation_types::{
//     SubFieldTranslationResult, TranslationPreference, TranslationResult, ValueKind, ValueRepr,
//     VariableInfo, VariableMeta as VMeta, VariableValue,
// };

// use lazy_static::lazy_static;
// use std::iter::zip;
// use std::sync::Mutex;

// use serde::{Deserialize, Serialize};
// use toml::{Table, Value as TVal};

// use camino::Utf8PathBuf;
// use egui::Color32;
// use num_bigint::{BigInt, BigUint};
// use std::collections::HashMap;

mod data;
mod translate;
mod structure;
mod stylevars;
mod convert;

mod config;
mod cache;
mod state;
mod plugin;





