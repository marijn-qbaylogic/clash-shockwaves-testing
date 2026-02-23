/*

All types for VCD Metadata (translators, signals, and LUTs)

*/

use serde::{Deserialize, Serialize};
use egui::Color32;
use std::collections::HashMap;

pub type SigMap = HashMap<String, String>;
pub type TypeMap = HashMap<String, Translator>;
pub type LutMap = HashMap<String, Lut>;

pub type Lut = HashMap<String, Translation>;


#[derive(Serialize, Deserialize, Debug)]
pub struct Data {
    pub signals: SigMap,
    pub types: TypeMap,
    pub luts: LutMap,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Translation(pub Render, pub Vec<(String, Translation)>);

pub type Render = Option<(Value, WaveStyle, Prec)>;
pub type Value = String;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum WaveStyle {
    #[serde(alias = "D")]
    Default,
    #[serde(alias = "E")]
    Error,
    #[serde(alias = "H")]
    Hidden,
    #[serde(alias = "I")]
    Inherit(usize),

    #[serde(alias = "N")]
    Normal,
    #[serde(alias = "W")]
    Warn,
    #[serde(alias = "U")]
    Undef,
    #[serde(alias = "Z")]
    HighImp,
    #[serde(alias = "X")]
    DontCare,
    #[serde(alias = "Q")]
    Weak,

    #[serde(alias = "C")]
    Color(Color32),
    #[serde(alias = "V")]
    Var(String, Box<WaveStyle>),
}

/// Operator precedence
pub type Prec = i16;
/// Precedence of an atomic (a number, an identifier, somthing between parentheses)
pub const ATOMIC: Prec = 11;

#[derive(Serialize, Deserialize, Debug)]
pub struct Translator {
    #[serde(alias = "w")]
    pub width: u32,
    #[serde(alias = "t")]
    #[serde(alias = "v")]
    pub trans: TranslatorVariant,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum TranslatorVariant {
    #[serde(alias = "R")]
    Ref(String),

    #[serde(alias = "S")]
    Sum(Vec<Translator>),

    #[serde(alias = "S+")]
    AdvancedSum {
        #[serde(alias = "i")]
        index: (usize, usize),
        #[serde(alias = "d")]
        default_translator: Box<Translator>,
        #[serde(alias = "t")]
        range_translators: Vec<((u128, u128), Translator)>,
    },

    #[serde(alias = "P")]
    Product {
        #[serde(alias = "t")]
        subs: Vec<(String, Translator)>,
        #[serde(alias = "[")]
        start: String,
        #[serde(alias = ",")]
        sep: String,
        #[serde(alias = "]")]
        stop: String,
        #[serde(alias = "n")]
        labels: Vec<String>,
        #[serde(alias = "p")]
        preci: Prec,
        #[serde(alias = "P")]
        preco: Prec,
    },

    #[serde(alias = "P+")]
    AdvancedProduct {
        #[serde(alias = "t")]
        slice_translators: Vec<((usize, usize), Translator)>,
        #[serde(alias = "h")]
        hierarchy: Vec<(String, usize)>,
        #[serde(alias = "v")]
        value_parts: Vec<ValuePart>,
        #[serde(alias = "P")]
        preco: Prec,
    },

    #[serde(alias = "C")]
    Const(Translation),

    #[serde(alias = "L")]
    Lut(String, Structure),

    #[serde(alias = "N")]
    Number {
        #[serde(alias = "f")]
        format: NumberFormat,
        #[serde(alias = "s")]
        spacer: NumberSpacer,
    },

    #[serde(alias = "A")]
    Array {
        #[serde(alias = "t")]
        sub: Box<Translator>,
        #[serde(alias = "l")]
        len: u32,
        #[serde(alias = "[")]
        start: String,
        #[serde(alias = ",")]
        sep: String,
        #[serde(alias = "]")]
        stop: String,
        #[serde(alias = "p")]
        preci: Prec,
        #[serde(alias = "P")]
        preco: Prec,
    },

    #[serde(alias = "X")]
    Styled(WaveStyle, Box<Translator>),

    #[serde(alias = "D")]
    Duplicate(String, Box<Translator>),

    #[serde(alias = "B")]
    ChangeBits {
        #[serde(alias = "t")]
        sub: Box<Translator>,
        #[serde(alias = "b")]
        bits: BitPart,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum ValuePart {
    #[serde(alias = "L")]
    Lit(String),
    #[serde(alias = "R")]
    Ref(usize, Prec),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum BitPart {
    #[serde(alias = "C")]
    Concat(Vec<BitPart>),
    #[serde(alias = "L")]
    Lit(String),
    #[serde(alias = "S")]
    Slice((usize, usize)),
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub enum NumberFormat {
    #[serde(alias = "S")]
    Sig,
    #[serde(alias = "U")]
    Uns,
    #[serde(alias = "H")]
    Hex,
    #[serde(alias = "O")]
    Oct,
    #[serde(alias = "B")]
    Bin,
}

pub type NumberSpacer = Option<(u32, String)>;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Structure(pub Vec<(String, Structure)>);


impl Data {
    pub fn new() -> Self {
        Self {
            signals: HashMap::new(),
            types: HashMap::new(),
            luts: HashMap::new(),
        }
    }
}