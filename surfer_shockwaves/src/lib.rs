

use extism_pdk::{plugin_fn, FnResult, Json};
use extism_pdk::host_fn;

pub use surfer_translation_types::plugin_types::TranslateParams;
use surfer_translation_types::WaveSource;
use surfer_translation_types::{
    translator::VariableNameInfo,
    TranslationResult, SubFieldTranslationResult, ValueRepr, ValueKind, 
    TranslationPreference, VariableInfo,
    VariableMeta as VMeta, VariableValue,
};

use lazy_static::lazy_static;
use std::sync::Mutex;

use serde_json;
use serde::{Deserialize, Serialize};

use std::collections::HashMap;
use egui::Color32;
use camino::Utf8PathBuf;

type VariableMeta = VMeta<(),()>;

type SigMap = HashMap<String,String>;
type TypeMap = HashMap<String,Translator>;
type LUTMap = HashMap<String,LUT>;

type LUT = HashMap<String,Translation>;

#[derive(Serialize,Deserialize,Debug)]
struct Data {
    signals: SigMap,
    types:   TypeMap,
    luts:    LUTMap,
}

#[derive(Serialize,Deserialize,Debug)]
struct Translation(Render,Vec<(String,Translation)>);

type Render = Option<(Value,WaveStyle,Prec)>;
type Value = String;

#[derive(Serialize,Deserialize,Debug,Clone,Copy)]
enum WaveStyle {
    #[serde(alias = "N")]
    Normal,
    #[serde(alias = "W")]
    Warn,
    #[serde(alias = "E")]
    Error,
    #[serde(alias = "C")]
    Color(Color32)
}

type Prec = i16;


#[derive(Serialize,Deserialize,Debug)]
struct Translator {
    #[serde(alias = "w")]
    width: u32,
    #[serde(alias = "t")]
    #[serde(alias = "v")]
    trans: TranslatorVariant,
}

#[derive(Serialize,Deserialize,Debug)]
enum TranslatorVariant {
    #[serde(alias = "R")]
    Ref(String),

    #[serde(alias = "S")]
    Sum(Vec<Translator>),
    
    #[serde(alias = "P")]
    Product {
        #[serde(alias = "t")]
        subs: Vec<(Option<String>,Translator)>,
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
        #[serde(alias = "s")]
        style: i16,
    },

    #[serde(alias = "C")]
    Const(Translation),

    #[serde(alias = "L")]
    Lut(String,Structure),

    #[serde(alias = "N")]
    Number{
        #[serde(alias = "f")]
        format: NumberFormat,
    },

    #[serde(alias = "A")]
    Array{
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
    Duplicate(Box<Translator>),
}

#[derive(Serialize,Deserialize,Debug,Clone,Copy)]
enum NumberFormat {
    #[serde(alias = "S")]
    Sig,
    #[serde(alias = "U")]
    Unsig,
    #[serde(alias = "H")]
    Hex,
    #[serde(alias = "O")]
    Oct,
    #[serde(alias = "B")]
    Bin
}

#[derive(Serialize,Deserialize,Debug)]
enum Structure {
    Structure(Vec<(String,Structure)>),
    Ref(String),
}





// globals


lazy_static!{
    static ref STATE: Mutex<Data> = Mutex::new(Data::new());
}
lazy_static!{
    static ref TRANSLATOR_NOT_FOUND: Translator = Translator{width:0,trans:TranslatorVariant::Const(Translation(Some((String::from("{translator not found}"),WaveStyle::Error,11)),vec![]))};
}



// some helper functions

fn signal_name(signal:&VariableMeta) -> String {
    signal.var.path.strs.join(".")+"."+&signal.var.name
}


// main functions

impl Data {
    fn new() -> Self {
        Self {
            signals: HashMap::new(),
            types: HashMap::new(),
            luts: HashMap::new(),
        }
    }

    /// Determine the full structure of a signal.
    fn structure(&self, signal: &String) -> VariableInfo {
        // lookup signal type
        let ty = self.get_type(signal).unwrap();

        // create flattened structure for type
        let st = self.flat_structure(ty);

        // convert to VariableInfo
        st.convert()
    }

    /// Get a structure without references to other types
    fn flat_structure(&self, ty: &String) -> Structure {
        // lookup type translator
        let trans = self.get_translator(ty);

        // create non-flattened structure for type
        let mut st = trans.structure();

        // flatten structure
        self.flatten_structure(&mut st);
        st
    }

    /// Flatten a structure (remove all references)
    fn flatten_structure(&self, structure: &mut Structure) {
        match structure {
            Structure::Structure(subs) => {
                subs.iter_mut().for_each(|(_name,st)| self.flatten_structure(st))
            }
            Structure::Ref(ty) => {
                *structure = self.flat_structure(ty);
            }
        }
    }

    /// Translate a value.
    fn translate(&self, signal: &String, value: &str) -> TranslationResult {
        // lookup signal type
        let ty = self.get_type(signal).unwrap();

        // lookup type translator
        let translator = self.get_translator(ty);
        
        // translate value with translator
        let translation = self.translate_with(translator,value);
        
        // convert to TranslationResult
        translation.convert()
    }

    /// Check whether a signal has a known associated Haskell type.
    fn can_translate(&self, signal: &String) -> bool {
        self.types.get(signal).is_some()
    }

    /// Translate a value using the provided translator
    fn translate_with(&self, Translator{width,trans}: &Translator, value: &str) -> Translation {
        todo!()
    }

    /// Get the type of a signal
    fn get_type(&self, signal: &String) -> Option<&String> {
        self.signals.get(signal)
    }

    /// Get the translator of a signal. If there is no such translator, return a default translator that displays an error message.
    fn get_translator(&self, ty: &String) -> &Translator {
        match self.types.get(ty) {
            Some(t) => t,
            None => &TRANSLATOR_NOT_FOUND,
        }
    }
}

impl Translation {
    /// Convert a Shockwaves `Translation` value into a Surfer `TranslationResult`
    fn convert(self) -> TranslationResult {
        let Translation(render,sub) = self;
        let subfields = sub.into_iter().map(|(name,t)|
            SubFieldTranslationResult{ name, result: t.convert() }
        ).collect();
        match render {
            Some((val,style,_prec)) => TranslationResult{
                val: ValueRepr::String(val),
                kind: style.convert(),
                subfields,
            },
            None => TranslationResult{
                val: ValueRepr::NotPresent,
                kind: ValueKind::Normal,
                subfields,
            },
        }
    }
}

impl WaveStyle {
    /// Convert a Shockwaves `WaveStyle` into a Surfer `ValueKind`
    fn convert(self) -> ValueKind {
        match self {
            WaveStyle::Normal => ValueKind::Normal,
            WaveStyle::Warn   => ValueKind::Warn,
            WaveStyle::Error  => ValueKind::Warn,
            WaveStyle::Color(c) => ValueKind::Custom(c),
        }
    }
}

impl Structure {
    /// Convert flattened Shockwaves `Structure` to Surfer `VariableInfo`
    fn convert(self) -> VariableInfo {
        match self {
            Structure::Structure(v) if v.len()==0 => VariableInfo::String,
            Structure::Structure(v) => VariableInfo::Compound{
                subfields: v.into_iter().map(|(name,st)| (name,st.convert())).collect()
            },
            Structure::Ref(_) => unreachable!(),
        }
    }
}

impl Translator {
    fn structure(&self) -> Structure {
        todo!()
    }
}













#[plugin_fn]
pub fn name() -> FnResult<String> {
    Ok("Shockwaves".to_string())
}

#[plugin_fn]
pub fn translates(variable: VariableMeta) -> FnResult<TranslationPreference> {
    let signal = signal_name(&variable);
    Ok(if STATE.lock().unwrap().can_translate(&signal) {
        TranslationPreference::Prefer
    } else {
        TranslationPreference::No
    })
}

#[plugin_fn]
pub fn variable_info(variable: VariableMeta) -> FnResult<VariableInfo> {
    let signal = signal_name(&variable);
    Ok(STATE.lock().unwrap().structure(&signal))
}

#[plugin_fn]
pub fn translate(
    TranslateParams { variable, value }: TranslateParams,
) -> FnResult<TranslationResult> {
    let val_raw = match value {
        VariableValue::BigUint(v) => format!("{v:0width$b}",width=&(variable.num_bits.unwrap_or(0) as usize)), // pad to the right length how?
        VariableValue::String(v) => v.clone(),
    };
    let signal = signal_name(&variable);
    Ok(STATE.lock().unwrap().translate(&signal,val_raw.as_str()))
}

#[plugin_fn]
pub fn set_wave_source(Json(wave_source): Json<Option<WaveSource>>) -> FnResult<()> {
    if let Some(wave_source) = wave_source {
        let file = match wave_source {
            WaveSource::File(f) => Some(f),
            WaveSource::Data => None,
            WaveSource::DragAndDrop(f) => f,
            WaveSource::Url(_) => None,
            WaveSource::Cxxrtl => None,
        };

        if let Some(file) = file {
            let metafile = Utf8PathBuf::from(file).set_extension("json").to_string();
            if let Ok(true) = unsafe{file_exists(metafile.clone())} {
                let bytes = unsafe{read_file(metafile.clone())};
                match bytes {
                    Ok(bytes) => {
                        match serde_json::from_slice(&bytes) {
                            Ok(data) => {
                                extism_pdk::error!("Parsed Shockwaves metadata file");
                                *STATE.lock().unwrap() = data;
                            }
                            Err(e) => {
                                extism_pdk::error!("Could not parse Shockwaves metadata file: {e}");
                                *STATE.lock().unwrap() = Data::new();
                            }
                        }
                    }
                    Err(e) => {
                        extism_pdk::error!("Could not read Shockwaves metadata file: {e}");
                        *STATE.lock().unwrap() = Data::new();
                    }
                }
            } else {
                extism_pdk::warn!("Could not find Shockwaves metadata file ({metafile})");
                *STATE.lock().unwrap() = Data::new();
            }

        } else {
            *STATE.lock().unwrap() = Data::new();
            extism_pdk::info!("Not loading Shockwaves translator for non-files");
        }
    } else {
        extism_pdk::info!("No waveform source");
        *STATE.lock().unwrap() = Data::new();
    }
    Ok(())
}


#[host_fn]
extern "ExtismHost" {
    pub fn read_file(filename: String) -> Vec<u8>;
    pub fn file_exists(filename: String) -> bool;
}

