

use extism_pdk::{plugin_fn, FnResult, Json};
use extism_pdk::host_fn;

pub use surfer_translation_types::plugin_types::TranslateParams;
use surfer_translation_types::WaveSource;
use surfer_translation_types::{
    TranslationResult, SubFieldTranslationResult, ValueRepr, ValueKind, 
    TranslationPreference, VariableInfo,
    VariableMeta as VMeta, VariableValue,
};

use lazy_static::lazy_static;
use std::iter::zip;
use std::sync::Mutex;

use serde_json;
use serde::{Deserialize, Serialize};

use std::collections::HashMap;
use egui::Color32;
use camino::Utf8PathBuf;
use num_bigint::{BigInt,BigUint};

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

#[derive(Serialize,Deserialize,Debug,Clone)]
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
    Duplicate(String,Box<Translator>),
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

#[derive(Serialize,Deserialize,Debug,Clone)]
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
        let st = self.type_structure(ty);

        // convert to VariableInfo
        st.convert()
    }

    /// Get a structure without references to other types
    fn type_structure(&self, ty: &String) -> Structure {
        // lookup type translator
        let trans = self.get_translator(ty);

        // create non-flattened structure for type
        self.trans_structure(trans)
    }

    /// Get the (flattened!) structure of a translator
    fn trans_structure(&self, trans: &Translator) -> Structure {
        let Translator{trans, ..} = trans;
        match trans {
            TranslatorVariant::Ref(ty) => self.type_structure(ty),
            TranslatorVariant::Sum(translators) => {
                let ts = translators.iter().map(|t|
                    match self.trans_structure(t) {
                        Structure::Structure(s) => s,
                        Structure::Ref(_) => unreachable!(),
                    }
                ).flatten().collect();
                Structure::Structure(ts)
            },
            TranslatorVariant::Product{subs, ..} => Structure::Structure(
                subs.iter()
                    .filter(|(name,_)| name.is_some())
                    .map(|(name,t)| (name.as_ref().unwrap().clone(), self.trans_structure(t)))
                    .collect()
                ),
            TranslatorVariant::Const(_) => Structure::Structure(vec![]),
            TranslatorVariant::Lut(_, structure) => structure.clone(),
            TranslatorVariant::Number{..} => Structure::Structure(vec![]),
            TranslatorVariant::Array{ sub, len, ..} => Structure::Structure(
                std::iter::repeat(self.trans_structure(sub)).take(*len as usize)
                    .enumerate().map(|(i,s)| (i.to_string(),s))
                    .collect()
            ),
            TranslatorVariant::Styled(_,translator) => self.trans_structure(translator),
            TranslatorVariant::Duplicate(name,translator) => Structure::Structure(
                vec![(name.clone(),self.trans_structure(translator))]
            ),
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
    fn translate_with(&self, translator: &Translator, value: &str) -> Translation {
        match self.translate_with_opt(translator,value) {
            Some(t) => t,
            _ => Translation(Some((String::from("{unknown}"),WaveStyle::Error,11)),vec![])
        }
    }
    fn translate_with_opt(&self, Translator{width,trans}: &Translator, value: &str) -> Option<Translation> {
        if (*width as usize) < value.len() {
            return Some(error("{insufficient bits}"));
        }

        return Some(match trans {
            TranslatorVariant::Array { sub, len, start, sep, stop, preci, preco } => {
                let mut subs = vec![];
                for i in 0u32..*len {
                    subs.push(self.translate_with(sub,&value[(i*sub.width) as usize..]));
                }

                let mut res: Vec<&str> = vec![start];
                for (i,t) in zip(0..,subs.iter()) {
                    let p = match t.0 {
                        Some((_,_,p)) => p,
                        None => 11
                    };
                    if p <= *preci {res.push("(");}
                    res.push(match &t.0 {
                        Some((v,_,_)) => &v,
                        None => "{value missing}",
                    });
                    if p <= *preci {res.push(")");}
                    if i!=subs.len()-1 {res.push(sep);}
                }
                res.push(stop);

                Translation(
                    Some((res.join(""),WaveStyle::Normal,*preco)),
                    subs.into_iter().enumerate().map(|(i,t)| (format!("{i}"),t)).collect()
                )
            },
            TranslatorVariant::Const(t) => {
                t.clone()
            }
            TranslatorVariant::Lut(n,_) => {
                self.luts.get(n)?.get(value)?.clone()
            }
            TranslatorVariant::Duplicate(n,t) => {
                let translation = self.translate_with(t,value);
                Translation(translation.0.clone(),vec![(n.clone(),translation)])
            },
            TranslatorVariant::Number{format } => {
                Translation(Some(match format {
                    NumberFormat::Sig   => {
                        // slightly cursed way of doing this - convert to base 256 bytes, then to bigint, then to string
                        let n = (value.len()+7)/8;
                        let mut bytes = vec![0u8; n];
                        for i in 0..(8*n) {
                            let j = if i>=value.len() {0} else {value.len()-1-i};
                            let c = value.chars().nth(j);
                            if c == Some('1') {
                                bytes[i/8] |= (1<<(i%8)) as u8;
                            } else if c == Some('0') {
                            } else if let Some(_) = c {
                                return None;
                            }
                        }
                        
                        let big = BigInt::from_signed_bytes_be(&bytes);

                        (big.to_string(),WaveStyle::Normal,11)
                    },
                    NumberFormat::Unsig => (BigUint::parse_bytes(value.as_bytes(),2)?.to_string(),WaveStyle::Normal,11),
                    NumberFormat::Bin   => (value.to_string(),if value.contains('x') {WaveStyle::Error} else {WaveStyle::Normal},11),
                    _ => error("{number format not implemented}").0.unwrap()
                }),vec![])
            },
            TranslatorVariant::Product { subs, start, sep, stop, labels, preci, preco, style } => {
                let mut sub = vec![];
                let _ = subs.iter().fold(0usize,|i,(n,t)| {
                    let translation = self.translate_with(t,&value[i..]);
                    sub.push((n.clone(),translation));
                    i+t.width as usize
                });

                let vals: Vec<&str> = if labels.len()!=0 {
                    let mut res: Vec<&str> = vec![start];
                    for (i,(l,(_,t))) in zip(0..,zip(labels,sub.iter())) {
                        let p = match t.0 {
                            Some((_,_,p)) => p,
                            None => 11
                        };
                        res.push(l);
                        if p <= *preci {res.push("(");}
                        res.push(match &t.0 {
                            Some((v,_,_)) => v,
                            None => "{value missing}",
                        });
                        if p <= *preci {res.push(")");}
                        if i!=subs.len()-1 {res.push(sep);}
                    }
                    res.push(stop);
                    res
                } else {
                    let mut res: Vec<&str> = vec![start];
                    for (i,(_,t)) in zip(0..,sub.iter()) {
                        let p = match t.0 {
                            Some((_,_,p)) => p,
                            None => 11
                        };
                        if p <= *preci {res.push("(");}
                        res.push(match &t.0 {
                            Some((v,_,_)) => &v,
                            None => "{value missing}",
                        });
                        if p <= *preci {res.push(")");}
                        if i!=subs.len()-1 {res.push(sep);}
                    }
                    res.push(stop);
                    res
                };
                let val = vals.join("");

                Translation(
                    Some((
                        val,
                        // take style from other field if specified and present
                        if *style>=0 {
                            sub.get(*style as usize)?
                                .1.0.as_ref()
                                .map_or(WaveStyle::Normal,|r| r.1)
                        } else {
                            WaveStyle::Normal
                        },
                        *preco
                    )),
                    // filter out subsignals without labels
                    sub.into_iter().filter_map(|(n,t)| match n {
                        Some(n) => Some((n,t)),
                        None => None,
                    }).collect()
                )
            },
            TranslatorVariant::Ref(ty) => {
                self.translate_with(&self.get_translator(ty),value)
            },
            TranslatorVariant::Sum(translators) => {
                let n = translators.len();
                let bits = (usize::BITS - usize::leading_zeros(n)) as usize;
                let variant = usize::from_str_radix(&value[..bits], 2).ok()?;
                if variant>=n {return Some(error("{variant out of range}"));}
                let t = &translators[variant];
                self.translate_with(t,&value[bits..])
            },
            TranslatorVariant::Styled(s, t) => {
                let translation = self.translate_with(t,value);
                match translation {
                    Translation(Some((v,_,p)),sub) => Translation(Some((v,*s,p)),sub),
                    trans => trans,
                }
            },
        });



        fn error(msg: &str) -> Translation {
            Translation(Some((String::from(msg),WaveStyle::Error,11)),vec![])
        }
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

