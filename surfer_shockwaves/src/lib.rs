
use compile_time::datetime_str;

use either::Either;
use either::Either::*;
use extism_pdk::{plugin_fn, FnResult, Json};
use extism_pdk::host_fn;
use extism_pdk::{info,warn,error};

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
use toml::{Table, Value as TVal};
use serde::{Deserialize, Serialize};

use std::collections::HashMap;
use egui::Color32;
use camino::Utf8PathBuf;
use num_bigint::{BigInt,BigUint};

type VariableMeta = VMeta<(),()>;

type SigMap = HashMap<String,String>;
type TypeMap = HashMap<String,Translator>;
type LUTMap = HashMap<String,LUT>;
type StructureMap = HashMap<String,Structure>; 

type LUT = HashMap<String,Translation>;

#[derive(Debug)]
struct State {
    data: Data,
    config: Config,
    cache: Cache,
}

#[derive(Debug,Default)]
struct Cache {
    structures: StructureMap,
}

#[derive(Debug,Default)]
struct Config {
    global: Configuration,
    local: Configuration,

    conf_dir: Option<Utf8PathBuf>,
    wavesource_dir: Option<Utf8PathBuf>,

    style: StyleMap,
}


#[derive(Debug,Serialize,Deserialize,Default)]
struct Configuration {
    #[serde(default)]
    propagate_errors: Option<bool>,

    #[serde(default)]
    override_sig_spacer: Option<NumberSpacer>,
    #[serde(default)]
    override_uns_spacer: Option<NumberSpacer>,
    #[serde(default)]
    override_hex_spacer: Option<NumberSpacer>,
    #[serde(default)]
    override_oct_spacer: Option<NumberSpacer>,
    #[serde(default)]
    override_bin_spacer: Option<NumberSpacer>,

    #[serde(default)]
    styles: Vec<String>,
    #[serde(default)]
    style: Option<Table>,
}

type StyleMap = HashMap<String,Either<Option<WaveStyle>,toml::Value>>;

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

#[derive(Serialize,Deserialize,Debug,Clone)]
enum WaveStyle {
    #[serde(alias = "N")]
    Normal,
    #[serde(alias = "W")]
    Warn,
    #[serde(alias = "E")]
    Error,
    #[serde(alias = "C")]
    Color(Color32),
    #[serde(alias = "V")]
    Var(String,Box<WaveStyle>),
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
        #[serde(alias = "s")]
        spacer: NumberSpacer,
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
    Uns,
    #[serde(alias = "H")]
    Hex,
    #[serde(alias = "O")]
    Oct,
    #[serde(alias = "B")]
    Bin
}

type NumberSpacer = Option<(u32,String)>;

#[derive(Serialize,Deserialize,Debug,Clone)]
struct Structure(Vec<(String,Structure)>);
// enum Structure {
//     Structure(Vec<(String,Structure)>),
//     Ref(String),
// }





// globals


lazy_static!{
    static ref STATE: Mutex<State> = Mutex::new(State::new());
}
lazy_static!{
    static ref TRANSLATOR_NOT_FOUND: Translator = 
        Translator{
            width:0,
            trans:TranslatorVariant::Const(
                Translation(
                    Some((String::from("{translator not found}"),WaveStyle::Error,11)),
                    vec![]
                )
            )
        };
}



// helper functions

fn signal_name(signal:&VariableMeta) -> String {
    signal.var.path.strs.join(".")+"."+&signal.var.name
}


// main functions

impl State {
    fn new() -> Self {
        Self {
            data: Data::new(),
            config: Config::default(),
            cache: Cache::default(),
        }
    }

    fn set_data(&mut self,data:Data) {
        self.data=data;
        self.replace_wavestyles();
        self.cache=Cache::default();
    }

    fn replace_wavestyles(&mut self) {
        for translator in self.data.types.values_mut() {
            translator.replace_wavestyles(&mut self.config);
        }
    }

    /// Determine the full structure of a signal.
    fn structure(&mut self, signal: &String) -> VariableInfo {
        // lookup signal type
        let ty = self.data.get_type(signal).unwrap();

        // try to return structure from cache
        if let Some(st) = self.cache.structures.get(ty) {
            return st.convert();
        }

        // lookup type translator
        let trans = self.data.get_translator(ty);
        let st = self.data.trans_structure(trans);
        let ty = ty.clone();

        // create non-flattened structure for type
        let st = self.cache.structures.entry(ty).or_insert(st);

        // convert to VariableInfo
        st.convert()
    }

    /// Translate a value.
    fn translate(&mut self, signal: &String, value: &str) -> TranslationResult {
        // lookup signal type
        let ty = self.data.get_type(signal).unwrap().clone();

        // lookup type translator
        let structure = if let Some(st) = self.cache.structures.get(&ty) {
            st
        } else {
            self.cache.structures.insert(ty.clone(),self.data.type_structure(&ty));
            self.cache.structures.get(&ty).unwrap()
        };
        let translator = self.data.get_translator(&ty);
        
        // translate value with translator
        let mut translation = self.translate_with(translator,value);

        //propagate errors
        if self.config.do_prop_errors() {
            translation.prop_errors();
        }

        //fill out missing unknown fields
        translation.fill(&structure);
        
        // convert to TranslationResult
        translation.convert()
    }


    

    /// Translate a value using the provided translator
    // fn translate_with(&self, translator: &Translator, value: &str) -> Translation {
    //     match self.translate_with_opt(translator,value) {
    //         Some(t) => t,
    //         _ => Translation(Some((String::from("{unknown}"),WaveStyle::Error,11)),vec![])
    //     }
    // }
    fn translate_with(&self, Translator{width,trans}: &Translator, value: &str) -> Translation {
        if (*width as usize) > value.len() {
            return error("{insufficient bits}");
        }
        let value = &value[..(*width as usize)];

        return match trans {
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
                match self.data.luts.get(n) {
                    Some(lut) => match lut.get(value) {
                        Some(t) => t.clone(),
                        None => error("{value missing from LUT}"),
                    },
                    None => error("{unknown LUT}"),
                }
            }
            TranslatorVariant::Duplicate(n,t) => {
                let translation = self.translate_with(t,value);
                Translation(translation.0.clone(),vec![(n.clone(),translation)])
            },
            TranslatorVariant::Number{format ,spacer} => {
                fn apply_spacer(sp:&NumberSpacer,v:String) -> String {
                    match sp {
                        None => v,
                        Some((0,_)) => v,
                        Some((n,s)) => {
                            let n = *n as i32;
                            let mut chunks = vec![];
                            let mut i = v.len() as i32;
                            while i>0 {
                                chunks.push(&v[0i32.max(i-n) as usize ..i as usize]);
                                i-=n;
                            }
                            let mut res = vec![];
                            for i in 0..chunks.len() {
                                res.push(chunks[i]);
                                if i+1<chunks.len() && chunks[i+1]!="-" {
                                    res.push(s);
                                }
                            }
                            res.reverse();
                            res.join("")
                        }
                    }
                }

                let spacer = self.config.get_spacer_override(&format).unwrap_or(spacer);

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
                            } else {//if let Some(_) = c {
                                return error("unknown");
                            }
                        }
                        
                        let big = BigInt::from_signed_bytes_le(&bytes);

                        (apply_spacer(&spacer,big.to_string()),WaveStyle::Normal,11)
                    },
                    NumberFormat::Uns => {
                        match BigUint::parse_bytes(value.as_bytes(),2) {
                            Some(big) => (apply_spacer(&spacer,big.to_string()),WaveStyle::Normal,11),
                            None => {return error("unknown")},
                        }
                    },
                    NumberFormat::Bin   => (apply_spacer(&spacer,value.to_string()),if value.contains('x') {WaveStyle::Error} else {WaveStyle::Normal},11),
                    _ => error("{number format not implemented yet}").0.unwrap() // TODO: add oct and hex number formats
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
                            sub.get(*style as usize).map_or(WaveStyle::Normal,|f|
                                f.1.0.as_ref()
                                     .map_or(WaveStyle::Normal,|r| r.1.clone())
                            )
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
                self.translate_with(&self.data.get_translator(ty),value)
            },
            TranslatorVariant::Sum(translators) => {
                let n = translators.len();
                let bits = (usize::BITS - (n-1).leading_zeros()) as usize;
                let variant = usize::from_str_radix(&value[..bits], 2);
                match variant {
                    Ok(variant) => {
                        if variant>=n {return error("{variant out of range}");}
                        let t = &translators[variant];
                        self.translate_with(t,&value[bits..])
                    }
                    Err(_) => error("unknown"),
                }
            },
            TranslatorVariant::Styled(s, t) => {
                let translation = self.translate_with(t,value);
                match translation {
                    Translation(Some((v,_,p)),sub) => Translation(Some((v,s.clone(),p)),sub),
                    trans => trans,
                }
            },
        };

        fn error(msg: &str) -> Translation {
            Translation(Some((String::from(msg),WaveStyle::Error,11)),vec![])
        }
    }
}


impl Data {
    fn new() -> Self {
        Self {
            signals: HashMap::new(),
            types: HashMap::new(),
            luts: HashMap::new(),
        }
    }

    /// Get the structure of a type
    fn type_structure(&self, ty: &String) -> Structure {
        // lookup type translator
        let trans = self.get_translator(ty);

        // create non-flattened structure for type
        self.trans_structure(trans)
    }

    /// Get the structure of a translator
    fn trans_structure(&self, trans: &Translator) -> Structure {
        let Translator{trans, ..} = trans;
        match trans {
            TranslatorVariant::Ref(ty) => self.type_structure(ty),
            TranslatorVariant::Sum(translators) => {
                let ts = translators.iter().map(|t|
                    self.trans_structure(t).0
                ).flatten().collect();
                Structure(ts)
            },
            TranslatorVariant::Product{subs, ..} => Structure(
                subs.iter()
                    .filter(|(name,_)| name.is_some())
                    .map(|(name,t)| (name.as_ref().unwrap().clone(), self.trans_structure(t)))
                    .collect()
                ),
            TranslatorVariant::Const(t) => Structure::from_trans(t),
            TranslatorVariant::Lut(_, structure) => structure.clone(),
            TranslatorVariant::Number{..} => Structure(vec![]),
            TranslatorVariant::Array{ sub, len, ..} => Structure(
                std::iter::repeat(self.trans_structure(sub)).take(*len as usize)
                    .enumerate().map(|(i,s)| (i.to_string(),s))
                    .collect()
            ),
            TranslatorVariant::Styled(_,translator) => self.trans_structure(translator),
            TranslatorVariant::Duplicate(name,translator) => Structure(
                vec![(name.clone(),self.trans_structure(translator))]
            ),
        }
    }


    /// Check whether a signal has a known associated Haskell type.
    fn can_translate(&self, signal: &String) -> bool {
        self.signals.get(signal).is_some()
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

impl Config {
    fn set_global_conf(&mut self, conf:Configuration) {
        self.global = conf;
        self.read_styles();
    }
    fn set_local_conf(&mut self, conf:Configuration) {
        self.local = conf;
        self.read_styles();
    }

    fn read_styles(&mut self) {
        self.global.style.clone().map(|s|self.apply_styles(s));
        for sfile in self.global.styles.clone() {
            self.read_style(sfile);
        }
        self.local.style.clone().map(|s|self.apply_styles(s));
        for sfile in self.local.styles.clone() {
            self.read_style(sfile);
        }
    }

    fn read_style(&mut self, sfile:String) {
        let mut path = Utf8PathBuf::from(sfile.replace("\\","/"));
        if let None = path.extension() {
            path.set_extension("toml");
        }

        let file = if path.starts_with("./") {
            //use local file - store VCD path in config
            //info!("path relative to waveform dir {:?}",self.wavesource_dir);
            self.wavesource_dir.as_ref().map(|dir|dir.join(&path).into_string())
        } else if path.is_absolute() || path.to_string().chars().skip(1).next()==Some(':') { // C-style disk
            //use absolute path
            Some(path.to_string())
        } else {
            //use path relative to config dir
            self.conf_dir.as_ref().map(|dir|dir.join(&path).into_string())
        };

        if let Some(file) = file {
            info!("Looking for style file {file}");
            if let Some(styles) = read_style_file(file) {
                self.apply_styles(styles);
            }
        } else {
            warn!("Could not find path for for {path:?}");
        }
    }
    fn apply_styles(&mut self, styles:Table) {
        for (key,value) in styles.into_iter() {
            self.style.insert(key,Right(value));
        }
    }

    fn get_style(&mut self, ws: &WaveStyle) -> WaveStyle {
        match ws {
            WaveStyle::Var(v,def) => {
                self.get_style_var(v).unwrap_or_else(||self.get_style(&**def))
            }
            ws => ws.clone()
        }
    }
    fn get_style_var(&mut self, var: &str) -> Option<WaveStyle> {
        match self.style.get(var) {
            Some(Left(ws)) => ws.clone(),
            Some(Right(val)) => {
                let ws = self.get_style_toml(val.clone());
                self.style.insert(var.to_string(),Left(ws.clone()));
                ws
            }
            None => {
                error!("SHOCKWAVES: Unknown style var {var}");
                None
            }
        }
    }
    fn get_style_toml(&mut self, val: TVal) -> Option<WaveStyle> {
        match val {
            TVal::String(s) => self.get_style_string(&s),
            TVal::Array(a) => a.into_iter().filter_map(|v|self.get_style_toml(v)).next(),
            v => {
                error!("SHOCKWAVES: Could not parse value {v:?}");
                None
            }
        }
    }
    fn get_style_string(&mut self, ws: &str) -> Option<WaveStyle> {
        Some(match ws {
            ws if ws=="NORMAL" => WaveStyle::Normal,
            ws if ws=="WARN" => WaveStyle::Warn,
            ws if ws=="ERROR" => WaveStyle::Error,
            hex if hex.starts_with("#") => {
                match Color32::from_hex(hex) {
                    Ok(c) => WaveStyle::Color(c),
                    Err(_) => {
                        error!("SHOCKWAVES: Invalid hex code: {hex}");
                        return None
                    }
                }
            }
            var if var.starts_with("$") => {
                return self.get_style_var(&var[1..])
            }
            col => {
                error!("SHOCKWAVES: Arbitrary color names are currently not surported: {col}");
                return None
            }
        })
        // hex6
        // hex3
        // var
        // colname
        // default name
    }

    fn do_prop_errors(&self) -> bool {
        self.local.propagate_errors.unwrap_or(
            self.global.propagate_errors.unwrap_or(true)
        )
    }

    fn get_spacer_override(&self,f:&NumberFormat) -> Option<&NumberSpacer> {
        match f {
            NumberFormat::Bin => self.local.override_bin_spacer.as_ref().or(
                self.global.override_bin_spacer.as_ref()),
            NumberFormat::Oct => self.local.override_oct_spacer.as_ref().or(
                self.global.override_oct_spacer.as_ref()),
            NumberFormat::Hex => self.local.override_hex_spacer.as_ref().or(
                self.global.override_hex_spacer.as_ref()),
            NumberFormat::Uns => self.local.override_uns_spacer.as_ref().or(
                self.global.override_uns_spacer.as_ref()),
            NumberFormat::Sig => self.local.override_sig_spacer.as_ref().or(
                self.global.override_sig_spacer.as_ref()),
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
    /// Fill out a translation according to the provided Structure. Currently O(n^2).
    fn fill(&mut self,structure: &Structure) -> () {
        let ssub = &structure.0;

        // recursively fill out provided subsignals
        for (n,t) in &mut self.1 {
            for (n2,s) in ssub {
                if n==n2 {
                    t.fill(&s);
                    break;
                }
            }
        }

        // add missing subsignals
        'outer: for (n,s) in ssub {
            for (n2,_) in &self.1 {
                if n==n2 {continue 'outer;}
            }
            self.1.push((n.clone(),Translation::from_struct(&s)))
        }
    }
    /// Create an empty translation from a structure
    fn from_struct(structure: &Structure) -> Self {
        Translation(None,structure.0.iter().map(|(n,s)| (n.clone(),Translation::from_struct(s))).collect())
    }
    /// Propagate error style upwards
    fn prop_errors(&mut self) -> bool {
        if self.1.iter_mut().map(|(_n,t)| t.prop_errors()).fold(false,|a,b| a||b) {
            match &mut self.0 {
                Some((_,s,_)) => {*s=WaveStyle::Error;},
                _ => {},
            }
            return true
        }
        match self.0 {
            Some((_,WaveStyle::Error,_)) => true,
            _ => false,
        }
    }
    /// Replace variables with actual wavestyles
    fn replace_wavestyles(&mut self,conf:&mut Config) {
        self.0.as_mut().map(|(_,s,_)| s.replace_wavestyles(conf));
        self.1.iter_mut().for_each(|(_,t)| t.replace_wavestyles(conf));
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
            WaveStyle::Var(..) => unreachable!(),
        }
    }
    fn replace_wavestyles(&mut self, conf:&mut Config) {
        if let WaveStyle::Var(..) = self {
            *self = conf.get_style(self);
        }
    }
}

impl Translator {
    fn replace_wavestyles(&mut self, conf:&mut Config) {
        self.trans.replace_wavestyles(conf);
    }
}

impl TranslatorVariant {
    fn replace_wavestyles(&mut self, conf:&mut Config) {
        match self {
            TranslatorVariant::Ref(_) => {},
            TranslatorVariant::Sum(subs) => subs.iter_mut().for_each(|t|t.replace_wavestyles(conf)),
            TranslatorVariant::Product{subs,..} => subs.iter_mut().for_each(|(_,t)|t.replace_wavestyles(conf)),
            TranslatorVariant::Const(t) => t.replace_wavestyles(conf),
            TranslatorVariant::Lut(..) => {},
            TranslatorVariant::Number{..} => {},
            TranslatorVariant::Array{sub,..} => sub.replace_wavestyles(conf),
            TranslatorVariant::Styled(style, translator) => {
                style.replace_wavestyles(conf);
                translator.replace_wavestyles(conf);
            },
            TranslatorVariant::Duplicate(_, t) => t.replace_wavestyles(conf),
        }
    }
}

impl Structure {
    /// Convert flattened Shockwaves `Structure` to Surfer `VariableInfo`
    fn convert(&self) -> VariableInfo {
        if self.0.len()==0 {
            VariableInfo::String
        } else {
            VariableInfo::Compound{
                subfields: self.0.iter().map(|(name,st)| (name.clone(),st.convert())).collect()
            }
        }
    }

    /// Generate a structure from a translation
    fn from_trans(trans:&Translation) -> Self {
        Structure(trans.1.iter().map(|(n,t)| (n.clone(),Self::from_trans(t))).collect())
    }
}








#[plugin_fn]
pub fn new() -> FnResult<()> {
    info!("SHOCKWAVES: Created plugin");

    // rewrite: set conf dir, read global conf.

    let dir = unsafe{conf_dir(())};
    if let Ok(Json(Some(dir))) = dir {
        let dir = Utf8PathBuf::from(&dir);
        info!("SHOCKWAVES: Config dir: {dir:?}");

        let mut file = dir.clone();
        file.push("shockwaves.toml");
        let file = file.into_string();

        info!("SHOCKWAVES: Looking for global config file: {file}");
        read_conf_file(file).map(|c| STATE.lock().unwrap().config.set_global_conf(c));

        STATE.lock().unwrap().config.conf_dir=Some(dir);
    } else {
        info!("SHOCKWAVES: No config directory detected");
    }
    STATE.lock().unwrap().config.conf_dir=None;
    Ok(())
}

#[plugin_fn]
pub fn name() -> FnResult<String> {
    Ok("Shockwaves ".to_string()+datetime_str!())
}

#[plugin_fn]
pub fn translates(variable: VariableMeta) -> FnResult<TranslationPreference> {
    let signal = signal_name(&variable);
    Ok(if STATE.lock().unwrap().data.can_translate(&signal) {
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
    info!("SHOCKWAVES: Loading source {wave_source:?}");

    let source_file = wave_source.map(|ws|
        match ws {
            WaveSource::File(f) => Some(f),
            WaveSource::Data => None,
            WaveSource::DragAndDrop(f) => f,
            WaveSource::Url(_) => None,
            WaveSource::Cxxrtl => None,
        }
    ).flatten().map(|ws|ws.replace("\\","/"));

    let (data,conf,wavesource_dir) = if let Some(source_file) = source_file {
        // if there is a proper file path, try to find the metadata file locally,
        // as well as a local config file.

        // META:
        let mut metafile = Utf8PathBuf::from(&source_file);
        metafile.set_extension("json");
        let metafile = metafile.into_string();

        info!("SHOCKWAVES: Looking for metadata file {metafile}");
        let data = read_meta_file(metafile);

        // CONF:
        let mut ws_dir = Utf8PathBuf::from(&source_file);
        ws_dir.pop();
        let mut conf_file = ws_dir.clone();
        conf_file.push("shockwaves.toml");
        let conf_file = conf_file.into_string();

        info!("SHOCKWAVES: Looking for local config file {conf_file}");
        let conf = read_conf_file(conf_file);

        (data,conf,Some(ws_dir))
    } else {
        info!("SHOCKWAVES: No suitable waveform source");
        (None,None,None)
    };

    let mut state = STATE.lock().unwrap();
    state.config.wavesource_dir = wavesource_dir;
    state.config.set_local_conf(conf.unwrap_or_else(||Configuration::default())); //change conf first, as this updates the styles too
    state.set_data(data.unwrap_or_else(||Data::new()));

    Ok(())
}

fn try_read_file(file: String) -> Option<Vec<u8>> {
    if let Ok(true) = unsafe{file_exists(file.clone())} {
        let bytes = unsafe{read_file(file.clone())};
        match bytes {
            Ok(bytes) => {
                Some(bytes)
            }
            Err(e) => {
                error!("SHOCKWAVES: Could not read file: {e}");
                None
            }
        }
    } else {
        warn!("SHOCKWAVES: File not found");
        None
    }
}

fn read_conf_file(file:String) -> Option<Configuration> {
    try_read_file(file).map(|bytes|
        match toml::from_slice(&bytes) {
            Ok(conf) => {
                info!("SHOCKWAVES: Parsed config file");
                Some(conf)
            }
            Err(e) => {
                error!("SHOCKWAVES: Could not config file: {e}");
                None
            }
        }
    ).flatten()
}

fn read_meta_file(file:String) -> Option<Data> {
    try_read_file(file).map(|bytes|
        match serde_json::from_slice(&bytes) {
            Ok(data) => {
                info!("SHOCKWAVES: Parsed metadata file");
                Some(data)
            }
            Err(e) => {
                error!("SHOCKWAVES: Could not parse metadata file: {e}");
                None
            }
        }
    ).flatten()
}

fn read_style_file(file:String) -> Option<Table> {
    try_read_file(file).map(|bytes|
        match toml::from_slice::<Table>(&bytes) {
            Ok(style) => {
                info!("SHOCKWAVES: Parsed metadata file");
                Some(style)
            }
            Err(e) => {
                error!("SHOCKWAVES: Could not parse metadata file: {e}");
                None
            }
        }
    ).flatten()
}

#[host_fn]
extern "ExtismHost" {
    pub fn read_file(filename: String) -> Vec<u8>;
    pub fn file_exists(filename: String) -> bool;
    pub fn conf_dir(_user_data: ()) -> Json<Option<String>>;
}

