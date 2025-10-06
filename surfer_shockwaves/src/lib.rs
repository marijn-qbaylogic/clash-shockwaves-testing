

use extism_pdk::{plugin_fn, FnResult, Json};
use extism_pdk::host_fn;
use std::sync::Mutex;
pub use surfer_translation_types::plugin_types::TranslateParams;
use surfer_translation_types::{
    translator::{TrueName, VariableNameInfo},
    SubFieldTranslationResult, TranslationPreference, TranslationResult, ValueKind, VariableInfo,
    VariableMeta, VariableValue,
};


static STATE: Mutex<bool> = Mutex::new(false);


#[plugin_fn]
pub fn name() -> FnResult<String> {
    Ok("Shockwaves".to_string())
}

#[plugin_fn]
pub fn translates(variable: VariableMeta<(), ()>) -> FnResult<TranslationPreference> {
    //Ok(TranslationPreference::Prefer)
    Ok(TranslationPreference::No)
}

#[plugin_fn]
pub fn variable_info(variable: VariableMeta<(), ()>) -> FnResult<VariableInfo> {
    Ok(VariableInfo::Compound {
        subfields: vec![(String::from("sub"),VariableInfo::String)]
    })
}

#[plugin_fn]
pub fn translate(
    TranslateParams { variable, value }: TranslateParams,
) -> FnResult<TranslationResult> {

    todo!()
}



#[host_fn]
extern "ExtismHost" {
    pub fn read_file(filename: String) -> Vec<u8>;
    pub fn file_exists(filename: String) -> bool;
}

