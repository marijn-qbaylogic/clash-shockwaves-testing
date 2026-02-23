mod cache;
mod config;
mod convert;
mod data;
mod plugin;
mod state;
mod structure;
mod stylevars;
mod translate;

/*

The extension has a global state, storing the translation metadata,
configuration options, and a cache of structures.

When a VCD file is opened, the translator looks for a JSON file with the same
base name. If found, this file is parsed to obtain the metadata.

On start and when a VCD file is loaded, the extension also looks for a global and
local configuration respectively. When a VCD is loaded, any style files listed in
these configuration files are re-read, and all defined style variables are listed.
Any occurances of these style variables in the metadata are then replaced.

When Surfer looks whether a signal can be translated, the signal is looked for
in the map of known signals.

When Surfer asks for a signal's structure, the signal's type is determined, and
the structure of that type is either recovered from cache, or determined from the
type's translator.

When Surfer translates a value, the type is obtained from the signal, the translator
is obtained from the type, and the translator is used to translate the binary data.
The translation is then parsed once again to propagate any special styles.

The translators produce a translation that has an render value (the text, style and
operator precedence of the toplevel signal) and optional subsignals.

*/
