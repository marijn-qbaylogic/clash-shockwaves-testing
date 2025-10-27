# Shockwaves Haskell Library

This is the Haskell library for Shockwaves, a typed waveform solution designed for Clash.

The basic concept of the library is that for all types for which signals exist in simulation,
the `Waveform` class is implemented. This class provides all the functionality needed to store
metadata about types when simulating. This metadata is stored separately, and can be used
by the waveform viewer to reconstruct a typed representation in the waveform viewer.

By default, `Waveform` uses `Generics` to derive a waveform viewer representation similar
to `Show`. Types with multiple constructors and constructors with fields get subsignals.

The `Waveform` class can of course be customized. This allows for more advanced control over
the way data is represented in the waveform viewer. Particularly, through `WaveformLUT`,
translations can be implemented as lookup tables, allowing for arbitrary code execution
in Haskell to create whatever data is desired.

The library includes a modified version of `Clash.Signal.Trace` that uses `Waveform` to generate metadata.




## Default behaviour

The default implementations of Waveform make several "clever" choices in how to display values.

For types with multiple constructors and fields, subsignals are created per constructor,
and for each constructor, subsignals are created for each of its fields.
Constructor subsignals are labeled with the name of the constructor,
and field subsignals with the field names.
If there are no field names, the subsignals are numbered instead.
The value displayed at the toplevel is the same as that of the current constructor.

If a type has only one constructor, there is no need for subsignals for the constructor.
Instead, the subsignals for the fields added to the toplevel signal directly.

If a constructor has exactly one field, it will by default copy the style of the contained value.

If a constructor is an operator and has exactly two fields, the constructor will be rendered as
and infix. The operator precedence is used to appropriately add parentheses in nested values.
If an operator constructor has a different number of fields, it is parenthesized and used as normal.

Values are always rendered according to the information actually stored in the binary representation.
This means the system will render both `(undefined,undefined)` and `undefined` as `(undefined,undefined)`:
since there is only one constructor, the value can only be that constructor, and this is known
regardless of whether the value was actually `unknown` during code execution or not.
The translation functions provided in the library, which are meant for creating LUT-based translations,
mimic this behaviour.

(TODO) If a translation has the `WSError` style, this is propagated through all signals above.
`WSWarn` is displayed in the same manner, but not propagated.


# Tutorial

This sections contains instructions for some common uses of Shockwaves.

## Derive Waveform

The easiest way to use Shockwaves is to simply derive the `Waveform` class for all types
that appear in the output. This requires `Generic` and `Typeable` to also be derived.

## Adding coloured constructors

It might be useful to mark the different constructors of a type using various colors.
To do this, one simply needs to do a custom implementation of Waveform where `styles`
is overwritten with a list of waveform styles, in the order that the constructors appear.

## Translations using LUTs

Sometimes, it is easier to render values in Haskell, and use a lookup table in the
waveform viewer. This can be implemented using `WaveformLUT`. To use `WaveformLUT`,
derive `Waveform` via `WaveformForLUT`.

By default, the class uses the `Show` instance of the type, and `Generic` to determine
subsignals. However, this can be easily modified.

To change the display behaviour, overwrite `displayL` or any of the functions `labelL`
(for the text), `styleL` (the wavestyle) and `precL` (operator precedence - set to 11
for atomic values and 10 for values containing spaces).

To change the subsignals, modify `splitL` to generate the desired subsignal translations
from a value. Also overwrite `structureL` to return a structure that matches that of
the translations created by `splitL`.

It is also possible to overwrite `translateL` instead of `displayL` and `splitL`, but
this is discouraged. The function needs to be written such that it always returns
a valid translation, even if there are unknown (sub)values.

## Integer values

It is easy to add representation of integer values. Simply derive `Waveform` via
`WaveformForNumber <format>`, where `<format>` specifies the formatting that should
be used. To use this, the type must be convertible to `Integer`.

example: Unsigned

## Custom translations

It is of course also possible to customize the behaviour further by using a custom
implementation of `Waveform`.

(TODO: explain various functions)

example: Maybe

## GADTs

GADTs are particularly annoying to deal with, and are a good reason to use LUTs.
However, in case of large data structures, this is non-ideal, and a custom
implementation of `Waveform` is preferred.

example: Vec, RTree

## Constant value

Sometimes, it might be easiest to provide a constant value as the translation
of a value. In that case, `Waveform` can be derived via `WaveformForConst`.
The class `WaveformConst` then provides a constant translation value under
`constTrans`.