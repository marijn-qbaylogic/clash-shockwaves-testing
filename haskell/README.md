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