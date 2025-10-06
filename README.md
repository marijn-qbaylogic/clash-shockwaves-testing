# clash-shockwaves

Shockwaves is a typed waveform viewer solution for Clash. It consists of two parts:

1. A Haskell library (`Clash.Shockwaves`) that provides tools for representing Haskell types as waveforms,
   customizing the appearance, and performing tracing simulations that store additional translation information.
2. An extension to the Surfer waveform viewer, that can use this additional information to display the waveforms.

