{-|

Module exporting the tools required for custom 'WaveformLUT' implementations.

-}

module Clash.Shockwaves.LUT (
  Waveform,
  WaveformLUT(structureL,translateL),
  displaySplit,
  displayWith, displayShow,
  displayAtomWith, displayAtomShow,
  displayAtomSigWith, displayAtomSigShow,
  noSplit, splitG,
  precL,
  WaveformForLUT(..),

  Translation(..),
  Value,
  WaveStyle(..),
  Prec,
  Structure(..),
  SubSignal,
  structure
) where

import Clash.Shockwaves.Internal.Types
import Clash.Shockwaves.Internal.Waveform
import Clash.Shockwaves.Internal.Translator