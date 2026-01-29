{-|

Module exporting the tools required for custom 'WaveformLUT' implementations.

-}

module Clash.Shockwaves.LUT (
  Waveform,
  WaveformLUT(structureL,translateL),
  translateWith,
  renderWith, renderShow,
  translateAtomWith, translateAtomShow,
  translateAtomSigWith, translateAtomSigShow,
  safeTranslation, safeVal, safeValOr,
  noSplit, splitL,
  precL,
  WaveformForLUT(..),
  tLut,

  Translation(..),
  Value,
  WaveStyle(..),
  Prec,
  Structure(..),
  SubSignal,
  structure,structureT,
  tFromVal
) where

import Clash.Shockwaves.Internal.Types
import Clash.Shockwaves.Internal.Waveform
import Clash.Shockwaves.Internal.Translator
import Clash.Shockwaves.Internal.Util