{-|

A collection of all the things needed to create custom 'Waveform' implementations.

-}


-- all you need for custom waveform implementations
module Clash.Shockwaves.Waveform (
  Waveform(translator,translate',addSubtypes,addValue,hasLUT,styles),
  Translation(..),
  WaveStyle(..),
  Value,
  Prec,
  SubSignal,
  Translator(..),
  Structure(..),
  structure,
  NumberFormat(..),
  WaveformForConst(..),
  WaveformForNumber(..),
) where

import Clash.Shockwaves.Internal.Types
import Clash.Shockwaves.Internal.Translator

import Clash.Shockwaves.Internal.Waveform