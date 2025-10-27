
-- all you need for custom waveform implementations
module Clash.Shockwaves.Waveform (
  Waveform(translator,translate',addSubtypes,addValue,hasLUT,styles),
  Translation(..),
  WaveStyle(..),
  Value,
  Prec,
  SubSignal,
  Structure,
  Translator(..),
  NumberFormat(..),
  WaveformForConst(..),
  WaveformForNumber(..),
) where

import Clash.Shockwaves.Internal.Types

import Clash.Shockwaves.Internal.Waveform