{-|
Module      : Clash.Shockwaves.LUT
Description : Shockwaves tools for LUTs

Everything needed to create 'Waveform' instances that use lookup tables.

-}

module Clash.Shockwaves.LUT (
  -- * WaveformLUT
  WaveformLUT(structureL,translateL),
  translateWith,
  renderWith, renderShow,
  translateAtomWith, translateAtomShow,
  translateAtomSigWith, translateAtomSigShow,
  noSplit, splitL,
  precL,
  WaveformForLUT(..),
  tLut,

  -- * Utility
  tFromVal, rFromVal,
  safeWHNF, safeVal, safeValOr,
  safeTranslation,
  errorT, errorR,

  -- * Standard Shockwaves types and functions
  Waveform,
  Translation(..),
  Value,
  WaveStyle(..),
  Prec,
  Structure(..),
  SubSignal,
  structure,structureT,
) where

import Clash.Shockwaves.Internal.Types
import Clash.Shockwaves.Internal.Waveform
import Clash.Shockwaves.Internal.Translator
import Clash.Shockwaves.Internal.Util