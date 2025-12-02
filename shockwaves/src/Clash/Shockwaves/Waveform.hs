{-|

A collection of all the things needed to create custom 'Waveform' implementations.

-}


-- all you need for custom waveform implementations
module Clash.Shockwaves.Waveform (
  Waveform(translator,translate',addSubtypes,addValue,hasLUT,styles),
  Translation(..),
  Render,
  WaveStyle(..),
  Value,
  Prec,
  SubSignal,
  Translator(..),
  tRef,tDup,tStyled,
  TranslatorVariant(..),
  Structure(..),
  structure,
  NumberFormat(..),
  WaveformForConst(..),
  WaveformForNumber(..),
  DecSpacer,HexSpacer,OctSpacer,BinSpacer,NoSpacer,
) where

import Clash.Shockwaves.Internal.Types
import Clash.Shockwaves.Internal.Translator

import Clash.Shockwaves.Internal.Waveform