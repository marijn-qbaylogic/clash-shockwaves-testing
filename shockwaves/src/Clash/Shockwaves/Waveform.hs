{-|

A collection of all the things needed to create custom 'Waveform' implementations.

-}


-- all you need for custom waveform implementations
module Clash.Shockwaves.Waveform (
  Waveform(translator,styles),
  translate,translateBin,hasLut,
  translateBinT,hasLutT,
  Translation(..),
  Render,
  WaveStyle(..),
  Value,
  Prec,
  SubSignal,
  Translator(..),
  tRef,tDup,tStyled,tConst,
  TranslatorVariant(..),
  Structure(..),
  structure,
  NumberFormat(..),
  WaveformConst(..),WaveformForConst,
  WaveformForNumber(..),
  DecSpacer,HexSpacer,OctSpacer,BinSpacer,NoSpacer,
) where

import Clash.Shockwaves.Internal.Types
import Clash.Shockwaves.Internal.Translator

import Clash.Shockwaves.Internal.Waveform