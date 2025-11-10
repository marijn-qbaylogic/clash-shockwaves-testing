{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Tests.Types where

import Clash.Prelude
import Data.Typeable
import Clash.Shockwaves.Waveform
import Clash.Shockwaves.LUT

{-

(void) / single constructor / multiple constructors
no fields / one field / two fields / multiple fields / struct

-}

data S = S
  deriving (ShowX, BitPack, NFDataX, Generic, Typeable, Waveform)
data M = Ma | Mb | Mc
  deriving (ShowX, BitPack, NFDataX, Generic, Typeable, Waveform)

data U = U
  deriving (ShowX, BitPack, NFDataX, Generic, Typeable, Waveform)
data F = M Bool Int
  deriving (ShowX, BitPack, NFDataX, Generic, Typeable, Waveform)

infixl 5 ://:
data Op a b = a ://: b
  deriving (ShowX, BitPack, NFDataX, Generic, Typeable, Waveform)
data St = St{a::Bool,b::Int}
  deriving (ShowX, BitPack, NFDataX, Generic, Typeable, Waveform)

data C = Red | Green | Blue
   deriving (ShowX, BitPack, NFDataX, Generic, Typeable)
instance Waveform C where
  styles = [WSVar "red" "#f00",WSVar "green" "lime",WSVar "blue" "#0000ff"]

infixr 6 :**:
data Mix z
  = A
  | B Bool
  | C Bool Int
  | D {x::Bool,y::Int}
  | (Unsigned 4) :**: z
  deriving (ShowX, BitPack, NFDataX, Generic, Typeable, Waveform)

data L
  = La Bool Bool
  | Lb Bool Bool
  deriving (ShowX,BitPack, NFDataX, Generic, Typeable)
  deriving Waveform via (WaveformForLUT L)

instance WaveformLUT L where
  labelL (La x y) = show x <> " <A> " <> show y
  labelL (Lb x y) = show x <> " <B> " <> show y

  styleL (La _ _) = "red"
  styleL (Lb _ _) = "green"