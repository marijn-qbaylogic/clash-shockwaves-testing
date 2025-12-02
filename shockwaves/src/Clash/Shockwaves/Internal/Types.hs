{-|

Type definitions for Shockwaves.


-}


{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Clash.Shockwaves.Internal.Types where
import Clash.Prelude hiding (sub)
import qualified Data.List as L
import Data.Map as M
import Data.Data (Typeable)

import Data.Aeson hiding (Value)
import Data.Colour.SRGB (RGB(..), toSRGB24, Colour)
import Data.Word (Word8)
import Control.DeepSeq (NFData (rnf))
import Data.String (IsString)
import GHC.Exts (IsString(fromString))
import Data.Colour.Names (readColourName)
import Data.Maybe (fromJust)
import Data.Char (digitToInt)

-- some type aliases for clarity
type TypeName = String
-- ^ Name of a type.
type SubSignal = String
-- ^ Name of a subsignal.
type SignalName = SubSignal
-- ^ Name of a signal.
type Value = String
-- ^ Text displayed as the value of a signal.
type Prec = Integer
-- ^ Operator precedence of the value.
type Render = Maybe (Value, WaveStyle, Prec)
-- ^ Rendered value. This can be @Nothing@ is the value does not exists,
-- or a tuple of the text representation, style, and precedence.
type BinRep = String
-- ^ Binary representation of a haskell value (like 'BitVector', but arbitrarily sized).
type LUTName = TypeName
-- ^ Reference to a LUT.


-- | Map that links signal names to their types.
type SignalMap = Map SignalName TypeName
-- | Map that links type names to their information.
type TypeMap = Map TypeName Translator
-- | Table of LUTs. Usually, the index is a type name, but this is not necessarily the case.
type LUTMap = Map LUTName LUT
-- | A lookup table of 'Translation's.
type LUT = Map BinRep Translation

type Color = RGB Word8
-- ^ The color type used in 'WaveStyle'.

-- | Translation of a value.
-- The translation consists of a 'Render' value (the representation of the value itself)
-- and a list of subsignal translations.
data Translation
  = Translation Render [(SubSignal,Translation)]
  deriving (Show,Generic,ToJSON,NFData,Eq)

-- | The style in which a signal should be displayed.
data WaveStyle
  = WSNormal -- ^ The default waveform style.
  | WSWarn -- ^ A warning value.
  | WSError -- ^ An error value. Errors are propagated by translators.
  | WSUndef -- ^ An undefined value.
  | WSHighImp -- ^ A high impedance value.
  | WSDontCare -- ^ A value that does not matter.
  | WSWeak -- ^ A weakly defined value.
  | WSColor Color -- ^ A custom color. See "Clash.Shockwaves.Style" for more information.
  | WSVar String WaveStyle -- ^ A variable in a style configuration file, with a default.
  deriving (Show, Generic, Eq)

instance NFData WaveStyle where
  rnf !_ = ()

-- | Different number formats.
data NumberFormat
  = NFSig -- ^ A signed decimal value.
  | NFUns -- ^ An unsigned decimal value.
  | NFHex -- ^ A hexadecimal value. TODO: Supports partially undefined values.
  | NFOct -- ^ An octal value. TODO: Supports partially undefined values.
  | NFBin -- ^ A binary value.
  deriving (Show, Typeable, Generic, NFData)

type NumberSpacer = Maybe (Integer,String)

-- | A structure value that shows what subsignals are present.
newtype Structure
  = Structure [(SubSignal,Structure)]
  deriving (Show,Generic,ToJSON)

-- | A translator. The translator has a width, indicating the number of bits it
-- translates, as well as a 'TranslatorVariant' that determines the translation algorithm.
data Translator = Translator Int TranslatorVariant deriving (Show)

-- | The translation algorithm used.
data TranslatorVariant
  -- | Use the translator of a different type. Note that the width value of the
  -- 'Translator's should still match. The structure is only used so that the
  -- structure can be reconstructed from the translator alone, and is not
  -- actually stored in the final output.
  = TRef TypeName Structure

  -- | A reference to a lookup table.
  | TLut LUTName Structure

  | TSum [Translator]
  -- ^ Select one translator to be used based on the first bits of the binary
  -- representation. Translate using the selected translator. Keep in mind that
  -- problems may occur if subsignal names are shared.

  -- | Split the binary data into separate fields, translate each of these,
  -- and join together the values.
  --
  -- Example:
  --
  -- @
  -- data T = T{a::Bool,b::Bool}
  -- translatorVariantT = TProduct
  --   { subs = [("a",Bool,"b",Bool)],
  --   , start = "T{"
  --   , sep = ","
  --   , stop = "}"
  --   , labels = ["a=","b="]
  --   , preci = 0
  --   , preco = 11
  --   }
  -- @
  | TProduct
    { subs          :: [(Maybe SubSignal, Translator)] -- ^ List of fields to translate.
    , start        :: Value -- ^ Text to insert at the start of the value.
    , sep          :: Value -- ^ Text to use to separate values.
    , stop         :: Value -- ^ Text to insert at the end of the value.
    , labels       :: [Value]
    -- ^ List of labels to insert before each value.
    -- If empty, insert no labels.
    -- Else, the length must match that of @subs@, and provided values are inserted.
    , preci        :: Prec -- ^ Inner precedence: used on subvalues.
    , preco        :: Prec -- ^ Outer precedence: used for the combined value.
    , style        :: Int
    -- ^ Select which field should be used to determine the style.
    -- If no style is present in this field, or style is set to -1,
    -- use the default style instead.
    }

  -- | An array value. This behaves much like 'TProduct', except that no labels
  -- are provided, and all fields use the same translator.
  | TArray
    { sub    :: Translator -- ^ Translator used for all values.
    , len    :: Int -- ^ Length of the array.
    , start  :: Value -- ^ Text inserted at the start of the value.
    , sep    :: Value -- ^ Text to use to separate values.
    , stop   :: Value -- ^ Text to insert at the end of the value.
    , preci  :: Prec -- ^ Inner precedence: used on subvalues.
    , preco  :: Prec -- ^ Outer precedence: used for the combined value.
    }

  -- | Translate a value only if the first bit of the binary representation is
  -- @1@. If it is @0@, display nothing.
  | TDuplicate SubSignal Translator

  -- | Apply a style to a translation.
  -- Does not change the structure.
  | TStyled WaveStyle Translator

  -- | A numerical value.
  | TNumber
    { format :: NumberFormat
      -- ^ Format used to display data.
    , spacer :: NumberSpacer
      -- ^ Optional spacer to improve readability
    }

  -- | A constant translation value. The binary value
  -- provided is completely ignored, even if not properly defined.
  | TConst Translation

  deriving (Show)



instance IsString WaveStyle where
  fromString ('#':hex) = WSColor $ go $ L.map (fromIntegral . digitToInt) hex
    where go :: [Word8] -> Color
          go [r,r',g,g',b,b'] = RGB (16*r+r') (16*g+g') (16*b+b')
          go [r,g,b] = RGB (17*r) (17*g) (17*b)
          go _ = error ("bad hex code #"<>hex)
  fromString ('$':var) = WSVar var WSNormal
  fromString s =
      WSColor . toSRGB24 . fromJust
    $ (readColourName s :: (Maybe (Colour Double)))


instance ToJSON Translator where
  toJSON (Translator w v) = object ["w" .= w, "v" .= v']
    where v' = case v of
                TRef n _ -> object ["R" .= n]
                TSum subs -> object ["S" .= toJSON subs]
                TProduct{subs,start,sep,stop,labels,preci,preco,style} ->
                  object
                    ["P" .= object
                      [ "t" .= toJSON subs
                      , "[" .= start
                      , "," .= sep
                      , "]" .= stop
                      , "n" .= labels
                      , "p" .= preci
                      , "P" .= preco
                      , "s" .= style ]]
                TConst t -> object ["C" .= toJSON t]
                TLut lut s -> object ["L" .= [toJSON lut,toJSON s]]
                TNumber{format,spacer} -> object ["N" .= object
                  [ "f" .= format
                  , "s" .= spacer]]
                TArray{sub,len,start,sep,stop,preci,preco} -> object ["A" .= object
                  [ "t" .= toJSON sub
                  , "l" .= len
                  , "[" .= start
                  , "," .= sep
                  , "]" .= stop
                  , "p" .= preci
                  , "P" .= preco ]]
                TStyled s t -> object ["X" .= [toJSON s,toJSON t]]
                TDuplicate n t -> object ["D" .= [toJSON n,toJSON t]]

instance ToJSON WaveStyle where
  toJSON = \case
    WSNormal   -> "N"
    WSWarn     -> "W"
    WSError    -> "E"
    WSUndef    -> "U"
    WSHighImp  -> "Z"
    WSDontCare -> "X"
    WSWeak     -> "Q"
    WSColor (RGB r g b) -> object ["C" .= [r,g,b,255]]
    WSVar var dflt -> object ["V" .= [toJSON var, toJSON dflt]]
instance ToJSON NumberFormat where
  toJSON = \case
    NFSig -> "S"
    NFUns -> "U"
    NFHex -> "H"
    NFOct -> "O"
    NFBin -> "B"