{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Shockwaves.Internal.Waveform where

import           Clash.Prelude

import           Clash.Shockwaves.Internal.Types
import qualified Clash.Shockwaves.BitList as BL
import           Clash.Shockwaves.BitList (BitList)
import           Clash.Shockwaves.Internal.Translator
import           Clash.Shockwaves.Internal.Util

import           GHC.Generics
import           Data.Proxy
import           Data.Typeable
import           Data.Map             as M
import qualified Data.List            as L
import           Data.Maybe           (fromMaybe)
import           Data.Char            (isAlpha)

-- for standard type instances
import           Data.Int             (Int8,Int16,Int32,Int64)
import           Data.Word            (Word8,Word16,Word32,Word64)

import           Clash.Num.Zeroing    (Zeroing, fromZeroing)
import           Clash.Num.Wrapping   (Wrapping, fromWrapping)
import           Clash.Num.Saturating (Saturating, fromSaturating)
import           Clash.Num.Overflowing(Overflowing, fromOverflowing)
import           Clash.Num.Erroring   (Erroring, fromErroring)
import           Data.Complex         (Complex)
import           Data.Ord             (Down)
import           Data.Functor.Identity(Identity)
import           Control.DeepSeq      (NFData)

-- import Data.Maybe (fromMaybe)
-- import Clash.Sized.Internal.BitVector (xToBV)


-- | Get a 'Render' from a 'Value' using 'WSDefault' and precedence 11.
rFromVal :: Value -> Render
rFromVal v = Just (v,WSDefault,11)

-- | Get a 'Translation' from a 'Value' using 'WSDefault' and precedence 11.
tFromVal :: Value -> Translation
tFromVal v = Translation (rFromVal v) []


-- | Wrap a 'Translator' in a 'TStyled' translator using some style, unless the
-- provided style is 'WSDefault'.
wrapStyle :: WaveStyle -> Translator -> Translator
wrapStyle WSDefault t = t
wrapStyle s         t = tStyled s t

-- | Wrap a 'Translator' in a 'TStyled' variant translator with the
-- provided style.
tStyled :: WaveStyle -> Translator -> Translator
tStyled s (Translator w v) = Translator w $ TStyled s (Translator w v)

-- | Wrap a 'Translator' in a 'TDuplicate' variant translator with the
-- provided subsignal name.
tDup :: SubSignal -> Translator -> Translator
tDup name (Translator w t) = Translator w $ TDuplicate name (Translator w t)

-- | Generate a translator reference for a type.
tRef :: Waveform a => Proxy a -> Translator
tRef (_::Proxy a) = Translator (width @a) $ TRef (typeName @a) (structure $ translator @a) (translateBin @a)

tConst :: Render -> Translator
tConst r = Translator 0 $ TConst $ Translation r []

-- | Create an error value from an optional error message.
errMsg :: Maybe Value -> Value
errMsg = maybe "undefined" (\e -> "{undefinedmsg:"<>e<>"}")




-- -- | Check if a value is safe.
-- -- If not, create an error message and apply the provided function to it.
-- safeValOrMsg :: (NFData a) => (Value -> a) -> a -> a
-- safeValOrMsg f x = case safeVal x of
--   Right x' -> x'
--   Left e -> f $ errMsg e

-- | Check if a value is safe.
-- If not, return the default value provided.
safeValOr :: (NFData a) => a -> a -> a
safeValOr y x = case safeVal x of
  Right x' -> x'
  Left _e -> y

------------------------------------------ WAVEFORM --------------------------------------

{-|

Main class for making types displayable in the waveform viewer.
The class is responsible for defining an appropriate translator and subsignal
structure, as well as registering types.

To make LUT approaches possible, the class must also be able to translate values,
and to register individual values. The latter requires the values to be split into
their subvalues, so that they may be registered in their subsignals.

By default, 'GHC.Generics.Generic' is used to automatically derive this behaviour.
Extra classes are provided to help implement lookup tables or numerical translators.
Custom implementations are generally ill-advised, but may be needed for GADTs.

-}

class (Typeable a, BitPack a) => Waveform a where
  -- | Provide the type name.
  -- Overriding this value is only really useful for derive via strategies.
  typeName :: TypeName
  typeName = typeNameP (Proxy @a)

  -- | The translator used for the data type. Must match the structure value.
  translator :: Translator
  default translator :: (WaveformG (Rep a ())) => Translator
  translator = translatorG @(Rep a ()) (width @a) (styles' @a)

  -- | Function to translate values. By default, this function creates a translation from
  -- the binary representation of the data using translateBin, and the translator.
  -- There isn't really a good reason to overwrite this function.
  translate :: a -> Translation
  translate x = translateBin @a (BL.binPack x)
    -- Left (Just e) -> transError ("{undefined: "<>e<>"}")

  -- | Translate binary data.
  -- For LUTs, this involves translating the value back to a bitvector and back to the original type.
  -- For other types, this simply translates the value according to the translator.
  translateBin :: BitList -> Translation
  translateBin = translateBinWith (translator @a)
  -- waveform: translateBinWith (translator @a)
  -- lut: string -> bitvector -> a -> translation

  -- | Register this type and all its subtypes. Do not override.
  addTypes :: TypeMap -> TypeMap
  addTypes tm = if M.member self tm then
                  tm
                else
                  addSubtypes @a $ M.insert self (translator @a) tm
    where self = typeName @a
    --addSubtypes @a $ insertIfMissing (typeName (Proxy @a)) (meta @a) tm

  -- | Register all subtypes of this type. Custom implementations might need to
  -- modify this.
  addSubtypes :: TypeMap -> TypeMap
  default addSubtypes :: (WaveformG (Rep a ())) => TypeMap -> TypeMap
  addSubtypes = addTypesG @(Rep a ())

  -- | Register a value in the table of LUTs.
  -- If this type does not use a LUT, but at least one of its subvalues does,
  -- split the value and run this function recursively on the subvalues.
  addValue :: a -> LUTMap -> LUTMap
  default addValue :: (Generic a, WaveformG (Rep a ())) => a -> LUTMap -> LUTMap
  addValue x = if hasLUT @a then
                 addValueG (from x :: Rep a ())
               else
                 id

  -- | Denotes if data type or any of its subtypes uses LUTs.
  hasLUT :: Bool
  default hasLUT :: (WaveformG (Rep a ())) => Bool
  hasLUT = hasLUTG @(Rep a())

  -- | Runtime bitsize of the type.
  width :: Int
  width = bitsize (Proxy @a)

  -- | List of styles used for constructors.
  --
  -- Since giving different constructors different colors is a very common usecase
  -- of the waveform style,
  -- this list can be overridden to provides styles for the constructors, in order.
  -- For missing styles, the default is used.
  styles :: [WaveStyle]
  styles = []

  -- | Helper function that adds the default style to the 'styles' list. Do not override.
  styles' :: [WaveStyle]
  styles' = styles @a <> L.repeat WSDefault









------------------------------------------- GENERIC -------------------------------------

-- | A class for obtaining the required behaviour of 'Waveform' through "GHC.Generics".
-- The exact details might change later; use at your own risk.
class WaveformG a where
  -- | Given a bitsize and list of styles for the constructors, provide a translator. Defined only for full types and constructors
  translatorG :: Int -> [WaveStyle] -> Translator

  -- Return a list of translators for constructors as subsignals; defined for constructors, :+: and types with multiple constructors.
  constrTranslatorsG :: [WaveStyle] -> [Translator]
  -- Return a list of translators for fields; defined for fields, :*:, constructors, and types with a single constructor.
  -- Product types are numbered for types and constructors only.
  fieldTranslatorsG :: [(SubSignal,Translator)]

  -- | Add all subtypes to the type map, recusively.
  addTypesG :: TypeMap -> TypeMap
  -- | Split a value into its subvalues and add them or their subvalues to a LUT 
  -- if needed, recursively.
  addValueG :: a -> LUTMap -> LUTMap
  -- | Return whether any of the subtypes makes use of a LUT.
  hasLUTG :: Bool

  -- | Bitsize of a type. Only used to determine the width of constructors
  -- (and their fields): constructors, :*:, fields.
  widthG :: Int -- for individual constructors

  -- | For LUTs, create translation subsignals from supplied 'Render' value.
  -- Duplicate the value if there are multiple constructors, and
  -- just translate the fields. If getting the constructor fails,
  -- create no subsignals.
  -- Defined for types, :+: and constructors.
  translateWithG :: Render -> a -> [(SubSignal,Translation)]

  -- | For LUTs. Translate all fields of a (the) constructor. Defined for constructors, :*:, fields
  -- and types with 1 constructor.
  translateFieldsG :: a -> [(SubSignal,Translation)]



-- void type (assuming it has a custom bitpack implementation)
instance WaveformG (D1 m1 V1 k) where
  translatorG _ _ = Translator 0 $ TConst $ Translation Nothing []
  constrTranslatorsG _ = undefined
  fieldTranslatorsG = undefined

  addTypesG = id
  addValueG _ = id
  hasLUTG = False

  widthG = undefined

  translateWithG _ _ = []
  translateFieldsG = undefined

-- single constructor type
instance (WaveformG (C1 m2 s k), WaveformG (s k)) => WaveformG (D1 m1 (C1 m2 s) k) where
  translatorG = translatorG @(C1 m2 s k)
  constrTranslatorsG = undefined
  fieldTranslatorsG = fieldTranslatorsG @(C1 m2 s k)

  addTypesG = addTypesG @(C1 m2 s k)
  addValueG x = addValueG @(C1 m2 s k) (unM1 x)
  hasLUTG = hasLUTG @(C1 m2 s k)

  widthG = undefined

  translateWithG r x = case translateWithG r (unM1 x) of
    [(_,Translation _ subs)] -> subs --remove duplicated singal from constructor
    _                        -> undefined
  translateFieldsG x = translateFieldsG (unM1 x)


-- multiple constructors type
instance WaveformG ((a :+: b) k) => WaveformG (D1 m1 (a :+: b) k) where
  translatorG w sty = Translator w . TSum $ constrTranslatorsG @((a :+: b) k) sty
  constrTranslatorsG = constrTranslatorsG @((a :+: b) k)
  fieldTranslatorsG = undefined

  addTypesG = addTypesG @((a :+: b) k)
  addValueG x = addValueG (unM1 x)
  hasLUTG = hasLUTG @((a :+: b) k)

  widthG = undefined

  translateWithG r x = fromMaybe [] $ safeWHNF $ translateWithG r (unM1 x)
  translateFieldsG = undefined


-- leftright :: (a :+: b) k -> Either (a k) (b k)
-- leftright (L1 x) = Left x
-- leftright (R1 y) = Right y

-- multiple constructors
instance (WaveformG (a k), WaveformG (b k)) => WaveformG ((a :+: b) k) where
  translatorG = undefined
  constrTranslatorsG sty = a <> b
    where
      a = constrTranslatorsG @(a k) sty
      b = constrTranslatorsG @(b k) (L.drop (L.length a) sty)
  fieldTranslatorsG = undefined

  addTypesG = addTypesG @(a k) . addTypesG @(b k)
  addValueG xy = case safeWHNF xy of
    Just (L1 x) -> addValueG x
    Just (R1 y) -> addValueG y
    Nothing     -> id
  hasLUTG = hasLUTG @(a k) || hasLUTG @(b k)

  widthG = undefined

  translateWithG r xy = case safeWHNF xy of
    Just (L1 x) -> translateWithG r x
    Just (R1 y) -> translateWithG r y
    Nothing     -> []
  translateFieldsG = undefined

-- struct constructor
instance (WaveformG (fields k), KnownSymbol name)
  => WaveformG (C1 (MetaCons name fix True) fields k) where
  translatorG _ sty = t'
    where
      t' = case L.head sty of
        WSDefault ->
          if L.length subs == 1 then
            tStyled (WSInherit 0) t
          else t
        s -> tStyled s t
      subs = fieldTranslatorsG @(C1 (MetaCons name fix True) fields k)
      t = Translator (widthG @(fields k)) $ TProduct
        { start  = sym @name <> "{"
        , sep    = ", "
        , stop   = "}"
        , preci  = -1
        , preco  = 11
        , labels = L.map ((<>" = ") . fst) subs
        , subs   = subs
        }

  constrTranslatorsG sty =
    [ tDup (sym @name)
      $ translatorG @(C1 (MetaCons name fix True) fields k) undefined sty ]
  fieldTranslatorsG = fieldTranslatorsG @(fields k)

  addTypesG = addTypesG @(fields k)
  addValueG x = addValueG (unM1 x)
  hasLUTG = hasLUTG @(fields k)

  widthG = undefined

  translateWithG r x = [(sym @name, Translation r $ translateFieldsG x)]
  translateFieldsG x = translateFieldsG (unM1 x)

-- applicative product
instance (WaveformG (fields k), KnownSymbol name, PrecF fix)
  => WaveformG (C1 (MetaCons name fix False) fields k) where
  translatorG _ sty = t'
    where
      t' = case L.head sty of
        WSDefault ->
          if L.length subs == 1 then
            tStyled (WSInherit 0) t
          else t
        s -> tStyled s t
      subs = fieldTranslatorsG @(C1 (MetaCons name fix False) fields k)
      t = if isOperator then
            Translator (widthG @(fields k)) $ TProduct
              { start  = ""
              , sep    = " " <> sym @name <> " "
              , stop   = ""
              , preci  = precF @fix
              , preco  = precF @fix
              , labels = []
              , subs = subs
              }
          else
            Translator (widthG @(fields k)) $ TProduct
              { start  = case subs of
                          [] -> sname
                          _  -> sname <> " "
              , sep    = " "
              , stop   = ""
              , preci  = 10
              , preco  = case subs of
                          [] -> 11
                          _  -> 10
              , labels = []
              , subs = subs
              }

      sname = safeName (sym @name)
      isOperator = not (isAlpha . L.head $ sym @name) && (L.length subs == 2)

  constrTranslatorsG sty =
    [ tDup (sym @name)
      $ translatorG @(C1 (MetaCons name fix False) fields k) undefined sty ]
  fieldTranslatorsG = enumLabel $ fieldTranslatorsG @(fields k)

  addTypesG = addTypesG @(fields k)
  addValueG x = addValueG (unM1 x)
  hasLUTG = hasLUTG @(fields k)

  widthG = undefined

  translateWithG r x = [(sym @name, Translation r $ translateFieldsG x)]
  translateFieldsG x = enumLabel $ translateFieldsG (unM1 x)



-- no fields
instance WaveformG (U1 k) where
  translatorG = undefined
  constrTranslatorsG = undefined
  fieldTranslatorsG = []

  addTypesG = id
  addValueG _ = id
  hasLUTG = False

  widthG = 0

  translateWithG _ _ = undefined
  translateFieldsG _ = []


-- | Lazily get left field.
left :: (a :*: b) k -> a k
left (x :*: _y) = x

-- | Lazily get right field.
right :: (a :*: b) k -> b k
right (_x :*: y) = y

-- multiple fields
instance (WaveformG (a k), WaveformG (b k)) => WaveformG ((a :*: b) k) where
  translatorG = undefined
  constrTranslatorsG = undefined
  fieldTranslatorsG = fieldTranslatorsG @(a k) <> fieldTranslatorsG @(b k)

  addTypesG = addTypesG @(a k) . addTypesG @(b k)
  addValueG xy = addValueG (left xy) . addValueG (right xy)
  hasLUTG = hasLUTG @(a k) || hasLUTG @(b k)

  widthG = widthG @(a k) + widthG @(b k)

  translateWithG _ _ = undefined
  translateFieldsG xy = translateFieldsG (left xy) <> translateFieldsG (right xy)

-- struct field
instance (Waveform t, KnownSymbol name)
  => WaveformG (S1 (MetaSel (Just name) p q r) (Rec0 t) k) where
  translatorG = undefined
  constrTranslatorsG = undefined
  fieldTranslatorsG = [(sym @name, tRef (Proxy @t))]

  addTypesG = addTypes @t
  addValueG x = addValue $ unK1 . unM1 $ x
  hasLUTG = hasLUT @t

  widthG = width @t

  translateWithG _ _ = undefined
  translateFieldsG x = [(sym @name,translate $ unK1 $ unM1 x)]


-- unnamed field
instance (Waveform t) => WaveformG (S1 (MetaSel Nothing p q r) (Rec0 t) k) where
  translatorG = undefined
  constrTranslatorsG = undefined
  fieldTranslatorsG = [("", tRef (Proxy @t))]

  addTypesG = addTypes @t
  addValueG x = addValue $ unK1 . unM1 $ x
  hasLUTG = hasLUT @t

  widthG = width @t

  translateWithG _ _ = undefined
  translateFieldsG x = [("",translate $ unK1 $ unM1 x)]










------------------------------------------------ LUTS ------------------------------------

{-|
Class for easily defining custom translations for a type by using LUTs.
To use this class, a type must derive 'Waveform' via 'WaveformForLUT'.

Bye default, the implementation uses 'GHC.Generics.Generic' for defining subsignals
and operator precedence, and 'Show' for displaying the value.
-}
class (Typeable a, BitPack a) => WaveformLUT a where
  -- | Provides the hierarchy of subsignals.
  structureL :: Structure
  default structureL :: (WaveformG (Rep a ())) => Structure
  structureL = structure $ translatorG @(Rep a ()) 0 (L.repeat WSDefault)

  translateL :: a -> Translation
  default translateL :: (Generic a, Show a, WaveformG (Rep a ()), PrecG (Rep a ())) => a -> Translation
  translateL = displaySplit displayShow splitG

displaySplit :: (a -> Render) -> (Render -> a -> [(SubSignal,Translation)]) -> a -> Translation
displaySplit d s x = Translation ren subs
      ren = safeValOr (renError "undefined") $ d x
      subs = safeValOr [] $
             s ren x

-- | Display a value with 'Show', the default wave style, and operator precedence determined using 'Generic'.
displayShow :: (Show a, Generic a, PrecG (Rep a ())) => a -> Render
displayShow = displayWith show (const WSDefault) precL

-- | Display a value with the provided functions for creating the text value, style and operator precedence.
displayWith :: (a -> Value) -> (a -> WaveStyle) -> (a -> Prec) -> a -> Render
displayWith v s p x = Just (v x, s x, p x)

-- | Display an atomic value (such as a number) using the provided function to obtain the value.BinSpacer
-- (normal wavestyle, precedence 11).
displayAtomWith :: (a -> Value) -> a -> Translation
displayAtomWith f = displaySplit (displayWith f (const WSDefault) (const 11)) noSplit

-- | Display an atomic value (like a number) with 'Show'. See 'displayAtomWith'.
displayAtomShow :: (Show a) => a -> Translation
displayAtomShow = displayAtomWith show

displayAtomSigWith :: (Show a) => (a -> Value) -> a -> Translation
displayAtomSigWith f = displaySplit go noSplit
  where
    go x = Just (v,WSDefault,p)
      where
        v = f x
        p = case v of
          '-':_ -> 0
          _     -> 11 


displayAtomSigShow :: (Show a) => a -> Translation
displayAtomSigShow = displayAtomSigWith show


-- | Create subsignals for the constructors and fields.
-- Constructor translations are a copy of the toplevel render value provided.
splitG :: (Generic a, WaveformG (Rep a ())) => Render -> a -> [(SubSignal, Translation)]
splitG r x = translateWithG r (from @_ @() x)

-- | Create no subsignals for this type.
noSplit :: Render -> a -> [(SubSignal,Translation)]
noSplit _r _x = []

-- | Get the operator precedence of a value.
precL :: (PrecG (Rep a ()), Generic a) => a -> Prec
precL x = precG (from @_ @() x)

-- | Type for deriving 'Waveform' for types implementing 'WaveformLUT'.
--
-- @
-- type T = ... deriving (...)
-- deriving via WaveformForLUT T instance Waveform T
--
-- isntance WaveformLUT T where
--   ...
-- @
newtype WaveformForLUT a = WfLUT a deriving (Generic,BitPack,Typeable)

instance (WaveformLUT a, BitPack a, Typeable a)
  => Waveform (WaveformForLUT a) where
  typeName = typeNameP (Proxy @a)

  translator =
      Translator (width @(WaveformForLUT a))
    $ TLut (typeNameP (Proxy @a)) (structureL @a)
  translateBin bin = translateL @a (BL.binUnpack bin)

  addSubtypes = id

  addValue x m = M.insert ty
    (insertIfMissing bin trans $ M.findWithDefault M.empty ty m)
    m
   where
    ty = typeNameP (Proxy @a)
    bin = BL.binPack x
    trans = translateBin @(WaveformForLUT a) bin

  hasLUT = True








----------------------------------------------- PREC ----------------------------------

-- Stuff for figuring out the operator precedence of a type.

-- | Helper class for determining the precedence and number of fields of a
-- value's constructor.
class (Generic a) => PrecG a where
  -- | Operator precedence of a value.
  precG :: a -> Prec

  -- | Return the number of fields of a constructor.
  nFields :: Integer
  nFields = undefined

-- get constructor(s)
instance PrecG (c k) => PrecG (D1 m1 c k) where
  precG M1{unM1=x} = precG x

-- no constructors (void tpye)
instance PrecG (V1 k) where
  precG _ = 11

-- multiple constructors
instance (PrecG (a k), PrecG (b k)) => PrecG ((a :+: b) k) where
  precG (L1 x) = precG x
  precG (R1 y) = precG y

-- struct
instance (PrecG (fields k), PrecF fix)
  => PrecG (C1 (MetaCons name fix True) fields k) where
  precG _ = 11

-- applicative
instance (PrecG (fields k), PrecF fix)
  => PrecG (C1 (MetaCons name fix False) fields k) where
  precG _ = if nFields @(fields k) == 0 then 11 else precF @fix


-- count fields
instance PrecG (U1 k) where
  precG = undefined
  nFields = 0

instance (PrecG (a k), PrecG (b k)) => PrecG ((a :*: b) k) where
  precG = undefined
  nFields = nFields @(a k) + nFields @(b k)

instance PrecG (S1 (MetaSel n p q r) t k) where
  precG = undefined
  nFields = 1


-- | Class for obtaining the runtime precedence of a typelevel fixity value.
class PrecF (f::FixityI) where
  -- | Return the precedence of a fixity value as an 'Integer'.
  precF :: Prec
instance PrecF PrefixI where
  precF = 10
instance (KnownNat p) => PrecF (InfixI a p) where
  precF = natVal (Proxy @p)









---------------------------------------- OTHER VARIANTS ----------------------------------

-- CONST

-- | Helper class for defining a constant translation value. To use this,
-- derive Waveform via WaveformForConst.
class (BitPack a, Typeable a) => WaveformConst a where
  -- | The constant translation value.
  constTrans :: Translation
  constTrans = Translation (constRen @a) []
  -- | Constant render value. Overwrite this if the constant value has no
  -- subsignals.
  constRen :: Render
  constRen = undefined
  {-# MINIMAL constTrans | constRen #-}

-- | Helper class for deriving 'Waveform' for types implementing 'WaveformConst'.
newtype WaveformForConst a = WfConst a deriving (Generic,BitPack,Typeable)

instance (WaveformConst a, BitPack a, Typeable a)
  => Waveform (WaveformForConst a) where
  typeName = typeNameP (Proxy @a)
  translator = Translator 0 $ TConst $ constTrans @a
  addSubtypes = id
  addValue _ = id
  hasLUT = False




-- NUMBERS

-- | Helper class for deriving 'Waveform' for numerical types.
-- Options are provided at the type level (signed, format).
--
-- @
-- deriving via WaveformForNumber NFSig (Signed 3) instance Waveform (Signed 3)
-- @
newtype WaveformForNumber (f::NumberFormat) (s::Maybe NSPair) a
  = WfNum a
  deriving (Generic,BitPack,Typeable)

type NSPair = (Nat,Symbol)

instance (
    BitPack a
  , Typeable a
  , Typeable f
  , Typeable s
  , KnownNFormat f
  , KnownNSpacer s
  , Integral a
  ) => Waveform (WaveformForNumber (f::NumberFormat) (s::Maybe NSPair) a) where
  typeName = typeNameP (Proxy @a)
  translator =
      Translator (width @(WaveformForNumber f s a))
    $ TNumber{format = formatVal (Proxy @f), spacer = spacerVal (Proxy @s)}
  addSubtypes = id
  addValue _ = id
  hasLUT = False


-- | Class for turning a type level 'NumberFormat' into a runtime value.
class KnownNFormat (f::NumberFormat) where
  formatVal :: forall proxy. proxy f -> NumberFormat
instance KnownNFormat NFSig where
  formatVal _ = NFSig
instance KnownNFormat NFUns where
  formatVal _ = NFUns
instance KnownNFormat NFHex where
  formatVal _ = NFHex
instance KnownNFormat NFOct where
  formatVal _ = NFOct
instance KnownNFormat NFBin where
  formatVal _ = NFBin


class KnownNSpacer (f :: Maybe NSPair) where
  spacerVal :: proxy f -> Maybe (Integer, String)
instance KnownNSpacer 'Nothing where
  spacerVal _ = Nothing
instance (KnownNat n, KnownSymbol s) => KnownNSpacer ('Just '(n, s)) where
  spacerVal _ = Just (natVal (Proxy @n), sym @s)






--------------------------------------- IMPLEMENTATIONS ----------------------------------

-- TUPLES
{-
# python script for generating tuple instances

for i in range(2,12):
	v = [f"a{j}" for j in range(i)]
	c = ",".join("Waveform "+k for k in v)
	vs = ",".join(v)
	print(f"""instance ({c}) => Waveform ({vs}) where
  translator = Translator (width @({vs})) $ TProduct{{start="(",sep=",",stop=")",labels=[],preci= -1,preco=11,subs=enumLabel $ fieldTranslatorsG @(Rep ({vs}) ())}}
""")
-}

instance (Waveform a0,Waveform a1) => Waveform (a0,a1) where
  translator = Translator (width @(a0,a1)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci= -1,preco=11,subs=enumLabel $ fieldTranslatorsG @(Rep (a0,a1) ())}

instance (Waveform a0,Waveform a1,Waveform a2) => Waveform (a0,a1,a2) where
  translator = Translator (width @(a0,a1,a2)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci= -1,preco=11,subs=enumLabel $ fieldTranslatorsG @(Rep (a0,a1,a2) ())}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3) => Waveform (a0,a1,a2,a3) where
  translator = Translator (width @(a0,a1,a2,a3)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci= -1,preco=11,subs=enumLabel $ fieldTranslatorsG @(Rep (a0,a1,a2,a3) ())}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4) => Waveform (a0,a1,a2,a3,a4) where
  translator = Translator (width @(a0,a1,a2,a3,a4)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci= -1,preco=11,subs=enumLabel $ fieldTranslatorsG @(Rep (a0,a1,a2,a3,a4) ())}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5) => Waveform (a0,a1,a2,a3,a4,a5) where
  translator = Translator (width @(a0,a1,a2,a3,a4,a5)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci= -1,preco=11,subs=enumLabel $ fieldTranslatorsG @(Rep (a0,a1,a2,a3,a4,a5) ())}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6) => Waveform (a0,a1,a2,a3,a4,a5,a6) where
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci= -1,preco=11,subs=enumLabel $ fieldTranslatorsG @(Rep (a0,a1,a2,a3,a4,a5,a6) ())}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6,Waveform a7) => Waveform (a0,a1,a2,a3,a4,a5,a6,a7) where
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6,a7)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci= -1,preco=11,subs=enumLabel $ fieldTranslatorsG @(Rep (a0,a1,a2,a3,a4,a5,a6,a7) ())}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6,Waveform a7,Waveform a8) => Waveform (a0,a1,a2,a3,a4,a5,a6,a7,a8) where
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6,a7,a8)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci= -1,preco=11,subs=enumLabel $ fieldTranslatorsG @(Rep (a0,a1,a2,a3,a4,a5,a6,a7,a8) ())}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6,Waveform a7,Waveform a8,Waveform a9) => Waveform (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) where
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci= -1,preco=11,subs=enumLabel $ fieldTranslatorsG @(Rep (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) ())}

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6,Waveform a7,Waveform a8,Waveform a9,Waveform a10) => Waveform (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci= -1,preco=11,subs=enumLabel $ fieldTranslatorsG @(Rep (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) ())}


-- INSTANCES FOR OTHER STANDARD HASKELL TYPES

instance WaveformConst () where
  constRen = rFromVal "()"
deriving via WaveformForConst () instance Waveform ()

instance Waveform Bool where
  translator = Translator 1 $ TSum
    [ tConst $ Just ("False","$bool_false",11)
    , tConst $ Just ("True" ,"$bool_true" ,11) ]

instance (Waveform a) => Waveform (Maybe a) where
  translator = Translator (width @(Maybe a)) $ TSum
    [ Translator 0 $ TConst $ Translation (Just ("Nothing","$maybe_nothing",11)) []
    , tStyled (WSVar "$maybe_just" (WSInherit 0)) $ Translator (width @a) $ TProduct
      { start = "Just "
      , sep = ""
      , stop = ""
      , labels = []
      , preci = 10
      , preco = 10
      , subs = [("Just.0",tRef (Proxy @a))]
      }
    ]


instance (Waveform a, Waveform b) => Waveform (Either a b) where
  styles = ["$either_left","$either_right"]

instance (BitPack Char) => WaveformLUT Char where
  structureL = Structure []
  translateL = displayAtomShow
deriving via WaveformForLUT Char instance (BitPack Char) => Waveform Char

instance WaveformLUT Bit where
  structureL = Structure []
  translateL = displayAtomShow
deriving via WaveformForLUT Bit instance Waveform Bit

instance WaveformLUT Double where
  structureL = Structure []
  translateL = displayAtomSigShow
deriving via WaveformForLUT Double instance Waveform Double

instance WaveformLUT Float where
  structureL = Structure []
  translateL = displayAtomSigShow
deriving via WaveformForLUT Float instance Waveform Float

type DecSpacer = 'Just '(3,"_")
type HexSpacer = 'Just '(4,"_")
type OctSpacer = 'Just '(4,"_")
type BinSpacer = 'Just '(4,"_")
type NoSpacer = 'Nothing :: (Maybe NSPair)

deriving via WaveformForNumber NFSig DecSpacer Int instance Waveform Int
deriving via WaveformForNumber NFSig DecSpacer Int8 instance Waveform Int8
deriving via WaveformForNumber NFSig DecSpacer Int16 instance Waveform Int16
deriving via WaveformForNumber NFSig DecSpacer Int32 instance Waveform Int32
deriving via WaveformForNumber NFSig DecSpacer Int64 instance Waveform Int64

instance Waveform Ordering

deriving via WaveformForNumber NFUns DecSpacer Word instance Waveform Word
deriving via WaveformForNumber NFUns DecSpacer Word8 instance Waveform Word8
deriving via WaveformForNumber NFUns DecSpacer Word16 instance Waveform Word16
deriving via WaveformForNumber NFUns DecSpacer Word32 instance Waveform Word32
deriving via WaveformForNumber NFUns DecSpacer Word64 instance Waveform Word64

-- instance Display CUShort where
-- deriving via NoSplit CUShort instance Split CUShort

-- instance Display Half where
-- deriving via NoSplit Half instance Split Half

deriving via WaveformForNumber NFSig DecSpacer (Signed n)
  instance (KnownNat n) => Waveform (Signed n)
deriving via WaveformForNumber NFUns DecSpacer (Unsigned n)
  instance (KnownNat n) =>  Waveform (Unsigned n)
deriving via WaveformForNumber NFUns DecSpacer (Index n)
  instance (1 <= n, KnownNat n) => Waveform (Index n)

instance Waveform a => Waveform (Complex a)

instance Waveform a => Waveform (Down a)

instance Waveform a => Waveform (Identity a)



-- number wrappers
instance (Waveform a) => Waveform (Zeroing a) where
  translator = tDup "zeroing" $ tRef (Proxy @a)
  addSubtypes = addTypes @a
  addValue z = if hasLUT @a then addValue $ fromZeroing z
               else id
  hasLUT = hasLUT @a

instance (Waveform a) => Waveform (Wrapping a) where
  translator = tDup "wrapping" $ tRef (Proxy @a)
  addSubtypes = addTypes @a
  addValue z = if hasLUT @a then addValue $ fromWrapping z
               else id
  hasLUT = hasLUT @a

instance (Waveform a) => Waveform (Saturating a) where
  translator = tDup "saturating" $ tRef (Proxy @a)
  addSubtypes = addTypes @a
  addValue z = if hasLUT @a then addValue $ fromSaturating z
               else id
  hasLUT = hasLUT @a

instance (Waveform a) => Waveform (Overflowing a) where
  translator = tDup "overflowing" $ tRef (Proxy @a)
  addSubtypes = addTypes @a
  addValue z = if hasLUT @a then addValue $ fromOverflowing z
               else id
  hasLUT = hasLUT @a

instance (Waveform a) => Waveform (Erroring a) where
  translator = tDup "erroring" $ tRef (Proxy @a)
  addSubtypes = addTypes @a
  addValue z = if hasLUT @a then addValue $ fromErroring z
               else id
  hasLUT = hasLUT @a


-- vectors
instance (KnownNat n, Waveform a) => Waveform (Vec n a) where
  translator = Translator (width @(Vec n a)) $ if natVal (Proxy @n) /= 0 then
    TArray
      { start = ""
      , sep = " :> "
      , stop = " :> Nil"
      , preci = 5
      , preco = 5
      , len = fromIntegral $ natVal (Proxy @n)
      , sub = tRef (Proxy @a)
      }
    else
      TConst $ tFromVal "Nil"

  addSubtypes = addTypes @a
  addValue v = if hasLUT @(Vec n a) then go $ Clash.Prelude.toList v else id
    where
      go l = case safeWHNF l of
        Just []     -> id
        Just (x:xs) -> addValue x . go xs
        Nothing     -> id
  hasLUT = hasLUT @a && (natVal (Proxy @n) /= 0)


deriving via WaveformForNumber NFBin BinSpacer (BitVector n)
  instance (KnownNat n) => Waveform (BitVector n)

-- fixed point
instance (BitPack (Fixed r i f), KnownNat i, KnownNat f, Show (Fixed r i f), Typeable r)
  => WaveformLUT (Fixed r i f) where
  structureL = Structure []
  translateL = displayAtomSigShow
deriving via WaveformForLUT (Fixed r i f)
  instance (BitPack (Fixed r i f), KnownNat i, KnownNat f, Show (Fixed r i f), Typeable r)
    => Waveform (Fixed r i f)

-- snat
instance (KnownNat n, BitPack (SNat n)) => WaveformConst (SNat n) where
  constRen = rFromVal $ show $ natVal $ Proxy @n
deriving via WaveformForConst (SNat n)
  instance (KnownNat n, BitPack (SNat n)) => Waveform (SNat n)


-- the monster that is RTree :/

-- | Helper family for implementing 'Waveform' for 'RTree'.
type family RTreeIsLeaf d where
  RTreeIsLeaf 0 = True
  RTreeIsLeaf d = False

instance (Waveform a, KnownNat d, WaveformRTree (RTreeIsLeaf d) d a)
  => Waveform (RTree d a) where
  translator = translatorRTree @(RTreeIsLeaf d) @d @a
  addSubtypes =
    if hasLUT @a then
      addSubtypesRTree @(RTreeIsLeaf d) @d @a
    else id
  addValue = addValueRTree @(RTreeIsLeaf d) @d @a
  hasLUT = hasLUT @a

-- | Helper class for implementing 'Waveform' for 'RTree'.
class WaveformRTree (isLeaf::Bool) d a where
  translatorRTree :: Translator
  addValueRTree :: RTree d a -> LUTMap -> LUTMap
  addSubtypesRTree :: TypeMap -> TypeMap

instance (Waveform a) => WaveformRTree True 0 a where
  translatorRTree = tRef (Proxy @a)
  addValueRTree t = case safeWHNF t of
    Just (RLeaf x) -> addValue x
    Nothing        -> id
    _              -> undefined
  addSubtypesRTree = addTypes @a

instance (Waveform (RTree d1 a), Waveform a, d ~ d1 + 1, KnownNat d, KnownNat d1)
  => WaveformRTree False d a where
  translatorRTree = Translator (bitsize $ Proxy @(RTree d a)) $ TProduct
      { start = "<"
      , sep = ","
      , stop = ">"
      , labels = []
      , preci = -1
      , preco = 11
      , subs = [("left",tsub),("right",tsub)]
      }
    where tsub = tRef (Proxy @(RTree d1 a))
  addValueRTree t = case safeWHNF t of
    Just (RBranch x y) -> addValue (x:: RTree d1 a) . addValue y
    Nothing            -> id
    _                  -> undefined
  addSubtypesRTree = addTypes @(RTree d1 a)
