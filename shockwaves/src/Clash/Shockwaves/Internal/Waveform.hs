{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Shockwaves.Internal.Waveform where
import Clash.Prelude
import GHC.Generics
import Data.Proxy
import Data.Typeable
import Clash.Shockwaves.Internal.Types
import Clash.Shockwaves.Internal.Binary
import Clash.Shockwaves.Internal.Translator
import Clash.Shockwaves.Internal.Util
import Data.Map as M
import qualified Data.List as L
import Data.Char (isAlpha)
import Data.Bifunctor (first)

-- for standard type instances
import Data.Int             (Int8,Int16,Int32,Int64)
import Data.Word            (Word8,Word16,Word32,Word64)

import Clash.Num.Zeroing    (Zeroing, fromZeroing)
import Clash.Num.Wrapping   (Wrapping, fromWrapping)
import Clash.Num.Saturating (Saturating, fromSaturating)
import Clash.Num.Overflowing(Overflowing, fromOverflowing)
import Clash.Num.Erroring   (Erroring, fromErroring)
import Data.Complex         (Complex)
import Data.Ord             (Down)
import Data.Functor.Identity(Identity)
import Control.DeepSeq (NFData)
import Numeric (showInt, showHex, showOct, showBin)

-- import Data.Maybe (fromMaybe)
-- import Clash.Sized.Internal.BitVector (xToBV)

-- | Get a 'Translation' from a 'Value' using 'WSNormal' and prescedence 11.
tFromVal :: Value -> Translation
tFromVal v = Translation (Just (v,WSNormal,11)) []


-- | Wrap a 'Translator' in a 'TStyled' translator using some style, unless the
-- provided style is 'WSNormal'.
wrapStyle :: WaveStyle -> Translator -> Translator
wrapStyle WSNormal t = t
wrapStyle s        t = tStyled s t

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
tRef (_::Proxy a) = Translator (width @a) $ TRef (typeName @a) (structure $ translator @a)


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

class (Typeable a) => Waveform a where
  -- | Provide the type name.
  -- Overriding this value is only really useful for derive via strategies.
  typeName :: TypeName
  typeName = typeNameP (Proxy @a)

  -- | The translator used for the data type. Must match the structure value.
  translator :: Translator
  default translator :: (WaveformG (Rep a ())) => Translator
  translator = translatorG @(Rep a ()) (width @a) (styles' @a)

  -- | Function to translate values. This function has builtin error handling,
  -- and should not be overridden.
  -- To change the translation, override 'translate'' instead.
  translate :: a -> Translation
  translate x = translateBinary (translator @a) (stringPack x)
    -- Left (Just e) -> transError ("{undefined: "<>e<>"}")

  -- | Translate a binary string.
  -- For LUTs, this involves translating the value back to a bitvector and back to the original type.
  -- For other types, this simply translates the value according to the translator.
  translateBin :: String -> Translation
  translateBin = ...
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
  styles' = styles @a <> L.repeat WSNormal









------------------------------------------- GENERIC -------------------------------------

-- | A class for obtaining the required behaviour of 'Waveform' through "GHC.Generics".
-- The exact details might change later; use at your own risk.
class WaveformG a where
  -- | Given a list of styles for the constructors, translate a value.
  translateG :: [WaveStyle] -> a -> Translation
  -- | Given a bitsize and list of styles for the constructors, provide a translator.
  translatorG :: Int -> [WaveStyle] -> Translator

  -- | Given one or more constructors and a list of styles, or one or more fields,
  -- give a list of translators.
  -- For single constructor types, immediately return a list of field translators.
  translatorsG :: [WaveStyle] -> [(SubSignal,Translator)]
  -- for :+:  and :*: but also used at a type level to get all field translators
  -- for defining tuples more easily

  -- Return a list of translators for constructors; defined for constructors, :+: and types with multiple constructors.
  constrTranslatorsG :: [WaveStyle] -> [(SubSignal,Translator)]
  -- Return a list of translators for fields; defined for fields, :*:, constructors, and types with a single constructor.
  -- Product types are numbered for types and constructors only.
  fieldTranslatorsG :: [(SubSignal,Translator)]


  -- | Given a toplevel render, create a full translation with constructor field translations.
  -- This is only used for LUTs. Defined for all types, :+: and constructors.
  addFieldTranslations :: Render -> a -> Translation

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



-- void type (assuming it has a custom bitpack implementation)
instance WaveformG (D1 m1 V1 k) where
  translateG _ _ = Translation Nothing []
  translatorG _ _ = Translator 0 $ TConst $ Translation Nothing []

  translateAllG = undefined
  translatorsG _ = []

  splitG _ _ = []

  addTypesG = id
  addValueG _ = id
  hasLUTG = False

  widthG = undefined

-- single constructor type
instance (WaveformG (C1 m2 s k), WaveformG (s k)) => WaveformG (D1 m1 (C1 m2 s) k) where
  translateG sty = translateG sty . unM1
  translatorG = translatorG @(C1 m2 s k)

  translateAllG = translateAllG . unM1
  -- these are used for tuple definitions,
  -- and generally making it easier to write 1-constructor Waveform implementations
  translatorsG = translatorsG @(s k)

  splitG _ = translateAllG . unM1

  addTypesG = addTypesG @(C1 m2 s k)
  addValueG x = addValueG @(C1 m2 s k) (unM1 x)
  hasLUTG = hasLUTG @(C1 m2 s k)

  widthG = undefined


-- multiple constructors type
instance WaveformG ((a :+: b) k) => WaveformG (D1 m1 (a :+: b) k) where
  translateG sty = translateAsSumG sty . unM1
  translatorG w sty = Translator w . TSum $ L.map snd $ translatorsG @((a :+: b) k) sty -- TODO: bug? shouldn't this include a tDup?

  translateAllG = undefined
  translatorsG = undefined

  splitG r = splitG r . unM1

  addTypesG = addTypesG @((a :+: b) k)
  addValueG x = addValueG (unM1 x)
  hasLUTG = hasLUTG @((a :+: b) k)

  widthG = undefined


-- leftright :: (a :+: b) k -> Either (a k) (b k)
-- leftright (L1 x) = Left x
-- leftright (R1 y) = Right y

-- multiple constructors
instance (WaveformG (a k), WaveformG (b k)) => WaveformG ((a :+: b) k) where
  translateG sty (L1 x) = translateG sty x
  translateG sty (R1 y) = translateG (L.drop (L.length $ translatorsG @(a k) sty) sty) y
  translateAsSumG sty (L1 x) = translateAsSumG sty x
  translateAsSumG sty (R1 y) = translateAsSumG
    (L.drop (L.length $ translatorsG @(a k) sty) sty) y

  translatorsG sty =
       translatorsG @(a k) sty
    <> translatorsG @(b k) (L.drop (L.length $ translatorsG @(a k) sty) sty)

  translatorG = undefined
  translateAllG = undefined

  splitG r (L1 x) = splitG r x
  splitG r (R1 y) = splitG r y

  addTypesG = addTypesG @(a k) . addTypesG @(b k)
  addValueG xy = case safeWHNF xy of
    Just (L1 x) -> addValueG x
    Just (R1 y) -> addValueG y
    Nothing     -> id
  hasLUTG = hasLUTG @(a k) || hasLUTG @(b k)

  widthG = undefined

-- struct constructor
instance (WaveformG (fields k), KnownSymbol name)
  => WaveformG (C1 (MetaCons name fix True) fields k) where
  translateG sty x = translateFromSubs
    (translatorG @(C1 (MetaCons name fix True) fields k) undefined sty)
    (translateAllG $ unM1 x)
  translateAsSumG sty x = Translation ren [(sym @name, t)] -- TODO: use function
    where t = translateG sty x
          Translation ren _ = t
  translatorG _ sty = t'
    where
      t' = wrapStyle (L.head sty) t
      subs = translatorsG @(fields k) sty
      t = Translator (widthG @(fields k)) $ TProduct
        { start  = sym @name <> "{"
        , sep    = ", "
        , stop   = "}"
        , preci  = 0
        , preco  = 11
        , labels = L.map ((<>" = ") . fst) subs
        , subs   = L.map (first Just) subs
        , style  = if L.length subs == 1 then 0 else -1
        }

  translateAllG x = translateAllG $ unM1 x
  translatorsG sty =
    [   (sym @name, tDup (sym @name)
      $ translatorG @(C1 (MetaCons name fix True) fields k) undefined sty)]

  splitG r x = [(sym @name, Translation r $ translateAllG $ unM1 x)] -- TODO: point to own translateAllG?

  addTypesG = addTypesG @(fields k)
  addValueG x = addValueG (unM1 x)
  hasLUTG = hasLUTG @(fields k)

  widthG = undefined


-- | Replace empty labels with numbers
enumLabel :: [(SubSignal,a)] -> [(SubSignal,a)]
enumLabel = L.zipWith (\i (_,t) -> (show i,t)) [(0::Integer)..]


-- applicative product
instance (WaveformG (fields k), KnownSymbol name, PrecF fix)
  => WaveformG (C1 (MetaCons name fix False) fields k) where
  translateG sty x = translateFromSubs --safe?
    (translatorG @(C1 (MetaCons name fix False) fields k) undefined sty)
    (enumLabel $ translateAllG $ unM1 x)
  translateAsSumG sty x = Translation ren [(sym @name, t)] -- TODO: use function
    where t = translateG sty x
          Translation ren _ = t

  translatorG _ sty = t'
    where
      t' = wrapStyle (L.head sty) t
      subs = enumLabel $ translatorsG @(fields k) sty
      t = if isOperator then
            Translator (widthG @(fields k)) $ TProduct
              { start  = ""
              , sep    = " " <> sym @name <> " "
              , stop   = ""
              , preci  = precF @fix
              , preco  = precF @fix
              , labels = []
              , subs = L.map (first Just) subs
              , style = -1
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
              , subs = L.map (first Just) subs
              , style = -1
              }

      sname = safeName (sym @name)
      isOperator = not (isAlpha . L.head $ sym @name) && (L.length subs == 2)

  translateAllG x = enumLabel $ translateAllG $ unM1 x
  translatorsG sty =
    [   (sym @name, tDup (sym @name)
      $ translatorG @(C1 (MetaCons name fix False) fields k) undefined sty)]

  splitG r x = [(sym @name, Translation r $ enumLabel $ translateAllG $ unM1 x)]

  addTypesG = addTypesG @(fields k)
  addValueG x = addValueG (unM1 x)
  hasLUTG = hasLUTG @(fields k)

  widthG = undefined



-- no fields
instance WaveformG (U1 k) where
  translateG = undefined
  translateAsSumG = undefined
  translateAllG _ = []

  translatorG = undefined
  translatorsG _ = []

  splitG = undefined

  addTypesG = id
  addValueG _ = id
  hasLUTG = False

  widthG = 0


-- | Lazily get left field.
left :: (a :*: b) k -> a k
left (x :*: _y) = x

-- | Lazily get right field.
right :: (a :*: b) k -> b k
right (_x :*: y) = y

-- multiple fields
instance (WaveformG (a k), WaveformG (b k)) => WaveformG ((a :*: b) k) where
  translateG = undefined
  translateAsSumG = undefined
  translateAllG xy = translateAllG (left xy) <> translateAllG (right xy)

  translatorG = undefined
  translatorsG sty = translatorsG @(a k) sty <> translatorsG @(b k) sty

  splitG = undefined

  addTypesG = addTypesG @(a k) . addTypesG @(b k)
  addValueG xy = addValueG (left xy) . addValueG (right xy)
  hasLUTG = hasLUTG @(a k) || hasLUTG @(b k)

  widthG = widthG @(a k) + widthG @(b k)

-- struct field
instance (Waveform t, KnownSymbol name)
  => WaveformG (S1 (MetaSel (Just name) p q r) (Rec0 t) k) where
  translateG = undefined
  translateAsSumG = undefined
  translateAllG x = [(sym @name, translate $ unK1 . unM1 $ x)]

  translatorG = undefined
  translatorsG _ = [(sym @name, tRef (Proxy @t))]

  splitG = undefined

  addTypesG = addTypes @t
  addValueG x = addValue $ unK1 . unM1 $ x
  hasLUTG = hasLUT @t

  widthG = width @t


-- unnamed field
instance (Waveform t) => WaveformG (S1 (MetaSel Nothing p q r) (Rec0 t) k) where
  translateG = undefined
  translateAsSumG = undefined
  translateAllG x = [("", translate $ unK1 . unM1 $ x)]

  translatorG = undefined
  translatorsG _ = [("", tRef (Proxy @t))]

  splitG = undefined

  addTypesG = addTypes @t
  addValueG x = addValue $ unK1 . unM1 $ x
  hasLUTG = hasLUT @t

  widthG = width @t










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
  structureL = structure $ translatorG @(Rep a ()) 0 (L.repeat WSNormal)

  -- | Translate a value. By default, this uses the 'displayL' and 'splitL' functions,
  -- and takes care of any error handling.
  translateL :: a -> Translation
  translateL x = Translation ren subs
    where
      ren = safeValOr (renError "undefined") $ displayL x
      subs = safeValOr [] $
             splitL ren x

  -- | Create subsignal translations of a value from that value and its toplevel render.
  --
  -- The toplevel render is provided so that if a value has multiple constructors, the
  -- render can be reused for the subsignal of the constructor.
  splitL :: Render -> a -> [(SubSignal,Translation)]
  default splitL
    :: (Generic a, WaveformG (Rep a ()))
    => Render -> a -> [(SubSignal,Translation)]
  splitL r x = splitG r (from x :: Rep a ())

  -- | Render a value. By default, this is split into the functions 'labelL',
  -- 'styleL' and 'precL'.
  displayL :: a -> Render
  displayL x = Just (labelL x, styleL x, precL x)

  -- | Provide the textual representation of a value.
  labelL :: a -> Value
  default labelL :: Show a => a -> Value
  labelL = show

  -- | Provide the operator precedence of a value. By default, this uses
  -- 'GHC.Generics.Generic'.
  precL :: a -> Prec
  default precL :: (Generic a, PrecG (Rep a ())) => a -> Prec
  precL x = precG (from x :: Rep a ())

  -- | Provide the waveform style of a value. Defaults to 'WSNormal'.
  styleL :: a -> WaveStyle
  styleL _ = WSNormal

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
  translate' (WfLUT x) = translateL x

  addSubtypes = id

  addValue x m = M.insert ty
    (insertIfMissing bin trans $ M.findWithDefault M.empty ty m)
    m
   where
    ty = typeNameP (Proxy @a)
    bin = binPack x
    trans = translate x

  hasLUT = True








----------------------------------------------- PREC ----------------------------------

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
  translate' _ = constTrans @a
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
  translate' (WfNum x) = Translation (Just (v',WSNormal,11)) []
    where
      v = (case formatVal $ Proxy @f of
            NFSig -> const $ show $ toInteger x
            NFUns -> showInt x
            NFHex -> showHex x
            NFOct -> showOct x
            NFBin -> showBin x) ""
      v' = applySpacer (spacerVal (Proxy @s)) v
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
  translator = Translator (width @({vs})) $ TProduct{{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,style= -1,subs=L.map (first Just) $ enumLabel $ translatorsG @(Rep ({vs}) ()) undefined}}
  translate' x = translateFromSubs (translator @({vs})) $ translateAllG (from x::Rep ({vs}) ())
""")
-}

instance (Waveform a0,Waveform a1) => Waveform (a0,a1) where
  translator = Translator (width @(a0,a1)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,style= -1,subs=L.map (first Just) $ enumLabel $ translatorsG @(Rep (a0,a1) ()) undefined}
  translate' x = translateFromSubs (translator @(a0,a1)) $ translateAllG (from x::Rep (a0,a1) ())

instance (Waveform a0,Waveform a1,Waveform a2) => Waveform (a0,a1,a2) where
  translator = Translator (width @(a0,a1,a2)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,style= -1,subs=L.map (first Just) $ enumLabel $ translatorsG @(Rep (a0,a1,a2) ()) undefined}
  translate' x = translateFromSubs (translator @(a0,a1,a2)) $ translateAllG (from x::Rep (a0,a1,a2) ())

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3) => Waveform (a0,a1,a2,a3) where
  translator = Translator (width @(a0,a1,a2,a3)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,style= -1,subs=L.map (first Just) $ enumLabel $ translatorsG @(Rep (a0,a1,a2,a3) ()) undefined}
  translate' x = translateFromSubs (translator @(a0,a1,a2,a3)) $ translateAllG (from x::Rep (a0,a1,a2,a3) ())

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4) => Waveform (a0,a1,a2,a3,a4) where
  translator = Translator (width @(a0,a1,a2,a3,a4)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,style= -1,subs=L.map (first Just) $ enumLabel $ translatorsG @(Rep (a0,a1,a2,a3,a4) ()) undefined}
  translate' x = translateFromSubs (translator @(a0,a1,a2,a3,a4)) $ translateAllG (from x::Rep (a0,a1,a2,a3,a4) ())

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5) => Waveform (a0,a1,a2,a3,a4,a5) where
  translator = Translator (width @(a0,a1,a2,a3,a4,a5)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,style= -1,subs=L.map (first Just) $ enumLabel $ translatorsG @(Rep (a0,a1,a2,a3,a4,a5) ()) undefined}
  translate' x = translateFromSubs (translator @(a0,a1,a2,a3,a4,a5)) $ translateAllG (from x::Rep (a0,a1,a2,a3,a4,a5) ())

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6) => Waveform (a0,a1,a2,a3,a4,a5,a6) where
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,style= -1,subs=L.map (first Just) $ enumLabel $ translatorsG @(Rep (a0,a1,a2,a3,a4,a5,a6) ()) undefined}
  translate' x = translateFromSubs (translator @(a0,a1,a2,a3,a4,a5,a6)) $ translateAllG (from x::Rep (a0,a1,a2,a3,a4,a5,a6) ())

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6,Waveform a7) => Waveform (a0,a1,a2,a3,a4,a5,a6,a7) where
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6,a7)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,style= -1,subs=L.map (first Just) $ enumLabel $ translatorsG @(Rep (a0,a1,a2,a3,a4,a5,a6,a7) ()) undefined}
  translate' x = translateFromSubs (translator @(a0,a1,a2,a3,a4,a5,a6,a7)) $ translateAllG (from x::Rep (a0,a1,a2,a3,a4,a5,a6,a7) ())

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6,Waveform a7,Waveform a8) => Waveform (a0,a1,a2,a3,a4,a5,a6,a7,a8) where
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6,a7,a8)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,style= -1,subs=L.map (first Just) $ enumLabel $ translatorsG @(Rep (a0,a1,a2,a3,a4,a5,a6,a7,a8) ()) undefined}
  translate' x = translateFromSubs (translator @(a0,a1,a2,a3,a4,a5,a6,a7,a8)) $ translateAllG (from x::Rep (a0,a1,a2,a3,a4,a5,a6,a7,a8) ())

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6,Waveform a7,Waveform a8,Waveform a9) => Waveform (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) where
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,style= -1,subs=L.map (first Just) $ enumLabel $ translatorsG @(Rep (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) ()) undefined}
  translate' x = translateFromSubs (translator @(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)) $ translateAllG (from x::Rep (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) ())

instance (Waveform a0,Waveform a1,Waveform a2,Waveform a3,Waveform a4,Waveform a5,Waveform a6,Waveform a7,Waveform a8,Waveform a9,Waveform a10) => Waveform (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
  translator = Translator (width @(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)) $ TProduct{start="(",sep=",",stop=")",labels=[],preci=0,preco=11,style= -1,subs=L.map (first Just) $ enumLabel $ translatorsG @(Rep (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) ()) undefined}
  translate' x = translateFromSubs (translator @(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)) $ translateAllG (from x::Rep (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) ())


-- INSTANCES FOR OTHER STANDARD HASKELL TYPES

instance WaveformConst () where
  constRen = Just ("()",WSNormal,11)
deriving via WaveformForConst () instance Waveform ()

instance Waveform Bool where
  translator = Translator 1 $ TSum
    [ Translator 0 $ TConst $ translate' False
    , Translator 0 $ TConst $ translate' True ]
  translate' False = Translation (Just ("False","$bool_false",11)) []
  translate' True  = Translation (Just ("True","$bool_true",11)) []

instance (Waveform a) => Waveform (Maybe a) where
  translator = Translator (width @(Maybe a)) $ TSum
    [ Translator 0 $ TConst $ Translation (Just ("Nothing","$maybe_nothing",11)) []
    , Translator (width @a) $ TProduct
      { start = "Just "
      , sep = ""
      , stop = ""
      , style = 0
      , labels = []
      , preci = 10
      , preco = 10
      , subs = [(Just "Just.0",tRef (Proxy @a))]
      }
    ]
  translate' Nothing = Translation (Just ("Nothing","$maybe_nothing",11)) []
  translate' (Just x) = translateFromSubs t [("Just.0",translate x)] --safe?
    where
      t = case translator @(Maybe a) of
        Translator _ (TSum [_,t']) -> t'
        _ -> undefined


instance (Waveform a, Waveform b) => Waveform (Either a b) where
  styles = ["$either_left","$either_right"]

instance (BitPack Char) => WaveformLUT Char where
  structureL = Structure []
  splitL _ _ = []
  precL _ = 11
deriving via WaveformForLUT Char instance (BitPack Char) => Waveform Char

instance WaveformLUT Bit where
  structureL = Structure []
  splitL _ _ = []
  precL _ = 11
deriving via WaveformForLUT Bit instance Waveform Bit

instance WaveformLUT Double where
  structureL = Structure []
  splitL _ _ = []
  precL _ = 11
deriving via WaveformForLUT Double instance Waveform Double

instance WaveformLUT Float where
  structureL = Structure []
  splitL _ _ = []
  precL _ = 11
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
  translator = Translator (width @a) $ TDuplicate "zeroing" $ tRef (Proxy @a)
  translate' z = Translation ren [("zeroing",t)]
    where
      t = translate (fromZeroing z)
      Translation ren _ = t
  addSubtypes = addTypes @a
  addValue z = if hasLUT @a then addValue $ fromZeroing z
               else id
  hasLUT = hasLUT @a

instance (Waveform a) => Waveform (Wrapping a) where
  translator = Translator (width @a) $ TDuplicate "wrapping" $ tRef (Proxy @a)
  translate' z = Translation ren [("wrapping",t)]
    where
      t = translate (fromWrapping z)
      Translation ren _ = t
  addSubtypes = addTypes @a
  addValue z = if hasLUT @a then addValue $ fromWrapping z
               else id
  hasLUT = hasLUT @a

instance (Waveform a) => Waveform (Saturating a) where
  translator = Translator (width @a) $ TDuplicate "saturating" $ tRef (Proxy @a)
  translate' z = Translation ren [("saturating",t)]
    where
      t = translate (fromSaturating z)
      Translation ren _ = t
  addSubtypes = addTypes @a
  addValue z = if hasLUT @a then addValue $ fromSaturating z
               else id
  hasLUT = hasLUT @a

instance (Waveform a) => Waveform (Overflowing a) where
  translator = Translator (width @a) $ TDuplicate "overflowing" $ tRef (Proxy @a)
  translate' z = Translation ren [("overflowing",t)]
    where
      t = translate (fromOverflowing z)
      Translation ren _ = t
  addSubtypes = addTypes @a
  addValue z = if hasLUT @a then addValue $ fromOverflowing z
               else id
  hasLUT = hasLUT @a

instance (Waveform a) => Waveform (Erroring a) where
  translator = Translator (width @a) $ TDuplicate "erroring" $ tRef (Proxy @a)
  translate' z = Translation ren [("erroring",t)]
    where
      t = translate (fromErroring z)
      Translation ren _ = t
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
      TConst $ Translation (Just ("Nil",WSNormal,11)) []
  translate' v = translateFromSubs (translator @(Vec n a)) $
    L.zipWith (\i x -> (show i,translate x)) [(0::Int)..] $ go v
    where
      go :: forall (k::Natural). KnownNat k => Vec k a -> [a]
      go (v'::Vec k a) = case isX v' of
        Right Nil -> []
        Right (x `Cons` v'') -> x : go v''
        Left _e -> L.replicate (fromIntegral . natVal $ Proxy @k) (undefined::a)

  addSubtypes = addTypes @a
  addValue v = L.foldl (.) id $ L.map addValue $ Clash.Prelude.toList v
  hasLUT = hasLUT @a


deriving via WaveformForNumber NFBin BinSpacer (BitVector n)
  instance (KnownNat n) => Waveform (BitVector n)

-- fixed point
instance (BitPack (Fixed r i f), KnownNat i, KnownNat f, Show (Fixed r i f), Typeable r)
  => WaveformLUT (Fixed r i f) where
  structureL = Structure []
  splitL _ _ = []
  precL _ = 11
deriving via WaveformForLUT (Fixed r i f)
  instance (BitPack (Fixed r i f), KnownNat i, KnownNat f, Show (Fixed r i f), Typeable r)
    => Waveform (Fixed r i f)

-- snat
instance (KnownNat n, BitPack (SNat n)) => WaveformConst (SNat n) where
  constRen = Just (show $ natVal $ Proxy @n, WSNormal, 11)
deriving via WaveformForConst (SNat n)
  instance (KnownNat n, BitPack (SNat n)) => Waveform (SNat n)


-- the monster that is RTree :/

-- | Helper family for implementing 'Waveform' for 'RTree'.
type family RTreeIsLeaf d where
  RTreeIsLeaf 0 = True
  RTreeIsLeaf d = False

instance (Waveform a, KnownNat d, WaveformRTree (RTreeIsLeaf d) d a)
  => Waveform (RTree d a) where
  translator = Translator (width @(RTree d a)) $ if natVal (Proxy @d) == 0 then
      TProduct
        { start = "BR "
        , sep = ""
        , stop = ""
        , labels = []
        , preci = 10
        , preco = 10
        , subs = [(Just "0",tRef (Proxy @a))]
        , style = -1
        }
    else
      TProduct
        { start = "<"
        , sep = ","
        , stop = ">"
        , labels = []
        , preci = 0
        , preco = 11
        , subs = [(Just "left",tsub),(Just "right",tsub)]
        , style = -1
        }
    where tsub = tRef (Proxy @a)
  translate' = translateRTree @(RTreeIsLeaf d) @d @a
  addSubtypes = addSubtypesRTree @(RTreeIsLeaf d) @d @a
  addValue = addValueRTree @(RTreeIsLeaf d) @d @a
  hasLUT = hasLUT @a

-- | Helper class for implementing 'Waveform' for 'RTree'.
class WaveformRTree (isLeaf::Bool) d a where
  addValueRTree :: RTree d a -> LUTMap -> LUTMap
  addSubtypesRTree :: TypeMap -> TypeMap
  translateRTree :: RTree d a -> Translation
instance (Waveform a) => WaveformRTree True 0 a where
  addValueRTree t = if hasLUT @a then
    case t of
      RLeaf x -> addValue x
      _ -> undefined
    else id
  addSubtypesRTree = addTypes @a
  translateRTree t = translateFromSubs (translator @(RTree 0 a)) subs --safe?
    where
      subs = case t of
        RLeaf x -> [("0",translate x)]
        _ -> undefined

instance (Waveform (RTree d1 a), Waveform a, d ~ d1 + 1, KnownNat d1)
  => WaveformRTree False d a where
  addValueRTree t = if hasLUT @a then
    case t of
      RBranch x y -> addValue (x:: RTree d1 a) . addValue y
      _ -> undefined
    else id
  addSubtypesRTree = addTypes @a
  translateRTree t = translateFromSubs (translator @(RTree 0 a)) subs
    where
      subs = case t of
        RBranch x y -> [("left",translate (x::RTree d1 a)),("right",translate y)]
        _ -> undefined
