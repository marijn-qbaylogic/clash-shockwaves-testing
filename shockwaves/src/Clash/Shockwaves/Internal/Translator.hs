{-|

Code for rendering values using the translators specified.
Values are constructed from their subvalues.

-}

module Clash.Shockwaves.Internal.Translator where

import Clash.Prelude hiding (sub)
import Clash.Shockwaves.Internal.Types
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first)
import Clash.Shockwaves.Internal.Util


-- | Apply a 'WaveStyle' to a 'Translation'
applyStyle :: WaveStyle -> Translation -> Translation
applyStyle s (Translation r sb) = Translation (applyStyleR s r) sb

-- | Apply a 'WaveStyle' to a 'Render' value
applyStyleR :: WaveStyle -> Render -> Render
applyStyleR s (Just (l,WSNormal,p)) = Just (l,s,p)
applyStyleR _ r = r

-- | Render some error message. The precedence is set to 11 (i.e. an atomic).
renError :: Value -> Render
renError v = Just (v, WSError, 11)

-- | Create a translation from an error message.
transError :: Value -> Translation
transError e = Translation (renError e) []

-- | Apply a precedence value to a 'Translation'.
-- If the precedence is higher or equal to that of the current value,
-- it is wrapped in parentheses.
applyPrec :: Prec -> Translation -> Translation
applyPrec p (Translation r s) = Translation (applyPrecR p r) s

-- | Apply a precedence value to a 'Render'.
-- If the precedence is higher or equal to that of the current value,
-- it is wrapped in parentheses.
applyPrecR :: Prec -> Render -> Render
applyPrecR p (Just (v,s,p')) = if p'>p then Just (v,s,p')
                               else Just (parenthesize v, s, 11)
applyPrecR _ Nothing = Nothing

-- | Apply a precedence value to a list of subsignal translations.
-- If the precedence is higher or equal to that of the current value,
-- it is wrapped in parentheses.
applyPrecL :: Prec -> [(a, Translation)] -> [(a, Translation)]
applyPrecL p = L.map (\(a,b) -> (a,applyPrec p b))


-- | Get the value of a 'Translation'. If the value is not defined,
-- return @{value missing}@.
getVal :: Translation -> Value
getVal t = case t of
              Translation (Just (v,_,_)) _ -> v
              _ -> "{Value missing}"


-- | Remove subsignal translators that do not have a subsignal name.
filterSignals :: [(SubSignal,Translation)] -> [(SubSignal,Translation)]
filterSignals = L.filter ((/="") . fst)



-- -- | Like 'translateFromSubs', but includes an error catching mechanism.
-- safeTranslateFromSubs :: Translator -> [(SubSignal,Translation)] -> Translation
-- safeTranslateFromSubs t subs = case safeVal subs of
--   Right subs' -> case safeVal (translateFromSubs t subs') of
--     Right t' -> t'
--     Left _e -> Translation (renError "{translation error}") $ filterSignals subs
--   Left _e -> transError "{undefined}"

-- | Complete a translation based on already translated subsignals.
--
-- The exact behaviour is non-trivial.
-- Translators that require special translation ('TRef','TLut','TNumber')
-- cannot be translated. If a single subsignal is provided with label `""`,
-- this translation is used as if it was the result of the translation.
-- Otherwise, an error is raised.
--
-- 'TSum', 'TProduct', 'TArray' and 'TConst' render a value as expected based on
-- the subtranslations. Note for TSum that this is a list containing only the
-- translation of the variant used, i.e. it behaves like 'TRef', 'TLut' and 'TNumber'.
--
-- The final two variants, 'TStyle' and 'TDuplicate' are considered *wrappers*
-- and translate the value recursively.
translateFromSubs :: Translator -> [(SubSignal,Translation)] -> Translation
translateFromSubs (Translator _ translator) subs = case translator of
  TRef _ _ -> case subs of
    [("",t)] -> t
    _ -> errorX "Ref should only appear as a nested type that is translated through split; for referenced types, modify Waveform.translate"
  TLut _ _ -> case subs of
    [("",t)] -> t
    _ -> errorX "LUT translators require a custom implementation of Waveform.translate that does not call render"
  TNumber{} -> case subs of
    [("",t)] -> t
    _ -> errorX "Number translators require a custom implementation of Waveform.translate that does not call render"

  -- normal
  TSum _ -> case subs of
    [(_,t)] -> t
    _ -> transError "{invalid variant}"
  TProduct
    { start, sep, stop
    , labels
    , preci, preco
    } -> Translation (Just (v,WSNormal,preco)) $ filterSignals subs
      where
        labels' = L.map Just labels <> L.repeat Nothing
        subs' = applyPrecL preci subs
        vals = L.map (getVal . snd) subs'
        v = start <> joinWith sep fields <> stop
        fields = L.zipWith addLabel labels' vals
        addLabel = \case
          Just l -> (l<>)
          Nothing -> id
  TConst t -> t
  TArray
    { len
    , start, sep, stop
    , preci, preco
    } -> Translation ren subs
      where
        ren = if L.length subs == len then
          Just
            (    start
              <> joinWith sep (L.map (getVal . applyPrec preci . snd) subs)
              <> stop
            , WSNormal, preco )
        else
          renError "{values missing}"

  -- recursive
  TStyled sty t -> applyStyle sty $ translateFromSubs t subs
  TDuplicate n t -> Translation ren [(n,t')]
    where t' = translateFromSubs t subs
          Translation ren _ = t'

-- | Return the 'Structure' implied by a 'Translator'. Useful for determining
-- the structure of a constant translation.
structure :: Translator -> Structure
structure (Translator _ t) = case t of
  TRef _ s -> s
  TSum ts -> Structure subs
    where subs = L.concatMap (getS . structure) ts
          getS (Structure s) = s
  TProduct{subs} -> Structure (mapMaybe go subs)
    where go (Nothing,_) = Nothing
          go (Just n, t') = Just (n, structure t')
  TConst _ -> Structure [] -- TODO: derive from value
  TLut _ s -> s
  TNumber{} -> Structure []
  TArray{sub,len} ->
      Structure . L.map (first show) . enumerate
    $ L.replicate len $ structure sub
   where enumerate = L.zip [(0::Int)..]
  TStyled _ t' -> structure t'
  TDuplicate n t' -> Structure [(n,structure t')]


translateBinWith :: Translator -> String -> Translation
translateBinWith trans@(Translator width variant) bin = case variant of
  TRef _ _ btf -> btf bin
  TLut _ _ -> errorX "Can't translate binary data with TLut."
  TNumber{format,sep} -> Translation (Just (val,WSNormal,11)) []
    where
      val = map (applySep sep) $ case format of
        NFBin -> Just $ bin
        NFOct -> Just $ map octDigit $ chunksOf 3 extend3
        NFHex -> Just $ map hexDigit $ chunksOf 4 extend4
        NFUns -> map show (decodeUns 0 bin)
        NFDec -> map show (decodeSig bin) --todo: decode, translate, format etc.
      extend3 = (replicate (2-(n+2)%3) '0') ++ bin
      extend4 = (replicate (3-(n+3)%4) '0') ++ bin
      n = length bin

  -- normal
  TSum subs -> translateFromSubs subTs
   where
    subTs = translateBinWith (subs ! v) b'' -- todo: take first clog len bits, switch based on that, then call translateFromSubs
    v = unsFromBinString b'
    (b',b'') = splitAt k bin
    k = clog $ length subs

  TProduct
    { subs
    , labels
    } -> translateFromSubs trans subTs
    where subTs = enumLabels $ zipWith (,) (labels++repeat "") $ carryFoldl go bin subs -- todo: create subtranslations first
          go b t@(Translator w _) = let (b',b'') = splitAt w b in (b'',translateBinWith t b')
  TConst t -> t

  TArray{ sub=sub@(Translator w _) }
    -> translateFromSubs trans subTs
    where subTs = enumLabel $ map (("",) . translateBinWith sub) $ chunksOf w bin-- todo: create subtranslations

  -- recursive
  TStyled sty t -> applyStyle sty $ translateBinWith t bin
  TDuplicate n t -> Translation ren [(n,t')]
    where t' = translateBinWith t bin
          Translation ren _ = t'

carryFoldl :: (a -> b -> (a,c)) -> a -> [b] -> [c]
carryFoldl f i [] = []
carryFoldl f i (x:xs) = y:carryFoldl f i' xs
  where (i,y) = f i x

applySep (0,_) num = num
applySep (_,"") num = num
applySep (k,s) num = sign ++ joinWith s parts
  where
    (sign,digits) = case num of
      sign@'-':digits -> (sign,digits)
      digits          -> (  "",digits)
    parts = reverse $ map reverse $ chunksOf k $ reverse digits

decodeUns :: Integer -> String -> Maybe Integer
decodeUns k "" = Just k;
decodeUns k ('0':r) = decodeUns (k*2) r
decodeUns k ('1':r) = decodeUns (k*2+1) r
decodeUns _ _ = Nothing

decodeSig :: String -> Maybe Integer
decodeSig "" = 0
decodeSig ('0':r) = decodeUns 0 r
decodeSig ('1':r) = decodeUns (-1) r
decodeSig _ = Nothing