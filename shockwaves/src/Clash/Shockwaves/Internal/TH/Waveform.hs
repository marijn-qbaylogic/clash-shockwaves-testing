module Clash.Shockwaves.Internal.TH.Waveform where

import Prelude

-- import           Data.List (foldl')
import Language.Haskell.TH

import GHC.Generics (Rep) -- Generic
-- import           Data.Typeable (Typeable)
-- import           Clash.Prelude (BitPack)

import Clash.Shockwaves.Internal.Types (Translator (..), TranslatorVariant (..))
import Control.Monad (replicateM)

deriveWaveformTuples :: Int -> Int -> DecsQ
deriveWaveformTuples minSize maxSize = do
  let waveform = ConT $ mkName "Waveform"
      -- waveformG = ConT $ mkName "WaveformG"

      -- bitPack = ConT ''BitPack
      -- typeable = ConT ''Typeable
      -- generic = ConT ''Generic
      rep = ConT ''Rep
      unit = ConT ''()

  allNames <- replicateM maxSize (newName "a")

  return $ flip map [minSize .. maxSize] $ \tupleNum ->
    let names = take tupleNum allNames
        vs = map VarT names
        tuple = foldl' AppT (TupleT tupleNum) vs
        -- tupleQ = pure tuple

        context =
          -- [ bitPack `AppT` tuple
          -- , typeable `AppT` tuple
          -- , generic `AppT` tuple
          -- , waveformG `AppT` (rep `AppT` tuple `AppT` unit)
          -- ] <>
          map (waveform `AppT`) vs
        instTy = AppT waveform tuple

        translatorE =
          ConE 'Translator
            `AppE` AppTypeE (VarE $ mkName "width") tuple
            `AppE` RecConE
              'TProduct
              [ (mkName "start", LitE $ StringL "(")
              , (mkName "sep", LitE $ StringL ",")
              , (mkName "stop", LitE $ StringL ")")
              , (mkName "labels", ConE '[])
              , (mkName "preci", AppE (VarE 'negate) $ LitE $ IntegerL 1)
              , (mkName "preco", LitE $ IntegerL 11)
              ,
                ( mkName "subs"
                , AppTypeE
                    (VarE $ mkName "fieldTranslatorsG")
                    $ AppT (AppT rep tuple) unit
                )
              ]

        translator =
          FunD
            (mkName "translator")
            [ Clause
                []
                (NormalB translatorE)
                []
            ]
    in  InstanceD Nothing context instTy [translator]
