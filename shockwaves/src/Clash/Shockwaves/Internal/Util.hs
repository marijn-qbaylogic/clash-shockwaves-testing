{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Some small helper functions.
-}


module Clash.Shockwaves.Internal.Util where

import Clash.Prelude
import Clash.Shockwaves.Internal.Types
import qualified Data.List as L
import Data.Aeson (encodeFile,ToJSON)
import Data.Char (isAlpha)
import Data.Proxy
import Data.Map (Map,member)
import qualified Data.Map as M
import Data.Typeable
import Data.List.Split (chunksOf)
import Control.Exception (SomeException, evaluate, catch)
import GHC.IO (unsafeDupablePerformIO)
import Control.DeepSeq (force, NFData)
import Control.Exception.Base (Exception(toException))



-- | Wrap parentheses around a value.
parenthesize :: Value -> Value
parenthesize n = "("<>n<>")"


-- | Returns the 'BitSize' of a type as a runtime 'Int'.
bitsize :: (BitPack a) => Proxy a -> Int
bitsize (_ :: Proxy a) = fromInteger $ natVal $ Proxy @(BitSize a)

-- | Re-export of 'Data.Aeson.encodeFile' for cleaner naming in tracing functions.
writeFileJSON :: forall a. ToJSON a => FilePath -> a -> IO ()
writeFileJSON = encodeFile

-- | Add parentheses around an identifier if it is an operator.
safeName :: Value -> Value
safeName n = if isAlpha $ L.head n then n else parenthesize n

-- | Insert value into dictionary if the key was not yet present.
insertIfMissing :: (Ord k) => k -> v -> Map k v -> Map k v
insertIfMissing k v m = if member k m then m else M.insert k v m

-- | Join a list of values with a separator. If the list is empty, an empty
-- value is returned.
joinWith :: Value -> [Value] -> Value
joinWith s (x:y:r) = x <> s <> joinWith s (y:r)
joinWith _ [x] = x
joinWith _ [] = ""

-- | Obtain the name of a type from a proxy value.
-- The name consists of a unique fingerprint (which is safe to use)
-- and a human readable representation of the type (which may not be unique
-- if multiple sources define the same types).
typeNameP :: Typeable a => Proxy a -> TypeName
typeNameP p = show (typeRepFingerprint r) <> ":" <> show r
  where r = typeRep p

-- | Class for improving runtime symbol syntax readability.
class (KnownSymbol s) => QuickSymbol s where
  -- | Shorter way of obtaining the runtime value of a type level string.
  sym :: String
  sym = symbolVal (Proxy @s)
instance (KnownSymbol s) => QuickSymbol s

-- | Check if a value is safe to use.
-- If not, optionally return an error message.
safeVal :: (NFData a) => a -> Either (Maybe Value) a
safeVal x = unsafeDupablePerformIO $ catch
  (   evaluate . force . unsafeDupablePerformIO
    $ catch (evaluate . force $ Right x)
            (\(e::SomeException) ->
              return $ Left (Just $ show $ toException e)))
  (\(XException e) -> return $ Left (Just e))

-- | Evaluate to WHNF. If this fails, return a default value.
safeWHNF :: a -> Maybe a
safeWHNF x = unsafeDupablePerformIO $ catch
  (   evaluate . unsafeDupablePerformIO
    $ catch (evaluate (x `seq` Just x))
            (\(_::SomeException) ->
              return Nothing))
  (\(XException _e) -> return Nothing)


applySpacer :: NumberSpacer -> Value -> Value
applySpacer Nothing v = v
applySpacer (Just (0,_)) v = v
applySpacer (Just (n,s)) v = v'
  where
    chunks = chunksOf (fromIntegral n) $ L.reverse v
    v' = L.reverse (if L.last chunks == "-" then
        joinWith (L.reverse s) (L.init chunks) <> "-"
      else
        joinWith (L.reverse s) chunks)