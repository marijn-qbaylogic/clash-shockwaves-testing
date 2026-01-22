{-# LANGUAGE ScopedTypeVariables #-}

module Clash.Shockwaves.Internal.BitList where

import           Clash.Prelude hiding (take,split,concat,drop)
import           Clash.Sized.Internal.BitVector
import           Data.Aeson hiding (Value)
import           Data.String (IsString (fromString))

-- | Like BitVector, but with a dynamic size as to not restrict mixing them for different types
data BitList = BL
  { unsafeMask      :: !Natural
  , unsafeToNatural :: !Natural
  , bitLength       :: !Int
  } deriving (Eq,Ord)

instance Show BitList where
  show BL{unsafeMask,unsafeToNatural,bitLength} = go bitLength unsafeMask unsafeToNatural []
    where
      go 0 _ _ s = s
      go n m0 v0 s =
          let
          (!v1, !vBit) = quotRem v0 2
          (!m1, !mBit) = quotRem m0 2
          !renderedBit = showBit mBit vBit
          in
          go (n - 1) m1 v1 (renderedBit :       s)

      showBit 0 0 = '0'
      showBit 0 1 = '1'
      showBit _ _ = 'x'

bvToBl :: KnownNat n => BitVector n -> BitList
bvToBl (BV @n m i) = BL m i (natToNum @n)

blToBv :: forall n. KnownNat n => BitList -> BitVector n
blToBv (BL m i l) | natToNum @n == l = BV m i
blToBv _ = errorX "BitList does not match BitVector size" 

binPack :: (BitPack a) => a -> BitList
binPack = bvToBl . pack

binUnpack :: (BitPack a) => BitList -> a
binUnpack = unpack . blToBv


-- | Remove MSBs: `| n | l-n | -> | l-n |`
drop :: Int -> BitList -> BitList
drop x = snd . split x

-- | Take MSBs: `| n | l-n | -> | n |`
take :: Int -> BitList -> BitList
take n (BL m i l) | n > l = error ("Attempt to take "<>show n<>" from BitList of size "<>show l)
                  | otherwise = BL m' i' n
  where
    s = l - n
    m' = shiftR m s
    i' = shiftR i s

-- | Split into MSBs and LSBs: `| n | l-n | -> | n |, | l-n |`
split :: Int -> BitList -> (BitList,BitList)
split n bv@(BL mm ii l) = (a,b)
  where
    a@(BL m i _n) = take n bv
    m' = shiftL m (l-n)
    i' = shiftL i (l-n)
    b = BL (mm-m') (ii-i') (l-n)

concat :: BitList -> BitList -> BitList
concat (BL ma ia la) (BL mb ib lb) = BL m i l
  where m = (ma `shiftL` lb) .|. mb
        i = (ia `shiftL` lb) .|. ib
        l = la + lb

slice :: (Int,Int) -> BitList -> BitList
slice (from,to) = drop from . take to 


toInteger :: BitList -> Maybe Integer
toInteger (BL m i _) | m == 0 = Just $ fromIntegral i
toInteger _ = Nothing


instance Semigroup BitList where
  (<>) = concat



instance ToJSON BitList where
  toJSON = toJSON . show

instance ToJSONKey BitList where

instance IsString BitList where
  fromString ss = go ss (BL 0 0 0)
    where
      go  ""      bl        = bl
      go ('_':s)  bl        = go s bl
      go ('0':s) (BL m i l) = go s (BL (2*m  ) (2*i  ) (l+1))
      go ('1':s) (BL m i l) = go s (BL (2*m  ) (2*i+1) (l+1))
      go (_  :s) (BL m i l) = go s (BL (2*m+1) (2*i  ) (l+1))