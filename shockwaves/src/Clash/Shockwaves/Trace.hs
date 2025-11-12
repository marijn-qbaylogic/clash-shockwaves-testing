-- adapted from commit e702e9a

{-|
Copyright  :  (C) 2018, Google Inc.
                  2019, Myrtle Software Ltd
                  2022-2024, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Utilities for tracing signals and dumping them in various ways. Example usage:

@
import Clash.Prelude hiding (writeFile)
import Data.Text.IO  (writeFile)
import Clash.Shockwaves
import qualified Clash.Shockwaves.Trace as T

-- | Count and wrap around
subCounter :: SystemClockResetEnable => Signal System (Index 3)
subCounter = T.traceSignal1 "sub" counter
  where
    counter =
      register 0 (fmap succ' counter)

    succ' c
      | c == maxBound = 0
      | otherwise     = c + 1

-- | Count, but only when my subcounter is wrapping around
mainCounter :: SystemClockResetEnable => Signal System (Signed 64)
mainCounter = T.traceSignal1 "main" counter
  where
    counter =
      register 0 (fmap succ' $ bundle (subCounter,counter))

    succ' (sc, c)
      | sc == maxBound = c + 1
      | otherwise      = c

-- | Collect traces, and dump them to a VCD file.
main :: IO ()
main = do
  let cntrOut = exposeClockResetEnable mainCounter systemClockGen systemResetGen enableGen
  vcd <- T.dumpVCD (0, 100) cntrOut ["main", "sub"]
  case vcd of
    Left msg ->
      error msg
    Right (contents,meta) -> do
      writeFile     "mainCounter.vcd"  contents
      writeFileJSON "mainCounter.json" meta
@
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Clash.Shockwaves.Trace
  (
  -- * Tracing functions
  -- ** Simple
    traceSignal1
  , traceVecSignal1
  -- ** Tracing in a multi-clock environment
  , traceSignal
  , traceVecSignal

  -- * VCD dump functions
  , dumpVCD
  , advancedDumpVCD
  , VCDTime
  , simCycles
  , clockCycles
  , timePs
  , exactTime
  , clockWave

  -- * Replay functions
  , dumpReplayable
  , replay

  -- * Internal
  -- ** Types
  , Period
  , Changed
  , Value
  , Width
  , Maps
  , TraceMap
  , TypeRepBS
  -- ** Functions
  , traceSignal#
  , traceVecSignal#
  , dumpVCD#
  , dumpVCD##
  , waitForTraces#
  , maps#
  ) where

import           Prelude

-- Clash:
import           Clash.Annotations.Primitive (hasBlackBox)
import           Clash.Signal.Internal (fromList,DomainPeriod)
import           Clash.Signal          (clockPeriod)
import           Clash.Signal
  (KnownDomain(..), SDomainConfiguration(..), Signal, bundle, unbundle)
import           Clash.Sized.Vector    (Vec, iterateI)
import qualified Clash.Sized.Vector    as Vector
import           Clash.Class.BitPack   (BitPack, BitSize, pack, unpack)
import           Clash.Promoted.Nat    (snatToNum, SNat(..))
import           Clash.Signal.Internal (Signal ((:-)), sample)
import           Clash.XException      (deepseqX, NFDataX)
import           Clash.Sized.Internal.BitVector
  (BitVector(BV))

-- Haskell / GHC:
import           Control.Monad         (foldM)
import           Data.Bits             (testBit)
import           Data.Binary           (encode, decodeOrFail)
import           Data.ByteString.Lazy  (ByteString)
import qualified Data.ByteString.Lazy  as ByteStringLazy
import           Data.Char             (ord, chr)
import           Data.IORef
  (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
#if !MIN_VERSION_base(4,20,0)
import           Data.List             (foldl')
#endif
import           Data.List             (foldl1', unzip5, transpose, uncons)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromMaybe, catMaybes)
import qualified Data.Text             as Text
import           Data.Time.Clock       (UTCTime, getCurrentTime)
import           Data.Time.Format      (formatTime, defaultTimeLocale)
import           GHC.Natural           (Natural)
import           GHC.Stack             (HasCallStack)
import           GHC.TypeLits          (KnownNat, type (+))
import           System.IO.Unsafe      (unsafePerformIO)
import           Type.Reflection       (Typeable, TypeRep, typeRep)
import qualified Data.Aeson as Json
import           Data.Aeson            ((.=))

import           GHC.Conc              (pseq)

-- Shockwaves
import           Clash.Shockwaves.Internal.Types hiding (Value)
import           Clash.Shockwaves.Internal.Waveform hiding (width)

#ifdef CABAL
import qualified Data.Version
import qualified Paths_shockwaves

import           System.FilePath
import           System.IO.Unsafe
import           Clash.Annotations.Primitive

{-# ANN module
  ( Primitive [minBound..] 
    (unsafePerformIO Paths_shockwaves.getDataDir </> "prims" </> "common")
  ) #-}
#endif

type Period   = Int
type Changed  = Bool
type Value    = (Natural, Natural) -- (Mask, Value)
type Width    = Int

-- | Serialized TypeRep we need to store for dumpReplayable / replay
type TypeRepBS = ByteString

type AddValue = LUTMap -> LUTMap
type TraceMap = Map.Map String (TypeRepBS, Period, Width, Maybe [AddValue], [Value])
type Maps     = (SignalMap,TypeMap,TraceMap)

type JSON = Json.Value

-- | A type for keeping track of time values for use with advancedDumpVCD
data VCDTime = VCDTime
  { vcdTimePs :: Period -- ^ Time in ps
  , vcdTimeTs :: Period -- ^ Time in signal timescale units
  , vcdTimeStrict :: Bool -- ^ Whether to force exact times
  }

instance Num VCDTime where
  (VCDTime p t s) + (VCDTime p' t' s') = VCDTime (p+p') (t+t') (s && s')
  negate (VCDTime p t s) = VCDTime (-p) (-t) s
  fromInteger n = VCDTime 0 (fromInteger n) False
  (*) = error "Cannot multiply time values"
  abs = error "Cannot take absolute value of time"
  signum = error "Signum unavailable for time values"

-- | A time unit in multiples of the GCD of the periods of all traced signals.
-- This is the default unit (i.e. number literals will be transformed using this
-- function), but may not be practical in designs with multiple clocks.
simCycles :: Int -> VCDTime
simCycles n = VCDTime 0 n False

-- | A time unit in multiples of a clock period in the provided domain.
clockCycles :: forall dom period. (KnownDomain dom, DomainPeriod dom ~ period) => Int -> VCDTime
clockCycles n = VCDTime 0 (n * clockPeriod' @dom) False

-- | A time unit in ps.
timePs :: Int -> VCDTime
timePs n = VCDTime n 0 False

-- | Make a time moment exact. By default, times are rounded down to the nearest
-- time value that is a multiple of the time scale used. By making the time
-- exact, the time value will be considered into the computation of the time scale.
exactTime :: VCDTime -> VCDTime
exactTime t = t{vcdTimeStrict=True}


-- | Create a named clock signal for a domain.
clockWave :: forall dom. KnownDomain dom => String -> (String,Period)
clockWave label = (label, clockPeriod' @dom)

-- | Get the clock period of a domain as a 'Period' value.
clockPeriod' :: forall dom. KnownDomain dom => Period
clockPeriod' = snatToNum $ clockPeriod @dom


-- | Map of traces used by the non-internal trace and dumpvcd functions.
maps# :: IORef Maps
maps# = unsafePerformIO $ newIORef (Map.empty,Map.empty,Map.empty)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceMap# #-}

mkTrace
  :: HasCallStack
  => BitPack a
  => NFDataX a
  => Signal dom a
  -> [Value]
mkTrace signal = sample (unsafeToTup . pack <$> signal)
 where
  unsafeToTup (BV mask value) = (mask, value)

-- | Trace a single signal. Will emit an error if a signal with the same name
-- was previously registered.
traceSignal#
  :: forall dom a
   . ( BitPack a
     , NFDataX a
     , Typeable a
     , Waveform a )
  => IORef Maps
  -- ^ Map to store the trace
  -> Int
  -- ^ The associated clock period for the trace
  -> String
  -- ^ Name of signal in the VCD output
  -> Signal dom a
  -- ^ Signal to trace
  -> IO (Signal dom a)
traceSignal# maps period traceName signal =
  atomicModifyIORef' maps $ \(signals,types,traces) ->
    if Map.member traceName traces then
      error $ "Already tracing a signal with the name: '" ++ traceName ++ "'."
    else
      ( ( Map.insert ("logic." <> traceName) (typeName @a) signals
        , addTypes @a types
        , Map.insert
            traceName
            ( encode (typeRep @a)
            , period * 1000 -- convert to fs
            , width
            , if hasLUT @a then Just $ map addValue $ sample signal else Nothing
            , mkTrace signal)
            traces
        )
      , signal)
 where
  width = snatToNum (SNat @(BitSize a))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceSignal# #-}

-- | Trace a single vector signal: each element in the vector will show up as
-- a different trace. If the trace name already exists, this function will emit
-- an error.
traceVecSignal#
  :: forall dom n a
   . ( KnownNat n
     , BitPack a
     , NFDataX a
     , Typeable a
     , Waveform a )
  => IORef Maps
  -- ^ Map to store the traces
  -> Int
  -- ^ Associated clock period for the trace
  -> String
  -- ^ Name of signal in the VCD output. Will be appended by _0, _1, ..., _n.
  -> Signal dom (Vec (n+1) a)
  -- ^ Signal to trace
  -> IO (Signal dom (Vec (n+1) a))
traceVecSignal# maps period vecTraceName (unbundle -> vecSignal) =
  fmap bundle . sequenceA $
    Vector.zipWith trace' (iterateI succ (0 :: Int)) vecSignal
 where
  trace' i s = traceSignal# maps period (name' i) s
  name' i    = vecTraceName ++ "_" ++ show i
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceVecSignal# #-}

-- | Trace a single signal. Will emit an error if a signal with the same name
-- was previously registered.
--
-- __NB__: Works correctly when creating VCD files from traced signal in
-- multi-clock circuits. However 'traceSignal1' might be more convenient to
-- use when the domain of your circuit is polymorphic.
traceSignal
  :: forall dom  a
   . ( KnownDomain dom
     , BitPack a
     , NFDataX a
     , Typeable a
     , Waveform a )
  => String
  -- ^ Name of signal in the VCD output
  -> Signal dom a
  -- ^ Signal to trace
  -> Signal dom a
traceSignal traceName signal =
  case knownDomain @dom of
    SDomainConfiguration{sPeriod} ->
      unsafePerformIO $
        traceSignal# maps# (snatToNum sPeriod) traceName signal
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceSignal #-}
{-# ANN traceSignal hasBlackBox #-}

-- | Trace a single signal. Will emit an error if a signal with the same name
-- was previously registered.
--
-- __NB__: Associates the traced signal with a clock period of /1/, which
-- results in incorrect VCD files when working with circuits that have
-- multiple clocks. Use 'traceSignal' when working with circuits that have
-- multiple clocks.
traceSignal1
  :: ( BitPack a
     , NFDataX a
     , Typeable a
     , Waveform a )
  => String
  -- ^ Name of signal in the VCD output
  -> Signal dom a
  -- ^ Signal to trace
  -> Signal dom a
traceSignal1 traceName signal =
  unsafePerformIO (traceSignal# maps# 1 traceName signal)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceSignal1 #-}
{-# ANN traceSignal1 hasBlackBox #-}

-- | Trace a single vector signal: each element in the vector will show up as
-- a different trace. If the trace name already exists, this function will emit
-- an error.
--
-- __NB__: Works correctly when creating VCD files from traced signal in
-- multi-clock circuits. However 'traceSignal1' might be more convenient to
-- use when the domain of your circuit is polymorphic.
traceVecSignal
  :: forall dom a  n
   . ( KnownDomain dom
     , KnownNat n
     , BitPack a
     , NFDataX a
     , Typeable a
     , Waveform a )
  => String
  -- ^ Name of signal in debugging output. Will be appended by _0, _1, ..., _n.
  -> Signal dom (Vec (n+1) a)
  -- ^ Signal to trace
  -> Signal dom (Vec (n+1) a)
traceVecSignal traceName signal =
  case knownDomain @dom of
    SDomainConfiguration{sPeriod} ->
      unsafePerformIO $
        traceVecSignal# maps# (snatToNum sPeriod) traceName signal
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceVecSignal #-}
{-# ANN traceVecSignal hasBlackBox #-}

-- | Trace a single vector signal: each element in the vector will show up as
-- a different trace. If the trace name already exists, this function will emit
-- an error.
--
-- __NB__: Associates the traced signal with a clock period of /1/, which
-- results in incorrect VCD files when working with circuits that have
-- multiple clocks. Use 'traceSignal' when working with circuits that have
-- multiple clocks.
traceVecSignal1
  :: ( KnownNat n
     , BitPack a
     , NFDataX a
     , Typeable a
     , Waveform a )
  => String
  -- ^ Name of signal in debugging output. Will be appended by _0, _1, ..., _n.
  -> Signal dom (Vec (n+1) a)
  -- ^ Signal to trace
  -> Signal dom (Vec (n+1) a)
traceVecSignal1 traceName signal =
  unsafePerformIO $ traceVecSignal# maps# 1 traceName signal
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceVecSignal1 #-}
{-# ANN traceVecSignal1 hasBlackBox #-}

iso8601Format :: UTCTime -> String
iso8601Format = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

toPeriodMap :: TraceMap -> Map.Map Period [(String, Width, Maybe [AddValue], [Value])]
toPeriodMap m = foldl' go Map.empty (Map.assocs m)
  where
    go periodMap (traceName, (_rep, period, width, addValues, values)) =
      Map.alter (Just . go') period periodMap
        where
          go' = ((traceName, width, addValues, values):) . (fromMaybe [])

flattenMap :: Map.Map a [b] -> [(a, b)]
flattenMap m = concat [[(a, b) | b <- bs] | (a, bs) <- Map.assocs m]

printable :: Char -> Bool
printable (ord -> c) = 33 <= c && c <= 126

-- | Same as @dumpVCD@, but supplied with a custom tracemap and a custom timestamp
dumpVCD##
  :: (VCDTime, VCDTime)
  -- ^ (start, stop)
  -> VCDTime
  -- ^ time of first clock edge
  -> [(String,Period)]
  -- ^ list of clock signals
  -> Maps
  -> UTCTime
  -> Either String (Text.Text, JSON)
dumpVCD## (start, stop) startDelay clocks (signalMap,typeMap,traceMap) now
  -- TODO: equivalent
  -- | offset < 0 =
  --     error $ "dumpVCD: offset was " ++ show offset ++ ", but cannot be negative."
  -- | stop < 0 =
  --     error $ "dumpVCD: cycles was " ++ show cycles ++ ", but cannot be negative."
  | null traceMap && null clocks =
      error $ "dumpVCD: no traces found. Extend the given trace names."
  | (nm:_) <- offensiveNames =
      Left $ unwords [ "Trace '" ++ nm ++ "' contains"
                     , "non-printable ASCII characters, which is not"
                     , "supported by VCD." ]
  | otherwise =
      Right ( Text.unlines [ Text.unwords headerDate
                           , Text.unwords headerVersion
                           , Text.unwords headerComment
                           , Text.pack $ unwords headerTimescale
                           , "$scope module logic $end"
                           , Text.intercalate "\n" headerWires
                           , "$upscope $end"
                           , "$enddefinitions $end"
                           , "#0"
                           , "$dumpvars"
                           , Text.intercalate "\n" initValues
                           , "$end"
                           , Text.intercalate "\n" $ catMaybes bodyParts
                           ]
            , Json.object  [ "signals" .= signalMap
                           , "types" .= typeMap
                           , "luts" .= lutMap
                           ]
            )
 where
  offensiveNames = filter (any (not . printable)) traceNames

  -- Create labels like reversed integers (e.g. 1,2,01,11,21,02,12,22,001)
  labels = map go [1..]
   where
    go 0 = ""
    go n = chr ( 33 + n `mod` l) : go (n `div` l)
    l = 126-33+1

  -- Add clocks to traceMap
  traceMapWithClocks = Map.union traceMap $ Map.fromList $ map go clocks
    where 
       go (name,period) = (name,(undefined, period*500, 1, Nothing, cycle [(0,0),(0,1)]))


  -- The timescale used for timescale units. Clocks are included at their
  -- nominal frequency.
  -- Defined in fs.
  signalTimescale = foldl1' gcd (Map.keys periodMap <> map ((*1000) . snd) clocks)
  periodMap = toPeriodMap traceMapWithClocks

  -- Compute the final timescale depending on the signals, the clocks (which
  -- have double their nominal frequencies), and any strict time arguments.
  -- Defined in fs.
  timescale = foldl1' gcd
    (  [signalTimescale]
    <> map ((*500) . snd) clocks
    <> [p*1000 | VCDTime p _t s <- [start,stop,startDelay], s]
    )
  
  -- Use the largest timescale possible for the VCD file.
  timescaleWithUnit = go timescale "fs" ["ps","ns","us","ms","s"]
    where
      go n _ (u:us) | mod n 1000==0 = go (n `div` 1000) u us
      go n u _ = show n <> " " <> u
  
  -- Convert VCDTime values to actual timestamps
  withTimescale (VCDTime p t _s) = (1000*p + t*signalTimescale) `div` timescale
  start'      = withTimescale start
  stop'       = withTimescale stop
  startDelay' = withTimescale startDelay

  -- Normalize traces until they have the "same" period. That is, assume
  -- we have two traces; trace A with a period of 20 ps and trace B with
  -- a period of 40 ps:
  --
  --   A: [A0, A1, A2, A3, ...]
  --   B: [B0, B1, B2, B3, ...]
  --
  -- After normalization these look like:
  --
  --   A: [A0, A1, A2, A3, A4, A5, A6, ...]
  --   B: [B0, B1, B1, B2, B2, B3, B3, ...]
  --
  -- ..because B is "twice as slow" as A.
  --
  -- Note that the first value, which is the initial value, is not changed based
  -- on the clock period, since the first clock edge happens at the same time
  -- for all domains.
  (periods, traceNames, widths, addValuess, valuess) =
    unzip5 $ map
      (\(a, (b, c, d, e)) -> (a, b, c, d, e))
      (flattenMap periodMap)

  periods'    = map (`quot` timescale) periods
  valuess'    = map slice        $ zipWith normalize  periods' valuess
  addValuess' = map (fmap slice) $ zipWith normalize' periods' addValuess
  
  -- Copy values, but only copy LUT add function once.
  normalize  period (v:values) = 
       replicate (startDelay'+1) v
    <> concatMap (replicate period) values
  normalize  _      []            = []
  normalize' period (Just (a:addValues)) =
    Just $    replicate (startDelay'+1) a
           <> concatMap (\x -> x : replicate (period-1) id) addValues
  normalize' _      _             = Nothing

  slice :: [a] -> [a]
  slice values = drop start' $ take (stop'+1) values

  --
  lutMap = foldl (flip ($)) Map.empty $ concat $ catMaybes addValuess'


  -- HEADERS
  headerDate       = ["$date", Text.pack $ iso8601Format now, "$end"]

#ifdef CABAL
  clashVer         = Data.Version.showVersion Paths_shockwaves.version -- actually Shockwaves version; TODO
#else
  clashVer         = "development"
#endif
  headerVersion    = ["$version", "Generated by Clash", Text.pack clashVer , "$end"]
  headerComment    = ["$comment", "No comment", "$end"]
  headerTimescale  = ["$timescale", timescaleWithUnit, "$end"]
  headerWires      = [ Text.unwords $ headerWire w l n
                     | (w, l, n) <- (zip3 widths labels traceNames)]
  headerWire w l n = map Text.pack ["$var wire", show w, l, n, "$end"]
  initValues       = map Text.pack $ zipWith ($) formatters inits

  -- SIGNALS
  formatters = zipWith format widths labels
  inits = map (maybe (error "dumpVCD##: empty value") fst . uncons) valuess'
  tails = map changed valuess'

  -- | Format single value according to VCD spec
  format :: Width -> String -> Value -> String
  format 1 label (0,0)   = '0': label <> "\n" --TODO: why the newline???
  format 1 label (0,1)   = '1': label <> "\n"
  format 1 label (1,_)   = 'x': label <> "\n"
  format 1 label (mask,val) =
    error $ "Can't format 1 bit wide value for " ++ show label ++ ": value " ++ show val ++ " and mask " ++ show mask
  format n label (mask,val) =
    "b" ++ map digit (reverse [0..n-1]) ++ " " ++ label
    where
      digit d = case (testBit mask d, testBit val d) of
        (False,False) -> '0'
        (False,True)  -> '1'
        (True,_)      -> 'x'

  -- | Given a list of values, return a list of list of bools indicating
  -- if a value changed. The first value is *not* included in the result.
  changed :: [Value] -> [(Changed, Value)]
  changed (s:ss) = zip (zipWith (/=) (s:ss) ss) ss
  changed []     = []

  bodyParts :: [Maybe Text.Text]
  bodyParts = zipWith go [0..] (map bodyPart (Data.List.transpose tails))
              <> [Just $ Text.concat ["#", Text.pack $ show stop', "\n"]]
    where
      go :: Int -> Maybe Text.Text -> Maybe Text.Text
      go (Text.pack . show -> n) t =
        let pre = Text.concat ["#", n, "\n"] in
        fmap (Text.append pre) t

  bodyPart :: [(Changed, Value)] -> Maybe Text.Text
  bodyPart values =
    let formatted  = [(c, f v) | (f, (c,v)) <- zip formatters values]
        formatted' = map (Text.pack . snd) $ filter fst $ formatted in
    if null formatted' then Nothing else Just $ Text.intercalate "\n" formatted'

-- | Same as @dumpVCD@, but supplied with a custom tracemap
dumpVCD#
  :: NFDataX a
  => IORef Maps
  -- ^ Map with collected traces
  -> (VCDTime, VCDTime)
  -- ^ (start time, stop time)
  -> VCDTime
  -- ^ Time at which the first clock edges appear.
  -> [(String,Period)]
  -- ^ List of clock waveforms to generate.
  -> [String]
  -- ^ The names of the traces you definitely want to be dumped in the VCD file
  -> Signal dom a
  -- ^ (One of) the outputs of the circuit containing the traces
  -- ^ The names of the traces you definitely want to be dumped to the VCD file
  -> IO (Either String (Text.Text, JSON))
dumpVCD# maps slice startDelay clocks traceNames signal = do
  waitForTraces# maps signal traceNames
  m <- readIORef maps
  fmap (dumpVCD## slice startDelay clocks m) getCurrentTime

-- | Produce a four-state VCD (Value Change Dump) according to IEEE
-- 1364-{1995,2001}. This function fails if a trace name contains either
-- non-printable or non-VCD characters.
--
-- Due to lazy evaluation, the created VCD files might not contain all the
-- traces you were expecting. You therefore have to provide a list of names
-- you definately want to be dumped in the VCD file.
--
-- For example:
--
-- @
-- vcd <- dumpVCD (0, 100) cntrOut ["main", "sub"]
-- @
--
-- Evaluates /cntrOut/ long enough in order for to guarantee that the @main@,
-- and @sub@ traces end up in the generated VCD file.
dumpVCD
  :: NFDataX a
  => (Int, Int)
  -- ^ (offset, number of samples)
  -> Signal dom a
  -- ^ (One of) the outputs of the circuit containing the traces
  -> [String]
  -- ^ The names of the traces you definitely want to be dumped in the VCD file
  -> IO (Either String (Text.Text, JSON))
dumpVCD (start,stop) sig traceNames = dumpVCD# maps# (simCycles start,simCycles stop) 0 [] traceNames sig



-- | Produce a four-state VCD (Value Change Dump) according to IEEE
-- 1364-{1995,2001}. This function fails if a trace name contains either
-- non-printable or non-VCD characters.
--
-- Due to lazy evaluation, the created VCD files might not contain all the
-- traces you were expecting. You therefore have to provide a list of names
-- you definately want to be dumped in the VCD file.
--
-- For example:
--
-- @
-- vcd <- dumpVCD (0, 100) 0 [clockWave "clk" @Dom] ["main", "sub"] cntrOut
-- @
--
-- Evaluates /cntrOut/ long enough in order for to guarantee that the @main@,
-- and @sub@ traces end up in the generated VCD file.
advancedDumpVCD
  :: NFDataX a
  => (VCDTime, VCDTime)
  -- ^ (start time, stop time)
  -> VCDTime
  -- ^ Time at which the first clock edges appear.
  -> [(String,Period)]
  -- ^ List of clock waveforms to generate.
  -> [String]
  -- ^ The names of the traces you definitely want to be dumped in the VCD file
  -> Signal dom a
  -- ^ (One of) the outputs of the circuit containing the traces
  -> IO (Either String (Text.Text, JSON))
advancedDumpVCD = dumpVCD# maps#



-- | Dump a number of samples to a replayable bytestring.
dumpReplayable
  :: forall a dom
   . NFDataX a
  => Int
  -- ^ Number of samples
  -> Signal dom a
  -- ^ (One of) the outputs of the circuit containing the traces
  -> String
  -- ^ Name of trace to dump
  -> IO ByteString
dumpReplayable n oSignal traceName = do
  waitForTraces# maps# oSignal [traceName]
  replaySignal <- (Map.! traceName) . (\(_a,_b,c)->c) <$> readIORef maps#
  let (tRep, _period, _width, _addValues, samples) = replaySignal
  pure (ByteStringLazy.concat (tRep : map encode (take n samples)))

-- | Take a serialized signal (dumped with @dumpReplayable@) and convert it
-- back into a signal. Will error if dumped type does not match requested
-- type. The first value in the signal that fails to decode will stop the
-- decoding process and yield an error. Not that this always happens if you
-- evaluate more values than were originally dumped.
replay
  :: forall a dom n
   . ( Typeable a
     , NFDataX a
     , BitPack a
     , KnownNat n
     , n ~ BitSize a )
  => ByteString
  -> Either String (Signal dom a)
replay bytes0 = samples1
 where
  samples1 =
    case decodeOrFail bytes0 of
      Left (_, _, err) ->
        Left ("Failed to decode typeRep. Parser reported:\n\n" ++ err)
      Right (bytes1, _, _ :: TypeRep a) ->
        let samples0 = decodeSamples bytes1 in
        let err = "Failed to decode value in signal. Parser reported:\n\n " in
        Right (fromList (map (either (error . (err ++)) id) samples0))

-- | Helper function of 'replay'. Decodes ByteString to some type with
-- BitVector as an intermediate type.
decodeSamples
  :: forall a n
   . ( BitPack a
     , KnownNat n
     , n ~ BitSize a )
  => ByteString
  -> [Either String a]
decodeSamples bytes0 =
  case decodeOrFail bytes0 of
    Left (_, _, err) ->
      [Left err]
    Right (bytes1, _, (m, v)) ->
      (Right (unpack (BV m v))) : decodeSamples bytes1

-- | Keep evaluating given signal until all trace names are present.
waitForTraces#
  :: NFDataX a
  => IORef Maps
  -- ^ Map with collected traces
  -> Signal dom a
  -- ^ (One of) the output(s) the circuit containing the traces
  -> [String]
  -- ^ The names of the traces you definitely want to be dumped to the VCD file
  -> IO ()
waitForTraces# maps signal traceNames = do
  written <- atomicWriteIORef maps (Map.empty,Map.empty,Map.empty)
  rest <- foldM go (written `pseq` signal) traceNames
  -- atomicWriteIORef maps (Map.empty,Map.empty,Map.empty)
  -- rest <- foldM go signal traceNames
  seq rest (return ())
 where
  go (s0 :- ss) nm = do
    (_,_,m) <- readIORef maps
    if Map.member nm m then
      deepseqX s0 (return ss)
    else
      deepseqX
        s0
        (go ss nm)