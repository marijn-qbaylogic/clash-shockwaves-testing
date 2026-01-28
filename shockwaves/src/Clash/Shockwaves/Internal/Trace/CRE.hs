{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}


module Clash.Shockwaves.Internal.Trace.CRE where

import Clash.Prelude hiding (traceSignal, register)
import Clash.Explicit.Prelude (register,noReset)
import Data.Typeable
import qualified Data.List as L
import Data.Tuple.Extra (uncurry3)

import Clash.Shockwaves.Waveform hiding (tConst)
import Clash.Shockwaves.LUT
import Clash.Shockwaves.Trace





data ClockWave = ClockWave Bool
               | ClockInit
  deriving (Generic,Typeable,BitPack,NFDataX)

data ResetWave (dom::Domain) = ResetWave Bool
  deriving (Generic,Typeable,BitPack,NFDataX)

data EnableWave = EnableWave Bool
  deriving (Generic,Typeable,BitPack,NFDataX)

data CREWave dom = CREWave {clock::ClockWave, reset::ResetWave dom, enable::EnableWave}
  deriving (Generic,Typeable,BitPack,NFDataX)
  deriving Waveform via (WaveformForLUT (CREWave dom))

vConst :: Render -> Translator
vConst r = Translator 0 $ TConst $ tConst r
tConst :: Render -> Translation
tConst r = Translation r []


clkI :: Render
clkI = Nothing
clkA :: Render
clkA = Just ("",WSVar "clk_a" "#fff",11)
clkB :: Render
clkB = Just ("",WSVar "clk_b" "#83b",11)

rstOff :: Render
rstOff = Just ("ACTIVE", WSVar "reset_off" WSHidden,11)
rstOn :: Render
rstOn = Just ("RESET", WSVar "reset_on" WSWarn,11)

enOn :: Render
enOn = Just ("ENABLED", WSVar "enable_on" WSHidden,11)
enOff :: Render
enOff = Just ("DISABLED", WSVar "enable_off" WSWarn,11)

rstAndDis :: Render
rstAndDis = Just ("DISABLED|RESET", WSVar "reset_on_enable_off" WSWarn,11)

instance Waveform ClockWave where
  translator = Translator 2 $ TSum
    [ Translator 1 $ TSum
      [ vConst clkA
      , vConst clkB
      ]
    , vConst clkI
    ]

instance (KnownDomain dom) => Waveform (ResetWave dom) where
  translator = Translator 1 $ TSum $ L.map vConst
    ( case resetPolarity @dom of
        SActiveHigh -> [rstOff,rstOn]
        SActiveLow  -> [rstOff,rstOn] )

instance Waveform EnableWave where
  translator = Translator 1 $ TSum
    [ vConst enOff
    , vConst enOn
    ]

instance KnownDomain dom => WaveformLUT (CREWave dom) where
  translateL = displaySplit displayL splitG
    where 
      displayL (CREWave c r (EnableWave e)) = case (c,isRst r,e) of
        (_              ,True,False) -> rstAndDis
        (_              ,True,_    ) -> rstOn
        (_              ,_   ,False) -> enOff
        (ClockInit      ,_   ,_    ) -> clkI
        (ClockWave False,_   ,_    ) -> clkA
        (ClockWave True ,_   ,_    ) -> clkB
        where
          isRst (ResetWave r') = case resetPolarity @dom of
            SActiveHigh -> r'
            SActiveLow  -> not r'


clkSignal :: (KnownDomain dom) => Clock dom -> Signal dom ClockWave
clkSignal clk = s
  where
    s = register clk noReset enableGen ClockInit s'
    s' = next <$> s
    next val = case val of
      ClockInit -> ClockWave False
      ClockWave b -> ClockWave (not b)

traceClock :: (KnownDomain dom) => String -> Clock dom -> Clock dom
traceClock lbl clk = traceSignal lbl (clkSignal clk)
                     `seq` clk

traceReset :: (KnownDomain dom) => String -> Reset dom -> Reset dom
traceReset lbl (rst::Reset dom) = traceSignal lbl (ResetWave @dom <$> unsafeFromReset rst)
                                  `seq` rst

traceEnable :: (KnownDomain dom) => String -> Enable dom -> Enable dom
traceEnable lbl en = traceSignal lbl (EnableWave <$> fromEnable en)
                     `seq` en

traceClockResetEnable :: (KnownDomain dom) => String -> Clock dom -> Reset dom -> Enable dom -> (Clock dom, Reset dom, Enable dom) -- is this a useful return value?
traceClockResetEnable lbl (c::Clock dom) r e = traceSignal lbl
                                     ( uncurry3 CREWave <$> bundle
                                       ( clkSignal c
                                       , ResetWave <$> unsafeFromReset r
                                       , EnableWave <$> fromEnable e
                                       ) :: Signal dom (CREWave dom) )
                                     `seq` (c,r,e)


traceHiddenClock :: (KnownDomain dom, HiddenClock dom) => String -> r -> r
traceHiddenClock lbl x = traceClock lbl hasClock `seq` x
traceHiddenReset :: (KnownDomain dom, HiddenReset dom) => String -> r -> r
traceHiddenReset lbl x = traceReset lbl hasReset `seq` x
traceHiddenEnable :: (KnownDomain dom,HiddenEnable dom) => String -> r -> r
traceHiddenEnable lbl x = traceEnable lbl hasEnable `seq` x
traceHiddenClockResetEnable :: (KnownDomain dom, HiddenClockResetEnable dom) => String -> r -> r
traceHiddenClockResetEnable lbl x = traceClockResetEnable lbl hasClock hasReset hasEnable `seq` x