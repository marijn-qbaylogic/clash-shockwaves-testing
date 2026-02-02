{-|
Module      : Clash.Shockwaves.Trace.CRE
Description : Trace clock, reset and enable signals

Functions for tracing clock, reset and enable signals. The signals can be traced
separately or together, using either hidden or explicit signals. The styles can
be configured through style variables.
-}

module Clash.Shockwaves.Trace.CRE (
  -- * Tracing explicit signals
  traceClock,
  traceReset,
  traceEnable,
  traceClockResetEnable,
  -- * Tracing hidden signals
  traceHiddenClock,
  traceHiddenReset,
  traceHiddenEnable,
  traceHiddenClockResetEnable,
) where

import Clash.Shockwaves.Internal.Trace.CRE