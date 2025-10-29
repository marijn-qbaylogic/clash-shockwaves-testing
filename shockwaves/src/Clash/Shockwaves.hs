{- |

Shockwaves is a system for typed waveforms in Clash.
'Clash.Shockwaves' exports the minimum requirements for using Shockwaves.

For more options, see 'Clash.Shockwaves.Waveform', 'Clash.Shockwaves.Style',
'Clash.Shockwaves.LUT' and 'Clash.Shockwaves.Trace'.

-}
-- exports everything you need for standard Waveform derives: the Waveform class with the styles list, wavform styles, and the tracing lib
module Clash.Shockwaves (
  Waveform(styles),
  WaveStyle(..),
  module Clash.Shockwaves.Trace,
  -- ^ Adaptation of 'Clash.Signal.Trace' for use with Shockwaves.
  writeFileJSON
) where

import Clash.Shockwaves.Internal.Types
import Clash.Shockwaves.Internal.Waveform
import Clash.Shockwaves.Internal.Util
import Clash.Shockwaves.Trace
