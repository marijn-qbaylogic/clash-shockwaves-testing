
-- exports everything you need for standard Waveform derives: the Waveform class with the styles list, wavform styles, and the tracing lib
module Clash.Shockwaves (
  Waveform(styles),
  WaveStyle(..),
  module Clash.Shockwaves.Trace,
  writeFileJSON
) where

import Clash.Shockwaves.Internal.Types
import Clash.Shockwaves.Internal.Waveform
import Clash.Shockwaves.Internal.Util
import Clash.Shockwaves.Trace
