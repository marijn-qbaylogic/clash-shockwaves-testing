{-|

Module for more advanced styling.

Waveforms can be styled in a number of ways. Various standard styles are provided
in 'WaveformStyle'. It is also possible to use custom colors using 'WSColor'.
These values can be provided in 3 typical ways.
- Directly from an @RGB Word8@ value, using the 'WSColor' constructor:
  @WSColor (RGB 128 128 0)@
- From a @Colour Double@ value, using the function 'wsColor'.
  Many such values are provided in "Clash.Shockwaves.Style.Colors"
- Using a string literal: if @OverloadedStrings@ is enabled, the 'WaveStyle'
  value can be represented using a lowercase string literal.
  See 'Data.Colour.Names.readColourName'.

@
import Shockwaves.Style.Colors as C

data Col = Red | Green | Blue deriving ...

instance WaveformLUT Col where
  styleL = \case
    Red   -> WSColor (RGB 255 0 0) -- RGB value
    Green -> wsColor C.lime        -- Colour value
    Blue  -> "blue"                -- string literal
@


-}

module Clash.Shockwaves.Style (
  module Colors,
  WaveStyle(..),
  Color,
  RGB(..),
  wsColor,
  Word8,
) where
import Clash.Prelude
import Clash.Shockwaves.Internal.Types (WaveStyle(..),Color)
import Clash.Shockwaves.Style.Colors as Colors
import Data.Colour.SRGB (RGB(..), toSRGB24)
import Data.Word (Word8)
import Data.Colour (Colour)

-- | Create a colored 'WaveStyle' from a color value defined in 'Double's.
--   This is the type of the predefined default colors in 'Data.Colour'.
wsColor :: Colour Double -> WaveStyle
wsColor = WSColor . toSRGB24
