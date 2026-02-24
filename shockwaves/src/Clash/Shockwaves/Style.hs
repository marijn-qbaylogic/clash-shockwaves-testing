{- |
Copyright  :  (C) 2025-2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
Module      : Clash.Shockwaves.Style
Description : Shockwaves tools for styling signals

Everything needed to add different styles to the waveform viewer.

Waveforms can be styled in a number of ways. Various standard styles are provided
in 'WaveformStyle'. It is also possible to use custom colors using 'WSColor'.
These values can be provided in 3 typical ways:

- Directly from an @RGB Word8@ value, using the 'WSColor' constructor:
  @WSColor (RGB 128 128 0)@.
- From a @Colour Double@ value, using the function 'wsColor'.
  Many such values are provided in "Clash.Shockwaves.Style.Colors".
- Using a string literal: if @OverloadedStrings@ is enabled, the 'WaveStyle'
  value can be represented using a color name or hex color string literal.
  See 'Data.Colour.Names.readColourName'.

Finally, 'WSVar' makes it possible to define style variables in config files.
This can also be achieved by using a string literal starting with @$@.

Some examples:

@
import Shockwaves.Style
import Shockwaves.Style.Colors as C

data Col = Red | Green | Blue | Yellow | Cyan deriving ...

instance Waveform Col where
  styles = [ WSColor (RGB 255 0 0) -- RGB value
           , wsColor C.lime        -- Colour value
           , "blue"                -- color name
           , "#ffff00"             -- hexadecimal color
           , "$cyan"               -- style variable
@
-}
module Clash.Shockwaves.Style (
  WaveStyle (..),
  Color,
  RGB (..),
  wsColor,
  Word8,
) where

import Clash.Prelude
import Clash.Shockwaves.Internal.Types (Color, WaveStyle (..))
import Data.Colour (Colour)
import Data.Colour.SRGB (RGB (..), toSRGB24)
import Data.Word (Word8)

-- | Create a colored 'WaveStyle' from a color value defined in 'Double's.
--   This is the type of the predefined default colors in "Data.Colour".
wsColor :: Colour Double -> WaveStyle
wsColor = WSColor . toSRGB24
