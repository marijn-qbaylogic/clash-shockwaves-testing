
## How to add translations for difficult to unpack types
Sometimes, you find yourself face to face with a type that cannot be represented
properly using the standard translators - or the implementation is simply too much work.
In that case, you make consider using _lookup tables_.

When using LUTs, Shockwaves translates all values of types that use LUTs during simulation,
and stores these translations. In the waveform viewer, these are simply retreived using
the binary representations in the VCD file.

Since using LUTs is generally much simpler than doing a full `Waveform` implementation,
Shockwaves provides the `WaveformLUT` class that has some functions for making the process
easier. To connect the instance of `Waveform` to that of `WaveformLUT`, it can be derived
via `WaveformForLUT`.

By default, `WaveformLUT` uses the standard `Generic`-based functions to create subsignals,
but uses `Show` to determine the value. This means that a custom implementation may look like:

`WaveformLUT` splits the functionality into translating values for their own signal,
and the translations of subsignals.


> **Important:** Using LUTs has several drawbacks. Any miniscule change to the data type
> will result in a completely new translation being stored, which is bad for container types
> that may be very large or contain large values. Secondly, the lookup table only has the binary
> representation of the data type available: as such, `undefined` is indistinguishable from
> `(undefined,undefined)`, even though they are different values in Haskell. When writing
> a LUT instance, be aware of these properties.


### Changing a value

```hs
idk some example TODO
data MyData = ....
  deriving (...,Generic)
  deriving via (WaveformForLUT) instance Waveform

instance Show MyData
  show x = ...

instance WaveformLUT MyData
```

... you can overwrite this with `labelL`, `precL` and `styleL`

(example)

Example: color type

```hs
import style

data MyColor = MyRGB Word8 Word8 Word8 deriving (...)
  deriving via ....

instance Show MyColor where
  show (MyRGB r g b) = "#" ++ hex (r `div` 16)
                           ++ hex (r `rem` 16)
                           ++ hex (g `div` 16)
                           ++ hex (g `rem` 16)
                           ++ hex (b `div` 16)
                           ++ hex (b `rem` 16)

instance WaveformLUT MyColor where
  precL _ = 11
  styleL (MyRGB r g b) = WSColor (RGB r g b)
```

### Changing subsignals

To change the subsignals, you need two things:
- to define the structure
- to translate values into subsignal translations

By default, this is implemented the same way as the default `Waveform` instances:
using `Generic`, but this is not always what you want. Let's say that for our
`MyColor` data type, we want to label the color channel subsignals, and
display them in their own colors.

First, we need to define the structure. Our data type has three subsignals, called
`red`, `blue` and `green`, which each do not have subsignals. We write:

```hs
structureL = Structure
  [ ("red"  , Structure [])
  , ("green", Structure [])
  , ("blue" , Structure []) ]
```

Now we simply have to translate the values! It is extremely important that the
translations match, or Surfer might crash! Though matching here means we have no
signals that are not present int the structure - leaving _out_ signals is fine.

```hs
splitL rgb =
  [ ("red"  , applyStyle (WSColor (RGB (getR rgb) 0 0)) $ translate $ getR rgb)
  , ("green", applyStyle (WSColor (RGB 0 (getG rgb) 0)) $ translate $ getG rgb)
  , ("blue" , applyStyle (WSColor (RGB 0 0 (getB rgb))) $ translate $ getB rgb) ]
 where
  getR (MyRGB r _g _b) = r
  getG (MyRGB _r g _b) = g
  getB (MyRGB _r _g b) = b
```

> Note how we do not write `??? (MyRGB r g b) = `. This would fail on `undefined`,
> even though the binary representation of our type is the same as
> `MyRGB undefined undefined undefined`. Furthermore, we use the default translation
> of `Word8`, which has builtin error detection, so that a single `undefined` among
> `r`, `g`, `b` does not cause the entire function to fail.

Putting all the code together:
```hs
full code TODO
```

If we look at our type in the waveform viewer now, we see:
[TODO:img]


