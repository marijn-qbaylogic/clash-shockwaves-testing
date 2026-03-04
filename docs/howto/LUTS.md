TODO: update

## How to add translations for difficult to unpack types
Sometimes, you find yourself face to face with a type that cannot be represented
properly using the standard translators - or the implementation is simply too much work.
In that case, you make consider using _lookup tables_.

When using LUTs, Shockwaves translates all values of types that use LUTs during simulation,
and stores these translations along with the translators. In the waveform viewer, these are
simply retreived using the binary representations in the VCD file.

Since using LUTs is generally much simpler than doing a full `Waveform` implementation,
Shockwaves provides the `WaveformLUT` class that has some functions for making the process
easier. To connect the instance of `Waveform` to that of `WaveformLUT`, it can be derived
via `WaveformForLUT`.

By default, `WaveformLUT` uses the standard `Generic`-based functions to create subsignals,
but uses `Show` to determine the value.

A LUT implementation requires two functions: `structureL` to provide the subsignal structure,
and `translateL` to create the translation of a runtime value.


> **Important:** Using LUTs has several drawbacks. Any miniscule change to the data type
> will result in a completely new translation being stored, which is bad for container types
> that may be very large or contain large values. Secondly, the lookup table only has the binary
> representation of the data type available: as such, `undefined` is indistinguishable from
> `(undefined,undefined)`, even though they are different values in Haskell. Shockwaves first
> converts value to binary and then reconstructs them to guarantee that the translations
> accurately match the values in the VCD file. This does however mean that they are not direct
> translations of the values that occur in simulation!


### CHANGING THE RENDER VALUE

Since `WaveformLUT` uses `Show` by default, it's very easy to change the text of a label:
derive `Waveform` via `WaveformForLUT`, create a `WaveformLUT` instance,
and simply overwrite `show`:

```hs
idk some example TODO
data MyData = ....
  deriving (...,Generic)
  deriving via (WaveformForLUT) instance Waveform

instance Show MyData
  show x = ...

instance WaveformLUT MyData
```

If you want to change more, we need to first look at the default implementation
of `translateL`:

```hs
translateL :: a -> Translation
default translateL :: (Generic a, Show a, WaveformG (Rep a ()), PrecG (Rep a ())) => a -> Translation
translateL = translateWith renderShow splitL
```

`renderWith` splits the translation functionality into the creation of a render value,
and the creation of subsignals.

`renderShow` is defined as `renderWith show (const WSNormal) precL`: to create a render value,
call `show` for the label, `const WSNormal` for the style, and `precL` for the operator precedence.

`splitL` uses `WaveformG` to create a structure like the standard translator, but uses the render value
for the toplevel and constructor render values.


Since a simple value with `WSNormal` and precedence `11` is fairly common (for floats, for example),
there are a few special translation functions for these cases: `translateAtomWith`, `translateAtomShow`,
`translateAtomSigWith`, `translateAtomSigShow`.


Let's have a look at an example. Say we have a color value,
and want to actually show the waveform in this color. We can
then write a `WaveformLUT` implementation that assigns a
custom color style to each value individually.

```hs
import Clash.Shockwaves.Style

data MyColor = MyRGB Word8 Word8 Word8 deriving (...)
  deriving via ....

instance Show MyColor where
  show (MyRGB r g b) = "#" ++ hex (r `div` 16)
                           ++ hex (r `rem` 16)
                           ++ hex (g `div` 16)
                           ++ hex (g `rem` 16)
                           ++ hex (b `div` 16)
                           ++ hex (b `rem` 16)

colorToStyle (MyRGB r g b) = WSColor (RGB r g b)

instance WaveformLUT MyColor where
  translateL = translateWith (renderWith show colorStyle (const 11)) splitL
    where colorStyle (MyRGB r g b) = WSColor (RGB r g b)
```


### CHANGING THE SUBSIGNALS

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
translateL = translateWith (renderWith show colorStyle (const 11)) splitColor
  where
    colorStyle (MyRGB r g b) = WSColor (RGB r g b)
    splitColor (MyRGB r g b) =
      [ ("red"  , applyStyle (WSColor (RGB r 0 0)) $ translate $ r)
      , ("green", applyStyle (WSColor (RGB 0 g 0)) $ translate $ g)
      , ("blue" , applyStyle (WSColor (RGB 0 0 b)) $ translate $ b) ]
```

> Note that `MyRGB undefined 0 0` will show up as `undefined`,
> since the both the split and style functions fail.
> Making the subsignals show up is relatively easy: instead of
> deconstructing the `MyRGB` value in `splitColor`, create lazily accessed
> value and pass these to `translate` and `applyStyle`.
> If one of the channels is undefined, `translate` will neatly return
> a translation with the `WSError` style, which is not replaced by
> the partially undefined style by `applyStyle`.
> ```hs
> splitColor rgb =
>   [ ("red"  , applyStyle (WSColor (RGB r 0 0)) $ translate $ r)
>   , ("green", applyStyle (WSColor (RGB 0 g 0)) $ translate $ g)
>   , ("blue" , applyStyle (WSColor (RGB 0 0 b)) $ translate $ b) ]
>   where
>     r = (\(MyRGB r _ _) -> r) rgb
>     g = (\(MyRGB _ g _) -> g) rgb
>     b = (\(MyRGB _ _ b) -> b) rgb
> ```


If we look at our type in the waveform viewer now, we see:
[TODO:img]


