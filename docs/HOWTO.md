# Shockwaves How-To Guides
- how does shockwaves work

Shockwaves is a system for getting typed Clash waveforms in Surfer. It works by storing additional type information
alongside the typical VCD output.

[TOC]

## How to set up Shockwaves

To start using shockwaves, you need three things:
- the latest version of Surfer [TODO:link]
- the Shockwaves Surfer extension [TODO:link]
- the `Clash.Shockwaves` Haskell library [TODO:link]

Install Surfer (see [TODO:link]), add the Shockwaves extension (download [TODO:link] and install according to the instructions [TODO:link]).
If the extension is installed correctly, it should print messages to the output.

Add the repository `clash-lang/clash-shockwaves` to your project and configure it to use subdirectory `shockwaves` [TODO:how].

## How to get typed waveforms
//- simple tracing
//- deriving Waveform

Shockwaves has to output additional type information alongside the VCD output.
For this to work, you need to do two things:
- specify how types are shown in the waveform viewer
- trace signals using the `Clash.Shockwaves.Trace` library

Let's first take a look at tracing.

### TRACING
This is what a typical usage of `Clash.Signal.Trace` looks like:

```hs

sth sth traceSignal

sth sth dumpVCD -> error|vcd

```

The code for `Clash.Shockwaves.Trace` is very close: instead of creating only producing a VCD file,
the function also creates a JSON object containing all of the translation information.

```hs
sth sth traceSignal
sth sht dumpVCD -> error|(vcd,meta)
```

**IMPORTANT:** The JSON file must have the same base filename as the VCD file! Otherwise, Shockwaves
does not know where to look for it.

If we now run the `main` function, we get the files `waveform.vcd` and `waveform.json`. Opening the
VCD file in Surfer, this is what the result looks like:

[TODO: image]

### CUSTOM DATA TYPES
This is great and all, but what if we want to use our own data types?

Imagine we have the following code

```hs
data Led = Red | Blue | Green
etc TODO
```

If we try to run this, we get:
```
error: Waveform TODO
```

`Waveform` is the class that allows Shockwaves to render data types. It specifies exactly
how a type should be translated. To be able to trace our data type `Led`, we need to create a
`Waveform` instance.

Luckily, in most cases `Waveform` can be derived directly! Deriving `Waveform` uses `Generic`
to create subsignals for constructors and data fields.

If we add `Generic` and `Waveform` to our type:
```hs
TODO: code
```
and run the code again, it should now compile and run. Opening the file in `Surfer`, we see:

[TODO:image]

That's it! Using this method, you can trace _almost_ all data types.

In cases where such a derivation fails, you might want to use one of the following resources:
- Custom Waveform [TODO:link]
- GADTS [TODO:link]
- Numbers [TODO:link]
- Constant values [TODO:link]
- Difficult to translate values [TODO:link]

If you want to customize the behaviour, you can use one of these resources:
- Using styles [todo:LINK]
- Shockwaves configuration [todo:LINK]
- Adding extra information [todo:LINK]


## How to use styles to improve legibility
- wavestyles
- adding to waveform instance
- error

Color can be an incredible visual aid. Shockwaves allows you to add colors through
`WaveStyles`. This guide will show you how to use `WaveStyles`, and how to apply them
to data constructors.

Let's assume we are working on a simple CPU that has several arithmetic instructions.
We would like to be able to mark these instructions with some colors: addition in green,
subtraction in red, multiplication in blue and division in purple.

```hs
data Instr = Add | Sub | Mul | Div
-- green red blue purple
```

### PICKING A WAVESTYLE
There are a number of standard styles to pick from, corresponding to different
VCD value types:
- ...
- ...
- ...

```hs
style = WSWarn
```

These will change with the selected theme. However, these do not seem to include
exactly the colors we want, so we are better off using a custom style:

- `WSColor c`

There are multiple ways of creating a color value. First of all, you can directly
specify the color as an 8-bit RGB value:
```hs
style = WSColor (RGB 255 0 0)
```

It is also possible to use the predefined colors in `Data.Colour.???TODO`, which is exported
in `Clash.Shockwaves.Style.???TODO`, but since these are defined using real values,
they need to be converted first. You can make a `WaveStyle` directly using the `wsColor`
function:

```hs
import ...
style = wsColor red
```

Alternatively, when using `OverloadedStrings`, you can specify the color name as a `String`
directly:

```hs
{OverloadedStrings}
import ...
style = "red"
```

It is also possible to specify the string as a hexadecimal value:
```hs
{OverloadedStrings}
style = "#f00"
```

Finally, it is possible to use _style variables_ in a configuration file (see [TODO:link]).
If the variable cannot be found, the second argument (default value) is used.
```hs
style = WSVar "myRed" $ WSWarn
```
It is also possible to get a variable from a string, in which case `WSNormal` is taken
as the default value:
```hs
{OverloadedStrings}
style = "$myRed"
```


**IMPORTANT:** There exists one more wavestyle: `WSError`. This style is meant for
`unknown` values, and gets propagated upwards in the signal tree all the way to the
topmost signal.

### ADDING WAVESTYLES TO THE DATA TYPE
Now it is time to add the wavestyles we picked to the design.
Since marking constructors is a common usecase, constructor colors were added
to the default derivation of `Waveform`. All we need to do is create a `Waveform`
instance and overwrite the `styles` list:

```hs
data Instr = ...
instance Waveform Instr where
  styles = ["green", wsColor red, "#11f", WSColor (RGB 100 0 255)]
```



## How to do a completely custom implementation of Waveform?
- waveform: all functions
- translators
- things to keep in mind

## How to implement Waveform for integer-like types?
- waveform for number types

## How to implement Waveform for constant value types?
- waveform for const types

You may be using integer number types that are not yet supported by Shockwaves,
and find yourself unable to derive a proper `Waveform` instance for your data type.
Luckily, you do not need a fully custom implementation of `Waveform`.

Say you have some data type `MyInt` that you want to display.
Then you can derive `Waveform` via `WaveformForNum`!

```hs
import ...
data MyNum = ...
  deriving via WaveformForNum format spacer MyNum instance Waveform MyNum
```

As you can see, for using `WaveformForNum`, we need to provide some extra types.
The first one determines what you want your number to be translated into:
- `NFSig` for signed decimal numbers (using two's complement)
- `NFUns` for unsigned decimal numbers
- `NFBin` for binary numbers
- `NFOct` for octal numbers
- `NFHex` for hexadecimal numbers

The second one is a spacer. Since large numbers may be difficult to read,
Shockwaves has the option to add spacer strings every few characters. For
example, the number `123456` may be displayed as `123_456` instead.

`Maybe (Int,String)`
If you do not want any spacers, you can use `'Nothing`. Otherwise, this provides
the string to insert and the number of digits between occurances.

For example, if we want out `MyNum` to have digits grouped by hundreds instead of thousands,
we may write:

```hs
...
  deriving via (WaveformForNum NFUns ('Just '(2,"_")) MyNum) instance Waveform MyNum
```

Some default spacers are defined. `NoSpacer` is the same as `'Nothing`. `SpacerDec` is the default for decimal numbers and
places a `_` every 3 digits; `SpacerBin`, `SpacerOct` and `SpacerHex` all place a `_`
every 4 digits.

**Note:** The spacer configuration can be overwritten globally per number format!
Check [TODO:link] for more.


## How to implement Waveform for GADTs?
- custom waveform

GADTs can be rather annoying to work with, since they do not allow the derivation of `Generic`.
This means you will either need to go the lazy route and use lookup tables (see [TODO:link];
this is not covered in this howto) or do a fully custom implementation of `Waveform`.
This guide expects you to already know how to do custom `Waveform` implementations (seel [TODO:link]),
and discusses two ways to deal with GADTs in particular.

### OPTION 1: DO ALL OF IT AT ONCE
The first option is to disregard the GADTs structure and look at the actual data type instead.
This type may be easier to translate than the actual GADTs. A good example of this is `Vec`:
while it would be possible to create instances for `Cons` and `Nil`, this is far more cumbersome
(and would result in far less clear subsignals) than by simply turning the `Vec` into a list,
and generating the rest from there.

This method depends heavily on what you are trying to achieve. We will go through the
implementation of `Vec` in here.

...

### OPTION 2: SPLIT THE WAVEFORM CLASS
A different method it to split up implementations per constructor by creating a
an extra class that selects the constructor at the type level. An example of this is
`RTree`, which has the constructors `???` and `Leaf`. At the type level, _we_ already know
which one of these constructors is going to be used, but Haskell does not.

First of all, let's look at all the methods we need to overwrite:
...
(some might be the same for branch and leaf)

Now we can create a helper class:

```hs
class WaveformForRTree isLeaf n a where
  ...
```

We can then write instances for the different constructors:

```hs
instance (Waveform (RTree (n-1) a)) => WaveformForRTree False n a where
  ...
```

Note how we could not have added the `Waveform` bound for a smaller `n` for the leaf node!

```hs
instance (Waveform a) => WaveformForRTree True n a where
  ...
```

Now all that's left is to call the right functions from our `Waveform` instance!

```hs
instance (WaveformForRTree (n==0) n a) => Waveform (RTree n a)
  ... @(WaveformForRTree (n==0) n a)
```

**Important:** keep in mind that values must be lazily evaluated where possible!
For example, if the type leaves only one constructor that can be created, do not use:
```hs
func (Constructor a b) = func2 a + func3 b
func _ = undefined
```
Instead, try to write code like:
```hs
func c = func2 (getA c) + func3 (getB c)
```






## How to add translations for difficult to unpack types?
- luts

Sometimes, you find yourself face to face with a type that cannot be represented
properly using standard translators, or the implementation is simply too much work.
In that case, you make consider using lookup tables.

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
...
```

If we look at our type in the waveform viewer now, we see:
[TODO:img]


## How to add extra information to your data types?
- waveform for luts

## How to customize Shockwaves behaviour?
- config files
- error prop
- number spacers
- styles

## How to add clock signals?
- advancedDumpVCD clocks
- special clock signals

## How to control VCD timings in multi-domain designs?
- advancedDumpVCD timings
