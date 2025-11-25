## How to use colors to improve clarity
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
