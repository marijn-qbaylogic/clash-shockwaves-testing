
## How to add extra information to your data types
With Shockwaves, you can add extra debug information to your data type. For example,
if you have a `Coordinate` data type that is defined in Carthesian coordinates,
you might want to display the distance to the origin. This data is not directly
available for translation, but we can add it with lookup tables!

The full guide on using lookup tables can be found [here](LUTS.md).

### SETTING UP LUTS
Let's assume we have our datatype `Coordinate`:

```hs
import Clash.Shockwaves

data Coordinate = Coordinate{ x::Int, y::Int }
  deriving (Typeable,Generic,BitPack,NFDataX,Waveform)

norm :: Coordinate -> Float
norm Coordinate{x,y} = sqrt $ fromIntegral (x*x + y*y)
```

First, we modify it to use lookup tables instead:

```hs
{-# LANGUAGE DerivingVia #-}
import Clash.Shockwaves
import Clash.Shockwaves.Waveform
import Clash.Shockwaves.LUT

data Coordinate = Coordinate Int Int
  deriving (Typeable,Generic,BitPack,NFDataX,Show)
  deriving Waveform via WaveformForLUT Coordinate

instance WaveformLUT Coordinate where

norm :: Coordinate -> Float
norm (Coordinate x y) = sqrt $ fromIntegral (x*x + y*y)
```

We can now modify what our data types look like by changing the `WaveformLUT` implementation.


### ADDING A SIGNAL

Let's assume we don't want to change the appearance of the toplevel signal -
the LUT guide already has enough information on that anyways, and if you change
`Show` that will already automatically show up. Instead, we are going to add a
subsignal.

The `splitL` and `structureL` functions let you overwrite the subsignals that get created.
`splitL` creates the actual subsignal translations, which must be a subtree of the
hierarchy provided by `structureL`. We can easily translate the normal fields using
`translate` and obtain the structure using `structure` and `translator`.

```hs
instance WaveformLUT Coordinate where
  splitL _ (Coordinate x y) =
    [ ("x", translate x)
    , ("y", translate y)
    , ("norm", Translation (Just (show $ norm (Coordinate x y),WSNormal,11)) [])
    ]
  structureL = Structure
    [ ("x", structure $ translator @Int)
    , ("y", structure $ translator @Int)
    , ("norm", Structure [])
    ]
```

> Note: You do not need to make `splitL` robust against `undefined`.


That's it! If we show our data type in Surfer, we see:

![now with norm](extra/norm.png)

There are a few ways to improve our code. First of all, we can get rid of the ugly
manual `Translation` using `tFromVal`:

```hs
splitL _ c@(Coordinate x y) =
  [ ("x", translate x)
  , ("y", translate y)
  , ("norm", tFromVal . show $ norm c)
  ]
```

Furthermore, instead of also redefining the signals that are originally present,
you can use the underlying `Generic`-based functions that are used to create these
subsignals, and simply add the new signals. These internal functions might change,
but can be convenient if there are a lot of standard subsignals.

```hs
import Clash.Shockwaves.Internal.Waveform (splitG,translatorG)

instance WaveformLUT Coordinate where
  splitL r c =
       splitG r (from c :: Rep Coordinate ())
    <> [("norm", tFromVal . show $ norm c)]

  structureL = Structure (dflt <> extra)
    where
      Structure dflt = structure $ translatorG @(Rep Coordinate ()) 0 (Data.List.repeat WSNormal)
      extra = [("norm", Structure [])]
```

