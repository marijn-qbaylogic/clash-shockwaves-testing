
## How to implement Waveform for GADTs
GADTs can be rather annoying to work with, since they do not allow the derivation of `Generic`.
This means you will either need to go the lazy route and use (lookup tables)[LUTS.md];
or do a fully custom implementation of `Waveform`. This guide expects you to already know
how to do custom `Waveform` implementations (see [this guide](WAVEFORM.md)),
and discusses two ways to deal with GADTs in particular.

### OPTION 1: DO ALL OF IT AT ONCE
The first option is to disregard the GADTs structure in implementation and look at the
abstract data type instead. This type may be easier to translate than the actual GADTs.
A good example of this is `Vec`: while it would be possible to create instances for `Cons`
and `Nil`, this is far more cumbersome (and would result in far less clear subsignals)
than by simply turning the `Vec` into a list, and generating the rest from there.
These functions do not need to be synthesizable anyways.

This method depends heavily on what you are trying to achieve. We will go through the
implementation of `Vec` in here.

This is the `Waveform` instance for `Vec` inside `Clash.Shockwaves.Internal.Waveform`:

```hs
instance (KnownNat n, Waveform a) => Waveform (Vec n a) where
  translator = Translator (width @(Vec n a)) $ if natVal (Proxy @n) /= 0 then
    TArray
      { start = ""
      , sep = " :> "
      , stop = " :> Nil"
      , preci = 5
      , preco = 5
      , len = fromIntegral $ natVal (Proxy @n)
      , sub = tRef (Proxy @a)
      }
    else
      TConst $ Translation (Just ("Nil",WSNormal,11)) []

  addSubtypes = addTypes @a
  addValue v = if hasLUT @(Vec n a) then go $ Clash.Prelude.toList v else id
    where
      go l = case safeWHNF l of
        Just []     -> id
        Just (x:xs) -> addValue x . go xs
        Nothing     -> id
  hasLUT = hasLUT @a && (natVal (Proxy @n) /= 0)
```

The implementation completely discards the notion of `Cons` and `Nil` constructors,
and simply treats the array as a list with known length.

To start, we define an array translator in `translator`. For empty vectors,
there is a separate constant implementation.

In `addSubtypes`, we only have to use the element's type, and the same goes for `hasLUT`.
In the case of an empty vector, we make sure to set `hasLUT` to `False` for empty vectors,
to potentially save on LUT-induced overhead.

The difficult part lies in `addValue`. `Clash.Prelude.toList` converts the vector to a list,
but if the vector is not fully defined, this can cause problems. Therefore, we define a folding
function that checks that the list can be safely converted to WHNF. Each `addValue x` creates a function
to add one value to the lookup table of `a`, and these are combined with `.`.



### OPTION 2: SPLIT THE WAVEFORM CLASS
A different method it to split up implementations per constructor by creating a
an extra class that selects the constructor at the type level. An example of this is
`RTree`, which has the constructors `???` and `Leaf`. At the type level, _we_ already know
which one of these constructors is going to be used, but the compiler does not.

First of all, let's look at all the methods we need to overwrite:
- `translate`
- `addSubtypes`
- `addValue`
- `hasLUT`

Now we can create a helper class `WaveformRTree` which takes care of the different constructors
of `RTree`, and select the implementation based on a type-level check (`RTreeIsLeaf`).

```hs
type family RTreeIsLeaf d where
  RTreeIsLeaf 0 = True
  RTreeIsLeaf d = False

instance (Waveform a, KnownNat d, WaveformRTree (RTreeIsLeaf d) d a)
  => Waveform (RTree d a) where
  translator = translatorRTree @(RTreeIsLeaf d) @d @a
  addSubtypes =
    if hasLUT @a then
      addSubtypesRTree @(RTreeIsLeaf d) @d @a
    else id
  addValue = addValueRTree @(RTreeIsLeaf d) @d @a
  hasLUT = hasLUT @a

class WaveformRTree (isLeaf::Bool) d a where
  translatorRTree :: Translator
  addValueRTree :: RTree d a -> LUTMap -> LUTMap
  addSubtypesRTree :: TypeMap -> TypeMap
```

In this case, `hasLUT` does not depend on the subtree type - we can get it from `a` directly.
This means we don't need to include it in our helper class.

Now we can write the instances for leaves and branches. Let's start with the leaves,
and make them invisible (i.e. a leaf is rendered as it's containing data).

```hs
instance (Waveform a) => WaveformRTree True 0 a where
  translatorRTree = tRef (Proxy @a) -- translate as `a`
  addValueRTree t = case safeWHNF t of -- make the function undefined-proof!!!
    Just (RLeaf x) -> addValue x
    Nothing        -> id
    _              -> undefined
  addSubtypesRTree = addTypes @a
```

> **Important**: Make sure that `addValue` (`addValueRTree`) does not error when encountering `undefined` values!

The branch translator is slightly more complex. Since `RTree` is naturally rendered like
`<<a,b>,<c,d>>` we'll just keep to that structure using a product translator:

```hs
instance (Waveform (RTree d1 a), Waveform a, d ~ d1 + 1, KnownNat d, KnownNat d1)
  => WaveformRTree False d a where
  translatorRTree = Translator (bitsize $ Proxy @(RTree d a)) $ TProduct
      { start = "<"
      , sep = ","
      , stop = ">"
      , labels = []
      , preci = -1
      , preco = 11
      , subs = [("left",tsub),("right",tsub)]
      , style = -1
      }
    where tsub = tRef (Proxy @(RTree d1 a))
  addValueRTree t = case safeWHNF t of -- make the function undefined-proof!
    Just (RBranch x y) -> addValue (x:: RTree d1 a) . addValue y
    Nothing            -> id
    _                  -> undefined -- leaf nodes do not exist here
  addSubtypesRTree = addTypes @(RTree d1 a)
```

> Note: it would be a bit more efficient to use `TArray` here; however, we would not be able to
> assign the subsignal names `"left"` and `"right"`.


And just like that, we can translate our GADT type!