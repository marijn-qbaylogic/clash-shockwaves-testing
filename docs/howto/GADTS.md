
## How to implement Waveform for GADTs
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

TODO

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
  ... TODO
```

We can then write instances for the different constructors:

```hs
instance (Waveform (RTree (n-1) a)) => WaveformForRTree False n a where
  ... TODO
```

Note how we could not have added the `Waveform` bound for a smaller `n` for the leaf node!

```hs
instance (Waveform a) => WaveformForRTree True n a where
  ... TODO
```

Now all that's left is to call the right functions from our `Waveform` instance!

```hs
instance (WaveformForRTree (n==0) n a) => Waveform (RTree n a)
  ... TODO @(WaveformForRTree (n==0) n a)
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
