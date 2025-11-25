## How to write a custom Waveform instance
There are a number of times where you might have to write a custom instance of
`Waveform`. This guide will explain which functions need to be implemented,
and how to do so.

First, let's go through the functions and see what we need to do:

- `translator` is the most important function, as it produces the data that is used
   to determine how bits in the VCD file get translated. `addTypes` and `addSubtypes`
   are used to register these values. This will be covered first.
- `translate` and `translate'` are for performing translations inside Haskell.
  This is covered in the second section.
- `hasLUT` and `addValue` exist to make LUT-based translations possible and are covered
in the last section.
- `width` is the bitwidth as reported by `BitSize` and should not be changed.
- Similarly, `typeName` does not need to be changed, unless you are writing an
  instance meant to be used as a `derive via`. In that case, use:
  ```hs
  instance Waveform (WaveformForX a) where
    typeName = typeNameP (Proxy @a)
  ```
- `styles` and `styles'` are for adding constructor styles, and will likely go
  unused in a custom instance. `styles` is meant to be overwritten, while `styles'`
  fills in any missing values and is meant to be left untouched.



### TRANSLATOR

TODO


If you have a constant value (such as a unit type or a constructor without fields), use
`TConst`. This is simply a constant translation value.

```hs
Translator 0 $ TConst $ Translation (Just (TODO,TODO,TODO)) [("sub",Translation (Just (TODO,TODO,TODO)) [])]
```

If you have multiple constructors (a sum type), use `TSum`. This will by default not create
any subsignals, so you'll likely want to use `TDuplicate` for all subtranslators.
```hs
translator = Translator width $ TSum
  [ Translator width1 $ TDuplicate "Constr1" $ translatorForConstr1 
  , Translator width2 $ TDuplicate "Constr2" $ translatorForConstr2
  ]
```

Instead of creating the `TDuplicate` translator manually, you can also use the
`tDup` function:

```hs
translator = Translator width $ TSum
  [ tDup "Constr1" $ translatorForConstr1 
  , tDup "Constr2" $ translatorForConstr2
  ]
```

> **Note:** The sum translator will consume $clog(|translators|)$ bits.

Next, if a constructor has fields (a product type), you can use `TProduct`.
This translator has a lot of options.

```hs
translator = Translator width $ TProduct
  { TODO 
  , TODO
  }
```

If your datatype looks more like a homogeneous list, you'll likely want to use `TArray`
instead. It is basically a reduced version of `TProduct` that has a single subtype
and numbered subsignals.

TODO

If you need to reference a different translator, you can use `TRef`. Instead of
manually creating this value, use the `tRef` function to create the full translator:
```hs
translator = tRef (Proxy @MyType)
```

`TNumber` is used to translate integer types. It is used as `TNumber{format,spacer}`,
and more info about its behaviour can be found in [TODO:link].

`TLut` is used for LUT based types, and unless you _really_ know what you are doing,
you should not touch this translator.


Although the translators must still process the bits correctly, a lot of
configuration is possible. For example, `Maybe a` would by default get a translator
like this:

```hs
translator = Translator _ $ TSum
  [ tDup "Nothing" $ Translator 0 $ TConst $ Translation (Just ("Nothing",_,_)) []
  , tDup "Just" $ Translator _ $ TProduct
      { start = "Just "
      , subs = [(Just "0", tRef (Proxy @a))]
      , ... }]
```

But instead, to reduce unnecessary subsignal clutter, it looks like this:

```hs
translator = Translator _ $ TSum
  [ Translator 0 $ TConst $ Translation (Just ("Nothing",_,_)) []
  , Translator _ $ TProduct
      { start = "Just "
      , subs = [(Just "Just.0", tRef (Proxy @a))]
      , ... }]
```




### TRANSLATION

> **Note:** the translation function is used for LUTs. If you are sure your data
> type will not be used inside some other data type that is translated using LUTs,
> it is fine to leave it undefined.
> ```
> translate' = undefined
> ```
> However, if you are writing code that may be used by others, please consider
> adding the full implementation.

TODO



### LUTS

`hasLUT` indicates whether there are any data types inside that need to be translated
and added to a LUT.
`addValue` is responsible for actually translating and storing these values.

If your data type has no subtypes, or if you already know these do not use LUTs,
the implementation is simple:
```hs
hasLUT = False
```

If not, add it for all subtypes:
```hs
hasLUT = hasLUT @(SubType1) || hasLUT @(SubType2)
```

If your implementation does not differ too much from the default `Waveform`, you
can use the `WaveformG` function `hasLutsG` to `or` the `hasLuts` of all subtypes.

Finally, you need to appropriately implement `addValue`. This is a function that,
given a value of your data type, returns a function that modifies the LUT table with
all subvalues using LUTs. This means that if your data type has no LUTs, the implementation
is really simple:

```hs
addValue _ = id
```

If there are subvalues, things get harder. Most importantly, the function *must*
be properly defined for `undefined` values. This means you cannot directly evaluate
based on the constructor! For example:

```hs
import Clash.Shockwaves.Internal.Util (safeWHNF)

addValue x = case safeWHNF x of
  Just (Cons1 a b) -> addValue a . addValue b
  Just (Cons2)     -> id
  Nothing          -> id
```

Here, `safeWHNF` will try to evaluate its argument to WHNF. If the value is `undefined`,
it returns `Nothing` and we return the identity function.

Keep in mind that when the structure is known even for `undefined` values (as is the case
when there is only one constructor), the function should simply not evaluate the value
_at all_ and instead pass on the undefinedness to the subvalues, in the same way
this is done for `translate'`.

```hs
addValue x = addValue (getA x) . addValue (getB x)
  where
    getA (Cons a _b) = a
    getB (Cons _a b) = b
-- Note: `addValue (Cons a b) = addValue a . addValue b` fails for `undefined`
```

