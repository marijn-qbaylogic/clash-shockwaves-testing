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

The most important function of `Waveform` is `translator`: this specifies the `Translator`
that the waveform viewer should use to unpack and translate the binary values in the VCD
file. We will discussed it later, but first, let's make sure the translator can be used.

The translator is registered by `addTypes`. By default, this function adds the type
itself to the type map, and then calls `addSubtypes`, and does not need modification.
`addSubtypes` then has to register all subtypes in the data type. This is fairly simple.
For example, given:
```hs
data MyType c = P A | Q B c
```
the implementation looks like:
```hs
addSubtypes = addSubtypes @A . addSubtypes @B . addSubtypes @c
```

Now we can implement `translator`, which needs to produce a `Translator` value.
A `Translator` consists of two values: a width, denoting the number of bits the translator
is expected to receive, and a a `TranslatorVariant` that actually describes how these bits
are to be interpreted. There are many variants to choose from which are discussed below.



#### Constant values
If you have a constant value (such as a unit type or a constructor without fields), use
`TConst`. This is simply a constant translation value.

```hs
Translator 0 $ TConst $ Translation (Just (TODO,TODO,TODO)) [("sub",Translation (Just (TODO,TODO,TODO)) [])]
```

#### Sum types
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


#### Product types
Next, if a constructor has fields (a product type), you can use `TProduct`.
This translator has a lot of options.

```hs
translator = Translator width $ TProduct
  { subs
  , start, sep, stop,
  , labels
  , preci, preco
  , style
  }
```

`subs` is a list of translators. If no subsignal name is provided, they will be used,
and will consume bits, but will not be shown as a subsignal.

`start`, `sep` and `stop` are used to attach subsignal values together. If `labels`
is not empty, the values will be inserted before each subtranslator value - this is
often used for record-like data.
```
<start> <sub[0]> <sep> <sub[1]> <stop>
<start> <labels[0]> <sub[0]> <sep> <labels[1]> <sub[1]> <stop>
```
For example, a Haskell list would have `start="[",sep=",",stop="]",labels=[]` while
a record might look like `start="MyRecord{",sep=",",stop="}",labels=["field1=","field2=]`.

`preci` and `preco` denote the inner and outer operator precedence respectively. The precedence of the
subvalue translation is compared to the inner prescedence, and if this inner precedence is equal or higher,
the subvalue is wrapped in parentheses. The outer precedence is used for the final translation.
If your data type is joined using spaces (as is often the case), set both to `10`.
For custom operators, just use the operator precedence for both.
If values never need parentheses, use `preci=0` and `preco=11`.
In a case like `fromList [<sub[0]>,<sub[1]>]` you'd want to use `preco=10` (because the value is joined by a space)
but `preci=0` (since the list syntax isolates the subvalues, so they never need parentheses).


`style` is the index of the translator whose style should be copied. Setting this
to `-1` will make the value have the default style. This is mostly useful for wrapper types
- by default, types that look like newtypes will copy the style of their subvalue so that
these styles become visible at a higher level in the signal hierarchy.


#### Arrays
If your datatype looks more like a homogeneous list, you'll likely want to use `TArray`
instead. It is basically a reduced version of `TProduct` that has a single subtype
and numbered subsignals.

```hs
translator = Translator width $ TArray
  { sub, len
  , start, sep, stop
  , preci, preco
  }
```

Here `sub` is a single translator, and `len` is the number of elements in the array.
The other fields all have the same functionality as they have in `TProduct`.

#### Other types
If you have a subvalue that needs to be translated, do not include its full translator,
but include a reference to the type using `TRef`. Instead of manually creating this value,
use the `tRef` function to create the full translator:
```hs
translator = tRef (Proxy @MyType)
```

#### Numbers
`TNumber` is used to translate integer types. It is used as `TNumber{format,spacer}`,
and more info about its behaviour can be found [here](NUMBER.md).

#### Lookup tables
`TLut` is used for LUT based types, and unless you _really_ know what you are doing,
you should not touch this translator.

#### Creating custom translator configurations
Although the translators must still process the bits correctly, a lot of
configuration is possible. For example, `Maybe a` would by default get a translator
like this:

```hs
translator = Translator (width @(Maybe a)) $ TSum
  [ tDup "Nothing" $ Translator 0 $ TConst $ Translation (Just ("Nothing",_,11)) []
  , tDup "Just" $ _ $ TProduct
      { subs = [(Just "0", tRef (Proxy @a))] 
      , start = "Just ", sep = "", stop = "", labels = []
      , preci = 10, preco = 10
      , style = 0 }]
```

Corresponding to the following structure:
```
signal     |{ Nothing }{ Just True }
|- Nothing |{ Nothing }
+- Just    |           { Just True }
   +- 0    |           { True      }
```

But instead, to reduce unnecessary subsignal clutter, it looks like this:

```hs
translator = Translator _ $ TSum
  [ Translator 0 $ TConst $ Translation (Just ("Nothing","$maybe_nothing",11)) []
  , Translator (width @a) $ TProduct
      { subs = [(Just "Just.0", tRef (Proxy @a))] 
      , start = "Just ", sep = "", stop = "", labels = []
      , preci = 10, preco = 10
      , style = 0 }]
```

This shows up as:
```
signal    |{ Nothing }{ Just True }
+- Just.0 |           { True      }
```

In general, the translators `TDuplicate` and `TStyled` can be inserted or removed
freely, since they do not influence how bits are interpreted, as can all styles and
precedence and text values.

### TRANSLATION

> **Note:** the translation function is used for LUTs. If you are sure your data
> type will not be used inside some other data type that is translated using LUTs,
> it is fine to leave it undefined.
> ```
> translate' = undefined
> ```
> However, if you are writing code that may be used by others, please consider
> adding the full implementation.

TODO - actually, why not make it use pack instead? would suffice in most situations. only LUTs need an actual translation, and those are also safer with (unpack . pack)



An important tool in translation is `translateFromSubs`. This function takes a translator and a list of
translated subsignals, and produces the resulting translation. The exact behaviour is as follows:

- `TSum`, `TProduct` and `TArray` take the subsignal translations as the subsignal translations.
  Note that any subsignal names are ignored.
- `TConst` ignores any subsignal translations and just returns its constant value.
- `TRef`, `TLut` and `TNumber` cannot create a translation themselves.
  The input list must be a single subsignal with name `""`, and is assumed to be the
  translation of the translator itself.
- `TStyled` and `TDuplicate` are a kind of 'wrapper' translators. Instead of using the provided
  translations directly, they call `translateFromSubs` on their subtranslator and then process
  the result.

Let us look at an example data type:

```hs
data P = A | B Int Int
```

with a translator structure that looks like:

```
1) TSum
2)   TDuplicate
3)     TConst "A"
4)   TDuplicate
5)     TProduct "B"
6)       TRef Int
7)       TRef Int
```

you can render the translation value of 6 and 7 by providing their translations (which changes nothing),
4 and 5 by providing the translations of 6 and 7,
2 and 3 by providing nothing,
and 1 by providing the translation of 2 or 4.

Behind the scenes, this function is mostly used to render `TSum` and `TProduct` without having to deal with
the prescence of `TDuplicate` and `TStyled`.

> Note that `translateFromSubs` is currently in `Clash.Shockwaves.Internal.Translator`, which is subject to change.


### LUT CREATION

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

To avoid splitting values unnecessarily, if it is unknown whether subvalues use luts,
`addValue` is usually implemented as:
```hs
addValue x = if hasLUT @(MyType a b) then ... else id
```


That's it! For some specific purposes of creating custom `Waveform` instances, see:
- [How to implement Waveform for GADTs](GADTS.md)
- [How to add extra information to your data types](EXTRA_INFO.md)