## How to implement Waveform for constant value types

TODO

Sometimes, it's easiest to just assign a constant value to a signal. This might be the case if you have a unit type, or
if actual translation is both difficult and not really necessary.

It is very simple to add a constant translator. Just create a `TConst` translator with the translation;
any subsignal hierarchy will be inferred automatically:

```hs
myTranslator = TConst <translation>
```

However, it is still a bit tedious to implement full `Waveform` instances this way.
Instead, there is a much easier method:
the `WaveformConst` class.

Say you only need a single signal, like `()`. In that case, you only need to provide the `Render` value by overwriting `constRen`:


```hs
instance WaveformConst () where
  constRen = Just ("()",WSNormal,11)
```

Should you need a full translation, you should overwrite `constTrans` instead. The subsignal structure is automatically inferred from
this translation:

```hs
instance WaveformConst MyType where
  constTrans = Translation Nothing [("subsignal",Translation Nothing [])]
```


