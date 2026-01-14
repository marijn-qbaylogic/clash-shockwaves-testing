



This is an attempt at a flowchart decision tree for creating `Waveform` instances.


? Simple translation (sum, product, subtypes with `Waveform`)?
Y:  Derive `Waveform` / create `Waveform` instance (see [this](WAVEFORM.md))
    ? Standard structure ok?
    Y:  Keep most defaults
        ? Do you want custom styles per constructor?
        Y: Overwrite `styles` (see [this](STYLES.md))
        N: Don't
    N:  Overwrite `translator` with a custom implementation that matches the `BitPack` representation.
        Use functions like `tRef`, `tConst`, `tStyled`, `tDup` to make your life easier.
        Use `addValueG` and `hasLUTG` from `WaveformG`
N:  ? Is the type anteger?
    Y:  Use `WaveformForNum` (see [this](NUMBERS.md))
    N:  ? Constant?
        Y:  Use `WaveformConst` (see [this](CONSTANT.md))
        N:  Use WaveformLUT (see [this](LUTS.md))
            ? Only minor change to text value?
            Y:  Standard derive, overwrite `show`
            N:  ? Simple atomic?
                Y:  Use one of the atomic functions
                    ? Is `show` good enough?
                    Y:  Use `displayAtom`
                    N:  Use `displayAtomWith` with a custom display function
                N:  Use `displaySplit`
                    ? Standard `show`, `WSNormal`, standard precedence?
                        Y:  Use `displayShow`
                        N:  Use `displayWith` with a custom function
                    ? Custom style?
                        Y:  Use a custom function (see [this](STYLES.md))
                        N:  Use `const WSNormal`
                    ? Custom operator precedence?
                        Y:  Use a custom function for the precedence, like `const 11`
                        N:  Use `precL`
                    ? Standard subsignals?
                        Y:  Use `splitG`
                        N:  Use custom implementation and overwrite `structureL` accordingly.
                            Use `translateFieldsG` from `WaveformG` if possible to make life easier.
                            