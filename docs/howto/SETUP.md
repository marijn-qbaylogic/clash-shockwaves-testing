## How to set up Shockwaves

To start using shockwaves, you need three things:
- the latest version of [Surfer](https://gitlab.com/surfer-project/surfer)
- the Shockwaves Surfer extension (this repo, in `/surfer_shockwaves/`)
- the `Clash.Shockwaves` Haskell library (this repo, in `/shockwaves/`)

Install Surfer (see [TODO:link]) and add the Shockwaves extension (instructions below).
If the extension is installed correctly, it should print messages to the output.

Add the repository `clash-lang/clash-shockwaves` to your project and configure it to use subdirectory `shockwaves` [TODO:how].

### GETTING AND INSTALLING THE SURFER EXTENSION

To use Shockwaves, you need the Shockwaves translator extension WASM file.
Either obtain a precompiled copy, or run the `compile.sh` script in `surfer_shockwaves/`.
You can supply the script with the options `linux` or `windows` to automatically copy the
translator to the right directory.

If you need to manually install the WASM file, look at the output of Surfer.
It should contain a message like this, indicating the right folder. Alternatively, use the Shockwaves documentation.

```
INFO libsurfer::translation::wasm_translator: Looking for translators in /home/Me/.local/share/surfer/translators
```

If the translator is found, the output should look like this:
```
INFO libsurfer::translation::wasm_translator: Found /home/Me/.local/share/surfer/translators/surfer_shockwaves.wasm
```

> Note: some of the logging statements may be changed by the Surfer devs!