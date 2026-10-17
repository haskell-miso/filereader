:ramen: 📁 miso-filereader
====================

A [FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader) example. Pick or drop a text file and read it in the browser; nothing is uploaded. See live [here](https://file-reader.haskell-miso.org).

> [!TIP]
> This requires installing [nix](https://nixos.org) with [Nix Flakes](https://wiki.nixos.org/wiki/Flakes) enabled.
> Although not required, we recommend using [miso's binary cache](https://github.com/dmjio/miso?tab=readme-ov-file#binary-cache).

### Example

```haskell
updateModel :: Action -> Effect context props Model Action
updateModel = \case
  ReadFile input -> do
    dragging .= False
    withSink $ \sink -> do
      -- Index the FileList directly: 'files' expects a real Array, and a
      -- FileList is only array-like
      file <- (!! 0) =<< input ! "files"
      noFile <- isUndefined file
      unless noFile $ do
        info <- fileInfo file
        case checkFile info of
          Just err -> sink (ReadFailed err)
          Nothing -> do
            sink (StartRead info)
            reader <- newFileReader
            setField reader "onload" =<< asyncCallback (do
              result <- fromJSValUnchecked =<< reader ! "result"
              sink (SetContent info result))
            setField reader "onerror" =<< asyncCallback
              (sink (ReadFailed ("Could not read " <> fileName info <> ".")))
            void $ reader # "readAsText" $ [file]
      -- Clear the input so picking the same file again fires "change"
      setValue input ""
```

The file input covers the whole drop zone, so a dropped file lands in the input and fires `change` just like a picked one; no drop handling code is needed. Styles are written in Haskell with `Miso.CSS`.

### Development

Call `nix develop` to enter a shell with [GHC](https://haskell.org/ghc)

```bash
$ nix develop
```

### Build (Web Assembly)

```bash
$ nix develop .#wasm --command bash -c "make"
```

### Serve

```bash
$ nix develop .#wasm --command bash -c "make serve"
```

### Clean

```bash
$ nix develop .#wasm --command bash -c "make clean"
```

This comes with a GitHub action that builds and auto hosts the example.
