# Readme

Converting tenhou paifu (XML) to readable json.

## Install

This project uses Haskell.

The haskell version need Regex.pcre library. You can install it by

```sh
$ cabal install regex-pcre-builtin
```

Then just

```sh
$ ghc -O2 --make main.hs
```

## Usage

```sh
$ ./main <[Input File] >[Output File]
```
