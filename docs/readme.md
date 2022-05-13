# Readme

Converting tenhou paifu (XML) to readable json.

## Install

This project uses C++ or Haskell.

#### To use the C++ version:

```sh
$ cmake .
$ make
```

#### To use the Haskell version:

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
$ main <[Input File] >[Output File]
```

## Format
