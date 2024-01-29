# cabal-gild

## Synopsis

```sh
$ cabal install cabal-gild
$ ...
$ cabal-gild --inplace example.cabal
```

## Output

Turns this...

```cabal
cabal-version: 2.4
name: cabal-gild
version: 0

-- An example formatter
executable cabal-gild
    default-language: Haskell2010
    hs-source-dirs: src
    main-is: CabalGild.hs
    -- build depends will be in
    -- a nice tabular format
    build-depends: base >=4.11 && <4.13, pretty >=1.1.3.6 && <1.2, bytestring, Cabal ^>=2.5, containers ^>=0.5.11.0 || ^>=0.6.0.1
    -- extensions will be sorted
    other-extensions:
      DeriveFunctor FlexibleContexts ExistentialQuantification OverloadedStrings
      RankNTypes
```

...into this:

```cabal
cabal-version: 2.4
name:          cabal-gild
version:       0

-- An example formatter
executable cabal-gild
  default-language: Haskell2010
  hs-source-dirs:   src
  main-is:          CabalGild.hs

  -- build depends will be in
  -- a nice tabular format
  build-depends:
    , base        >=4.11      && <4.13
    , bytestring
    , Cabal       ^>=2.5
    , containers  ^>=0.5.11.0 || ^>=0.6.0.1
    , pretty      >=1.1.3.6   && <1.2

  -- extensions will be sorted
  other-extensions:
    DeriveFunctor
    ExistentialQuantification
    FlexibleContexts
    OverloadedStrings
    RankNTypes
```
