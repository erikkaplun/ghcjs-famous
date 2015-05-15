Introduction
------------

This code base is a work in progress. Any comments, suggestions and proposals for changes are
welcome as this is the first ever Haskell project for the author.

Example code
------------

See `src/JavaScript/Famous/Examples.hs`.

Setup
-----

```
$ cabal update && cabal install cabal-install
$ cabal sandbox init
$ cabal install --dependencies-only
$ cabal build
$ open html/famous-test.html
```
