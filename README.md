# Lisper [![Build Status](https://travis-ci.org/jaseemabid/lisper.svg?branch=master)](https://travis-ci.org/jaseemabid/lisper)

A tiny *WIP* Scheme implementation in Haskell.

## Resources

1. [http://dev.stephendiehl.com/hask/](What I Wish I Knew When Learning Haskell)
1. [http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours](Wikibook: Write yourself a scheme in 48 hours)
1. https://github.com/haskell-lisp

## Getting started

    $ brew install haskell-stack
    $ git clone https://github.com/jaseemabid/lisper && cd lisper
    $ stack build
    $ stack test

## Evaluating examples with REPL

Top level function exec can be used from ghci

    $ stack repl
    λ exec "(+ 1 2)"
    3
    λ

A rudimentary lisper shell that barely works is also available.

    $ stack exec -- lisper -i
    λ (cons 1 '(2 3 4))
    (1 2 3 4)
    λ

## License

MIT licensed.
