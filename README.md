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
    $ stack install

## REPL

Lisper has a simple shell.

    λ (cons 1 '(2 3 4))
    (1 2 3 4)
    λ (define (add a b) (+ a b))
    <λ add >
    λ (define (add a b) (+ a b)) (add 10 20)
    30

## Contributing

Install command line issue manager
[https://github.com/nhmood/watson-ruby](Watson)

    $ watson

See reported issues as well as Issues.txt

## License

MIT licensed.
