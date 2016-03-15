# Lisper [![Build Status](https://travis-ci.org/jaseemabid/lisper.svg?branch=master)](https://travis-ci.org/jaseemabid/lisper)

A tiny *WIP*
[Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language))
implementation in Haskell.

## Getting started

    $ brew install haskell-stack
    $ git clone https://github.com/jaseemabid/lisper && cd lisper
    $ stack build
    $ stack test
    $ stack install

Stack installs a binary called `lisper` into $PATH.


## REPL

Lisper has a simple shell.

    $ lisper
    λ (cons 1 '(2 3 4))
    (1 2 3 4)
    λ (define (add a b) (+ a b))
    <λ add >
    λ (add 10 20)
    30


## Resources

1. [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/])
1. [Wikibook: Write yourself a scheme in 48 hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
1. https://github.com/haskell-lisp


## Contributing

Install command line issue manager
[Watson](https://github.com/nhmood/watson-ruby)

    $ watson

See reported issues as well as Issues.txt


## License

MIT licensed.
