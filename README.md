# Lisper [![Build Status](https://travis-ci.org/jaseemabid/lisper.svg?branch=master)](https://travis-ci.org/jaseemabid/lisper)

A tiny *WIP*
[Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language))
implementation in Haskell.

## Getting started

    $ brew install haskell-stack
    $ gem install watson-ruby (optional)
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

1. [The scheme programming language](http://groups.csail.mit.edu/mac/projects/scheme/)
1. [Revised 5 report on the algorithmic language scheme](./docs/r5rs.pdf)
1. [(welcome '(schemers . org))](http://www.schemers.org)
1. [The Scheme Programming Language (book)](http://www.scheme.com/tspl4/)

--

1. [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/])
1. [Wikibook: Write yourself a scheme in 48 hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
1. [A bunch of related projects](https://github.com/haskell-lisp)


## Contributing

See reported issues, the Issues.txt file or run `$ watson` to look for minor
issues in code. Fix failing test cases if any.

## License

MIT licensed.
