# idris2-rhone-js : Interactive Single Page Web Applications in Idris2

This is an experimental library about putting a nice API on top
of [idris2-dom](https://github.com/stefan-hoeck/idris2-dom).
For declaring the interactions between different parts of
a web page, [idris2-rhone](https://github.com/stefan-hoeck/idris2-rhone),
a small arrowized FRP (functional reactive programming) library
in the spirit of [dunai](https://hackage.haskell.org/package/dunai)
is used.

This is still very much work in progress and *a lot* of stuff
is still missing, but a small and very simple example web page
is already available. A tutorial is in the making and can
be found [here](docs/src/Examples/Main.md).

## Dependencies

This project makes use of several other Idris2 projects:

* [idris2-elab-util](https://github.com/stefan-hoeck/idris2-elab-util)
* [idris2-sop](https://github.com/stefan-hoeck/idris2-sop)
* [idris2-dom](https://github.com/stefan-hoeck/idris2-dom)
* [idris2-tailrec](https://github.com/stefan-hoeck/idris2-tailrec)
* [idris2-rhone](https://github.com/stefan-hoeck/idris2-rhone)

The latest commit is daily tested to build against the current
HEAD of the Idris compiler. Since Idris2 releases are happening
rather infrequently at the moment, it is suggested to use
a package manager like [pack](https://github.com/stefan-hoeck/idris2-pack)
to install and maintain matching versions of the Idris compiler
and this library. Pack will also automatically install all
required dependencies.

## Building the Example Page

After downloading and installing the dependencies listed above,
you can build the example page with `make page` and have a look at
it by loading `rhone.html` into your browser.
