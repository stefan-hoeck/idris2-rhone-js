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
be found [here](src/Examples/Main.idr).

## Dependencies

This project makes use of several other Idris2 projects:

  * [idris2-elab-util](https://github.com/stefan-hoeck/idris2-elab-util)
  * [idris2-sop](https://github.com/stefan-hoeck/idris2-sop)
  * [idris2-dom](https://github.com/stefan-hoeck/idris2-dom)
  * [idris2-tailrec](https://github.com/stefan-hoeck/idris2-tailrec)
  * [idris2-rhone](https://github.com/stefan-hoeck/idris2-rhone)

It is being developed against the current main branch of Idris2.
The latest commit has been built against Idris 2, version 0.5.1-3536f8dab.

## Building the Example Page

After downloading and installing the dependencies listed above,
you can build the example page with `make page` and have a look at
it by loading `rhone.html` into your browser.
