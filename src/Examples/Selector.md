## The Basic Layout of a rhone-js Web Page

This module defines the HTML structure of the main page and
implements the functionality of the `select` element that is
used to choose and load one of the example applications.

First some imports:

```idris
module Examples.Selector

import Examples.CSS
import Examples.Balls
import Examples.Fractals
import Examples.Performance
import Examples.Reset
import Rhone.JS

%default total
```

This imports all example applications plus module `Examples.CSS`, where
most of the CSS rules for the page are defined. In addition, `Rhone.JS`
is imported, the kitchen sink re-exporting the core functionality necessary
to write a rhone-js wep application.

### The Effect Type: `MonadDom`

Most tutorials about writing functional web applications that I have come
across so far show nice examples of increasing complexity but leave out the
gory details about the actual implementation. I don't like this approach. In
order to truly make use of a web framework, you will eventually need to
know a bit about its implementation and hence its limitations.

I therefore start with this: A short explanation of what is going on under the
hood and where to start looking for further information.

```idris
public export
MSel : Type -> Type
MSel = DomIO String JSIO
```

I typically start the examples with an alias for the effect type being
used, and we will use `DomIO` from module `Control.Monad.Dom.DomIO`
most of the time. `DomIO` is the reference
implementation of interface `MonadDom` from `Control.Monad.Dom.Interface`.
This is an IO monad, which right now provides only two additional
pieces of functionality:
A way to generate unique id strings, which are used to look up
interactive components in the DOM, and a function to register event
handlers.

`MonadDom` and `DomIO` are parameterized over the event type they
understand, and in case of the select widget we define in
this module, we just use `String`
as our event type. In addition, `DomIO` is parameterized over the effect
type it uses internally. This will be just `JSIO` most of the
time. When you look at the implementation of `DomIO` you'll see
that it's structure is very simple: It's just a reader monad in disguise,
taking a value of type `DomEnv ev` as input, and `DomEnv ev` is a record
holding a mutable reference for creating unique integers plus
a function for handling UI events.

### Writing HTML in Idris2

Module `Text.Html` and its submodules provide a small DSL for
declaring HTML nodes and their attributes. These are pure
Idris data types and can be used to write and render
properly formatted HTML on any backend.

Here is the layout of the main page:

```idris
content : Node String
content =
  div [ class contentList ]
      [ div [class pageTitle] ["rhone-js: Examples"]
      , div [class contentHeader]
          [ label [class widgetLabel] ["Choose an Example"]
          , select
              [ classes [widget, selectIn, exampleSelector], onChange id]
              [ option [ value "reset", selected True ] ["Counting Clicks"]
              , option [ value "performance" ] ["Performance"]
              , option [ value "fractals" ] ["Fractals"]
              , option [ value "balls" ] ["Bouncing Balls"]
              ]
          ]
      , div [id exampleDiv.id, class widgetList] []
      ]
```

A typical node constructor like `div` or `label` takes
two arguments: A list of attributes and a list of child
nodes.

Several things need some quick explanation: CSS classes like
`pageTitle` or `contentHeader` are just `String`s defined in
module `Examples.CSS` together with the corresponding CSS rules.
(If you are new to web development: [CSS](https://developer.mozilla.org/en-US/docs/Web/CSS)
is a domain specific
language used to describe the presentation of documents written
in HTML and similar markup languages. It is used to define the
appearance of the example web page.)

DOM identifiers like `exampleDiv` are of type `ElementRef t` (defined
in `Control.Monad.Dom.Interface`), where `t` is the type of the
corresponding HTML element. They are mainly typed wrappers around ID
strings and are used to lookup HTML elements in the DOM.
We need these, whenever an element in the DOM is not static.
We can either define these manually, or let the runtime generate
a unique identifier for us by invoking the `MonadDom` function
`uniqueId`. In the example above, `exampleDiv` points to the DOM element
where the content of the example applications will go. It's the
only part of the main web page that is not static.

Finally, we also encode the events an element fires in the `Node`
type, and that's what `Node`'s parameter stands for. Events are
just attributes, and in the example above, the `select` element
fires an event whenever the user changes the selected value
(`onChange id`).

### The Interactive Part: Monadic Stream Functions

Now that we have the structure of our web page specified, we
can have a quick look at how we define its interactive behavior.

rhone-js is named after [idris2-rhone](https://github.com/stefan-hoeck/idris2-rhone)
a port of monadic stream functions (MSF) first introduced
in Haskell's [dunai](https://hackage.haskell.org/package/dunai)
library and explained in detail in a nice
[article](https://www.cs.nott.ac.uk/~psxip1/#FRPRefactored).
This is probably the most accessible implementation of (arrowized)
functional reactive programming I have so far come across.

In its most general form, an MSF can be thought of
as having the following type:

```haskell
data MSF m i o = MSF (i -> m (o, MSF m i o))
```

This is an effectful computation over a monad type `m` converting
an input value of type `i` to an output value of type `o` plus
a new MSF (the original MSF's continuation), which will be used
to convert the next piece of input.

In the *rhone* library, we don't use this most general form,
as it does not go well with the totality checker. Instead,
our implementation of `MSF` encodes a rather large set
of primitive operations in the data type itself. This gives
us the benefit of provably total stream functions, which is
extremely valuable especially when we start using advanced
components like event switches.

`MSF` implements interfaces `Functor` and `Applicative` but also
`Category` and `Arrow`. It lets us lift any effectful function
into an `MSF` value via function `arrM` (used in the code below),
supports feedback loops for stateful computations and several
kinds of event switches to dynamically change the behavior
of a web page. The ability to lift arbitrary effectful computations
is especially useful, as it allows us to update the DOM
directly from within an MSF.

Enough talk, here's the code:

```idris
msf : MSF MSel String ()
msf = feedback (pure ())
    $ par [arrM liftJSIO, arrM select] >>^ (\[_,cl] => [cl,()])
  where select : String -> MSel (JSIO ())
        select "reset"       = reactimateInDomIni (const 0) Reset.ui
        select "performance" = reactimateInDom Performance.ui
        select "fractals"    = reactimateInDom Fractals.ui
        select "balls"       = reactimateInDom Balls.ui
        select _             = pure (pure ())

export
ui : MSel (MSF MSel String (), JSIO ())
ui = do
  rawInnerHtmlAt appStyle allRules
  innerHtmlAt contentDiv content
  pure (msf, pure ())
```

I'll quickliy break this down a bit: The first line,
`innerHtmlAt contentDiv content`, is where half of the magic
happens: We change the inner HTML of the element with ID
`"content"` (the string wrapped up in `contentDiv`) to the
result of rendering `content` (the HTML `Node` we defined above).
But before doing so, every element that can fire an event is
given its own unique ID (if it doesn't already have one) and
after adding these updated elements to the DOM, theses IDs are
looked up and the necessary event handlers are registered
at these elements. So the procedure is as follows: Add IDs where
necessary, render to text form, replace inner HTML of `contentDiv`
with the rendered text, lookup reactive components by their
IDs and register the event handlers.

Afterwards, the monadic stream function is created: We
use `arrM` to lift an effectful computation to the MSF context.
This is just a pattern match on our event type (`String`), which consists
of the values fired by the select element. If the value is one we
know about, we start the corresponding user interface, again
by invoking the mysterious `reactimateInDom` function. This
is where the other half of the magic happens, but that's for
another post.

### Comparison with other MVC Libraries

Frameworks for writing interactive web applications often
use the term *MVC* (model, view, controller), where
*model* refers to the underlying data model (or state)
of the web page, *view* is the visualization of the model
as a collection of DOM elements, and *controller* is the part where
the logic of the web application lies: It is responsible
for updating the model (state) due to user interactions
and refresh the web page accordingly.

Several MVC implementations,
for instance the one used by the *Elm* programming language,
make use of a *virtual DOM*. This is an in-memory model of
the real DOM used by the browser, and this is the *view*
that is being manipulated in Elm applications. On each
update, the virtual DOM is compared to its previous state
(a process called *DOM diffing*) and the real DOM is updated
to reflect the changes made to the virtual DOM. The advantage of this
approach is that we can write a single (pure!) function for converting
the model to the view and never have to interact with
the real DOM explicitly. The downside is, that we loose some
control over which parts of the web page are updated when,
which can have an impact on performance, especially when
the web page - and thus the virtual DOM - consists of
many elements.

So far, rhone-js doesn't use a virtual DOM, but interacts with
the real DOM directly through a network of monadic stream
functions. Whether this will result in a nice way to write web applications
or will lead to unmaintainable tangles of code, only time
and experience will tell.

### Whats next?

In the [next part](Reset.md), I'll explain a first sample application
with some real application state and reactive components in detail.
Have fun!
