## State and Everything: A minimalistic Example

I have been told by the nice people of the Idris community
on Discord that every functional web framework has some clickable
counter as its first example.

If that's true, I'm not the one to break with tradition, so here we go.

The application will consist of three buttons and an internal
counter: One button to increase the counter by one, one button
to decrease the counter, and one button to reset the counter to
zero. On every button click, the user interface should be updated
and display the actual count. In addition, we do not want users
to increase or decrease the counter too much, so the corresponding
buttons should be disabled if values are getting too large or too small.
Resetting the state makes no sense when the counter is at zero,
so the *reset* button should be disabled in that case as well.


```idris
module Examples.Reset

import Examples.CSS
import Rhone.JS
import Text.CSS

%default total
```

### Model

Our model is still too primitive to require a custom
data type. Since the button clicks update an integer value,
our event type will be a function on integers:

```idris
public export
Ev : Type
Ev = Int8 -> Int8
```

### View

First, we define some custom CSS rules for the
elements specific to this application:

```idris
inc : String
inc = "inc"

output : String
output = "output"

css : List Rule
css =
  [ class output  !!
      [ FontSize        .= Large
      , Margin          .= pt 5
      , TextAlign       .= End
      , Width           .= perc 10
      ]

  , class inc  !!
      [ Margin          .= pt 5
      , Width           .= perc 10
      ]
  ]
```

Next, we need to identify the dynamic components
of our application whose behavior or appearance will
change depending on the current state.
There are four of them: The buttons for increasing and decreasing
the counter, which will be disabled if the value gets to
small or too big, the reset button, which will be disabled
if the counter is at zero, and the div element where we will output
the current count:

```idris
out : ElemRef Div
out = MkRef Div "outdiv"

btnInc : ElemRef Button
btnInc = MkRef Button "btninc"

btnDec : ElemRef Button
btnDec = MkRef Button "btndec"

btnReset : ElemRef Button
btnReset = MkRef Button "reset"
```

The DOM elements will be laid out as a list of
four lines, each with a descriptive label at the
beginning, followed by the active components:

```idris
line : (lbl: String) -> List (Node Ev) -> Node Ev
line lbl ns =
  div [class widgetLine] $ 
      label [class widgetLabel] [Text lbl] :: ns
```

The three buttons all display some descriptive
text and need to know about the event they fire
when they are being clicked:

```idris
btn : ElemRef Button -> Ev -> (lbl: String) -> Node Ev
btn ref ev lbl =
  button [id ref.id, onClick ev, classes [widget,btn,inc]] [Text lbl]
```

Finally, we can put the components together and define
the overall application layout:

```idris
content : Node Ev
content =
  div [ class widgetList ]
      [ line "Reset counter:"    [ btn btnReset (const 0) "Reset" ]
      , line "Increase counter:" [ btn btnInc   (+ 1)     "+" ]
      , line "Decrease counter:" [ btn btnDec   (+ (-1))  "-" ]
      , line "Count:"            [ div [id out.id, class output] [] ]
      ]
```

### Controller

We first define our effect type:

```idris
public export
M : Type -> Type
M = DomIO Ev JSIO
```

The actual controlling MSF is a simple state accumulator, the
output of which will be broadcast to the different dynamic
elements. `MSF m i o` comes with a `Monoid` implementation if
`o` is a `Monoid`, so we use `concat` on a list to bundle the data sinks
(a *sink* is a monadic stream function that produces no
output of interest):

```idris
msf : MSF M Ev ()
msf =
  accumulateWith apply 0 >>> fan_
    [ show     ^>> text out
    , (<= -10) ^>> disabledAt btnDec
    , (>=  10) ^>> disabledAt btnInc
    , (==   0) ^>> disabledAt btnReset
    ]
```

In the code above, `(>>>)` is sequencing of computations defined in
`Control.Category` in contrib, `(f ^>> g)` is an alias for
`arr f >>> g`, that allows us to use pure functions in a sequence of
computations directly, and `accumulateWith` is one of the looping
combinators defined for monadic stream functions. The implementation
of these combinators is pretty simple, so I suggest you have a look
at the code and tutorials in the *rhone* library to get a better understanding
of what's going on under the hood. The data sinks `text` and `disabled`
are defined in module `Rhone.JS.Sink`. They merely use `arrM` to
lift the corresponding effectful computations from the idris2-dom
library to the MSF context.

Finally, we put everything together in a single effectful
computation: Applying the CSS rules, setting up the HTML
content and registering all necessary event listeners,
and returning the stream function.

```idris
export
ui : M (MSF M Ev ())
ui = do
  applyCSS $ coreCSS ++ css
  innerHtmlAt exampleDiv content
  pure msf
```

### Some Background: Running Monadic Stream Functions

We will now have a closer look at how the machinery in the background
operates. The first piece of functionality for understanding what's going
on comes in form of function `Data.MSF.step`: This is a single step
evaluation function for MSFs: An MSF is passed an input value, and the result
will be an effectful computation of an output value together with
a new MSF, which will be used in the next evaluation step.

All of this is set up by invoking one of the `reactimateXY` functions
from `Control.Monad.Dom.DomIO`. Go ahead and have a look at their implementations:
I tried to properly annotate the code to make it easier to understand
what's going on: An event handler is being setup and registered at all
active components (this happens, when `innerHtmlAt` is executed), which
will read the current `MSF` (holding the current application state!)
from a mutable variable
and evaluate it using `Data.MSF.step`, whenever an event is fired.
The resulting continuation is then written back to the mutable
variable.

When you look again at the [selector implementation](Selector.md), you
will see that the MSF we defined there will invoke `reactimateDomIni`
on our `ui` function whenever the user selected the `"reset"` application.

Why the call to `reactimateDomIni`? When you have a closer look at
the structure of `content` above, you will note that the initial output
field shows no text, and the *reset* button is enabled (the `disabled`
attribute has not been set explicitly). This is not what we want: The current
application state (the value 0) should be correctly shown and the
*reset* button disabled accordingly. We could set these things up
manually in `content`, but that would be a repetition of application
logic. It's often better to setup everything by invoking the `MSF`
once with an *initialization event*. Then everything will behave
correctly from the very beginning.
