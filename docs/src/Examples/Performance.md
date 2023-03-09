# Making Buttons: A look at Performance

This tutorial's web application comes with the following
ingredients: A validated text input where users are
supposed to enter a positive natural number.
When they hit `Enter` after entering their
number, a corresponding number of buttons is created, each
of which can be clicked exactly once before being disabled,
and the sum of the values clicked will be accumulated and
displayed at the UI. The sum should be reset when a new set
of buttons is created.

Since we want to have a look at the performance of this,
we also include an output field for the time it took to
create and display the buttons.

We are going to iterate over large lists of
items, therefore we need to be conscious about stack space and make
use of the tail-recursive functions from
[idris2-tailrec](https://github.com/stefan-hoeck/idris2-tailrec).
We also write our first event data type, for which we will
automatically derive certain interfaces using elaborator
reflection. The corresponding functionality comes from
the [idris2-sop](https://github.com/stefan-hoeck/idris2-sop)
library.

Here's the list of imports:

```idris
module Examples.Performance

import Data.DPair
import Data.Either
import Data.List.TR
import Data.Nat
import Data.String
import Examples.CSS.Performance
import Examples.Util
import Rhone.JS
import Derive.Prelude

%language ElabReflection
%default total
```

## Model

Since the buttons in the button grid have no effect
on the behavior of the components responsible for
creating the button grid, we can completely decouple
the two parts. This is similar to what we did for
the [example selector](Selector.md).

This means we have two unrelated models, the first being just
natural numbers that are summed up, the second being
the events from the text input. For the latter, we use
a custom data type:

```idris
data Ev = Reload | Validate

%runElab derive "Ev" [Show,Eq]

PosNat : Type
PosNat = Subset Nat IsSucc
```

We also require a function for input validation:

```idris
validate : String -> Either String PosNat
validate s = case cast {to = Nat} s of
  Z       => Left "Not a positive natural number: \{s}"
  n@(S _) => Right $ Element n ItIsSucc
```

## View

The CSS rules and reference IDs have again been moved
to their [own module](CSS/Performance.idr), to declutter
the code here. We also use labeled lines of input elements
as in the [previous example](Reset.idr). For the
grid of buttons, we need a reference for each button,
since we want to disable them after they have been clicked:

```idris
btnRef : Nat -> ElemRef Button
btnRef n = Id Button "BTN\{show n}"

btn : Nat -> Node Nat
btn n =
  button
    [ref (btnRef n), onClick n, classes [widget,btn,inc]]
    [Text $ show n]
```

Next, we write the function to create a grid of buttons.
Since we plan to create thousands of buttons at once, we must
make sure to do this in a stack-safe manner.
List functions like `map` or `range` (used to implement
the range syntax: `[1..1000]`) are (so far) not stack safe,
so we use `mapTR` and `iterateTR` from `Data.List.TR`
instead. For these, the recursive calls are in tail position,
so the JS backends can convert them to while loops using
constant stack space.

```idris
btns : PosNat -> Node Nat
btns (Element n _) = div [class grid] . map btn $ iterateTR n (+1) 1
```

And, finally, the overall layout of the application:

```idris
content : Node Ev
content =
  div [ class performanceContent ]
    [ lbl "Number of buttons:" numButtonsLbl
    , input [ ref natIn
            , onInput (const Validate)
            , onEnterDown Reload
            , classes [widget, textIn]
            , placeholder "Enter a positive integer"
            ] []
    , button [ref btnRun, onClick Reload, classes [widget, btn]] ["Run"]
    , lbl "Sum:" sumLbl
    , div [ref out] []
    , div [ref time] []
    , div [ref buttons] []
    ]
```

We register two events at the text field: Whenever users input
text in it, the field should fire an event to get the validation
routine started. If the *Enter* key is pressed, the grid of
buttons should be generated. This should also happen if the
*Run* button is clicked.

## Controller

The controller - especially the MSF used for
generating the buttons and validating the user input -
is quite a bit more involved. We have two unrelated
parts in the UI (unrelated meaning, that they do not
react on a shared set of events), so we need
two aliases for the corresponding effect types:

```idris

public export
MI : Type -> Type
MI = DomIO Ev JSIO

public export
MB : Type -> Type
MB = DomIO Nat JSIO
```

We also need a way to calculate the time taken to create
and display the buttons. The idris2-dom library does not
yet provide this functionality, but it is available
from `Rhone.JS.Util`:

```idris
dispTime : Nat -> Integer -> String
dispTime 1 ms = "\Loaded one button in \{show ms} ms."
dispTime n ms = "\Loaded \{show n} buttons in \{show ms} ms."
```

The reactive behavior of the grid of buttons consists of
two pieces of functionality: The first is used to accumulate
the sum of values and print the result to the output div,
the second is responsible for disabling every button that
has been clicked. However, trying to disable a non-existing
button will lead to an error message being printed to the
console. Since we plan to initialize the signal function
with an input of zero, we must make sure the disabling
part is only invoked on non-zero input.
The call to `ifIsNot 0` makes sure that the following
stream function is only evaluated if the input is indeed
a positive natural number. Again, have a look at this
function's implementation in the *rhone* library. There
are many similar combinator and they are very
convenient to use.

```idris
sumNats : MSF MB Nat ()
sumNats = fan_
  [ accumulateWith (+) 0 >>> show ^>> text out
  , ifIsNot 0 $ fan [arr btnRef, const True] >>> disabled
  ]
```

Here is the function used to create a non-zero number of
buttons, add them to the UI and display the time taken
to do so:

```idris
btnsSF : PosNat -> MB (MSF MB Nat (), JSIO ())
btnsSF n = do
  t1 <- currentTime
  innerHtmlAt buttons (btns n)
  t2 <- currentTime
  rawInnerHtmlAt time (dispTime n.fst $ t2 - t1)
  pure (sumNats, pure ())
```

The second controller takes care of validating user
input (the number of buttons entered in the text
field), and reloading the button grid upon a
`Reload` event:

```idris
count : MSF MI Ev (Either String PosNat)
count =    getInput Validate validate natIn
       >>> observeWith (isLeft ^>> disabledAt btnRun)
```

Input validation can be quite involved (I wrote a lengthy
tutorial about this for the *rhone* project), but
utility function `getValue` takes care of this for us.
However, we also need to make sure to disable the *Run* button
in case of invalid input.

```idris
msf : MSF MI Ev ()
msf =   fan [count, is Reload]
    >>> rightOnEvent
    ?>> arrM (ignore . reactimateInDomIni 0 . btnsSF)
```

The `rightOnEvent` combinator comes up often in user
interfaces: Some input needs to be validated and
processed whenever an appropriate event is fired.
The input stream function holds an `Either a b`, but we need
an `a` to continue. There are of course also combinators
`leftOnEvent`, `justOnEvent`, and `nothingOnEvent`.

```idris
export
ui : MI (MSF MI Ev (), JSIO ())
ui = innerHtmlAt exampleDiv content $> (msf, pure ())
```

<!-- vi: filetype=idris2:syntax=markdown
-->
