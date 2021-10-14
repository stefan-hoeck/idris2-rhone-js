## Making Buttons: A look at Performance

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
import Data.List.TR
import Data.Nat
import Data.String
import Examples.CSS
import Generics.Derive
import Rhone.JS
import Text.CSS

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

%runElab derive "Ev" [Generic,Meta,Show,Eq]

PosNat : Type
PosNat = Subset Nat IsSucc
```

We provide some utility functions for input validation
and reloading the buttons

```idris
validate : String -> Either String PosNat
validate s = case cast {to = Nat} s of
  Z       => Left #"Not a positive natural number: \#{s}"#
  n@(S _) => Right $ Element n ItIsSucc
```

## View

First, the CSS:

```idris
incSmall : String
incSmall = "incSmall"

output : String
output = "output"

buttonLine : String
buttonLine = "buttonline"

numButtons : String
numButtons = "numbuttons"

grid : String
grid = "grid"

css : List Rule
css =
  [ class output  !!
      [ FontSize        .= Large
      , Margin          .= pt 5
      , TextAlign       .= End
      , Width           .= perc 20
      ]

  , class grid  !!
      [ Display         .= Flex
      , FlexWrap        .= "wrap"
      ]

  , class incSmall !!
      [ FlexBasis       .= perc 5
      , FontSize        .= XXSmall
      ]

  , class numButtons !!
      [ Margin          .= pt 5
      , TextAlign       .= End
      , Width           .= perc 20
      ]
  ]
```

Next, the reference IDs for the active components:

```idris
-- displays the current sum of clicks
out : ElemRef Div
out = MkRef Div "outdiv"

-- text fields where users can enter the number of buttons
natIn : ElemRef Input
natIn = MkRef Input "numbuttons"

-- where the created buttons go
buttons : ElemRef Div
buttons = MkRef Div "buttons"

-- displays the time take to create the buttons
time : ElemRef Div
time = MkRef Div "time"
```

We have two labeled lines similar to the ones from
[the last tutorial](Reset.md):

```idris
line : (lbl: String) -> List (Node e) -> Node e
line lbl ns =
  div [class widgetLine] $ 
      label [class widgetLabel] [Text lbl] :: ns
```

And here's the function to create a single button:
It must come with its own ID, since we need to
disable it, once it has been clicked.

```idris
btnRef : Nat -> ElemRef Button
btnRef n = MkRef Button #"BTN\#{show n}"#

btn : Nat -> Node Nat
btn n =
  button
    [id $ id (btnRef n), onClick n, classes [widget,btn,incSmall]]
    [Text $ show n]
```

Next, we write the function to create a grid of buttons.
Since we plan to create thousands of buttons at once, we must
make sure do this in a stack-safe manner.
List functions like `map` or `range` (used to implement
the range syntax: `[1..1000]`) are (so far) not stack safe,
so we use `mapTR` and `iterateTR` from `Data.List.TR`
instead. For these, the recursive calls are in tail position,
so the JS backends can convert them to while loops using
constant stack space.

```idris
btns : PosNat -> Node Nat
btns (Element n _) = div [class grid] . mapTR btn $ iterateTR n (+1) 1
```

And, finally, the overall layout of the application:

```idris
content : Node Ev
content =
  div [ class widgetList ]
      [ line "Number of buttons:"
          [ input [ id natIn.id
                  , onInput (const Validate)
                  , onEnterDown Reload
                  , classes [ widget, numButtons ]
                  , placeholder "Enter a positive integer"
                  ] []
          ]
      , line "Sum:" [ div [id out.id, class output] [] ]
      , div [id time.id, class widgetLine] []
      , div [id buttons.id, class widgetLine] []
      ]
```

## Controller

The controller - especially the MSF used for
generating the buttons and validating the user input -
is quite a bit more involved. We have two unrelated
parts in the UI (unrelated meaning, that they do not
react on a shared set of UI events), so we need
to aliases for the corresponding effect types:

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
yet provide this functionality, so we quickly hack together
our own FFI call:

```idris
%foreign "javascript:lambda:(w) => new Date().getTime()"
prim__time : PrimIO Int32

dispTime : Nat -> Int32 -> String
dispTime 1 ms = #"\#Loaded one button in \#{show ms} ms."#
dispTime n ms = #"\#Loaded \#{show n} buttons in \#{show ms} ms."#
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
`isNot 0` has type `MSF MB Nat (Event Nat)`, this is what
we call an *event stream*, a stream function, which only
holds a value on certain occasions. The signal functions
that follow after can only be evaluated if an input value
is available, so this makes sure that they are not being
evaluated with the initial zero event. Finally, the (>|)
operator is a convenienc function used to send an event
stream down a *sink*: A signal function with a `Unit`
result type.

```idris
sumNats : MSF MB Nat ()
sumNats = fan_
  [ accumulateWith (+) 0 >>> show ^>> text out
  , ifFalse (0 ==) $ fan [arr btnRef, const True] >>> disabled
  ]
```

Here is the function used to create a non-zero number of
buttons, add them to the UI and display the time taken
to do so:

```idris
btnsSF : PosNat -> MB (MSF MB Nat (), JSIO ())
btnsSF n = do
  t1 <- primIO prim__time
  innerHtmlAt buttons (btns n)
  t2 <- primIO prim__time
  rawInnerHtmlAt time (dispTime n.fst $ t2 - t1)
  pure (sumNats, pure ())
```

```idris
count : MSF MI Ev (Either String PosNat)
count = valueOf natIn >>> validate ^>> observeWith (leftInvalid natIn)

msf : MSF MI Ev ()
msf =   fan [count, is Reload]
    >>> rightOnEvent
    >>> ifEvent (arrM (ignore . reactimateInDomIni 0 . btnsSF))

export
ui : MI (MSF MI Ev (), JSIO ())
ui = do
  applyCSS $ coreCSS ++ css
  innerHtmlAt exampleDiv content
  pure $ (ignore msf, pure ())
```
