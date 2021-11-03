## For our Children: A simple Math Game

This is a simple math game I wrote for our children.
A picture is hidden behind a grid of panels and they
have to solve randomly generated equations. Every time
an equation is solved correctly, a randomly chosen
tile is removed and part of the picture revealed.

The interactive part of this application is very simple,
but the example demonstrates some other topics that
keep coming up like random value generation and
localization (I'm Swiss, so my children don't yet
speak English).

```idris
module Examples.MathGame

import Control.Monad.State
import Data.List
import Data.MSF.Trans
import Data.Vect
import Examples.CSS.MathGame
import Examples.Util
import Generics.Derive
import Rhone.Canvas
import Rhone.JS
import System.Random
import Text.CSS.Color
import Text.CSS.Percentage

%language ElabReflection
%default total
```

### Model

We first define the events our application handles. Users
chan enter a string and check it against the currect
calculation, the can abort and start a new game,
and they can change the UI's language:

```idris
data Language = EN | DE

%runElab derive "Language" [Generic,Meta,Show,Eq]

public export
data Ev = Lang String | Check | NewGame

%runElab derive "Ev" [Generic,Meta,Show,Eq]
```

We also need data types for representing the calculations
our users should perform. To keep things simple, we
only support very basic arithmetics:

```idris
data Op = Plus | Minus | Mult

record Calc where
  constructor MkCalc
  x  : Integer
  y  : Integer
  op : Op

result : Calc -> Integer
result (MkCalc x y Plus)  = x + y
result (MkCalc x y Minus) = x - y
result (MkCalc x y Mult)  = x * y

dispCalc : Calc -> String
dispCalc (MkCalc x y op) = "\{show x} \{dispOp op} \{show y} = "
  where dispOp : Op -> String
        dispOp Plus  = "+"
        dispOp Minus = "-"
        dispOp Mult  = "*"
```

Next, we need to keep track of the current game state:
The current calculation to solve, the tiles on the
picture that have already been removed, the stuck tiles
from wrong answers, and the picture (represented as
the image's URL) hidden behind the tiles:

```idris
record GameState where
  constructor MkGS
  lang   : Language
  rows   : Bits8
  stuck  : List (Bits8,Bits8)
  tiles  : List (Bits8,Bits8)
  pic    : String
  calc   : Calc

noTilesLeft : GameState -> Bool
noTilesLeft gs = null $ gs.tiles
```

Finally, we need a list of pictures from which we
randomly choose one. These were all taken from
[pexels](https://www.pexels.com/search/open%20source/)
and scaled down to 500 x 500 pixels:

```idris
pictures : List String
pictures = map (\n => "pics/pic\{show n}.jpg") [the Bits8 1..11]
```

### View

As usual, the application's CSS rules have been moved to
a [separate module](CSS/MathGame.idr). We start with defining
the localized strings we need:

```idris
data Result = Ended Language 
            | Correct Language
            | Wrong Language Calc Integer

style : Result -> Maybe String
style (Ended _)     = Nothing
style (Correct _)   = Just "color : \{render green}"
style (Wrong _ _ _) = Just "color : \{render red}"

language : Language -> String
language DE = "Sprache"
language EN = "Language"

german : Language -> String
german DE = "Deutsch"
german EN = "German"

english : Language -> String
english DE = "Englisch"
english EN = "English"

resultStr : Language -> String
resultStr DE = "Resultat"
resultStr EN = "result"

checkAnswerStr : Language -> String
checkAnswerStr DE = "Antwort prüfen"
checkAnswerStr EN = "Check answer"

reply : Result -> String
reply (Ended EN)   = "The game has ended."
reply (Correct EN) = "Correct!"
reply (Ended DE)   = "Das Spiel ist vorbei."
reply (Correct DE) = "Richtig!"
reply (Wrong EN c n) =
     "That's not correct. Your answer was \{show n}. "
  ++ "The correct answer is: \{dispCalc c} \{show $ result c}."
reply (Wrong DE c n) =
     "Leider falsch. Deine Antwort war \{show n}. "
  ++ "Die richtige Antwort ist: \{dispCalc c} \{show $ result c}."
```

We can now define the HTML elements of the application:

```idris
wcanvas : Bits32
wcanvas = 500

content : Language -> Node Ev
content l =
  div [ class mathContent ]
    [ lbl "\{language l}:" lblLang
    , select
        [ id langIn.id, classes [widget, selectIn], onChange Lang]
        [ option [ value "de", selected (l == DE)] [Text $ german l]
        , option [ value "en", selected (l == EN)] [Text $ english l]
        ]
     
    , div [ id calc.id ] []

    , input [ id resultIn.id
            , onEnterDown Check
            , class widget
            , placeholder (resultStr l)
            ] []

    , button [ id checkBtn.id
             , onClick Check
             , classes [widget,btn]
             ] [Text $ checkAnswerStr l]
     
    , div [ id out.id ] []

    , canvas [ id pic.id, width wcanvas, height wcanvas ] []
    ]
```

We also need to display the tiles still hiding the
picture, distinguishing between *stuck* tiles from wrong
answers and initial tiles from calculations not yet
solved:

```idris
tile : (Bits8,Bits8) -> Scene
tile (x,y) = S1 [] Id $ Rect (cast x) (cast y) 1 1 Fill

stuckColor : Color
stuckColor = HSLA 0 0 50 80

dispState : GameState -> Scene
dispState gs = 
  let sf = cast {to = Double} wcanvas / cast gs.rows
   in SM [] (scale sf sf)
        [ SM [ Fill black ] Id $ map tile gs.tiles
        , SM [ Fill stuckColor ] Id $ map tile gs.stuck
        ]

renderGame : LiftJSIO m => GameState -> m ()
renderGame gs =
  render $ MkCanvas pic (cast wcanvas) (cast wcanvas) (dispState gs)
```

### Controller

```idris
upperBound : Int32
upperBound = 100

randomCalc : HasIO io => io Calc
randomCalc = do
  op   <- rndSelect' [Plus,Minus,Mult]
  case op of
    Plus => do
      x <- randomRIO (0, upperBound)
      y <- randomRIO (0, upperBound - x)
      pure $ MkCalc (cast x) (cast y) op

    Minus => do
      x <- randomRIO (0, upperBound)
      y <- randomRIO (0, x)
      pure $ MkCalc (cast x) (cast y) op

    Mult => do
      x <- randomRIO (1, 12)
      y <- randomRIO (0, upperBound `div` x)
      pure $ MkCalc (cast x) (cast y) op

randomGame : HasIO io => Language -> io GameState
randomGame l = do
  pic   <- rndSelect pictures
  calc  <- randomCalc
  pairs <- for [| MkPair [0..3] [0..3] |] $
             \p => (,p) <$> randomRIO (0,the Int32 1000)
  let ts = snd <$> sortBy (comparing fst) pairs
  pure $ MkGS l 4 Nil ts pic calc
```

```idris
public export
M : Type -> Type
M = DomIO Ev JSIO

checkAnswer : NP I [String,GameState] -> NP I [Result,GameState]
checkAnswer [s,MkGS l nr stuck (h :: t) pic calc] =
  let answer = cast {to = Integer} s
   in if result calc == answer
     then [Correct l, MkGS l nr stuck t pic calc]
     else [Wrong  l calc answer, MkGS l nr (h :: stuck) t pic calc]
checkAnswer [s,gs] = [Ended gs.lang,gs]

setPic : LiftJSIO m => MSF m GameState ()
setPic =   (\gs => #"background-image : url('\#{gs.pic}');"#)
       ^>> attributeAt_ "style" pic 

dispGame : LiftJSIO m => MSF m GameState ()
dispGame = fan_ [ noTilesLeft ^>> disabledAt checkBtn
                , arrM (liftJSIO . renderGame)
                , calc ^>> dispCalc ^>> text calc
                , const "" >>> Sink.valueOf resultIn
                , setPic
                ]

check : LiftJSIO m => MSF (StateT GameState m) i ()
check =  fan [valueOf resultIn, get]
     >>> checkAnswer
     ^>> fan_ [ snd >>! (\g => randomCalc >>= (\c => put $ record { calc = c} g))
              , hd  >>> reply ^>> text out
              , hd  >>> style ^>> attributeAt "style" out
              ]

newGame : LiftJSIO m => MSF (StateT GameState m) i ()
newGame = get >>> lang ^>> arrM randomGame >>> fan_ [setPic, put]

adjLang : MSF (StateT GameState M) Ev ()
adjLang = readLang ^>> ifJust (
            arrM $ \l => innerHtmlAt exampleDiv (content l)
                      >> modify (record { lang = l })
          )
  where readLang : Ev -> Maybe Language
        readLang (Lang "en") = Just EN
        readLang (Lang "de") = Just DE
        readLang _           = Nothing

msf : MSF (StateT GameState M) Ev ()
msf =   fan_ [ifIs NewGame newGame, ifIs Check check, adjLang]
    >>> observeWith (get >>> dispGame)

export
ui : M (MSF M Ev (), JSIO ())
ui = do
  innerHtmlAt exampleDiv (content DE)
  ini <- randomGame DE
  pure (feedback ini (fromState msf), pure ())
```

<!-- vi: filetype=idris2
-->
