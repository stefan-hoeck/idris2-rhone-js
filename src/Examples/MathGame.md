## For our Children: A simple Math Game

This is a simple math game I wrote for our children.
An picture is hidden behind a grid of panels and they
have to solve randomly generated equations. Every time
an equation is solved correctly, a randomly chosen
tile is removed and part of the picture revealed.

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

%language ElabReflection
%default total
```

### Model

```idris
pictures : List String
pictures = map (\n => "pics/pic\{show n}.jpg") (1 :: [2..7])

data Language = English | German

%runElab derive "Language" [Generic,Meta,Show,Eq]

data Op = Plus | Minus | Mult

record Calc where
  constructor MkCalc
  x  : Bits32
  y  : Bits32
  op : Op

result : Calc -> Bits32
result (MkCalc x y Plus)  = x + y
result (MkCalc x y Minus) = x - y
result (MkCalc x y Mult)  = x * y

dispCalc : Calc -> String
dispCalc (MkCalc x y op) = "\{show x} \{dispOp op} \{show y} = "
  where dispOp : Op -> String
        dispOp Plus  = "+"
        dispOp Minus = "-"
        dispOp Mult  = "*"

data Result = Ended | Correct | Wrong Bits32

reply : Result -> String
reply Ended     = "The game has ended"
reply Correct   = "Correct!"
reply (Wrong n) = "That's not correct. The right answer was \{show n}."

style : Result -> Maybe String
style Ended     = Nothing
style Correct   = Just "color : \{render green}"
style (Wrong n) = Just "color : \{render red}"

record GameState where
  constructor MkGS
  rows   : Bits8
  stuck  : List (Bits8,Bits8)
  tiles  : List (Bits8,Bits8)
  pic    : String
  calc   : Calc

noTilesLeft : GameState -> Bool
noTilesLeft gs = null $ gs.tiles

newState : String -> Calc -> GameState
newState = MkGS 4 Nil [| MkPair [0..3] [0..3] |]

public export
data Ev = Lang String
        | Check
        | NewGame
        | Init

%runElab derive "Ev" [Generic,Meta,Show,Eq]
```

### View

```idris
wcanvas : Bits32
wcanvas = 500

content : Language -> Node Ev
content lang =
  div [ class mathContent ]
    [ lbl "Language:" lblLang
    , select
        [ id langIn.id, classes [widget, selectIn], onChange Lang]
        [ option [ value "de", selected True ] ["Deutsch"]
        , option [ value "en" ] ["English"]
        ]
     
    , div [ id calc.id ] []
    , input [ id resultIn.id
            , onEnterDown Check
            , class widget
            , placeholder "Result"
            ] []
    , button [ id checkBtn.id
             , onClick Check
             , classes [widget,btn]
             ] ["Check Answer"]
     
    , div [ id out.id ] []
    , canvas [ id pic.id
             , width wcanvas
             , height wcanvas
             ] []
    ]
```

```idris
tile : (Bits8,Bits8) -> Scene
tile (x,y) = S1 [] Id $ Rect (cast x) (cast y) 1 1 Fill

dispState : GameState -> Scene
dispState gs = 
  let sf = cast {to = Double} wcanvas / cast gs.rows
   in SM [] (scale sf sf) $ map tile (gs.stuck ++ toList gs.tiles)
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

randomGame : HasIO io => io GameState
randomGame = do
  pic   <- rndSelect pictures
  calc  <- randomCalc
  pairs <- for [| MkPair [0..3] [0..3] |] $
             \p => (,p) <$> randomRIO (0,the Int32 1000)
  let ts = snd <$> sortBy (comparing fst) pairs
  pure $ MkGS 4 Nil ts pic calc
```

```idris
public export
M : Type -> Type
M = DomIO Ev JSIO

checkAnswer : NP I [String,GameState] -> NP I [Result,GameState]
checkAnswer [s,MkGS nr stuck (h :: t) pic calc] =
  if result calc == cast s
     then [Correct, MkGS nr stuck t pic calc]
     else [Wrong $ result calc, MkGS nr (h :: stuck) t pic calc]
checkAnswer [s,gs] = [Ended,gs]

renderGame : GameState -> JSIO ()
renderGame gs =
  render $ MkCanvas pic (cast wcanvas) (cast wcanvas) (dispState gs)

dispGame : LiftJSIO m => MSF m GameState ()
dispGame = fan_ [ noTilesLeft ^>> disabledAt checkBtn
                , arrM (liftJSIO . renderGame)
                , calc ^>> dispCalc ^>> text calc
                , const "" >>> Sink.valueOf resultIn
                ]

setPic : LiftJSIO m => MSF m GameState ()
setPic =   (\gs => #"background-image : url('\#{gs.pic}');"#)
       ^>> attributeAt_ "style" pic 

check : LiftJSIO m => MSF (StateT GameState m) i ()
check =  fan [valueOf resultIn, get]
     >>> checkAnswer
     ^>> fan_ [ snd >>! (\g => randomCalc >>= (\c => put $ record { calc = c} g))
              , hd  >>> reply ^>> text out
              , hd  >>> style ^>> attributeAt "style" out
              ]

newGame : LiftJSIO m => MSF (StateT GameState m) i ()
newGame = constM randomGame >>> fan_ [setPic, put]

msf : GameState -> MSF M Ev ()
msf ini = feedback ini
        $   fromState (fan_ [ ifIs NewGame newGame
                            , ifIs Init newGame
                            , ifIs Check check ])
        >>> observeWith (hd >>> dispGame)

export
ui : M (MSF M Ev (), JSIO ())
ui = do
  innerHtmlAt exampleDiv (content English)
  ini <- randomGame
  pure (msf ini, pure ())
```

<!-- vi: filetype=idris2
-->
