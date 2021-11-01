```idris
module Examples.MathGame

import Data.Vect
import Examples.CSS.Reset
import Examples.Util
import Rhone.JS
import System.Random

%default total
```

### Model

```idris
data Language = English | German

data Op = Plus | Minus | Mult

GameState : Type 
GameState = List (Bits8,Bits8)

record Calc where
  constructor MkCalc
  x  : Bits32
  y  : Bits32
  op : Op

data Ev = Lang Language
        | Check
        | Cancel
```

### View

### Controller

```idris
upperBound : Int32
upperBound = 100

randomCalc : HasIO io => io Calc
randomCalc = do
  op <- rndSelect' [Plus,Minus,Mult]
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
```

<!-- vi: filetype=idris2
-->
