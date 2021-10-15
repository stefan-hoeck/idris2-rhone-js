module Examples.Balls

import Data.DPair
import Data.Either
import Data.Nat
import Data.Vect
import Data.VectorSpace
import Examples.CSS.Balls
import Examples.FRP.Ball
import Examples.FRP.Basics
import Generics.Derive
import Rhone.JS

%language ElabReflection
%default total

inis : Vect 10 Ball
inis = [ MkBall [5,5] [0.002,0.003]
       , MkBall [3,5] [0.002,0.000]
       , MkBall [1,4] [0.010,0.000]
       , MkBall [2,2] [0.001,0.010]
       , MkBall [3,7] [0.011,0.003]
       , MkBall [1,9] [0.000,0.020]
       , MkBall [2,1] [0.020,0.012]
       , MkBall [4,4] [0.020,0.012]
       , MkBall [7,6] [0.020,0.002]
       , MkBall [8,3] [0.010,0.012]
       ]

content : Node ()
content =
  div [ class widgetList ]
      [ div [id log.id] []
      , div [id out.id] []
      ]

--------------------------------------------------------------------------------
--          UI
--------------------------------------------------------------------------------

public export
M : Type -> Type
M = DomIO () JSIO

fps : DTime -> String
fps 0  = #"FPS: 0"#
fps dt = #"FPS: \#{show $ 1000 `div` dt}"#

msf : MSF M () ()
msf =   runSF (fan [balls inis, dtime])
    >>> par [ ballsSVG ^>> innerHtml out
            , arr fps >>> text log]
    >>> neutral

export
ui : M (MSF M () (), JSIO ())
ui = do
  innerHtmlAt exampleDiv content
  h     <- handler <$> env 
  newID <- setInterval 5 (h ())
  pure (msf, clearInterval newID)
