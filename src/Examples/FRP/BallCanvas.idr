module Examples.FRP.BallCanvas

import Control.Monad.Dom
import Data.List.TR
import Data.Vect
import Examples.FRP.Ball
import JS
import Rhone.Canvas
import Text.CSS.Color
import Web.Dom

ballToSegment : Ball -> Shape
ballToSegment (MkBall [x,y] _) = circle x (10 - y) 0.1 Fill

export
ballsToScene : List Ball -> Scene
ballsToScene bs =
  SM  Current (Transform 50 0 0 50 10 10)
    [ S1 (Col yellow) Id (Path [Move 0 0, Line 0 10, Line 10 10, Line 10 0] Stroke)
    , S1 (Col red) Id $ Shapes (mapTR ballToSegment bs)
    ]

export
renderBalls :  LiftJSIO io
            => ElemRef HTMLCanvasElement
            -> (w,h : Double)
            -> List Ball
            -> io ()
renderBalls ref w h bs =
  liftJSIO . render $ MkCanvas ref w h (ballsToScene bs)
