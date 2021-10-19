module Examples.FRP.BallCanvas

import Control.Monad.Dom
import Data.List.TR
import Data.Vect
import Examples.CSS.Colors
import Examples.FRP.Ball
import JS
import Rhone.Canvas
import Text.CSS.Color
import Web.Dom

inBounds : Ball -> Bool
inBounds (MkBall _ [x,y] _) = y >= 0 && x >= 0 && x <= w

-- we hide balls, which are temporarily out of bounds
-- to give the illusion (at high enough redraw rates)
-- that balls don't go through walls.
ballToScene : Ball -> Scene
ballToScene b@(MkBall _ [x,y] _) =
  S1 [Fill $ if inBounds b then b.col else transparent] Id $
    circle x (w - y) r Fill

wallThickness : Double
wallThickness = 0.20

-- walls and floor of the room.
walls : Shape
walls = 
  let hwt = wallThickness / 2
   in polyLine [(-hwt, 0), (-hwt, w+hwt), (w+hwt,w+hwt), (w+hwt,0)]

||| Converts a list of balls to a canvas scene.
export
ballsToScene : List Ball -> Scene
ballsToScene bs =
  SM  [] (Transform 50 0 0 50 10 10) $
    [ SM [] Id $ mapTR ballToScene bs
    , S1 [Stroke base80, LineWidth wallThickness] Id walls
    ]

export
renderBalls :  LiftJSIO io
            => ElemRef HTMLCanvasElement
            -> (w,h : Double)
            -> List Ball
            -> io ()
renderBalls ref w h bs =
  liftJSIO . render $ MkCanvas ref w h (ballsToScene bs)
