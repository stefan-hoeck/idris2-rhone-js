||| Simulating the frictionless movement of a set of balls
||| in a room with walls under the influence of gravity in 2D.
module Examples.FRP.Ball

import Data.MSF
import Data.List.TR
import Data.Vect
import Data.VectorSpace
import Examples.CSS.Colors
import Rhone.JS.Util
import Text.CSS.Color

%default total

||| 2D Vector
public export
V2 : Type
V2 = Vect 2 Double

||| Position of a point in 2D space
public export
Position : Type
Position = V2

||| Velocity of a point in 2D space
public export
Velocity : Type
Velocity = V2

||| Acceleration of a point in 2D space
public export
Acceleration : Type
Acceleration = V2

--------------------------------------------------------------------------------
--          Constants
--------------------------------------------------------------------------------

||| Acceleration vector
export
acc : Acceleration
acc = [0,-9.81]

||| Height and width of the room in meters
export
w : Double
w = 10

||| Start height of all balls
export
h0 : Double
h0 = 9

||| Ball radius
export
r : Double
r = 0.1

||| Start velocity in m/s
v0 : Double
v0 = 4

--------------------------------------------------------------------------------
--          Ball
--------------------------------------------------------------------------------

||| A bouncing ball.
public export
record Ball where
  constructor MkBall
  col : Color
  pos : Position
  vel : Velocity

||| Generates a list of balls to start the simulation.
export
initialBalls : (n : Nat) -> List Ball
initialBalls n = go n
  where col : Bits8 -> Color
        col 0 = comp100 
        col 1 = comp80
        col 2 = comp60
        col 3 = comp40
        col _ = comp20

        ball : Nat -> Ball
        ball k =
          let factor = cast {to = Double} k / (cast n - 1.0)
              phi    = pi * factor
              x0     = 1.0 + factor * 8
           in MkBall (col $ cast k `mod` 5) [x0,9] [3 * sin phi, 3 * cos phi]

        go : (k : Nat) -> List Ball
        go 0     = []
        go (S k) = ball k :: go k

--------------------------------------------------------------------------------
--          SF
--------------------------------------------------------------------------------

-- Collision detection: We verify that the given ball
-- is still in the room. If this is not the case, we simulate
-- a bouncing off the walls by inverting the x-velocity (if the
-- ball hit a wall) or the y-velocity (if the ball hit the ground)
checkBounds : Ball -> Ball
checkBounds b@(MkBall c [px,py] [vx,vy]) =
  if      (py <= r  && vy < 0)      then (MkBall c [px,py] [vx,-vy])
  else if (px <= r  && vx < 0)      then (MkBall c [px,py] [-vx,vy])
  else if (px >= (w - r) && vx > 0) then (MkBall c [px,py] [-vx,vy])
  else b

-- moves a ball after a given time delta
-- by adjusting its position and velocity
nextBall : DTime -> Ball -> Ball
nextBall delta (MkBall c p v) =
  let dt   = cast delta / the Double 1000 -- time in seconds
      v2   = v ^+^ (dt *^ acc)
      p2   = p ^+^ (dt / 2 *^ (v ^+^ v2))
   in checkBounds (MkBall c p2 v2)

||| Signal function of a list of bouncing balls.
export
balls : (inis : List Ball) -> MSF m DTime (List Ball)
balls inis = accumulateWith (mapTR . nextBall) inis
