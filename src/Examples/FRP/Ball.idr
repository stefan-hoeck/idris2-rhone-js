module Examples.FRP.Ball

import Control.Monad.Reader
import Data.Bits
import Data.MSF
import Data.MSF.Switch
import Data.MSF.Trans
import Data.Vect
import Data.VectorSpace
import Examples.FRP.Basics
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

export
g : Double
g = -9.81 / 1000000

export
acc : Acceleration
acc = [0,-9.81 / 1000000]

export
velocity :  m DTime
         => (a0 : Acceleration)
         -> (v0 : Velocity)
         -> MSF m Acceleration Velocity
velocity a0 v0 = integralFrom a0 >>^ (^+^ v0)

export
position :  m DTime
         => (v0 : Velocity)
         -> (p0 : Position)
         -> MSF m Velocity Position
position v0 p0 = integralFrom v0 >>^ (^+^ p0)

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

export
initialBalls : (n : Nat) -> List Ball
initialBalls n = go n
  where col : Nat -> Color
        col k = 
          let f = (the Bits32 0xffffff * cast k) `div` cast n
              r = cast {to = Bits8} f
              g = cast {to = Bits8} $ f `shiftR` fromNat 8
              b = cast {to = Bits8} $ f `shiftR` fromNat 16
           in MkColor (max 20 r) (max 20 g) (max 20 b)

        ball : Nat -> Ball
        ball k =
          let factor = cast {to = Double} k / cast n
              angle  = 2 * pi * factor
              x0     = 1.0 + factor * 8
           in MkBall (col k) [x0,5] [0.003 * cos angle, 0.003 * sin angle]

        go : (k : Nat) -> List Ball
        go 0     = []
        go (S k) = ball (S k) :: go k

--------------------------------------------------------------------------------
--          SF
--------------------------------------------------------------------------------

-- checkBounds : Ball -> Ball
-- checkBounds b@(MkBall col [px,py] [vx,vy]) =
--   if      (py <= 0  && vy < 0) then (MkBall col [px,py] [vx,-vy])
--   else if (px <= 0  && vx < 0) then (MkBall col [-px,py] [-vx,vy])
--   else if (px >= 10 && vx > 0) then (MkBall col [2*10 - px,py] [-vx,vy])
--   else b
-- 
-- nextBall : DTime -> Ball -> NP I [Ball,Ball]
-- nextBall dt (MkBall col [px,py] [vx,vy]) =
--   let delta = cast {to = Double} dt
--       vy2  = vy + g * delta
--       px2  = px + vx * delta
--       py2  = py + (vy + vy2) * delta / 2
--       b2   = checkBounds (MkBall col [px2,py2] [vx,vy2])
--    in [b2,b2]
-- 
-- export
-- ballGame : (ini : Ball) -> MSF m DTime Ball
-- ballGame ini = mealy nextBall ini
-- 
-- export
-- balls : (inis : List Ball) -> MSF m DTime (List Ball)
-- balls = traverse ballGame

--------------------------------------------------------------------------------
--          SF
--------------------------------------------------------------------------------

checkBounds : Ball -> Event Ball
checkBounds (MkBall col [px,py] [vx,vy]) =
  if      (py <= 0  && vy < 0) then Ev (MkBall col [px,py] [vx,-vy])
  else if (px <= 0  && vx < 0) then Ev (MkBall col [-px,py] [-vx,vy])
  else if (px >= 10 && vx > 0) then Ev (MkBall col [2*10 - px,py] [-vx,vy])
  else NoEv

nextBall : Ball -> Either (Ball,Ball) Ball
nextBall b = case checkBounds b of
  Ev b2 => Left  (b2,b2)
  NoEv  => Right b

ballFrom : m DTime => (ini : Ball) -> MSF m i Ball
ballFrom b =   const acc
           >>> velocity acc b.vel
           >>> fan [position b.vel b.pos,id]
           >>^ (\[p,v] => MkBall b.col p v)

export
ballGame : m DTime => (ini : Ball) -> MSF m i Ball
ballGame ini = resetOn ballFrom checkBounds ini 

export
balls : m DTime -> (inis : List Ball) -> MSF m i (List Ball)
balls dt = traverse ballGame
