module Examples.FRP.Ball

import Control.Monad.Reader
import Data.MSF
import Data.MSF.Switch
import Data.MSF.Trans
import Data.Vect
import Data.VectorSpace
import Examples.FRP.Basics

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

-- export
-- g : Acceleration
-- g = [0,-9.81 / 1000000]

export
velocity :  Monad m
         => (a0 : Acceleration)
         -> (v0 : Velocity)
         -> SF m Acceleration Velocity
velocity a0 v0 = integralFrom a0 >>^ (^+^ v0)

export
position :  Monad m
         => (v0 : Velocity)
         -> (p0 : Position)
         -> SF m Velocity Position
position v0 p0 = integralFrom v0 >>^ (^+^ p0)

--------------------------------------------------------------------------------
--          Ball
--------------------------------------------------------------------------------

||| A bouncing ball.
public export
record Ball where
  constructor MkBall
  pos : Position
  vel : Velocity

export
printBall : Ball -> String
printBall (MkBall p v) = #"Ball: \#{show p} \#{show v}"#

export
initialBalls : (n : Nat) -> List Ball
initialBalls n = go n
  where ball : Nat -> Ball
        ball k =
          let factor = cast {to = Double} k / cast n
              angle = 2 * pi * factor
              x0 = 1.0 + factor * 8
           in MkBall [x0,5] [0.003 * cos angle, 0.003 * sin angle]

        go : (k : Nat) -> List Ball
        go 0     = []
        go (S k) = ball (S k) :: go k

--------------------------------------------------------------------------------
--          SF
--------------------------------------------------------------------------------

checkBounds : Ball -> Ball
checkBounds b@(MkBall [px,py] [vx,vy]) =
  if      (py <= 0  && vy < 0) then (MkBall [px,py] [vx,-vy])
  else if (px <= 0  && vx < 0) then (MkBall [-px,py] [-vx,vy])
  else if (px >= 10 && vx > 0) then (MkBall [2*10 - px,py] [-vx,vy])
  else b

nextBall : DTime -> Ball -> NP I [Ball,Ball]
nextBall dt (MkBall [px,py] [vx,vy]) =
  let delta = cast {to = Double} dt
      vy2  = vy + g * delta
      px2  = px + vx * delta
      py2  = py + (vy + vy2) * delta / 2
      b2   = checkBounds (MkBall [px2,py2] [vx,vy2])
   in [b2,b2]

export
ballGame : Monad m => (ini : Ball) -> SF m i Ball
ballGame ini = dtime >>> mealy nextBall ini

export
balls : Monad m => (inis : List Ball) -> SF m i (List Ball)
balls = traverse ballGame

-- --------------------------------------------------------------------------------
-- --          SF
-- --------------------------------------------------------------------------------
-- 
-- checkBounds : Ball -> Event Ball
-- checkBounds (MkBall [px,py] [vx,vy]) =
--   if      (py <= 0  && vy < 0) then Ev (MkBall [px,py] [vx,-vy])
--   else if (px <= 0  && vx < 0) then Ev (MkBall [-px,py] [-vx,vy])
--   else if (px >= 10 && vx > 0) then Ev (MkBall [2*10 - px,py] [-vx,vy])
--   else NoEv
-- 
-- nextBall : Ball -> Either (Ball,Ball) Ball
-- nextBall b = case checkBounds b of
--   Ev b2 => Left  (b2,b2)
--   NoEv  => Right b
-- 
-- ballFrom : Monad m => (ini : Ball) -> SF m i Ball
-- ballFrom b =   const g
--            >>> velocity g b.vel
--            >>> fan [position b.vel b.pos,id] >>^ (\[p,v] => MkBall p v)
-- 
-- export
-- ballGame : Monad m => (ini : Ball) -> SF m i Ball
-- ballGame ini = resetOn ballFrom checkBounds ini 
-- 
-- export
-- balls : Monad m => (inis : List Ball) -> SF m i (List Ball)
-- balls = traverse ballGame
