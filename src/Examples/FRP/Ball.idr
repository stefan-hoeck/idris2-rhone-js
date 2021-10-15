module Examples.FRP.Ball

import Control.Monad.Reader
import Data.MSF
import Data.MSF.Switch
import Data.MSF.Trans
import Data.Vect
import Data.VectorSpace
import Examples.FRP.Basics

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
g : Acceleration
g = [0,-9.81 / 1000000]

export
velocity : Monad m => (v0 : Velocity) -> SF m Acceleration Velocity
velocity = integralFrom

export
position : Monad m => (p0 : Position) -> SF m Velocity Position
position = integralFrom

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

checkBounds : Ball -> Event Ball
checkBounds (MkBall [px,py] [vx,vy]) =
  if      (py <= 0  && vy < 0) then Ev (MkBall [px,-py] [vx,-vy])
  else if (px <= 0  && vx < 0) then Ev (MkBall [-px,py] [-vx,vy])
  else if (px >= 10 && vx > 0) then Ev (MkBall [2*10 - px,py] [-vx,vy])
  else NoEv

nextBall : Ball -> Either (Ball,Ball) Ball
nextBall b = case checkBounds b of
  Ev b2 => Left (b2,b2)
  NoEv  => Right b

ballFrom : Monad m => (ini : Ball) -> SF m i Ball
ballFrom b =   const g
           >>> velocity b.vel
           >>> fan [position b.pos,id] >>^ (\[p,v] => MkBall p v)

export
ballGame : Monad m => (ini : Ball) -> SF m i Ball
ballGame ini = dswitch (ballFrom ini >>^ nextBall) ballGame

export
balls : Monad m => (inis : Vect n Ball) -> SF m i (Vect n Ball)
balls = traverse ballGame

ballToSVG : Ball -> String
ballToSVG (MkBall [x,y] _) =
  #"<circle cx="\#{show x}" cy="\#{show $ 10 - y}" r="0.3" fill="red"/>"#

export
ballsSVG : Vect n Ball -> String
ballsSVG balls =
  let header =
        the String #"""
                   <svg version="1.1"
                        width="50%"
                        viewBox="0 0 20 20"
                        xmlns="http://www.w3.org/2000/svg">
                   """#
   in #"""
      \#{header}
      <g transform="translate(5,5)">
      <polyline points="0, 0, 0, 10 10, 10 10, 0"
                fill="none"
                stroke="yellow"
                vector-effect="non-scaling-stroke"
                stroke-width="2"/>
      \#{concatMap ballToSVG balls}
      </g>
      </svg>
      """#
