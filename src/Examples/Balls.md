# Running Animations: Bouncing Balls

In this tutorial we are going to have a look at
running a (non-interactive) animation. We simulate
the frictionless movement of a group of balls under
the influence of gravitation in a two-dimensional
room.

The user interface will be very simple: Just a
validated text input for defining the number of
balls to animate and a button to (re)start the
animation. The main focus of the tutorial will
be the animation itself.

```idris
module Examples.Balls

import Data.Either
import Data.List.TR
import Data.MSF.Switch
import Data.Nat
import Data.Vect
import Data.VectorSpace

import Examples.CSS.Colors
import Examples.CSS.Balls
import Examples.Util

import Generics.Derive
import Rhone.Canvas
import Rhone.JS
import Text.CSS.Color

%language ElabReflection
%default total
```

## Model

We first define a couple of physical entities:

```idris
-- 2D Vector
V2 : Type
V2 = Vect 2 Double

-- Velocity of a point in 2D space
Velocity : Type
Velocity = V2

-- Acceleration of a point in 2D space
Acceleration : Type
Acceleration = V2

-- constant acceleration vector
acc : Acceleration
acc = [0,-9.81]

-- height and width of the room in m
w : Double
w = 10

-- start height of all balls
h0 : Double
h0 = 9

-- ball radius in m
r : Double
r = 0.1

-- start velocity in m/s
v0 : Double
v0 = 4
```

We need a data type to hold the current state of a
ball in motion: Its color, position and velocity:

```idris
record Ball where
  constructor MkBall
  col : Color
  pos : V2
  vel : Velocity
```

## View

We draw our set of balls in a canvas, so we need
some instructions for doing so. A ball will sometimes
move beyond its physical boundaries, in which case the
controller (see below) will adjust its direction
of movement and it will move back into the room.
To get the illusion of reflecting the ball at the
correct location, we hide the ball as long as it is
outside the room (this happens only for very short
moments due to the limited time resolution of
our animation):

```idris
inBounds : Ball -> Bool
inBounds (MkBall _ [x,y] _) = y >= 0 && x >= 0 && x <= w

ballToScene : Ball -> Scene
ballToScene b@(MkBall _ [x,y] _) =
  S1 [Fill $ if inBounds b then b.col else transparent] Id $
    circle x (w - y) r Fill
```

The utilities for describing and rendering a canvas scene
can be found at `Rhone.Canvas` and its submodules.

We also draw some primitive walls and a floor to visualize
the room:

```idris
-- room wall thickness in meters
wallThickness : Double
wallThickness = 0.20

-- walls and floor of the room.
walls : Shape
walls =
  let hwt = wallThickness / 2
   in polyLine [(-hwt, 0), (-hwt, w+hwt), (w+hwt,w+hwt), (w+hwt,0)]
```

We can now describe a scene of balls plus the room
at a given point in time:

```idris
ballsToScene : List Ball -> Scene
ballsToScene bs =
  SM  [] (Transform 50 0 0 50 10 10) $
    [ SM [] Id $ mapTR ballToScene bs
    , S1 [Stroke base80, LineWidth wallThickness] Id walls
    ]
```

Note again the call to `mapTR`: We hope to be able to
animate hundreds of balls at the same time, so we need
to make sure we are not going to overflow the limited
call stack.

Of course, we also need to set up the HTML objects of
our application plus the events they fire: `Next dt`
is used to move the animation forward by `dt`
milliseconds.

```idris
public export
data Ev = Run | NumIn | Next DTime

next : Ev -> Event Bits32
next (Next d) = Ev d
next _        = NoEv

%runElab derive "Ev" [Generic,Meta,Show,Eq]

-- canvas width and height
wcanvas : Bits32
wcanvas = 520

content : Node Ev
content =
  div [ class ballsContent ]
    [ lbl "Number of balls:" lblCount
    , input [ ref txtCount
            , onInput (const NumIn)
            , onEnterDown Run
            , class widget
            , placeholder "Range: [1,1000]"
            ] []
    , button [ref btnRun, onClick Run, classes [widget,btn]] ["Run"]
    , div [ref log] []
    , canvas [ref out, width wcanvas, height wcanvas] []
    ]
```

## Controller

The main focus of the controller will be to properly
animate the bouncing balls. We could try and use a functional
reactive programming approach implemented on top of
monadic stream functions. However, while MSFs perform
reasonably well, their monadic contexts will impose
some overhead, and we don't want that when animating
lots of objects (I tried this application first
with having a dedicated MSF for each individual ball:
Up to about 500 balls, performance was pretty good
at about 40 FPS on my machine, which is very
promising for programming interactive animations).

For calculating the next position and velocity vector
of a ball, we use simple Newtonian physics and some
help from the `VectorSpace` interface. We
also need some form of collision detection to make
sure our balls don't leave the room:

```idris
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
```

We also need a way to create an initial set of
balls based user input. We evenly distribute
them at a height of nine meters, giving them
slightly different colors and starting velocities:

```idris
||| Generates a list of balls to start the simulation.
export
initialBalls : (n : Nat) -> List Ball
initialBalls n = go n Nil
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
           in MkBall (col $ cast k `mod` 5) [x0,9] (v0 *^ [sin phi, cos phi])

        go : (k : Nat) -> List Ball -> List Ball
        go 0     bs = bs
        go (S k) bs = go k $ ball k :: bs
```

The controller handling the animation will use a state
accumulater for the balls and advance them the given
number of milliseconds:

```idris
public export
M : Type -> Type
M = DomIO Ev JSIO

renderBalls : List Ball -> M ()
renderBalls bs =
  liftJSIO . render $ MkCanvas out (cast wcanvas) (cast wcanvas) (ballsToScene bs)

animation : List Ball -> MSF M Ev ()
animation bs = arr next ?>-
                 [ accumulateWith (mapTR . nextBall) bs >>! renderBalls
                 , fps 15 ?>> showFPS ^>> text log
                 ]
```

We now only need to write the controllers for reading user
input and running the application:

```idris
read : String -> Either String (List Ball)
read s =
  let n = cast {to = Nat} s
   in if 0 < n && n <= 1000
        then Right (initialBalls n)
        else Left "Enter a number between 1 and 1000"


msf : MSF M Ev ()
msf = drswitchWhen neutral initialBalls animation
  where readInit : MSF M Ev (Either String (List Ball))
        readInit =    getInput NumIn read txtCount
                 >>>  observeWith (isLeft ^>> disabledAt btnRun)

        initialBalls : MSF M Ev (MSFEvent $ List Ball)
        initialBalls =   fan [readInit, is Run]
                     >>> rightOnEvent

export
ui : M (MSF M Ev (), JSIO ())
ui = do
  innerHtmlAt exampleDiv content
  h     <- handler <$> env
  clear <- animate (h . Next)
  pure (msf, liftIO clear)
```

<!-- vi: filetype=idris2
-->
