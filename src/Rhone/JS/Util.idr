||| Utilities not (yet) available from idris2-dom
module Rhone.JS.Util

import Data.IORef
import Data.MSF
import JS

--------------------------------------------------------------------------------
--          Time
--------------------------------------------------------------------------------

%foreign "javascript:lambda:(w) => BigInt(new Date().getTime())"
prim__time : PrimIO Integer

||| Get the current time in milliseconds since 1970/01/01.
export
currentTime : HasIO io => io Integer
currentTime = primIO prim__time

--------------------------------------------------------------------------------
--          Timers
--------------------------------------------------------------------------------

||| ID used to identify and cancel a running timer.
public export
data IntervalID : Type where [external]

%foreign "browser:lambda:(n,h,w)=>setInterval(() => h(w),n)"
prim__setInterval : Bits32 -> IO () -> PrimIO IntervalID

%foreign "browser:lambda:(i,w)=>clearInterval(i)"
prim__clearInterval : IntervalID -> PrimIO ()

||| Sets a timer to repeatedly carry out the given IO action
||| after the given number of milliseconds.
|||
||| Returns an ID, which can be used with `clearInterval` to
||| cancel the timer.
export
setInterval : HasIO io => Bits32 -> JSIO () -> io (IntervalID)
setInterval millis run = primIO $ prim__setInterval millis (runJS run)

||| Cancel a running timer with the given ID.
export
clearInterval : HasIO io => IntervalID -> io ()
clearInterval id = primIO $ prim__clearInterval id

--------------------------------------------------------------------------------
--          Animations
--------------------------------------------------------------------------------

%foreign """
         browser:lambda:(h,w)=>{
            let previousTimeStamp;
            let stop = 0;

            function step(timestamp) {
              if (previousTimeStamp === undefined)
                previousTimeStamp = timestamp;
              const dtime = timestamp - previousTimeStamp;
              previousTimeStamp = timestamp;
              stop = h(dtime)(w);
              if (stop === 0) {
                window.requestAnimationFrame(step);
              }
            }

            window.requestAnimationFrame(step);
         }
         """
prim__animate : (Bits32 -> IO Bits32) -> PrimIO ()

||| Alias for a time delta in milliseconds
public export
DTime : Type
DTime = Bits32

||| Use `window.requestAnimationFrame` to repeatedly
||| animate the given function.
|||
||| The function takes the time delta (in milliseconds) since
||| the previous animation step as input.
|||
||| Returns a cleanup action, which can be run to
||| stop the running animation.
export
animate : HasIO io => (DTime -> JSIO ()) -> io (IO ())
animate run = do
  ref <- newIORef (the Bits32 0)
  primIO $ prim__animate (\dt => runJS (run dt) >> readIORef ref)
  pure (writeIORef ref 1)


export
showFPS : Bits32 -> String
showFPS n = #"FPS: \#{show n}"#

||| Averages the frames per second (FPS) of an animation
||| firing an event with the value every `n` steps.
export
fps : (n : Nat) -> MSF m DTime (Event Bits32)
fps n = mealy acc (n,0)
  where acc : DTime -> (Nat,DTime) -> HList [(Nat,DTime),Event Bits32]
        acc dt (0,tot)   = [(n,0),Ev $ (1000 * cast (S n)) `div` (tot + dt)]
        acc dt (S k,tot) = [(k, tot + dt), NoEv]
