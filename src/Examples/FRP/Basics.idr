module Examples.FRP.Basics

import Control.Monad.Reader
import Data.IORef
import Data.MSF
import Data.MSF.Trans
import Data.VectorSpace
import Rhone.JS

%hide Web.Dom.Alias.Time

||| Time passed since the last evaluation step.
||| The resolution is in ms, since we plan to
||| use JS `getTime` to calculate the delta.
public export
DTime : Type
DTime = Nat

||| Local time of a signal function (since when it
||| has been running)
public export
Time : Type
Time = Nat

||| Read the current time delta
export
dtime : (dt : m DTime) => MSF m i DTime
dtime = constM dt

||| The local time passed since initializing the signal
||| function
export
time : m DTime => MSF m i Time
time = dtime >>> accumulateWith (+) 0

--------------------------------------------------------------------------------
--          Integration
--------------------------------------------------------------------------------

calc : VectorSpace o => NP I [o, DTime] -> (o,o) -> NP I [(o,o),o]
calc [new,dt] (acc,prev) =
  let acc2 = acc ^+^  ((cast dt / 2) *^ (prev ^+^ new))
   in [(acc2,new),acc2]

export
integralFrom : VectorSpace o => m DTime => o -> MSF m o o
integralFrom a0 = fan [id,dtime] >>> mealy calc (a0,a0)

export
integral : VectorSpace o => m DTime => MSF m o o
integral = integralFrom zeroVector

--------------------------------------------------------------------------------
--          Running a Signal Function
--------------------------------------------------------------------------------

delta : (current,last : Bits32) -> NP I [Bits32,DTime]
delta c l = if l == 0 then [c,0] else [c, cast $ c - l]

export
timer : HasIO io => io (io DTime, io ())
timer = do
  ref <- newIORef {a = NP I [Bits32,DTime]} [0,0]

  pure ( map (\[_,dt] => dt) (readIORef ref)
       , do nt <- currentTime
            modifyIORef ref $ \[old,_] => delta nt old
       )


export
realTimeDelta : HasIO m => MSF m i DTime
realTimeDelta = constM currentTime >>> mealy delta 0
