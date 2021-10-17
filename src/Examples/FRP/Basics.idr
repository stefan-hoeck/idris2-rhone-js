module Examples.FRP.Basics

import Control.Monad.Reader
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

||| A signal function
public export
SF : (m : Type -> Type) -> (i,o : Type) -> Type
SF m i o = MSF (ReaderT DTime m) i o

||| An event stream
public export
ES : (m : Type -> Type) -> (i,o : Type) -> Type
ES m i o = SF m i (Event o)

||| Local time of a signal function (since when it
||| has been running)
public export
Time : Type
Time = Nat

||| Read the current time delta
export
dtime : Monad m => SF m i DTime
dtime = ask

||| The local time passed since initializing the signal
||| function
export
time : Monad m => SF m i Time
time = dtime >>> accumulateWith (+) 0

--------------------------------------------------------------------------------
--          Integration
--------------------------------------------------------------------------------

calc : VectorSpace o => NP I [o, DTime] -> (o,o) -> NP I [(o,o),o]
calc [new,dt] (acc,prev) =
  let acc2 = acc ^+^  ((cast dt / 2) *^ (prev ^+^ new))
   in [(acc2,new),acc2]

export
integralFrom : (Monad m, VectorSpace o) => o -> SF m o o
integralFrom a0 = fan [id,dtime] >>> mealy calc (a0,a0)

export
integral : (Monad m, VectorSpace o) => SF m o o
integral = integralFrom zeroVector

--------------------------------------------------------------------------------
--          Running a Signal Function
--------------------------------------------------------------------------------

delta : (current,last : Bits32) -> NP I [Bits32,DTime]
delta c l = if l == 0 then [c,0] else [c, cast $ c - l]

export
realTimeDelta : HasIO m => MSF m i DTime
realTimeDelta =constM currentTime >>> mealy delta 0

export
runSF : Monad m => MSF m i DTime -> SF m i o -> MSF m i o
runSF dt sf = fan [dt, id] >>> unReader sf
