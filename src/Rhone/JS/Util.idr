||| Utilities not (yet) available from idris2-dom
module Rhone.JS.Util

import JS

--------------------------------------------------------------------------------
--          Time
--------------------------------------------------------------------------------

%foreign "javascript:lambda:(w) => new Date().getTime()"
prim__time : PrimIO Bits32

||| Get the current time in milliseconds since 1970/01/01.
export
currentTime : HasIO io => io Bits32
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
