module Control.Monad.Dom.Event

import JS
import Web.Dom
import Web.Raw.UIEvents

%default total

%foreign "browser:lambda:(et)=> et.target.id === undefined ? '' : et.target.id"
prim__etId : Event -> PrimIO String

etId : HasIO io => Event -> io String
etId et = primIO $ prim__etId et

--------------------------------------------------------------------------------
--          Mouse Event
--------------------------------------------------------------------------------

public export
record MouseInfo where
  constructor MkMouseInfo
  id    : String
  shift : Bool
  meta  : Bool

export
toMouseInfo : MouseEvent -> JSIO MouseInfo
toMouseInfo e =
  [| MkMouseInfo (etId $ up e) (shiftKey e) (metaKey e) |]

--------------------------------------------------------------------------------
--          Event
--------------------------------------------------------------------------------

public export
data DomEvent : Type where
  Click    : (info : MouseInfo) -> DomEvent
  DblClick : (info : MouseInfo) -> DomEvent
