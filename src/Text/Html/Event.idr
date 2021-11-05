module Text.Html.Event

import Data.Maybe

%default total

--------------------------------------------------------------------------------
--          Event Info Types
--------------------------------------------------------------------------------

public export
record MouseInfo where
  constructor MkMouseInfo
  -- buttons
  button  : Int16
  buttons : Bits16

  -- coordinates
  clientX : Int32
  clientY : Int32
  screenX : Int32
  screenY : Int32

  -- keys
  alt     : Bool
  ctrl    : Bool
  meta    : Bool
  shift   : Bool

public export
record InputInfo where
  constructor MkInputInfo
  value : String

public export
record KeyInfo where
  constructor MkKeyInfo
  key         : String
  code        : String
  location    : Bits32
  isComposing : Bool

  -- control keys
  alt         : Bool
  ctrl        : Bool
  meta        : Bool
  shift       : Bool


--------------------------------------------------------------------------------
--          Events
--------------------------------------------------------------------------------

public export
data DOMEvent : Type -> Type where
  -- Mouse clicks
  Click      : (MouseInfo -> Maybe a) -> DOMEvent a
  DblClick   : (MouseInfo -> Maybe a) -> DOMEvent a
  MouseDown  : (MouseInfo -> Maybe a) -> DOMEvent a
  MouseUp    : (MouseInfo -> Maybe a) -> DOMEvent a

  -- Mouse movement
  MouseEnter : (MouseInfo -> Maybe a) -> DOMEvent a
  MouseLeave : (MouseInfo -> Maybe a) -> DOMEvent a
  MouseOver  : (MouseInfo -> Maybe a) -> DOMEvent a
  MouseOut   : (MouseInfo -> Maybe a) -> DOMEvent a
  MouseMove  : (MouseInfo -> Maybe a) -> DOMEvent a

  -- Focus
  Blur       : a -> DOMEvent a
  Focus      : a -> DOMEvent a

  -- Keyboard
  KeyDown    : (KeyInfo -> Maybe a) -> DOMEvent a
  KeyUp      : (KeyInfo -> Maybe a) -> DOMEvent a

  -- Keyboard
  Change     : (InputInfo -> Maybe a) -> DOMEvent a
  Input      : (InputInfo -> Maybe a) -> DOMEvent a
