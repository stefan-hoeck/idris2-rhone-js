module Rhone.Canvas.FillStyle

import Data.SOP
import JS
import Text.CSS.Color
import Web.Html

%default total

public export
data FillStyle : Type where
  Current : FillStyle
  Col     : Color -> FillStyle

--------------------------------------------------------------------------------
--          IO
--------------------------------------------------------------------------------

export
apply : CanvasRenderingContext2D -> FillStyle -> JSIO ()
apply ctxt (Col c) = fillStyle ctxt .= inject (render c)
apply ctxt Current = pure ()
