module Rhone.Canvas.Style

import Data.SOP
import JS
import Text.CSS.Color
import Web.Html

%default total

public export
data Style : Type where
  Fill      : Color -> Style
  Stroke    : Color -> Style
  LineWidth : Double -> Style

--------------------------------------------------------------------------------
--          IO
--------------------------------------------------------------------------------

export
apply : CanvasRenderingContext2D -> Style -> JSIO ()
apply ctxt (Fill c)      = fillStyle ctxt   .= inject (render c)
apply ctxt (Stroke c)    = strokeStyle ctxt .= inject (render c)
apply ctxt (LineWidth v) = lineWidth ctxt   .= v
