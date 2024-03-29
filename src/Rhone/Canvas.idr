module Rhone.Canvas

import Data.List
import JS
import Rhone.JS.ElemRef
import Web.Html

import public Rhone.Canvas.Angle
import public Rhone.Canvas.Scene
import public Rhone.Canvas.Shape
import public Rhone.Canvas.Style
import public Rhone.Canvas.Transformation

%default total

public export
record Canvas where
  constructor MkCanvas
  ref           : ElemRef HTMLCanvasElement
  width, height : Double
  scene         : Scene

--------------------------------------------------------------------------------
--          IO
--------------------------------------------------------------------------------

export
context2D : ElemRef HTMLCanvasElement -> JSIO CanvasRenderingContext2D
context2D ref = do
  canvas <- getElementByRef ref
  m      <- getContext canvas "2d"
  case m >>= project CanvasRenderingContext2D of
    Just c  => pure c
    Nothing => throwError $ Caught "Rhone.Canvas.context2d: No rendering context for canvas"

export
render : Canvas -> JSIO ()
render (MkCanvas ref w h scene) = do
  ctxt <- context2D ref
  apply ctxt $ Rect 0 0 w h Clear
  apply ctxt scene
