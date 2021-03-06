module Rhone.Canvas

import Control.Monad.Dom
import Data.List
import Data.SOP
import JS
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
context2D :  LiftJSIO m
          => ElemRef HTMLCanvasElement
          -> m CanvasRenderingContext2D
context2D ref = liftJSIO $ do
  canvas <- getElementByRef ref
  m      <- getContext' canvas "2d"
  case m >>= (\ns => extract CanvasRenderingContext2D ns) of
    Just c  => pure c
    Nothing => throwError $ Caught "Rhone.Canvas.context2d: No rendering context for canvas"

export
render : LiftJSIO m => Canvas -> m ()
render (MkCanvas ref w h scene) = liftJSIO $ do
  ctxt <- context2D ref
  apply ctxt $ Rect 0 0 w h Clear
  apply ctxt scene
