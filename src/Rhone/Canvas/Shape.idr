module Rhone.Canvas.Shape

import Control.MonadRec
import Data.Iterable
import JS
import Rhone.Canvas.Angle
import Web.Html

%default total

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

namespace PathType
  public export
  data PathType = Fill | Stroke

public export
data Segment : Type where
  Move  :  (x,y : Double) -> Segment
  Line  :  (x,y : Double) -> Segment
  Arc   :  (x,y,radius       : Double)
        -> (start,stop       : Angle)
        -> (counterclockwise : Bool)
        -> Segment
  ArcTo :  (x1,y1,x2,y2,radius : Double) -> Segment

namespace RectType
  public export
  data RectType = Fill | Stroke | Clear

public export
data Shape : Type where
  Rect   : (x,y,w,h : Double) -> RectType -> Shape
  Path   : List Segment -> PathType -> Shape
  Shapes : List Shape -> Shape

export
circle : (x,y,radius : Double) -> PathType -> Shape
circle x y r = Path [Arc x y r (rad 0) (rad $ 2 * pi) False]

export
Semigroup Shape where
  x         <+> Shapes [] = x
  Shapes [] <+> y         = y
  Shapes xs <+> Shapes ys = Shapes $ xs ++ ys
  x         <+> Shapes ys = Shapes $ x :: ys
  x         <+> y         = Shapes [x,y]

export
Monoid Shape where
  neutral = Shapes []

--------------------------------------------------------------------------------
--          IO
--------------------------------------------------------------------------------

applySegment : CanvasRenderingContext2D -> Segment -> JSIO () 
applySegment ctxt (Move x y) = moveTo ctxt x y
applySegment ctxt (Line x y) = lineTo ctxt x y
applySegment ctxt (Arc x y r start stop ccw) = do
  putStrLn #"Arc: \#{show x} \#{show y} \#{show r} \#{show start} \#{show stop} \#{show ccw}"#
  arc ctxt x y r (toRadians start) (toRadians stop) (Def ccw)
applySegment ctxt (ArcTo x1 y1 x2 y2 radius) =
  arcTo ctxt x1 y1 x2 y2 radius

mutual
  export
  applyAll : CanvasRenderingContext2D -> List Shape -> JSIO ()
  applyAll ctxt = assert_total $ forM_ (apply ctxt)

  export
  apply : CanvasRenderingContext2D -> Shape -> JSIO ()
  apply ctxt (Rect x y w h Fill)   = fillRect ctxt x y w h
  apply ctxt (Rect x y w h Stroke) = strokeRect ctxt x y w h
  apply ctxt (Rect x y w h Clear)  = clearRect ctxt x y w h
  apply ctxt (Path ss st)          = do
    beginPath ctxt
    forM_ (applySegment ctxt) ss
    case st of
      Fill   => fill ctxt Undef
      Stroke => stroke ctxt
  apply ctxt (Shapes xs)           = applyAll ctxt xs
