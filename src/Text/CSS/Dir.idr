module Text.CSS.Dir

import Data.List

%default total

public export
data Dir : Type -> Type where
  All    : a -> Dir a
  Left   : a -> Dir a
  Right  : a -> Dir a
  Top    : a -> Dir a
  Bottom : a -> Dir a
  ||| Vertical and horizontal width
  VH   : (v, h : a) -> Dir a
  ||| Top, horizontal, bottom width
  THB  : (t, h, b : a) -> Dir a
  ||| Top, right, bottom, left
  TRBL : (t, r, b, l : a) -> Dir a

-- prefix
prfx : Dir a -> String
prfx (Left _)   = "-left"
prfx (Right _)  = "-right"
prfx (Top _)    = "-top"
prfx (Bottom _) = "-bottom"
prfx _          = ""

export
vals : Dir a -> List a
vals (All  x)       = [x]
vals (Left  x)      = [x]
vals (Right  x)     = [x]
vals (Top  x)       = [x]
vals (Bottom  x)    = [x]
vals (VH   v h)     = [v,h]
vals (THB  t h b)   = [t,h,b]
vals (TRBL t r b l) = [t,r,b,l]

export
render : (prop : String) -> (a -> String) -> Dir a -> String
render prop f d = 
  let vs  = fastConcat . intersperse " " . map f $ vals d
      pre = prfx d
   in #"\#{prop}\#{pre}: \#{vs}"#
