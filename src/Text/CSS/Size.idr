module Text.CSS.Size

%default total

public export
data Size : Type where
  Pt       : Bits16 -> Size
  Px       : Bits16 -> Size
  Em       : Double -> Size
  Rem      : Double -> Size
  Perc     : Bits16 -> Size

export
render : Size -> String
render (Pt x)   = show x ++ "pt"
render (Px x)   = show x ++ "px"
render (Em x)   = show x ++ "em"
render (Rem x)  = show x ++ "rem"
render (Perc x) = show x ++ "%"
