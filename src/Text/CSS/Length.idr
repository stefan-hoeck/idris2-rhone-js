module Text.CSS.Length

import Text.CSS.Render

%default total

public export
data Length : Type where
  Pt       : Bits16 -> Length
  Px       : Bits16 -> Length
  Em       : Double -> Length
  Rem      : Double -> Length

export
Render Length where
  render (Pt x)   = show x ++ "pt"
  render (Px x)   = show x ++ "px"
  render (Em x)   = show x ++ "em"
  render (Rem x)  = show x ++ "rem"

export %inline
pt : Cast Length a => Bits16 -> a
pt = cast . Pt

export %inline
px : Cast Length a => Bits16 -> a
px = cast . Px

export %inline
em : Cast Length a => Double -> a
em = cast . Em

export %inline
rem : Cast Length a => Double -> a
rem = cast . Rem
