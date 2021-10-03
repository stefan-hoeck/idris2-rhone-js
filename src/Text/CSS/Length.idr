module Text.CSS.Length

%default total

public export
data Length : Type where
  Pt       : Bits16 -> Length
  Px       : Bits16 -> Length
  Em       : Double -> Length
  Rem      : Double -> Length

export
render : Length -> String
render (Pt x)   = show x ++ "pt"
render (Px x)   = show x ++ "px"
render (Em x)   = show x ++ "em"
render (Rem x)  = show x ++ "rem"

public export
interface FromLength a where
  fromLength : Length -> a

public export %inline
FromLength Length where
  fromLength = id

export %inline
pt : FromLength a => Bits16 -> a
pt = fromLength . Pt

export %inline
px : FromLength a => Bits16 -> a
px = fromLength . Px

export %inline
em : FromLength a => Double -> a
em = fromLength . Em

export %inline
rem : FromLength a => Double -> a
rem = fromLength . Rem
