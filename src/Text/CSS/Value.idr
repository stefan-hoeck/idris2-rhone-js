module Text.CSS.Value

public export
record Color where
  constructor MkColor
  red   : Bits8
  green : Bits8
  blue  : Bits8
