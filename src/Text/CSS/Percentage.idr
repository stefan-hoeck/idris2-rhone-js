module Text.CSS.Percentage

%default total

public export
record Percentage where
  constructor MkPercentage
  value : Bits32

export
render : Percentage -> String
render (MkPercentage v) = show v ++ "%"

public export
interface FromPercentage a where
  fromPercentage : Percentage -> a

public export %inline
FromPercentage Percentage where
  fromPercentage = id

export %inline
perc : FromPercentage a => Bits32 -> a
perc = fromPercentage . MkPercentage
