module Text.CSS.Percentage

import Language.Reflection.Refined
import public Language.Reflection.Refined.Util

%language ElabReflection
%default total

public export
record Percentage where
  constructor MkPercentage
  value : Double
  0 prf : So (0 <= value && value <= 100)

%runElab refinedDouble "Percentage"

export
fromInteger :  (n : Integer)
            -> {auto 0 _ : IsJust (Percentage.refine (cast n))}
            -> Percentage
fromInteger n = fromJust $ Percentage.refine (cast n)

export
render : Percentage -> String
render (MkPercentage v _) = show v ++ "%"

public export
interface FromPercentage a where
  fromPercentage : Percentage -> a

public export %inline
FromPercentage Percentage where
  fromPercentage = id

export %inline
perc :  FromPercentage a
     => (v : Double)
     -> {auto 0 prf : So (0 <= v && v <= 100)}
     -> a
perc v = fromPercentage $ MkPercentage v prf
