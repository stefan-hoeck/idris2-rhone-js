module Text.CSS.Percentage

import Language.Reflection.Refined
import public Language.Reflection.Refined.Util
import Text.CSS.Render

%language ElabReflection
%default total

||| A floating point percentage value in the the
||| range [0,100].
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
Render Percentage where
  render (MkPercentage v _) = show v ++ "%"

||| Convenience function for creating percentages with little
||| syntactic overhead.
|||
||| ```idris example
||| perc 12
||| ```
export %inline
perc :  Cast Percentage a
     => (v : Double)
     -> {auto 0 prf : So (0 <= v && v <= 100)}
     -> a
perc v = cast $ MkPercentage v prf
