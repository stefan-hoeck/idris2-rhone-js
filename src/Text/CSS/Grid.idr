module Text.CSS.Grid

import Data.List
import Text.CSS.Length
import Text.CSS.Percentage
import Text.CSS.Render

--------------------------------------------------------------------------------
--          Flex Values
--------------------------------------------------------------------------------

public export
record Flex where
  constructor MkFlex
  value : Bits16

export %inline
fr : Cast Flex a => Bits16 -> a
fr = cast . MkFlex

export
Render Flex where
  render f = "\{show f.value}fr"

--------------------------------------------------------------------------------
--          MinMax Values
--------------------------------------------------------------------------------

public export
data MinMaxValue : Type where
  MML : Length     -> MinMaxValue
  MMP : Percentage -> MinMaxValue
  MMF : Flex       -> MinMaxValue

export %inline
Cast Length MinMaxValue where
  cast = MML

export %inline
Cast Percentage MinMaxValue where
  cast = MMP

export %inline
Cast Flex MinMaxValue where
  cast = MMF

export
Render MinMaxValue where
  render (MML x) = render x
  render (MMP x) = render x
  render (MMF x) = render x

--------------------------------------------------------------------------------
--          GridValue
--------------------------------------------------------------------------------

public export
data GridValue : Type where
  GL         : Length -> GridValue
  GP         : Percentage -> GridValue
  GF         : Flex -> GridValue
  MinMax     : (min,max : MinMaxValue) -> GridValue
  MaxContent : GridValue
  MinContent : GridValue

export
Render GridValue where
  render (GL x)           = render x
  render (GP x)           = render x
  render (GF x)           = render x
  render (MinMax min max) = "minmax(\{render min}, \{render max})"
  render MaxContent       = "max-content"
  render MinContent       = "min-content"

export
Render (List GridValue) where
  render = fastConcat . intersperse " " . map render

export %inline
Cast Length GridValue where
  cast = GL

export %inline
Cast Percentage GridValue where
  cast = GP

export %inline
Cast Flex GridValue where
  cast = GF

--------------------------------------------------------------------------------
--          GridPosition
--------------------------------------------------------------------------------

public export
data GridPosition : Type where
  At     : Bits32 -> GridPosition
  FromTo : Bits32 -> Bits32 -> GridPosition

export
Render GridPosition where
  render (At x)       = show x
  render (FromTo x y) = "\{show x} / \{show y}"
