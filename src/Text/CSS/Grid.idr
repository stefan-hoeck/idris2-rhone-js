module Text.CSS.Grid

import Data.List
import Text.CSS.Length
import Text.CSS.Percentage

--------------------------------------------------------------------------------
--          Flex Values
--------------------------------------------------------------------------------

public export
record Flex where
  constructor MkFlex
  value : Bits16

public export
interface FromFlex a where
  fromFlex : Flex -> a

export %inline
fr : FromFlex a => Bits16 -> a
fr = fromFlex . MkFlex

namespace Flex
  export
  render : Flex -> String
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
FromLength MinMaxValue where
  fromLength = MML

export %inline
FromPercentage MinMaxValue where
  fromPercentage = MMP

export %inline
FromFlex MinMaxValue where
  fromFlex = MMF

namespace MinMaxValue
  export
  render : MinMaxValue -> String
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

namespace GridValue
  export
  render : GridValue -> String
  render (GL x)           = render x
  render (GP x)           = render x
  render (GF x)           = render x
  render (MinMax min max) = "minmax(\{render min}, \{render max})"
  render MaxContent       = "max-content"
  render MinContent       = "min-content"

namespace GridValues
  export
  render : List GridValue -> String
  render = fastConcat . intersperse " " . map render

export %inline
FromLength GridValue where
  fromLength = GL

export %inline
FromPercentage GridValue where
  fromPercentage = GP

export %inline
FromFlex GridValue where
  fromFlex = GF

--------------------------------------------------------------------------------
--          GridPosition
--------------------------------------------------------------------------------

public export
data GridPosition : Type where
  At     : Bits32 -> GridPosition
  FromTo : Bits32 -> Bits32 -> GridPosition

namespace GridPosition
  export
  render : GridPosition -> String
  render (At x)       = show x
  render (FromTo x y) = "\{show x} / \{show y}"
