module Text.CSS.Property

import Data.List
import Data.String
import Data.Vect
import Text.CSS.Color
import Text.CSS.Dir
import Text.CSS.Flexbox
import Text.CSS.Grid
import Text.CSS.Length
import Text.CSS.ListStyleType
import Text.CSS.Percentage

%default total

namespace Direction
  public export
  data Direction = LTR | RTL

  export
  Interpolation Direction where
    interpolate LTR = "ltr"
    interpolate RTL = "rtl"

namespace Display
  public export
  interface AreaTag a where
    showTag : a -> String

  public export
  data Display : Type where
    Flex  : Display
    Grid  : Display
    Area  :  {0 n,m : Nat}
          -> {0 a : Type}
          -> AreaTag a
          => (rows    : Vect (S m) GridValue)
          -> (columns : Vect (S n) GridValue)
          -> (area    : Vect (S m) (Vect (S n) a))
          -> Display

  export
  renderArea :  AreaTag a
             => Vect (S m) GridValue
             -> Vect (S n) GridValue
             -> Vect (S m) (Vect (S n) a)
             -> String
  renderArea rs cs as =
    let rsStr = "grid-template-rows: \{toList rs}"
        csStr = "grid-template-columns: \{toList cs}"
        aStr  = fastConcat . intersperse " " . map col $ toList as
     in "display: grid; \{rsStr}; \{csStr}; grid-template-areas: \{aStr}"
    where col : Vect (S n) a -> String
          col vs =
            let str = concat . intersperse " " . map showTag $ toList vs
             in #""\#{str}""#

namespace FlexBasis
  public export
  data FlexBasis : Type where
    FL       : Length -> FlexBasis
    FP       : Percentage -> FlexBasis

  export
  Interpolation FlexBasis where
    interpolate (FL x)   = interpolate x
    interpolate (FP x)   = interpolate x

  export %inline
  Cast Length FlexBasis where
    cast = FL

  export %inline
  Cast Percentage FlexBasis where
    cast = FP

namespace FontSize
  public export
  data FontSize : Type where
    FL       : Length -> FontSize
    FP       : Percentage -> FontSize
    XXSmall  : FontSize
    XSmall   : FontSize
    Small    : FontSize
    Medium   : FontSize
    Large    : FontSize
    XLarge   : FontSize
    XXLarge  : FontSize
    XXXLarge : FontSize

  export
  Interpolation FontSize where
    interpolate (FL x)   = interpolate x
    interpolate (FP x)   = interpolate x
    interpolate XXSmall  = "xx-small"
    interpolate XSmall   = "x-small"
    interpolate Small    = "small"
    interpolate Medium   = "medium"
    interpolate Large    = "large"
    interpolate XLarge   = "x-large"
    interpolate XXLarge  = "xx-large"
    interpolate XXXLarge = "xxx-large"

  export %inline
  Cast Length FontSize where
    cast = FL

  export %inline
  Cast Percentage FontSize where
    cast = FP

namespace BorderRadius
  public export
  data BorderRadius : Type where
    BL : Length -> BorderRadius
    BP : Percentage -> BorderRadius
    BS : String -> BorderRadius

  export
  Interpolation BorderRadius where
    interpolate (BL x) = interpolate x
    interpolate (BP x) = interpolate x
    interpolate (BS x) = x

  export %inline
  Cast Length BorderRadius where
    cast = BL

  export %inline
  Cast Percentage BorderRadius where
    cast = BP

  export %inline
  FromString BorderRadius where
    fromString = BS

namespace BorderStyle
  public export
  data BorderStyle : Type where
    None   : BorderStyle
    Hidden : BorderStyle
    Dotted : BorderStyle
    Dashed : BorderStyle
    Solid  : BorderStyle
    Dbl    : BorderStyle
    Groove : BorderStyle
    Ridge  : BorderStyle
    Inset  : BorderStyle
    Outset : BorderStyle


  export
  Interpolation BorderStyle where
    interpolate None   = "none"
    interpolate Hidden = "hidden"
    interpolate Dotted = "dotted"
    interpolate Dashed = "dashed"
    interpolate Solid  = "solid"
    interpolate Dbl    = "double"
    interpolate Groove = "groove"
    interpolate Ridge  = "ridge"
    interpolate Inset  = "inset"
    interpolate Outset = "outset"

namespace BorderWidth
  public export
  data BorderWidth : Type where
    BL     : Length -> BorderWidth
    Thin   : BorderWidth
    Medium : BorderWidth
    Thick  : BorderWidth

  export
  Interpolation BorderWidth where
    interpolate (BL x) = interpolate x
    interpolate Thin   = "thin"
    interpolate Medium = "medium"
    interpolate Thick  = "thick"

  export %inline
  Cast Length BorderWidth where
    cast = BL

namespace TextAlign
  public export
  data TextAlign : Type where
    ||| The same as left if direction is left-to-right and right if direction is right-to-left.
    Start   : TextAlign
    ||| The same as right if direction is left-to-right and left if direction is right-to-left.
    End     : TextAlign
    ||| The inline contents are aligned to the left edge of the line box.
    Left    : TextAlign
    ||| The inline contents are aligned to the right edge of the line box.
    Right   : TextAlign
    ||| The inline contents are centered within the line box.
    Center  : TextAlign
    ||| The inline contents are justified. Text should be spaced to line up its left and right edges to the left and right edges of the line box, except for the last line.
    Justify : TextAlign

  export
  Interpolation TextAlign where
    interpolate Start   = "start"
    interpolate End     = "end"
    interpolate Left    = "left"
    interpolate Right   = "right"
    interpolate Center  = "center"
    interpolate Justify = "justify"

namespace Width
  public export
  data Width : Type where
    WL       : Length -> Width
    WP       : Percentage -> Width

  export
  Interpolation Width where
    interpolate (WL x)   = interpolate x
    interpolate (WP x)   = interpolate x

  export %inline
  Cast Length Width where
    cast = WL

  export %inline
  Cast Percentage Width where
    cast = WP
