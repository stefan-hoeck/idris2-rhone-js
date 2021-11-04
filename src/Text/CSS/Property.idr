module Text.CSS.Property

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
  render : Direction -> String
  render LTR = "ltr"
  render RTL = "rtl"

namespace Display
  public export
  data Display : Type where
    Flex : Display
    Grid : Display

  export
  render : Display -> String
  render Flex = "flex"
  render Grid = "grid"

namespace FlexBasis
  public export
  data FlexBasis : Type where
    FL       : Length -> FlexBasis
    FP       : Percentage -> FlexBasis

  export
  render : FlexBasis -> String
  render (FL x)   = render x
  render (FP x)   = render x

  export %inline
  FromLength FlexBasis where
    fromLength = FL

  export %inline
  FromPercentage FlexBasis where
    fromPercentage = FP

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
  render : FontSize -> String
  render (FL x)   = render x
  render (FP x)   = render x
  render XXSmall  = "xx-small"
  render XSmall   = "x-small"
  render Small    = "small"
  render Medium   = "medium"
  render Large    = "large"
  render XLarge   = "x-large"
  render XXLarge  = "xx-large"
  render XXXLarge = "xxx-large"

  export %inline
  FromLength FontSize where
    fromLength = FL

  export %inline
  FromPercentage FontSize where
    fromPercentage = FP

namespace BorderRadius
  public export
  data BorderRadius : Type where
    BL : Length -> BorderRadius
    BP : Percentage -> BorderRadius
    BS : String -> BorderRadius

  export
  render : BorderRadius -> String
  render (BL x) = render x
  render (BP x) = render x
  render (BS x) = x

  export %inline
  FromLength BorderRadius where
    fromLength = BL

  export %inline
  FromPercentage BorderRadius where
    fromPercentage = BP

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
  render : BorderStyle -> String
  render None   = "none"
  render Hidden = "hidden"
  render Dotted = "dotted"
  render Dashed = "dashed"
  render Solid  = "solid"
  render Dbl    = "double"
  render Groove = "groove"
  render Ridge  = "ridge"
  render Inset  = "inset"
  render Outset = "outset"

namespace BorderWidth
  public export
  data BorderWidth : Type where
    BL     : Length -> BorderWidth
    Thin   : BorderWidth
    Medium : BorderWidth
    Thick  : BorderWidth

  export
  render : BorderWidth -> String
  render (BL x) = render x
  render Thin   = "thin"
  render Medium = "medium"
  render Thick  = "thick"

  export %inline
  FromLength BorderWidth where
    fromLength = BL

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
  render : TextAlign -> String
  render Start   = "start"
  render End     = "end"
  render Left    = "left"
  render Right   = "right"
  render Center  = "center"
  render Justify = "justify"

namespace Width
  public export
  data Width : Type where
    WL       : Length -> Width
    WP       : Percentage -> Width

  export
  render : Width -> String
  render (WL x)   = render x
  render (WP x)   = render x

  export %inline
  FromLength Width where
    fromLength = WL

  export %inline
  FromPercentage Width where
    fromPercentage = WP

public export
data Property : Type -> Type where
  AlignItems      : Property FlexAlign
  AlignSelf       : Property FlexAlign
  BackgroundColor : Property Color
  BackgroundSize  : Property Width
  BorderColor     : Property (Dir Color)
  BorderRadius    : Property BorderRadius
  BorderStyle     : Property (Dir BorderStyle)
  BorderWidth     : Property (Dir BorderWidth)
  Color           : Property Color
  ColumnGap       : Property Length
  Direction       : Property Direction
  Display         : Property Display
  Flex            : Property String
  FlexBasis       : Property FlexBasis
  FlexDirection   : Property FlexDirection
  FlexWrap        : Property String
  FontFamily      : Property String
  FontSize        : Property FontSize
  GridColumn      : Property GridPosition
  GridRow         : Property GridPosition
  GridTemplateColumns : Property (List GridValue)
  GridTemplateRows : Property (List GridValue)
  Height          : Property Width
  JustifyContent  : Property FlexJustify
  JustifySelf     : Property FlexJustify
  ListStyleType   : Property ListStyleType
  Margin          : Property (Dir Length)
  MaxHeight       : Property Width
  MaxWidth        : Property Width
  MinHeight       : Property Width
  MinWidth        : Property Width
  Padding         : Property (Dir Length)
  RowGap          : Property Length
  TextAlign       : Property TextAlign
  Width           : Property Width

export
renderProp : Property t -> t -> String
renderProp AlignItems y      = "align-items: "      ++ render y
renderProp AlignSelf y       = "align-self: "       ++ render y
renderProp BackgroundColor y = "background-color: " ++ render y
renderProp BackgroundSize y  = "background-size: "  ++ render y
renderProp BorderColor y     = render2 "border" "color" render y
renderProp BorderRadius y    = "border-radius: "    ++ render y
renderProp BorderStyle y     = render2 "border" "style" render y
renderProp BorderWidth y     = render2 "border" "width" render y
renderProp Color y           = "color: "            ++ render y
renderProp ColumnGap y       = "column-gap: "       ++ render y
renderProp Direction y       = "direction: "        ++ render y
renderProp Display y         = "display: "          ++ render y
renderProp Flex y            = "flex: "             ++ y
renderProp FlexBasis y       = "flex-basis: "       ++ render y
renderProp FlexWrap y        = "flex-wrap: "        ++ y
renderProp FlexDirection y   = "flex-direction: "   ++ render y
renderProp FontFamily y      = "font-family: "      ++ y
renderProp FontSize y        = "font-size: "        ++ render y
renderProp GridColumn y      = "grid-column: " ++ render y
renderProp GridRow y        = "grid-row: " ++ render y
renderProp GridTemplateColumns y = "grid-template-columns: " ++ render y
renderProp GridTemplateRows y    = "grid-template-rows: " ++ render y
renderProp Height y          = "height: "           ++ render y
renderProp JustifyContent y  = "justify-content: "  ++ render y
renderProp JustifySelf y     = "justify-self: "  ++ render y
renderProp Margin y          = render "margin"  render y
renderProp MaxHeight y       = "max-height: "       ++ render y
renderProp MaxWidth y        = "max-width: "        ++ render y
renderProp MinHeight y       = "min-height: "       ++ render y
renderProp MinWidth y        = "min-width: "        ++ render y
renderProp Padding y         = render "padding" render y
renderProp ListStyleType y   = "list-style-type: "  ++ render y
renderProp RowGap y          = "row-gap: "       ++ render y
renderProp TextAlign y       = "text-align: "       ++ render y
renderProp Width y           = "width: "            ++ render y
