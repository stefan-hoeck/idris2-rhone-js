module Text.CSS.Property

import Text.CSS.Color
import Text.CSS.Dir
import Text.CSS.Flexbox
import Text.CSS.ListStyleType
import Text.CSS.Size

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

  export
  render : Display -> String
  render Flex = "flex"

namespace FontSize
  public export
  data FontSize : Type where
    FS       : Size -> FontSize
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
  render (FS x)   = render x
  render XXSmall  = "xx-small"
  render XSmall   = "x-small"
  render Small    = "small"
  render Medium   = "medium"
  render Large    = "large"
  render XLarge   = "x-large"
  render XXLarge  = "xx-large"
  render XXXLarge = "xxx-large"

  export %inline
  pt : Bits16 -> FontSize
  pt = FS . Pt

  export %inline
  px : Bits16 -> FontSize
  px = FS . Px

  export %inline
  em : Double -> FontSize
  em = FS . Em

  export %inline
  rem : Double -> FontSize
  rem = FS . Rem

  export %inline
  perc : Bits16 -> FontSize
  perc = FS . Perc

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
    BW     : Size -> BorderWidth
    Thin   : BorderWidth
    Medium : BorderWidth
    Thick  : BorderWidth

  export
  render : BorderWidth -> String
  render (BW x) = render x
  render Thin   = "thin"
  render Medium = "medium"
  render Thick  = "thick"

namespace Width
  public export
  data Width : Type where
    WI       : Size -> Width

  export
  render : Width -> String
  render (WI x)   = render x

  export %inline
  pt : Bits16 -> Width
  pt = WI . Pt

  export %inline
  px : Bits16 -> Width
  px = WI . Px

  export %inline
  em : Double -> Width
  em = WI . Em

  export %inline
  rem : Double -> Width
  rem = WI . Rem

  export %inline
  perc : Bits16 -> Width
  perc = WI . Perc

public export
data Property : Type -> Type where
  AlignItems      : Property FlexAlign
  AlignSelf       : Property FlexAlign
  BackgroundColor : Property Color
  BorderColor     : Property (Dir Color)
  BorderStyle     : Property (Dir BorderStyle)
  BorderWidth     : Property (Dir BorderWidth)
  Color           : Property Color
  Direction       : Property Direction
  Display         : Property Display
  Flex            : Property String
  FlexDirection   : Property FlexDirection
  FontSize        : Property FontSize
  Height          : Property Width
  JustifyContent  : Property FlexJustify
  Margin          : Property (Dir Size)
  MaxHeight       : Property Width
  MaxWidth        : Property Width
  MinHeight       : Property Width
  MinWidth        : Property Width
  Padding         : Property (Dir Size)
  ListStyleType   : Property ListStyleType
  Width           : Property Width

export
renderProp : Property t -> t -> String
renderProp AlignItems y      = "align-items: "      ++ render y
renderProp AlignSelf y       = "align-self: "       ++ render y
renderProp BackgroundColor y = "background-color: " ++ render y
renderProp BorderColor y     = render "border-color" render y
renderProp BorderStyle y     = render "border-style" render y
renderProp BorderWidth y     = render "border-width" render y
renderProp Color y           = "color: "            ++ render y
renderProp Direction y       = "direction: "        ++ render y
renderProp Display y         = "display: "          ++ render y
renderProp Flex y            = "flex: "             ++ y
renderProp FlexDirection y   = "flex-direction: "   ++ render y
renderProp FontSize y        = "font-size: "        ++ render y
renderProp Height y          = "height: "           ++ render y
renderProp JustifyContent y  = "justify-content: "  ++ render y
renderProp Margin y          = render "margin"  render y
renderProp MaxHeight y       = "max-height: "       ++ render y
renderProp MaxWidth y        = "max-width: "        ++ render y
renderProp MinHeight y       = "min-height: "       ++ render y
renderProp MinWidth y        = "min-width: "        ++ render y
renderProp Padding y         = render "padding" render y
renderProp ListStyleType y   = "list-style-type: "  ++ render y
renderProp Width y           = "width: "            ++ render y
