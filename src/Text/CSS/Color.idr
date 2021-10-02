module Text.CSS.Color

import Data.Bits
import Data.Maybe

%default total

--------------------------------------------------------------------------------
--          Bits8
--------------------------------------------------------------------------------

public export
toHexChar : Bits8 -> Char
toHexChar n =
  if n < 10 then chr (ord '0' + cast n)
  else chr (ord 'a' + cast n - 10)

public export
toHex : Bits8 -> String
toHex n = pack [ toHexChar $ n `shiftR` fromNat 4
               , toHexChar $ n .&. 0xf
               ]

public export
fromHexChar : Char -> Maybe Bits8
fromHexChar c =
  if c >= '0' && c <= '9' then Just $ cast (ord c - ord '0')
  else if c >= 'a' && c <= 'f' then Just $ 10 + cast (ord c - ord 'a')
  else Nothing

public export
fromHex : Char -> Char -> Maybe Bits8
fromHex c1 c2 = [| calc (fromHexChar c1) (fromHexChar c2) |]
  where
    calc : Bits8 -> Bits8 -> Bits8
    calc x y = x * 0x10 + y

--------------------------------------------------------------------------------
--          Color
--------------------------------------------------------------------------------

public export
record Color where
  constructor MkColor
  red   : Bits8
  green : Bits8
  blue  : Bits8

public export
render : Color -> String
render (MkColor r g b) = "#" ++ toHex r ++ toHex g ++ toHex b

public export
fromHexString : String -> Maybe Color
fromHexString s =
  case unpack s of
    ['#',r1,r2,g1,g2,b1,b2] =>
      [| MkColor (fromHex r1 r2) (fromHex g1 g2) (fromHex b1 b2) |]
    _ => Nothing

export
Show Color where show = render

public export
fromString : (s : String) -> {auto 0 _ : IsJust (fromHexString s) } -> Color
fromString s = fromJust $ fromHexString s

--------------------------------------------------------------------------------
--          X11 Colors (https://www.w3.org/TR/css3-color/#svg-color)
--------------------------------------------------------------------------------

export
aliceblue : Color
aliceblue = MkColor 240 248 255

export
antiquewhite : Color
antiquewhite = MkColor 250 235 215

export
aqua : Color
aqua = MkColor 0 255 255

export
aquamarine : Color
aquamarine = MkColor 127 255 212

export
azure : Color
azure = MkColor 240 255 255

export
beige : Color
beige = MkColor 245 245 220

export
bisque : Color
bisque = MkColor 255 228 196

export
blanchedalmond : Color
blanchedalmond = MkColor 255 235 205

export
black : Color
black = MkColor 0 0 0

export
blue : Color
blue = MkColor 0 0 255

export
blueviolet : Color
blueviolet = MkColor 138 43 226

export
brown : Color
brown = MkColor 165 42 42

export
burlywood : Color
burlywood = MkColor 222 184 135

export
cadetblue : Color
cadetblue = MkColor 95 158 160

export
chartreuse : Color
chartreuse = MkColor 127 255 0

export
chocolate : Color
chocolate = MkColor 210 105 30

export
coral : Color
coral = MkColor 255 127 80

export
cornflowerblue : Color
cornflowerblue = MkColor 100 149 237

export
cornsilk : Color
cornsilk = MkColor 255 248 220

export
crimson : Color
crimson = MkColor 220 20 60

export
cyan : Color
cyan = MkColor 0 255 255

export
darkblue : Color
darkblue = MkColor 0 0 139

export
darkcyan : Color
darkcyan = MkColor 0 139 139

export
darkgoldenrod : Color
darkgoldenrod = MkColor 184 134 11

export
darkgray : Color
darkgray = MkColor 169 169 169

export
darkgreen : Color
darkgreen = MkColor 0 100 0

export
darkgrey : Color
darkgrey = MkColor 169 169 169

export
darkkhaki : Color
darkkhaki = MkColor 189 183 107

export
darkmagenta : Color
darkmagenta = MkColor 139 0 139

export
darkolivegreen : Color
darkolivegreen = MkColor 85 107 47

export
darkorange : Color
darkorange = MkColor 255 140 0

export
darkorchid : Color
darkorchid = MkColor 153 50 204

export
darkred : Color
darkred = MkColor 139 0 0

export
darksalmon : Color
darksalmon = MkColor 233 150 122

export
darkseagreen : Color
darkseagreen = MkColor 143 188 143

export
darkslateblue : Color
darkslateblue = MkColor 72 61 139

export
darkslategray : Color
darkslategray = MkColor 47 79 79

export
darkslategrey : Color
darkslategrey = MkColor 47 79 79

export
darkturquoise : Color
darkturquoise = MkColor 0 206 209

export
darkviolet : Color
darkviolet = MkColor 148 0 211

export
deeppink : Color
deeppink = MkColor 255 20 147

export
deepskyblue : Color
deepskyblue = MkColor 0 191 255

export
dimgray : Color
dimgray = MkColor 105 105 105

export
dimgrey : Color
dimgrey = MkColor 105 105 105

export
dodgerblue : Color
dodgerblue = MkColor 30 144 255

export
firebrick : Color
firebrick = MkColor 178 34 34

export
floralwhite : Color
floralwhite = MkColor 255 250 240

export
forestgreen : Color
forestgreen = MkColor 34 139 34

export
fuchsia : Color
fuchsia = MkColor 255 0 255

export
gainsboro : Color
gainsboro = MkColor 220 220 220

export
ghostwhite : Color
ghostwhite = MkColor 248 248 255

export
gold : Color
gold = MkColor 255 215 0

export
goldenrod : Color
goldenrod = MkColor 218 165 32

export
gray : Color
gray = MkColor 128 128 128

export
green : Color
green = MkColor 0 128 0

export
greenyellow : Color
greenyellow = MkColor 173 255 47

export
grey : Color
grey = MkColor 128 128 128

export
honeydew : Color
honeydew = MkColor 240 255 240

export
hotpink : Color
hotpink = MkColor 255 105 180

export
indianred : Color
indianred = MkColor 205 92 92

export
indigo : Color
indigo = MkColor 75 0 130

export
ivory : Color
ivory = MkColor 255 255 240

export
khaki : Color
khaki = MkColor 240 230 140

export
lavender : Color
lavender = MkColor 230 230 250

export
lavenderblush : Color
lavenderblush = MkColor 255 240 245

export
lawngreen : Color
lawngreen = MkColor 124 252 0

export
lemonchiffon : Color
lemonchiffon = MkColor 255 250 205

export
lightblue : Color
lightblue = MkColor 173 216 230

export
lightcoral : Color
lightcoral = MkColor 240 128 128

export
lightcyan : Color
lightcyan = MkColor 224 255 255

export
lightgoldenrodyellow : Color
lightgoldenrodyellow = MkColor 250 250 210

export
lightgray : Color
lightgray = MkColor 211 211 211

export
lightgreen : Color
lightgreen = MkColor 144 238 144

export
lightgrey : Color
lightgrey = MkColor 211 211 211

export
lightpink : Color
lightpink = MkColor 255 182 193

export
lightsalmon : Color
lightsalmon = MkColor 255 160 122

export
lightseagreen : Color
lightseagreen = MkColor 32 178 170

export
lightskyblue : Color
lightskyblue = MkColor 135 206 250

export
lightslategray : Color
lightslategray = MkColor 119 136 153

export
lightslategrey : Color
lightslategrey = MkColor 119 136 153

export
lightsteelblue : Color
lightsteelblue = MkColor 176 196 222

export
lightyellow : Color
lightyellow = MkColor 255 255 224

export
lime : Color
lime = MkColor 0 255 0

export
limegreen : Color
limegreen = MkColor 50 205 50

export
linen : Color
linen = MkColor 250 240 230

export
magenta : Color
magenta = MkColor 255 0 255

export
maroon : Color
maroon = MkColor 128 0 0

export
mediumaquamarine : Color
mediumaquamarine = MkColor 102 205 170

export
mediumblue : Color
mediumblue = MkColor 0 0 205

export
mediumorchid : Color
mediumorchid = MkColor 186 85 211

export
mediumpurple : Color
mediumpurple = MkColor 147 112 219

export
mediumseagreen : Color
mediumseagreen = MkColor 60 179 113

export
mediumslateblue : Color
mediumslateblue = MkColor 123 104 238

export
mediumspringgreen : Color
mediumspringgreen = MkColor 0 250 154

export
mediumturquoise : Color
mediumturquoise = MkColor 72 209 204

export
mediumvioletred : Color
mediumvioletred = MkColor 199 21 133

export
midnightblue : Color
midnightblue = MkColor 25 25 112

export
mintcream : Color
mintcream = MkColor 245 255 250

export
mistyrose : Color
mistyrose = MkColor 255 228 225

export
moccasin : Color
moccasin = MkColor 255 228 181

export
navajowhite : Color
navajowhite = MkColor 255 222 173

export
navy : Color
navy = MkColor 0 0 128

export
oldlace : Color
oldlace = MkColor 253 245 230

export
olive : Color
olive = MkColor 128 128 0

export
olivedrab : Color
olivedrab = MkColor 107 142 35

export
orange : Color
orange = MkColor 255 165 0

export
orangered : Color
orangered = MkColor 255 69 0

export
orchid : Color
orchid = MkColor 218 112 214

export
palegoldenrod : Color
palegoldenrod = MkColor 238 232 170

export
palegreen : Color
palegreen = MkColor 152 251 152

export
paleturquoise : Color
paleturquoise = MkColor 175 238 238

export
palevioletred : Color
palevioletred = MkColor 219 112 147

export
papayawhip : Color
papayawhip = MkColor 255 239 213

export
peachpuff : Color
peachpuff = MkColor 255 218 185

export
peru : Color
peru = MkColor 205 133 63

export
pink : Color
pink = MkColor 255 192 203

export
plum : Color
plum = MkColor 221 160 221

export
powderblue : Color
powderblue = MkColor 176 224 230

export
purple : Color
purple = MkColor 128 0 128

export
red : Color
red = MkColor 255 0 0

export
rosybrown : Color
rosybrown = MkColor 188 143 143

export
royalblue : Color
royalblue = MkColor 65 105 225

export
saddlebrown : Color
saddlebrown = MkColor 139 69 19

export
salmon : Color
salmon = MkColor 250 128 114

export
sandybrown : Color
sandybrown = MkColor 244 164 96

export
seagreen : Color
seagreen = MkColor 46 139 87

export
seashell : Color
seashell = MkColor 255 245 238

export
sienna : Color
sienna = MkColor 160 82 45

export
silver : Color
silver = MkColor 192 192 192

export
skyblue : Color
skyblue = MkColor 135 206 235

export
slateblue : Color
slateblue = MkColor 106 90 205

export
slategray : Color
slategray = MkColor 112 128 144

export
slategrey : Color
slategrey = MkColor 112 128 144

export
snow : Color
snow = MkColor 255 250 250

export
springgreen : Color
springgreen = MkColor 0 255 127

export
steelblue : Color
steelblue = MkColor 70 130 180

export
tan : Color
tan = MkColor 210 180 140

export
teal : Color
teal = MkColor 0 128 128

export
thistle : Color
thistle = MkColor 216 191 216

export
tomato : Color
tomato = MkColor 255 99 71

export
turquoise : Color
turquoise = MkColor 64 224 208

export
violet : Color
violet = MkColor 238 130 238

export
wheat : Color
wheat = MkColor 245 222 179

export
white : Color
white = MkColor 255 255 255

export
whitesmoke : Color
whitesmoke = MkColor 245 245 245

export
yellow : Color
yellow = MkColor 255 255 0

export
yellowgreen : Color
yellowgreen = MkColor 154 205 50
