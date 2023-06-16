module Text.Html.Node

import Data.String
import Text.Html.Attribute
import Text.Html.Event
import Web.Dom

%hide Web.Internal.DomTypes.Node

%default total

public export
data Node : (event : Type) -> Type where
  El    :  {0 ev : Type}
        -> (tag : String)
        -> List (Attribute ev)
        -> List (Node ev)
        -> Node ev

  Raw   : String -> Node ev

  Text  : String -> Node ev

  Empty : Node ev

public export %inline
FromString (Node ev) where
  fromString = Text

public export %inline
el :
     {tag : _}
  -> (0 et : ElementType tag e)
  -> List (Attribute ev)
  -> List (Node ev)
  -> Node ev
el _ = El tag

public export %inline
a : List (Attribute ev) -> List (Node ev) -> Node ev
a = el A

public export %inline
address : List (Attribute ev) -> List (Node ev) -> Node ev
address = el Address

public export %inline
area : List (Attribute ev) -> List (Node ev) -> Node ev
area = el Area

public export %inline
article : List (Attribute ev) -> List (Node ev) -> Node ev
article = el Article

public export %inline
audio : List (Attribute ev) -> List (Node ev) -> Node ev
audio = el Audio

public export %inline
base : List (Attribute ev) -> List (Node ev) -> Node ev
base = el Base

public export %inline
blockquote : List (Attribute ev) -> List (Node ev) -> Node ev
blockquote = el Blockquote

public export %inline
body : List (Attribute ev) -> List (Node ev) -> Node ev
body = el Body

public export %inline
br : List (Attribute ev) -> List (Node ev) -> Node ev
br = el Br

public export %inline
button : List (Attribute ev) -> List (Node ev) -> Node ev
button = el Button

public export %inline
canvas : List (Attribute ev) -> List (Node ev) -> Node ev
canvas = el Canvas

public export %inline
caption : List (Attribute ev) -> List (Node ev) -> Node ev
caption = el Caption

public export %inline
col : List (Attribute ev) -> List (Node ev) -> Node ev
col = el Col

public export %inline
colgroup : List (Attribute ev) -> List (Node ev) -> Node ev
colgroup = el Colgroup

public export %inline
data_ : List (Attribute ev) -> List (Node ev) -> Node ev
data_ = el Data

public export %inline
datalist : List (Attribute ev) -> List (Node ev) -> Node ev
datalist = el Datalist

public export %inline
del : List (Attribute ev) -> List (Node ev) -> Node ev
del = el Del

public export %inline
details : List (Attribute ev) -> List (Node ev) -> Node ev
details = el Details

public export %inline
dialog : List (Attribute ev) -> List (Node ev) -> Node ev
dialog = el Dialog

public export %inline
div : List (Attribute ev) -> List (Node ev) -> Node ev
div = el Div

public export %inline
dl : List (Attribute ev) -> List (Node ev) -> Node ev
dl = el Dl

public export %inline
embed : List (Attribute ev) -> List (Node ev) -> Node ev
embed = el Embed

public export %inline
fieldset : List (Attribute ev) -> List (Node ev) -> Node ev
fieldset = el FieldSet

public export %inline
footer : List (Attribute ev) -> List (Node ev) -> Node ev
footer = el Footer

public export %inline
form : List (Attribute ev) -> List (Node ev) -> Node ev
form = el Form

public export %inline
h1 : List (Attribute ev) -> List (Node ev) -> Node ev
h1 = el H1

public export %inline
h2 : List (Attribute ev) -> List (Node ev) -> Node ev
h2 = el H2

public export %inline
h3 : List (Attribute ev) -> List (Node ev) -> Node ev
h3 = el H3

public export %inline
h4 : List (Attribute ev) -> List (Node ev) -> Node ev
h4 = el H4

public export %inline
h5 : List (Attribute ev) -> List (Node ev) -> Node ev
h5 = el H5

public export %inline
h6 : List (Attribute ev) -> List (Node ev) -> Node ev
h6 = el H6

public export %inline
header : List (Attribute ev) -> List (Node ev) -> Node ev
header = el Header

public export %inline
hr : List (Attribute ev) -> List (Node ev) -> Node ev
hr = el HR

public export %inline
html : List (Attribute ev) -> List (Node ev) -> Node ev
html = el Html

public export %inline
iframe : List (Attribute ev) -> List (Node ev) -> Node ev
iframe = el IFrame

public export %inline
img : List (Attribute ev) -> List (Node ev) -> Node ev
img = el Img

public export %inline
input : List (Attribute ev) -> List (Node ev) -> Node ev
input = el Input

public export %inline
ins : List (Attribute ev) -> List (Node ev) -> Node ev
ins = el Ins

public export %inline
label : List (Attribute ev) -> List (Node ev) -> Node ev
label = el Label

public export %inline
legend : List (Attribute ev) -> List (Node ev) -> Node ev
legend = el Legend

public export %inline
li : List (Attribute ev) -> List (Node ev) -> Node ev
li = el Li

public export %inline
link : List (Attribute ev) -> List (Node ev) -> Node ev
link = el Link

public export %inline
map : List (Attribute ev) -> List (Node ev) -> Node ev
map = el Map

public export %inline
menu : List (Attribute ev) -> List (Node ev) -> Node ev
menu = el Menu

public export %inline
meta : List (Attribute ev) -> List (Node ev) -> Node ev
meta = el Meta

public export %inline
meter : List (Attribute ev) -> List (Node ev) -> Node ev
meter = el Meter

public export %inline
object : List (Attribute ev) -> List (Node ev) -> Node ev
object = el Object

public export %inline
ol : List (Attribute ev) -> List (Node ev) -> Node ev
ol = el Ol

public export %inline
optgroup : List (Attribute ev) -> List (Node ev) -> Node ev
optgroup = el OptGroup

public export %inline
option : List (Attribute ev) -> List (Node ev) -> Node ev
option = el Option

public export %inline
output : List (Attribute ev) -> List (Node ev) -> Node ev
output = el Output

public export %inline
p : List (Attribute ev) -> List (Node ev) -> Node ev
p = el P

public export %inline
param : List (Attribute ev) -> List (Node ev) -> Node ev
param = el Param

public export %inline
picture : List (Attribute ev) -> List (Node ev) -> Node ev
picture = el Picture

public export %inline
pre : List (Attribute ev) -> List (Node ev) -> Node ev
pre = el Pre

public export %inline
progress : List (Attribute ev) -> List (Node ev) -> Node ev
progress = el Progress

public export %inline
q : List (Attribute ev) -> List (Node ev) -> Node ev
q = el Q

public export %inline
script : List (Attribute ev) -> List (Node ev) -> Node ev
script = el Script

public export %inline
section : List (Attribute ev) -> List (Node ev) -> Node ev
section = el Section

public export %inline
select : List (Attribute ev) -> List (Node ev) -> Node ev
select = el Select

public export %inline
slot : List (Attribute ev) -> List (Node ev) -> Node ev
slot = el Slot

public export %inline
source : List (Attribute ev) -> List (Node ev) -> Node ev
source = el Source

public export %inline
span : List (Attribute ev) -> List (Node ev) -> Node ev
span = el Span

public export %inline
style : List (Attribute ev) -> List (Node ev) -> Node ev
style = el Style

public export %inline
table : List (Attribute ev) -> List (Node ev) -> Node ev
table = el Table

public export %inline
tbody : List (Attribute ev) -> List (Node ev) -> Node ev
tbody = el Tbody

public export %inline
td : List (Attribute ev) -> List (Node ev) -> Node ev
td = el Td

public export %inline
template : List (Attribute ev) -> List (Node ev) -> Node ev
template = el Template

public export %inline
textarea : List (Attribute ev) -> List (Node ev) -> Node ev
textarea = el TextArea

public export %inline
tfoot : List (Attribute ev) -> List (Node ev) -> Node ev
tfoot = el Tfoot

public export %inline
th : List (Attribute ev) -> List (Node ev) -> Node ev
th = el Th

public export %inline
thead : List (Attribute ev) -> List (Node ev) -> Node ev
thead = el Thead

public export %inline
time : List (Attribute ev) -> List (Node ev) -> Node ev
time = el Time

public export %inline
title : List (Attribute ev) -> List (Node ev) -> Node ev
title = el Title

public export %inline
tr : List (Attribute ev) -> List (Node ev) -> Node ev
tr = el Tr

public export %inline
track : List (Attribute ev) -> List (Node ev) -> Node ev
track = el Track

public export %inline
ul : List (Attribute ev) -> List (Node ev) -> Node ev
ul = el Ul

public export %inline
video : List (Attribute ev) -> List (Node ev) -> Node ev
video = el Video

--------------------------------------------------------------------------------
--          Rendering Html
--------------------------------------------------------------------------------

export
escape : String -> String
escape = fastConcat . map esc . unpack
  where esc : Char -> String
        esc '<'          = "&lt;"
        esc '>'          = "&gt;"
        esc '&'          = "&amp;"
        esc '"'          = "&quot;"
        esc '\''         = "&#x27"
        esc '\n'         = "\n"
        esc '\r'         = "\r"
        esc '\t'         = "\t"
        esc c            = if c < ' ' then "" else singleton c

attrs : List (Attribute ev) -> String
attrs as = let s = displayAttributes as in if null s then "" else " " ++ s

export
render : Node ev -> String
render n = case n of
  Raw x         => x
  Text x        => escape x
  El tag as ns  => "<\{tag}\{attrs as}>\{go [<] ns}</\{tag}>"
  Empty         => ""

  where
    go : SnocList String -> List (Node ev) -> String
    go ss (n :: ns) = go (ss :< render n) ns
    go ss []        = concat $ ss <>> []

export
renderMany : List (Node ev) -> String
renderMany = fastConcat . map render
