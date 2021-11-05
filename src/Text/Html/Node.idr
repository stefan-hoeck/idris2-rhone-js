module Text.Html.Node

import Data.String
import Text.Html.Attribute
import Text.Html.Event
import Web.Dom

%hide Web.Internal.DomTypes.Node

%default total

public export
data Node : (event : Type) -> Type where
  El   :  {0 ev, el : Type}
       -> {tag : String}
       -> (tpe : ElementType tag el)
       -> List (Attribute ev)
       -> List (Node ev)
       -> Node ev

  Raw  : String -> Node ev

  Text : String -> Node ev

public export %inline
FromString (Node ev) where
  fromString = Text

public export %inline
a : List (Attribute ev) -> List (Node ev) -> Node ev
a = El A

public export %inline
address : List (Attribute ev) -> List (Node ev) -> Node ev
address = El Address

public export %inline
area : List (Attribute ev) -> List (Node ev) -> Node ev
area = El Area

public export %inline
article : List (Attribute ev) -> List (Node ev) -> Node ev
article = El Article

public export %inline
audio : List (Attribute ev) -> List (Node ev) -> Node ev
audio = El Audio

public export %inline
base : List (Attribute ev) -> List (Node ev) -> Node ev
base = El Base

public export %inline
blockquote : List (Attribute ev) -> List (Node ev) -> Node ev
blockquote = El Blockquote

public export %inline
body : List (Attribute ev) -> List (Node ev) -> Node ev
body = El Body

public export %inline
br : List (Attribute ev) -> List (Node ev) -> Node ev
br = El Br

public export %inline
button : List (Attribute ev) -> List (Node ev) -> Node ev
button = El Button

public export %inline
canvas : List (Attribute ev) -> List (Node ev) -> Node ev
canvas = El Canvas

public export %inline
caption : List (Attribute ev) -> List (Node ev) -> Node ev
caption = El Caption

public export %inline
col : List (Attribute ev) -> List (Node ev) -> Node ev
col = El Col

public export %inline
colgroup : List (Attribute ev) -> List (Node ev) -> Node ev
colgroup = El Colgroup

public export %inline
data_ : List (Attribute ev) -> List (Node ev) -> Node ev
data_ = El Data

public export %inline
datalist : List (Attribute ev) -> List (Node ev) -> Node ev
datalist = El Datalist

public export %inline
del : List (Attribute ev) -> List (Node ev) -> Node ev
del = El Del

public export %inline
details : List (Attribute ev) -> List (Node ev) -> Node ev
details = El Details

public export %inline
dialog : List (Attribute ev) -> List (Node ev) -> Node ev
dialog = El Dialog

public export %inline
div : List (Attribute ev) -> List (Node ev) -> Node ev
div = El Div

public export %inline
dl : List (Attribute ev) -> List (Node ev) -> Node ev
dl = El Dl

public export %inline
embed : List (Attribute ev) -> List (Node ev) -> Node ev
embed = El Embed

public export %inline
fieldset : List (Attribute ev) -> List (Node ev) -> Node ev
fieldset = El FieldSet

public export %inline
footer : List (Attribute ev) -> List (Node ev) -> Node ev
footer = El Footer

public export %inline
form : List (Attribute ev) -> List (Node ev) -> Node ev
form = El Form

public export %inline
h1 : List (Attribute ev) -> List (Node ev) -> Node ev
h1 = El H1

public export %inline
h2 : List (Attribute ev) -> List (Node ev) -> Node ev
h2 = El H2

public export %inline
h3 : List (Attribute ev) -> List (Node ev) -> Node ev
h3 = El H3

public export %inline
h4 : List (Attribute ev) -> List (Node ev) -> Node ev
h4 = El H4

public export %inline
h5 : List (Attribute ev) -> List (Node ev) -> Node ev
h5 = El H5

public export %inline
h6 : List (Attribute ev) -> List (Node ev) -> Node ev
h6 = El H6

public export %inline
header : List (Attribute ev) -> List (Node ev) -> Node ev
header = El Header

public export %inline
hr : List (Attribute ev) -> List (Node ev) -> Node ev
hr = El HR

public export %inline
html : List (Attribute ev) -> List (Node ev) -> Node ev
html = El Html

public export %inline
iframe : List (Attribute ev) -> List (Node ev) -> Node ev
iframe = El IFrame

public export %inline
img : List (Attribute ev) -> List (Node ev) -> Node ev
img = El Img

public export %inline
input : List (Attribute ev) -> List (Node ev) -> Node ev
input = El Input

public export %inline
ins : List (Attribute ev) -> List (Node ev) -> Node ev
ins = El Ins

public export %inline
label : List (Attribute ev) -> List (Node ev) -> Node ev
label = El Label

public export %inline
legend : List (Attribute ev) -> List (Node ev) -> Node ev
legend = El Legend

public export %inline
li : List (Attribute ev) -> List (Node ev) -> Node ev
li = El Li

public export %inline
link : List (Attribute ev) -> List (Node ev) -> Node ev
link = El Link

public export %inline
map : List (Attribute ev) -> List (Node ev) -> Node ev
map = El Map

public export %inline
menu : List (Attribute ev) -> List (Node ev) -> Node ev
menu = El Menu

public export %inline
meta : List (Attribute ev) -> List (Node ev) -> Node ev
meta = El Meta

public export %inline
meter : List (Attribute ev) -> List (Node ev) -> Node ev
meter = El Meter

public export %inline
object : List (Attribute ev) -> List (Node ev) -> Node ev
object = El Object

public export %inline
ol : List (Attribute ev) -> List (Node ev) -> Node ev
ol = El Ol

public export %inline
optgroup : List (Attribute ev) -> List (Node ev) -> Node ev
optgroup = El OptGroup

public export %inline
option : List (Attribute ev) -> List (Node ev) -> Node ev
option = El Option

public export %inline
output : List (Attribute ev) -> List (Node ev) -> Node ev
output = El Output

public export %inline
p : List (Attribute ev) -> List (Node ev) -> Node ev
p = El P

public export %inline
param : List (Attribute ev) -> List (Node ev) -> Node ev
param = El Param

public export %inline
picture : List (Attribute ev) -> List (Node ev) -> Node ev
picture = El Picture

public export %inline
pre : List (Attribute ev) -> List (Node ev) -> Node ev
pre = El Pre

public export %inline
progress : List (Attribute ev) -> List (Node ev) -> Node ev
progress = El Progress

public export %inline
q : List (Attribute ev) -> List (Node ev) -> Node ev
q = El Q

public export %inline
script : List (Attribute ev) -> List (Node ev) -> Node ev
script = El Script

public export %inline
section : List (Attribute ev) -> List (Node ev) -> Node ev
section = El Section

public export %inline
select : List (Attribute ev) -> List (Node ev) -> Node ev
select = El Select

public export %inline
slot : List (Attribute ev) -> List (Node ev) -> Node ev
slot = El Slot

public export %inline
source : List (Attribute ev) -> List (Node ev) -> Node ev
source = El Source

public export %inline
span : List (Attribute ev) -> List (Node ev) -> Node ev
span = El Span

public export %inline
style : List (Attribute ev) -> List (Node ev) -> Node ev
style = El Style

public export %inline
table : List (Attribute ev) -> List (Node ev) -> Node ev
table = El Table

public export %inline
tbody : List (Attribute ev) -> List (Node ev) -> Node ev
tbody = El Tbody

public export %inline
td : List (Attribute ev) -> List (Node ev) -> Node ev
td = El Td

public export %inline
template : List (Attribute ev) -> List (Node ev) -> Node ev
template = El Template

public export %inline
textarea : List (Attribute ev) -> List (Node ev) -> Node ev
textarea = El TextArea

public export %inline
tfoot : List (Attribute ev) -> List (Node ev) -> Node ev
tfoot = El Tfoot

public export %inline
th : List (Attribute ev) -> List (Node ev) -> Node ev
th = El Th

public export %inline
thead : List (Attribute ev) -> List (Node ev) -> Node ev
thead = El Thead

public export %inline
time : List (Attribute ev) -> List (Node ev) -> Node ev
time = El Time

public export %inline
title : List (Attribute ev) -> List (Node ev) -> Node ev
title = El Title

public export %inline
tr : List (Attribute ev) -> List (Node ev) -> Node ev
tr = El Tr

public export %inline
track : List (Attribute ev) -> List (Node ev) -> Node ev
track = El Track

public export %inline
ul : List (Attribute ev) -> List (Node ev) -> Node ev
ul = El Ul

public export %inline
video : List (Attribute ev) -> List (Node ev) -> Node ev
video = El Video

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
  Raw x             => x
  Text x            => escape x
  El {tag} _ as ns  => #"<\#{tag}\#{attrs as}>\#{go Nil ns}</\#{tag}>"#

  where go : List String -> List (Node ev) -> String
        go ss (n :: ns) = go (render n :: ss) ns
        go ss []        = fastConcat $ reverse ss
