module Text.Html.Node

import Data.String
import Text.Html.Attribute
import Text.Html.Event
import Web.Dom

%hide Web.Internal.DomTypes.Node

%default total

public export
data Node : Type where
  El   :  {tag : String}
       -> (tpe : ElementType tag el)
       -> (extract : Bool)
       -> List EventType
       -> List Attribute
       -> List Node
       -> Node

  Raw  : String -> Node

  Text : String -> Node

public export %inline
FromString Node where
  fromString = Text

public export %inline
a : List EventType -> List Attribute -> List Node -> Node
a = El A True

public export %inline
area : List EventType -> List Attribute -> List Node -> Node
area = El Area True

public export %inline
audio : List EventType -> List Attribute -> List Node -> Node
audio = El Audio True

public export %inline
base : List EventType -> List Attribute -> List Node -> Node
base = El Base True

public export %inline
blockquote : List EventType -> List Attribute -> List Node -> Node
blockquote = El Blockquote True

public export %inline
body : List EventType -> List Attribute -> List Node -> Node
body = El Body True

public export %inline
br : List EventType -> List Attribute -> List Node -> Node
br = El Br True

public export %inline
button : List EventType -> List Attribute -> List Node -> Node
button = El Button True

public export %inline
canvas : List EventType -> List Attribute -> List Node -> Node
canvas = El Canvas True

public export %inline
caption : List EventType -> List Attribute -> List Node -> Node
caption = El Caption True

public export %inline
col : List EventType -> List Attribute -> List Node -> Node
col = El Col True

public export %inline
colgroup : List EventType -> List Attribute -> List Node -> Node
colgroup = El Colgroup True

public export %inline
data_ : List EventType -> List Attribute -> List Node -> Node
data_ = El Data True

public export %inline
datalist : List EventType -> List Attribute -> List Node -> Node
datalist = El Datalist True

public export %inline
del : List EventType -> List Attribute -> List Node -> Node
del = El Del True

public export %inline
details : List EventType -> List Attribute -> List Node -> Node
details = El Details True

public export %inline
dialog : List EventType -> List Attribute -> List Node -> Node
dialog = El Dialog True

public export %inline
div : List EventType -> List Attribute -> List Node -> Node
div = El Div True

public export %inline
dl : List EventType -> List Attribute -> List Node -> Node
dl = El Dl True

public export %inline
embed : List EventType -> List Attribute -> List Node -> Node
embed = El Embed True

public export %inline
fieldset : List EventType -> List Attribute -> List Node -> Node
fieldset = El FieldSet True

public export %inline
form : List EventType -> List Attribute -> List Node -> Node
form = El Form True

public export %inline
h1 : List EventType -> List Attribute -> List Node -> Node
h1 = El H1 True

public export %inline
h2 : List EventType -> List Attribute -> List Node -> Node
h2 = El H2 True

public export %inline
h3 : List EventType -> List Attribute -> List Node -> Node
h3 = El H3 True

public export %inline
h4 : List EventType -> List Attribute -> List Node -> Node
h4 = El H4 True

public export %inline
h5 : List EventType -> List Attribute -> List Node -> Node
h5 = El H5 True

public export %inline
h6 : List EventType -> List Attribute -> List Node -> Node
h6 = El H6 True

public export %inline
hr : List EventType -> List Attribute -> List Node -> Node
hr = El HR True

public export %inline
html : List EventType -> List Attribute -> List Node -> Node
html = El Html True

public export %inline
iframe : List EventType -> List Attribute -> List Node -> Node
iframe = El IFrame True

public export %inline
ime : List EventType -> List Attribute -> List Node -> Node
ime = El Ime True

public export %inline
input : List EventType -> List Attribute -> List Node -> Node
input = El Input True

public export %inline
ins : List EventType -> List Attribute -> List Node -> Node
ins = El Ins True

public export %inline
label : List EventType -> List Attribute -> List Node -> Node
label = El Label True

public export %inline
legend : List EventType -> List Attribute -> List Node -> Node
legend = El Legend True

public export %inline
li : List EventType -> List Attribute -> List Node -> Node
li = El Li True

public export %inline
link : List EventType -> List Attribute -> List Node -> Node
link = El Link True

public export %inline
map : List EventType -> List Attribute -> List Node -> Node
map = El Map True

public export %inline
menu : List EventType -> List Attribute -> List Node -> Node
menu = El Menu True

public export %inline
meta : List EventType -> List Attribute -> List Node -> Node
meta = El Meta True

public export %inline
meter : List EventType -> List Attribute -> List Node -> Node
meter = El Meter True

public export %inline
object : List EventType -> List Attribute -> List Node -> Node
object = El Object True

public export %inline
ol : List EventType -> List Attribute -> List Node -> Node
ol = El Ol True

public export %inline
optgroup : List EventType -> List Attribute -> List Node -> Node
optgroup = El OptGroup True

public export %inline
option : List EventType -> List Attribute -> List Node -> Node
option = El Option True

public export %inline
output : List EventType -> List Attribute -> List Node -> Node
output = El Output True

public export %inline
p : List EventType -> List Attribute -> List Node -> Node
p = El P True

public export %inline
param : List EventType -> List Attribute -> List Node -> Node
param = El Param True

public export %inline
picture : List EventType -> List Attribute -> List Node -> Node
picture = El Picture True

public export %inline
pre : List EventType -> List Attribute -> List Node -> Node
pre = El Pre True

public export %inline
progress : List EventType -> List Attribute -> List Node -> Node
progress = El Progress True

public export %inline
q : List EventType -> List Attribute -> List Node -> Node
q = El Q True

public export %inline
script : List EventType -> List Attribute -> List Node -> Node
script = El Script True

public export %inline
select : List EventType -> List Attribute -> List Node -> Node
select = El Select True

public export %inline
slot : List EventType -> List Attribute -> List Node -> Node
slot = El Slot True

public export %inline
source : List EventType -> List Attribute -> List Node -> Node
source = El Source True

public export %inline
span : List EventType -> List Attribute -> List Node -> Node
span = El Span True

public export %inline
style : List EventType -> List Attribute -> List Node -> Node
style = El Style True

public export %inline
table : List EventType -> List Attribute -> List Node -> Node
table = El Table True

public export %inline
tbody : List EventType -> List Attribute -> List Node -> Node
tbody = El Tbody True

public export %inline
td : List EventType -> List Attribute -> List Node -> Node
td = El Td True

public export %inline
template : List EventType -> List Attribute -> List Node -> Node
template = El Template True

public export %inline
textarea : List EventType -> List Attribute -> List Node -> Node
textarea = El TextArea True

public export %inline
tfoot : List EventType -> List Attribute -> List Node -> Node
tfoot = El Tfoot True

public export %inline
th : List EventType -> List Attribute -> List Node -> Node
th = El Th True

public export %inline
thead : List EventType -> List Attribute -> List Node -> Node
thead = El Thead True

public export %inline
time : List EventType -> List Attribute -> List Node -> Node
time = El Time True

public export %inline
title : List EventType -> List Attribute -> List Node -> Node
title = El Title True

public export %inline
tr : List EventType -> List Attribute -> List Node -> Node
tr = El Tr True

public export %inline
track : List EventType -> List Attribute -> List Node -> Node
track = El Track True

public export %inline
ul : List EventType -> List Attribute -> List Node -> Node
ul = El Ul True

public export %inline
video : List EventType -> List Attribute -> List Node -> Node
video = El Video True

public export %inline
a_ : List Attribute -> List Node -> Node
a_ = El A False []

public export %inline
area_ : List Attribute -> List Node -> Node
area_ = El Area False []

public export %inline
audio_ : List Attribute -> List Node -> Node
audio_ = El Audio False []

public export %inline
base_ : List Attribute -> List Node -> Node
base_ = El Base False []

public export %inline
blockquote_ : List Attribute -> List Node -> Node
blockquote_ = El Blockquote False []

public export %inline
body_ : List Attribute -> List Node -> Node
body_ = El Body False []

public export %inline
br_ : List Attribute -> List Node -> Node
br_ = El Br False []

public export %inline
button_ : List Attribute -> List Node -> Node
button_ = El Button False []

public export %inline
canvas_ : List Attribute -> List Node -> Node
canvas_ = El Canvas False []

public export %inline
caption_ : List Attribute -> List Node -> Node
caption_ = El Caption False []

public export %inline
col_ : List Attribute -> List Node -> Node
col_ = El Col False []

public export %inline
colgroup_ : List Attribute -> List Node -> Node
colgroup_ = El Colgroup False []

public export %inline
data__ : List Attribute -> List Node -> Node
data__ = El Data False []

public export %inline
datalist_ : List Attribute -> List Node -> Node
datalist_ = El Datalist False []

public export %inline
del_ : List Attribute -> List Node -> Node
del_ = El Del False []

public export %inline
details_ : List Attribute -> List Node -> Node
details_ = El Details False []

public export %inline
dialog_ : List Attribute -> List Node -> Node
dialog_ = El Dialog False []

public export %inline
div_ : List Attribute -> List Node -> Node
div_ = El Div False []

public export %inline
dl_ : List Attribute -> List Node -> Node
dl_ = El Dl False []

public export %inline
embed_ : List Attribute -> List Node -> Node
embed_ = El Embed False []

public export %inline
fieldset_ : List Attribute -> List Node -> Node
fieldset_ = El FieldSet False []

public export %inline
form_ : List Attribute -> List Node -> Node
form_ = El Form False []

public export %inline
h1_ : List Attribute -> List Node -> Node
h1_ = El H1 False []

public export %inline
h2_ : List Attribute -> List Node -> Node
h2_ = El H2 False []

public export %inline
h3_ : List Attribute -> List Node -> Node
h3_ = El H3 False []

public export %inline
h4_ : List Attribute -> List Node -> Node
h4_ = El H4 False []

public export %inline
h5_ : List Attribute -> List Node -> Node
h5_ = El H5 False []

public export %inline
h6_ : List Attribute -> List Node -> Node
h6_ = El H6 False []

public export %inline
hr_ : List Attribute -> List Node -> Node
hr_ = El HR False []

public export %inline
html_ : List Attribute -> List Node -> Node
html_ = El Html False []

public export %inline
iframe_ : List Attribute -> List Node -> Node
iframe_ = El IFrame False []

public export %inline
ime_ : List Attribute -> List Node -> Node
ime_ = El Ime False []

public export %inline
input_ : List Attribute -> List Node -> Node
input_ = El Input False []

public export %inline
ins_ : List Attribute -> List Node -> Node
ins_ = El Ins False []

public export %inline
label_ : List Attribute -> List Node -> Node
label_ = El Label False []

public export %inline
legend_ : List Attribute -> List Node -> Node
legend_ = El Legend False []

public export %inline
li_ : List Attribute -> List Node -> Node
li_ = El Li False []

public export %inline
link_ : List Attribute -> List Node -> Node
link_ = El Link False []

public export %inline
map_ : List Attribute -> List Node -> Node
map_ = El Map False []

public export %inline
menu_ : List Attribute -> List Node -> Node
menu_ = El Menu False []

public export %inline
meta_ : List Attribute -> List Node -> Node
meta_ = El Meta False []

public export %inline
meter_ : List Attribute -> List Node -> Node
meter_ = El Meter False []

public export %inline
object_ : List Attribute -> List Node -> Node
object_ = El Object False []

public export %inline
ol_ : List Attribute -> List Node -> Node
ol_ = El Ol False []

public export %inline
optgroup_ : List Attribute -> List Node -> Node
optgroup_ = El OptGroup False []

public export %inline
option_ : List Attribute -> List Node -> Node
option_ = El Option False []

public export %inline
output_ : List Attribute -> List Node -> Node
output_ = El Output False []

public export %inline
p_ : List Attribute -> List Node -> Node
p_ = El P False []

public export %inline
param_ : List Attribute -> List Node -> Node
param_ = El Param False []

public export %inline
picture_ : List Attribute -> List Node -> Node
picture_ = El Picture False []

public export %inline
pre_ : List Attribute -> List Node -> Node
pre_ = El Pre False []

public export %inline
progress_ : List Attribute -> List Node -> Node
progress_ = El Progress False []

public export %inline
q_ : List Attribute -> List Node -> Node
q_ = El Q False []

public export %inline
script_ : List Attribute -> List Node -> Node
script_ = El Script False []

public export %inline
select_ : List Attribute -> List Node -> Node
select_ = El Select False []

public export %inline
slot_ : List Attribute -> List Node -> Node
slot_ = El Slot False []

public export %inline
source_ : List Attribute -> List Node -> Node
source_ = El Source False []

public export %inline
span_ : List Attribute -> List Node -> Node
span_ = El Span False []

public export %inline
style_ : List Attribute -> List Node -> Node
style_ = El Style False []

public export %inline
table_ : List Attribute -> List Node -> Node
table_ = El Table False []

public export %inline
tbody_ : List Attribute -> List Node -> Node
tbody_ = El Tbody False []

public export %inline
td_ : List Attribute -> List Node -> Node
td_ = El Td False []

public export %inline
template_ : List Attribute -> List Node -> Node
template_ = El Template False []

public export %inline
textarea_ : List Attribute -> List Node -> Node
textarea_ = El TextArea False []

public export %inline
tfoot_ : List Attribute -> List Node -> Node
tfoot_ = El Tfoot False []

public export %inline
th_ : List Attribute -> List Node -> Node
th_ = El Th False []

public export %inline
thead_ : List Attribute -> List Node -> Node
thead_ = El Thead False []

public export %inline
time_ : List Attribute -> List Node -> Node
time_ = El Time False []

public export %inline
title_ : List Attribute -> List Node -> Node
title_ = El Title False []

public export %inline
tr_ : List Attribute -> List Node -> Node
tr_ = El Tr False []

public export %inline
track_ : List Attribute -> List Node -> Node
track_ = El Track False []

public export %inline
ul_ : List Attribute -> List Node -> Node
ul_ = El Ul False []

public export %inline
video_ : List Attribute -> List Node -> Node
video_ = El Video False []

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

attrs : List Attribute -> String
attrs as = let s = displayAttributes as in if null s then "" else " " ++ s

export
render : Node -> String
render (Raw x)                = x
render (Text x)               = escape x
render (El {tag} _ _ _ as ns) =
  #"<\#{tag}\#{attrs as}>\#{go Nil ns}</\#{tag}>"#

  where go : List String -> List Node -> String
        go ss (n :: ns) = go (render n :: ss) ns
        go ss []        = fastConcat $ reverse ss
