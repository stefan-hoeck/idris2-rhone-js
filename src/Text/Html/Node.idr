module Text.Html.Node

import Control.WellFounded
import Data.String
import Text.Html.Attribute
import Text.Html.Event
import Web.Dom

%hide Web.Internal.DomTypes.Node

%default total

mutual
  public export
  data NodeList :  (extracted : List Type)
                -> (event     : Type)
                -> Type where
    Nil : NodeList [] event
    (::) : Node ts ev -> NodeList tss ev -> NodeList (ts ++ tss) ev

  public export
  data Node :  (extracted : List Type)
            -> (event     : Type)
            -> Type where
    El   :  {0 ev, el : Type}
         -> {tag : String}
         -> (tpe : ElementType tag el)
         -> List (Attribute ev)
         -> NodeList ts ev
         -> Node ts ev

    ElE  :  {0 ev, el : Type}
         -> {tag : String}
         -> (tpe : ElementType tag el)
         -> List (Attribute ev)
         -> NodeList ts ev
         -> Node (el :: ts) ev
 
    Raw  : String -> Node [] ev

    Text : String -> Node [] ev

public export
Sized (NodeList ts ev) where
  size []       = 0
  size (h :: t) = S $ size t

export
fromList : List (Node [] ev) -> NodeList [] ev
fromList []        = []
fromList (x :: xs) = x :: fromList xs

public export %inline
FromString (Node [] ev) where
  fromString = Text

public export %inline
a : List (Attribute ev) -> NodeList ts ev -> Node ts ev
a = El A

public export %inline
area : List (Attribute ev) -> NodeList ts ev -> Node ts ev
area = El Area

public export %inline
audio : List (Attribute ev) -> NodeList ts ev -> Node ts ev
audio = El Audio

public export %inline
base : List (Attribute ev) -> NodeList ts ev -> Node ts ev
base = El Base

public export %inline
blockquote : List (Attribute ev) -> NodeList ts ev -> Node ts ev
blockquote = El Blockquote

public export %inline
body : List (Attribute ev) -> NodeList ts ev -> Node ts ev
body = El Body

public export %inline
br : List (Attribute ev) -> NodeList ts ev -> Node ts ev
br = El Br

public export %inline
button : List (Attribute ev) -> NodeList ts ev -> Node ts ev
button = El Button

public export %inline
canvas : List (Attribute ev) -> NodeList ts ev -> Node ts ev
canvas = El Canvas

public export %inline
caption : List (Attribute ev) -> NodeList ts ev -> Node ts ev
caption = El Caption

public export %inline
col : List (Attribute ev) -> NodeList ts ev -> Node ts ev
col = El Col

public export %inline
colgroup : List (Attribute ev) -> NodeList ts ev -> Node ts ev
colgroup = El Colgroup

public export %inline
data_ : List (Attribute ev) -> NodeList ts ev -> Node ts ev
data_ = El Data

public export %inline
datalist : List (Attribute ev) -> NodeList ts ev -> Node ts ev
datalist = El Datalist

public export %inline
del : List (Attribute ev) -> NodeList ts ev -> Node ts ev
del = El Del

public export %inline
details : List (Attribute ev) -> NodeList ts ev -> Node ts ev
details = El Details

public export %inline
dialog : List (Attribute ev) -> NodeList ts ev -> Node ts ev
dialog = El Dialog

public export %inline
div : List (Attribute ev) -> NodeList ts ev -> Node ts ev
div = El Div

public export %inline
dl : List (Attribute ev) -> NodeList ts ev -> Node ts ev
dl = El Dl

public export %inline
embed : List (Attribute ev) -> NodeList ts ev -> Node ts ev
embed = El Embed

public export %inline
fieldset : List (Attribute ev) -> NodeList ts ev -> Node ts ev
fieldset = El FieldSet

public export %inline
form : List (Attribute ev) -> NodeList ts ev -> Node ts ev
form = El Form

public export %inline
h1 : List (Attribute ev) -> NodeList ts ev -> Node ts ev
h1 = El H1

public export %inline
h2 : List (Attribute ev) -> NodeList ts ev -> Node ts ev
h2 = El H2

public export %inline
h3 : List (Attribute ev) -> NodeList ts ev -> Node ts ev
h3 = El H3

public export %inline
h4 : List (Attribute ev) -> NodeList ts ev -> Node ts ev
h4 = El H4

public export %inline
h5 : List (Attribute ev) -> NodeList ts ev -> Node ts ev
h5 = El H5

public export %inline
h6 : List (Attribute ev) -> NodeList ts ev -> Node ts ev
h6 = El H6

public export %inline
hr : List (Attribute ev) -> NodeList ts ev -> Node ts ev
hr = El HR

public export %inline
html : List (Attribute ev) -> NodeList ts ev -> Node ts ev
html = El Html

public export %inline
iframe : List (Attribute ev) -> NodeList ts ev -> Node ts ev
iframe = El IFrame

public export %inline
ime : List (Attribute ev) -> NodeList ts ev -> Node ts ev
ime = El Ime

public export %inline
input : List (Attribute ev) -> NodeList ts ev -> Node ts ev
input = El Input

public export %inline
ins : List (Attribute ev) -> NodeList ts ev -> Node ts ev
ins = El Ins

public export %inline
label : List (Attribute ev) -> NodeList ts ev -> Node ts ev
label = El Label

public export %inline
legend : List (Attribute ev) -> NodeList ts ev -> Node ts ev
legend = El Legend

public export %inline
li : List (Attribute ev) -> NodeList ts ev -> Node ts ev
li = El Li

public export %inline
link : List (Attribute ev) -> NodeList ts ev -> Node ts ev
link = El Link

public export %inline
map : List (Attribute ev) -> NodeList ts ev -> Node ts ev
map = El Map

public export %inline
menu : List (Attribute ev) -> NodeList ts ev -> Node ts ev
menu = El Menu

public export %inline
meta : List (Attribute ev) -> NodeList ts ev -> Node ts ev
meta = El Meta

public export %inline
meter : List (Attribute ev) -> NodeList ts ev -> Node ts ev
meter = El Meter

public export %inline
object : List (Attribute ev) -> NodeList ts ev -> Node ts ev
object = El Object

public export %inline
ol : List (Attribute ev) -> NodeList ts ev -> Node ts ev
ol = El Ol

public export %inline
optgroup : List (Attribute ev) -> NodeList ts ev -> Node ts ev
optgroup = El OptGroup

public export %inline
option : List (Attribute ev) -> NodeList ts ev -> Node ts ev
option = El Option

public export %inline
output : List (Attribute ev) -> NodeList ts ev -> Node ts ev
output = El Output

public export %inline
p : List (Attribute ev) -> NodeList ts ev -> Node ts ev
p = El P

public export %inline
param : List (Attribute ev) -> NodeList ts ev -> Node ts ev
param = El Param

public export %inline
picture : List (Attribute ev) -> NodeList ts ev -> Node ts ev
picture = El Picture

public export %inline
pre : List (Attribute ev) -> NodeList ts ev -> Node ts ev
pre = El Pre

public export %inline
progress : List (Attribute ev) -> NodeList ts ev -> Node ts ev
progress = El Progress

public export %inline
q : List (Attribute ev) -> NodeList ts ev -> Node ts ev
q = El Q

public export %inline
script : List (Attribute ev) -> NodeList ts ev -> Node ts ev
script = El Script

public export %inline
select : List (Attribute ev) -> NodeList ts ev -> Node ts ev
select = El Select

public export %inline
slot : List (Attribute ev) -> NodeList ts ev -> Node ts ev
slot = El Slot

public export %inline
source : List (Attribute ev) -> NodeList ts ev -> Node ts ev
source = El Source

public export %inline
span : List (Attribute ev) -> NodeList ts ev -> Node ts ev
span = El Span

public export %inline
style : List (Attribute ev) -> NodeList ts ev -> Node ts ev
style = El Style

public export %inline
table : List (Attribute ev) -> NodeList ts ev -> Node ts ev
table = El Table

public export %inline
tbody : List (Attribute ev) -> NodeList ts ev -> Node ts ev
tbody = El Tbody

public export %inline
td : List (Attribute ev) -> NodeList ts ev -> Node ts ev
td = El Td

public export %inline
template : List (Attribute ev) -> NodeList ts ev -> Node ts ev
template = El Template

public export %inline
textarea : List (Attribute ev) -> NodeList ts ev -> Node ts ev
textarea = El TextArea

public export %inline
tfoot : List (Attribute ev) -> NodeList ts ev -> Node ts ev
tfoot = El Tfoot

public export %inline
th : List (Attribute ev) -> NodeList ts ev -> Node ts ev
th = El Th

public export %inline
thead : List (Attribute ev) -> NodeList ts ev -> Node ts ev
thead = El Thead

public export %inline
time : List (Attribute ev) -> NodeList ts ev -> Node ts ev
time = El Time

public export %inline
title : List (Attribute ev) -> NodeList ts ev -> Node ts ev
title = El Title

public export %inline
tr : List (Attribute ev) -> NodeList ts ev -> Node ts ev
tr = El Tr

public export %inline
track : List (Attribute ev) -> NodeList ts ev -> Node ts ev
track = El Track

public export %inline
ul : List (Attribute ev) -> NodeList ts ev -> Node ts ev
ul = El Ul

public export %inline
video : List (Attribute ev) -> NodeList ts ev -> Node ts ev
video = El Video

public export %inline
a_ : List (Attribute ev) -> NodeList ts ev -> Node (Anchor :: ts) ev
a_ = ElE A

public export %inline
area_ : List (Attribute ev) -> NodeList ts ev -> Node (Area :: ts) ev
area_ = ElE Area

public export %inline
audio_ : List (Attribute ev) -> NodeList ts ev -> Node (Audio :: ts) ev
audio_ = ElE Audio

public export %inline
base_ : List (Attribute ev) -> NodeList ts ev -> Node (Base :: ts) ev
base_ = ElE Base

public export %inline
blockquote_ : List (Attribute ev) -> NodeList ts ev -> Node (Quote :: ts) ev
blockquote_ = ElE Blockquote

public export %inline
body_ : List (Attribute ev) -> NodeList ts ev -> Node (HTMLBodyElement :: ts) ev
body_ = ElE Body

public export %inline
br_ : List (Attribute ev) -> NodeList ts ev -> Node (BR :: ts) ev
br_ = ElE Br

public export %inline
button_ : List (Attribute ev) -> NodeList ts ev -> Node (Button :: ts) ev
button_ = ElE Button

public export %inline
canvas_ : List (Attribute ev) -> NodeList ts ev -> Node (Canvas :: ts) ev
canvas_ = ElE Canvas

public export %inline
caption_ : List (Attribute ev) -> NodeList ts ev -> Node (TableCaption :: ts) ev
caption_ = ElE Caption

public export %inline
col_ : List (Attribute ev) -> NodeList ts ev -> Node (TableCol :: ts) ev
col_ = ElE Col

public export %inline
colgroup_ : List (Attribute ev) -> NodeList ts ev -> Node (TableCol :: ts) ev
colgroup_ = ElE Colgroup

public export %inline
data__ : List (Attribute ev) -> NodeList ts ev -> Node (Data :: ts) ev
data__ = ElE Data

public export %inline
datalist_ : List (Attribute ev) -> NodeList ts ev -> Node (DataList :: ts) ev
datalist_ = ElE Datalist

public export %inline
del_ : List (Attribute ev) -> NodeList ts ev -> Node (Mod :: ts) ev
del_ = ElE Del

public export %inline
details_ : List (Attribute ev) -> NodeList ts ev -> Node (Details :: ts) ev
details_ = ElE Details

public export %inline
dialog_ : List (Attribute ev) -> NodeList ts ev -> Node (Dialog :: ts) ev
dialog_ = ElE Dialog

public export %inline
div_ : List (Attribute ev) -> NodeList ts ev -> Node (Div :: ts) ev
div_ = ElE Div

public export %inline
dl_ : List (Attribute ev) -> NodeList ts ev -> Node (DList :: ts) ev
dl_ = ElE Dl

public export %inline
embed_ : List (Attribute ev) -> NodeList ts ev -> Node (Embed :: ts) ev
embed_ = ElE Embed

public export %inline
fieldset_ : List (Attribute ev) -> NodeList ts ev -> Node (FieldSet :: ts) ev
fieldset_ = ElE FieldSet

public export %inline
form_ : List (Attribute ev) -> NodeList ts ev -> Node (Form :: ts) ev
form_ = ElE Form

public export %inline
h1_ : List (Attribute ev) -> NodeList ts ev -> Node (Heading :: ts) ev
h1_ = ElE H1

public export %inline
h2_ : List (Attribute ev) -> NodeList ts ev -> Node (Heading :: ts) ev
h2_ = ElE H2

public export %inline
h3_ : List (Attribute ev) -> NodeList ts ev -> Node (Heading :: ts) ev
h3_ = ElE H3

public export %inline
h4_ : List (Attribute ev) -> NodeList ts ev -> Node (Heading :: ts) ev
h4_ = ElE H4

public export %inline
h5_ : List (Attribute ev) -> NodeList ts ev -> Node (Heading :: ts) ev
h5_ = ElE H5

public export %inline
h6_ : List (Attribute ev) -> NodeList ts ev -> Node (Heading :: ts) ev
h6_ = ElE H6

public export %inline
hr_ : List (Attribute ev) -> NodeList ts ev -> Node (HR :: ts) ev
hr_ = ElE HR

public export %inline
html_ : List (Attribute ev) -> NodeList ts ev -> Node (Html :: ts) ev
html_ = ElE Html

public export %inline
iframe_ : List (Attribute ev) -> NodeList ts ev -> Node (IFrame :: ts) ev
iframe_ = ElE IFrame

public export %inline
ime_ : List (Attribute ev) -> NodeList ts ev -> Node (Image :: ts) ev
ime_ = ElE Ime

public export %inline
input_ : List (Attribute ev) -> NodeList ts ev -> Node (Input :: ts) ev
input_ = ElE Input

public export %inline
ins_ : List (Attribute ev) -> NodeList ts ev -> Node (Mod :: ts) ev
ins_ = ElE Ins

public export %inline
label_ : List (Attribute ev) -> NodeList ts ev -> Node (Label :: ts) ev
label_ = ElE Label

public export %inline
legend_ : List (Attribute ev) -> NodeList ts ev -> Node (Legend :: ts) ev
legend_ = ElE Legend

public export %inline
li_ : List (Attribute ev) -> NodeList ts ev -> Node (LI :: ts) ev
li_ = ElE Li

public export %inline
link_ : List (Attribute ev) -> NodeList ts ev -> Node (Link :: ts) ev
link_ = ElE Link

public export %inline
map_ : List (Attribute ev) -> NodeList ts ev -> Node (Map :: ts) ev
map_ = ElE Map

public export %inline
menu_ : List (Attribute ev) -> NodeList ts ev -> Node (Menu :: ts) ev
menu_ = ElE Menu

public export %inline
meta_ : List (Attribute ev) -> NodeList ts ev -> Node (Meta :: ts) ev
meta_ = ElE Meta

public export %inline
meter_ : List (Attribute ev) -> NodeList ts ev -> Node (Meter :: ts) ev
meter_ = ElE Meter

public export %inline
object_ : List (Attribute ev) -> NodeList ts ev -> Node (HTMLObjectElement :: ts) ev
object_ = ElE Object

public export %inline
ol_ : List (Attribute ev) -> NodeList ts ev -> Node (OList :: ts) ev
ol_ = ElE Ol

public export %inline
optgroup_ : List (Attribute ev) -> NodeList ts ev -> Node (OptGroup :: ts) ev
optgroup_ = ElE OptGroup

public export %inline
option_ : List (Attribute ev) -> NodeList ts ev -> Node (Option :: ts) ev
option_ = ElE Option

public export %inline
output_ : List (Attribute ev) -> NodeList ts ev -> Node (Output :: ts) ev
output_ = ElE Output

public export %inline
p_ : List (Attribute ev) -> NodeList ts ev -> Node (Paragraph :: ts) ev
p_ = ElE P

public export %inline
param_ : List (Attribute ev) -> NodeList ts ev -> Node (Param :: ts) ev
param_ = ElE Param

public export %inline
picture_ : List (Attribute ev) -> NodeList ts ev -> Node (Picture :: ts) ev
picture_ = ElE Picture

public export %inline
pre_ : List (Attribute ev) -> NodeList ts ev -> Node (Pre :: ts) ev
pre_ = ElE Pre

public export %inline
progress_ : List (Attribute ev) -> NodeList ts ev -> Node (Progress :: ts) ev
progress_ = ElE Progress

public export %inline
q_ : List (Attribute ev) -> NodeList ts ev -> Node (Quote :: ts) ev
q_ = ElE Q

public export %inline
script_ : List (Attribute ev) -> NodeList ts ev -> Node (Script :: ts) ev
script_ = ElE Script

public export %inline
select_ : List (Attribute ev) -> NodeList ts ev -> Node (Select :: ts) ev
select_ = ElE Select

public export %inline
slot_ : List (Attribute ev) -> NodeList ts ev -> Node (Slot :: ts) ev
slot_ = ElE Slot

public export %inline
source_ : List (Attribute ev) -> NodeList ts ev -> Node (Source :: ts) ev
source_ = ElE Source

public export %inline
span_ : List (Attribute ev) -> NodeList ts ev -> Node (Span :: ts) ev
span_ = ElE Span

public export %inline
style_ : List (Attribute ev) -> NodeList ts ev -> Node (Style :: ts) ev
style_ = ElE Style

public export %inline
table_ : List (Attribute ev) -> NodeList ts ev -> Node (Table :: ts) ev
table_ = ElE Table

public export %inline
tbody_ : List (Attribute ev) -> NodeList ts ev -> Node (TableSection :: ts) ev
tbody_ = ElE Tbody

public export %inline
td_ : List (Attribute ev) -> NodeList ts ev -> Node (TableCell :: ts) ev
td_ = ElE Td

public export %inline
template_ : List (Attribute ev) -> NodeList ts ev -> Node (Template :: ts) ev
template_ = ElE Template

public export %inline
textarea_ : List (Attribute ev) -> NodeList ts ev -> Node (TextArea :: ts) ev
textarea_ = ElE TextArea

public export %inline
tfoot_ : List (Attribute ev) -> NodeList ts ev -> Node (TableSection :: ts) ev
tfoot_ = ElE Tfoot

public export %inline
th_ : List (Attribute ev) -> NodeList ts ev -> Node (TableCell :: ts) ev
th_ = ElE Th

public export %inline
thead_ : List (Attribute ev) -> NodeList ts ev -> Node (TableSection :: ts) ev
thead_ = ElE Thead

public export %inline
time_ : List (Attribute ev) -> NodeList ts ev -> Node (Time :: ts) ev
time_ = ElE Time

public export %inline
title_ : List (Attribute ev) -> NodeList ts ev -> Node (Title :: ts) ev
title_ = ElE Title

public export %inline
tr_ : List (Attribute ev) -> NodeList ts ev -> Node (TableRow :: ts) ev
tr_ = ElE Tr

public export %inline
track_ : List (Attribute ev) -> NodeList ts ev -> Node (Track :: ts) ev
track_ = ElE Track

public export %inline
ul_ : List (Attribute ev) -> NodeList ts ev -> Node (UList :: ts) ev
ul_ = ElE Ul

public export %inline
video_ : List (Attribute ev) -> NodeList ts ev -> Node (Video :: ts) ev
video_ = ElE Video

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

renderImpl : Node ts ev -> String
renderImpl n = case n of
  Raw x             => x
  Text x            => escape x
  El {tag} _ as ns  =>
    #"<\#{tag}\#{attrs as}>\#{go Nil ns}</\#{tag}>"#
  ElE {tag} _ as ns =>
    #"<\#{tag}\#{attrs as}>\#{go Nil ns}</\#{tag}>"#

  where go : List String -> NodeList xs ev -> String
        go ss (n :: ns) = go (renderImpl n :: ss) ns
        go ss []        = fastConcat $ reverse ss

export %inline
render : Node [] ev -> String
render = renderImpl
