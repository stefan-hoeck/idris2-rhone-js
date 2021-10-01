module Text.Html.Attribute

import Data.List
import Data.String

public export
data Dir = LTR | RTL

export
Show Dir where
  show LTR = "ltr"
  show RTL = "rtl"

public export
data LoadType = Lzy | Eager

export
Show LoadType where
  show Lzy   = "lazy"
  show Eager = "eager"

public export
data InputType =
    Button
  | CheckBox
  | Color
  | Date
  | DateTime
  | Email
  | File
  | Image
  | Month
  | Number
  | Password
  | Radio
  | Range
  | Tel
  | Text
  | Time
  | URL
  | Week

export
Show InputType where
  show Button   = "button"
  show CheckBox = "checkbox"
  show Color    = "color"
  show Date     = "date"
  show DateTime = "datetime-local"
  show Email    = "email"
  show File     = "file"
  show Image    = "image"
  show Month    = "month"
  show Number   = "number"
  show Password = "password"
  show Radio    = "radio"
  show Range    = "range"
  show Tel      = "tel"
  show Text     = "text"
  show Time     = "time"
  show URL      = "url"
  show Week     = "week"

public export
data Attribute : Type where
  StrAttr  : (name : String) -> (value : String) -> Attribute
  BoolAttr : (name : String) -> (value : Bool) -> Attribute

public export
Attributes : Type
Attributes = List Attribute

export
displayAttribute : Attribute -> Maybe String
displayAttribute (StrAttr nm va)     = Just #"\#{nm}="\#{va}""#
displayAttribute (BoolAttr nm True)  = Just nm
displayAttribute (BoolAttr nm False) = Nothing

export
displayAttributes : Attributes -> String
displayAttributes = fastConcat . intersperse " " . mapMaybe displayAttribute

export
getId : Attributes -> Maybe String
getId (StrAttr "id" v :: _) = Just v
getId (_ :: t)              = getId t
getId []                    = Nothing

public export
record Mod (a : Type) where
  constructor MkMod
  appMod : a -> Attribute

export
mkMod : String -> (String -> a -> Attribute) -> Mod a
mkMod nm f = MkMod $ f nm

export
strMod : String -> Mod String
strMod nm =  mkMod nm StrAttr

export
showMod : Show a => String -> Mod a
showMod nm = mkMod nm $ \s => StrAttr s . show

export
boolMod : String -> Mod Bool
boolMod nm = mkMod nm BoolAttr

infixl 8 .=

export %inline
(.=) : Mod a -> a -> Attribute
(.=) = appMod

export
accesskey : Mod String
accesskey = strMod "accesskey"

export
action : Mod String
action = strMod "action"

export
alt : Mod String
alt = strMod "alt"

export
autocapitalize : Mod Bool
autocapitalize = boolMod "autocapitalize"

export
autocomplete : Mod Bool
autocomplete = boolMod "autocomplete"

export
autofocus : Mod Bool
autofocus = boolMod "autofocus"

export
autoplay : Mod Bool
autoplay = boolMod "autoplay"

export
checked : Mod Bool
checked = boolMod "checked"

export
cite : Mod String
cite = strMod "cite"

export
class : Mod String
class = strMod "class"

export
cols : Mod Bits32
cols = showMod "cols"

export
colspan : Mod Bits32
colspan = showMod "colspan"

export
contenteditable : Mod Bool
contenteditable = boolMod "contenteditable"

export
controls : Mod Bool
controls = boolMod "controls"

export
data_ : Mod String
data_ = strMod "data"

export
dir : Mod Dir
dir = showMod "dir"

export
disabled : Mod Bool
disabled = boolMod "disabled"

export
download : Mod String
download = strMod "download"

export
draggable : Mod Bool
draggable = boolMod "draggable"

export
for : Mod String
for = strMod "for"

export
form : Mod String
form = strMod "form"

export
height : Mod Bits32
height = showMod "height"

export
hidden : Mod Bool
hidden = boolMod "hidden"

export
href : Mod String
href = strMod "href"

export
hreflang : Mod String
hreflang = strMod "hreflang"

export
id : Mod String
id = strMod "id"

export
label : Mod String
label = strMod "label"

export
lang : Mod String
lang = strMod "lang"

export
loading : Mod LoadType
loading = showMod "loading"

export
list : Mod String
list = strMod "list"

export
loop : Mod Bool
loop = boolMod "loop"

export
maxlength : Mod Bits32
maxlength = showMod "maxlength"

export
minlength : Mod Bits32
minlength = showMod "minlength"

export
multiple : Mod Bool
multiple = boolMod "multiple"

export
muted : Mod Bool
muted = boolMod "muted"

export
name : Mod String
name = strMod "name"

export
placeholder : Mod String
placeholder = strMod "placeholder"

export
readonly : Mod Bool
readonly = boolMod "readonly"

export
required : Mod Bool
required = boolMod "required"

export
reverse : Mod Bool
reverse = boolMod "reverse"

export
rows : Mod Bits32
rows = showMod "rows"

export
rowspan : Mod Bits32
rowspan = showMod "rowspan"

export
selected : Mod String
selected = strMod "selected"

export
spellcheck : Mod Bool
spellcheck = boolMod "spellcheck"

export
src : Mod String
src = strMod "src"

export
style : Mod String
style = strMod "style"

export
target : Mod String
target = strMod "target"

export
title : Mod String
title = strMod "title"

export
type : Mod InputType
type = showMod "type"

export
value : Mod String
value = strMod "value"

export
width : Mod Bits32
width = showMod "width"

export
wrap : Mod Bool
wrap = boolMod "wrap"
