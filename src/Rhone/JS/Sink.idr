||| A `Sink` is a monadic streaming function that consumes
||| data but produces no relevant output.
module Rhone.JS.Sink

import Rhone.JS.Reactimate
import Data.Maybe
import Data.MSF
import JS
import Text.Html
import Web.Dom
import Web.Html

%default total

||| Sets the innerHTML property of the referenced node to
||| the input string value.
export %inline
innerHtml : ElemRef t -> MSF JSIO String ()
innerHtml = arrM . rawInnerHtmlAt

||| Replaces the target's child nodes with a `Text` node
||| displaying the input `String`. The `String` will be
||| properly escaped before being inserted.
export
text : ElemRef t -> MSF JSIO String ()
text ref = escape ^>> innerHtml ref

--------------------------------------------------------------------------------
--          Attributes
--------------------------------------------------------------------------------

||| Sets or removes the attribute of the given name
||| at the given target element.
export
attribute : (name : String) -> MSF JSIO (HList [ElemRef t, Maybe String]) ()
attribute name =
  arrM $ \[ref,m] => do
    el <- castElementByRef {t2 = Element} ref
    case m of
      Just s  => setAttribute el name s
      Nothing => removeAttribute el name

||| Sets or unsets the attribute of the given element.
export %inline
attributeAt : (name : String) -> ElemRef t -> MSF JSIO (Maybe String) ()
attributeAt = firstArg . attribute

||| Sets the attribute of the given name at the given target element.
export
attribute_ : (name : String) -> MSF JSIO (HList [ElemRef t, String]) ()
attribute_ name = (\[a,b] => [a,Just b]) ^>> attribute name

||| Sets or unsets the attribute of the given element.
export %inline
attributeAt_ : (name : String) -> ElemRef t -> MSF JSIO String ()
attributeAt_ = firstArg . attribute_

||| Sets or unsets the boolean attribute of the given name at
||| the given target element.
export
boolAttribute : (name : String) -> MSF JSIO (HList [ElemRef t, Bool]) ()
boolAttribute name = (\[a,b] => [a,toMaybe b ""]) ^>> attribute name

||| Sets or unsets the `disabled` attribute of the given element.
export %inline
disabled : MSF JSIO (HList [ElemRef t, Bool]) ()
disabled = boolAttribute "disabled"

||| Sets or unsets the `disabled` attribute of the given element.
export %inline
disabledAt : ElemRef t -> MSF JSIO Bool ()
disabledAt = firstArg disabled

||| Sets or unsets the `hidden` attribute of the given element.
export %inline
hidden : MSF JSIO (HList [ElemRef t, Bool]) ()
hidden = boolAttribute "hidden"

||| Sets or unsets the `hidden` attribute of the given element.
export %inline
hiddenAt : ElemRef t -> MSF JSIO Bool ()
hiddenAt = firstArg hidden

||| Sets the `class` attribute of the given element.
export %inline
class : MSF JSIO (HList [ElemRef t, String]) ()
class = attribute_ "class"

||| Sets the `class` attribute of the given element.
export %inline
classAt : ElemRef t -> MSF JSIO String ()
classAt = firstArg class

--------------------------------------------------------------------------------
--          Input Validation
--------------------------------------------------------------------------------

||| Interface for DOM elements that can have a custom
||| validity message set.
public export
interface SafeCast t => SetValidity t where
  setValidityMessage : t -> String -> JSIO ()

export
SetValidity HTMLButtonElement where
  setValidityMessage = setCustomValidity

export
SetValidity HTMLFieldSetElement where
  setValidityMessage = setCustomValidity

export
SetValidity HTMLInputElement where
  setValidityMessage = setCustomValidity

export
SetValidity HTMLObjectElement where
  setValidityMessage = setCustomValidity

export
SetValidity HTMLOutputElement where
  setValidityMessage = setCustomValidity

export
SetValidity HTMLSelectElement where
  setValidityMessage = setCustomValidity

export
SetValidity HTMLTextAreaElement where
  setValidityMessage = setCustomValidity

export
setValidityMessageAt : SetValidity t => ElemRef t -> String -> JSIO ()
setValidityMessageAt ref s =
  getElementByRef ref >>= (`setValidityMessage` s)

||| Sets a custom validity message at the given target element
export
validityMessageAt : SetValidity t => ElemRef t -> MSF JSIO String ()
validityMessageAt = arrM . setValidityMessageAt

||| Sets or unsets a custom validity message at the given target element
||| depending on whether the input value is a `Left`.
export
leftInvalid :
     {0 x : _}
  -> SetValidity t
  => ElemRef t
  -> MSF JSIO (Either String x) ()
leftInvalid ref = either id (const "") ^>> validityMessageAt ref

--------------------------------------------------------------------------------
--          Value
--------------------------------------------------------------------------------

public export
interface SafeCast t => SetValue t where
  setValue' : String -> t -> JSIO ()

public export
SetValue HTMLButtonElement where
  setValue' = (value =.)

public export
SetValue HTMLDataElement where
  setValue' = (value =.)

public export
SetValue HTMLInputElement where
  setValue' = (value =.)

public export
SetValue HTMLOptionElement where
  setValue' = (value =.)

public export
SetValue HTMLOutputElement where
  setValue' = (value =.)

public export
SetValue HTMLParamElement where
  setValue' = (value =.)

public export
SetValue HTMLSelectElement where
  setValue' = (value =.)

public export
SetValue HTMLTextAreaElement where
  setValue' = (value =.)

public export
SetValue RadioNodeList where
  setValue' = (value =.)

export
setValue : SetValue t => ElemRef t -> String -> JSIO ()
setValue r s = getElementByRef r >>= setValue' s

export
value : SetValue t => MSF JSIO (HList [ElemRef t,String]) ()
value = arrM $ \[r,s] => setValue r s

export %inline
valueOf : SetValue t => ElemRef t -> MSF JSIO String ()
valueOf = firstArg value

export
setChecked : Bool -> HTMLInputElement -> JSIO ()
setChecked b el = set (checked el) b

export
checked : MSF JSIO (HList [ElemRef HTMLInputElement,Bool]) ()
checked = arrM $ \[r,b] => getElementByRef r >>= setChecked b

export %inline
isChecked : ElemRef HTMLInputElement -> MSF JSIO Bool ()
isChecked = firstArg checked

namespace LocalStorage
  export
  setItem : MSF JSIO (HList [String,String]) ()
  setItem = arrM $ \[k,v] =>
    window >>= localStorage >>= (\s => setItem s k v)

  export %inline
  setItemAt : (key : String) -> MSF JSIO String ()
  setItemAt = firstArg setItem

--------------------------------------------------------------------------------
--          Focus
--------------------------------------------------------------------------------

export
setFocus :
     {auto 0 _ : JSType t}
  -> {auto 0 _ : Elem HTMLOrSVGElement (Types t)}
  -> {auto sc  : SafeCast t}
  -> t
  -> JSIO ()
setFocus v = HTMLOrSVGElement.focus v

export
focus :
     {auto 0 _ : JSType t}
  -> {auto 0 _ : Elem HTMLOrSVGElement (Types t)}
  -> {auto sc  : SafeCast t}
  -> MSF JSIO (ElemRef t) ()
focus = arrM $ \r => getElementByRef r >>= setFocus

export %inline
focusAt :
     {auto 0 _ : JSType t}
  -> {auto 0 _ : Elem HTMLOrSVGElement (Types t)}
  -> {auto sc  : SafeCast t}
  -> ElemRef t
  -> MSF JSIO i ()
focusAt r = const r >>> focus
