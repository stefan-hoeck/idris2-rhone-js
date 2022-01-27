||| A `Sink` is a monadic streaming function that consumes
||| data but produces no relevant output.
module Rhone.JS.Sink

import Control.Category
import Control.Monad.Dom
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
innerHtml : LiftJSIO m => ElemRef t -> MSF m String ()
innerHtml = arrM . rawInnerHtmlAt

||| Replaces the target's child nodes with a `Text` node
||| displaying the input `String`. The `String` will be
||| properly escaped before being inserted.
export
text : LiftJSIO m => ElemRef t -> MSF m String ()
text ref = escape ^>> innerHtml ref

--------------------------------------------------------------------------------
--          Attributes
--------------------------------------------------------------------------------

||| Sets or removes the attribute of the given name
||| at the given target element.
export
attribute :  LiftJSIO m
          => (name : String)
          -> MSF m (NP I [ElemRef t, Maybe String]) ()
attribute name =
  arrM $ \[ref,m] => liftJSIO $ do
    el <- castElementByRef {t2 = Element} ref
    case m of
      Just s  => setAttribute el name s
      Nothing => removeAttribute el name

||| Sets or unsets the attribute of the given element.
export %inline
attributeAt :  LiftJSIO m
            => (name : String)
            -> ElemRef t
            -> MSF m (Maybe String) ()
attributeAt = firstArg . attribute

||| Sets the attribute of the given name at the given target element.
export
attribute_ :  LiftJSIO m
           => (name : String)
           -> MSF m (NP I [ElemRef t, String]) ()
attribute_ name = (\[a,b] => [a,Just b]) ^>> attribute name

||| Sets or unsets the attribute of the given element.
export %inline
attributeAt_ :  LiftJSIO m
             => (name : String)
             -> ElemRef t
             -> MSF m String ()
attributeAt_ = firstArg . attribute_

||| Sets or unsets the boolean attribute of the given name at
||| the given target element.
export
boolAttribute :  LiftJSIO m
              => (name : String)
              -> MSF m (NP I [ElemRef t, Bool]) ()
boolAttribute name = (\[a,b] => [a,toMaybe b ""]) ^>> attribute name

||| Sets or unsets the `disabled` attribute of the given element.
export %inline
disabled : LiftJSIO m => MSF m (NP I [ElemRef t, Bool]) ()
disabled = boolAttribute "disabled"

||| Sets or unsets the `disabled` attribute of the given element.
export %inline
disabledAt : LiftJSIO m => ElemRef t -> MSF m Bool ()
disabledAt = firstArg disabled

||| Sets or unsets the `hidden` attribute of the given element.
export %inline
hidden : LiftJSIO m => MSF m (NP I [ElemRef t, Bool]) ()
hidden = boolAttribute "hidden"

||| Sets or unsets the `hidden` attribute of the given element.
export %inline
hiddenAt : LiftJSIO m => ElemRef t -> MSF m Bool ()
hiddenAt = firstArg hidden

||| Sets the `class` attribute of the given element.
export %inline
class : LiftJSIO m => MSF m (NP I [ElemRef t, String]) ()
class = attribute_ "class"

||| Sets the `class` attribute of the given element.
export %inline
classAt : LiftJSIO m => ElemRef t -> MSF m String ()
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
setValidityMessageAt : SetValidity t => LiftJSIO m => ElemRef t -> String -> m ()
setValidityMessageAt ref s =
  liftJSIO (getElementByRef ref >>= (`setValidityMessage` s))

||| Sets a custom validity message at the given target element
export
validityMessageAt : SetValidity t => LiftJSIO m => ElemRef t -> MSF m String ()
validityMessageAt = arrM . setValidityMessageAt

||| Sets or unsets a custom validity message at the given target element
||| depending on whether the input value is a `Left`.
export
leftInvalid : SetValidity t => LiftJSIO m => ElemRef t -> MSF m (Either String x) ()
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
setValue : LiftJSIO m => SetValue t => ElemRef t -> String -> m ()
setValue r s = getElementByRef r >>= liftJSIO . setValue' s

export
value : LiftJSIO m => SetValue t => MSF m (NP I [ElemRef t,String]) ()
value = arrM $ \[r,s] => setValue r s

export %inline
valueOf : LiftJSIO m => SetValue t => ElemRef t -> MSF m String ()
valueOf = firstArg value

export
setChecked : LiftJSIO m => Bool -> HTMLInputElement -> m ()
setChecked b el = liftJSIO $ set (checked el) b

export
checked : LiftJSIO m => MSF m (NP I [ElemRef HTMLInputElement,Bool]) ()
checked = arrM $ \[r,b] => getElementByRef r >>= setChecked b

export %inline
isChecked : LiftJSIO m => ElemRef HTMLInputElement -> MSF m Bool ()
isChecked = firstArg checked

namespace LocalStorage
  export
  setItem : LiftJSIO m => MSF m (NP I [String,String]) ()
  setItem = arrM $ \[k,v] =>
    liftJSIO (window >>= localStorage >>= (\s => setItem s k v))

  export %inline
  setItemAt : LiftJSIO m => (key : String) -> MSF m String ()
  setItemAt = firstArg setItem

--------------------------------------------------------------------------------
--          Focus
--------------------------------------------------------------------------------

export
setFocus :  LiftJSIO m
         => (0 _ : JSType t)
         => (0 _ : Elem HTMLOrSVGElement (Types t))
         => SafeCast t
         => t -> m ()
setFocus = liftJSIO . HTMLOrSVGElement.focus'

export
focus :  LiftJSIO m
      => (0 _ : JSType t)
      => (0 _ : Elem HTMLOrSVGElement (Types t))
      => SafeCast t
      => MSF m (ElemRef t) ()
focus = arrM $ \r => getElementByRef r >>= setFocus

export %inline
focusAt :  LiftJSIO m
        => (0 _ : JSType t)
        => (0 _ : Elem HTMLOrSVGElement (Types t))
        => SafeCast t
        => ElemRef t -> MSF m i ()
focusAt r = const r >>> focus
