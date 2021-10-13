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

||| Renders the input `Node` and sets the innerHTML property
||| of the target. Does not register any event listeners.
export
rawInnerHtml : LiftJSIO m => ElemRef t -> MSF m (Node ev) ()
rawInnerHtml ref = arrM $ rawInnerHtmlAt ref . render

||| Replaces the target's child nodes with a `Text` node
||| displaying the input `String`. The `String` will be
||| properly escaped before being inserted.
export
text : LiftJSIO m => ElemRef t -> MSF m String ()
text ref = arr Text >>> rawInnerHtml {ev = ()} ref

--------------------------------------------------------------------------------
--          Attributes
--------------------------------------------------------------------------------

||| Sets or removes the attribute of the given name
||| at the given target element.
export
attribute :  LiftJSIO m
          => (name : String)
          -> MSF m (ElemRef t, Maybe String) ()
attribute name =
  arrM $ \(MkRef {tag} _ id, m) => liftJSIO $ do
    el <- strictGetElementById {t = Element} tag id
    case m of
      Just s  => setAttribute el name s
      Nothing => removeAttribute el name

||| Sets the attribute of the given name at the given target element.
export
attribute_ : LiftJSIO m => (name : String) -> MSF m (ElemRef t, String) ()
attribute_ name = mapSnd Just ^>> attribute name

||| Sets or unsets the boolean attribute of the given name at
||| the given target element.
export
boolAttribute : LiftJSIO m => (name : String) -> MSF m (ElemRef t, Bool) ()
boolAttribute name = mapSnd (`toMaybe` "") ^>> attribute name

||| Sets or unsets the `disabled` attribute of the given element.
export %inline
disabled : LiftJSIO m => MSF m (ElemRef t, Bool) ()
disabled = boolAttribute "disabled"

||| Sets or unsets the `disabled` attribute of the given element.
export %inline
disabledAt : LiftJSIO m => ElemRef t -> MSF m Bool ()
disabledAt = firstArg disabled

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
