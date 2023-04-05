module Rhone.JS.ElemRef

import JS
import Text.CSS
import Text.Html
import Web.Dom

%default total

--------------------------------------------------------------------------------
--          ElemRef
--------------------------------------------------------------------------------

||| A typed reference to an element or container in the DOM. Elements can
||| either be referenced by their ID string or their CSS class
||| (both of which must be unique), or by holding a value directly.
||| This can be used to access the element in question,
||| for instance by invoking `getElementByRef`.
|||
||| In addition, we provide (pseudo-)element references for
||| `body`, `document`, and `window`.
public export
data ElemRef : (t : Type) -> Type where
  Id :  {tag   : String}
     -> (0 tpe : ElementType tag t)
     -> (id    : String)
     -> ElemRef t

  Class :  {tag   : String}
        -> (0 tpe : ElementType tag t)
        -> (class : String)
        -> ElemRef t

  Body : ElemRef HTMLElement

  Document : ElemRef Document

  Window : ElemRef Window

||| Predicate witnessing that a given `ElemRef` is a reference
||| by ID.
public export
data ById : ElemRef t -> Type where
  IsById : {0 tpe : _} -> {0 id : _} -> ById (Id tpe id)

||| Predicate witnessing that a given `ElemRef` is a reference
||| by Class.
public export
data ByClass : ElemRef t -> Type where
  IsByClass : {0 tpe : _} -> {0 id : _} -> ByClass (Class tpe id)

namespace Attribute
  ||| Uses an element ref as an ID attribute
  export
  ref : (r : ElemRef t) -> {auto 0 _ : ById r} -> Attribute ev
  ref (Id _ i) = id i

namespace CSS
  ||| Uses an element ref as an ID selector
  export
  idRef : (r : ElemRef t) -> {auto 0 _ : ById r} -> List Declaration -> Rule n
  idRef (Id _ i) = id i

  ||| Uses an element ref as a class selector
  export
  classRef : (r : ElemRef t) -> {auto 0 _ : ByClass r} -> List Declaration -> Rule n
  classRef (Class _ i) = class i

--------------------------------------------------------------------------------
--          Inserting Nodes
--------------------------------------------------------------------------------

||| Tries to retrieve an element of the given type by looking
||| up its ID in the DOM. Unlike `getElementById`, this will throw
||| an exception in the `JSIO` monad if the element is not found
||| or can't be safely cast to the desired type.
export
strictGetElementById : SafeCast t => (tag,id : String) -> JSIO t
strictGetElementById tag id = do
  Nothing <- castElementById t id | Just t => pure t
  liftJSIO $ throwError $
    Caught "Control.Monad.Dom.Interface.strictGetElementById: Could not find \{tag} with id \{id}"

||| Tries to retrieve a HTMLElement by looking
||| up its ID in the DOM. Unlike `getElementById`, this will throw
||| an exception in the `JSIO` monad if the element is not found
||| or can't be safely cast to the desired type.
export %inline
strictGetHTMLElementById : (tag,id : String) -> JSIO HTMLElement
strictGetHTMLElementById = strictGetElementById

||| Tries to retrieve an element of the given type by looking
||| up its ID in the DOM. Unlike `getElementById`, this will throw
||| an exception in the `JSIO` monad if the element is not found
||| or can't be safely cast to the desired type.
export
getElementByRef : SafeCast t => ElemRef t -> JSIO t
getElementByRef (Id {tag} _ id) = strictGetElementById tag id
getElementByRef (Class _ class) = getElementByClass class
getElementByRef Body            = body
getElementByRef Document        = document
getElementByRef Window          = window

err : String
err = "Control.Monad.Dom.Interface.castElementByRef"

||| Tries to retrieve an element of the given type by looking
||| up its ID in the DOM. Unlike `getElementById`, this will throw
||| an exception in the `JSIO` monad if the element is not found
||| or can't be safely cast to the desired type.
export
castElementByRef : SafeCast t2 => ElemRef t -> JSIO t2
castElementByRef (Id {tag} _ id) = strictGetElementById tag id
castElementByRef (Class _ class) = getElementByClass class
castElementByRef Body            = body >>= tryCast err
castElementByRef Document        = document >>= tryCast err
castElementByRef Window          = window >>= tryCast err
