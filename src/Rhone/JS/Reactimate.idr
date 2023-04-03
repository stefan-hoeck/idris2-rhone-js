module Rhone.JS.Reactimate

import Data.IORef
import Data.MSF
import Data.Nat
import JS
import Rhone.JS.Event
import Text.CSS
import Text.Html
import Web.Dom
import Web.Html

%default total

primTraverse_ : (t -> JSIO ()) -> List t -> PrimIO (Either JSErr ())
primTraverse_ f []        w = MkIORes (Right ()) w
primTraverse_ f (x :: xs) w =
  let MkIORes (Right ()) w2 := toPrim (runEitherT (f x)) w
        | MkIORes (Left err) w2 => MkIORes (Left err) w2
   in primTraverse_ f xs w2

-- TODO : This should got to the JS module
export
traverseJSIO_ : (t -> JSIO ()) -> List t -> JSIO ()
traverseJSIO_ f xs = MkEitherT $ fromPrim $ primTraverse_ f xs

-- TODO : This should got to the JS module
export %inline
forJSIO_ : List t -> (t -> JSIO ()) -> JSIO ()
forJSIO_ as f = traverseJSIO_ f as

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
  Id :  {tag : String}
     -> (tpe : ElementType tag t)
     -> (id : String)
     -> ElemRef t

  Class :  {tag   : String}
        -> (tpe   : ElementType tag t)
        -> (class : String)
        -> ElemRef t

  Ref : (ref : t) -> ElemRef t

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
--          Types
--------------------------------------------------------------------------------

public export
data Position = BeforeBegin | AfterBegin | BeforeEnd | AfterEnd

export
positionStr : Position -> String
positionStr BeforeBegin = "beforebegin"
positionStr AfterBegin  = "afterbegin"
positionStr BeforeEnd   = "beforeend"
positionStr AfterEnd    = "afterend"

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
getElementByRef (Ref t)         = pure t
getElementByRef Body            = liftJSIO body
getElementByRef Document        = liftJSIO document
getElementByRef Window          = liftJSIO window

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
castElementByRef (Ref t)         = tryCast err t

||| Low level method for registering `DOMEvents` at
||| HTML elements.
|||
||| Use this, for instance, to register `DOMEvents` at
||| a HTMLElement of a static document.
export
registerDOMEvent : Handler JSIO e => EventTarget -> DOMEvent e -> JSIO ()
registerDOMEvent el de = case de of
  Input f      => inst "input" inputInfo f
  Change f     => inst "change" changeInfo f
  Click f      => inst "click" mouseInfo f
  DblClick f   => inst "dblclick" mouseInfo f
  KeyDown f    => inst "keydown" keyInfo f
  KeyUp f      => inst "keyup" keyInfo f
  Blur v       => inst "blur" {a = Event} (const $ pure v) Just
  Focus v      => inst "focus" {a = Event} (const $ pure v) Just
  MouseDown f  => inst "mousedown" mouseInfo f
  MouseUp f    => inst "mouseup" mouseInfo f
  MouseEnter f => inst "mouseenter" mouseInfo f
  MouseLeave f => inst "mouseleave" mouseInfo f
  MouseOver f  => inst "mouseover" mouseInfo f
  MouseOut f   => inst "mouseout" mouseInfo f
  MouseMove f  => inst "mousemove" mouseInfo f
  HashChange v => inst "hashchange" {a = Event} (const $ pure v) Just

  where
    inst :
         {0 a,b : _}
      -> {auto c : SafeCast a}
      -> String
      -> (a -> JSIO b)
      -> (b -> Maybe e)
      -> JSIO ()
    inst s conv f = do
      c <- callback {cb = EventListener} $ \e => do
        va <- tryCast_ a "Control.Monad.Dom.Interface.inst" e
        conv va >>= maybe (pure ()) handle . f

      addEventListener el s (Just c)

parameters {0    e : Type}
           {auto h : Handler JSIO e}

  ||| Manually register an event handler at the given element
  export
  handleEvent : ElemRef t -> DOMEvent e -> JSIO ()
  handleEvent ref de = do
    el  <- castElementByRef ref
    registerDOMEvent el de

  export
  setAttribute : Element -> Attribute e -> JSIO ()
  setAttribute el (Id value)        = setAttribute el "id" value
  setAttribute el (Str name value)  = setAttribute el name value
  setAttribute el (Bool name value) = case value of
    True  => setAttribute el name ""
    False => removeAttribute el name
  setAttribute el (Event ev) = registerDOMEvent (up el) ev

  export
  setAttributeRef : ElemRef t -> Attribute e -> JSIO ()
  setAttributeRef ref a = do
    el <- castElementByRef {t2 = Element} ref
    setAttribute el a

  export
  setAttributesRef : ElemRef t -> List (Attribute e) -> JSIO ()
  setAttributesRef el = traverseJSIO_ (setAttributeRef el)

--------------------------------------------------------------------------------
--          Node Preparation
--------------------------------------------------------------------------------

parameters {0    e : Type}           -- event type
           {auto h : Handler JSIO e} -- event handler

  createNode : Document -> String -> List (Attribute e) -> JSIO Element
  createNode doc str xs = do
    el <- createElement doc str
    traverseJSIO_ (setAttribute el) xs
    pure el

  addNodes :
       {auto 0 _ : JSType t}
    -> {auto 0 _ : Elem ParentNode (Types t)}
    -> (doc      : Document)
    -> (parent   : t)
    -> (nodes    : List (Node e))
    -> JSIO ()

  addNode :
       {auto 0 _ : JSType t}
    -> {auto 0 _ : Elem ParentNode (Types t)}
    -> (doc      : Document)
    -> (parent   : t)
    -> (node     : Node e)
    -> JSIO ()
  addNode doc p (El tag xs ys) = do
    n <- createNode doc tag xs
    append p [inject $ n :> Node]
    addNodes doc n ys
  addNode doc p (Raw str) = do
    el <- createElement doc "template"
    Just temp <- pure (castTo HTMLTemplateElement el) | Nothing => pure ()
    innerHTML temp .= str
    c         <- content temp
    append p [inject $ c :> Node]

  addNode doc p (Text str) = append p [inject str]

  addNodes doc p = assert_total $ traverseJSIO_ (addNode doc p)

  ||| Sets up the reactive behavior of the given `Node` and
  ||| inserts it as the only child of the given target.
  |||
  ||| This adds unique IDs and event listeners to the generated
  ||| nodes as required in their attributes.
  export
  innerHtmlAtN : ElemRef t -> List (Node e) -> JSIO ()
  innerHtmlAtN ref ns = do
    doc  <- document
    elem <- castElementByRef {t2 = Element} ref
    innerHTML elem .= ""
    df   <- createDocumentFragment doc
    addNodes doc df ns
    append elem [inject $ df :> Node]

  ||| Sets up the reactive behavior of the given `Node` and
  ||| inserts it as the only child of the given target.
  |||
  ||| This adds unique IDs and event listeners to the generated
  ||| nodes as required in their attributes.
  export %inline
  innerHtmlAt : ElemRef t -> Node e -> JSIO ()
  innerHtmlAt ref n = innerHtmlAtN ref [n]

||| Replaces the `innerHTML` property of the target with the
||| given `String`. Warning: The string will not be escaped
||| before being inserted, so don't use this with text from
||| untrusted sources.
export
rawInnerHtmlAt : ElemRef t -> String -> JSIO ()
rawInnerHtmlAt ref str = do
  elem <- castElementByRef {t2 = Element} ref
  innerHTML elem .= str
