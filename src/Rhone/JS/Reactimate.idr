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

--------------------------------------------------------------------------------
--          Unique
--------------------------------------------------------------------------------

||| A utility for generating unique IDs
public export
record Unique where
  [noHints]
  constructor U
  unique : IO String

||| A new generator of unique IDs. These will consist of a natural number
||| prefixed with the given string.
export
mkUnique : (pre : String) -> IO Unique
mkUnique pre = do
  ref <- newIORef Z
  pure . U $ do
    n <- readIORef ref
    writeIORef ref (S n)
    pure $ pre ++ show n

||| Generate a unique ID to be used in the DOM
export %inline
uniqueId : {auto u : Unique} -> IO String
uniqueId = u.unique

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

      addEventListener' el s (Just c)

parameters {0    e : Type}
           {auto h : Handler JSIO e}

  ||| Manually register an event handler at the given element
  export
  handleEvent : ElemRef t -> DOMEvent e -> JSIO ()
  handleEvent ref de = do
    el  <- castElementByRef ref
    registerDOMEvent el de

  export
  setAttribute : ElemRef t -> Attribute e -> JSIO ()
  setAttribute ref a = do
    el <- castElementByRef {t2 = HTMLElement} ref
    case a of
      Id v         => setAttribute el "id" v
      Str n v      => setAttribute el n v
      Bool n True  => setAttribute el n ""
      Bool n False => removeAttribute el n
      Event ev     => registerDOMEvent (up el) ev

  export
  setAttributes : ElemRef t -> List (Attribute e) -> JSIO ()
  setAttributes el = traverseJSIO_ (setAttribute el)

--------------------------------------------------------------------------------
--          Node Preparation
--------------------------------------------------------------------------------

parameters {0    e : Type}           -- event type
           {auto h : Handler JSIO e} -- event handler
           {auto u : Unique}         -- unique ID generator

  -- generates an `ElemRef` for the given HTMLElement type `t`,
  -- either by using the ID already defined in the attribute list,
  -- or by creating a new unique ID.
  --
  -- This ID will be used by `innerHtmlAt` to properly set up the
  -- necessary event listeners.
  getRef :
       {str : String}
    -> Attributes e
    -> ElementType str t
    -> IO (Attributes e, ElemRef t)
  getRef as tpe = case getId as of
    Just i  => pure (as, Id tpe i)
    Nothing => (\i => (Id i :: as, Id tpe i)) <$> uniqueId

  0 PrepareRes : Type
  PrepareRes = (List (Node e), List (JSIO ()))

  -- inserts unique IDs where necessary and extracts a list of
  -- actions, which will register the necessary event listeners
  -- after creating the nodes in the DOM
  prepareNode : Node e -> PrimIO (Node e, List $ JSIO ())

  prepareNodes :
       SnocList (Node e)
    -> SnocList (JSIO ())
    -> List (Node e)
    -> PrimIO PrepareRes
  prepareNodes sx sy []        w = MkIORes (sx <>> [], sy <>> []) w
  prepareNodes sx sy (x :: xs) w =
    let MkIORes (x2,ys) w2 := prepareNode x w
     in prepareNodes (sx :< x2) (sy <>< ys) xs w2

  prepareNode (El tpe as ns) w = case getEvents as of
    Nil =>
      let MkIORes (ns2, ens) w2 := prepareNodes [<] [<] ns w
       in MkIORes (El tpe as ns2, ens) w2
    es  =>
      let MkIORes (as2,r)    w2 := toPrim (getRef as tpe) w
          MkIORes (ns2, ens) w3 := prepareNodes [<] [<] ns w2
       in MkIORes (El tpe as2 ns2, map (handleEvent r) es ++ ens) w3

  prepareNode r@(Raw _)  w = MkIORes (r,[]) w
  prepareNode r@(Text _) w = MkIORes (r,[]) w

  ||| Sets up the reactive behavior of the given `Node` and
  ||| inserts it as the only child of the given target.
  |||
  ||| This adds unique IDs and event listeners to the generated
  ||| nodes as required in their attributes.
  export
  innerHtmlAtN : ElemRef t -> List (Node e) -> JSIO ()
  innerHtmlAtN ref ns = do
    elem     <- castElementByRef {t2 = Element} ref
    (n2, es) <- liftIO $ fromPrim $ prepareNodes [<] [<] ns
    innerHTML elem .= renderMany n2
    traverseJSIO_ (\x => x) es

  ||| Sets up the reactive behavior of the given `Node` and
  ||| inserts it as the only child of the given target.
  |||
  ||| This adds unique IDs and event listeners to the generated
  ||| nodes as required in their attributes.
  export
  innerHtmlAt : ElemRef t -> Node e -> JSIO ()
  innerHtmlAt ref n = do
    elem     <- castElementByRef {t2 = Element} ref
    (n2, es) <- liftIO $ fromPrim $ prepareNode n
    innerHTML elem .= render n2
    traverseJSIO_ (\x => x) es

||| Replaces the `innerHTML` property of the target with the
||| given `String`. Warning: The string will not be escaped
||| before being inserted, so don't use this with text from
||| untrusted sources.
export
rawInnerHtmlAt : ElemRef t -> String -> JSIO ()
rawInnerHtmlAt ref str = do
  elem <- castElementByRef {t2 = Element} ref
  innerHTML elem .= str
