module Control.Monad.Dom.Interface

import Control.MonadRec
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.WellFounded
import Data.Iterable
import Data.Nat
import Data.SOP
import JS
import Text.CSS
import Text.Html
import Web.Dom

%default total

--------------------------------------------------------------------------------
--          ElemRef
--------------------------------------------------------------------------------

||| A typed reference to an element in the DOM. Elements can
||| either be referenced by their ID string or their CSS class
||| (both of which must be unique), or by holding a value directly.
||| This can be used to access the element in question,
||| for instance by invoking `getElementByRef`.
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
  idRef : (r : ElemRef t) -> {auto 0 _ : ById r} -> Selector 0 False False
  idRef (Id _ i) = Id i

  ||| Uses an element ref as a class selector
  export
  classRef : (r : ElemRef t) -> {auto 0 _ : ByClass r} -> Selector 0 False False
  classRef (Class _ i) = Class i

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
--          Interface
--------------------------------------------------------------------------------

||| Interface for setting up interactions with the DOM
public export
interface Monad m => MonadDom (0 ev : Type) (0 m : Type -> Type) | m where
  ||| Generates a new unique ID
  uniqueId      : m String

  ||| Registers an event listener at the given DOM element,
  ||| acting on events provided by the given `DOMEvent` value.
  registerEvent : ElemRef t -> DOMEvent ev -> m ()

export
MonadDom ev m => MonadDom ev (ReaderT e m) where
  uniqueId          = lift uniqueId
  registerEvent r e = lift (registerEvent r e)

export
MonadDom ev m => MonadDom ev (StateT s m) where
  uniqueId          = lift uniqueId
  registerEvent r e = lift (registerEvent r e)

export
MonadDom ev m => MonadDom ev (WriterT w m) where
  uniqueId          = lift uniqueId
  registerEvent r e = lift (registerEvent r e)

--------------------------------------------------------------------------------
--          Node Preparation
--------------------------------------------------------------------------------

-- generates an `ElemRef` for the given HTMLElement type `t`,
-- either by using the ID already defined in the attribute list,
-- or by creating a new unique ID.
--
-- This ID will be used by `innerHtmlAt` to properly set up the
-- necessary event listeners.
getRef :  {str : String}
       -> MonadDom ev m
       => Attributes ev
       -> ElementType str t
       -> m (Attributes ev, ElemRef t)
getRef as tpe = case getId as of
  Just i  => pure (as, Id tpe i)
  Nothing => (\i => (Id i :: as, Id tpe i)) <$> uniqueId

PrepareRes : Type -> (Type -> Type) -> Type
PrepareRes ev m = (List (Node ev), List (m ()))

mutual
  -- tail-call optimized preparation of nodes
  -- the totality checker needs a wee bit of help for this
  prepareNodes :  MonadDom ev m
               => MonadRec m
               => List (Node ev)
               -> m (PrepareRes ev m)
  prepareNodes ns = trSized go ns ([], [])
    where go :  (vs : List (Node ev))
             -> PrepareRes ev m
             -> m (Step Smaller vs (PrepareRes ev m) (PrepareRes ev m))
          go [] (      ns2,ens) = pure $ Done (reverse ns2, ens)
          go (h :: t) (ns2,ens) = do
            (n2, en) <- assert_total $ prepareNode h
            pure $ Cont t (reflexive {rel = LTE}) (n2 :: ns2, en ++ ens)

  -- inserts unique IDs where necessary and extracts a list of
  -- actions, which will register the necessary event listeners
  -- after creating the nodes in the DOM
  prepareNode :  MonadDom ev m
              => MonadRec m
              => Node ev
              -> m (Node ev, List $ m ())
  prepareNode (El tpe as ns) = case getEvents as of
    Nil => do
      (ns2, ens) <- prepareNodes ns
      pure (El tpe as ns2, ens)
    es  => do
      (as2,r)    <- getRef as tpe
      (ns2, ens) <- prepareNodes ns
      pure $ (El tpe as2 ns2, map (registerEvent r) es ++ ens)

  prepareNode r@(Raw _)  = pure (r,[])
  prepareNode r@(Text _) = pure (r,[])

--------------------------------------------------------------------------------
--          Inserting Nodes
--------------------------------------------------------------------------------

||| Tries to retrieve an element of the given type by looking
||| up its ID in the DOM. Unlike `getElementById`, this will throw
||| an exception in the `JSIO` monad if the element is not found
||| or can't be safely cast to the desired type.
export
strictGetElementById : LiftJSIO m => SafeCast t => (tag,id : String) -> m t
strictGetElementById tag id = do
  Nothing <- castElementById t id | Just t => pure t
  liftJSIO $ throwError $
    Caught "Control.Monad.Dom.Interface.strictGetElementById: Could not find \{tag} with id \{id}"

||| Tries to retrieve a HTMLElement by looking
||| up its ID in the DOM. Unlike `getElementById`, this will throw
||| an exception in the `JSIO` monad if the element is not found
||| or can't be safely cast to the desired type.
export %inline
strictGetHTMLElementById : LiftJSIO m => (tag,id : String) -> m HTMLElement
strictGetHTMLElementById = strictGetElementById

||| Tries to retrieve an element of the given type by looking
||| up its ID in the DOM. Unlike `getElementById`, this will throw
||| an exception in the `JSIO` monad if the element is not found
||| or can't be safely cast to the desired type.
export
getElementByRef : LiftJSIO m => SafeCast t => ElemRef t -> m t
getElementByRef (Id {tag} _ id) = strictGetElementById tag id
getElementByRef (Class _ class) = getElementByClass class
getElementByRef (Ref t)         = pure t

||| Tries to retrieve an element of the given type by looking
||| up its ID in the DOM. Unlike `getElementById`, this will throw
||| an exception in the `JSIO` monad if the element is not found
||| or can't be safely cast to the desired type.
export
castElementByRef : LiftJSIO m => SafeCast t2 => ElemRef t -> m t2
castElementByRef (Id {tag} _ id) = strictGetElementById tag id
castElementByRef (Class _ class) = getElementByClass class
castElementByRef (Ref t)         =
  liftJSIO $ tryCast "Control.Monad.Dom.Interface.castElementByRef" t

||| Sets up the reactive behavior of the given `Node` and
||| inserts it as the only child of the given target.
|||
||| This adds unique IDs and event listeners to the generated
||| nodes as required in their attributes.
export
innerHtmlAtN :  LiftJSIO m
             => MonadRec m
             => MonadDom ev m
             => ElemRef t
             -> List (Node ev)
             -> m ()
innerHtmlAtN ref ns = do
  elem     <- castElementByRef {t2 = Element} ref
  (n2, es) <- prepareNodes ns
  liftJSIO $ innerHTML elem .= renderMany n2
  forM_ (\x => x) es

||| Sets up the reactive behavior of the given `Node` and
||| inserts it as the only child of the given target.
|||
||| This adds unique IDs and event listeners to the generated
||| nodes as required in their attributes.
export
innerHtmlAt :  LiftJSIO m
            => MonadRec m
            => MonadDom ev m
            => ElemRef t
            -> Node ev
            -> m ()
innerHtmlAt ref n = do
  elem     <- castElementByRef {t2 = Element} ref
  (n2, es) <- prepareNode n
  liftJSIO $ innerHTML elem .= render n2
  forM_ (\x => x) es

||| Replaces the `innerHTML` property of the target with the
||| given `String`. Warning: The string will not be escaped
||| before being inserted, so don't use this with text from
||| untrusted sources.
export
rawInnerHtmlAt :  LiftJSIO m
               => ElemRef t
               -> String
               -> m ()
rawInnerHtmlAt ref str = liftJSIO $ do
  elem <- castElementByRef {t2 = Element} ref
  liftJSIO $ innerHTML elem .= str
