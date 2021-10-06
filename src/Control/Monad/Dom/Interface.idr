module Control.Monad.Dom.Interface

import Control.MonadRec
import Control.WellFounded
import Data.Iterable
import Data.Nat
import Data.SOP
import JS
import Text.Html
import Web.Dom

%default total

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

public export
record ElemRef (t : Type) where
  constructor MkRef
  {tag : String}
  tpe  : ElementType tag t
  id   : String

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

public export
interface Monad m => MonadDom (0 ev : Type) (0 m : Type -> Type) | m where
  uniqueId      : m String
  registerEvent : ElemRef t -> DOMEvent ev -> m ()

--------------------------------------------------------------------------------
--          Node Preparation
--------------------------------------------------------------------------------

getRef :  {str : String}
       -> MonadDom ev m
       => Attributes ev
       -> ElementType str t
       -> m (Attributes ev, ElemRef t)
getRef as tpe = case getId as of
  Just i  => pure (as, MkRef tpe i)
  Nothing => (\i => (Id i :: as, MkRef tpe i)) <$> uniqueId

PrepareRes : Type -> (Type -> Type) -> Type
PrepareRes ev m = (List (Node ev), List (m ()))

mutual
  covering
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
            (n2, en) <- prepareNode h
            pure $ Cont t (reflexive {rel = LTE}) (n2 :: ns2, en ++ ens)

  covering
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

export
strictGetElementById : SafeCast t => (tag,id : String) -> JSIO t
strictGetElementById tag id = do
  Nothing <- castElementById t id | Just t => pure t
  throwError $ Caught #"Control.Monad.Dom.Interface.strictGetElementById: Could not find \#{tag} with id \#{id}"#

export %inline
strictGetHTMLElementById : (tag,id : String) -> JSIO HTMLElement
strictGetHTMLElementById = strictGetElementById

export
getElementByRef : SafeCast t => ElemRef t -> JSIO t
getElementByRef (MkRef {tag} _ id) = strictGetElementById tag id

export covering
innerHtmlAt :  LiftJSIO m
            => MonadRec m
            => MonadDom ev m
            => ElemRef t
            -> Node ev
            -> m ()
innerHtmlAt (MkRef {tag} _ id) n = do
  elem     <- liftJSIO $ strictGetHTMLElementById tag id
  (n2, es) <- prepareNode n
  liftJSIO $ innerHTML elem .= render n2
  forM_ (\x => x) es

export
rawInnerHtmlAt : LiftJSIO m => ElemRef t -> String -> m ()
rawInnerHtmlAt (MkRef {tag} _ id) str = liftJSIO $ do
  elem <- strictGetHTMLElementById tag id
  innerHTML elem .= str
