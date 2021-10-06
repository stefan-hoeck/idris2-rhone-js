module Control.Monad.Dom.Interface

import Data.SOP
import Text.Html
import Web.Dom

%default total

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

public export
record ElemRef (t : Type) where
  constructor MkRef
  tpe : ElementType s t
  id  : String

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

record PrepRes (m : Type -> Type) (ts : List Type) (ev : Type) where
  constructor MkPrepRes
  node       : Node [] ev
  references : NP ElemRef ts
  events     : List (m ())

record PrepResL (m : Type -> Type) (ts : List Type) (ev : Type) where
  constructor MkPrepResL
  node       : NodeList [] ev
  references : NP ElemRef ts
  events     : List (m ())

getRef :  {0 s : _}
       -> MonadDom ev m
       => Attributes ev
       -> ElementType s t
       -> m (Attributes ev, ElemRef t)
getRef as tpe = case getId as of
  Just i  => pure (as, MkRef tpe i)
  Nothing => (\i => (Id i :: as, MkRef tpe i)) <$> uniqueId

mutual
  prepareNodes : MonadDom ev m => NodeList ts ev -> m (PrepResL m ts ev)
  prepareNodes [] = pure $ MkPrepResL [] [] []
  prepareNodes (n :: ns) = do
    MkPrepRes  n2  rn  en  <- prepareNode n
    MkPrepResL ns2 rns ens <- prepareNodes ns
    pure (MkPrepResL (n2 :: ns2) (rn `append` rns) (en ++ ens))

  prepareNode : MonadDom ev m => Node ts ev -> m (PrepRes m ts ev)
  prepareNode (El tpe as ns) = case getEvents as of
    Nil => do
      MkPrepResL ns2 rns ens <- prepareNodes ns
      pure $ MkPrepRes (El tpe as ns2) rns ens
    es  => do
      (as2,r)                <- getRef as tpe
      MkPrepResL ns2 rns ens <- prepareNodes ns
      pure $ MkPrepRes (El tpe as2 ns2) rns (map (registerEvent r) es ++ ens)

  prepareNode (ElE tpe as ns) = do
    (as2,r)                <- getRef as tpe
    MkPrepResL ns2 rns ens <- prepareNodes ns
    let es = getEvents as
    pure $ MkPrepRes (El tpe as2 ns2) (r :: rns) (map (registerEvent r) es ++ ens)

  prepareNode r@(Raw _)  = pure $ MkPrepRes r [] []
  prepareNode r@(Text _) = pure $ MkPrepRes r [] []
