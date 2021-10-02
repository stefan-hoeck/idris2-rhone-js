module Control.Monad.Dom.DomIO

import Control.Monad.Dom.Event
import Control.Monad.Dom.Interface
import Data.IORef
import JS
import Text.Html.Event
import Text.Html.Node as Html
import Web.Dom
import Web.Html
import Web.Raw.UIEvents

%hide Types.Event
%default total

--------------------------------------------------------------------------------
--          Implementation Utilities
--------------------------------------------------------------------------------

lookupRef : LiftJSIO m => (ref : ElemRef t es) -> m (Maybe t)
lookupRef Body          = liftJSIO $ document >>= to body
lookupRef (Ref et id _) = liftJSIO $ htmlElementById et id

lookupRefAsElement : LiftJSIO m => (ref : ElemRef t es) -> m (Maybe Element)
lookupRefAsElement Body          =
  liftJSIO $ map (\h => up h) <$> (document >>= to body)
lookupRefAsElement (Ref _ id _)  = liftJSIO $ getElementById id

listenImpl :  (handler : DomEvent -> JSIO ())
           -> (el : HTMLElement)
           -> (tpe : EventType)
           -> JSIO ()
listenImpl h el Click =
  onclick el !> (\e => toMouseInfo e >>= h . Click)
listenImpl h el DblClick =
  ondblclick el !> (\e => toMouseInfo e >>= h . DblClick)

listenRefImpl : (handler : DomEvent -> JSIO ()) -> ElemRef t es -> JSIO ()
listenRefImpl _ Body          = pure ()
listenRefImpl h (Ref _ id es) = do
  Just el <- castElementById_ id | Nothing => pure ()
  traverse_ (listenImpl h el) es

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

public export
record DomEnv where
  constructor MkDomEnv
  uniqueId : IORef Nat
  handler  : DomEvent -> JSIO ()

public export
record DomIO (io : Type -> Type) (a : Type) where
  constructor MkDom
  runDom : DomEnv -> io a

--------------------------------------------------------------------------------
--          Implementations
--------------------------------------------------------------------------------

export
Functor io => Functor (DomIO io) where
  map f dom = MkDom (map f . dom.runDom)

export
Applicative io => Applicative (DomIO io) where
  pure v = MkDom $ \_ => pure v
  f <*> v = MkDom $ \ref => f.runDom ref <*> v.runDom ref

export
Monad io => Monad (DomIO io) where
  v >>= f = MkDom $ \ref => v.runDom ref >>= (`runDom` ref) . f

export
HasIO io => HasIO (DomIO io) where
  liftIO act = MkDom $ \_ => liftIO act

export
LiftJSIO io => LiftJSIO (DomIO io) where
  liftJSIO act = MkDom $ \_ => liftJSIO act

export
LiftJSIO io => MonadDom (DomIO io) where
  unique = MkDom $ \env => do
    n <- readIORef env.uniqueId
    writeIORef env.uniqueId $ n+1
    pure n

  insertAdjacent ref pos n = do
    Just el <- lookupRefAsElement ref | Nothing => pure () 
    liftJSIO $ insertAdjacentHTML el (positionStr pos) (render n)

  innerHtml ref n = do
    Just el <- lookupRefAsElement ref | Nothing => pure () 
    liftJSIO $ innerHTML el `set` render n

  text ref s = do
    Just el <- lookupRefAsElement ref | Nothing => pure () 
    liftJSIO $ textContent el `set` s

  listenTo ref = 
    MkDom $ \env => liftJSIO $ listenRefImpl env.handler ref
