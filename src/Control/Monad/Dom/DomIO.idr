module Control.Monad.Dom.DomIO

import Control.MonadRec
import Control.Monad.Dom.Event
import Control.Monad.Dom.Interface
import Control.WellFounded
import Data.MSF
import Data.IORef
import JS
import Text.Html
import Web.Dom
import Web.Html

%default total

--------------------------------------------------------------------------------
--          Implementation Utilities
--------------------------------------------------------------------------------

public export
record DomEnv (event : Type) where
  constructor MkDomEnv
  pre      : String
  unique   : IORef Nat
  handler  : event -> JSIO ()

registerImpl : (ref : ElemRef t) -> DOMEvent e -> DomEnv e -> JSIO ()
registerImpl r@(MkRef {tag} _ id) de (MkDomEnv _ _ h) = do
  t <- strictGetHTMLElementById tag id
  case de of
    Input f      => oninput      t !> handle inputInfo f
    Change f     => onchange     t !> handle changeInfo f
    Click f      => onclick      t !> handle mouseInfo f
    DblClick f   => ondblclick   t !> handle mouseInfo f
    KeyDown f    => onkeydown    t !> handle keyInfo f
    KeyUp f      => onkeyup      t !> handle keyInfo f
    MouseDown f  => onmousedown  t !> handle mouseInfo f
    MouseUp f    => onmouseup    t !> handle mouseInfo f
    MouseEnter f => onmouseenter t !> handle mouseInfo f
    MouseLeave f => onmouseleave t !> handle mouseInfo f
    MouseOver f  => onmouseover  t !> handle mouseInfo f
    MouseOut f   => onmouseout   t !> handle mouseInfo f
    MouseMove f  => onmousemove  t !> handle mouseInfo f
  where handle : {0 a,b : _} -> (a -> JSIO b) -> (b -> Maybe e) -> a -> JSIO ()
        handle conv f va = conv va >>= maybe (pure ()) h . f

createId : DomEnv e -> JSIO String
createId (MkDomEnv pre u _) = do
  n <- readIORef u
  writeIORef u (S n)
  pure $ pre ++ show n

--------------------------------------------------------------------------------
--          Implementation
--------------------------------------------------------------------------------

public export
record DomIO (event : Type) (io : Type -> Type) (a : Type) where
  constructor MkDom
  runDom : DomEnv event -> io a

env : Monad m => DomIO ev m (DomEnv ev)
env = MkDom pure

export
Functor io => Functor (DomIO ev io) where
  map f dom = MkDom (map f . dom.runDom)

export
Applicative io => Applicative (DomIO ev io) where
  pure v = MkDom $ \_ => pure v
  f <*> v = MkDom $ \ref => f.runDom ref <*> v.runDom ref

export
Monad io => Monad (DomIO ev io) where
  v >>= f = MkDom $ \ref => v.runDom ref >>= (`runDom` ref) . f


convR :  {0 a,e,b,st : Type}
      -> {0 rel : a -> a -> Type}
      -> (f : (v : a) -> st -> DomIO e m (Step rel v st b))
      -> (env : DomEnv e)
      -> (v : a)
      -> (ini : st)
      -> m (Step rel v st b)
convR f env v s1 = runDom (f v s1) env

export
Monad io => MonadRec io => MonadRec (DomIO e io) where
  tailRecM f x ini acc =
    MkDom $ \env => tailRecM (convR f env) x ini acc

export
HasIO io => HasIO (DomIO ev io) where
  liftIO act = MkDom $ \_ => liftIO act

export
LiftJSIO io => LiftJSIO (DomIO ev io) where
  liftJSIO act = MkDom $ \_ => liftJSIO act

export %inline
LiftJSIO io => MonadDom ev (DomIO ev io) where
  registerEvent ref e = MkDom $ liftJSIO . registerImpl ref e
  uniqueId = MkDom $ liftJSIO . createId

--------------------------------------------------------------------------------
--          Reactimate
--------------------------------------------------------------------------------

reactimateDom_ :  (initialEvent : Maybe ev)
               -> (idPrefix     : String)
               -> DomIO ev JSIO (MSF (DomIO ev JSIO) ev ())
               -> JSIO ()
reactimateDom_ ie pre mkMSF = do
  hRef  <- newIORef {a = Maybe $ ev -> JSIO ()} Nothing
  idRef <- newIORef {a = Nat} 0
  let env = MkDomEnv pre idRef $ \ev => do
              Just h <- readIORef hRef | Nothing => pure ()
              h ev
  sf    <- mkMSF.runDom env
  sfRef <- newIORef sf

  let handle : ev -> JSIO ()
      handle = \e => do
        sf1      <- readIORef sfRef
        (_, sf2) <- runDom (step e sf1) env
        writeIORef sfRef sf2

  writeIORef hRef . Just $ handle
  traverse_ handle ie

export %inline
reactimateDom : (idPrefix : String)
              -> DomIO ev JSIO (MSF (DomIO ev JSIO) ev ())
              -> JSIO ()
reactimateDom = reactimateDom_ Nothing

export %inline
reactimateDomIni :  ev
                 -> (idPrefix : String)
                 -> DomIO ev JSIO (MSF (DomIO ev JSIO) ev ())
                 -> JSIO ()
reactimateDomIni = reactimateDom_ . Just
