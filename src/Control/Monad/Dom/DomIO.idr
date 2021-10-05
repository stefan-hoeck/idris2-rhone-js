module Control.Monad.Dom.DomIO

import Control.Monad.Dom.Event
import Control.Monad.Dom.Interface
import Control.Monad.Dom.Ref
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

lookupRef : LiftJSIO m => (ref : ElemRef t) -> m (Maybe t)
lookupRef (MkRef et id) = liftJSIO $ htmlElementById et id

lookupRefAsElement : LiftJSIO m => (ref : ElemRef t) -> m (Maybe Element)
lookupRefAsElement (MkRef _ id)  = liftJSIO $ getElementById id

registerEvent : DomEnv e -> (ref : ElemRef t) -> DOMEvent e -> JSIO ()
registerEvent (MkDomEnv _ _ h) r@(MkRef tpe id) de = do
  Just t <- castElementById HTMLElement id | Nothing => pure ()
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

refAndEvents :  DomEnv e
             -> ElementType tag t
             -> Attributes e
             -> JSIO (Attributes e, ElemRef t, List (JSIO ()))
refAndEvents env et as = do
  (ref, as2) <- case getId as of
    Just i  => pure (MkRef et i, as)
    Nothing => (\i => (MkRef et i, id i :: as)) <$> createId env
  pure (as2, ref, map (registerEvent env ref) (getEvents as))

mutual
  reactOnL :  DomEnv e
           -> (ns : List (Node e))
           -> JSIO (List $ Node e, List $ JSIO (), NodesRes ns)
  reactOnL _   []        = pure ([], [], [])
  reactOnL env (x :: xs) = do
    (x2,rx,refsX)   <- reactOn env x
    (xs2,rxs,refsXs)<- reactOnL env xs
    pure (x2 :: xs2, rx ++ rxs, refsX `append` refsXs)

  reactOn :  DomEnv e
          -> (n : Node e)
          -> JSIO (Node e, List $ JSIO (), NodeRes n)
  reactOn env (El False tpe as ys) = do
    (ys2, rs, refs)  <- reactOnL env ys
    (as2, ref, acts) <- refAndEvents env tpe as
    pure (El False tpe as2 ys2, acts ++ rs, refs)

  reactOn env (El True tpe as ys) = do
    (ys2, rs, refs) <- reactOnL env ys
    (as2, ref, acts) <- refAndEvents env tpe as
    pure (El False tpe as2 ys2, acts ++ rs, ref :: refs)

  reactOn env r@(Raw _) = pure (r, [], [])
  reactOn env r@(Text _) = pure (r, [], [])

renderAndInsert :  (String -> JSIO ())
                -> DomEnv ev
                -> (n : Node ev) -> JSIO (NodeRes n)
renderAndInsert f env n = do
  (n2,rs,refs) <- reactOn env n
  f (render n2)
  sequence_ rs
  pure refs

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

public export
record DomIO (event : Type) (io : Type -> Type) (a : Type) where
  constructor MkDom
  runDom : DomEnv event -> io a

env : Monad m => DomIO ev m (DomEnv ev)
env = MkDom pure

--------------------------------------------------------------------------------
--          Implementations
--------------------------------------------------------------------------------

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

export
HasIO io => HasIO (DomIO ev io) where
  liftIO act = MkDom $ \_ => liftIO act

export
LiftJSIO io => LiftJSIO (DomIO ev io) where
  liftJSIO act = MkDom $ \_ => liftJSIO act

-- export
-- LiftJSIO io => MonadDom ev (DomIO ev io) where
--   insertAdjacent ref pos n = do
--     Just el <- lookupRefAsElement ref | Nothing => pure () 
--     e <- env
--     liftJSIO $ renderAndInsert (insertAdjacentHTML el $ positionStr pos) e n
-- 
--   innerHtml ref n = do
--     Just el <- lookupRefAsElement ref | Nothing => pure () 
--     e <- env
--     liftJSIO $ renderAndInsert (set $ innerHTML el) e n
-- 
--   text ref s = do
--     Just el <- lookupRefAsElement ref | Nothing => pure () 
--     liftJSIO $ textContent el `set` s
