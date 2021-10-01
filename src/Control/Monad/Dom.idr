module Control.Monad.Dom

import Data.IORef

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

public export
interface Monad m => MonadDom m where
  unique : m Nat

--------------------------------------------------------------------------------
--          DomIO
--------------------------------------------------------------------------------

public export
record DomIO (io : Type -> Type) (a : Type) where
  constructor MkDom
  runDom : IORef Nat -> io a

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
HasIO io => MonadDom (DomIO io) where
  unique = MkDom $ \ref => do n <- readIORef ref
                              writeIORef ref $ n+1
                              pure n

--------------------------------------------------------------------------------
--          DomT
--------------------------------------------------------------------------------

public export
record DomT (m : Type -> Type) (a : Type) where
  constructor MkDomT
  runDom : Nat -> m (a, Nat)

export
Functor m => Functor (DomT m) where
  map f (MkDomT run) = MkDomT $ \n => mapFst f <$> run n

export
Monad m => Applicative (DomT m) where
  pure v  = MkDomT $ \n => pure (v,n)
  f <*> v = MkDomT $ \n => do
    (f2,n2) <- f.runDom n
    (v2,n3) <- v.runDom n2
    pure (f2 v2, n3)

export
Monad m => Monad (DomT m) where
  v >>= f = MkDomT $ \n =>
    v.runDom n >>= \(v2,n2) => runDom (f v2) n2

export
HasIO io => HasIO (DomT io) where
  liftIO act = MkDomT $ \n => liftIO ((,n) <$> act)

export
Monad m => MonadDom (DomT m) where
  unique = MkDomT $ \n => pure (n, n+1)
