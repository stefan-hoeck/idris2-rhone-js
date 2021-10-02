module Experimental.Main

import Control.Monad.Dom

import Data.Event
import Data.IORef
import Data.MES
import Data.MSF
import Data.Nat

import Experimental.Syntax
import JS
import Text.Html as Html

%default total

reactimateDom : DomIO JSIO (MES (DomIO JSIO) DomEvent ()) -> JSIO ()
reactimateDom mkMES = do
  hRef  <- newIORef {a = Maybe $ DomEvent -> JSIO ()} Nothing
  idRef <- newIORef {a = Nat} 0
  let env = MkDomEnv idRef $ \ev => do
              Just h <- readIORef hRef | Nothing => pure ()
              h ev
  MkMES sf <- mkMES.runDom env
  sfRef    <- newIORef sf

  writeIORef hRef . Just $ \ev => do
    sf1      <- readIORef sfRef
    (_, sf2) <- runDom (step {m = DomIO JSIO} ev sf1) env
    writeIORef sfRef sf2
  pure ()

covering
main : IO ()
main = runJS $ reactimateDom ui
