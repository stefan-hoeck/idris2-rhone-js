module Experimental.Main

import Data.IORef
import Experimental.Syntax
import JS
import Text.Html as Html

main : IO ()
main = pure () --do
  -- ref <- newIORef 0
  -- runJS $ runDom {io = JSIO} ui ref
