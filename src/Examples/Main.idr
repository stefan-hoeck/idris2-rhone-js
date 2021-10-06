module Examples.Main

import Control.MonadRec
import Control.Monad.Dom
import Examples.Reset
import Examples.Performance
import JS

--%default total

covering
main : IO ()
main = runJS $ reactimateDom Performance.ui
