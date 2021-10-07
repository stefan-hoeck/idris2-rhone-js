module Examples.Main

import Control.MonadRec
import Control.Monad.Dom
import Examples.Selector
import JS

--%default total

covering
main : IO ()
main = runJS $ reactimateDomIni "reset" "reset" ui
