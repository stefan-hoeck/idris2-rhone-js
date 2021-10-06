module Examples.Main

import Control.Monad.Dom
import Examples.Reset
import JS

--%default total

covering
main : IO ()
main = runJS $ reactimateDom ui
