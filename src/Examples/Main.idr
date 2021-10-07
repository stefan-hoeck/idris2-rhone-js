module Examples.Main

import Examples.Selector
import Rhone.JS

--%default total

covering
main : IO ()
main = runJS $ reactimateDomIni "reset" "reset" ui
