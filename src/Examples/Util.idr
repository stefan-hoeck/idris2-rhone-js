||| Reocurring fragments from the example applications 
module Examples.Util

import Data.MSF.Event
import Rhone.JS
import Examples.CSS

public export
MSFEvent : Type -> Type
MSFEvent = Data.MSF.Event.Event

--------------------------------------------------------------------------------
--          Usefule Nodes
--------------------------------------------------------------------------------

export
lbl : (text: String) -> (class : String) -> Node ev
lbl txt cl = label [classes [widgetLabel, cl]] [Text txt]

