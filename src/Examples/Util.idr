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
line : (lbl: String) -> List (Node ev) -> Node ev
line lbl ns =
  div [class widgetLine] $ 
      label [class widgetLabel] [Text lbl] :: ns

