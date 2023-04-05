||| A `Source` is a monadic streaming function that produces
||| values independently of the input value.
module Rhone.JS.Source

import Data.MSF
import JS
import Rhone.JS.ElemRef
import Text.Html
import Web.Dom
import Web.Html

%default total

--------------------------------------------------------------------------------
--          Value
--------------------------------------------------------------------------------

public export
interface SafeCast t => HasValue t where
  getValue : t -> JSIO String

public export
HasValue HTMLButtonElement where
  getValue = to value

public export
HasValue HTMLDataElement where
  getValue = to value

public export
HasValue HTMLInputElement where
  getValue = to value

public export
HasValue HTMLOptionElement where
  getValue = to value

public export
HasValue HTMLOutputElement where
  getValue = to value

public export
HasValue HTMLParamElement where
  getValue = to value

public export
HasValue HTMLSelectElement where
  getValue = to value

public export
HasValue HTMLTextAreaElement where
  getValue = to value

public export
HasValue RadioNodeList where
  getValue = to value

export
value : HasValue t => MSF JSIO (ElemRef t) String
value = arrM $ \r => getElementByRef r >>= getValue

export %inline
valueOf : HasValue t => ElemRef t -> MSF JSIO i String
valueOf r = const r >>> value

export
meterValue : MSF JSIO (ElemRef HTMLMeterElement) Double
meterValue = arrM $ \r => getElementByRef r >>= to value

export %inline
meterValueOf : ElemRef HTMLMeterElement -> MSF JSIO i Double
meterValueOf r = const r >>> meterValue

export
progressValue : MSF JSIO (ElemRef HTMLProgressElement) Double
progressValue = arrM $ \r => getElementByRef r >>= to value

export %inline
progressValueOf : ElemRef HTMLProgressElement -> MSF JSIO i Double
progressValueOf r = const r >>> progressValue

--------------------------------------------------------------------------------
--          Checked
--------------------------------------------------------------------------------

export
getChecked : HTMLInputElement -> JSIO Bool
getChecked el = get el checked

export
checked : MSF JSIO (ElemRef HTMLInputElement) Bool
checked = arrM getElementByRef >>! getChecked

export %inline
checkedAt : ElemRef HTMLInputElement -> MSF JSIO i Bool
checkedAt r = const r >>> checked

--------------------------------------------------------------------------------
--          Routing
--------------------------------------------------------------------------------

export
windowLocation : MSF JSIO i Location
windowLocation = constM $ window >>= location

export
windowHash : MSF JSIO i String
windowHash = windowLocation >>! to Location.hash
