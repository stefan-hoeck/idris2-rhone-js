||| A `Source` is a monadic streaming function that produces
||| values independently of the input value.
module Rhone.JS.Source

import Control.Category
import Control.Monad.Dom
import Data.MSF
import JS
import Text.Html
import Web.Dom
import Web.Html

%default total

--------------------------------------------------------------------------------
--          Value
--------------------------------------------------------------------------------

public export
interface SafeCast t => HasValue t where
  getValue' : t -> JSIO String

public export
HasValue HTMLButtonElement where
  getValue' = to value

public export
HasValue HTMLDataElement where
  getValue' = to value

public export
HasValue HTMLInputElement where
  getValue' = to value

public export
HasValue HTMLOptionElement where
  getValue' = to value

public export
HasValue HTMLOutputElement where
  getValue' = to value

public export
HasValue HTMLParamElement where
  getValue' = to value

public export
HasValue HTMLSelectElement where
  getValue' = to value

public export
HasValue HTMLTextAreaElement where
  getValue' = to value

public export
HasValue RadioNodeList where
  getValue' = to value

export
getValue : LiftJSIO m => HasValue t => t -> m String
getValue = liftJSIO . getValue'

export
value : LiftJSIO m => HasValue t => MSF m (ElemRef t) String
value = arrM $ \r => getElementByRef r >>= getValue

export %inline
valueOf : LiftJSIO m => HasValue t => ElemRef t -> MSF m i String
valueOf r = const r >>> value

export
meterValue : LiftJSIO m => MSF m (ElemRef HTMLMeterElement) Double
meterValue = arrM $ \r => liftJSIO (getElementByRef r >>= to value)

export %inline
meterValueOf : LiftJSIO m => ElemRef HTMLMeterElement -> MSF m i Double
meterValueOf r = const r >>> meterValue

export
progressValue : LiftJSIO m => MSF m (ElemRef HTMLProgressElement) Double
progressValue = arrM $ \r => liftJSIO (getElementByRef r >>= to value)

export %inline
progressValueOf : LiftJSIO m => ElemRef HTMLProgressElement -> MSF m i Double
progressValueOf r = const r >>> progressValue

--------------------------------------------------------------------------------
--          Checked
--------------------------------------------------------------------------------

export
getChecked : LiftJSIO m => HTMLInputElement -> m Bool
getChecked el = liftJSIO $ get el checked

export
checked : LiftJSIO m => MSF m (ElemRef HTMLInputElement) Bool
checked = arrM getElementByRef >>! getChecked

export %inline
checkedAt : LiftJSIO m => ElemRef HTMLInputElement -> MSF m i Bool
checkedAt r = const r >>> checked

--------------------------------------------------------------------------------
--          Routing
--------------------------------------------------------------------------------

export
windowLocation : LiftJSIO m => MSF m i Location
windowLocation = constM $ liftJSIO (window >>= location)

export
windowHash : LiftJSIO m => MSF m i String
windowHash = windowLocation >>! (liftJSIO . to Location.hash)
