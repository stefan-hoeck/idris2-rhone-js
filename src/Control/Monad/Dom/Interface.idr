module Control.Monad.Dom.Interface

import Data.SOP
import Text.Html
import Web.Dom

%default total

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

public export
record ElemRef (t : Type) where
  constructor MkRef
  tpe : ElementType s t
  id  : String

public export
data Position = BeforeBegin | AfterBegin | BeforeEnd | AfterEnd

export
positionStr : Position -> String
positionStr BeforeBegin = "beforebegin"
positionStr AfterBegin  = "afterbegin"
positionStr BeforeEnd   = "beforeend"
positionStr AfterEnd    = "afterend"

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

public export
interface Monad m => MonadDom (0 ev : Type) (0 m : Type -> Type) | m where
  uniqueId : m String
  getElem  : ElemRef t -> m (Maybe t)
