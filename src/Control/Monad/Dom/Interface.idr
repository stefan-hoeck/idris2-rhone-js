module Control.Monad.Dom.Interface

import Text.Html.Event
import Text.Html.Node as Html
import Web.Dom

%default total

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

public export
data ElemRef : Type -> List EventType -> Type where
  Body  : ElemRef HTMLElement []
  Ref :  (tpe    : ElementType str t)
      -> (id     : String)
      -> (events : List EventType)
      -> ElemRef t events

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
interface Monad m => MonadDom m where
  unique : m Nat

  insertAdjacent : ElemRef t es -> Position -> Html.Node -> m ()

  innerHtml : ElemRef t es -> Html.Node -> m ()

  text : ElemRef t es -> String -> m ()

  listenTo : ElemRef t es -> m ()
