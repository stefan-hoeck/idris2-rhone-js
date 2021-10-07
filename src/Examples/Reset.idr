module Examples.Reset

import JS
import Control.MonadRec
import Control.Monad.Dom
import Control.Category
import Data.MSF
import Data.SOP
import Examples.CSS
import Text.Html as Html
import Text.CSS as CSS
import Web.Dom

innerHtml : LiftJSIO m => ElemRef t -> MSF m (Node ev) ()
innerHtml ref = arrM $ rawInnerHtmlAt ref . render

text : LiftJSIO m => ElemRef t -> MSF m String ()
text ref = arr Text >>> innerHtml {ev = ()} ref

--------------------------------------------------------------------------------
--          CSS Classes
--------------------------------------------------------------------------------

inc : String
inc = "inc"

output : String
output = "output"

grid : String
grid = "grid"

--------------------------------------------------------------------------------
--          CSS
--------------------------------------------------------------------------------

css : List Rule
css =
  [ class output  !!
      [ FontSize        .= Large
      , Margin          .= pt 5
      , TextAlign       .= End
      , Width           .= perc 10
      ]

  , class grid  !!
      [ Display         .= Flex
      , FlexWrap        .= "wrap"
      ]

  , class inc  !!
      [ Margin          .= pt 5
      , Width           .= perc 10
      ]
  ]

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

public export
Ev : Type
Ev = Int32 -> Int32

out : ElemRef Div
out = MkRef Div "outdiv"

line : (lbl: String) -> List (Node Ev) -> Node Ev
line lbl ns =
  div [class widgetLine] $ 
      label [class widgetLabel] [Text lbl] :: ns

btn : Ev -> (lbl: String) -> Node Ev
btn ev lbl = button [onClick ev, classes [widget,btn,inc]] [Text lbl]

content : Node Ev
content =
  div [ class widgetList ]
      [ line "Reset counter:"    [ btn (const 0) "Reset" ]
      , line "Increase counter:" [ btn (+ 1)     "+" ]
      , line "Decrease counter:" [ btn (+ (-1))  "-" ]
      , line "Count:"            [ div [id out.id, class output] [Text "0"] ]
      ]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

export
ui : MonadRec m => LiftJSIO m => MonadDom Ev m => m (MSF m Ev ())
ui = do
  applyCSS $ coreCSS ++ css
  innerHtmlAt exampleDiv content
  pure $ accumulateWith apply 0 >>> arr show >>> text out
