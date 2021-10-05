module Examples.Reset

import JS
import Control.Monad.Dom
import Data.Event
import Data.MSF
import Data.String
import Examples.CSS
import Text.Html as Html
import Text.CSS as CSS
import Web.Dom

text : MonadDom e m => ElemRef t -> MSF m String ()
text ref = arrM $ text ref

--------------------------------------------------------------------------------
--          CSS Classes
--------------------------------------------------------------------------------

inc : String
inc = "inc"

output : String
output = "output"

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

  , class inc  !!
      [ Margin          .= pt 5
      , Width           .= perc 10
      ]
  ]

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

Ev : Type
Ev = Int32 -> Int32

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
      , line "Count:"            [ div [class output] ["0"] ]
      ]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

export
ui : MonadDom Ev m => m (MSF m Ev ())
ui = do
  applyCSS $ coreCSS ++ css
  innerHtml contentDiv content
  pure ?foo

--   
--   let val = ((1 `on` click plus) <|> (-1 `on` click minus) <|> once 0) ?>>
--             accumulateWith (+) 0
-- 
--   pure $ (val `resetOn` click reset) ?>> show {ty = Int16} ^>> text out
