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

click : ElemRef e (h :: t) -> MSF m DomEvent (Event ())
click (Ref _ id _) =
  when $ \case Click x => if id == x.id then Just () else Nothing
               _       => Nothing

text : MonadDom m => ElemRef t es -> MSF m String ()
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

line : (lbl: String) -> List Html.Node -> Html.Node
line lbl ns =
  div_ [ class .= widgetLine ] $ 
       label_ [ class .= widgetLabel ] [Text lbl] :: ns

btn : (lbl: String) -> Html.Node
btn lbl = button [Click] [classes .= [widget,btn,inc]] [Text lbl]

content : Html.Node
content =
  div_ [ class .= widgetList ]
       [ line "Reset counter:"    [ btn "Reset" ]
       , line "Increase counter:" [ btn "+" ]
       , line "Decrease counter:" [ btn "-" ]
       , line "Count:"            [ div [] [class .= output] ["0"] ]
       ]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

export
ui : MonadDom m => m (MSF m DomEvent $ Event ())
ui = do
  applyCSS $ coreCSS ++ css

  [reset, plus, minus, out] <- innerHtmlAt contentDiv content
  
  let val = ((1 `on` click plus) <|> (-1 `on` click minus) <|> once 0) ?>>
            accumulateWith (+) 0

  pure $ (val `resetOn` click reset) ?>> show {ty = Int16} ^>> text out
