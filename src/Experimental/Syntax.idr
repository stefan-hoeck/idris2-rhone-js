module Experimental.Syntax

import JS
import Control.Category
import Control.Monad.Dom
import Data.MES
import Data.MSF
import Text.Html as Html
import Web.Dom

click : ElemRef e (h :: t) -> MES m DomEvent ()
click (Ref _ id _) =
  when $ \case Click x => if id == x.id then Just () else Nothing
               _       => Nothing

text : MonadDom m => ElemRef t es -> MSF m String ()
text ref = arrM $ text ref

content : Html.Node
content = ul_ [ class .= "content" ]
              [ li_ [] [ button [Click] [class .= "inc_button"] ["+"]]
              , li_ [] [ button [Click] [class .= "inc_button"] ["-"]]
              , li_ [] [ label_ [] ["Count: "]
                       , div [] [class .= "output"] ["Hello Rhone!"]
                       ]
              ]

export
ui : MonadDom dom => dom (MES dom DomEvent ())
ui = do
  [btnPlus, btnMinus, txt] <- innerHtmlAt Body content
  pure $   (1 `on` click btnPlus) <|> (-1 `on` click btnMinus)
       ?>> accumulateWith (+) 0
       >>> show {ty = Int32}
       ^>> text txt
