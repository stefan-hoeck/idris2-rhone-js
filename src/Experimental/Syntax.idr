module Experimental.Syntax

import JS
import Control.Category
import Data.MES
import Data.MSF
import Text.Html as Html
import Web.Dom

data RawEv : Type where
  RawClick : MouseEvent -> RawEv

data UIEv : Type where
  Click : (id : String) -> UIEv

click : Ref t -> MES m UIEv ()
click (MkRef _ id) =
  when $ \case Click x => if id == x then Just () else Nothing

text : MonadDom m => Ref t -> MSF m String ()
text (MkRef tpe nodeId) = arrM $ text (IdRef tpe nodeId)

content : Html.Node
content = ul_ [ class .= "content" ]
              [ li_ [] [button [class .= "inc_button"] ["+"]]
              , li_ [] [button [class .= "inc_button"] ["-"]]
              , li_ [] [ label_ [] ["Count: "]
                       , div [class .= "output"] ["Hello Rhone!"]
                       ]
              ]

export
ui : MonadDom dom => dom (MES dom UIEv ())
ui = do
  [btnPlus, btnMinus, txt] <- innerHtmlAt Body content
  pure $   (1 `on` click btnPlus) <|> (-1 `on` click btnMinus)
       ?>> accumulateWith (+) 0
       >>> show {ty = Int32}
       ^>> text txt
