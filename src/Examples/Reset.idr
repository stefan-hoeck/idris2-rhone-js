module Examples.Reset

import JS
import Control.Monad.Dom
import Control.Category
import Data.Event
import Data.MSF
import Data.SOP
import Data.String
import Examples.CSS
import Text.Html as Html
import Text.CSS as CSS
import Web.Dom

innerHtml : LiftJSIO m => ElemRef t -> MSF m (Node [] ev) ()
innerHtml ref = arrM $ rawInnerHtmlAt ref . render

text : LiftJSIO m => ElemRef t -> MSF m String ()
text ref = arr (Text {ev = ()}) >>> innerHtml ref

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

public export
Ev : Type
Ev = Int32 -> Int32

line : (lbl: String) -> NodeList ts Ev -> Node ts Ev
line lbl ns =
  div [class widgetLine] $ 
      label [class widgetLabel] [Text lbl] :: ns

btn : Ev -> (lbl: String) -> Node [] Ev
btn ev lbl = button [onClick ev, classes [widget,btn,inc]] [Text lbl]

content : Node [Div] Ev
content =
  div [ class widgetList ]
      [ line "Reset counter:"    [ btn (const 0) "Reset" ]
      , line "Increase counter:" [ btn (+ 1)     "+" ]
      , line "Decrease counter:" [ btn (+ (-1))  "-" ]
      , line "Count:"            [ div_ [class output] [Text "0"] ]
      ]

--------------------------------------------------------------------------------
--          Many
--------------------------------------------------------------------------------

btn2 : Int32 -> Node [] Ev
btn2 n = btn (+n) #"Inc by \#{show n}"#

content2 : Node [Div] Ev
content2 =
  div [ class widgetList ] $ 
      line "Reset counter:"    [ btn (const 0) "Reset" ] ::
      line "Count:"            [ div_ [class output] [Text "0"] ] ::
      fromList (btn2 <$> [1..5000])

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

export
ui : LiftJSIO m => MonadDom Ev m => m (MSF m Ev ())
ui = do
  applyCSS $ coreCSS ++ css
  [out] <- innerHtmlAt contentDiv content2
  pure $ accumulateWith apply 0 >>> arr show >>> text out
