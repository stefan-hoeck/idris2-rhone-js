module Examples.Performance

import JS
import Control.MonadRec
import Control.Monad.Dom
import Control.Category
import Data.Event
import Data.List.TR
import Data.MSF
import Data.SOP
import Data.String
import Generics.Derive
import Examples.CSS
import Text.Html as Html
import Text.CSS as CSS
import Web.Dom

%language ElabReflection

innerHtml : LiftJSIO m => ElemRef t -> MSF m (Node ev) ()
innerHtml ref = arrM $ rawInnerHtmlAt ref . render

text : LiftJSIO m => ElemRef t -> MSF m String ()
text ref = arr Text >>> innerHtml {ev = ()} ref

--------------------------------------------------------------------------------
--          CSS Classes
--------------------------------------------------------------------------------

incSmall : String
incSmall = "incSmall"

output : String
output = "output"

buttonLine : String
buttonLine = "buttonline"

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

  , class incSmall !!
      [ Margin          .= pt 1
      , FontSize        .= XXSmall
      , Width           .= perc 3
      ]
  ]

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

data Ev =
    Inc      Bits32
  | Validate String
  | Reload

%runElab derive "Ev" [Generic,Meta,Show,Eq]

validate : Ev -> Maybe Nat
validate (Validate s) = Just $ cast s
validate _            = Nothing

out : ElemRef Div
out = MkRef Div "outdiv"

buttons : ElemRef Div
buttons = MkRef Div "buttons"

time : ElemRef Div
time = MkRef Div "time"

line : (lbl: String) -> List (Node Ev) -> Node Ev
line lbl ns =
  div [class widgetLine] $ 
      label [class widgetLabel] [Text lbl] :: ns

btn : Bits32 -> Node Ev
btn n = button [onClick (Inc n), classes [widget,btn,incSmall]] [Text $ show n]

btns : Nat -> Node Ev
btns n = div [class grid] . mapTR btn $ iterateTR n (+1) 1

content : Node Ev
content =
  div [ class widgetList ]
      [ line "Number of buttons:"
          [ input [ onInput Validate
                  , onEnterDown Reload
                  , class widget
                  , placeholder "Enter a positive integer"
                  ] []
          , div [id time.id] []
          ]
      , line "Sum:" [ div [id out.id, class output] [Text "0"] ]
      , div [id buttons.id, class buttonLine] []
      ]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

%foreign "javascript:lambda:() => new Date().getTime()"
prim__time : PrimIO Int32

btnsSF : MonadRec m => LiftJSIO m => MonadDom Ev m => Nat -> m (MSF m Ev ())
btnsSF n = do
  t1 <- primIO prim__time
  innerHtmlAt buttons (btns n)
  t2 <- primIO prim__time
  rawInnerHtmlAt time (#"\#{show $ t2 - t1} ms"#)
  pure $ accumulateWith add 0 >>> arr show >>> text out

  where add : Ev -> Bits32 -> Bits32
        add (Inc x)      y = x + y
        add (Validate _) y = y
        add Reload       _ = 0

export
ui : MonadRec m => LiftJSIO m => MonadDom Ev m => m (MSF m Ev ())
ui = do
  applyCSS $ coreCSS ++ css
  innerHtmlAt contentDiv content
  pure $ switchOnM btnsSF ((when validate >>> stepper 0) `on` is Reload) neutral
