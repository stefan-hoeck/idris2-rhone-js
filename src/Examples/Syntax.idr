module Examples.Syntax

import JS
import Control.Category
import Control.Monad.Dom
import Data.MES
import Data.MSF
import Data.String
import Examples.CSS
import Text.Html as Html
import Text.CSS as CSS
import Web.Dom

click : ElemRef e (h :: t) -> MES m DomEvent ()
click (Ref _ id _) =
  when $ \case Click x => if id == x.id then Just () else Nothing
               _       => Nothing

text : MonadDom m => ElemRef t es -> MSF m String ()
text ref = arrM $ text ref

--------------------------------------------------------------------------------
--          CSS Classes
--------------------------------------------------------------------------------

nonlist : String
nonlist = "nonlist"

inc : String
inc = "inc"

lstline : String
lstline = "lstline"

lstlbl : String
lstlbl = "lstlbl"

output : String
output = "output"

--------------------------------------------------------------------------------
--          CSS
--------------------------------------------------------------------------------

css : List Rule
css =
  [ class nonlist !!
      [ ListStyleType   .= None
      , Display         .= Flex
      , Flex            .= "1"
      , FlexDirection   .= Column
      , JustifyContent  .= FlexStart
      , Margin          .= All (Pt 5)
      , BackgroundColor .= palegreen
      ]

  , class lstline !!
      [ Display         .= Flex
      , Margin          .= All (Pt 5)
      ]

  , class lstlbl  !!
      [ Margin          .= All (Pt 5)
      , Width           .= perc 20
      ]

  , class output  !!
      [ Padding         .= All (Pt 5)
      , Margin          .= All (Pt 5)
      , Width           .= perc 10
      ]

  , classes [btn,inc]  !!
      [ Margin          .= All (Pt 5)
      , Width           .= perc 10
      ]
  ]

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

line : (lbl: String) -> List Html.Node -> Html.Node
line lbl ns = div_ [ class .= lstline ] $ 
                   label_ [ class .= lstlbl ] [Text lbl] :: ns

incbtn : (lbl: String) -> Html.Node
incbtn lbl = button [Click] [classes .= [btn,inc]] [Text lbl]

content : Html.Node
content =
  div_ [ class .= nonlist ]
       [ line "Increase counter:" [ incbtn "+" ]
       , line "Decrease counter:" [ incbtn "-" ]
       , line "Count:"            [ div [] [class .= output] ["0"] ]
       ]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

export
ui : MonadDom m => m (MES m DomEvent ())
ui = do
  applyCSS $ coreCSS ++ css

  [plus, minus, out] <- innerHtmlAt contentDiv content

  pure $   (1 `on` click plus) <|> (-1 `on` click minus)
       ?>> accumulateWith (+) 0
       >>> show {ty = Int32}
       ^>> text out
