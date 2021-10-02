module Experimental.Syntax

import JS
import Control.Category
import Control.Monad.Dom
import Data.MES
import Data.MSF
import Data.String
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
--          Element IDs
--------------------------------------------------------------------------------

contentDiv : ElemRef HTMLDivElement []
contentDiv = Ref Div "content" []

compStyle : ElemRef HTMLStyleElement []
compStyle = Ref Style "compstyle" []

--------------------------------------------------------------------------------
--          CSS Classes
--------------------------------------------------------------------------------

nonlist : String
nonlist = "nonlist"

incbtn : String
incbtn = "incbtn"

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
css = [ elem Body     !! [ BackgroundColor .= black 
                         , Display         .= Flex
                         , FlexDirection   .= Column
                         , Height          .= perc 100
                         ]
 

      , id "content"  !! [ AlignSelf       .= Center
                         , BackgroundColor .= "#101010"
                         , Display         .= Flex
                         , Flex            .= "1"
                         , FlexDirection   .= Column
                         , JustifyContent  .= FlexStart
                         , Padding         .= VH (Pt 20) (Pt 0)
                         , MinWidth        .= perc 70
                         ]

      , class nonlist !! [ ListStyleType   .= None
                         , Display         .= Flex
                         , Flex            .= "1"
                         , FlexDirection   .= Column
                         , JustifyContent  .= FlexStart
                         , Margin          .= All (Pt 5)
                         , BackgroundColor .= palegreen
                         ]

      , class lstline !! [ Display         .= Flex
                         , Margin          .= All (Pt 5)
                         ]

      , class lstlbl  !! [ Margin          .= All (Pt 5)
                         , Width           .= perc 20
                         ]

      , class output  !! [ Padding         .= All (Pt 5)
                         , Margin          .= All (Pt 5)
                         , Width           .= perc 10
                         ]

      , class incbtn  !! [ Padding         .= All (Pt 5)
                         , Margin          .= All (Pt 5)
                         , Width           .= perc 10
                         ]
      ]

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

line : (lbl: String) -> List Html.Node -> Html.Node
line lbl ns = div_ [ class .= lstline ] $ 
                   label_ [ class .= lstlbl ] [Text lbl] :: ns

content : Html.Node
content =
  div_ [ class .= nonlist ]
       [ line "Increase counter:" [ button [Click] [class .= incbtn] ["+"] ]
       , line "Decrease counter:" [ button [Click] [class .= incbtn] ["-"] ]
       , line "Count:"            [ div [] [class .= output] ["0"] ]
       ]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

export
ui : MonadDom dom => dom (MES dom DomEvent ())
ui = do
  ignore $ innerHtmlAt compStyle (Raw . unlines $ map render css)

  [plus, minus, out] <- innerHtmlAt contentDiv content

  pure $   (1 `on` click plus) <|> (-1 `on` click minus)
       ?>> accumulateWith (+) 0
       >>> show {ty = Int32}
       ^>> text out
