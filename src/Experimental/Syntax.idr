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

incButton : String
incButton = "incbutton"

outputLine : String
outputLine = "outputline"

output : String
output = "output"

--------------------------------------------------------------------------------
--          CSS
--------------------------------------------------------------------------------

css : List Rule
css = [ elem Body     !! [ BackgroundColor .= black 
                         , Display         .= Flex
                         , FlexDirection   .= Column
                         ]
 

      , id "content"  !! [ AlignSelf       .= Center
                         , BackgroundColor .= "#101010"
                         , Display         .= Flex
                         , FlexDirection   .= Column
                         , MinHeight       .= perc 100
                         , JustifyContent  .= FlexStart
                         , Padding         .= VH (Pt 20) (Pt 0)
                         , MinWidth        .= perc 70
                         ]

      , class nonlist   !! [ ListStyleType .= None
                           , Display         .= Flex
                           , FlexDirection   .= Column
                           , JustifyContent  .= FlexStart
                           , Margin  .= All (Pt 5)
                           , BackgroundColor .= palegreen
                           ]

      , class outputLine !! [ Display .= Flex
                            , Margin  .= All (Pt 5)
                            , BackgroundColor .= palegreen
                            ]

      , class output     !! [ Display         .= Flex
                            , Margin          .= Left (Pt 20)
                            , BackgroundColor .= palegreen
                            ]

      , class incButton !! [ Padding .= All (Pt 5)
                           , Margin  .= All (Pt 5)
                           , Width   .= perc 10
                           ]
      ]

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

content : Html.Node
content = ul_ [ class .= nonlist ]
              [ li_ [] [ button [Click] [class .= incButton] ["+"]]
              , li_ [] [ button [Click] [class .= incButton] ["-"]]
              , li_ [ class .= outputLine ]
                    [ label_ [] ["Count:"], div [] [class .= output] ["0"] ]
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
