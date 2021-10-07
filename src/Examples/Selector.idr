module Examples.Selector

import JS
import Control.MonadRec
import Control.Monad.Dom
import Control.Category
import Data.MSF
import Data.String
import Examples.CSS
import Examples.Performance
import Examples.Reset
import Text.Html as Html
import Text.CSS as CSS
import Web.Dom

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

content : Node String
content =
  div [ class contentList ]
      [ div [class pageTitle] ["rhone-js: Examples"]
      , div [class contentHeader]
          [ label [class widgetLabel] ["Choose an Example"]
          , select
              [ classes [widget, exampleSelector], onChange id]
              [ option [ value "reset", selected True ] ["Counting Clicks"]
              , option [ value "performance" ] ["Performance"]
              ]
          ]
      , div [id exampleDiv.id, class widgetList] []
      ]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

export
ui : MonadRec m => LiftJSIO m => MonadDom String m => m (MSF m String ())
ui = do
  innerHtmlAt contentDiv content
  pure . arrM $
    \case "reset"       => liftJSIO (reactimateDom "ex" Reset.ui)
          "performance" => liftJSIO (reactimateDom "ex" Performance.ui)
          _             => pure ()
