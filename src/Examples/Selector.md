module Examples.Selector

import Examples.CSS
import Examples.Performance
import Examples.Reset
import Rhone.JS

public export
MSel : Type -> Type
MSel = DomIO String JSIO

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
ui : MSel (MSF MSel String ())
ui = do
  innerHtmlAt contentDiv content
  pure . arrM $
    \case "reset"       => liftJSIO (reactimateDom "ex" Reset.ui)
          "performance" => liftJSIO (reactimateDom "ex" Performance.ui)
          _             => pure ()
