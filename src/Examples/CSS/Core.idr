module Examples.CSS.Core

import Examples.CSS.Colors
import Rhone.JS
import Text.CSS

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

||| ID of the `<body>` element. The page content will
||| be placed here.
public export
contentDiv : ElemRef HTMLBodyElement
contentDiv = MkRef Body "content"

||| The page consists of a static heading with a title an
||| (eventually) a short description of the project.
||| This is followed by a selection box, where visitors can
||| choose an example application.
|||
||| The example application will be dynamicall generated and
||| placed in to a `<div>` with ID `"example"`.
public export
exampleDiv : ElemRef HTMLDivElement
exampleDiv = MkRef Div "example"

||| ID of a `<style>` element in the page header.
||| The generated CSS rules will go here.
public export
appStyle : ElemRef HTMLStyleElement
appStyle = MkRef Style "appstyle"

--------------------------------------------------------------------------------
--          Classes
--------------------------------------------------------------------------------

||| a clickable button
public export
btn : String
btn = "btn"

||| a text input
public export
textIn : String
textIn = "textin"

||| a select input
public export
selectIn : String
selectIn = "selectin"

||| an input widget
public export
widget : String
widget = "widget"

||| a list of input widgets,
||| each on its own line with a label
||| on the left.
public export
widgetList : String
widgetList = "widgetList"

||| the main content, split into three rows:
||| a title, the example selector, and the
||| currently loaded example application
public export
contentList : String
contentList = "contentList"

||| the header row where the example selector
||| resides
public export
contentHeader : String
contentHeader = "contentHeader"

||| the row with the page title
public export
pageTitle : String
pageTitle = "pageTitle"

||| the select box used to choose an example
||| application
public export
exampleSelector : String
exampleSelector = "example_selector"

||| a single line in a column
||| of input widgets.
public export
widgetLine : String
widgetLine = "widgetline"

||| a label on the left of an input
||| widget.
public export
widgetLabel : String
widgetLabel = "widgetlabel"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

public export
coreCSS : List Rule
coreCSS =
  [ elem Html !!
      [ Height          .= perc 100]

  , elem Body !!
      [ BackgroundColor .= black 
      , Color           .= base100
      , Display         .= Flex
      , FlexDirection   .= Column
      , FontFamily      .= "Helvetica, Arial, sans-serif"
      , Height          .= perc 100
      , Margin          .= px 0
      ]
 
  , class contentList !!
      [ AlignSelf       .= Center
      , BackgroundColor .= darker_grey
      , Display         .= Flex
      , Flex            .= "1"
      , FlexDirection   .= Column
      , JustifyContent  .= FlexStart
      , Padding         .= VH (Pt 40) (Pt 20)
      , MaxWidth        .= perc 70
      , MinWidth        .= perc 70
      ]
 
  , class pageTitle !!
      [ BorderStyle     .= Bottom Solid
      , BorderWidth     .= Bottom (px 5)
      , BorderColor     .= Bottom base80
      , FontSize        .= XLarge
      , Margin          .= Bottom (px 40)
      , Padding         .= Bottom (px 60)
      , TextAlign       .= Center
      ]
 
  , class contentHeader !!
      [ Display         .= Flex
      , BorderStyle     .= Bottom Solid
      , BorderWidth     .= Bottom (px 2)
      , BorderColor     .= Bottom base80
      , Margin          .= Bottom (px 40)
      , Padding         .= Bottom (px 40)
      ]

  , class widget !! 
      [ BackgroundColor .= lighter_grey
      , BorderRadius    .= px 10
      , BorderStyle     .= All Solid
      , BorderWidth     .= px 3
      , BorderColor     .= All comp100
      , Color           .= darker_grey
      , FontSize        .= Large
      , Padding         .= px 5
      , Margin          .= px 5
      ]

  , Pseudo (class widget) Hover !!
      [ BackgroundColor .= lightest_grey
      , BorderColor     .= All comp60
      ]

  , Pseudo (class widget) Active !!
      [ BackgroundColor .= lightest_grey
      , BorderColor     .= All comp60
      ]

  , Pseudo (class widget) Disabled !!
      [ BackgroundColor .= light_grey
      , BorderColor     .= All dark_grey
      ]

  , class textIn  !!
      [ TextAlign       .= End
      ]

  , class selectIn  !!
      [ TextAlign       .= End
      ]

  , class exampleSelector  !!
      [ FontSize        .= Large
      ]

  , Pseudo (class widget) Invalid !!
      [ BorderColor     .= All red ]

  , class widgetList !!
      [ ListStyleType   .= None
      , Display         .= Flex
      , Flex            .= "1"
      , FlexDirection   .= Column
      , JustifyContent  .= FlexStart
      ]

  , class widgetLine !!
      [ AlignItems      .= FlexStart
      , Display         .= Flex
      , Margin          .= Bottom (px 5)
      ]

  , class widgetLabel !!
      [ FontSize        .= Large
      , Width           .= perc 20
      ]
  ]
