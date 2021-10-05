## CSS

The plan of *rhone-js* is to eventually come
'batteries included' and this means having a way
to programmatically declare (and change) the appearance
of a web page. The `Text.CSS` submodules therefore come
with a (so far incomplete) set of data types for
declaring CSS rules in a type-safe manner.

```idris
module Examples.CSS

import Control.Monad.Dom
import Data.Maybe
import Data.String
import JS
import Text.CSS
import Text.Html as Html
import Web.Dom
```

### IDs and Classes

The `rhone.html` document at the project root defines two
entry points for out single-page web page: A `style` element
in the header, where our CSS rules go, and a `div` in the body,
where the content of our web page goes. We typically refer
to HTML elements via `ElemRef` values
(defined in `Control.Monad.Dom.Interface`), which come with
an ID and a tag to allow us to safely request the properly
typed element from the DOM:

```idris
public export
contentDiv : ElemRef HTMLDivElement
contentDiv = MkRef Div "content"

public export
appStyle : ElemRef HTMLStyleElement
appStyle = MkRef Style "appstyle"
```

Note: The empty lists in the declarations above refer to
the events these elements will be able to throws. They
are used in all the interactive examples in this section.

I like to keep my CSS simple and use classes and pseudo
classes whenever possible, so here are the ones that
come up in most examples:

```idris
-- a clickable button
public export
btn : String
btn = "btn"

-- an input widget
public export
widget : String
widget = "widget"

-- a column of input widgets,
-- each on its own line with a label
-- on the left.
public export
widgetList : String
widgetList = "widgetList"

-- a single line in a column
-- of input widgets.
public export
widgetLine : String
widgetLine = "widgetline"

-- a label on the left of an input
-- widget.
public export
widgetLabel : String
widgetLabel = "widgetlabel"
```

### CSS Rules

Note: I'm by no means an expert, so
the CSS rules below might quite well seem horrible
to purists; suggestions of improvements are welcome.

Here are the core rules for laying out the web page.

```idris
export
lightest_grey : Color
lightest_grey = "#adadad"

export
lighter_grey : Color
lighter_grey = "#6d6d6d"

export
light_grey : Color
light_grey = "#4d4d4d"

export
dark_grey : Color
dark_grey = "#1d1d1d"

export
darker_grey : Color
darker_grey = "#0d0d0d"

export
base100 : Color
base100 = "#e57200"

export
base80 : Color
base80 = "#e68a2e"

export
base60 : Color
base60 = "#e6a15c"

export
base40 : Color
base40 = "#e6b88a"

export
base20 : Color
base20 = "#e6cfb8"

export
base0 : Color
base0 = "#e6e6e6"

export
comp100 : Color
comp100 = "#0073e5"

export
comp80 : Color
comp80 = "#2e8ae6"

export
comp60 : Color
comp60 = "#5ca1e6"

export
comp40 : Color
comp40 = "#8ab8e6"

export
comp20 : Color
comp20 = "#b8cfe6"

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
      , Margin          .= pt 0
      ]
 
  , id "content" !!
      [ AlignSelf       .= Center
      , BackgroundColor .= darker_grey
      , Display         .= Flex
      , Flex            .= "1"
      , FlexDirection   .= Column
      , JustifyContent  .= FlexStart
      , Padding         .= VH (Pt 40) (Pt 20)
      , MinWidth        .= perc 70
      ]

  , class btn !! 
      [ Padding         .= pt 5 ]

  , class widget !! 
      [ BackgroundColor .= lighter_grey
      , BorderRadius    .= px 10
      , BorderStyle     .= All Solid
      , BorderWidth     .= px 3
      , BorderColor     .= All comp100
      , Color           .= darker_grey
      , FontSize        .= Large
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

  , Pseudo (class widget) Invalid !!
      [ BorderColor     .= All red ]

  , class widgetList !!
      [ ListStyleType   .= None
      , Display         .= Flex
      , Flex            .= "1"
      , FlexDirection   .= Column
      , JustifyContent  .= FlexStart
      , Margin          .= pt 5
      ]

  , class widgetLine !!
      [ AlignItems      .= FlexStart
      , Display         .= Flex
      , Margin          .= pt 5
      ]

  , class widgetLabel !!
      [ FontSize        .= Large
      , Margin          .= pt 5
      , Width           .= perc 20
      ]
  ]
```

Finally, we will need a way to apply our CSS rules
upon loading the page. There is `Control.Class.MonadDom`
for effectful interactions with the DOM:

```idris
export
applyCSS : MonadDom e m => List Rule -> m ()
applyCSS = rawInnerHtmlAt appStyle . unlines . map render
```

The code above sends the rendered CSS rules to the
html content of the style element with id `appstyle` in
the document header.
