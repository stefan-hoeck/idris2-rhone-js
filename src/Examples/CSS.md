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
contentDiv : ElemRef HTMLDivElement []
contentDiv = Ref Div "content" []

public export
appStyle : ElemRef HTMLStyleElement []
appStyle = Ref Style "appstyle" []
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
```

### CSS Rules

Note: I'm by no means an expert, so
the CSS rules below might quite well seem horrible
to purists; suggestions of improvements are welcome.

Here are the core rules for laying out the web page.

```idris
public export
coreCSS : List Rule
coreCSS =
  [ elem Body !!
      [ BackgroundColor .= black 
      , Display         .= Flex
      , FlexDirection   .= Column
      , Height          .= perc 100
      , Margin          .= All (Pt 0)
      ]
 
  , id "content" !!
      [ AlignSelf       .= Center
      , BackgroundColor .= "#101010"
      , Display         .= Flex
      , Flex            .= "1"
      , FlexDirection   .= Column
      , JustifyContent  .= FlexStart
      , Padding         .= VH (Pt 40) (Pt 20)
      , MinWidth        .= perc 70
      ]

  , class btn !!
      [ Padding         .= All (Pt 5)
      ]
  ]
```

Finally, we will need a way to apply our CSS rules
upon loading the page. There is `Control.Class.MonadDom`
for effectful interactions with the DOM:

```idris
export
applyCSS : MonadDom m => List Rule -> m ()
applyCSS = rawInnerHtmlAt appStyle . unlines . map render
```
