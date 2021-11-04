module Examples.CSS.Fractals

import Data.List
import Examples.CSS.Colors
import public Examples.CSS.Core
import Rhone.JS
import Text.CSS

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

export
out : ElemRef HTMLDivElement
out = MkRef Div "fractals_out"

export
btnRun : ElemRef HTMLButtonElement
btnRun = MkRef Button "fractals_run"

export
txtIter : ElemRef HTMLInputElement
txtIter = MkRef Input "fractals_iterations"

export
txtRedraw : ElemRef HTMLInputElement
txtRedraw = MkRef Input "fractals_redrawdelay"

--------------------------------------------------------------------------------
--          Classes
--------------------------------------------------------------------------------

export
fractalContent : String
fractalContent = "fractals_content"

export
lblIter : String
lblIter = "fractals_lbliter"

export
lblDelay : String
lblDelay = "fractals_lbldelay"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

export
css : List (Rule 1)
css =
  [ class fractalContent !!
      [ Display             .= Grid
      , ColumnGap           .= px 10
      , RowGap              .= px 10
      , GridTemplateColumns .= [px 170, fr 1, fr 3]
      , GridTemplateRows    .= replicate 3 MinContent ++ [fr 1]
      , Padding             .= VH (px 20) (px 10)
      ]

  , class lblIter !!
      [ GridColumn      .= At 1
      , GridRow         .= At 1
      ]

  , id txtIter.id !!
      [ GridColumn      .= At 2
      , GridRow         .= At 1
      , TextAlign       .= End
      ]

  , class lblDelay !!
      [ GridColumn      .= At 1
      , GridRow         .= At 2
      ]

  , id txtRedraw.id !!
      [ GridColumn      .= At 2
      , GridRow         .= At 2
      , TextAlign       .= End
      ]

  , id btnRun.id !!
      [ GridColumn      .= At 1
      , GridRow         .= At 3
      ]

  , id out.id !!
      [ JustifySelf     .= Center
      , GridColumn      .= At 3
      , GridRow         .= FromTo 1 5
      , BorderStyle     .= Left Solid
      , BorderWidth     .= Left (px 2)
      , BorderColor     .= Left base80
      , MaxWidth        .= px 500
      , Width           .= px 500
      ]
  ]
