module Examples.CSS.Balls

import Data.List
import Examples.CSS.Colors
import public Examples.CSS.Core
import Rhone.JS
import Text.CSS

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

export
out : ElemRef HTMLCanvasElement
out = MkRef Canvas "balls_out"

export
btnRun : ElemRef HTMLButtonElement
btnRun = MkRef Button "balls_run"

export
txtCount : ElemRef HTMLInputElement
txtCount = MkRef Input "balls_numballs"

export
log : ElemRef HTMLDivElement
log = MkRef Div "balls_log"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

export
ballsContent : String
ballsContent = "balls_content"

export
lblCount : String
lblCount = "balls_lblcount"

export
css : List (Rule 1)
css =
  [ class ballsContent !!
      [ Display             .= Grid
      , ColumnGap           .= px 10
      , RowGap              .= px 10
      , GridTemplateColumns .= [px 170, fr 1, fr 3]
      , GridTemplateRows    .= replicate 3 MinContent ++ [fr 1]
      , Padding             .= VH (px 20) (px 10)
      ]

  , class lblCount !!
      [ GridColumn      .= At 1
      , GridRow         .= At 1
      ]

  , id txtCount.id !!
      [ GridColumn      .= At 2
      , GridRow         .= At 1
      , TextAlign       .= End
      ]

  , id btnRun.id !!
      [ GridColumn      .= At 1
      , GridRow         .= At 2
      ]

  , id log.id !!
      [ GridColumn      .= At 1
      , GridRow         .= At 3
      ]

  , id out.id !!
      [ JustifySelf     .= Center
      , GridColumn      .= At 3
      , GridRow         .= FromTo 1 5
      , MaxWidth        .= px 500
      , Width           .= px 500
      ]
  ]
