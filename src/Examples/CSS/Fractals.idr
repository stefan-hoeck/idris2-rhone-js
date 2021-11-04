module Examples.CSS.Fractals

import Data.Vect
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

data Tag = LIter | IIter | LDel | IDel | BRun | Fract | Dot

AreaTag Tag where
  showTag LIter = "LIter"
  showTag IIter = "IIter"
  showTag LDel  = "LDel"
  showTag IDel  = "IDel"
  showTag BRun  = "BRun"
  showTag Fract = "Fract"
  showTag Dot   = "."

export
css : List (Rule 1)
css =
  [ Media "min-width: 300px"
      [ class fractalContent !!
          [ Display             .= Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent]
              [ [LIter, IIter]
              , [LDel,  IDel ]
              , [Dot,   BRun ]
              , [Fract, Fract]
              ]

          , ColumnGap           .= px 10
          , RowGap              .= px 10
          , Padding             .= VH (px 20) (px 10)
          ]
      ]

  , Media "min-width: 800px"
      [ class fractalContent !!
          [ Display             .= Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent, fr 1]
              [ [LIter, IIter, Fract]
              , [LDel,  IDel,  Fract]
              , [Dot,   BRun,  Fract]
              , [Dot,   Dot,   Fract]
              ]

          , ColumnGap           .= px 10
          , RowGap              .= px 10
          , Padding             .= VH (px 20) (px 10)
          ]
      ]
  , class lblIter !! [ GridArea .= LIter ]

  , id txtIter.id !!
      [ GridArea        .= IIter
      , TextAlign       .= End
      ]

  , class lblDelay !! [ GridArea .= LDel ]

  , id txtRedraw.id !!
      [ GridArea        .= IDel
      , TextAlign       .= End
      ]

  , id btnRun.id !! [ GridArea .= BRun ]

  , id out.id !!
      [ JustifySelf     .= Center
      , GridArea        .= Fract
      , BorderStyle     .= Left Solid
      , BorderWidth     .= Left (px 2)
      , BorderColor     .= Left base80
      , MaxWidth        .= px 500
      , Width           .= px 500
      ]
  ]
