module Examples.CSS.Fractals

import Data.Vect
import Examples.CSS.Colors
import public Examples.CSS.Core
import Rhone.JS
import Text.CSS

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

public export
out : ElemRef HTMLDivElement
out = Id Div "fractals_out"

public export
btnRun : ElemRef HTMLButtonElement
btnRun = Id Button "fractals_run"

public export
txtIter : ElemRef HTMLInputElement
txtIter = Id Input "fractals_iterations"

public export
txtRedraw : ElemRef HTMLInputElement
txtRedraw = Id Input "fractals_redrawdelay"

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

  , idRef txtIter !!
      [ GridArea        .= IIter
      , TextAlign       .= End
      ]

  , class lblDelay !! [ GridArea .= LDel ]

  , idRef txtRedraw !!
      [ GridArea        .= IDel
      , TextAlign       .= End
      ]

  , idRef btnRun !! [ GridArea .= BRun ]

  , idRef out !!
      [ JustifySelf     .= Center
      , GridArea        .= Fract
      , BorderStyle     .= Left Solid
      , BorderWidth     .= Left (px 2)
      , BorderColor     .= Left base80
      , MaxWidth        .= px 500
      , Width           .= px 500
      ]
  ]
