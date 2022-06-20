module Examples.CSS.Balls

import Data.Vect
import Examples.CSS.Colors
import public Examples.CSS.Core
import Rhone.JS
import Text.CSS

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

public export
out : ElemRef HTMLCanvasElement
out = Id Canvas "balls_out"

public export
btnRun : ElemRef HTMLButtonElement
btnRun = Id Button "balls_run"

public export
txtCount : ElemRef HTMLInputElement
txtCount = Id Input "balls_numballs"

public export
log : ElemRef HTMLDivElement
log = Id Div "balls_log"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

export
ballsContent : String
ballsContent = "balls_content"

export
lblCount : String
lblCount = "balls_lblcount"

data Tag = LNum | INum | BRun | LFPS | Anim | Dot

AreaTag Tag where
  showTag LNum = "LNum"
  showTag INum = "INum"
  showTag BRun = "BRun"
  showTag LFPS = "LFPS"
  showTag Anim = "Anim"
  showTag Dot  = "."

export
css : List (Rule 1)
css =
  [ Media "min-width: 300px"
      [ class ballsContent !!
          [ Display             .= Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent]
              [ [LNum, INum]
              , [Dot,  BRun]
              , [LFPS, LFPS]
              , [Anim, Anim]
              ]

          , ColumnGap           .= px 10
          , RowGap              .= px 10
          , Padding             .= VH (px 20) (px 10)
          ]
      ]

  , Media "min-width: 800px"
      [ class ballsContent !!
          [ Display             .= Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent, fr 1]
              [ [LNum, INum, Anim]
              , [Dot,  BRun, Anim]
              , [LFPS, LFPS, Anim]
              , [Dot,  Dot,  Anim]
              ]

          , ColumnGap           .= px 10
          , RowGap              .= px 10
          , Padding             .= VH (px 20) (px 10)
          ]
      ]

  , class lblCount !! [ GridArea .= LNum ]

  , idRef txtCount !!
      [ GridArea        .= INum
      , TextAlign       .= End
      ]

  , idRef btnRun !! [ GridArea .= BRun ]

  , idRef log !! [ GridArea .= LFPS ]

  , idRef out !!
      [ JustifySelf     .= Center
      , GridArea        .= Anim
      , MaxWidth        .= px 500
      , Width           .= px 500
      ]
  ]
