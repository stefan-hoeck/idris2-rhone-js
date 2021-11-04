module Examples.CSS.Balls

import Data.Vect
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

  , id txtCount.id !!
      [ GridArea        .= INum
      , TextAlign       .= End
      ]

  , id btnRun.id !! [ GridArea .= BRun ]

  , id log.id !! [ GridArea .= LFPS ]

  , id out.id !!
      [ JustifySelf     .= Center
      , GridArea        .= Anim
      , MaxWidth        .= px 500
      , Width           .= px 500
      ]
  ]
