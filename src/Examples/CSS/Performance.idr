module Examples.CSS.Performance

import Data.Vect
import Examples.CSS.Colors
import public Examples.CSS.Core
import Rhone.JS
import Text.CSS

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

-- displays the current sum of clicks
export
out : ElemRef Div
out = MkRef Div "performance_sum"

-- text fields where users can enter the number of buttons
export
natIn : ElemRef Input
natIn = MkRef Input "performance_numbuttons"

export
btnRun : ElemRef HTMLButtonElement
btnRun = MkRef Button "performance_run"

-- where the created buttons go
export
buttons : ElemRef Div
buttons = MkRef Div "performance_buttons"

-- displays the time take to create the buttons
export
time : ElemRef Div
time = MkRef Div "performance_time"

--------------------------------------------------------------------------------
--          Classes
--------------------------------------------------------------------------------

export
inc : String
inc = "performance_inc"

export
numButtonsLbl : String
numButtonsLbl = "performance_numbuttonslbl"

export
sumLbl : String
sumLbl = "performance_sumlbl"

export
grid : String
grid = "performance_grid"

export
performanceContent : String
performanceContent = "performance_content"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

data Tag = LBtn | NBtn | BRun | LSum | OSum | Btns | OTme | Dot

AreaTag Tag where
  showTag LBtn  = "LBtn"
  showTag NBtn  = "NBtn"
  showTag BRun  = "BRun"
  showTag LSum  = "LSum"
  showTag Btns  = "Btns"
  showTag OSum  = "OSum"
  showTag OTme  = "OTme"
  showTag Dot   = "."

export
css : List (Rule 1)
css =
  [ Media "min-width: 300px"
      [ class performanceContent !!
          [ Display             .= Area
              (replicate 4 MinContent)
              [MaxContent, fr 1, MaxContent]
              [ [LBtn, NBtn, BRun]
              , [LSum, OSum, OSum]
              , [OTme, OTme, OTme]
              , [Btns, Btns, Btns]
              ]
          , ColumnGap           .= px 10
          , RowGap              .= px 10
          , Padding             .= VH (px 20) (px 10)
          ]
      ]

  , Media "min-width: 800px"
      [ class performanceContent !!
          [ Display             .= Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent, MaxContent, fr 1]
              [ [LBtn, NBtn, BRun, Btns]
              , [LSum, OSum, OSum, Btns]
              , [OTme, OTme, OTme, Btns]
              , [Dot,  Dot,  Dot,  Btns]
              ]
          , ColumnGap           .= px 10
          , RowGap              .= px 10
          , Padding             .= VH (px 20) (px 10)
          ]
      ]

  , class numButtonsLbl !! [ GridArea .= LBtn ]

  , id natIn.id !!
      [ GridArea        .= NBtn
      , TextAlign       .= End
      ]

  , id btnRun.id !! [ GridArea .= BRun ]

  , class sumLbl !! [ GridArea .= LSum ]

  , id out.id  !!
      [ GridArea        .= OSum
      , FontSize        .= Large
      ]

  , id time.id  !! [ GridArea .= OTme ]

  , id buttons.id  !!
      [ GridArea        .= Btns
      , BorderStyle     .= Left Solid
      , BorderWidth     .= Left (px 2)
      , BorderColor     .= Left base80
      , Padding         .= Left (px 10)
      ]

  , class grid  !!
      [ Display         .= Flex
      , FlexWrap        .= "wrap"
      ]

  , class inc !!
      [ FlexBasis       .= perc 5
      , FontSize        .= XXSmall
      ]
  ]
