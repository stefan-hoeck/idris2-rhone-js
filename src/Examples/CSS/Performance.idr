module Examples.CSS.Performance

import Data.List
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

export
css : List Rule
css =
  [ class performanceContent !!
      [ Display             .= Grid
      , ColumnGap           .= px 10
      , RowGap              .= px 10
      , GridTemplateColumns .= [px 170, fr 1, px 70, fr 3]
      , GridTemplateRows    .= replicate 3 MinContent ++ [fr 1]
      , Padding             .= VH (px 20) (px 10)
      ]

  , class numButtonsLbl !!
      [ GridColumn      .= At 1
      , GridRow         .= At 1
      ]

  , id natIn.id !!
      [ GridColumn      .= At 2
      , GridRow         .= At 1
      , TextAlign       .= End
      ]

  , id btnRun.id !!
      [ GridColumn      .= At 3
      , GridRow         .= At 1
      ]

  , class sumLbl !!
      [ GridColumn      .= At 1
      , GridRow         .= At 2
      ]
      
  , id out.id  !!
      [ GridColumn      .= At 3
      , GridRow         .= At 2
      , FontSize        .= Large
      , TextAlign       .= End
      ]
      
  , id time.id  !!
      [ GridColumn      .= FromTo 1 4
      , GridRow         .= At 3
      ]
      
  , id buttons.id  !!
      [ GridColumn      .= At 4
      , GridRow         .= FromTo 1 5
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
