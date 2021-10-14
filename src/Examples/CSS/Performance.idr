module Examples.CSS.Performance

import Rhone.JS
import public Examples.CSS.Core
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
buttonLine : String
buttonLine = "performance_buttonline"

export
numButtons : String
numButtons = "performance_numbuttons"

export
grid : String
grid = "performance_grid"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

export
css : List Rule
css =
  [ id out.id  !!
      [ FontSize        .= Large
      , Margin          .= px 5
      , TextAlign       .= End
      , Width           .= perc 20
      ]

  , id btnRun.id !!
      [ Width           .= perc 10
      ]

  , class grid  !!
      [ Display         .= Flex
      , FlexWrap        .= "wrap"
      ]

  , class inc !!
      [ FlexBasis       .= perc 5
      , FontSize        .= XXSmall
      ]

  , class numButtons !!
      [ Margin          .= px 5
      , Padding         .= px 5
      , TextAlign       .= End
      , Width           .= perc 20
      ]
  ]
