module Examples.CSS.Balls

import Rhone.JS
import public Examples.CSS.Core
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
css : List Rule
css =
  [ id out.id !!
      [ Flex            .= "1"
      , Margin          .= px 5
      ]

  , id txtCount.id !!
      [ Margin          .= px 5
      , TextAlign       .= End
      , Width           .= perc 20
      ]
  , id btnRun.id !!
      [ Width           .= perc 10
      ]
  ]
