module Examples.CSS.Fractals

import Rhone.JS
import public Examples.CSS.Core
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
--          Rules
--------------------------------------------------------------------------------

export
css : List Rule
css =
  [ id txtIter.id !!
      [ Margin          .= px 5
      , TextAlign       .= End
      , Width           .= perc 20
      ]

  , id txtRedraw.id !!
      [ Margin          .= px 5
      , TextAlign       .= End
      , Width           .= perc 20
      ]

  , id btnRun.id !!
      [ Width           .= perc 10
      ]

  , id out.id !!
      [ Flex            .= "1"
      , Margin          .= px 5
      ]
  ]
