module Examples.CSS.Reset

import Examples.CSS.Colors
import public Examples.CSS.Core
import Rhone.JS
import Text.CSS

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

export
out : ElemRef Div
out = MkRef Div "reset_out"

export
btnInc : ElemRef Button
btnInc = MkRef Button "reset_inc"

export
btnDec : ElemRef Button
btnDec = MkRef Button "reset_dec"

export
btnReset : ElemRef Button
btnReset = MkRef Button "reset_reset"

--------------------------------------------------------------------------------
--          CSS
--------------------------------------------------------------------------------

export
resetBtn : String
resetBtn = "reset_incbtn"

export
css : List Rule
css =
  [ id out.id  !!
      [ FontSize        .= Large
      , Margin          .= pt 5
      , TextAlign       .= End
      , Width           .= perc 10
      ]

  , class resetBtn  !!
      [ Margin          .= pt 5
      , Width           .= perc 10
      ]
  ]
