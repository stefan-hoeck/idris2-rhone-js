||| CSS Rules for the Inc. Buttons Example
module Examples.CSS.Reset

import Data.List
import Examples.CSS.Colors
import public Examples.CSS.Core
import Rhone.JS
import Text.CSS

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

||| Where the accumulated count is printed to
export
out : ElemRef Div
out = MkRef Div "reset_out"

||| ID of the increasing button
export
btnInc : ElemRef Button
btnInc = MkRef Button "reset_inc"

||| ID of the decreasing button
export
btnDec : ElemRef Button
btnDec = MkRef Button "reset_dec"

||| ID of the reset button
export
btnReset : ElemRef Button
btnReset = MkRef Button "reset_reset"

--------------------------------------------------------------------------------
--          CSS
--------------------------------------------------------------------------------

export
resetLbl : String
resetLbl = "reset_resetlbl"

export
incLbl : String
incLbl = "reset_inclbl"

export
decLbl : String
decLbl = "reset_declbl"

export
countLbl : String
countLbl = "reset_countlbl"

export
resetContent : String
resetContent = "reset_content"

export
resetBtn : String
resetBtn = "reset_incbtn"

export
css : List Rule
css =
  [ class resetContent !!
      [ Display             .= Grid
      , ColumnGap           .= px 10
      , RowGap              .= px 10
      , GridTemplateColumns .= [px 170, fr 1, fr 3]
      , GridTemplateRows    .= replicate 4 MinContent
      , Padding             .= VH (px 20) (px 10)
      ]

  , class resetLbl  !!
      [ GridColumn      .= At 1
      , GridRow         .= At 1
      ]

  , id btnReset.id  !!
      [ GridColumn      .= At 2
      , GridRow         .= At 1
      ]

  , class incLbl  !!
      [ GridColumn      .= At 1
      , GridRow         .= At 2
      ]

  , id btnInc.id  !!
      [ GridColumn      .= At 2
      , GridRow         .= At 2
      ]

  , class decLbl  !!
      [ GridColumn      .= At 1
      , GridRow         .= At 3
      ]

  , id btnDec.id  !!
      [ GridColumn      .= At 2
      , GridRow         .= At 3
      ]

  , class countLbl  !!
      [ GridColumn      .= At 1
      , GridRow         .= At 4
      ]

  , id out.id  !!
      [ FontSize        .= Large
      , GridColumn      .= At 2
      , GridRow         .= At 4
      , TextAlign       .= End
      ]
  ]
