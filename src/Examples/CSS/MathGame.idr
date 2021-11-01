||| CSS Rules for the Math Game Example
module Examples.CSS.MathGame

import Examples.CSS.Colors
import public Examples.CSS.Core
import Rhone.JS
import Text.CSS

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

||| Where the message about the correct result
||| is printed to.
export
out : ElemRef Div
out = MkRef Div "mathgame_out"

||| Text field where users enter their result.
export
resultIn : ElemRef Input
resultIn = MkRef Input "mathgame_input"

||| Text field where users enter their result.
export
checkBtn : ElemRef Button
checkBtn = MkRef Button "mathgame_check_btn"

||| ID of the picture canvas
export
pic : ElemRef Canvas
pic = MkRef Canvas "mathgame_pic"

||| ID of the calculation label
export
calc : ElemRef Div
calc = MkRef Div "mathgame_calc"

--------------------------------------------------------------------------------
--          CSS
--------------------------------------------------------------------------------

export
calculation : String
calculation = "calculation"

||| Message field class if answer is correct
export
correct : String
correct = "correct"

||| Message field class if answer is wrong
export
wrong : String
wrong = "wrong"

export
css : List Rule
css =
  [ id out.id  !!
      [ FontSize        .= Large
      , Margin          .= pt 5
      , TextAlign       .= End
      , Width           .= perc 10
      ]

  , id pic.id  !!
      [ BackgroundSize  .= perc 100
      ]

  , class correct !!
      [ Color           .= green ]

  , class wrong !!
      [ Color           .= red ]
  ]
