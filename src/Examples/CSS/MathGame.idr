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

export
input : ElemRef Input
input = MkRef Input "mathgame_input"

||| ID of the picture canvas
export
pic : ElemRef Canvas
pic = MkRef Canvas "mathgame_pic"

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

  , class correct !!
      [ Color           .= green ]

  , class wrong !!
      [ Color           .= red ]
  ]
