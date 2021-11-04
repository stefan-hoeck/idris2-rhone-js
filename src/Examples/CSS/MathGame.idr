||| CSS Rules for the Math Game Example
module Examples.CSS.MathGame

import Data.List
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

||| Select box where users choose the language.
export
langIn : ElemRef Select
langIn = MkRef Select "mathgame_language"

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

||| Message field class if answer is correct
export
correct : String
correct = "correct"

||| Message field class if answer is wrong
export
wrong : String
wrong = "wrong"

export
mathContent : String
mathContent = "mathgame_content"

export
lblLang : String
lblLang = "mathgame_lbllang"

export
css : List Rule
css =
  [ class mathContent !!
      [ Display             .= Grid
      , ColumnGap           .= px 10
      , RowGap              .= px 10
      , GridTemplateColumns .= [px 170, fr 1, fr 3]
      , GridTemplateRows    .= replicate 4 MinContent ++ [fr 1]
      , Padding             .= VH (px 20) (px 10)
      ]

  , class lblLang !!
      [ GridColumn      .= At 1
      , GridRow         .= At 1
      ]
  
  , id langIn.id  !!
      [ GridColumn      .= At 2
      , GridRow         .= At 1
      , FontSize        .= Large
      , TextAlign       .= End
      ]
  
  , id calc.id  !!
      [ GridColumn      .= At 1
      , GridRow         .= At 2
      , FontSize        .= Large
      , TextAlign       .= Start
      ]
  
  , id resultIn.id  !!
      [ GridColumn      .= At 2
      , GridRow         .= At 2
      , FontSize        .= Large
      , TextAlign       .= End
      ]
  
  , id checkBtn.id  !!
      [ GridColumn      .= At 1
      , GridRow         .= At 3
      ]
  
  , id out.id  !!
      [ GridColumn      .= At 2
      , GridRow         .= At 3
      , FontSize        .= Large
      , TextAlign       .= Start
      ]

  , id pic.id  !!
      [ BackgroundSize  .= perc 100
      , JustifySelf     .= Center
      , GridColumn      .= At 3
      , GridRow         .= FromTo 1 6
      , MaxWidth        .= px 500
      , Width           .= px 500
      ]

  , class correct !!
      [ Color           .= green ]

  , class wrong !!
      [ Color           .= red ]
  ]
