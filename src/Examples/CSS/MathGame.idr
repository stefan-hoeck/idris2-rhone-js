||| CSS Rules for the Math Game Example
module Examples.CSS.MathGame

import Data.Vect
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

||| Button to check the entered result
export
checkBtn : ElemRef Button
checkBtn = MkRef Button "mathgame_check_btn"

||| Button to start a new game
export
newBtn : ElemRef Button
newBtn = MkRef Button "mathgame_newbtn"

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

data Tag = LLan | ILan | OClc | IRes | BChk | ORep | BNew | OPic | Dot

AreaTag Tag where
  showTag LLan = "LLan"
  showTag ILan = "ILan"
  showTag OClc = "OClc"
  showTag IRes = "IRes"
  showTag BChk = "BChk"
  showTag ORep = "ORep"
  showTag BNew = "BNew"
  showTag OPic = "OPic"
  showTag Dot  = "."

export
css : List (Rule 1)
css =
  [ Media "min-width: 300px"
      [ class mathContent !!
          [ Display             .= Area
              (replicate 6 MinContent)
              [MaxContent, MaxContent]
              [ [LLan, ILan]
              , [OClc, IRes]
              , [Dot,  BChk]
              , [Dot,  BNew]
              , [ORep, ORep]
              , [OPic, OPic]
              ]

          , ColumnGap           .= px 10
          , RowGap              .= px 10
          , Padding             .= VH (px 20) (px 10)
          ]
      ]

  , Media "min-width: 800px"
      [ class mathContent !!
          [ Display             .= Area
              (replicate 6 MinContent)
              [MaxContent, MaxContent, fr 1]
              [ [LLan, ILan, OPic]
              , [OClc, IRes, OPic]
              , [Dot,  BChk, OPic]
              , [Dot,  BNew, OPic]
              , [ORep, ORep, OPic]
              , [Dot,  Dot,  OPic]
              ]

          , ColumnGap           .= px 10
          , RowGap              .= px 10
          , Padding             .= VH (px 20) (px 10)
          ]
      ]

  , class lblLang !! [ GridArea .= LLan ]
  
  , id langIn.id  !!
      [ GridArea        .= ILan
      , FontSize        .= Large
      , TextAlign       .= End
      ]
  
  , id calc.id  !!
      [ GridArea        .= OClc
      , FontSize        .= Large
      , TextAlign       .= Start
      ]
  
  , id resultIn.id  !!
      [ GridArea        .= IRes
      , FontSize        .= Large
      , TextAlign       .= End
      ]
  
  , id checkBtn.id  !! [ GridArea .= BChk ]
  
  , id newBtn.id  !! [ GridArea .= BNew ]
  
  , id out.id  !!
      [ GridArea        .= ORep
      , FontSize        .= Large
      , TextAlign       .= Start
      ]

  , id pic.id  !!
      [ BackgroundSize  .= perc 100
      , JustifySelf     .= Center
      , GridArea        .= OPic
      , MaxWidth        .= px 500
      , Width           .= px 500
      ]

  , class correct !!
      [ Color           .= green ]

  , class wrong !!
      [ Color           .= red ]
  ]
