module Examples.CSS.Balls

import Rhone.JS
import public Examples.CSS.Core
import Text.CSS

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

export
out : ElemRef HTMLDivElement
out = MkRef Div "balls_out"

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
  ]
