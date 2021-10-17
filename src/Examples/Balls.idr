module Examples.Balls

import Data.MSF.Switch
import Data.Either
import Data.Nat
import Data.Vect
import Examples.CSS.Balls
import Examples.FRP.Ball
import Examples.FRP.BallCanvas
import Examples.FRP.Basics
import Examples.Util
import Generics.Derive
import Rhone.Canvas
import Rhone.JS

%language ElabReflection
%default total

data Ev = Run | NumIn | Next

%runElab derive "Ev" [Generic,Meta,Show,Eq]

content : Node Ev
content =
  div [ class widgetList ]
      [ line "Number of balls:"
          [ input [ id txtCount.id
                  , onInput (const NumIn)
                  , onEnterDown Run
                  , class widget
                  , placeholder #"Range: [1,200]"#
                  ] []
          , button [id btnRun.id, onClick Run, classes [widget,btn]] ["Run"]
          ]
      , div [id log.id] []
      , div [] [canvas [id out.id, width 500, height 500] [] ]
      ]

--------------------------------------------------------------------------------
--          UI
--------------------------------------------------------------------------------

public export
M : Type -> Type
M = DomIO Ev JSIO

fps : DTime -> String
fps 0  = #"FPS: 0"#
fps dt = #"FPS: \#{show $ 1000 `div` dt}"#

read : String -> Either String (List Ball)
read s =
  let n = cast {to = Nat} s
   in if 0 < n && n <= 200
        then Right (initialBalls n)
        else Left "Enter a number between 1 and 100"


animation : List Ball -> MSF M Ev ()
animation inis =   ifIs Next
               $   runSF (const 20) (balls inis)
               >>> fan_ [ arrM (renderBalls out 500 500)
                        , realTimeDelta >>> fps ^>> text log]

msf : MSF M Ev ()
msf = rswitchWhen neutral initialBalls animation
  where readInit : MSF M Ev (Either String (List Ball))
        readInit =    getInput NumIn read txtCount
                 >>>  observeWith (isLeft ^>> disabledAt btnRun)

        initialBalls : MSF M Ev (MSFEvent $ List Ball)
        initialBalls =   fan [readInit, is Run]
                     >>> rightOnEvent

export
ui : M (MSF M Ev (), JSIO ())
ui = do
  innerHtmlAt exampleDiv content
  h     <- handler <$> env 
  newID <- setInterval 20 (h Next)
  pure (msf, clearInterval newID)