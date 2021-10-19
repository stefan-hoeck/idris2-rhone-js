module Examples.Balls

import Data.MSF.Switch
import Data.Either
import Data.Nat
import Data.Vect
import Examples.CSS.Balls
import Examples.FRP.Ball
import Examples.FRP.BallCanvas
import Examples.Util
import Generics.Derive
import Rhone.Canvas
import Rhone.JS

%language ElabReflection
%default total

data Ev = Run | NumIn | Next Bits32

next : Ev -> Event Bits32
next (Next d) = Ev d
next _        = NoEv

%runElab derive "Ev" [Generic,Meta,Show,Eq]

content : Node Ev
content =
  div [ class widgetList ]
      [ line "Number of balls:"
          [ input [ id txtCount.id
                  , onInput (const NumIn)
                  , onEnterDown Run
                  , class widget
                  , placeholder #"Range: [1,1000]"#
                  ] []
          , button [id btnRun.id, onClick Run, classes [widget,btn]] ["Run"]
          ]
      , div [id log.id] []
      , div [] [canvas [id out.id, width 520, height 520] [] ]
      ]

--------------------------------------------------------------------------------
--          UI
--------------------------------------------------------------------------------

public export
M : Type -> Type
M = DomIO Ev JSIO

read : String -> Either String (List Ball)
read s =
  let n = cast {to = Nat} s
   in if 0 < n && n <= 1000
        then Right (initialBalls n)
        else Left "Enter a number between 1 and 1000"


animation : List Ball -> MSF M Ev ()
animation bs = next ^>> ifEvent (
                 fan_ [ balls bs >>> arrM (renderBalls out 520 520)
                      , fps 15 >>> ifEvent (showFPS ^>> text log)
                      ]
               )

msf : MSF M Ev ()
msf = drswitchWhen neutral initialBalls animation
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
  clear <- animate (h . Next)
  pure (msf, liftIO clear)
