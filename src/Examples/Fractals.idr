module Examples.Fractals

import Data.Either
import Data.MSF.Switch
import Data.Nat
import Examples.CSS
import Examples.Fractals.Dragon
import Generics.Derive
import Rhone.JS
import Text.CSS

%language ElabReflection
%default total

--------------------------------------------------------------------------------
--          Interval
--------------------------------------------------------------------------------

data IntervalID : Type where [external]

%foreign "browser:lambda:(n,h,w)=>setInterval(() => h(w),n)"
prim__setInterval : Bits32 -> IO () -> PrimIO IntervalID

%foreign "browser:lambda:(i,w)=>clearInterval(i)"
prim__clearInterval : IntervalID -> PrimIO ()

setInterval : HasIO io => Bits32 -> JSIO () -> io (IntervalID)
setInterval millis run = primIO $ prim__setInterval millis (runJS run)

clearInterval : HasIO io => IntervalID -> io ()
clearInterval id = primIO $ prim__clearInterval id

--------------------------------------------------------------------------------
--          Model
--------------------------------------------------------------------------------

data Fractal = Dragon

%runElab derive "Fractal" [Generic,Meta,Show,Eq]

data Ev = Fract Fractal
        | Iter String
        | Run
        | Next

%runElab derive "Ev" [Generic,Meta,Show,Eq]

MaxIter : Nat
MaxIter = 18

record Iterations where
  constructor MkIterations
  value : Nat
  0 prf : LTE value MaxIter

fromInteger :  (n : Integer)
            -> {auto 0 prf : LTE (fromInteger n) MaxIter}
            -> Iterations
fromInteger n = MkIterations (cast n) prf

readIterations : String -> Either String Iterations
readIterations "0" = Right $ MkIterations 0 LTEZero
readIterations s = case cast {to = Nat} s of
  0 => Left #"Not a natural number: \#{s}"#
  n => case isLTE n MaxIter of
    Yes prf   => Right $ MkIterations n prf
    No contra => Left #"Value must be <= \#{show MaxIter}"#

record Config where
  constructor MkConfig
  iterations : Iterations
  fractal    : Fractal

0 tr : LTE k m -> LTE m n -> LTE k n
tr = transitive {rel = LTE}

calc : Config -> Iterations -> NP I [Iterations,String]
calc (MkConfig (MkIterations is pis) _) (MkIterations n pn) =
  case isLT n is of
    Yes prf => [MkIterations (S n) $ tr prf pis, mkDragon n]
    No _    => [0, mkDragon n]

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

out : ElemRef HTMLDivElement
out = MkRef Div "fractalout"

btnRun : ElemRef HTMLButtonElement
btnRun = MkRef Button "btnrun"

txtIter : ElemRef HTMLInputElement
txtIter = MkRef Input "txtiter"

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

public export
M : Type -> Type
M = DomIO Ev JSIO

MSFEvent : Type -> Type
MSFEvent = Data.MSF.Event.Event

msf : MSF M Ev ()
msf = rswitchWhen (const ()) config fractal
  where fractal : Config -> MSF M Ev ()
        fractal c = ifIs Next $ unfold (calc c) 0 >>> innerHtml out

        iters : MSF M Ev (Either String Config)
        iters =   arr (\case Iter s => Ev $ readIterations s; _ => NoEv)
              >>> hold (readIterations "")
              >>> observeWith (leftInvalid txtIter)
              >>> observeWith (isLeft ^>> disabledAt btnRun)
              >>^ map (`MkConfig` Dragon)

        config : MSF M Ev (MSFEvent Config)
        config = fan [iters, is Run] >>> rightOnEvent

--------------------------------------------------------------------------------
--          CSS
--------------------------------------------------------------------------------

css : List Rule
css =
  [ id txtIter.id !!
      [ Margin          .= pt 5
      , TextAlign       .= End
      , Width           .= perc 20
      ]

  , id out.id !!
      [ Flex            .= "1"
      , Margin          .= pt 5
      ]
  ]

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

line : (lbl: String) -> List (Node Ev) -> Node Ev
line lbl ns =
  div [class widgetLine] $ 
      label [class widgetLabel] [Text lbl] :: ns

content : Node Ev
content =
  div [ class widgetList ]
      [ line "Number of iterations:"
          [ input [ id txtIter.id
                  , onInput Iter
                  , class widget
                  , placeholder #"Enter a natural number <= \#{show MaxIter}"#
                  ] []
          , button [id btnRun.id, onClick Run, classes [widget,btn]] ["Run"]
          ]
      , div [id out.id] []
      ]

--------------------------------------------------------------------------------
--          UI
--------------------------------------------------------------------------------

export
ui : M (MSF M Ev (), JSIO ())
ui = do
  applyCSS $ coreCSS ++ css
  innerHtmlAt exampleDiv content
  h    <- handler <$> env 
  myID <- setInterval 2000 (h Next)
  pure (msf, clearInterval myID)
