module Examples.Fractals

import Control.Applicative.Syntax
import Data.Either
import Data.IORef
import Data.MSF.Switch
import Data.Nat
import Data.So
import Examples.CSS.Fractals
import Examples.Fractals.Dragon
import Generics.Derive
import Rhone.JS

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
        | Redraw String
        | Run
        | Next

%runElab derive "Ev" [Generic,Meta,Show,Eq]

MaxIter : Nat
MaxIter = 18

record Iterations where
  constructor MkIterations
  value : Nat
  0 prf : LTE value MaxIter

namespace Iterations
  export
  fromInteger :  (n : Integer)
              -> {auto 0 prf : LTE (fromInteger n) MaxIter}
              -> Iterations
  fromInteger n = MkIterations (cast n) prf

  export
  read : String -> Either String Iterations
  read "0" = Right $ MkIterations 0 LTEZero
  read s = case cast {to = Nat} s of
    0 => Left #"Not a natural number: \#{s}"#
    n => case isLTE n MaxIter of
      Yes prf   => Right $ MkIterations n prf
      No contra => Left #"Value must be <= \#{show MaxIter}"#

isDelay : Bits32 -> Bool
isDelay v = 100 <= v && v <= 10000

record RedrawAfter where
  constructor RA
  value : Bits32
  0 prf : So (isDelay value)

namespace RedrawAfter
  export
  read : String -> Either String RedrawAfter
  read s =
    let v = cast {to = Bits32} s
     in case choose (isDelay v) of
          Left oh => Right $ RA v oh
          Right _ => Left $ "Enter a value between 100 and 10'000"

record Config where
  constructor MkConfig
  fractal    : Fractal
  iterations : Iterations
  redraw     : RedrawAfter

0 tr : LTE k m -> LTE m n -> LTE k n
tr = transitive {rel = LTE}

calc : Config -> Iterations -> NP I [Iterations,String]
calc (MkConfig _ (MkIterations is pis) _) (MkIterations n pn) =
  case isLT n is of
    Yes prf => [MkIterations (S n) $ tr prf pis, mkDragon n]
    No _    => [0, mkDragon n]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

public export
M : Type -> Type
M = DomIO Ev JSIO

MSFEvent : Type -> Type
MSFEvent = Data.MSF.Event.Event

msf : (timer : RedrawAfter -> JSIO ()) -> MSF M Ev ()
msf timer = rswitchWhen (const ()) config fractal
  where fractal : Config -> MSF M Ev ()
        fractal c = ifIs Next $ unfold (calc c) 0 >>> innerHtml out

        readAll : MSF M Ev (Either String Config)
        readAll =    MkConfig Dragon
                <$$> input (\case Iter   s => Ev s; _ => NoEv) read txtIter
                <**> input (\case Redraw s => Ev s; _ => NoEv) read txtRedraw
                >>>  observeWith (isLeft ^>> disabledAt btnRun)

        config : MSF M Ev (MSFEvent Config)
        config =   fan [readAll, is Run]
               >>> rightOnEvent
               >>> observeWith (ifEvent $ arrM (liftJSIO . timer . redraw))

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
                  , onEnterDown Run
                  , class widget
                  , placeholder #"Enter a natural number <= \#{show MaxIter}"#

                  ] []
          ]
      , line "Iteration delay [ms]:"
          [ input [ id txtRedraw.id
                  , onInput Redraw
                  , onEnterDown Run
                  , class widget
                  , placeholder #"Enter a number in the range [100,10'000]"#
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
  innerHtmlAt exampleDiv content
  ref  <- newIORef {a = Maybe IntervalID} Nothing
  h    <- handler <$> env 

  let cleanup : JSIO ()
      cleanup = readIORef ref >>= traverse_ clearInterval

      timer   : RedrawAfter -> JSIO ()
      timer ra = do
        cleanup
        newID <- setInterval ra.value (h Next)
        writeIORef ref (Just newID)

  pure (msf timer, cleanup)
