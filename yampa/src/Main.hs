-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Test program to implement a graphical timer using yampa.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    Arrows
  , LambdaCase
  , RecordWildCards

  #-}

-----------------------------------------------------------------------------

module Main where

-----------------------------------------------------------------------------

import Control.Monad
  ( when
  , void
  )

import Control.Arrow
  ( arr
  , returnA
  )

import Data.IORef
  ( newIORef
  , readIORef
  , writeIORef
  )

import Sound.ProteaAudio
  ( initAudio
  , finishAudio
  , sampleFromMemoryWav
  , soundLoop
  , soundStop
  )

import FRP.Yampa
  ( Event(..)
  , Time
  , SF
  , reactInit
  , react
  , iPre
  , time
  )

import Graphics.Gloss
  ( Display(..)
  , Color
  , Picture
  , pictures
  , makeColor
  , translate
  , scale
  , line
  , blank
  , text
  , color
  , red
  , white
  , circleSolid
  , thickCircle
  )

import qualified Graphics.Gloss.Interface.IO.Game as G
  ( Event
  )

import Graphics.Gloss.Interface.IO.Game
  ( Key(..)
  , KeyState(..)
  , MouseButton(..)
  , Modifiers(..)
  , SpecialKey(..)
  , playIO
  )

import qualified Graphics.Gloss.Interface.IO.Game as Game
  ( Event(..)
  )

import qualified Control
  ( control
  )

import Beep
  ( beepData
  )

-----------------------------------------------------------------------------

data TTime =
  TTime
    { tMin :: Int
    , tSec :: Int
    , diff :: Time
    }
  deriving (Eq)

-----------------------------------------------------------------------------

data ClickEvent =
    MinButtonDown
  | SecButtonDown
  | StartStopButtonDown
  | ButtonUp

-----------------------------------------------------------------------------

control
  :: SF (Time, Bool, Bool, Bool) (Bool, Picture, TTime)

control =
  Control.control
    iPre
    equal
    zero
    cDown
    cUp
    display
    incM
    incS
    False
    (display zero)
    zero

  where
    equal t t'=
      tMin t == tMin t' && tSec t == tSec t'

    zero =
      TTime
        { tMin = 0
        , tSec = 0
        , diff = 0.0
        }

    incM t@TTime{..}
      | tMin >= 0 && tMin < 99 = t { tMin = tMin + 1, diff = 0.0 }
      | otherwise            = t { tMin = 0, diff = 0.0 }

    incS t@TTime{..}
      | tSec >= 0 && tSec < 60 = t { tSec = tSec + 1, diff = 0.0 }
      | otherwise            = incM t { tSec = 0, diff = 0.0 }

    cUp t@TTime{..} dt
      | diff + dt < 1.0 = t { diff = diff + dt }
      | otherwise       = incS t { diff = diff + dt - 1.0 }

    cDown t@TTime{..} dt
      | diff + dt < 1.0 = t { diff = diff + dt }
      | tSec > 0        = t { tSec = tSec - 1, diff = diff + dt - 1.0 }
      | tMin > 0        = t { tMin = tMin - 1, tSec = 59, diff = diff + dt - 1.0 }
      | otherwise       = zero

    display TTime{..} =
      translate (-165) 20 $ text $ dsp tMin ++ ":" ++ dsp tSec

    dsp i
      | i < 0     = "00"
      | i < 10    = '0' : show i
      | i < 100   = show i
      | otherwise = "99"

-----------------------------------------------------------------------------

main
  :: IO ()

main =
  playYampa
    (InWindow "Yampa Example" (360, 300) (800, 200))
    grey
    5
    mainSF

-----------------------------------------------------------------------------

-- Main signal function that is executed

mainSF
  :: SF (Event Game.Event) (Picture, Bool)

mainSF = proc e -> do
  e' <- arr filterInput -< e
  (bM, bS, bA) <- buttonState -< e'
  dt <- timeDiff -< ()
  (b,p,_) <- control -< (dt, bM, bS, bA)
  r <- arr render -< (bM, bS, bA, p)

  returnA -< (r,b)

-----------------------------------------------------------------------------

timeDiff
  :: SF a Time

timeDiff = proc e -> do
  rec
    t <- time -< e
    last <- iPre 0 -< t
  returnA -< (t - last)

-----------------------------------------------------------------------------

grey
  :: Color

grey =
  makeColor 0.5 0.5 0.5 1.0

-----------------------------------------------------------------------------

buttonState
  :: SF (Event ClickEvent)
       (Bool, Bool, Bool)

buttonState = proc e ->
  do
    rec
      (b0, b1, b2) <- iPre (False, False, False) -< (n0, n1, n2)
      (n0, n1, n2) <- arr process -< (b0, b1, b2, e)
    returnA -< (n0,n1,n2)

  where
    process (s0, s1, s2, e) = case e of
      NoEvent                   -> (   s0,    s1,    s2)
      Event MinButtonDown       -> ( True,    s1,    s2)
      Event SecButtonDown       -> (   s0,  True,    s2)
      Event StartStopButtonDown -> (   s0,    s1,  True)
      Event ButtonUp            -> (False, False, False)

-----------------------------------------------------------------------------

filterInput
  :: Event Game.Event -> Event ClickEvent

filterInput = \case
  Event (Game.EventKey (MouseButton LeftButton) Down Modifiers{..} p)
    | inRange (-120,-80) 50 p -> Event MinButtonDown
    | inRange (0,-80) 50 p    -> Event SecButtonDown
    | inRange (120,-80) 50 p  -> Event StartStopButtonDown
    | otherwise               -> NoEvent
  Event (Game.EventKey (MouseButton LeftButton) Up Modifiers{..} _) ->
    case shift of
      Down -> NoEvent
      Up   -> Event ButtonUp
  Event (Game.EventKey (SpecialKey KeyShiftL) Up _ _) ->
    Event ButtonUp
  Event (Game.EventKey (SpecialKey KeyShiftR) Up _ _) ->
    Event ButtonUp
  _                           -> NoEvent

  where
    inRange (xc,yc) r (x,y) =
      let
        dx = abs (x - xc)
        dy = abs (y - yc)
      in
        sqrt (dx * dx + dy * dy) <= r

-----------------------------------------------------------------------------

render
  :: (Bool, Bool, Bool, Picture) -> Picture

render (pm, ps, pr, tp) =
  pictures
    [ translate (-120) (-80) $
        pictures
          [ color (if pm then red else white) $ circleSolid 50
          , thickCircle 50 3
          , translate (-38) (-17) $ scale 0.4 0.4 $ text "Min"
          ]
    , translate 0 (-80) $
        pictures
          [ color (if ps then red else white) $ circleSolid 50
          , thickCircle 50 3
          , translate (-42) (-17) $ scale 0.4 0.4 $ text "Sec"
          ]
    , translate 120 (-80) $
        pictures
          [ color (if pr then red else white) $ circleSolid 50
          , thickCircle 50 3
          , translate (-34) 10 $ scale 0.26 0.20 $ text "Stop"
          , translate (-35) (-25) $ scale 0.26 0.20 $ text "Start"
          , line [ (-40.0, 0.0), (40.0, 0.0) ]
          ]
    , tp
    ]

-----------------------------------------------------------------------------

-- | Adapted version of 'playYampa' that is also capable of playing
-- beeps

playYampa
  :: Display -> Color -> Int
  -> SF (Event G.Event) (Picture, Bool)
  -> IO ()

playYampa display color frequency mainSF =
  -- initialize audio device
  initAudio 1 22050 256 >>= \case
    False -> putStrLn "Cannot open audio device..."
    True  -> do
      -- load buzzer sound from memory
      beep <- sampleFromMemoryWav beepData 1

      -- create IORefs for holding the picture state and the music
      -- playing state
      picRef <- newIORef (blank, False)
      musicState <- newIORef False

      -- initialize the handle
      handle <- reactInit
        (return NoEvent)
        (\_ updated output ->
          when updated (picRef `writeIORef` output)
            >> return False)
        mainSF

      let
        -- delta is fix, since we use a fixed freqency
        delta =
          0.01 / fromIntegral frequency

        -- output processing function
        outputs _ = do
          (pic, playNew) <- readIORef picRef
          playOld <- readIORef musicState
          when (not playOld && playNew) $
            soundLoop beep 1 1 0 0.9
          when (playOld && not playNew) $
            void $ soundStop beep
          writeIORef musicState playNew
          return pic

        -- put processing function
        inputs e t =
          react handle (delta, Just (Event e)) >> return (t + delta)

        -- update function to process one time step
        step d t =
          let delta' = realToFrac d - t
          in if delta' > 0
             then react handle (delta', Just NoEvent) >> return 0.0
             else return (-delta')

      playIO display color frequency 0 outputs inputs step

      -- release audio device
      finishAudio

-----------------------------------------------------------------------------
