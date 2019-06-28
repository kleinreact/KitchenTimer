-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Mark Santolucito (mark.santolucito@yale.edu)
--                Felix Klein (klein@react.uni-saarland.de)
--
-- Test program to implement a graphical timer using threepenny.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards

  #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import Prelude hiding
  ( div
  )

import Control.Monad
  ( when
  , void
  )

import Graphics.UI.Threepenny
  ( Element
  , id_
  , type_
  , div
  , h1
  , audio
  , button
  , mousedown
  , mouseup
  )

import Graphics.UI.Threepenny.Core
  ( Behavior
  , Event
  , UI
  , (#)
  , (#+)
  , (<@)
  , set
  , row
  , column
  , style
  , string
  , title
  , accumB
  , startGUI
  , defaultConfig
  , runFunction
  , domEvent
  , on
  , onChanges
  , unionWith
  , getBody
  , stepper
  , element
  , never
  , text
  , sink
  , ffi
  )

import Graphics.UI.Threepenny.Timer
  ( timer
  , interval
  , start
  , tick
  )

import Control
  ( control
  )

import Beep
  ( beepData
  )

-----------------------------------------------------------------------------

-- | Internal data structure to represent time.

data TTime =
  TTime
    { tMin :: Int
    , tSec :: Int
    , diff :: Int
    }

-----------------------------------------------------------------------------

-- | Data structure to separate 'click' from 'mouse leave' events.

data Action =
    Clicked
  | MouseLeave
  deriving (Eq)

-----------------------------------------------------------------------------

-- | Update interval in milliseconds.

updateInterval
  :: Int

updateInterval =
  100

-----------------------------------------------------------------------------

-- | Main entry.

main
  :: IO ()

main =
  startGUI defaultConfig $ \window -> do
    -- set title
    void $ return window # set title "Kitchen Timer"

    -- create the timer element to be displayed
    screen <-
      h1
        #+ [ string "00:00" ]
        # set style [ ("font-size", "10em") ]

    -- create the MIN button
    min <-
      button
      #+ [ string "MIN" ]
      # set style [ ("font-size", "4em") ]

    -- create the SEC button
    sec <-
      button
      #+ [ string "SEC" ]
      # set style [ ("font-size", "4em") ]

    -- create the START/STOP button
    startstop <-
      button
      #+ [ string "START/STOP" ]
      # set style [ ("font-size", "4em") ]

    -- create the "buzzer"
    buzzer <-
      audio
      #+ [ string "unsupported" ]
      # set id_ "beep"
      # set type_ "audio/mp3"

    -- create some spacer for between the elements
    d1 <- div # set style [ ("width", "3em") ]
    d2 <- div # set style [ ("width", "5em") ]


    -- initialize the timer and start it
    t <- set interval updateInterval timer
    start t

    -- join all elements to the overall layout
    block <-
      div
        #+ [ column
             [ element screen
             , row $ map element [ min, d1, sec, d2, startstop ]
             , element buzzer
             ]
           ]
        # set style
          [ ("text-align","center")
          , ("position","absolute")
          , ("top","40%")
          , ("left","50%")
          , ("margin-right", "-50%")
          , ("transform", "translate(-50%, -50%)")
          ]

    void $ getBody window #+ [ element block ]

    -- load the music blob for the "buzzer"
    runFunction $ ffi $ concat
      [ "{"
      , "  const byteArray = new Array(" ++ beepData ++ ");"
      , "  var byteArrays = ["
      , "  ];"
      , "  for (var offset = 0; offset < byteArray.length; offset += 512) {"
      , "    var slice = byteArray.slice(offset, offset + 512);"
      , "    var a = new Uint8Array(slice);"
      , "    byteArrays.push(a);"
      , "  }"
      , "  const blob = new Blob(byteArrays, {type: 'audio/mp3'});"
      , "  const blobUrl = URL.createObjectURL(blob);"
      , "  beep = document.getElementById(\"beep\");"
      , "  beep.setAttribute('src', blobUrl);"
      , "}"
      ]

    let
      -- collect all buttons
      buttons
        :: [Element]

      buttons = [ min, sec, startstop ]

      -- collect all input events
      allEvents
        :: Event ()

      allEvents =
        foldl (unionWith (\_ _ -> ())) never
        $ tick t :
          map (void . mousedown) buttons ++
          map (void . mouseup) buttons

     -- cell are updated on all events
      cell
        :: a -> Behavior a -> UI (Behavior a)

      cell v c =
        stepper v (c <@ allEvents)

      -- play command
      play
        :: UI ()

      play =
        runFunction $ ffi
          "document.getElementById(\"beep\").play();"

      -- stop command
      stop
        :: UI ()

      stop =
        runFunction $ ffi $ concat
          [ "{"
          , "  beep = document.getElementById(\"beep\");"
          , "  beep.pause();"
          , "  beep.currentTime = 0;"
          , "}"
          ]

    -- create the time difference behavior
    dt <-
        accumB (0 :: Int)
      $ fmap const
      $ foldl (unionWith max) never
      $ fmap (const updateInterval) (tick t) :
        map (fmap (const 0) . mousedown) buttons ++
        map (fmap (const 0) . mouseup) buttons

    -- turn the button events into the input behavior
    bMin <- btn min
    bSec <- btn sec
    bStartStop <- btn startstop

    -- plug everything together with the control
    (b, d, _) <-
      control
        cell
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
        dt
        bMin
        bSec
        bStartStop

    -- pipe the displayed time to the output display
    void $ element screen # sink text d

    -- turn the "buzzer" on, whenever the buzzer signal turns high
    onChanges b (`when` play)

    -- we use the "ended" event of the music player in combination
    -- with the returned control signal to stop the buzzer exactly
    -- at the end of one sequence.
    on ((b <@) . domEvent "ended") buzzer (\x -> if x then play else stop)

  where
    clicker = fmap (const $ const True) . mousedown
    leaver = fmap (const $ const False) . mouseup
    btn x = accumB False $ unionWith (\_ y-> y) (clicker x) (leaver x)

    equal t t'=
      tMin t == tMin t' && tSec t == tSec t'

    zero =
      TTime
        { tMin = 0
        , tSec = 0
        , diff = 0
        }

    incM t@TTime{..}
      | tMin >= 0 && tMin < 99 = t { tMin = tMin + 1, diff = 0 }
      | otherwise            = t { tMin = 0, diff = 0 }

    incS t@TTime{..}
      | tSec >= 0 && tSec < 60 = t { tSec = tSec + 1, diff = 0 }
      | otherwise            = incM t { tSec = 0, diff = 0 }

    cUp t@TTime{..} dt
      | diff + dt < 1000 = t { diff = diff + dt }
      | otherwise        = incS t { diff = diff + dt - 1000 }

    cDown t@TTime{..} dt
      | diff + dt < 1000 = t { diff = diff + dt }
      | tSec > 0         = t { tSec = tSec - 1, diff = diff + dt - 1000 }
      | tMin > 0         = t { tMin = tMin - 1
                             , tSec = 59
                             , diff = diff + dt - 1000
                             }
      | otherwise        = zero

    display TTime{..} =
      dsp tMin ++ ":" ++ dsp tSec

    dsp i
      | i < 0     = "00"
      | i < 10    = '0' : show i
      | i < 100   = show i
      | otherwise = "99"

-----------------------------------------------------------------------------
