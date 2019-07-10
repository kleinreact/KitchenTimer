-----------------------------------------------------------------------------
-- |
-- Module      :  TopEntity
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Kitchen Timer Control
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    DataKinds
  , LambdaCase
  , MultiWayIf
  , RecordWildCards
  , CPP

  #-}

-----------------------------------------------------------------------------

module TopEntity where

-----------------------------------------------------------------------------

import Clash.Prelude

import Control
  ( control
  )

-----------------------------------------------------------------------------

{-# ANN topEntity (
  Synthesize
    { t_name    = "KitchenTimer"
    , t_inputs  =
        [ PortProduct ""
            [ PortName "CLOCK"
            , PortName "RESET"
            ]
        , PortName "BTMIN"
        , PortName "BTSEC"
        , PortName "BTSAS"
        ]
    , t_output =
        PortProduct ""
          [ PortName "D0"
          , PortName "D1"
          , PortName "D2"
          , PortName "D3"
          , PortName "BUZZR"
          ]
    }
  )
  #-}

-----------------------------------------------------------------------------

type Digit = Unsigned 7

-----------------------------------------------------------------------------

data Time =
  Time
    { secD0 :: Unsigned 4
    , secD1 :: Unsigned 4
    , minD0 :: Unsigned 4
    , minD1 :: Unsigned 4
    , ticks :: Unsigned 19
    }
  deriving (Eq)

-----------------------------------------------------------------------------

zero
  :: Time

zero =
  Time 0 0 0 0 0

-----------------------------------------------------------------------------

topEntity
  :: SystemClockReset
  => Signal System Bool
  -> Signal System Bool
  -> Signal System Bool
  -> Signal System (Digit, Digit, Digit, Digit, Bool)

topEntity bM bS bA =
  let
    bM' = debounce bM
    bS' = debounce bS
    bA' = debounce bA

    (b, output, _) =
      control
        (==)
        zero
        countdown
        countup
        display
        incMin
        incSec
        False
        (0, 0, 0, 0)
        zero
        (pure 1)
        bM'
        bS'
        bA'

    (d0, d1, d2, d3) = unbundle output
  in
    bundle (d0, d1, d2, d3, beeper b)

{-# NOINLINE topEntity #-}

-----------------------------------------------------------------------------

countup
  :: Time -> Unsigned 19 -> Time

countup t@Time{..} dt
  | ticks < 333000 = t { ticks = ticks + dt }
  | otherwise      = incSec t { ticks = 0 }

-----------------------------------------------------------------------------

countdown
  :: Time -> Unsigned 19 -> Time

countdown t@Time{..} dt
  | ticks >= dt = t { ticks = ticks - dt }
  | secD0 > 0  = upd t { secD0 = secD0 - 1 }
  | secD1 > 0  = upd t { secD1 = secD1 - 1, secD0 = 9 }
  | minD0 > 0  = upd t { minD0 = minD0 - 1, secD1 = 5, secD0 = 9 }
  | minD1 > 0  = upd t { minD1 = minD1 - 1, minD0 = 9, secD1 = 5, secD0 = 9 }
  | otherwise  = zero

  where
    upd x = x { ticks = 333000 - dt + ticks }

-----------------------------------------------------------------------------

incSec ::
  Time -> Time

incSec t@Time{..}
  | secD0 < 9 = t { secD0 = secD0 + 1 }
  | secD1 < 5 = t { secD1 = secD1 + 1, secD0 = 0 }
  | minD0 < 9 = t { minD0 = minD0 + 1, secD0 = 0, secD1 = 0 }
  | minD1 < 9 = t { minD1 = minD1 + 1, minD0 = 0, secD0 = 0, secD1 = 0 }
  | otherwise = t { minD1 = 0, minD0 = 0, secD0 = 0, secD1 = 0 }

-----------------------------------------------------------------------------

incMin ::
  Time -> Time

incMin t@Time{..}
  | minD0 < 9 = t { minD0 = minD0 + 1 }
  | minD1 < 9 = t { minD1 = minD1 + 1, minD0 = 0 }
  | otherwise = t { minD1 = 0, minD0 = 0 }

-----------------------------------------------------------------------------

display
  :: Time -> (Digit, Digit, Digit, Digit)

display Time{..} =
  ( toDigit minD1
  , toDigit minD0
  , toDigit secD1
  , toDigit secD0
  )

-----------------------------------------------------------------------------

toDigit
  :: Unsigned 4 -> Digit

toDigit = \case
  0 -> 8
  1 -> 91
  2 -> 34
  3 -> 18
  4 -> 81
  5 -> 20
  6 -> 4
  7 -> 90
  8 -> 0
  _ -> 16

-----------------------------------------------------------------------------

debounce
  :: HiddenClockReset domain gated synchronous
  => Signal domain Bool -> Signal domain Bool

debounce input =
  let
    state = register False state'
    counter = register 0 counter'
    r = upd <$> input <*> state <*> counter
    (state',counter') = unbundle r
  in
    state'

  where
    upd
      :: Bool -> Bool -> Unsigned 11 -> (Bool, Unsigned 11)

    upd i s c
      | c > 0     = (s, c - 1)
      | i == s     = (s, 0)
      | otherwise = (i, 1665)

-----------------------------------------------------------------------------

beeper
  :: HiddenClockReset domain gated synchronous
  => Signal domain Bool
  -> Signal domain Bool

beeper enabled =
  let
    hold = register False hold'
    counter = register (0 :: Unsigned 19) counter'

    hold' =
      (hold `_and_` ((< 40000) <$> counter))
      `_or_` (enabled `_and_` ((< 320000) <$> counter))

    counter' =
      gate
        id hold'
        (+1) (const 0) counter
  in
    gate
      id (hold' `_and_` (beeptime <$> counter))
#ifdef PASSIVEBUZZER
      id (const False) beepFreq
#else
      id (const False) (pure True)
#endif

  where
    beeptime x =
        x <  19000
      || x >=  40000 && x <  59000
      || x >=  80000 && x <  99000
      || x >= 120000 && x < 139000

    _or_ = liftA2 (||)
    _and_ = liftA2 (&&)

-----------------------------------------------------------------------------

#ifdef PASSIVEBUZZER
beepFreq
  :: HiddenClockReset domain gated synchronous
  => Signal domain Bool

beepFreq =
  let
    output = register False output'
    step = register (0 :: Unsigned 6) step'

    output' = gate (== 0) step not id output
    step' = gate (< 36) step (+ 1) (const 0) step
  in
    output'
#endif

-----------------------------------------------------------------------------

gate
  :: Applicative f
  => (a -> Bool) -> f a -> (b -> c) -> (b -> c) -> f b -> f c

gate check b u1 u2 x =
  (\a b -> if check a then u1 b else u2 b) <$> b <*> x

-----------------------------------------------------------------------------
