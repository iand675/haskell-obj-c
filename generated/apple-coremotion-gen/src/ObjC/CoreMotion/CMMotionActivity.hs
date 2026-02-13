{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMMotionActivity@.
module ObjC.CoreMotion.CMMotionActivity
  ( CMMotionActivity
  , IsCMMotionActivity(..)
  , confidence
  , startDate
  , unknown
  , stationary
  , walking
  , running
  , automotive
  , cycling
  , automotiveSelector
  , confidenceSelector
  , cyclingSelector
  , runningSelector
  , startDateSelector
  , stationarySelector
  , unknownSelector
  , walkingSelector

  -- * Enum types
  , CMMotionActivityConfidence(CMMotionActivityConfidence)
  , pattern CMMotionActivityConfidenceLow
  , pattern CMMotionActivityConfidenceMedium
  , pattern CMMotionActivityConfidenceHigh

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- confidence@
confidence :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO CMMotionActivityConfidence
confidence cmMotionActivity =
  sendMessage cmMotionActivity confidenceSelector

-- | @- startDate@
startDate :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO (Id NSDate)
startDate cmMotionActivity =
  sendMessage cmMotionActivity startDateSelector

-- | @- unknown@
unknown :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO Bool
unknown cmMotionActivity =
  sendMessage cmMotionActivity unknownSelector

-- | @- stationary@
stationary :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO Bool
stationary cmMotionActivity =
  sendMessage cmMotionActivity stationarySelector

-- | @- walking@
walking :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO Bool
walking cmMotionActivity =
  sendMessage cmMotionActivity walkingSelector

-- | @- running@
running :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO Bool
running cmMotionActivity =
  sendMessage cmMotionActivity runningSelector

-- | @- automotive@
automotive :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO Bool
automotive cmMotionActivity =
  sendMessage cmMotionActivity automotiveSelector

-- | @- cycling@
cycling :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO Bool
cycling cmMotionActivity =
  sendMessage cmMotionActivity cyclingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @confidence@
confidenceSelector :: Selector '[] CMMotionActivityConfidence
confidenceSelector = mkSelector "confidence"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @unknown@
unknownSelector :: Selector '[] Bool
unknownSelector = mkSelector "unknown"

-- | @Selector@ for @stationary@
stationarySelector :: Selector '[] Bool
stationarySelector = mkSelector "stationary"

-- | @Selector@ for @walking@
walkingSelector :: Selector '[] Bool
walkingSelector = mkSelector "walking"

-- | @Selector@ for @running@
runningSelector :: Selector '[] Bool
runningSelector = mkSelector "running"

-- | @Selector@ for @automotive@
automotiveSelector :: Selector '[] Bool
automotiveSelector = mkSelector "automotive"

-- | @Selector@ for @cycling@
cyclingSelector :: Selector '[] Bool
cyclingSelector = mkSelector "cycling"

