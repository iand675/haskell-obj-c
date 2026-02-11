{-# LANGUAGE PatternSynonyms #-}
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
  , confidenceSelector
  , startDateSelector
  , unknownSelector
  , stationarySelector
  , walkingSelector
  , runningSelector
  , automotiveSelector
  , cyclingSelector

  -- * Enum types
  , CMMotionActivityConfidence(CMMotionActivityConfidence)
  , pattern CMMotionActivityConfidenceLow
  , pattern CMMotionActivityConfidenceMedium
  , pattern CMMotionActivityConfidenceHigh

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- confidence@
confidence :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO CMMotionActivityConfidence
confidence cmMotionActivity  =
  fmap (coerce :: CLong -> CMMotionActivityConfidence) $ sendMsg cmMotionActivity (mkSelector "confidence") retCLong []

-- | @- startDate@
startDate :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO (Id NSDate)
startDate cmMotionActivity  =
  sendMsg cmMotionActivity (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- unknown@
unknown :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO Bool
unknown cmMotionActivity  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionActivity (mkSelector "unknown") retCULong []

-- | @- stationary@
stationary :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO Bool
stationary cmMotionActivity  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionActivity (mkSelector "stationary") retCULong []

-- | @- walking@
walking :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO Bool
walking cmMotionActivity  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionActivity (mkSelector "walking") retCULong []

-- | @- running@
running :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO Bool
running cmMotionActivity  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionActivity (mkSelector "running") retCULong []

-- | @- automotive@
automotive :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO Bool
automotive cmMotionActivity  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionActivity (mkSelector "automotive") retCULong []

-- | @- cycling@
cycling :: IsCMMotionActivity cmMotionActivity => cmMotionActivity -> IO Bool
cycling cmMotionActivity  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionActivity (mkSelector "cycling") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @confidence@
confidenceSelector :: Selector
confidenceSelector = mkSelector "confidence"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @unknown@
unknownSelector :: Selector
unknownSelector = mkSelector "unknown"

-- | @Selector@ for @stationary@
stationarySelector :: Selector
stationarySelector = mkSelector "stationary"

-- | @Selector@ for @walking@
walkingSelector :: Selector
walkingSelector = mkSelector "walking"

-- | @Selector@ for @running@
runningSelector :: Selector
runningSelector = mkSelector "running"

-- | @Selector@ for @automotive@
automotiveSelector :: Selector
automotiveSelector = mkSelector "automotive"

-- | @Selector@ for @cycling@
cyclingSelector :: Selector
cyclingSelector = mkSelector "cycling"

