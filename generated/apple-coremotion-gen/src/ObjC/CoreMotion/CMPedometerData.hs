{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMPedometerData@.
module ObjC.CoreMotion.CMPedometerData
  ( CMPedometerData
  , IsCMPedometerData(..)
  , startDate
  , endDate
  , numberOfSteps
  , distance
  , floorsAscended
  , floorsDescended
  , currentPace
  , currentCadence
  , averageActivePace
  , averageActivePaceSelector
  , currentCadenceSelector
  , currentPaceSelector
  , distanceSelector
  , endDateSelector
  , floorsAscendedSelector
  , floorsDescendedSelector
  , numberOfStepsSelector
  , startDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- startDate@
startDate :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO (Id NSDate)
startDate cmPedometerData =
  sendMessage cmPedometerData startDateSelector

-- | @- endDate@
endDate :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO (Id NSDate)
endDate cmPedometerData =
  sendMessage cmPedometerData endDateSelector

-- | @- numberOfSteps@
numberOfSteps :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO (Id NSNumber)
numberOfSteps cmPedometerData =
  sendMessage cmPedometerData numberOfStepsSelector

-- | @- distance@
distance :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO (Id NSNumber)
distance cmPedometerData =
  sendMessage cmPedometerData distanceSelector

-- | @- floorsAscended@
floorsAscended :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO (Id NSNumber)
floorsAscended cmPedometerData =
  sendMessage cmPedometerData floorsAscendedSelector

-- | @- floorsDescended@
floorsDescended :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO (Id NSNumber)
floorsDescended cmPedometerData =
  sendMessage cmPedometerData floorsDescendedSelector

-- | @- currentPace@
currentPace :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO RawId
currentPace cmPedometerData =
  sendMessage cmPedometerData currentPaceSelector

-- | @- currentCadence@
currentCadence :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO RawId
currentCadence cmPedometerData =
  sendMessage cmPedometerData currentCadenceSelector

-- | @- averageActivePace@
averageActivePace :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO RawId
averageActivePace cmPedometerData =
  sendMessage cmPedometerData averageActivePaceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @numberOfSteps@
numberOfStepsSelector :: Selector '[] (Id NSNumber)
numberOfStepsSelector = mkSelector "numberOfSteps"

-- | @Selector@ for @distance@
distanceSelector :: Selector '[] (Id NSNumber)
distanceSelector = mkSelector "distance"

-- | @Selector@ for @floorsAscended@
floorsAscendedSelector :: Selector '[] (Id NSNumber)
floorsAscendedSelector = mkSelector "floorsAscended"

-- | @Selector@ for @floorsDescended@
floorsDescendedSelector :: Selector '[] (Id NSNumber)
floorsDescendedSelector = mkSelector "floorsDescended"

-- | @Selector@ for @currentPace@
currentPaceSelector :: Selector '[] RawId
currentPaceSelector = mkSelector "currentPace"

-- | @Selector@ for @currentCadence@
currentCadenceSelector :: Selector '[] RawId
currentCadenceSelector = mkSelector "currentCadence"

-- | @Selector@ for @averageActivePace@
averageActivePaceSelector :: Selector '[] RawId
averageActivePaceSelector = mkSelector "averageActivePace"

