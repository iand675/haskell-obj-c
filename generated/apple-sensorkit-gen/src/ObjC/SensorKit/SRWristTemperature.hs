{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRWristTemperature@.
module ObjC.SensorKit.SRWristTemperature
  ( SRWristTemperature
  , IsSRWristTemperature(..)
  , init_
  , new
  , timestamp
  , value
  , condition
  , errorEstimate
  , conditionSelector
  , errorEstimateSelector
  , initSelector
  , newSelector
  , timestampSelector
  , valueSelector

  -- * Enum types
  , SRWristTemperatureCondition(SRWristTemperatureCondition)
  , pattern SRWristTemperatureConditionNone
  , pattern SRWristTemperatureConditionOffWrist
  , pattern SRWristTemperatureConditionOnCharger
  , pattern SRWristTemperatureConditionInMotion

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRWristTemperature srWristTemperature => srWristTemperature -> IO (Id SRWristTemperature)
init_ srWristTemperature =
  sendOwnedMessage srWristTemperature initSelector

-- | @+ new@
new :: IO (Id SRWristTemperature)
new  =
  do
    cls' <- getRequiredClass "SRWristTemperature"
    sendOwnedClassMessage cls' newSelector

-- | timestamp
--
-- Timestamp of when temperature measurement was taken.
--
-- ObjC selector: @- timestamp@
timestamp :: IsSRWristTemperature srWristTemperature => srWristTemperature -> IO (Id NSDate)
timestamp srWristTemperature =
  sendMessage srWristTemperature timestampSelector

-- | value
--
-- Temperature sensor value in celsius
--
-- ObjC selector: @- value@
value :: IsSRWristTemperature srWristTemperature => srWristTemperature -> IO (Id NSMeasurement)
value srWristTemperature =
  sendMessage srWristTemperature valueSelector

-- | condition
--
-- Indicate system conditions that may impact the temperature sample.
--
-- ObjC selector: @- condition@
condition :: IsSRWristTemperature srWristTemperature => srWristTemperature -> IO SRWristTemperatureCondition
condition srWristTemperature =
  sendMessage srWristTemperature conditionSelector

-- | errorEstimate
--
-- Estimated temperature error per sample.        Error could be in either positive or negative direction.
--
-- ObjC selector: @- errorEstimate@
errorEstimate :: IsSRWristTemperature srWristTemperature => srWristTemperature -> IO (Id NSMeasurement)
errorEstimate srWristTemperature =
  sendMessage srWristTemperature errorEstimateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRWristTemperature)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRWristTemperature)
newSelector = mkSelector "new"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector '[] (Id NSDate)
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSMeasurement)
valueSelector = mkSelector "value"

-- | @Selector@ for @condition@
conditionSelector :: Selector '[] SRWristTemperatureCondition
conditionSelector = mkSelector "condition"

-- | @Selector@ for @errorEstimate@
errorEstimateSelector :: Selector '[] (Id NSMeasurement)
errorEstimateSelector = mkSelector "errorEstimate"

