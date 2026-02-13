{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRPhotoplethysmogramSample@.
module ObjC.SensorKit.SRPhotoplethysmogramSample
  ( SRPhotoplethysmogramSample
  , IsSRPhotoplethysmogramSample(..)
  , init_
  , new
  , startDate
  , nanosecondsSinceStart
  , usage
  , opticalSamples
  , accelerometerSamples
  , temperature
  , accelerometerSamplesSelector
  , initSelector
  , nanosecondsSinceStartSelector
  , newSelector
  , opticalSamplesSelector
  , startDateSelector
  , temperatureSelector
  , usageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO (Id SRPhotoplethysmogramSample)
init_ srPhotoplethysmogramSample =
  sendOwnedMessage srPhotoplethysmogramSample initSelector

-- | @+ new@
new :: IO (Id SRPhotoplethysmogramSample)
new  =
  do
    cls' <- getRequiredClass "SRPhotoplethysmogramSample"
    sendOwnedClassMessage cls' newSelector

-- | startDate
--
-- the start date of a data collection session
--
-- ObjC selector: @- startDate@
startDate :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO (Id NSDate)
startDate srPhotoplethysmogramSample =
  sendMessage srPhotoplethysmogramSample startDateSelector

-- | nanosecondsSinceStart
--
-- nanoseconds since the start date of this specific sample
--
-- ObjC selector: @- nanosecondsSinceStart@
nanosecondsSinceStart :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO CLong
nanosecondsSinceStart srPhotoplethysmogramSample =
  sendMessage srPhotoplethysmogramSample nanosecondsSinceStartSelector

-- | usage
--
-- How the sensor was being used during the sample reading
--
-- It is possible for these to occur in combination
--
-- ObjC selector: @- usage@
usage :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO (Id NSArray)
usage srPhotoplethysmogramSample =
  sendMessage srPhotoplethysmogramSample usageSelector

-- | @- opticalSamples@
opticalSamples :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO (Id NSArray)
opticalSamples srPhotoplethysmogramSample =
  sendMessage srPhotoplethysmogramSample opticalSamplesSelector

-- | @- accelerometerSamples@
accelerometerSamples :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO (Id NSArray)
accelerometerSamples srPhotoplethysmogramSample =
  sendMessage srPhotoplethysmogramSample accelerometerSamplesSelector

-- | temperature
--
-- temperature of the PPG sensors in the watch, measured in celsius
--
-- This may be @nil@ when the sensor data reading is invalid or if is not supported by the hardware
--
-- ObjC selector: @- temperature@
temperature :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO (Id NSMeasurement)
temperature srPhotoplethysmogramSample =
  sendMessage srPhotoplethysmogramSample temperatureSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRPhotoplethysmogramSample)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRPhotoplethysmogramSample)
newSelector = mkSelector "new"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @nanosecondsSinceStart@
nanosecondsSinceStartSelector :: Selector '[] CLong
nanosecondsSinceStartSelector = mkSelector "nanosecondsSinceStart"

-- | @Selector@ for @usage@
usageSelector :: Selector '[] (Id NSArray)
usageSelector = mkSelector "usage"

-- | @Selector@ for @opticalSamples@
opticalSamplesSelector :: Selector '[] (Id NSArray)
opticalSamplesSelector = mkSelector "opticalSamples"

-- | @Selector@ for @accelerometerSamples@
accelerometerSamplesSelector :: Selector '[] (Id NSArray)
accelerometerSamplesSelector = mkSelector "accelerometerSamples"

-- | @Selector@ for @temperature@
temperatureSelector :: Selector '[] (Id NSMeasurement)
temperatureSelector = mkSelector "temperature"

