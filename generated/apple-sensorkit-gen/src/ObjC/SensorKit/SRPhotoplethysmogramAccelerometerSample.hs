{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRPhotoplethysmogramAccelerometerSample@.
module ObjC.SensorKit.SRPhotoplethysmogramAccelerometerSample
  ( SRPhotoplethysmogramAccelerometerSample
  , IsSRPhotoplethysmogramAccelerometerSample(..)
  , init_
  , new
  , nanosecondsSinceStart
  , samplingFrequency
  , x
  , y
  , z
  , initSelector
  , nanosecondsSinceStartSelector
  , newSelector
  , samplingFrequencySelector
  , xSelector
  , ySelector
  , zSelector


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
init_ :: IsSRPhotoplethysmogramAccelerometerSample srPhotoplethysmogramAccelerometerSample => srPhotoplethysmogramAccelerometerSample -> IO (Id SRPhotoplethysmogramAccelerometerSample)
init_ srPhotoplethysmogramAccelerometerSample =
  sendOwnedMessage srPhotoplethysmogramAccelerometerSample initSelector

-- | @+ new@
new :: IO (Id SRPhotoplethysmogramAccelerometerSample)
new  =
  do
    cls' <- getRequiredClass "SRPhotoplethysmogramAccelerometerSample"
    sendOwnedClassMessage cls' newSelector

-- | nanosecondsSinceStart
--
-- nanoseconds since the @SRPhotoplethysmogramSample@ start date of the specific  accelerometer sample
--
-- ObjC selector: @- nanosecondsSinceStart@
nanosecondsSinceStart :: IsSRPhotoplethysmogramAccelerometerSample srPhotoplethysmogramAccelerometerSample => srPhotoplethysmogramAccelerometerSample -> IO CLong
nanosecondsSinceStart srPhotoplethysmogramAccelerometerSample =
  sendMessage srPhotoplethysmogramAccelerometerSample nanosecondsSinceStartSelector

-- | samplingFrequency
--
-- Sampling frequency of accelerometer data in Hz
--
-- ObjC selector: @- samplingFrequency@
samplingFrequency :: IsSRPhotoplethysmogramAccelerometerSample srPhotoplethysmogramAccelerometerSample => srPhotoplethysmogramAccelerometerSample -> IO (Id NSMeasurement)
samplingFrequency srPhotoplethysmogramAccelerometerSample =
  sendMessage srPhotoplethysmogramAccelerometerSample samplingFrequencySelector

-- | x
--
-- X-axis acceleration in G's
--
-- ObjC selector: @- x@
x :: IsSRPhotoplethysmogramAccelerometerSample srPhotoplethysmogramAccelerometerSample => srPhotoplethysmogramAccelerometerSample -> IO (Id NSMeasurement)
x srPhotoplethysmogramAccelerometerSample =
  sendMessage srPhotoplethysmogramAccelerometerSample xSelector

-- | y
--
-- Y-axis acceleration in G's
--
-- ObjC selector: @- y@
y :: IsSRPhotoplethysmogramAccelerometerSample srPhotoplethysmogramAccelerometerSample => srPhotoplethysmogramAccelerometerSample -> IO (Id NSMeasurement)
y srPhotoplethysmogramAccelerometerSample =
  sendMessage srPhotoplethysmogramAccelerometerSample ySelector

-- | z
--
-- Z-axis acceleration in G's
--
-- ObjC selector: @- z@
z :: IsSRPhotoplethysmogramAccelerometerSample srPhotoplethysmogramAccelerometerSample => srPhotoplethysmogramAccelerometerSample -> IO (Id NSMeasurement)
z srPhotoplethysmogramAccelerometerSample =
  sendMessage srPhotoplethysmogramAccelerometerSample zSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRPhotoplethysmogramAccelerometerSample)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRPhotoplethysmogramAccelerometerSample)
newSelector = mkSelector "new"

-- | @Selector@ for @nanosecondsSinceStart@
nanosecondsSinceStartSelector :: Selector '[] CLong
nanosecondsSinceStartSelector = mkSelector "nanosecondsSinceStart"

-- | @Selector@ for @samplingFrequency@
samplingFrequencySelector :: Selector '[] (Id NSMeasurement)
samplingFrequencySelector = mkSelector "samplingFrequency"

-- | @Selector@ for @x@
xSelector :: Selector '[] (Id NSMeasurement)
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector '[] (Id NSMeasurement)
ySelector = mkSelector "y"

-- | @Selector@ for @z@
zSelector :: Selector '[] (Id NSMeasurement)
zSelector = mkSelector "z"

