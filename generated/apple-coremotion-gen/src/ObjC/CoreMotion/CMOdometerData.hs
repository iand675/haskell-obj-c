{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMOdometerData@.
module ObjC.CoreMotion.CMOdometerData
  ( CMOdometerData
  , IsCMOdometerData(..)
  , startDate
  , endDate
  , deltaDistance
  , deltaDistanceAccuracy
  , speed
  , speedAccuracy
  , gpsDate
  , deltaAltitude
  , verticalAccuracy
  , originDevice
  , slope
  , maxAbsSlope
  , deltaAltitudeSelector
  , deltaDistanceAccuracySelector
  , deltaDistanceSelector
  , endDateSelector
  , gpsDateSelector
  , maxAbsSlopeSelector
  , originDeviceSelector
  , slopeSelector
  , speedAccuracySelector
  , speedSelector
  , startDateSelector
  , verticalAccuracySelector

  -- * Enum types
  , CMOdometerOriginDevice(CMOdometerOriginDevice)
  , pattern CMOdometerOriginDeviceUnknown
  , pattern CMOdometerOriginDeviceLocal
  , pattern CMOdometerOriginDeviceRemote

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

-- | @- startDate@
startDate :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO (Id NSDate)
startDate cmOdometerData =
  sendMessage cmOdometerData startDateSelector

-- | @- endDate@
endDate :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO (Id NSDate)
endDate cmOdometerData =
  sendMessage cmOdometerData endDateSelector

-- | @- deltaDistance@
deltaDistance :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CDouble
deltaDistance cmOdometerData =
  sendMessage cmOdometerData deltaDistanceSelector

-- | @- deltaDistanceAccuracy@
deltaDistanceAccuracy :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CDouble
deltaDistanceAccuracy cmOdometerData =
  sendMessage cmOdometerData deltaDistanceAccuracySelector

-- | @- speed@
speed :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CDouble
speed cmOdometerData =
  sendMessage cmOdometerData speedSelector

-- | @- speedAccuracy@
speedAccuracy :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CDouble
speedAccuracy cmOdometerData =
  sendMessage cmOdometerData speedAccuracySelector

-- | @- gpsDate@
gpsDate :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO (Id NSDate)
gpsDate cmOdometerData =
  sendMessage cmOdometerData gpsDateSelector

-- | @- deltaAltitude@
deltaAltitude :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CDouble
deltaAltitude cmOdometerData =
  sendMessage cmOdometerData deltaAltitudeSelector

-- | @- verticalAccuracy@
verticalAccuracy :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CDouble
verticalAccuracy cmOdometerData =
  sendMessage cmOdometerData verticalAccuracySelector

-- | @- originDevice@
originDevice :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CMOdometerOriginDevice
originDevice cmOdometerData =
  sendMessage cmOdometerData originDeviceSelector

-- | @- slope@
slope :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO (Id NSNumber)
slope cmOdometerData =
  sendMessage cmOdometerData slopeSelector

-- | @- maxAbsSlope@
maxAbsSlope :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO (Id NSNumber)
maxAbsSlope cmOdometerData =
  sendMessage cmOdometerData maxAbsSlopeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @deltaDistance@
deltaDistanceSelector :: Selector '[] CDouble
deltaDistanceSelector = mkSelector "deltaDistance"

-- | @Selector@ for @deltaDistanceAccuracy@
deltaDistanceAccuracySelector :: Selector '[] CDouble
deltaDistanceAccuracySelector = mkSelector "deltaDistanceAccuracy"

-- | @Selector@ for @speed@
speedSelector :: Selector '[] CDouble
speedSelector = mkSelector "speed"

-- | @Selector@ for @speedAccuracy@
speedAccuracySelector :: Selector '[] CDouble
speedAccuracySelector = mkSelector "speedAccuracy"

-- | @Selector@ for @gpsDate@
gpsDateSelector :: Selector '[] (Id NSDate)
gpsDateSelector = mkSelector "gpsDate"

-- | @Selector@ for @deltaAltitude@
deltaAltitudeSelector :: Selector '[] CDouble
deltaAltitudeSelector = mkSelector "deltaAltitude"

-- | @Selector@ for @verticalAccuracy@
verticalAccuracySelector :: Selector '[] CDouble
verticalAccuracySelector = mkSelector "verticalAccuracy"

-- | @Selector@ for @originDevice@
originDeviceSelector :: Selector '[] CMOdometerOriginDevice
originDeviceSelector = mkSelector "originDevice"

-- | @Selector@ for @slope@
slopeSelector :: Selector '[] (Id NSNumber)
slopeSelector = mkSelector "slope"

-- | @Selector@ for @maxAbsSlope@
maxAbsSlopeSelector :: Selector '[] (Id NSNumber)
maxAbsSlopeSelector = mkSelector "maxAbsSlope"

