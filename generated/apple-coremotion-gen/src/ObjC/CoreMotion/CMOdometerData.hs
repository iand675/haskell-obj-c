{-# LANGUAGE PatternSynonyms #-}
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
  , startDateSelector
  , endDateSelector
  , deltaDistanceSelector
  , deltaDistanceAccuracySelector
  , speedSelector
  , speedAccuracySelector
  , gpsDateSelector
  , deltaAltitudeSelector
  , verticalAccuracySelector
  , originDeviceSelector
  , slopeSelector
  , maxAbsSlopeSelector

  -- * Enum types
  , CMOdometerOriginDevice(CMOdometerOriginDevice)
  , pattern CMOdometerOriginDeviceUnknown
  , pattern CMOdometerOriginDeviceLocal
  , pattern CMOdometerOriginDeviceRemote

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

-- | @- startDate@
startDate :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO (Id NSDate)
startDate cmOdometerData  =
    sendMsg cmOdometerData (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- endDate@
endDate :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO (Id NSDate)
endDate cmOdometerData  =
    sendMsg cmOdometerData (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deltaDistance@
deltaDistance :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CDouble
deltaDistance cmOdometerData  =
    sendMsg cmOdometerData (mkSelector "deltaDistance") retCDouble []

-- | @- deltaDistanceAccuracy@
deltaDistanceAccuracy :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CDouble
deltaDistanceAccuracy cmOdometerData  =
    sendMsg cmOdometerData (mkSelector "deltaDistanceAccuracy") retCDouble []

-- | @- speed@
speed :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CDouble
speed cmOdometerData  =
    sendMsg cmOdometerData (mkSelector "speed") retCDouble []

-- | @- speedAccuracy@
speedAccuracy :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CDouble
speedAccuracy cmOdometerData  =
    sendMsg cmOdometerData (mkSelector "speedAccuracy") retCDouble []

-- | @- gpsDate@
gpsDate :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO (Id NSDate)
gpsDate cmOdometerData  =
    sendMsg cmOdometerData (mkSelector "gpsDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deltaAltitude@
deltaAltitude :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CDouble
deltaAltitude cmOdometerData  =
    sendMsg cmOdometerData (mkSelector "deltaAltitude") retCDouble []

-- | @- verticalAccuracy@
verticalAccuracy :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CDouble
verticalAccuracy cmOdometerData  =
    sendMsg cmOdometerData (mkSelector "verticalAccuracy") retCDouble []

-- | @- originDevice@
originDevice :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO CMOdometerOriginDevice
originDevice cmOdometerData  =
    fmap (coerce :: CLong -> CMOdometerOriginDevice) $ sendMsg cmOdometerData (mkSelector "originDevice") retCLong []

-- | @- slope@
slope :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO (Id NSNumber)
slope cmOdometerData  =
    sendMsg cmOdometerData (mkSelector "slope") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- maxAbsSlope@
maxAbsSlope :: IsCMOdometerData cmOdometerData => cmOdometerData -> IO (Id NSNumber)
maxAbsSlope cmOdometerData  =
    sendMsg cmOdometerData (mkSelector "maxAbsSlope") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @deltaDistance@
deltaDistanceSelector :: Selector
deltaDistanceSelector = mkSelector "deltaDistance"

-- | @Selector@ for @deltaDistanceAccuracy@
deltaDistanceAccuracySelector :: Selector
deltaDistanceAccuracySelector = mkSelector "deltaDistanceAccuracy"

-- | @Selector@ for @speed@
speedSelector :: Selector
speedSelector = mkSelector "speed"

-- | @Selector@ for @speedAccuracy@
speedAccuracySelector :: Selector
speedAccuracySelector = mkSelector "speedAccuracy"

-- | @Selector@ for @gpsDate@
gpsDateSelector :: Selector
gpsDateSelector = mkSelector "gpsDate"

-- | @Selector@ for @deltaAltitude@
deltaAltitudeSelector :: Selector
deltaAltitudeSelector = mkSelector "deltaAltitude"

-- | @Selector@ for @verticalAccuracy@
verticalAccuracySelector :: Selector
verticalAccuracySelector = mkSelector "verticalAccuracy"

-- | @Selector@ for @originDevice@
originDeviceSelector :: Selector
originDeviceSelector = mkSelector "originDevice"

-- | @Selector@ for @slope@
slopeSelector :: Selector
slopeSelector = mkSelector "slope"

-- | @Selector@ for @maxAbsSlope@
maxAbsSlopeSelector :: Selector
maxAbsSlopeSelector = mkSelector "maxAbsSlope"

