{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMDeviceMotion@.
module ObjC.CoreMotion.CMDeviceMotion
  ( CMDeviceMotion
  , IsCMDeviceMotion(..)
  , attitude
  , heading
  , sensorLocation
  , attitudeSelector
  , headingSelector
  , sensorLocationSelector

  -- * Enum types
  , CMDeviceMotionSensorLocation(CMDeviceMotionSensorLocation)
  , pattern CMDeviceMotionSensorLocationDefault
  , pattern CMDeviceMotionSensorLocationHeadphoneLeft
  , pattern CMDeviceMotionSensorLocationHeadphoneRight

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

-- | @- attitude@
attitude :: IsCMDeviceMotion cmDeviceMotion => cmDeviceMotion -> IO (Id CMAttitude)
attitude cmDeviceMotion =
  sendMessage cmDeviceMotion attitudeSelector

-- | @- heading@
heading :: IsCMDeviceMotion cmDeviceMotion => cmDeviceMotion -> IO CDouble
heading cmDeviceMotion =
  sendMessage cmDeviceMotion headingSelector

-- | @- sensorLocation@
sensorLocation :: IsCMDeviceMotion cmDeviceMotion => cmDeviceMotion -> IO CMDeviceMotionSensorLocation
sensorLocation cmDeviceMotion =
  sendMessage cmDeviceMotion sensorLocationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attitude@
attitudeSelector :: Selector '[] (Id CMAttitude)
attitudeSelector = mkSelector "attitude"

-- | @Selector@ for @heading@
headingSelector :: Selector '[] CDouble
headingSelector = mkSelector "heading"

-- | @Selector@ for @sensorLocation@
sensorLocationSelector :: Selector '[] CMDeviceMotionSensorLocation
sensorLocationSelector = mkSelector "sensorLocation"

