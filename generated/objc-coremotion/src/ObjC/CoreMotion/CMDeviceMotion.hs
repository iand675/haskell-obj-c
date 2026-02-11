{-# LANGUAGE PatternSynonyms #-}
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

-- | @- attitude@
attitude :: IsCMDeviceMotion cmDeviceMotion => cmDeviceMotion -> IO (Id CMAttitude)
attitude cmDeviceMotion  =
  sendMsg cmDeviceMotion (mkSelector "attitude") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- heading@
heading :: IsCMDeviceMotion cmDeviceMotion => cmDeviceMotion -> IO CDouble
heading cmDeviceMotion  =
  sendMsg cmDeviceMotion (mkSelector "heading") retCDouble []

-- | @- sensorLocation@
sensorLocation :: IsCMDeviceMotion cmDeviceMotion => cmDeviceMotion -> IO CMDeviceMotionSensorLocation
sensorLocation cmDeviceMotion  =
  fmap (coerce :: CLong -> CMDeviceMotionSensorLocation) $ sendMsg cmDeviceMotion (mkSelector "sensorLocation") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attitude@
attitudeSelector :: Selector
attitudeSelector = mkSelector "attitude"

-- | @Selector@ for @heading@
headingSelector :: Selector
headingSelector = mkSelector "heading"

-- | @Selector@ for @sensorLocation@
sensorLocationSelector :: Selector
sensorLocationSelector = mkSelector "sensorLocation"

