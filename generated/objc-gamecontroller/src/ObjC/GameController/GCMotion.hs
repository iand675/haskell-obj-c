{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A profile for getting motion input from a controller that has the ability to measure acceleration and rotation rate.
--
-- You check for the availablity of motion inputs by getting the motion property of a controller. If that returns a nil value; motion is not available. A non-nil value is a valid GCMotion profile that is able to provide motion input.
--
-- See: GCController.motion
--
-- Generated bindings for @GCMotion@.
module ObjC.GameController.GCMotion
  ( GCMotion
  , IsGCMotion(..)
  , setAttitude
  , setStateFromMotion
  , controller
  , valueChangedHandler
  , setValueChangedHandler
  , sensorsRequireManualActivation
  , sensorsActive
  , setSensorsActive
  , hasGravityAndUserAcceleration
  , hasAttitudeAndRotationRate
  , hasAttitude
  , hasRotationRate
  , attitude
  , setAttitudeSelector
  , setStateFromMotionSelector
  , controllerSelector
  , valueChangedHandlerSelector
  , setValueChangedHandlerSelector
  , sensorsRequireManualActivationSelector
  , sensorsActiveSelector
  , setSensorsActiveSelector
  , hasGravityAndUserAccelerationSelector
  , hasAttitudeAndRotationRateSelector
  , hasAttitudeSelector
  , hasRotationRateSelector
  , attitudeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.GameController.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | Sets the current rotation rate of the controller.
--
-- Note: If the controller's snapshot flag is set to NO, this method has no effect.
--
-- See: attitude
--
-- ObjC selector: @- setAttitude:@
setAttitude :: IsGCMotion gcMotion => gcMotion -> GCQuaternion -> IO ()
setAttitude gcMotion  attitude =
  sendMsg gcMotion (mkSelector "setAttitude:") retVoid [argGCQuaternion attitude]

-- | Sets the state vector of the motion profile to a copy of the input motion profile's state vector.
--
-- Note: If the controller's snapshot flag is set to NO, this method has no effect.
--
-- See: GCController.snapshot
--
-- ObjC selector: @- setStateFromMotion:@
setStateFromMotion :: (IsGCMotion gcMotion, IsGCMotion motion) => gcMotion -> motion -> IO ()
setStateFromMotion gcMotion  motion =
withObjCPtr motion $ \raw_motion ->
    sendMsg gcMotion (mkSelector "setStateFromMotion:") retVoid [argPtr (castPtr raw_motion :: Ptr ())]

-- | A profile keeps a reference to the controller that it is mapping input from.
--
-- See: GCController
--
-- ObjC selector: @- controller@
controller :: IsGCMotion gcMotion => gcMotion -> IO (Id GCController)
controller gcMotion  =
  sendMsg gcMotion (mkSelector "controller") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- valueChangedHandler@
valueChangedHandler :: IsGCMotion gcMotion => gcMotion -> IO (Ptr ())
valueChangedHandler gcMotion  =
  fmap castPtr $ sendMsg gcMotion (mkSelector "valueChangedHandler") (retPtr retVoid) []

-- | @- setValueChangedHandler:@
setValueChangedHandler :: IsGCMotion gcMotion => gcMotion -> Ptr () -> IO ()
setValueChangedHandler gcMotion  value =
  sendMsg gcMotion (mkSelector "setValueChangedHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | If this property is returns YES, you are responsible for setting sensorsActive to YES when you need motion data from the controller.
--
-- Some controllers, such as the Siri Remote, automatically activate and deactivate motion sensors. In such a case, this property will return NO.
--
-- See: sensorsActive
--
-- ObjC selector: @- sensorsRequireManualActivation@
sensorsRequireManualActivation :: IsGCMotion gcMotion => gcMotion -> IO Bool
sensorsRequireManualActivation gcMotion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcMotion (mkSelector "sensorsRequireManualActivation") retCULong []

-- | Set this property to YES when you wish to receive motion data from the controller. When you set this property to NO, the motion sensors will be disabled and the GCMotion profile will not be updated.
--
-- Note: It is highly recommended that you only enable sensor during the period of time you directly need motion data. Motion sensors can drain controller battery, device battery, and needlessly consume Bluetooth bandwidth.
--
-- See: sensorsRequireManualActivation
--
-- ObjC selector: @- sensorsActive@
sensorsActive :: IsGCMotion gcMotion => gcMotion -> IO Bool
sensorsActive gcMotion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcMotion (mkSelector "sensorsActive") retCULong []

-- | Set this property to YES when you wish to receive motion data from the controller. When you set this property to NO, the motion sensors will be disabled and the GCMotion profile will not be updated.
--
-- Note: It is highly recommended that you only enable sensor during the period of time you directly need motion data. Motion sensors can drain controller battery, device battery, and needlessly consume Bluetooth bandwidth.
--
-- See: sensorsRequireManualActivation
--
-- ObjC selector: @- setSensorsActive:@
setSensorsActive :: IsGCMotion gcMotion => gcMotion -> Bool -> IO ()
setSensorsActive gcMotion  value =
  sendMsg gcMotion (mkSelector "setSensorsActive:") retVoid [argCULong (if value then 1 else 0)]

-- | Returns YES if the controller is capable of reporting gravity and user acceleration separately.
--
-- Note: Some controllers do not separate gravity from user acceleration, and only report the total acceleration of the controller. Query whether the connected controller has the ability to separate gravity and user acceleration, and it doesn’t, use acceleration instead.
--
-- See: acceleration
--
-- ObjC selector: @- hasGravityAndUserAcceleration@
hasGravityAndUserAcceleration :: IsGCMotion gcMotion => gcMotion -> IO Bool
hasGravityAndUserAcceleration gcMotion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcMotion (mkSelector "hasGravityAndUserAcceleration") retCULong []

-- | The controller generating the motion data has sensors that can accurately determine the current attitude and rotation rate. If this is enabled the motion data for attitude and rotation rate are usable for inputs.
--
-- ObjC selector: @- hasAttitudeAndRotationRate@
hasAttitudeAndRotationRate :: IsGCMotion gcMotion => gcMotion -> IO Bool
hasAttitudeAndRotationRate gcMotion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcMotion (mkSelector "hasAttitudeAndRotationRate") retCULong []

-- | The controller generating the motion data has sensors that can accurately determine the current attitude. If this is enabled the motion data for attitude is usable for inputs.
--
-- ObjC selector: @- hasAttitude@
hasAttitude :: IsGCMotion gcMotion => gcMotion -> IO Bool
hasAttitude gcMotion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcMotion (mkSelector "hasAttitude") retCULong []

-- | The controller generating the motion data has sensors that can accurately determine the current rotation rate. If this is enabled the motion data for rotation rate is usable for inputs.
--
-- ObjC selector: @- hasRotationRate@
hasRotationRate :: IsGCMotion gcMotion => gcMotion -> IO Bool
hasRotationRate gcMotion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcMotion (mkSelector "hasRotationRate") retCULong []

-- | The current attitude of the controller.
--
-- Note: Remotes without accurate attitude and rotation rate can not determine a stable attitude so the values will be (0,0,0,1) at all times.
--
-- See: hasAttitude
--
-- See: GCMicroGamepad
--
-- ObjC selector: @- attitude@
attitude :: IsGCMotion gcMotion => gcMotion -> IO GCQuaternion
attitude gcMotion  =
  sendMsgStret gcMotion (mkSelector "attitude") retGCQuaternion []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setAttitude:@
setAttitudeSelector :: Selector
setAttitudeSelector = mkSelector "setAttitude:"

-- | @Selector@ for @setStateFromMotion:@
setStateFromMotionSelector :: Selector
setStateFromMotionSelector = mkSelector "setStateFromMotion:"

-- | @Selector@ for @controller@
controllerSelector :: Selector
controllerSelector = mkSelector "controller"

-- | @Selector@ for @valueChangedHandler@
valueChangedHandlerSelector :: Selector
valueChangedHandlerSelector = mkSelector "valueChangedHandler"

-- | @Selector@ for @setValueChangedHandler:@
setValueChangedHandlerSelector :: Selector
setValueChangedHandlerSelector = mkSelector "setValueChangedHandler:"

-- | @Selector@ for @sensorsRequireManualActivation@
sensorsRequireManualActivationSelector :: Selector
sensorsRequireManualActivationSelector = mkSelector "sensorsRequireManualActivation"

-- | @Selector@ for @sensorsActive@
sensorsActiveSelector :: Selector
sensorsActiveSelector = mkSelector "sensorsActive"

-- | @Selector@ for @setSensorsActive:@
setSensorsActiveSelector :: Selector
setSensorsActiveSelector = mkSelector "setSensorsActive:"

-- | @Selector@ for @hasGravityAndUserAcceleration@
hasGravityAndUserAccelerationSelector :: Selector
hasGravityAndUserAccelerationSelector = mkSelector "hasGravityAndUserAcceleration"

-- | @Selector@ for @hasAttitudeAndRotationRate@
hasAttitudeAndRotationRateSelector :: Selector
hasAttitudeAndRotationRateSelector = mkSelector "hasAttitudeAndRotationRate"

-- | @Selector@ for @hasAttitude@
hasAttitudeSelector :: Selector
hasAttitudeSelector = mkSelector "hasAttitude"

-- | @Selector@ for @hasRotationRate@
hasRotationRateSelector :: Selector
hasRotationRateSelector = mkSelector "hasRotationRate"

-- | @Selector@ for @attitude@
attitudeSelector :: Selector
attitudeSelector = mkSelector "attitude"

