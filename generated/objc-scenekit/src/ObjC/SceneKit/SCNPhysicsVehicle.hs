{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPhysicsVehicle
--
-- SCNPhysicsVehicle provides a vehicle behavior.
--
-- Generated bindings for @SCNPhysicsVehicle@.
module ObjC.SceneKit.SCNPhysicsVehicle
  ( SCNPhysicsVehicle
  , IsSCNPhysicsVehicle(..)
  , vehicleWithChassisBody_wheels
  , applyEngineForce_forWheelAtIndex
  , setSteeringAngle_forWheelAtIndex
  , applyBrakingForce_forWheelAtIndex
  , speedInKilometersPerHour
  , wheels
  , chassisBody
  , vehicleWithChassisBody_wheelsSelector
  , applyEngineForce_forWheelAtIndexSelector
  , setSteeringAngle_forWheelAtIndexSelector
  , applyBrakingForce_forWheelAtIndexSelector
  , speedInKilometersPerHourSelector
  , wheelsSelector
  , chassisBodySelector


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

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ vehicleWithChassisBody:wheels:@
vehicleWithChassisBody_wheels :: (IsSCNPhysicsBody chassisBody, IsNSArray wheels) => chassisBody -> wheels -> IO (Id SCNPhysicsVehicle)
vehicleWithChassisBody_wheels chassisBody wheels =
  do
    cls' <- getRequiredClass "SCNPhysicsVehicle"
    withObjCPtr chassisBody $ \raw_chassisBody ->
      withObjCPtr wheels $ \raw_wheels ->
        sendClassMsg cls' (mkSelector "vehicleWithChassisBody:wheels:") (retPtr retVoid) [argPtr (castPtr raw_chassisBody :: Ptr ()), argPtr (castPtr raw_wheels :: Ptr ())] >>= retainedObject . castPtr

-- | @- applyEngineForce:forWheelAtIndex:@
applyEngineForce_forWheelAtIndex :: IsSCNPhysicsVehicle scnPhysicsVehicle => scnPhysicsVehicle -> CDouble -> CLong -> IO ()
applyEngineForce_forWheelAtIndex scnPhysicsVehicle  value index =
  sendMsg scnPhysicsVehicle (mkSelector "applyEngineForce:forWheelAtIndex:") retVoid [argCDouble (fromIntegral value), argCLong (fromIntegral index)]

-- | @- setSteeringAngle:forWheelAtIndex:@
setSteeringAngle_forWheelAtIndex :: IsSCNPhysicsVehicle scnPhysicsVehicle => scnPhysicsVehicle -> CDouble -> CLong -> IO ()
setSteeringAngle_forWheelAtIndex scnPhysicsVehicle  value index =
  sendMsg scnPhysicsVehicle (mkSelector "setSteeringAngle:forWheelAtIndex:") retVoid [argCDouble (fromIntegral value), argCLong (fromIntegral index)]

-- | @- applyBrakingForce:forWheelAtIndex:@
applyBrakingForce_forWheelAtIndex :: IsSCNPhysicsVehicle scnPhysicsVehicle => scnPhysicsVehicle -> CDouble -> CLong -> IO ()
applyBrakingForce_forWheelAtIndex scnPhysicsVehicle  value index =
  sendMsg scnPhysicsVehicle (mkSelector "applyBrakingForce:forWheelAtIndex:") retVoid [argCDouble (fromIntegral value), argCLong (fromIntegral index)]

-- | @- speedInKilometersPerHour@
speedInKilometersPerHour :: IsSCNPhysicsVehicle scnPhysicsVehicle => scnPhysicsVehicle -> IO CDouble
speedInKilometersPerHour scnPhysicsVehicle  =
  sendMsg scnPhysicsVehicle (mkSelector "speedInKilometersPerHour") retCDouble []

-- | @- wheels@
wheels :: IsSCNPhysicsVehicle scnPhysicsVehicle => scnPhysicsVehicle -> IO (Id NSArray)
wheels scnPhysicsVehicle  =
  sendMsg scnPhysicsVehicle (mkSelector "wheels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- chassisBody@
chassisBody :: IsSCNPhysicsVehicle scnPhysicsVehicle => scnPhysicsVehicle -> IO (Id SCNPhysicsBody)
chassisBody scnPhysicsVehicle  =
  sendMsg scnPhysicsVehicle (mkSelector "chassisBody") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vehicleWithChassisBody:wheels:@
vehicleWithChassisBody_wheelsSelector :: Selector
vehicleWithChassisBody_wheelsSelector = mkSelector "vehicleWithChassisBody:wheels:"

-- | @Selector@ for @applyEngineForce:forWheelAtIndex:@
applyEngineForce_forWheelAtIndexSelector :: Selector
applyEngineForce_forWheelAtIndexSelector = mkSelector "applyEngineForce:forWheelAtIndex:"

-- | @Selector@ for @setSteeringAngle:forWheelAtIndex:@
setSteeringAngle_forWheelAtIndexSelector :: Selector
setSteeringAngle_forWheelAtIndexSelector = mkSelector "setSteeringAngle:forWheelAtIndex:"

-- | @Selector@ for @applyBrakingForce:forWheelAtIndex:@
applyBrakingForce_forWheelAtIndexSelector :: Selector
applyBrakingForce_forWheelAtIndexSelector = mkSelector "applyBrakingForce:forWheelAtIndex:"

-- | @Selector@ for @speedInKilometersPerHour@
speedInKilometersPerHourSelector :: Selector
speedInKilometersPerHourSelector = mkSelector "speedInKilometersPerHour"

-- | @Selector@ for @wheels@
wheelsSelector :: Selector
wheelsSelector = mkSelector "wheels"

-- | @Selector@ for @chassisBody@
chassisBodySelector :: Selector
chassisBodySelector = mkSelector "chassisBody"

