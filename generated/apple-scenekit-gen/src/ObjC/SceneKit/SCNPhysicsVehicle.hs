{-# LANGUAGE DataKinds #-}
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
  , applyBrakingForce_forWheelAtIndexSelector
  , applyEngineForce_forWheelAtIndexSelector
  , chassisBodySelector
  , setSteeringAngle_forWheelAtIndexSelector
  , speedInKilometersPerHourSelector
  , vehicleWithChassisBody_wheelsSelector
  , wheelsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ vehicleWithChassisBody:wheels:@
vehicleWithChassisBody_wheels :: (IsSCNPhysicsBody chassisBody, IsNSArray wheels) => chassisBody -> wheels -> IO (Id SCNPhysicsVehicle)
vehicleWithChassisBody_wheels chassisBody wheels =
  do
    cls' <- getRequiredClass "SCNPhysicsVehicle"
    sendClassMessage cls' vehicleWithChassisBody_wheelsSelector (toSCNPhysicsBody chassisBody) (toNSArray wheels)

-- | @- applyEngineForce:forWheelAtIndex:@
applyEngineForce_forWheelAtIndex :: IsSCNPhysicsVehicle scnPhysicsVehicle => scnPhysicsVehicle -> CDouble -> CLong -> IO ()
applyEngineForce_forWheelAtIndex scnPhysicsVehicle value index =
  sendMessage scnPhysicsVehicle applyEngineForce_forWheelAtIndexSelector value index

-- | @- setSteeringAngle:forWheelAtIndex:@
setSteeringAngle_forWheelAtIndex :: IsSCNPhysicsVehicle scnPhysicsVehicle => scnPhysicsVehicle -> CDouble -> CLong -> IO ()
setSteeringAngle_forWheelAtIndex scnPhysicsVehicle value index =
  sendMessage scnPhysicsVehicle setSteeringAngle_forWheelAtIndexSelector value index

-- | @- applyBrakingForce:forWheelAtIndex:@
applyBrakingForce_forWheelAtIndex :: IsSCNPhysicsVehicle scnPhysicsVehicle => scnPhysicsVehicle -> CDouble -> CLong -> IO ()
applyBrakingForce_forWheelAtIndex scnPhysicsVehicle value index =
  sendMessage scnPhysicsVehicle applyBrakingForce_forWheelAtIndexSelector value index

-- | @- speedInKilometersPerHour@
speedInKilometersPerHour :: IsSCNPhysicsVehicle scnPhysicsVehicle => scnPhysicsVehicle -> IO CDouble
speedInKilometersPerHour scnPhysicsVehicle =
  sendMessage scnPhysicsVehicle speedInKilometersPerHourSelector

-- | @- wheels@
wheels :: IsSCNPhysicsVehicle scnPhysicsVehicle => scnPhysicsVehicle -> IO (Id NSArray)
wheels scnPhysicsVehicle =
  sendMessage scnPhysicsVehicle wheelsSelector

-- | @- chassisBody@
chassisBody :: IsSCNPhysicsVehicle scnPhysicsVehicle => scnPhysicsVehicle -> IO (Id SCNPhysicsBody)
chassisBody scnPhysicsVehicle =
  sendMessage scnPhysicsVehicle chassisBodySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vehicleWithChassisBody:wheels:@
vehicleWithChassisBody_wheelsSelector :: Selector '[Id SCNPhysicsBody, Id NSArray] (Id SCNPhysicsVehicle)
vehicleWithChassisBody_wheelsSelector = mkSelector "vehicleWithChassisBody:wheels:"

-- | @Selector@ for @applyEngineForce:forWheelAtIndex:@
applyEngineForce_forWheelAtIndexSelector :: Selector '[CDouble, CLong] ()
applyEngineForce_forWheelAtIndexSelector = mkSelector "applyEngineForce:forWheelAtIndex:"

-- | @Selector@ for @setSteeringAngle:forWheelAtIndex:@
setSteeringAngle_forWheelAtIndexSelector :: Selector '[CDouble, CLong] ()
setSteeringAngle_forWheelAtIndexSelector = mkSelector "setSteeringAngle:forWheelAtIndex:"

-- | @Selector@ for @applyBrakingForce:forWheelAtIndex:@
applyBrakingForce_forWheelAtIndexSelector :: Selector '[CDouble, CLong] ()
applyBrakingForce_forWheelAtIndexSelector = mkSelector "applyBrakingForce:forWheelAtIndex:"

-- | @Selector@ for @speedInKilometersPerHour@
speedInKilometersPerHourSelector :: Selector '[] CDouble
speedInKilometersPerHourSelector = mkSelector "speedInKilometersPerHour"

-- | @Selector@ for @wheels@
wheelsSelector :: Selector '[] (Id NSArray)
wheelsSelector = mkSelector "wheels"

-- | @Selector@ for @chassisBody@
chassisBodySelector :: Selector '[] (Id SCNPhysicsBody)
chassisBodySelector = mkSelector "chassisBody"

