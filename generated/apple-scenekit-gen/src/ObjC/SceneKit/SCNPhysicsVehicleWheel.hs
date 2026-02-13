{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPhysicsVehicleWheel
--
-- SCNPhysicsVehicleWheel represents a wheel that can be attached to a SCNPhysicsVehicle instance.
--
-- Generated bindings for @SCNPhysicsVehicleWheel@.
module ObjC.SceneKit.SCNPhysicsVehicleWheel
  ( SCNPhysicsVehicleWheel
  , IsSCNPhysicsVehicleWheel(..)
  , wheelWithNode
  , node
  , suspensionStiffness
  , setSuspensionStiffness
  , suspensionCompression
  , setSuspensionCompression
  , suspensionDamping
  , setSuspensionDamping
  , maximumSuspensionTravel
  , setMaximumSuspensionTravel
  , frictionSlip
  , setFrictionSlip
  , maximumSuspensionForce
  , setMaximumSuspensionForce
  , connectionPosition
  , setConnectionPosition
  , steeringAxis
  , setSteeringAxis
  , axle
  , setAxle
  , radius
  , setRadius
  , suspensionRestLength
  , setSuspensionRestLength
  , axleSelector
  , connectionPositionSelector
  , frictionSlipSelector
  , maximumSuspensionForceSelector
  , maximumSuspensionTravelSelector
  , nodeSelector
  , radiusSelector
  , setAxleSelector
  , setConnectionPositionSelector
  , setFrictionSlipSelector
  , setMaximumSuspensionForceSelector
  , setMaximumSuspensionTravelSelector
  , setRadiusSelector
  , setSteeringAxisSelector
  , setSuspensionCompressionSelector
  , setSuspensionDampingSelector
  , setSuspensionRestLengthSelector
  , setSuspensionStiffnessSelector
  , steeringAxisSelector
  , suspensionCompressionSelector
  , suspensionDampingSelector
  , suspensionRestLengthSelector
  , suspensionStiffnessSelector
  , wheelWithNodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ wheelWithNode:@
wheelWithNode :: IsSCNNode node => node -> IO (Id SCNPhysicsVehicleWheel)
wheelWithNode node =
  do
    cls' <- getRequiredClass "SCNPhysicsVehicleWheel"
    sendClassMessage cls' wheelWithNodeSelector (toSCNNode node)

-- | @- node@
node :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO (Id SCNNode)
node scnPhysicsVehicleWheel =
  sendMessage scnPhysicsVehicleWheel nodeSelector

-- | @- suspensionStiffness@
suspensionStiffness :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
suspensionStiffness scnPhysicsVehicleWheel =
  sendMessage scnPhysicsVehicleWheel suspensionStiffnessSelector

-- | @- setSuspensionStiffness:@
setSuspensionStiffness :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setSuspensionStiffness scnPhysicsVehicleWheel value =
  sendMessage scnPhysicsVehicleWheel setSuspensionStiffnessSelector value

-- | @- suspensionCompression@
suspensionCompression :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
suspensionCompression scnPhysicsVehicleWheel =
  sendMessage scnPhysicsVehicleWheel suspensionCompressionSelector

-- | @- setSuspensionCompression:@
setSuspensionCompression :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setSuspensionCompression scnPhysicsVehicleWheel value =
  sendMessage scnPhysicsVehicleWheel setSuspensionCompressionSelector value

-- | @- suspensionDamping@
suspensionDamping :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
suspensionDamping scnPhysicsVehicleWheel =
  sendMessage scnPhysicsVehicleWheel suspensionDampingSelector

-- | @- setSuspensionDamping:@
setSuspensionDamping :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setSuspensionDamping scnPhysicsVehicleWheel value =
  sendMessage scnPhysicsVehicleWheel setSuspensionDampingSelector value

-- | @- maximumSuspensionTravel@
maximumSuspensionTravel :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
maximumSuspensionTravel scnPhysicsVehicleWheel =
  sendMessage scnPhysicsVehicleWheel maximumSuspensionTravelSelector

-- | @- setMaximumSuspensionTravel:@
setMaximumSuspensionTravel :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setMaximumSuspensionTravel scnPhysicsVehicleWheel value =
  sendMessage scnPhysicsVehicleWheel setMaximumSuspensionTravelSelector value

-- | @- frictionSlip@
frictionSlip :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
frictionSlip scnPhysicsVehicleWheel =
  sendMessage scnPhysicsVehicleWheel frictionSlipSelector

-- | @- setFrictionSlip:@
setFrictionSlip :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setFrictionSlip scnPhysicsVehicleWheel value =
  sendMessage scnPhysicsVehicleWheel setFrictionSlipSelector value

-- | @- maximumSuspensionForce@
maximumSuspensionForce :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
maximumSuspensionForce scnPhysicsVehicleWheel =
  sendMessage scnPhysicsVehicleWheel maximumSuspensionForceSelector

-- | @- setMaximumSuspensionForce:@
setMaximumSuspensionForce :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setMaximumSuspensionForce scnPhysicsVehicleWheel value =
  sendMessage scnPhysicsVehicleWheel setMaximumSuspensionForceSelector value

-- | @- connectionPosition@
connectionPosition :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO SCNVector3
connectionPosition scnPhysicsVehicleWheel =
  sendMessage scnPhysicsVehicleWheel connectionPositionSelector

-- | @- setConnectionPosition:@
setConnectionPosition :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> SCNVector3 -> IO ()
setConnectionPosition scnPhysicsVehicleWheel value =
  sendMessage scnPhysicsVehicleWheel setConnectionPositionSelector value

-- | @- steeringAxis@
steeringAxis :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO SCNVector3
steeringAxis scnPhysicsVehicleWheel =
  sendMessage scnPhysicsVehicleWheel steeringAxisSelector

-- | @- setSteeringAxis:@
setSteeringAxis :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> SCNVector3 -> IO ()
setSteeringAxis scnPhysicsVehicleWheel value =
  sendMessage scnPhysicsVehicleWheel setSteeringAxisSelector value

-- | @- axle@
axle :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO SCNVector3
axle scnPhysicsVehicleWheel =
  sendMessage scnPhysicsVehicleWheel axleSelector

-- | @- setAxle:@
setAxle :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> SCNVector3 -> IO ()
setAxle scnPhysicsVehicleWheel value =
  sendMessage scnPhysicsVehicleWheel setAxleSelector value

-- | @- radius@
radius :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
radius scnPhysicsVehicleWheel =
  sendMessage scnPhysicsVehicleWheel radiusSelector

-- | @- setRadius:@
setRadius :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setRadius scnPhysicsVehicleWheel value =
  sendMessage scnPhysicsVehicleWheel setRadiusSelector value

-- | @- suspensionRestLength@
suspensionRestLength :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
suspensionRestLength scnPhysicsVehicleWheel =
  sendMessage scnPhysicsVehicleWheel suspensionRestLengthSelector

-- | @- setSuspensionRestLength:@
setSuspensionRestLength :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setSuspensionRestLength scnPhysicsVehicleWheel value =
  sendMessage scnPhysicsVehicleWheel setSuspensionRestLengthSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wheelWithNode:@
wheelWithNodeSelector :: Selector '[Id SCNNode] (Id SCNPhysicsVehicleWheel)
wheelWithNodeSelector = mkSelector "wheelWithNode:"

-- | @Selector@ for @node@
nodeSelector :: Selector '[] (Id SCNNode)
nodeSelector = mkSelector "node"

-- | @Selector@ for @suspensionStiffness@
suspensionStiffnessSelector :: Selector '[] CDouble
suspensionStiffnessSelector = mkSelector "suspensionStiffness"

-- | @Selector@ for @setSuspensionStiffness:@
setSuspensionStiffnessSelector :: Selector '[CDouble] ()
setSuspensionStiffnessSelector = mkSelector "setSuspensionStiffness:"

-- | @Selector@ for @suspensionCompression@
suspensionCompressionSelector :: Selector '[] CDouble
suspensionCompressionSelector = mkSelector "suspensionCompression"

-- | @Selector@ for @setSuspensionCompression:@
setSuspensionCompressionSelector :: Selector '[CDouble] ()
setSuspensionCompressionSelector = mkSelector "setSuspensionCompression:"

-- | @Selector@ for @suspensionDamping@
suspensionDampingSelector :: Selector '[] CDouble
suspensionDampingSelector = mkSelector "suspensionDamping"

-- | @Selector@ for @setSuspensionDamping:@
setSuspensionDampingSelector :: Selector '[CDouble] ()
setSuspensionDampingSelector = mkSelector "setSuspensionDamping:"

-- | @Selector@ for @maximumSuspensionTravel@
maximumSuspensionTravelSelector :: Selector '[] CDouble
maximumSuspensionTravelSelector = mkSelector "maximumSuspensionTravel"

-- | @Selector@ for @setMaximumSuspensionTravel:@
setMaximumSuspensionTravelSelector :: Selector '[CDouble] ()
setMaximumSuspensionTravelSelector = mkSelector "setMaximumSuspensionTravel:"

-- | @Selector@ for @frictionSlip@
frictionSlipSelector :: Selector '[] CDouble
frictionSlipSelector = mkSelector "frictionSlip"

-- | @Selector@ for @setFrictionSlip:@
setFrictionSlipSelector :: Selector '[CDouble] ()
setFrictionSlipSelector = mkSelector "setFrictionSlip:"

-- | @Selector@ for @maximumSuspensionForce@
maximumSuspensionForceSelector :: Selector '[] CDouble
maximumSuspensionForceSelector = mkSelector "maximumSuspensionForce"

-- | @Selector@ for @setMaximumSuspensionForce:@
setMaximumSuspensionForceSelector :: Selector '[CDouble] ()
setMaximumSuspensionForceSelector = mkSelector "setMaximumSuspensionForce:"

-- | @Selector@ for @connectionPosition@
connectionPositionSelector :: Selector '[] SCNVector3
connectionPositionSelector = mkSelector "connectionPosition"

-- | @Selector@ for @setConnectionPosition:@
setConnectionPositionSelector :: Selector '[SCNVector3] ()
setConnectionPositionSelector = mkSelector "setConnectionPosition:"

-- | @Selector@ for @steeringAxis@
steeringAxisSelector :: Selector '[] SCNVector3
steeringAxisSelector = mkSelector "steeringAxis"

-- | @Selector@ for @setSteeringAxis:@
setSteeringAxisSelector :: Selector '[SCNVector3] ()
setSteeringAxisSelector = mkSelector "setSteeringAxis:"

-- | @Selector@ for @axle@
axleSelector :: Selector '[] SCNVector3
axleSelector = mkSelector "axle"

-- | @Selector@ for @setAxle:@
setAxleSelector :: Selector '[SCNVector3] ()
setAxleSelector = mkSelector "setAxle:"

-- | @Selector@ for @radius@
radiusSelector :: Selector '[] CDouble
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector '[CDouble] ()
setRadiusSelector = mkSelector "setRadius:"

-- | @Selector@ for @suspensionRestLength@
suspensionRestLengthSelector :: Selector '[] CDouble
suspensionRestLengthSelector = mkSelector "suspensionRestLength"

-- | @Selector@ for @setSuspensionRestLength:@
setSuspensionRestLengthSelector :: Selector '[CDouble] ()
setSuspensionRestLengthSelector = mkSelector "setSuspensionRestLength:"

