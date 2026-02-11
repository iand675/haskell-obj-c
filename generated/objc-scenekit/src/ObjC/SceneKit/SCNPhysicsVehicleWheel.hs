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
  , wheelWithNodeSelector
  , nodeSelector
  , suspensionStiffnessSelector
  , setSuspensionStiffnessSelector
  , suspensionCompressionSelector
  , setSuspensionCompressionSelector
  , suspensionDampingSelector
  , setSuspensionDampingSelector
  , maximumSuspensionTravelSelector
  , setMaximumSuspensionTravelSelector
  , frictionSlipSelector
  , setFrictionSlipSelector
  , maximumSuspensionForceSelector
  , setMaximumSuspensionForceSelector
  , connectionPositionSelector
  , setConnectionPositionSelector
  , steeringAxisSelector
  , setSteeringAxisSelector
  , axleSelector
  , setAxleSelector
  , radiusSelector
  , setRadiusSelector
  , suspensionRestLengthSelector
  , setSuspensionRestLengthSelector


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

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ wheelWithNode:@
wheelWithNode :: IsSCNNode node => node -> IO (Id SCNPhysicsVehicleWheel)
wheelWithNode node =
  do
    cls' <- getRequiredClass "SCNPhysicsVehicleWheel"
    withObjCPtr node $ \raw_node ->
      sendClassMsg cls' (mkSelector "wheelWithNode:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ())] >>= retainedObject . castPtr

-- | @- node@
node :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO (Id SCNNode)
node scnPhysicsVehicleWheel  =
  sendMsg scnPhysicsVehicleWheel (mkSelector "node") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- suspensionStiffness@
suspensionStiffness :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
suspensionStiffness scnPhysicsVehicleWheel  =
  sendMsg scnPhysicsVehicleWheel (mkSelector "suspensionStiffness") retCDouble []

-- | @- setSuspensionStiffness:@
setSuspensionStiffness :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setSuspensionStiffness scnPhysicsVehicleWheel  value =
  sendMsg scnPhysicsVehicleWheel (mkSelector "setSuspensionStiffness:") retVoid [argCDouble (fromIntegral value)]

-- | @- suspensionCompression@
suspensionCompression :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
suspensionCompression scnPhysicsVehicleWheel  =
  sendMsg scnPhysicsVehicleWheel (mkSelector "suspensionCompression") retCDouble []

-- | @- setSuspensionCompression:@
setSuspensionCompression :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setSuspensionCompression scnPhysicsVehicleWheel  value =
  sendMsg scnPhysicsVehicleWheel (mkSelector "setSuspensionCompression:") retVoid [argCDouble (fromIntegral value)]

-- | @- suspensionDamping@
suspensionDamping :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
suspensionDamping scnPhysicsVehicleWheel  =
  sendMsg scnPhysicsVehicleWheel (mkSelector "suspensionDamping") retCDouble []

-- | @- setSuspensionDamping:@
setSuspensionDamping :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setSuspensionDamping scnPhysicsVehicleWheel  value =
  sendMsg scnPhysicsVehicleWheel (mkSelector "setSuspensionDamping:") retVoid [argCDouble (fromIntegral value)]

-- | @- maximumSuspensionTravel@
maximumSuspensionTravel :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
maximumSuspensionTravel scnPhysicsVehicleWheel  =
  sendMsg scnPhysicsVehicleWheel (mkSelector "maximumSuspensionTravel") retCDouble []

-- | @- setMaximumSuspensionTravel:@
setMaximumSuspensionTravel :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setMaximumSuspensionTravel scnPhysicsVehicleWheel  value =
  sendMsg scnPhysicsVehicleWheel (mkSelector "setMaximumSuspensionTravel:") retVoid [argCDouble (fromIntegral value)]

-- | @- frictionSlip@
frictionSlip :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
frictionSlip scnPhysicsVehicleWheel  =
  sendMsg scnPhysicsVehicleWheel (mkSelector "frictionSlip") retCDouble []

-- | @- setFrictionSlip:@
setFrictionSlip :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setFrictionSlip scnPhysicsVehicleWheel  value =
  sendMsg scnPhysicsVehicleWheel (mkSelector "setFrictionSlip:") retVoid [argCDouble (fromIntegral value)]

-- | @- maximumSuspensionForce@
maximumSuspensionForce :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
maximumSuspensionForce scnPhysicsVehicleWheel  =
  sendMsg scnPhysicsVehicleWheel (mkSelector "maximumSuspensionForce") retCDouble []

-- | @- setMaximumSuspensionForce:@
setMaximumSuspensionForce :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setMaximumSuspensionForce scnPhysicsVehicleWheel  value =
  sendMsg scnPhysicsVehicleWheel (mkSelector "setMaximumSuspensionForce:") retVoid [argCDouble (fromIntegral value)]

-- | @- connectionPosition@
connectionPosition :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO SCNVector3
connectionPosition scnPhysicsVehicleWheel  =
  sendMsgStret scnPhysicsVehicleWheel (mkSelector "connectionPosition") retSCNVector3 []

-- | @- setConnectionPosition:@
setConnectionPosition :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> SCNVector3 -> IO ()
setConnectionPosition scnPhysicsVehicleWheel  value =
  sendMsg scnPhysicsVehicleWheel (mkSelector "setConnectionPosition:") retVoid [argSCNVector3 value]

-- | @- steeringAxis@
steeringAxis :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO SCNVector3
steeringAxis scnPhysicsVehicleWheel  =
  sendMsgStret scnPhysicsVehicleWheel (mkSelector "steeringAxis") retSCNVector3 []

-- | @- setSteeringAxis:@
setSteeringAxis :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> SCNVector3 -> IO ()
setSteeringAxis scnPhysicsVehicleWheel  value =
  sendMsg scnPhysicsVehicleWheel (mkSelector "setSteeringAxis:") retVoid [argSCNVector3 value]

-- | @- axle@
axle :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO SCNVector3
axle scnPhysicsVehicleWheel  =
  sendMsgStret scnPhysicsVehicleWheel (mkSelector "axle") retSCNVector3 []

-- | @- setAxle:@
setAxle :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> SCNVector3 -> IO ()
setAxle scnPhysicsVehicleWheel  value =
  sendMsg scnPhysicsVehicleWheel (mkSelector "setAxle:") retVoid [argSCNVector3 value]

-- | @- radius@
radius :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
radius scnPhysicsVehicleWheel  =
  sendMsg scnPhysicsVehicleWheel (mkSelector "radius") retCDouble []

-- | @- setRadius:@
setRadius :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setRadius scnPhysicsVehicleWheel  value =
  sendMsg scnPhysicsVehicleWheel (mkSelector "setRadius:") retVoid [argCDouble (fromIntegral value)]

-- | @- suspensionRestLength@
suspensionRestLength :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> IO CDouble
suspensionRestLength scnPhysicsVehicleWheel  =
  sendMsg scnPhysicsVehicleWheel (mkSelector "suspensionRestLength") retCDouble []

-- | @- setSuspensionRestLength:@
setSuspensionRestLength :: IsSCNPhysicsVehicleWheel scnPhysicsVehicleWheel => scnPhysicsVehicleWheel -> CDouble -> IO ()
setSuspensionRestLength scnPhysicsVehicleWheel  value =
  sendMsg scnPhysicsVehicleWheel (mkSelector "setSuspensionRestLength:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wheelWithNode:@
wheelWithNodeSelector :: Selector
wheelWithNodeSelector = mkSelector "wheelWithNode:"

-- | @Selector@ for @node@
nodeSelector :: Selector
nodeSelector = mkSelector "node"

-- | @Selector@ for @suspensionStiffness@
suspensionStiffnessSelector :: Selector
suspensionStiffnessSelector = mkSelector "suspensionStiffness"

-- | @Selector@ for @setSuspensionStiffness:@
setSuspensionStiffnessSelector :: Selector
setSuspensionStiffnessSelector = mkSelector "setSuspensionStiffness:"

-- | @Selector@ for @suspensionCompression@
suspensionCompressionSelector :: Selector
suspensionCompressionSelector = mkSelector "suspensionCompression"

-- | @Selector@ for @setSuspensionCompression:@
setSuspensionCompressionSelector :: Selector
setSuspensionCompressionSelector = mkSelector "setSuspensionCompression:"

-- | @Selector@ for @suspensionDamping@
suspensionDampingSelector :: Selector
suspensionDampingSelector = mkSelector "suspensionDamping"

-- | @Selector@ for @setSuspensionDamping:@
setSuspensionDampingSelector :: Selector
setSuspensionDampingSelector = mkSelector "setSuspensionDamping:"

-- | @Selector@ for @maximumSuspensionTravel@
maximumSuspensionTravelSelector :: Selector
maximumSuspensionTravelSelector = mkSelector "maximumSuspensionTravel"

-- | @Selector@ for @setMaximumSuspensionTravel:@
setMaximumSuspensionTravelSelector :: Selector
setMaximumSuspensionTravelSelector = mkSelector "setMaximumSuspensionTravel:"

-- | @Selector@ for @frictionSlip@
frictionSlipSelector :: Selector
frictionSlipSelector = mkSelector "frictionSlip"

-- | @Selector@ for @setFrictionSlip:@
setFrictionSlipSelector :: Selector
setFrictionSlipSelector = mkSelector "setFrictionSlip:"

-- | @Selector@ for @maximumSuspensionForce@
maximumSuspensionForceSelector :: Selector
maximumSuspensionForceSelector = mkSelector "maximumSuspensionForce"

-- | @Selector@ for @setMaximumSuspensionForce:@
setMaximumSuspensionForceSelector :: Selector
setMaximumSuspensionForceSelector = mkSelector "setMaximumSuspensionForce:"

-- | @Selector@ for @connectionPosition@
connectionPositionSelector :: Selector
connectionPositionSelector = mkSelector "connectionPosition"

-- | @Selector@ for @setConnectionPosition:@
setConnectionPositionSelector :: Selector
setConnectionPositionSelector = mkSelector "setConnectionPosition:"

-- | @Selector@ for @steeringAxis@
steeringAxisSelector :: Selector
steeringAxisSelector = mkSelector "steeringAxis"

-- | @Selector@ for @setSteeringAxis:@
setSteeringAxisSelector :: Selector
setSteeringAxisSelector = mkSelector "setSteeringAxis:"

-- | @Selector@ for @axle@
axleSelector :: Selector
axleSelector = mkSelector "axle"

-- | @Selector@ for @setAxle:@
setAxleSelector :: Selector
setAxleSelector = mkSelector "setAxle:"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector
setRadiusSelector = mkSelector "setRadius:"

-- | @Selector@ for @suspensionRestLength@
suspensionRestLengthSelector :: Selector
suspensionRestLengthSelector = mkSelector "suspensionRestLength"

-- | @Selector@ for @setSuspensionRestLength:@
setSuspensionRestLengthSelector :: Selector
setSuspensionRestLengthSelector = mkSelector "setSuspensionRestLength:"

