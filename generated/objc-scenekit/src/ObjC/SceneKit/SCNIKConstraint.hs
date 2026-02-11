{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNIKConstraint
--
-- A SCNIKConstraint applies an inverse kinematics constraint
--
-- Generated bindings for @SCNIKConstraint@.
module ObjC.SceneKit.SCNIKConstraint
  ( SCNIKConstraint
  , IsSCNIKConstraint(..)
  , initWithChainRootNode
  , inverseKinematicsConstraintWithChainRootNode
  , setMaxAllowedRotationAngle_forJoint
  , maxAllowedRotationAngleForJoint
  , chainRootNode
  , targetPosition
  , setTargetPosition
  , initWithChainRootNodeSelector
  , inverseKinematicsConstraintWithChainRootNodeSelector
  , setMaxAllowedRotationAngle_forJointSelector
  , maxAllowedRotationAngleForJointSelector
  , chainRootNodeSelector
  , targetPositionSelector
  , setTargetPositionSelector


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

-- | initWithChainRootNode:
--
-- Creates and returns a SCNIKConstraint object with the specified parameter.
--
-- @chainRootNode@ — The root node of the kinematic chain.
--
-- "chainRootNode" must be an ancestor of the node on which the constraint is applied.
--
-- ObjC selector: @- initWithChainRootNode:@
initWithChainRootNode :: (IsSCNIKConstraint scnikConstraint, IsSCNNode chainRootNode) => scnikConstraint -> chainRootNode -> IO (Id SCNIKConstraint)
initWithChainRootNode scnikConstraint  chainRootNode =
withObjCPtr chainRootNode $ \raw_chainRootNode ->
    sendMsg scnikConstraint (mkSelector "initWithChainRootNode:") (retPtr retVoid) [argPtr (castPtr raw_chainRootNode :: Ptr ())] >>= ownedObject . castPtr

-- | inverseKinematicsConstraintWithChainRootNode:
--
-- Creates and returns a SCNIKConstraint object with the specified parameter.
--
-- @chainRootNode@ — The root node of the kinematic chain.
--
-- "chainRootNode" must be an ancestor of the node on which the constraint is applied.
--
-- ObjC selector: @+ inverseKinematicsConstraintWithChainRootNode:@
inverseKinematicsConstraintWithChainRootNode :: IsSCNNode chainRootNode => chainRootNode -> IO (Id SCNIKConstraint)
inverseKinematicsConstraintWithChainRootNode chainRootNode =
  do
    cls' <- getRequiredClass "SCNIKConstraint"
    withObjCPtr chainRootNode $ \raw_chainRootNode ->
      sendClassMsg cls' (mkSelector "inverseKinematicsConstraintWithChainRootNode:") (retPtr retVoid) [argPtr (castPtr raw_chainRootNode :: Ptr ())] >>= retainedObject . castPtr

-- | setMaxAllowedRotationAngle:forJoint:
--
-- Specifies the maximum rotation allowed (in degrees) for the specified joint from its initial orientation. Defaults to 180.
--
-- ObjC selector: @- setMaxAllowedRotationAngle:forJoint:@
setMaxAllowedRotationAngle_forJoint :: (IsSCNIKConstraint scnikConstraint, IsSCNNode node) => scnikConstraint -> CDouble -> node -> IO ()
setMaxAllowedRotationAngle_forJoint scnikConstraint  angle node =
withObjCPtr node $ \raw_node ->
    sendMsg scnikConstraint (mkSelector "setMaxAllowedRotationAngle:forJoint:") retVoid [argCDouble (fromIntegral angle), argPtr (castPtr raw_node :: Ptr ())]

-- | @- maxAllowedRotationAngleForJoint:@
maxAllowedRotationAngleForJoint :: (IsSCNIKConstraint scnikConstraint, IsSCNNode node) => scnikConstraint -> node -> IO CDouble
maxAllowedRotationAngleForJoint scnikConstraint  node =
withObjCPtr node $ \raw_node ->
    sendMsg scnikConstraint (mkSelector "maxAllowedRotationAngleForJoint:") retCDouble [argPtr (castPtr raw_node :: Ptr ())]

-- | chainRootNode
--
-- Specifies the root node of the kinematic chain.
--
-- ObjC selector: @- chainRootNode@
chainRootNode :: IsSCNIKConstraint scnikConstraint => scnikConstraint -> IO (Id SCNNode)
chainRootNode scnikConstraint  =
  sendMsg scnikConstraint (mkSelector "chainRootNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | target
--
-- Specifies the target position (in world space coordinates) of the end joint (i.e the node that owns the IK constraint). Defaults to (0,0,0). Animatable.
--
-- ObjC selector: @- targetPosition@
targetPosition :: IsSCNIKConstraint scnikConstraint => scnikConstraint -> IO SCNVector3
targetPosition scnikConstraint  =
  sendMsgStret scnikConstraint (mkSelector "targetPosition") retSCNVector3 []

-- | target
--
-- Specifies the target position (in world space coordinates) of the end joint (i.e the node that owns the IK constraint). Defaults to (0,0,0). Animatable.
--
-- ObjC selector: @- setTargetPosition:@
setTargetPosition :: IsSCNIKConstraint scnikConstraint => scnikConstraint -> SCNVector3 -> IO ()
setTargetPosition scnikConstraint  value =
  sendMsg scnikConstraint (mkSelector "setTargetPosition:") retVoid [argSCNVector3 value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChainRootNode:@
initWithChainRootNodeSelector :: Selector
initWithChainRootNodeSelector = mkSelector "initWithChainRootNode:"

-- | @Selector@ for @inverseKinematicsConstraintWithChainRootNode:@
inverseKinematicsConstraintWithChainRootNodeSelector :: Selector
inverseKinematicsConstraintWithChainRootNodeSelector = mkSelector "inverseKinematicsConstraintWithChainRootNode:"

-- | @Selector@ for @setMaxAllowedRotationAngle:forJoint:@
setMaxAllowedRotationAngle_forJointSelector :: Selector
setMaxAllowedRotationAngle_forJointSelector = mkSelector "setMaxAllowedRotationAngle:forJoint:"

-- | @Selector@ for @maxAllowedRotationAngleForJoint:@
maxAllowedRotationAngleForJointSelector :: Selector
maxAllowedRotationAngleForJointSelector = mkSelector "maxAllowedRotationAngleForJoint:"

-- | @Selector@ for @chainRootNode@
chainRootNodeSelector :: Selector
chainRootNodeSelector = mkSelector "chainRootNode"

-- | @Selector@ for @targetPosition@
targetPositionSelector :: Selector
targetPositionSelector = mkSelector "targetPosition"

-- | @Selector@ for @setTargetPosition:@
setTargetPositionSelector :: Selector
setTargetPositionSelector = mkSelector "setTargetPosition:"

