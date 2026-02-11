{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNNode
--
-- SCNNode is the model class for node-tree objects.
--
-- It encapsulates the position, rotations, and other transforms of a node, which define a coordinate system.		     The coordinate systems of all the sub-nodes are relative to the one of their parent node.
--
-- Generated bindings for @SCNNode@.
module ObjC.SceneKit.SCNNode
  ( SCNNode
  , IsSCNNode(..)
  , node
  , nodeWithGeometry
  , clone
  , flattenedClone
  , setWorldTransform
  , addChildNode
  , insertChildNode_atIndex
  , removeFromParentNode
  , replaceChildNode_with
  , childNodeWithName_recursively
  , childNodesPassingTest
  , enumerateChildNodesUsingBlock
  , enumerateHierarchyUsingBlock
  , convertPosition_toNode
  , convertPosition_fromNode
  , convertVector_toNode
  , convertVector_fromNode
  , convertTransform_toNode
  , convertTransform_fromNode
  , hitTestWithSegmentFromPoint_toPoint_options
  , addAudioPlayer
  , removeAllAudioPlayers
  , removeAudioPlayer
  , addParticleSystem
  , removeAllParticleSystems
  , removeParticleSystem
  , lookAt
  , lookAt_up_localFront
  , localTranslateBy
  , localRotateBy
  , rotateBy_aroundTarget
  , name
  , setName
  , light
  , setLight
  , camera
  , setCamera
  , geometry
  , setGeometry
  , skinner
  , setSkinner
  , morpher
  , setMorpher
  , transform
  , setTransform
  , worldTransform
  , position
  , setPosition
  , worldPosition
  , setWorldPosition
  , rotation
  , setRotation
  , orientation
  , setOrientation
  , worldOrientation
  , setWorldOrientation
  , eulerAngles
  , setEulerAngles
  , scale
  , setScale
  , pivot
  , setPivot
  , hidden
  , setHidden
  , opacity
  , setOpacity
  , renderingOrder
  , setRenderingOrder
  , castsShadow
  , setCastsShadow
  , movabilityHint
  , setMovabilityHint
  , parentNode
  , childNodes
  , physicsBody
  , setPhysicsBody
  , physicsField
  , setPhysicsField
  , constraints
  , setConstraints
  , presentationNode
  , paused
  , setPaused
  , rendererDelegate
  , setRendererDelegate
  , categoryBitMask
  , setCategoryBitMask
  , audioPlayers
  , particleSystems
  , focusBehavior
  , setFocusBehavior
  , localUp
  , localRight
  , localFront
  , worldUp
  , worldRight
  , worldFront
  , nodeSelector
  , nodeWithGeometrySelector
  , cloneSelector
  , flattenedCloneSelector
  , setWorldTransformSelector
  , addChildNodeSelector
  , insertChildNode_atIndexSelector
  , removeFromParentNodeSelector
  , replaceChildNode_withSelector
  , childNodeWithName_recursivelySelector
  , childNodesPassingTestSelector
  , enumerateChildNodesUsingBlockSelector
  , enumerateHierarchyUsingBlockSelector
  , convertPosition_toNodeSelector
  , convertPosition_fromNodeSelector
  , convertVector_toNodeSelector
  , convertVector_fromNodeSelector
  , convertTransform_toNodeSelector
  , convertTransform_fromNodeSelector
  , hitTestWithSegmentFromPoint_toPoint_optionsSelector
  , addAudioPlayerSelector
  , removeAllAudioPlayersSelector
  , removeAudioPlayerSelector
  , addParticleSystemSelector
  , removeAllParticleSystemsSelector
  , removeParticleSystemSelector
  , lookAtSelector
  , lookAt_up_localFrontSelector
  , localTranslateBySelector
  , localRotateBySelector
  , rotateBy_aroundTargetSelector
  , nameSelector
  , setNameSelector
  , lightSelector
  , setLightSelector
  , cameraSelector
  , setCameraSelector
  , geometrySelector
  , setGeometrySelector
  , skinnerSelector
  , setSkinnerSelector
  , morpherSelector
  , setMorpherSelector
  , transformSelector
  , setTransformSelector
  , worldTransformSelector
  , positionSelector
  , setPositionSelector
  , worldPositionSelector
  , setWorldPositionSelector
  , rotationSelector
  , setRotationSelector
  , orientationSelector
  , setOrientationSelector
  , worldOrientationSelector
  , setWorldOrientationSelector
  , eulerAnglesSelector
  , setEulerAnglesSelector
  , scaleSelector
  , setScaleSelector
  , pivotSelector
  , setPivotSelector
  , hiddenSelector
  , setHiddenSelector
  , opacitySelector
  , setOpacitySelector
  , renderingOrderSelector
  , setRenderingOrderSelector
  , castsShadowSelector
  , setCastsShadowSelector
  , movabilityHintSelector
  , setMovabilityHintSelector
  , parentNodeSelector
  , childNodesSelector
  , physicsBodySelector
  , setPhysicsBodySelector
  , physicsFieldSelector
  , setPhysicsFieldSelector
  , constraintsSelector
  , setConstraintsSelector
  , presentationNodeSelector
  , pausedSelector
  , setPausedSelector
  , rendererDelegateSelector
  , setRendererDelegateSelector
  , categoryBitMaskSelector
  , setCategoryBitMaskSelector
  , audioPlayersSelector
  , particleSystemsSelector
  , focusBehaviorSelector
  , setFocusBehaviorSelector
  , localUpSelector
  , localRightSelector
  , localFrontSelector
  , worldUpSelector
  , worldRightSelector
  , worldFrontSelector

  -- * Enum types
  , SCNMovabilityHint(SCNMovabilityHint)
  , pattern SCNMovabilityHintFixed
  , pattern SCNMovabilityHintMovable
  , SCNNodeFocusBehavior(SCNNodeFocusBehavior)
  , pattern SCNNodeFocusBehaviorNone
  , pattern SCNNodeFocusBehaviorOccluding
  , pattern SCNNodeFocusBehaviorFocusable

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
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | node
--
-- Creates and initializes a node instance.
--
-- ObjC selector: @+ node@
node :: IO (Id SCNNode)
node  =
  do
    cls' <- getRequiredClass "SCNNode"
    sendClassMsg cls' (mkSelector "node") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | nodeWithGeometry:
--
-- Creates and initializes a node instance with the specified geometry attached.
--
-- @geometry@ — The geometry to attach.
--
-- ObjC selector: @+ nodeWithGeometry:@
nodeWithGeometry :: IsSCNGeometry geometry => geometry -> IO (Id SCNNode)
nodeWithGeometry geometry =
  do
    cls' <- getRequiredClass "SCNNode"
    withObjCPtr geometry $ \raw_geometry ->
      sendClassMsg cls' (mkSelector "nodeWithGeometry:") (retPtr retVoid) [argPtr (castPtr raw_geometry :: Ptr ())] >>= retainedObject . castPtr

-- | clone
--
-- Returns a copy of the receiver. The returned instance is autoreleased.
--
-- The copy is recursive: every child node will be cloned, too. For a non-recursive copy, use copy instead. The copied nodes will share their attached objects (light, geometry, camera, ...) with the original instances; if you want, for example, to change the materials of the copy independently of the original object, you'll have to copy the geometry of the node separately.
--
-- ObjC selector: @- clone@
clone :: IsSCNNode scnNode => scnNode -> IO (Id SCNNode)
clone scnNode  =
    sendMsg scnNode (mkSelector "clone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- flattenedClone@
flattenedClone :: IsSCNNode scnNode => scnNode -> IO (Id SCNNode)
flattenedClone scnNode  =
    sendMsg scnNode (mkSelector "flattenedClone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWorldTransform:@
setWorldTransform :: IsSCNNode scnNode => scnNode -> SCNMatrix4 -> IO ()
setWorldTransform scnNode  worldTransform =
    sendMsg scnNode (mkSelector "setWorldTransform:") retVoid [argSCNMatrix4 worldTransform]

-- | addChildNode:
--
-- Appends the node to the receiver’s childNodes array.
--
-- @child@ — The node to be added to the receiver’s childNodes array.
--
-- ObjC selector: @- addChildNode:@
addChildNode :: (IsSCNNode scnNode, IsSCNNode child) => scnNode -> child -> IO ()
addChildNode scnNode  child =
  withObjCPtr child $ \raw_child ->
      sendMsg scnNode (mkSelector "addChildNode:") retVoid [argPtr (castPtr raw_child :: Ptr ())]

-- | insertChildNode:atIndex:
--
-- Insert a node in the childNodes array at the specified index.
--
-- @child@ — The node to insert.
--
-- @index@ — Index in the childNodes array to insert the node.
--
-- ObjC selector: @- insertChildNode:atIndex:@
insertChildNode_atIndex :: (IsSCNNode scnNode, IsSCNNode child) => scnNode -> child -> CULong -> IO ()
insertChildNode_atIndex scnNode  child index =
  withObjCPtr child $ \raw_child ->
      sendMsg scnNode (mkSelector "insertChildNode:atIndex:") retVoid [argPtr (castPtr raw_child :: Ptr ()), argCULong index]

-- | removeFromParentNode
--
-- Removes the node from the childNodes array of the receiver’s parentNode.
--
-- ObjC selector: @- removeFromParentNode@
removeFromParentNode :: IsSCNNode scnNode => scnNode -> IO ()
removeFromParentNode scnNode  =
    sendMsg scnNode (mkSelector "removeFromParentNode") retVoid []

-- | replaceChildNode:with:
--
-- Remove `child' from the childNode array of the receiver and insert 'child2' if non-nil in its position.
--
-- If the parentNode of `child' is not the receiver, the behavior is undefined.
--
-- @oldChild@ — The node to replace in the childNodes array.
--
-- @newChild@ — The new node that will replace the previous one.
--
-- ObjC selector: @- replaceChildNode:with:@
replaceChildNode_with :: (IsSCNNode scnNode, IsSCNNode oldChild, IsSCNNode newChild) => scnNode -> oldChild -> newChild -> IO ()
replaceChildNode_with scnNode  oldChild newChild =
  withObjCPtr oldChild $ \raw_oldChild ->
    withObjCPtr newChild $ \raw_newChild ->
        sendMsg scnNode (mkSelector "replaceChildNode:with:") retVoid [argPtr (castPtr raw_oldChild :: Ptr ()), argPtr (castPtr raw_newChild :: Ptr ())]

-- | childNodeWithName:recursively:
--
-- Returns the first node found in the node tree with the specified name.
--
-- The search uses a pre-order tree traversal.
--
-- @name@ — The name of the node you are searching for.
--
-- @recursively@ — Set to YES if you want the search to look through the sub-nodes recursively.
--
-- ObjC selector: @- childNodeWithName:recursively:@
childNodeWithName_recursively :: (IsSCNNode scnNode, IsNSString name) => scnNode -> name -> Bool -> IO (Id SCNNode)
childNodeWithName_recursively scnNode  name recursively =
  withObjCPtr name $ \raw_name ->
      sendMsg scnNode (mkSelector "childNodeWithName:recursively:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (if recursively then 1 else 0)] >>= retainedObject . castPtr

-- | childNodesPassingTest:
--
-- Returns the child nodes of the receiver that passes a test in a given Block.
--
-- The search is recursive and uses a pre-order tree traversal.
--
-- @predicate@ — The block to apply to child nodes of the receiver. The block takes two arguments: "child" is a child node and "stop" is a reference to a Boolean value. The block can set the value to YES to stop further processing of the node hierarchy. The stop argument is an out-only argument. You should only ever set this Boolean to YES within the Block. The Block returns a Boolean value that indicates whether "child" passed the test.
--
-- ObjC selector: @- childNodesPassingTest:@
childNodesPassingTest :: IsSCNNode scnNode => scnNode -> Ptr () -> IO (Id NSArray)
childNodesPassingTest scnNode  predicate =
    sendMsg scnNode (mkSelector "childNodesPassingTest:") (retPtr retVoid) [argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | enumerateChildNodesUsingBlock:
--
-- Executes a given block on each child node under the receiver.
--
-- The search is recursive and uses a pre-order tree traversal.
--
-- @block@ — The block to apply to child nodes of the receiver. The block takes two arguments: "child" is a child node and "stop" is a reference to a Boolean value. The block can set the value to YES to stop further processing of the node hierarchy. The stop argument is an out-only argument. You should only ever set this Boolean to YES within the Block.
--
-- ObjC selector: @- enumerateChildNodesUsingBlock:@
enumerateChildNodesUsingBlock :: IsSCNNode scnNode => scnNode -> Ptr () -> IO ()
enumerateChildNodesUsingBlock scnNode  block =
    sendMsg scnNode (mkSelector "enumerateChildNodesUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | enumerateHierarchyUsingBlock:
--
-- Executes a given block on the receiver and its child nodes.
--
-- The search is recursive and uses a pre-order tree traversal.
--
-- @block@ — The block to apply to the receiver and its child nodes. The block takes two arguments: "node" is a node in the hierarchy of the receiver (including the receiver) and "stop" is a reference to a Boolean value. The block can set the value to YES to stop further processing of the node hierarchy. The stop argument is an out-only argument. You should only ever set this Boolean to YES within the Block.
--
-- ObjC selector: @- enumerateHierarchyUsingBlock:@
enumerateHierarchyUsingBlock :: IsSCNNode scnNode => scnNode -> Ptr () -> IO ()
enumerateHierarchyUsingBlock scnNode  block =
    sendMsg scnNode (mkSelector "enumerateHierarchyUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | convertPosition:toNode:
--
-- Converts a position from the receiver’s coordinate system to that of the specified node.
--
-- @position@ — A position specified in the local coordinate system of the receiver.
--
-- @node@ — The node into whose coordinate system "position" is to be converted. If "node" is nil, this method instead converts to world coordinates.
--
-- ObjC selector: @- convertPosition:toNode:@
convertPosition_toNode :: (IsSCNNode scnNode, IsSCNNode node) => scnNode -> SCNVector3 -> node -> IO SCNVector3
convertPosition_toNode scnNode  position node =
  withObjCPtr node $ \raw_node ->
      sendMsgStret scnNode (mkSelector "convertPosition:toNode:") retSCNVector3 [argSCNVector3 position, argPtr (castPtr raw_node :: Ptr ())]

-- | convertPosition:fromNode:
--
-- Converts a position from the coordinate system of a given node to that of the receiver.
--
-- @position@ — A position specified in the local coordinate system of "node".
--
-- @node@ — The node from whose coordinate system "position" is to be converted. If "node" is nil, this method instead converts from world coordinates.
--
-- ObjC selector: @- convertPosition:fromNode:@
convertPosition_fromNode :: (IsSCNNode scnNode, IsSCNNode node) => scnNode -> SCNVector3 -> node -> IO SCNVector3
convertPosition_fromNode scnNode  position node =
  withObjCPtr node $ \raw_node ->
      sendMsgStret scnNode (mkSelector "convertPosition:fromNode:") retSCNVector3 [argSCNVector3 position, argPtr (castPtr raw_node :: Ptr ())]

-- | Converts a vector from the coordinate system of a given node to that of the receiver.
--
-- @vector@ — A vector specified in the local coordinate system the receiver.
--
-- @node@ — The node defining the space from which the vector should be transformed. If "node" is nil, this method instead converts from world coordinates.
--
-- Returns: vector transformed from receiver local space to node local space.
--
-- ObjC selector: @- convertVector:toNode:@
convertVector_toNode :: (IsSCNNode scnNode, IsSCNNode node) => scnNode -> SCNVector3 -> node -> IO SCNVector3
convertVector_toNode scnNode  vector node =
  withObjCPtr node $ \raw_node ->
      sendMsgStret scnNode (mkSelector "convertVector:toNode:") retSCNVector3 [argSCNVector3 vector, argPtr (castPtr raw_node :: Ptr ())]

-- | Converts a vector from the coordinate system of a given node to that of the receiver.
--
-- @vector@ — A vector specified in the local coordinate system of "node".
--
-- @node@ — The node defining the space to which the vector should be transformed to. If "node" is nil, this method instead converts from world coordinates.
--
-- Returns: vector transformed from node space to reveiver local space.
--
-- ObjC selector: @- convertVector:fromNode:@
convertVector_fromNode :: (IsSCNNode scnNode, IsSCNNode node) => scnNode -> SCNVector3 -> node -> IO SCNVector3
convertVector_fromNode scnNode  vector node =
  withObjCPtr node $ \raw_node ->
      sendMsgStret scnNode (mkSelector "convertVector:fromNode:") retSCNVector3 [argSCNVector3 vector, argPtr (castPtr raw_node :: Ptr ())]

-- | convertTransform:toNode:
--
-- Converts a transform from the receiver’s coordinate system to that of the specified node.
--
-- @transform@ — A transform specified in the local coordinate system of the receiver.
--
-- @node@ — The node into whose coordinate system "transform" is to be converted. If "node" is nil, this method instead converts to world coordinates.
--
-- ObjC selector: @- convertTransform:toNode:@
convertTransform_toNode :: (IsSCNNode scnNode, IsSCNNode node) => scnNode -> SCNMatrix4 -> node -> IO SCNMatrix4
convertTransform_toNode scnNode  transform node =
  withObjCPtr node $ \raw_node ->
      sendMsgStret scnNode (mkSelector "convertTransform:toNode:") retSCNMatrix4 [argSCNMatrix4 transform, argPtr (castPtr raw_node :: Ptr ())]

-- | convertTransform:fromNode:
--
-- Converts a transform from the coordinate system of a given node to that of the receiver.
--
-- @transform@ — A transform specified in the local coordinate system of "node".
--
-- @node@ — The node from whose coordinate system "transform" is to be converted. If "node" is nil, this method instead converts from world coordinates.
--
-- ObjC selector: @- convertTransform:fromNode:@
convertTransform_fromNode :: (IsSCNNode scnNode, IsSCNNode node) => scnNode -> SCNMatrix4 -> node -> IO SCNMatrix4
convertTransform_fromNode scnNode  transform node =
  withObjCPtr node $ \raw_node ->
      sendMsgStret scnNode (mkSelector "convertTransform:fromNode:") retSCNMatrix4 [argSCNMatrix4 transform, argPtr (castPtr raw_node :: Ptr ())]

-- | hitTestWithSegmentFromPoint:toPoint:options:
--
-- Returns an array of SCNHitTestResult for each node in the receiver's sub tree that intersects the specified segment.
--
-- @pointA@ — The first point of the segment relative to the receiver.
--
-- @pointB@ — The second point of the segment relative to the receiver.
--
-- @options@ — Optional parameters (see the "Hit test options" section in SCNSceneRenderer.h for the available options).
--
-- See SCNSceneRenderer.h for a screen-space hit testing method.
--
-- ObjC selector: @- hitTestWithSegmentFromPoint:toPoint:options:@
hitTestWithSegmentFromPoint_toPoint_options :: (IsSCNNode scnNode, IsNSDictionary options) => scnNode -> SCNVector3 -> SCNVector3 -> options -> IO (Id NSArray)
hitTestWithSegmentFromPoint_toPoint_options scnNode  pointA pointB options =
  withObjCPtr options $ \raw_options ->
      sendMsg scnNode (mkSelector "hitTestWithSegmentFromPoint:toPoint:options:") (retPtr retVoid) [argSCNVector3 pointA, argSCNVector3 pointB, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | addAudioPlayer:
--
-- Add an audio player to the node and starts playing it right away.
--
-- ObjC selector: @- addAudioPlayer:@
addAudioPlayer :: (IsSCNNode scnNode, IsSCNAudioPlayer player) => scnNode -> player -> IO ()
addAudioPlayer scnNode  player =
  withObjCPtr player $ \raw_player ->
      sendMsg scnNode (mkSelector "addAudioPlayer:") retVoid [argPtr (castPtr raw_player :: Ptr ())]

-- | removeAllAudioPlayers
--
-- Remove all audio players from this node and stop playing them.
--
-- ObjC selector: @- removeAllAudioPlayers@
removeAllAudioPlayers :: IsSCNNode scnNode => scnNode -> IO ()
removeAllAudioPlayers scnNode  =
    sendMsg scnNode (mkSelector "removeAllAudioPlayers") retVoid []

-- | removeAudioPlayer:
--
-- Remove the given audio player from this node and stop playing it.
--
-- ObjC selector: @- removeAudioPlayer:@
removeAudioPlayer :: (IsSCNNode scnNode, IsSCNAudioPlayer player) => scnNode -> player -> IO ()
removeAudioPlayer scnNode  player =
  withObjCPtr player $ \raw_player ->
      sendMsg scnNode (mkSelector "removeAudioPlayer:") retVoid [argPtr (castPtr raw_player :: Ptr ())]

-- | @- addParticleSystem:@
addParticleSystem :: (IsSCNNode scnNode, IsSCNParticleSystem system) => scnNode -> system -> IO ()
addParticleSystem scnNode  system =
  withObjCPtr system $ \raw_system ->
      sendMsg scnNode (mkSelector "addParticleSystem:") retVoid [argPtr (castPtr raw_system :: Ptr ())]

-- | @- removeAllParticleSystems@
removeAllParticleSystems :: IsSCNNode scnNode => scnNode -> IO ()
removeAllParticleSystems scnNode  =
    sendMsg scnNode (mkSelector "removeAllParticleSystems") retVoid []

-- | @- removeParticleSystem:@
removeParticleSystem :: (IsSCNNode scnNode, IsSCNParticleSystem system) => scnNode -> system -> IO ()
removeParticleSystem scnNode  system =
  withObjCPtr system $ \raw_system ->
      sendMsg scnNode (mkSelector "removeParticleSystem:") retVoid [argPtr (castPtr raw_system :: Ptr ())]

-- | Convenience for calling lookAt:up:localFront: with worldUp set to @self.worldUp@ and localFront [SCNNode localFront].
--
-- @worldTarget@ — target position in world space.
--
-- ObjC selector: @- lookAt:@
lookAt :: IsSCNNode scnNode => scnNode -> SCNVector3 -> IO ()
lookAt scnNode  worldTarget =
    sendMsg scnNode (mkSelector "lookAt:") retVoid [argSCNVector3 worldTarget]

-- | Set the orientation of the node so its front vector is pointing toward a given target. Using a reference up vector in world space and a front vector in local space.
--
-- @worldTarget@ — position in world space.
--
-- @worldUp@ — the up vector in world space.
--
-- @localFront@ — the front vector in local space.
--
-- ObjC selector: @- lookAt:up:localFront:@
lookAt_up_localFront :: IsSCNNode scnNode => scnNode -> SCNVector3 -> SCNVector3 -> SCNVector3 -> IO ()
lookAt_up_localFront scnNode  worldTarget worldUp localFront =
    sendMsg scnNode (mkSelector "lookAt:up:localFront:") retVoid [argSCNVector3 worldTarget, argSCNVector3 worldUp, argSCNVector3 localFront]

-- | Translate the current node position along the given vector in local space.
--
-- @translation@ — the translation in local space.
--
-- ObjC selector: @- localTranslateBy:@
localTranslateBy :: IsSCNNode scnNode => scnNode -> SCNVector3 -> IO ()
localTranslateBy scnNode  translation =
    sendMsg scnNode (mkSelector "localTranslateBy:") retVoid [argSCNVector3 translation]

-- | Apply a the given rotation to the current one.
--
-- @rotation@ — rotation in local space.
--
-- ObjC selector: @- localRotateBy:@
localRotateBy :: IsSCNNode scnNode => scnNode -> SCNQuaternion -> IO ()
localRotateBy scnNode  rotation =
    sendMsg scnNode (mkSelector "localRotateBy:") retVoid [argSCNQuaternion rotation]

-- | Apply a rotation relative to a target point in parent space.
--
-- @worldRotation@ — rotation to apply in world space.
--
-- @worldTarget@ — position of the target in world space.
--
-- ObjC selector: @- rotateBy:aroundTarget:@
rotateBy_aroundTarget :: IsSCNNode scnNode => scnNode -> SCNQuaternion -> SCNVector3 -> IO ()
rotateBy_aroundTarget scnNode  worldRotation worldTarget =
    sendMsg scnNode (mkSelector "rotateBy:aroundTarget:") retVoid [argSCNQuaternion worldRotation, argSCNVector3 worldTarget]

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- name@
name :: IsSCNNode scnNode => scnNode -> IO (Id NSString)
name scnNode  =
    sendMsg scnNode (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- setName:@
setName :: (IsSCNNode scnNode, IsNSString value) => scnNode -> value -> IO ()
setName scnNode  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnNode (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | light
--
-- Determines the light attached to the receiver.
--
-- ObjC selector: @- light@
light :: IsSCNNode scnNode => scnNode -> IO (Id SCNLight)
light scnNode  =
    sendMsg scnNode (mkSelector "light") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | light
--
-- Determines the light attached to the receiver.
--
-- ObjC selector: @- setLight:@
setLight :: (IsSCNNode scnNode, IsSCNLight value) => scnNode -> value -> IO ()
setLight scnNode  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnNode (mkSelector "setLight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | camera
--
-- Determines the camera attached to the receiver.
--
-- ObjC selector: @- camera@
camera :: IsSCNNode scnNode => scnNode -> IO (Id SCNCamera)
camera scnNode  =
    sendMsg scnNode (mkSelector "camera") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | camera
--
-- Determines the camera attached to the receiver.
--
-- ObjC selector: @- setCamera:@
setCamera :: (IsSCNNode scnNode, IsSCNCamera value) => scnNode -> value -> IO ()
setCamera scnNode  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnNode (mkSelector "setCamera:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | geometry
--
-- Returns the geometry attached to the receiver.
--
-- ObjC selector: @- geometry@
geometry :: IsSCNNode scnNode => scnNode -> IO (Id SCNGeometry)
geometry scnNode  =
    sendMsg scnNode (mkSelector "geometry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | geometry
--
-- Returns the geometry attached to the receiver.
--
-- ObjC selector: @- setGeometry:@
setGeometry :: (IsSCNNode scnNode, IsSCNGeometry value) => scnNode -> value -> IO ()
setGeometry scnNode  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnNode (mkSelector "setGeometry:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | skinner
--
-- Returns the skinner attached to the receiver.
--
-- ObjC selector: @- skinner@
skinner :: IsSCNNode scnNode => scnNode -> IO (Id SCNSkinner)
skinner scnNode  =
    sendMsg scnNode (mkSelector "skinner") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | skinner
--
-- Returns the skinner attached to the receiver.
--
-- ObjC selector: @- setSkinner:@
setSkinner :: (IsSCNNode scnNode, IsSCNSkinner value) => scnNode -> value -> IO ()
setSkinner scnNode  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnNode (mkSelector "setSkinner:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | morpher
--
-- Returns the morpher attached to the receiver.
--
-- ObjC selector: @- morpher@
morpher :: IsSCNNode scnNode => scnNode -> IO (Id SCNMorpher)
morpher scnNode  =
    sendMsg scnNode (mkSelector "morpher") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | morpher
--
-- Returns the morpher attached to the receiver.
--
-- ObjC selector: @- setMorpher:@
setMorpher :: (IsSCNNode scnNode, IsSCNMorpher value) => scnNode -> value -> IO ()
setMorpher scnNode  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnNode (mkSelector "setMorpher:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | transform
--
-- Determines the receiver's transform. Animatable.
--
-- The transform is the combination of the position, rotation and scale defined below. So when the transform is set, the receiver's position, rotation and scale are changed to match the new transform.
--
-- ObjC selector: @- transform@
transform :: IsSCNNode scnNode => scnNode -> IO SCNMatrix4
transform scnNode  =
    sendMsgStret scnNode (mkSelector "transform") retSCNMatrix4 []

-- | transform
--
-- Determines the receiver's transform. Animatable.
--
-- The transform is the combination of the position, rotation and scale defined below. So when the transform is set, the receiver's position, rotation and scale are changed to match the new transform.
--
-- ObjC selector: @- setTransform:@
setTransform :: IsSCNNode scnNode => scnNode -> SCNMatrix4 -> IO ()
setTransform scnNode  value =
    sendMsg scnNode (mkSelector "setTransform:") retVoid [argSCNMatrix4 value]

-- | worldTransform
--
-- Determines the receiver's transform in world space (relative to the scene's root node). Animatable.
--
-- ObjC selector: @- worldTransform@
worldTransform :: IsSCNNode scnNode => scnNode -> IO SCNMatrix4
worldTransform scnNode  =
    sendMsgStret scnNode (mkSelector "worldTransform") retSCNMatrix4 []

-- | position
--
-- Determines the receiver's position. Animatable.
--
-- ObjC selector: @- position@
position :: IsSCNNode scnNode => scnNode -> IO SCNVector3
position scnNode  =
    sendMsgStret scnNode (mkSelector "position") retSCNVector3 []

-- | position
--
-- Determines the receiver's position. Animatable.
--
-- ObjC selector: @- setPosition:@
setPosition :: IsSCNNode scnNode => scnNode -> SCNVector3 -> IO ()
setPosition scnNode  value =
    sendMsg scnNode (mkSelector "setPosition:") retVoid [argSCNVector3 value]

-- | worldPosition
--
-- Determines the receiver's position in world space (relative to the scene's root node).
--
-- ObjC selector: @- worldPosition@
worldPosition :: IsSCNNode scnNode => scnNode -> IO SCNVector3
worldPosition scnNode  =
    sendMsgStret scnNode (mkSelector "worldPosition") retSCNVector3 []

-- | worldPosition
--
-- Determines the receiver's position in world space (relative to the scene's root node).
--
-- ObjC selector: @- setWorldPosition:@
setWorldPosition :: IsSCNNode scnNode => scnNode -> SCNVector3 -> IO ()
setWorldPosition scnNode  value =
    sendMsg scnNode (mkSelector "setWorldPosition:") retVoid [argSCNVector3 value]

-- | rotation
--
-- Determines the receiver's rotation. Animatable.
--
-- The rotation is axis angle rotation. The three first components are the axis, the fourth one is the rotation (in radian).
--
-- ObjC selector: @- rotation@
rotation :: IsSCNNode scnNode => scnNode -> IO SCNVector4
rotation scnNode  =
    sendMsgStret scnNode (mkSelector "rotation") retSCNVector4 []

-- | rotation
--
-- Determines the receiver's rotation. Animatable.
--
-- The rotation is axis angle rotation. The three first components are the axis, the fourth one is the rotation (in radian).
--
-- ObjC selector: @- setRotation:@
setRotation :: IsSCNNode scnNode => scnNode -> SCNVector4 -> IO ()
setRotation scnNode  value =
    sendMsg scnNode (mkSelector "setRotation:") retVoid [argSCNVector4 value]

-- | orientation
--
-- Determines the receiver's orientation as a unit quaternion. Animatable.
--
-- ObjC selector: @- orientation@
orientation :: IsSCNNode scnNode => scnNode -> IO SCNQuaternion
orientation scnNode  =
    sendMsgStret scnNode (mkSelector "orientation") retSCNQuaternion []

-- | orientation
--
-- Determines the receiver's orientation as a unit quaternion. Animatable.
--
-- ObjC selector: @- setOrientation:@
setOrientation :: IsSCNNode scnNode => scnNode -> SCNQuaternion -> IO ()
setOrientation scnNode  value =
    sendMsg scnNode (mkSelector "setOrientation:") retVoid [argSCNQuaternion value]

-- | worldOrientation
--
-- Determines the receiver's orientation in world space (relative to the scene's root node). Animatable.
--
-- ObjC selector: @- worldOrientation@
worldOrientation :: IsSCNNode scnNode => scnNode -> IO SCNQuaternion
worldOrientation scnNode  =
    sendMsgStret scnNode (mkSelector "worldOrientation") retSCNQuaternion []

-- | worldOrientation
--
-- Determines the receiver's orientation in world space (relative to the scene's root node). Animatable.
--
-- ObjC selector: @- setWorldOrientation:@
setWorldOrientation :: IsSCNNode scnNode => scnNode -> SCNQuaternion -> IO ()
setWorldOrientation scnNode  value =
    sendMsg scnNode (mkSelector "setWorldOrientation:") retVoid [argSCNQuaternion value]

-- | eulerAngles
--
-- Determines the receiver's euler angles. Animatable.
--
-- The order of components in this vector matches the axes of rotation:               1. Pitch (the x component) is the rotation about the node's x-axis (in radians)               2. Yaw   (the y component) is the rotation about the node's y-axis (in radians)               3. Roll  (the z component) is the rotation about the node's z-axis (in radians)            SceneKit applies these rotations in the reverse order of the components:               1. first roll               2. then yaw               3. then pitch
--
-- ObjC selector: @- eulerAngles@
eulerAngles :: IsSCNNode scnNode => scnNode -> IO SCNVector3
eulerAngles scnNode  =
    sendMsgStret scnNode (mkSelector "eulerAngles") retSCNVector3 []

-- | eulerAngles
--
-- Determines the receiver's euler angles. Animatable.
--
-- The order of components in this vector matches the axes of rotation:               1. Pitch (the x component) is the rotation about the node's x-axis (in radians)               2. Yaw   (the y component) is the rotation about the node's y-axis (in radians)               3. Roll  (the z component) is the rotation about the node's z-axis (in radians)            SceneKit applies these rotations in the reverse order of the components:               1. first roll               2. then yaw               3. then pitch
--
-- ObjC selector: @- setEulerAngles:@
setEulerAngles :: IsSCNNode scnNode => scnNode -> SCNVector3 -> IO ()
setEulerAngles scnNode  value =
    sendMsg scnNode (mkSelector "setEulerAngles:") retVoid [argSCNVector3 value]

-- | scale
--
-- Determines the receiver's scale. Animatable.
--
-- ObjC selector: @- scale@
scale :: IsSCNNode scnNode => scnNode -> IO SCNVector3
scale scnNode  =
    sendMsgStret scnNode (mkSelector "scale") retSCNVector3 []

-- | scale
--
-- Determines the receiver's scale. Animatable.
--
-- ObjC selector: @- setScale:@
setScale :: IsSCNNode scnNode => scnNode -> SCNVector3 -> IO ()
setScale scnNode  value =
    sendMsg scnNode (mkSelector "setScale:") retVoid [argSCNVector3 value]

-- | pivot
--
-- Determines the receiver's pivot. Animatable.
--
-- ObjC selector: @- pivot@
pivot :: IsSCNNode scnNode => scnNode -> IO SCNMatrix4
pivot scnNode  =
    sendMsgStret scnNode (mkSelector "pivot") retSCNMatrix4 []

-- | pivot
--
-- Determines the receiver's pivot. Animatable.
--
-- ObjC selector: @- setPivot:@
setPivot :: IsSCNNode scnNode => scnNode -> SCNMatrix4 -> IO ()
setPivot scnNode  value =
    sendMsg scnNode (mkSelector "setPivot:") retVoid [argSCNMatrix4 value]

-- | hidden
--
-- Determines whether the receiver is displayed. Defaults to NO. Animatable.
--
-- ObjC selector: @- hidden@
hidden :: IsSCNNode scnNode => scnNode -> IO Bool
hidden scnNode  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnNode (mkSelector "hidden") retCULong []

-- | hidden
--
-- Determines whether the receiver is displayed. Defaults to NO. Animatable.
--
-- ObjC selector: @- setHidden:@
setHidden :: IsSCNNode scnNode => scnNode -> Bool -> IO ()
setHidden scnNode  value =
    sendMsg scnNode (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | opacity
--
-- Determines the opacity of the receiver. Default is 1. Animatable.
--
-- ObjC selector: @- opacity@
opacity :: IsSCNNode scnNode => scnNode -> IO CDouble
opacity scnNode  =
    sendMsg scnNode (mkSelector "opacity") retCDouble []

-- | opacity
--
-- Determines the opacity of the receiver. Default is 1. Animatable.
--
-- ObjC selector: @- setOpacity:@
setOpacity :: IsSCNNode scnNode => scnNode -> CDouble -> IO ()
setOpacity scnNode  value =
    sendMsg scnNode (mkSelector "setOpacity:") retVoid [argCDouble value]

-- | renderingOrder
--
-- Determines the rendering order of the receiver.
--
-- Nodes with greater rendering orders are rendered last. Defaults to 0.
--
-- ObjC selector: @- renderingOrder@
renderingOrder :: IsSCNNode scnNode => scnNode -> IO CLong
renderingOrder scnNode  =
    sendMsg scnNode (mkSelector "renderingOrder") retCLong []

-- | renderingOrder
--
-- Determines the rendering order of the receiver.
--
-- Nodes with greater rendering orders are rendered last. Defaults to 0.
--
-- ObjC selector: @- setRenderingOrder:@
setRenderingOrder :: IsSCNNode scnNode => scnNode -> CLong -> IO ()
setRenderingOrder scnNode  value =
    sendMsg scnNode (mkSelector "setRenderingOrder:") retVoid [argCLong value]

-- | castsShadow
--
-- Determines if the node is rendered in shadow maps. Defaults to YES.
--
-- ObjC selector: @- castsShadow@
castsShadow :: IsSCNNode scnNode => scnNode -> IO Bool
castsShadow scnNode  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnNode (mkSelector "castsShadow") retCULong []

-- | castsShadow
--
-- Determines if the node is rendered in shadow maps. Defaults to YES.
--
-- ObjC selector: @- setCastsShadow:@
setCastsShadow :: IsSCNNode scnNode => scnNode -> Bool -> IO ()
setCastsShadow scnNode  value =
    sendMsg scnNode (mkSelector "setCastsShadow:") retVoid [argCULong (if value then 1 else 0)]

-- | movabilityHint
--
-- Communicates to SceneKit’s rendering system about how you want to move content in your scene; it does not affect your ability to change the node’s position or add animations or physics to the node. Defaults to SCNMovabilityHintFixed.
--
-- ObjC selector: @- movabilityHint@
movabilityHint :: IsSCNNode scnNode => scnNode -> IO SCNMovabilityHint
movabilityHint scnNode  =
    fmap (coerce :: CLong -> SCNMovabilityHint) $ sendMsg scnNode (mkSelector "movabilityHint") retCLong []

-- | movabilityHint
--
-- Communicates to SceneKit’s rendering system about how you want to move content in your scene; it does not affect your ability to change the node’s position or add animations or physics to the node. Defaults to SCNMovabilityHintFixed.
--
-- ObjC selector: @- setMovabilityHint:@
setMovabilityHint :: IsSCNNode scnNode => scnNode -> SCNMovabilityHint -> IO ()
setMovabilityHint scnNode  value =
    sendMsg scnNode (mkSelector "setMovabilityHint:") retVoid [argCLong (coerce value)]

-- | parentNode
--
-- Returns the parent node of the receiver.
--
-- ObjC selector: @- parentNode@
parentNode :: IsSCNNode scnNode => scnNode -> IO (Id SCNNode)
parentNode scnNode  =
    sendMsg scnNode (mkSelector "parentNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | childNodes
--
-- Returns the child node array of the receiver.
--
-- ObjC selector: @- childNodes@
childNodes :: IsSCNNode scnNode => scnNode -> IO (Id NSArray)
childNodes scnNode  =
    sendMsg scnNode (mkSelector "childNodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | physicsBody
--
-- The description of the physics body of the receiver.
--
-- Default is nil.
--
-- ObjC selector: @- physicsBody@
physicsBody :: IsSCNNode scnNode => scnNode -> IO (Id SCNPhysicsBody)
physicsBody scnNode  =
    sendMsg scnNode (mkSelector "physicsBody") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | physicsBody
--
-- The description of the physics body of the receiver.
--
-- Default is nil.
--
-- ObjC selector: @- setPhysicsBody:@
setPhysicsBody :: (IsSCNNode scnNode, IsSCNPhysicsBody value) => scnNode -> value -> IO ()
setPhysicsBody scnNode  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnNode (mkSelector "setPhysicsBody:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | physicsField
--
-- The description of the physics field of the receiver.
--
-- Default is nil.
--
-- ObjC selector: @- physicsField@
physicsField :: IsSCNNode scnNode => scnNode -> IO (Id SCNPhysicsField)
physicsField scnNode  =
    sendMsg scnNode (mkSelector "physicsField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | physicsField
--
-- The description of the physics field of the receiver.
--
-- Default is nil.
--
-- ObjC selector: @- setPhysicsField:@
setPhysicsField :: (IsSCNNode scnNode, IsSCNPhysicsField value) => scnNode -> value -> IO ()
setPhysicsField scnNode  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnNode (mkSelector "setPhysicsField:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | constraints
--
-- An array of SCNConstraint that are applied to the receiver.
--
-- Adding or removing a constraint can be implicitly animated based on the current transaction.
--
-- ObjC selector: @- constraints@
constraints :: IsSCNNode scnNode => scnNode -> IO (Id NSArray)
constraints scnNode  =
    sendMsg scnNode (mkSelector "constraints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | constraints
--
-- An array of SCNConstraint that are applied to the receiver.
--
-- Adding or removing a constraint can be implicitly animated based on the current transaction.
--
-- ObjC selector: @- setConstraints:@
setConstraints :: (IsSCNNode scnNode, IsNSArray value) => scnNode -> value -> IO ()
setConstraints scnNode  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnNode (mkSelector "setConstraints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | presentationNode
--
-- Returns the presentation node.
--
-- Returns a copy of the node containing all the properties as they were at the start of the current transaction, with any active animations applied.             This gives a close approximation to the version of the node that is currently displayed.             The effect of attempting to modify the returned node in any way is undefined. The returned node has no parent and no child nodes.
--
-- ObjC selector: @- presentationNode@
presentationNode :: IsSCNNode scnNode => scnNode -> IO (Id SCNNode)
presentationNode scnNode  =
    sendMsg scnNode (mkSelector "presentationNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | paused
--
-- Controls whether or not the node's actions and animations are updated or paused. Defaults to NO.
--
-- ObjC selector: @- paused@
paused :: IsSCNNode scnNode => scnNode -> IO Bool
paused scnNode  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnNode (mkSelector "paused") retCULong []

-- | paused
--
-- Controls whether or not the node's actions and animations are updated or paused. Defaults to NO.
--
-- ObjC selector: @- setPaused:@
setPaused :: IsSCNNode scnNode => scnNode -> Bool -> IO ()
setPaused scnNode  value =
    sendMsg scnNode (mkSelector "setPaused:") retVoid [argCULong (if value then 1 else 0)]

-- | rendererDelegate
--
-- Specifies the receiver's renderer delegate object.
--
-- Setting a renderer delegate prevents the SceneKit renderer from drawing the node and lets you use custom OpenGL code instead.             The preferred way to customize the rendering is to tweak the material properties of the different materials of the node's geometry. SCNMaterial conforms to the SCNShadable protocol and allows for more advanced rendering using GLSL.             You would typically use a renderer delegate with a node that has no geometry and only serves as a location in space. An example would be attaching a particle system to that node and render it with custom OpenGL code.
--
-- ObjC selector: @- rendererDelegate@
rendererDelegate :: IsSCNNode scnNode => scnNode -> IO RawId
rendererDelegate scnNode  =
    fmap (RawId . castPtr) $ sendMsg scnNode (mkSelector "rendererDelegate") (retPtr retVoid) []

-- | rendererDelegate
--
-- Specifies the receiver's renderer delegate object.
--
-- Setting a renderer delegate prevents the SceneKit renderer from drawing the node and lets you use custom OpenGL code instead.             The preferred way to customize the rendering is to tweak the material properties of the different materials of the node's geometry. SCNMaterial conforms to the SCNShadable protocol and allows for more advanced rendering using GLSL.             You would typically use a renderer delegate with a node that has no geometry and only serves as a location in space. An example would be attaching a particle system to that node and render it with custom OpenGL code.
--
-- ObjC selector: @- setRendererDelegate:@
setRendererDelegate :: IsSCNNode scnNode => scnNode -> RawId -> IO ()
setRendererDelegate scnNode  value =
    sendMsg scnNode (mkSelector "setRendererDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | categoryBitMask
--
-- Defines what logical 'categories' the receiver belongs too. Defaults to 1.
--
-- Categories can be used to                1. exclude nodes from the influence of a given light (see SCNLight.categoryBitMask)                2. include/exclude nodes from render passes (see SCNTechnique.h)                3. specify which nodes to use when hit-testing (see SCNHitTestOptionCategoryBitMask)
--
-- ObjC selector: @- categoryBitMask@
categoryBitMask :: IsSCNNode scnNode => scnNode -> IO CULong
categoryBitMask scnNode  =
    sendMsg scnNode (mkSelector "categoryBitMask") retCULong []

-- | categoryBitMask
--
-- Defines what logical 'categories' the receiver belongs too. Defaults to 1.
--
-- Categories can be used to                1. exclude nodes from the influence of a given light (see SCNLight.categoryBitMask)                2. include/exclude nodes from render passes (see SCNTechnique.h)                3. specify which nodes to use when hit-testing (see SCNHitTestOptionCategoryBitMask)
--
-- ObjC selector: @- setCategoryBitMask:@
setCategoryBitMask :: IsSCNNode scnNode => scnNode -> CULong -> IO ()
setCategoryBitMask scnNode  value =
    sendMsg scnNode (mkSelector "setCategoryBitMask:") retVoid [argCULong value]

-- | audioPlayers
--
-- Get an array with all the audio players connected and playing on this node.
--
-- ObjC selector: @- audioPlayers@
audioPlayers :: IsSCNNode scnNode => scnNode -> IO (Id NSArray)
audioPlayers scnNode  =
    sendMsg scnNode (mkSelector "audioPlayers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- particleSystems@
particleSystems :: IsSCNNode scnNode => scnNode -> IO (Id NSArray)
particleSystems scnNode  =
    sendMsg scnNode (mkSelector "particleSystems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | focusBehavior
--
-- Controls the behavior of the receiver regarding the UIFocus system. Defaults to SCNNodeFocusBehaviorNone.
--
-- ObjC selector: @- focusBehavior@
focusBehavior :: IsSCNNode scnNode => scnNode -> IO SCNNodeFocusBehavior
focusBehavior scnNode  =
    fmap (coerce :: CLong -> SCNNodeFocusBehavior) $ sendMsg scnNode (mkSelector "focusBehavior") retCLong []

-- | focusBehavior
--
-- Controls the behavior of the receiver regarding the UIFocus system. Defaults to SCNNodeFocusBehaviorNone.
--
-- ObjC selector: @- setFocusBehavior:@
setFocusBehavior :: IsSCNNode scnNode => scnNode -> SCNNodeFocusBehavior -> IO ()
setFocusBehavior scnNode  value =
    sendMsg scnNode (mkSelector "setFocusBehavior:") retVoid [argCLong (coerce value)]

-- | localUp
--
-- The local unit Y axis (0, 1, 0).
--
-- ObjC selector: @+ localUp@
localUp :: IO SCNVector3
localUp  =
  do
    cls' <- getRequiredClass "SCNNode"
    sendClassMsgStret cls' (mkSelector "localUp") retSCNVector3 []

-- | localRight
--
-- The local unit X axis (1, 0, 0).
--
-- ObjC selector: @+ localRight@
localRight :: IO SCNVector3
localRight  =
  do
    cls' <- getRequiredClass "SCNNode"
    sendClassMsgStret cls' (mkSelector "localRight") retSCNVector3 []

-- | localFront
--
-- The local unit -Z axis (0, 0, -1).
--
-- ObjC selector: @+ localFront@
localFront :: IO SCNVector3
localFront  =
  do
    cls' <- getRequiredClass "SCNNode"
    sendClassMsgStret cls' (mkSelector "localFront") retSCNVector3 []

-- | worldUp
--
-- The local unit Y axis (0, 1, 0) in world space.
--
-- ObjC selector: @- worldUp@
worldUp :: IsSCNNode scnNode => scnNode -> IO SCNVector3
worldUp scnNode  =
    sendMsgStret scnNode (mkSelector "worldUp") retSCNVector3 []

-- | worldRight
--
-- The local unit X axis (1, 0, 0) in world space.
--
-- ObjC selector: @- worldRight@
worldRight :: IsSCNNode scnNode => scnNode -> IO SCNVector3
worldRight scnNode  =
    sendMsgStret scnNode (mkSelector "worldRight") retSCNVector3 []

-- | worldFront
--
-- The local unit -Z axis (0, 0, -1) in world space.
--
-- ObjC selector: @- worldFront@
worldFront :: IsSCNNode scnNode => scnNode -> IO SCNVector3
worldFront scnNode  =
    sendMsgStret scnNode (mkSelector "worldFront") retSCNVector3 []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @node@
nodeSelector :: Selector
nodeSelector = mkSelector "node"

-- | @Selector@ for @nodeWithGeometry:@
nodeWithGeometrySelector :: Selector
nodeWithGeometrySelector = mkSelector "nodeWithGeometry:"

-- | @Selector@ for @clone@
cloneSelector :: Selector
cloneSelector = mkSelector "clone"

-- | @Selector@ for @flattenedClone@
flattenedCloneSelector :: Selector
flattenedCloneSelector = mkSelector "flattenedClone"

-- | @Selector@ for @setWorldTransform:@
setWorldTransformSelector :: Selector
setWorldTransformSelector = mkSelector "setWorldTransform:"

-- | @Selector@ for @addChildNode:@
addChildNodeSelector :: Selector
addChildNodeSelector = mkSelector "addChildNode:"

-- | @Selector@ for @insertChildNode:atIndex:@
insertChildNode_atIndexSelector :: Selector
insertChildNode_atIndexSelector = mkSelector "insertChildNode:atIndex:"

-- | @Selector@ for @removeFromParentNode@
removeFromParentNodeSelector :: Selector
removeFromParentNodeSelector = mkSelector "removeFromParentNode"

-- | @Selector@ for @replaceChildNode:with:@
replaceChildNode_withSelector :: Selector
replaceChildNode_withSelector = mkSelector "replaceChildNode:with:"

-- | @Selector@ for @childNodeWithName:recursively:@
childNodeWithName_recursivelySelector :: Selector
childNodeWithName_recursivelySelector = mkSelector "childNodeWithName:recursively:"

-- | @Selector@ for @childNodesPassingTest:@
childNodesPassingTestSelector :: Selector
childNodesPassingTestSelector = mkSelector "childNodesPassingTest:"

-- | @Selector@ for @enumerateChildNodesUsingBlock:@
enumerateChildNodesUsingBlockSelector :: Selector
enumerateChildNodesUsingBlockSelector = mkSelector "enumerateChildNodesUsingBlock:"

-- | @Selector@ for @enumerateHierarchyUsingBlock:@
enumerateHierarchyUsingBlockSelector :: Selector
enumerateHierarchyUsingBlockSelector = mkSelector "enumerateHierarchyUsingBlock:"

-- | @Selector@ for @convertPosition:toNode:@
convertPosition_toNodeSelector :: Selector
convertPosition_toNodeSelector = mkSelector "convertPosition:toNode:"

-- | @Selector@ for @convertPosition:fromNode:@
convertPosition_fromNodeSelector :: Selector
convertPosition_fromNodeSelector = mkSelector "convertPosition:fromNode:"

-- | @Selector@ for @convertVector:toNode:@
convertVector_toNodeSelector :: Selector
convertVector_toNodeSelector = mkSelector "convertVector:toNode:"

-- | @Selector@ for @convertVector:fromNode:@
convertVector_fromNodeSelector :: Selector
convertVector_fromNodeSelector = mkSelector "convertVector:fromNode:"

-- | @Selector@ for @convertTransform:toNode:@
convertTransform_toNodeSelector :: Selector
convertTransform_toNodeSelector = mkSelector "convertTransform:toNode:"

-- | @Selector@ for @convertTransform:fromNode:@
convertTransform_fromNodeSelector :: Selector
convertTransform_fromNodeSelector = mkSelector "convertTransform:fromNode:"

-- | @Selector@ for @hitTestWithSegmentFromPoint:toPoint:options:@
hitTestWithSegmentFromPoint_toPoint_optionsSelector :: Selector
hitTestWithSegmentFromPoint_toPoint_optionsSelector = mkSelector "hitTestWithSegmentFromPoint:toPoint:options:"

-- | @Selector@ for @addAudioPlayer:@
addAudioPlayerSelector :: Selector
addAudioPlayerSelector = mkSelector "addAudioPlayer:"

-- | @Selector@ for @removeAllAudioPlayers@
removeAllAudioPlayersSelector :: Selector
removeAllAudioPlayersSelector = mkSelector "removeAllAudioPlayers"

-- | @Selector@ for @removeAudioPlayer:@
removeAudioPlayerSelector :: Selector
removeAudioPlayerSelector = mkSelector "removeAudioPlayer:"

-- | @Selector@ for @addParticleSystem:@
addParticleSystemSelector :: Selector
addParticleSystemSelector = mkSelector "addParticleSystem:"

-- | @Selector@ for @removeAllParticleSystems@
removeAllParticleSystemsSelector :: Selector
removeAllParticleSystemsSelector = mkSelector "removeAllParticleSystems"

-- | @Selector@ for @removeParticleSystem:@
removeParticleSystemSelector :: Selector
removeParticleSystemSelector = mkSelector "removeParticleSystem:"

-- | @Selector@ for @lookAt:@
lookAtSelector :: Selector
lookAtSelector = mkSelector "lookAt:"

-- | @Selector@ for @lookAt:up:localFront:@
lookAt_up_localFrontSelector :: Selector
lookAt_up_localFrontSelector = mkSelector "lookAt:up:localFront:"

-- | @Selector@ for @localTranslateBy:@
localTranslateBySelector :: Selector
localTranslateBySelector = mkSelector "localTranslateBy:"

-- | @Selector@ for @localRotateBy:@
localRotateBySelector :: Selector
localRotateBySelector = mkSelector "localRotateBy:"

-- | @Selector@ for @rotateBy:aroundTarget:@
rotateBy_aroundTargetSelector :: Selector
rotateBy_aroundTargetSelector = mkSelector "rotateBy:aroundTarget:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @light@
lightSelector :: Selector
lightSelector = mkSelector "light"

-- | @Selector@ for @setLight:@
setLightSelector :: Selector
setLightSelector = mkSelector "setLight:"

-- | @Selector@ for @camera@
cameraSelector :: Selector
cameraSelector = mkSelector "camera"

-- | @Selector@ for @setCamera:@
setCameraSelector :: Selector
setCameraSelector = mkSelector "setCamera:"

-- | @Selector@ for @geometry@
geometrySelector :: Selector
geometrySelector = mkSelector "geometry"

-- | @Selector@ for @setGeometry:@
setGeometrySelector :: Selector
setGeometrySelector = mkSelector "setGeometry:"

-- | @Selector@ for @skinner@
skinnerSelector :: Selector
skinnerSelector = mkSelector "skinner"

-- | @Selector@ for @setSkinner:@
setSkinnerSelector :: Selector
setSkinnerSelector = mkSelector "setSkinner:"

-- | @Selector@ for @morpher@
morpherSelector :: Selector
morpherSelector = mkSelector "morpher"

-- | @Selector@ for @setMorpher:@
setMorpherSelector :: Selector
setMorpherSelector = mkSelector "setMorpher:"

-- | @Selector@ for @transform@
transformSelector :: Selector
transformSelector = mkSelector "transform"

-- | @Selector@ for @setTransform:@
setTransformSelector :: Selector
setTransformSelector = mkSelector "setTransform:"

-- | @Selector@ for @worldTransform@
worldTransformSelector :: Selector
worldTransformSelector = mkSelector "worldTransform"

-- | @Selector@ for @position@
positionSelector :: Selector
positionSelector = mkSelector "position"

-- | @Selector@ for @setPosition:@
setPositionSelector :: Selector
setPositionSelector = mkSelector "setPosition:"

-- | @Selector@ for @worldPosition@
worldPositionSelector :: Selector
worldPositionSelector = mkSelector "worldPosition"

-- | @Selector@ for @setWorldPosition:@
setWorldPositionSelector :: Selector
setWorldPositionSelector = mkSelector "setWorldPosition:"

-- | @Selector@ for @rotation@
rotationSelector :: Selector
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @setRotation:@
setRotationSelector :: Selector
setRotationSelector = mkSelector "setRotation:"

-- | @Selector@ for @orientation@
orientationSelector :: Selector
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @worldOrientation@
worldOrientationSelector :: Selector
worldOrientationSelector = mkSelector "worldOrientation"

-- | @Selector@ for @setWorldOrientation:@
setWorldOrientationSelector :: Selector
setWorldOrientationSelector = mkSelector "setWorldOrientation:"

-- | @Selector@ for @eulerAngles@
eulerAnglesSelector :: Selector
eulerAnglesSelector = mkSelector "eulerAngles"

-- | @Selector@ for @setEulerAngles:@
setEulerAnglesSelector :: Selector
setEulerAnglesSelector = mkSelector "setEulerAngles:"

-- | @Selector@ for @scale@
scaleSelector :: Selector
scaleSelector = mkSelector "scale"

-- | @Selector@ for @setScale:@
setScaleSelector :: Selector
setScaleSelector = mkSelector "setScale:"

-- | @Selector@ for @pivot@
pivotSelector :: Selector
pivotSelector = mkSelector "pivot"

-- | @Selector@ for @setPivot:@
setPivotSelector :: Selector
setPivotSelector = mkSelector "setPivot:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @opacity@
opacitySelector :: Selector
opacitySelector = mkSelector "opacity"

-- | @Selector@ for @setOpacity:@
setOpacitySelector :: Selector
setOpacitySelector = mkSelector "setOpacity:"

-- | @Selector@ for @renderingOrder@
renderingOrderSelector :: Selector
renderingOrderSelector = mkSelector "renderingOrder"

-- | @Selector@ for @setRenderingOrder:@
setRenderingOrderSelector :: Selector
setRenderingOrderSelector = mkSelector "setRenderingOrder:"

-- | @Selector@ for @castsShadow@
castsShadowSelector :: Selector
castsShadowSelector = mkSelector "castsShadow"

-- | @Selector@ for @setCastsShadow:@
setCastsShadowSelector :: Selector
setCastsShadowSelector = mkSelector "setCastsShadow:"

-- | @Selector@ for @movabilityHint@
movabilityHintSelector :: Selector
movabilityHintSelector = mkSelector "movabilityHint"

-- | @Selector@ for @setMovabilityHint:@
setMovabilityHintSelector :: Selector
setMovabilityHintSelector = mkSelector "setMovabilityHint:"

-- | @Selector@ for @parentNode@
parentNodeSelector :: Selector
parentNodeSelector = mkSelector "parentNode"

-- | @Selector@ for @childNodes@
childNodesSelector :: Selector
childNodesSelector = mkSelector "childNodes"

-- | @Selector@ for @physicsBody@
physicsBodySelector :: Selector
physicsBodySelector = mkSelector "physicsBody"

-- | @Selector@ for @setPhysicsBody:@
setPhysicsBodySelector :: Selector
setPhysicsBodySelector = mkSelector "setPhysicsBody:"

-- | @Selector@ for @physicsField@
physicsFieldSelector :: Selector
physicsFieldSelector = mkSelector "physicsField"

-- | @Selector@ for @setPhysicsField:@
setPhysicsFieldSelector :: Selector
setPhysicsFieldSelector = mkSelector "setPhysicsField:"

-- | @Selector@ for @constraints@
constraintsSelector :: Selector
constraintsSelector = mkSelector "constraints"

-- | @Selector@ for @setConstraints:@
setConstraintsSelector :: Selector
setConstraintsSelector = mkSelector "setConstraints:"

-- | @Selector@ for @presentationNode@
presentationNodeSelector :: Selector
presentationNodeSelector = mkSelector "presentationNode"

-- | @Selector@ for @paused@
pausedSelector :: Selector
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector
setPausedSelector = mkSelector "setPaused:"

-- | @Selector@ for @rendererDelegate@
rendererDelegateSelector :: Selector
rendererDelegateSelector = mkSelector "rendererDelegate"

-- | @Selector@ for @setRendererDelegate:@
setRendererDelegateSelector :: Selector
setRendererDelegateSelector = mkSelector "setRendererDelegate:"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

-- | @Selector@ for @audioPlayers@
audioPlayersSelector :: Selector
audioPlayersSelector = mkSelector "audioPlayers"

-- | @Selector@ for @particleSystems@
particleSystemsSelector :: Selector
particleSystemsSelector = mkSelector "particleSystems"

-- | @Selector@ for @focusBehavior@
focusBehaviorSelector :: Selector
focusBehaviorSelector = mkSelector "focusBehavior"

-- | @Selector@ for @setFocusBehavior:@
setFocusBehaviorSelector :: Selector
setFocusBehaviorSelector = mkSelector "setFocusBehavior:"

-- | @Selector@ for @localUp@
localUpSelector :: Selector
localUpSelector = mkSelector "localUp"

-- | @Selector@ for @localRight@
localRightSelector :: Selector
localRightSelector = mkSelector "localRight"

-- | @Selector@ for @localFront@
localFrontSelector :: Selector
localFrontSelector = mkSelector "localFront"

-- | @Selector@ for @worldUp@
worldUpSelector :: Selector
worldUpSelector = mkSelector "worldUp"

-- | @Selector@ for @worldRight@
worldRightSelector :: Selector
worldRightSelector = mkSelector "worldRight"

-- | @Selector@ for @worldFront@
worldFrontSelector :: Selector
worldFrontSelector = mkSelector "worldFront"

