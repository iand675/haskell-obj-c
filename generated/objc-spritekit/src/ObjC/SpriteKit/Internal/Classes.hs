{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.SpriteKit.Internal.Classes (
    module ObjC.SpriteKit.Internal.Classes,
    module ObjC.AVFAudio.Internal.Classes,
    module ObjC.AVFoundation.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.CoreImage.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.GameplayKit.Internal.Classes,
    module ObjC.Metal.Internal.Classes,
    module ObjC.SceneKit.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.GameplayKit.Internal.Classes
import ObjC.Metal.Internal.Classes
import ObjC.SceneKit.Internal.Classes

-- ---------- SKAction ----------

-- | An SKAction object is an action that is executed by a node in the scene. Actions are most often used to change the structure and content of the node to which they are attached, but can also make other changes to the scene. When the scene processes its nodes, actions associated with those nodes are evaluated.
-- 
-- Phantom type for @SKAction@.
data SKAction

instance IsObjCObject (Id SKAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKAction"

class IsNSObject a => IsSKAction a where
  toSKAction :: a -> Id SKAction

instance IsSKAction (Id SKAction) where
  toSKAction = unsafeCastId

instance IsNSObject (Id SKAction) where
  toNSObject = unsafeCastId

-- ---------- SKAttribute ----------

-- | Phantom type for @SKAttribute@.
data SKAttribute

instance IsObjCObject (Id SKAttribute) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKAttribute"

class IsNSObject a => IsSKAttribute a where
  toSKAttribute :: a -> Id SKAttribute

instance IsSKAttribute (Id SKAttribute) where
  toSKAttribute = unsafeCastId

instance IsNSObject (Id SKAttribute) where
  toNSObject = unsafeCastId

-- ---------- SKAttributeValue ----------

-- | Phantom type for @SKAttributeValue@.
data SKAttributeValue

instance IsObjCObject (Id SKAttributeValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKAttributeValue"

class IsNSObject a => IsSKAttributeValue a where
  toSKAttributeValue :: a -> Id SKAttributeValue

instance IsSKAttributeValue (Id SKAttributeValue) where
  toSKAttributeValue = unsafeCastId

instance IsNSObject (Id SKAttributeValue) where
  toNSObject = unsafeCastId

-- ---------- SKConstraint ----------

-- | SKConstraints are evaluated each frame after actions and physics. The node's transform will be changed to staisfy the constarint
-- 
-- Phantom type for @SKConstraint@.
data SKConstraint

instance IsObjCObject (Id SKConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKConstraint"

class IsNSObject a => IsSKConstraint a where
  toSKConstraint :: a -> Id SKConstraint

instance IsSKConstraint (Id SKConstraint) where
  toSKConstraint = unsafeCastId

instance IsNSObject (Id SKConstraint) where
  toNSObject = unsafeCastId

-- ---------- SKKeyframeSequence ----------

-- | Phantom type for @SKKeyframeSequence@.
data SKKeyframeSequence

instance IsObjCObject (Id SKKeyframeSequence) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKKeyframeSequence"

class IsNSObject a => IsSKKeyframeSequence a where
  toSKKeyframeSequence :: a -> Id SKKeyframeSequence

instance IsSKKeyframeSequence (Id SKKeyframeSequence) where
  toSKKeyframeSequence = unsafeCastId

instance IsNSObject (Id SKKeyframeSequence) where
  toNSObject = unsafeCastId

-- ---------- SKPhysicsBody ----------

-- | A SpriteKit physics body. These are the physical representations of your nodes. These specify the area and mass and any collision masking needed.
--
-- All bodies have zero, one or more shapes that define its area. A body with no shapes is ethereal and does not collide with other bodies.
-- 
-- Phantom type for @SKPhysicsBody@.
data SKPhysicsBody

instance IsObjCObject (Id SKPhysicsBody) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKPhysicsBody"

class IsNSObject a => IsSKPhysicsBody a where
  toSKPhysicsBody :: a -> Id SKPhysicsBody

instance IsSKPhysicsBody (Id SKPhysicsBody) where
  toSKPhysicsBody = unsafeCastId

instance IsNSObject (Id SKPhysicsBody) where
  toNSObject = unsafeCastId

-- ---------- SKPhysicsContact ----------

-- | Phantom type for @SKPhysicsContact@.
data SKPhysicsContact

instance IsObjCObject (Id SKPhysicsContact) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKPhysicsContact"

class IsNSObject a => IsSKPhysicsContact a where
  toSKPhysicsContact :: a -> Id SKPhysicsContact

instance IsSKPhysicsContact (Id SKPhysicsContact) where
  toSKPhysicsContact = unsafeCastId

instance IsNSObject (Id SKPhysicsContact) where
  toNSObject = unsafeCastId

-- ---------- SKPhysicsJoint ----------

-- | Phantom type for @SKPhysicsJoint@.
data SKPhysicsJoint

instance IsObjCObject (Id SKPhysicsJoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKPhysicsJoint"

class IsNSObject a => IsSKPhysicsJoint a where
  toSKPhysicsJoint :: a -> Id SKPhysicsJoint

instance IsSKPhysicsJoint (Id SKPhysicsJoint) where
  toSKPhysicsJoint = unsafeCastId

instance IsNSObject (Id SKPhysicsJoint) where
  toNSObject = unsafeCastId

-- ---------- SKPhysicsWorld ----------

-- | Phantom type for @SKPhysicsWorld@.
data SKPhysicsWorld

instance IsObjCObject (Id SKPhysicsWorld) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKPhysicsWorld"

class IsNSObject a => IsSKPhysicsWorld a where
  toSKPhysicsWorld :: a -> Id SKPhysicsWorld

instance IsSKPhysicsWorld (Id SKPhysicsWorld) where
  toSKPhysicsWorld = unsafeCastId

instance IsNSObject (Id SKPhysicsWorld) where
  toNSObject = unsafeCastId

-- ---------- SKRange ----------

-- | SKRange object used to define a range of allowable values
-- 
-- Phantom type for @SKRange@.
data SKRange

instance IsObjCObject (Id SKRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKRange"

class IsNSObject a => IsSKRange a where
  toSKRange :: a -> Id SKRange

instance IsSKRange (Id SKRange) where
  toSKRange = unsafeCastId

instance IsNSObject (Id SKRange) where
  toNSObject = unsafeCastId

-- ---------- SKReachConstraints ----------

-- | Phantom type for @SKReachConstraints@.
data SKReachConstraints

instance IsObjCObject (Id SKReachConstraints) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKReachConstraints"

class IsNSObject a => IsSKReachConstraints a where
  toSKReachConstraints :: a -> Id SKReachConstraints

instance IsSKReachConstraints (Id SKReachConstraints) where
  toSKReachConstraints = unsafeCastId

instance IsNSObject (Id SKReachConstraints) where
  toNSObject = unsafeCastId

-- ---------- SKRegion ----------

-- | Phantom type for @SKRegion@.
data SKRegion

instance IsObjCObject (Id SKRegion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKRegion"

class IsNSObject a => IsSKRegion a where
  toSKRegion :: a -> Id SKRegion

instance IsSKRegion (Id SKRegion) where
  toSKRegion = unsafeCastId

instance IsNSObject (Id SKRegion) where
  toNSObject = unsafeCastId

-- ---------- SKRenderer ----------

-- | A renderer for displaying a SpriteKit scene in an existing Metal workflow.
-- 
-- Phantom type for @SKRenderer@.
data SKRenderer

instance IsObjCObject (Id SKRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKRenderer"

class IsNSObject a => IsSKRenderer a where
  toSKRenderer :: a -> Id SKRenderer

instance IsSKRenderer (Id SKRenderer) where
  toSKRenderer = unsafeCastId

instance IsNSObject (Id SKRenderer) where
  toNSObject = unsafeCastId

-- ---------- SKShader ----------

-- | Phantom type for @SKShader@.
data SKShader

instance IsObjCObject (Id SKShader) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKShader"

class IsNSObject a => IsSKShader a where
  toSKShader :: a -> Id SKShader

instance IsSKShader (Id SKShader) where
  toSKShader = unsafeCastId

instance IsNSObject (Id SKShader) where
  toNSObject = unsafeCastId

-- ---------- SKTexture ----------

-- | A texture to be mapped onto SKSpriteNode instances.
-- 
-- Phantom type for @SKTexture@.
data SKTexture

instance IsObjCObject (Id SKTexture) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKTexture"

class IsNSObject a => IsSKTexture a where
  toSKTexture :: a -> Id SKTexture

instance IsSKTexture (Id SKTexture) where
  toSKTexture = unsafeCastId

instance IsNSObject (Id SKTexture) where
  toNSObject = unsafeCastId

-- ---------- SKTextureAtlas ----------

-- | Phantom type for @SKTextureAtlas@.
data SKTextureAtlas

instance IsObjCObject (Id SKTextureAtlas) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKTextureAtlas"

class IsNSObject a => IsSKTextureAtlas a where
  toSKTextureAtlas :: a -> Id SKTextureAtlas

instance IsSKTextureAtlas (Id SKTextureAtlas) where
  toSKTextureAtlas = unsafeCastId

instance IsNSObject (Id SKTextureAtlas) where
  toNSObject = unsafeCastId

-- ---------- SKTileDefinition ----------

-- | A tile definition contains the information needed to represent a single type of tile within a tile map.
-- 
-- Phantom type for @SKTileDefinition@.
data SKTileDefinition

instance IsObjCObject (Id SKTileDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKTileDefinition"

class IsNSObject a => IsSKTileDefinition a where
  toSKTileDefinition :: a -> Id SKTileDefinition

instance IsSKTileDefinition (Id SKTileDefinition) where
  toSKTileDefinition = unsafeCastId

instance IsNSObject (Id SKTileDefinition) where
  toNSObject = unsafeCastId

-- ---------- SKTileGroup ----------

-- | A tile group encapsulates a collection of related tile definitions that are designed to be pieced together within a tile map. How those tiles are pieced together is governed by the set of rules. When a tile group is placed in a tile map, the map evaluates the rules to determine which tiles should be placed to achieve the desired outcome.
-- 
-- Phantom type for @SKTileGroup@.
data SKTileGroup

instance IsObjCObject (Id SKTileGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKTileGroup"

class IsNSObject a => IsSKTileGroup a where
  toSKTileGroup :: a -> Id SKTileGroup

instance IsSKTileGroup (Id SKTileGroup) where
  toSKTileGroup = unsafeCastId

instance IsNSObject (Id SKTileGroup) where
  toNSObject = unsafeCastId

-- ---------- SKTileGroupRule ----------

-- | A tile group rule defines how a certain type of tile should be placed on the map. These tiles are like puzzle pieces, and the rules define how they should be pieced together. This is accomplished by defining which neighboring spaces need to be filled with tiles that belong to the same group, and which tiles are required to be empty. The required pattern of neighboring tiles is defined using the SKTileAdjacencyMask.
-- 
-- Phantom type for @SKTileGroupRule@.
data SKTileGroupRule

instance IsObjCObject (Id SKTileGroupRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKTileGroupRule"

class IsNSObject a => IsSKTileGroupRule a where
  toSKTileGroupRule :: a -> Id SKTileGroupRule

instance IsSKTileGroupRule (Id SKTileGroupRule) where
  toSKTileGroupRule = unsafeCastId

instance IsNSObject (Id SKTileGroupRule) where
  toNSObject = unsafeCastId

-- ---------- SKTileSet ----------

-- | A tile set contains all of the tile definitions that are available for use in a tile map. In addition, it also contains tile groups, which define collections of related tile definitions and the rules that govern their placement.
-- 
-- Phantom type for @SKTileSet@.
data SKTileSet

instance IsObjCObject (Id SKTileSet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKTileSet"

class IsNSObject a => IsSKTileSet a where
  toSKTileSet :: a -> Id SKTileSet

instance IsSKTileSet (Id SKTileSet) where
  toSKTileSet = unsafeCastId

instance IsNSObject (Id SKTileSet) where
  toNSObject = unsafeCastId

-- ---------- SKTransition ----------

-- | A transition style from one scene to another.
-- 
-- Phantom type for @SKTransition@.
data SKTransition

instance IsObjCObject (Id SKTransition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKTransition"

class IsNSObject a => IsSKTransition a where
  toSKTransition :: a -> Id SKTransition

instance IsSKTransition (Id SKTransition) where
  toSKTransition = unsafeCastId

instance IsNSObject (Id SKTransition) where
  toNSObject = unsafeCastId

-- ---------- SKUniform ----------

-- | Phantom type for @SKUniform@.
data SKUniform

instance IsObjCObject (Id SKUniform) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKUniform"

class IsNSObject a => IsSKUniform a where
  toSKUniform :: a -> Id SKUniform

instance IsSKUniform (Id SKUniform) where
  toSKUniform = unsafeCastId

instance IsNSObject (Id SKUniform) where
  toNSObject = unsafeCastId

-- ---------- SKWarpGeometry ----------

-- | Phantom type for @SKWarpGeometry@.
data SKWarpGeometry

instance IsObjCObject (Id SKWarpGeometry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKWarpGeometry"

class IsNSObject a => IsSKWarpGeometry a where
  toSKWarpGeometry :: a -> Id SKWarpGeometry

instance IsSKWarpGeometry (Id SKWarpGeometry) where
  toSKWarpGeometry = unsafeCastId

instance IsNSObject (Id SKWarpGeometry) where
  toNSObject = unsafeCastId

-- ---------- SKNode ----------

-- | Phantom type for @SKNode@.
data SKNode

instance IsObjCObject (Id SKNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKNode"

class IsNSResponder a => IsSKNode a where
  toSKNode :: a -> Id SKNode

instance IsSKNode (Id SKNode) where
  toSKNode = unsafeCastId

instance IsNSObject (Id SKNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKNode) where
  toNSResponder = unsafeCastId

-- ---------- SKPhysicsJointFixed ----------

-- | Phantom type for @SKPhysicsJointFixed@.
data SKPhysicsJointFixed

instance IsObjCObject (Id SKPhysicsJointFixed) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKPhysicsJointFixed"

class IsSKPhysicsJoint a => IsSKPhysicsJointFixed a where
  toSKPhysicsJointFixed :: a -> Id SKPhysicsJointFixed

instance IsSKPhysicsJointFixed (Id SKPhysicsJointFixed) where
  toSKPhysicsJointFixed = unsafeCastId

instance IsNSObject (Id SKPhysicsJointFixed) where
  toNSObject = unsafeCastId

instance IsSKPhysicsJoint (Id SKPhysicsJointFixed) where
  toSKPhysicsJoint = unsafeCastId

-- ---------- SKPhysicsJointLimit ----------

-- | Phantom type for @SKPhysicsJointLimit@.
data SKPhysicsJointLimit

instance IsObjCObject (Id SKPhysicsJointLimit) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKPhysicsJointLimit"

class IsSKPhysicsJoint a => IsSKPhysicsJointLimit a where
  toSKPhysicsJointLimit :: a -> Id SKPhysicsJointLimit

instance IsSKPhysicsJointLimit (Id SKPhysicsJointLimit) where
  toSKPhysicsJointLimit = unsafeCastId

instance IsNSObject (Id SKPhysicsJointLimit) where
  toNSObject = unsafeCastId

instance IsSKPhysicsJoint (Id SKPhysicsJointLimit) where
  toSKPhysicsJoint = unsafeCastId

-- ---------- SKPhysicsJointPin ----------

-- | Phantom type for @SKPhysicsJointPin@.
data SKPhysicsJointPin

instance IsObjCObject (Id SKPhysicsJointPin) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKPhysicsJointPin"

class IsSKPhysicsJoint a => IsSKPhysicsJointPin a where
  toSKPhysicsJointPin :: a -> Id SKPhysicsJointPin

instance IsSKPhysicsJointPin (Id SKPhysicsJointPin) where
  toSKPhysicsJointPin = unsafeCastId

instance IsNSObject (Id SKPhysicsJointPin) where
  toNSObject = unsafeCastId

instance IsSKPhysicsJoint (Id SKPhysicsJointPin) where
  toSKPhysicsJoint = unsafeCastId

-- ---------- SKPhysicsJointSliding ----------

-- | Phantom type for @SKPhysicsJointSliding@.
data SKPhysicsJointSliding

instance IsObjCObject (Id SKPhysicsJointSliding) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKPhysicsJointSliding"

class IsSKPhysicsJoint a => IsSKPhysicsJointSliding a where
  toSKPhysicsJointSliding :: a -> Id SKPhysicsJointSliding

instance IsSKPhysicsJointSliding (Id SKPhysicsJointSliding) where
  toSKPhysicsJointSliding = unsafeCastId

instance IsNSObject (Id SKPhysicsJointSliding) where
  toNSObject = unsafeCastId

instance IsSKPhysicsJoint (Id SKPhysicsJointSliding) where
  toSKPhysicsJoint = unsafeCastId

-- ---------- SKPhysicsJointSpring ----------

-- | Phantom type for @SKPhysicsJointSpring@.
data SKPhysicsJointSpring

instance IsObjCObject (Id SKPhysicsJointSpring) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKPhysicsJointSpring"

class IsSKPhysicsJoint a => IsSKPhysicsJointSpring a where
  toSKPhysicsJointSpring :: a -> Id SKPhysicsJointSpring

instance IsSKPhysicsJointSpring (Id SKPhysicsJointSpring) where
  toSKPhysicsJointSpring = unsafeCastId

instance IsNSObject (Id SKPhysicsJointSpring) where
  toNSObject = unsafeCastId

instance IsSKPhysicsJoint (Id SKPhysicsJointSpring) where
  toSKPhysicsJoint = unsafeCastId

-- ---------- SKMutableTexture ----------

-- | Phantom type for @SKMutableTexture@.
data SKMutableTexture

instance IsObjCObject (Id SKMutableTexture) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKMutableTexture"

class IsSKTexture a => IsSKMutableTexture a where
  toSKMutableTexture :: a -> Id SKMutableTexture

instance IsSKMutableTexture (Id SKMutableTexture) where
  toSKMutableTexture = unsafeCastId

instance IsNSObject (Id SKMutableTexture) where
  toNSObject = unsafeCastId

instance IsSKTexture (Id SKMutableTexture) where
  toSKTexture = unsafeCastId

-- ---------- SKWarpGeometryGrid ----------

-- | Phantom type for @SKWarpGeometryGrid@.
data SKWarpGeometryGrid

instance IsObjCObject (Id SKWarpGeometryGrid) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKWarpGeometryGrid"

class IsSKWarpGeometry a => IsSKWarpGeometryGrid a where
  toSKWarpGeometryGrid :: a -> Id SKWarpGeometryGrid

instance IsSKWarpGeometryGrid (Id SKWarpGeometryGrid) where
  toSKWarpGeometryGrid = unsafeCastId

instance IsNSObject (Id SKWarpGeometryGrid) where
  toNSObject = unsafeCastId

instance IsSKWarpGeometry (Id SKWarpGeometryGrid) where
  toSKWarpGeometry = unsafeCastId

-- ---------- SKView ----------

-- | Phantom type for @SKView@.
data SKView

instance IsObjCObject (Id SKView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKView"

class IsNSView a => IsSKView a where
  toSKView :: a -> Id SKView

instance IsSKView (Id SKView) where
  toSKView = unsafeCastId

instance IsNSObject (Id SKView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id SKView) where
  toNSView = unsafeCastId

-- ---------- SK3DNode ----------

-- | Phantom type for @SK3DNode@.
data SK3DNode

instance IsObjCObject (Id SK3DNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SK3DNode"

class IsSKNode a => IsSK3DNode a where
  toSK3DNode :: a -> Id SK3DNode

instance IsSK3DNode (Id SK3DNode) where
  toSK3DNode = unsafeCastId

instance IsNSObject (Id SK3DNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SK3DNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SK3DNode) where
  toSKNode = unsafeCastId

-- ---------- SKAudioNode ----------

-- | A SpriteKit scene graph audio node that provides a way to link audio graphs to a SpriteKit scene. The currently presented scene is responsible for mixing the audio from nodes in the scene.
--
-- Positional sounds will use their relative location and velocity to the scene's listener to apply distance attenuation, doppler shift and pan.
--
-- See: AVAudio3DMixing
--
-- See: SKScene.listener
-- 
-- Phantom type for @SKAudioNode@.
data SKAudioNode

instance IsObjCObject (Id SKAudioNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKAudioNode"

class IsSKNode a => IsSKAudioNode a where
  toSKAudioNode :: a -> Id SKAudioNode

instance IsSKAudioNode (Id SKAudioNode) where
  toSKAudioNode = unsafeCastId

instance IsNSObject (Id SKAudioNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKAudioNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKAudioNode) where
  toSKNode = unsafeCastId

-- ---------- SKCameraNode ----------

-- | A Camera node is a full fledged SKNode that can have actions and physics applied to it. It also uses the standard SKNode transform system so modifying the camera node's position is how you translate the camera's viewport. Applying a scale to the node would zoom the viewport in or out etc. As an added benefit you can now rotate the viewport by applying a zRotation to the camera node, just as you would with any other SKNode.
--
-- The camera viewport is centered on the camera's position. It uses the scene's frame and scale mode along with the node transforms to determine the size, origin and rotation of the viewport.
--
-- There are some convenience functions included for testing if nodes are contained within the camera viewport. It can be used to determine if objects are no longer visible on the display.
--
-- In order to use a camera; set it on the scene that contains the camera.
--
-- See: SKScene.camera
-- 
-- Phantom type for @SKCameraNode@.
data SKCameraNode

instance IsObjCObject (Id SKCameraNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKCameraNode"

class IsSKNode a => IsSKCameraNode a where
  toSKCameraNode :: a -> Id SKCameraNode

instance IsSKCameraNode (Id SKCameraNode) where
  toSKCameraNode = unsafeCastId

instance IsNSObject (Id SKCameraNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKCameraNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKCameraNode) where
  toSKNode = unsafeCastId

-- ---------- SKCropNode ----------

-- | A SpriteKit node that masks child nodes using another node's alpha component
-- 
-- Phantom type for @SKCropNode@.
data SKCropNode

instance IsObjCObject (Id SKCropNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKCropNode"

class IsSKNode a => IsSKCropNode a where
  toSKCropNode :: a -> Id SKCropNode

instance IsSKCropNode (Id SKCropNode) where
  toSKCropNode = unsafeCastId

instance IsNSObject (Id SKCropNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKCropNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKCropNode) where
  toSKNode = unsafeCastId

-- ---------- SKEffectNode ----------

-- | A SpriteKit node that applies frame buffer effects to the rendered results of its child nodes. This is done continuously on live content and is not a simple snapshot of the rendered result at one instant of time.
-- 
-- Phantom type for @SKEffectNode@.
data SKEffectNode

instance IsObjCObject (Id SKEffectNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKEffectNode"

class IsSKNode a => IsSKEffectNode a where
  toSKEffectNode :: a -> Id SKEffectNode

instance IsSKEffectNode (Id SKEffectNode) where
  toSKEffectNode = unsafeCastId

instance IsNSObject (Id SKEffectNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKEffectNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKEffectNode) where
  toSKNode = unsafeCastId

-- ---------- SKEmitterNode ----------

-- | An emitter of particle sprites.
-- 
-- Phantom type for @SKEmitterNode@.
data SKEmitterNode

instance IsObjCObject (Id SKEmitterNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKEmitterNode"

class IsSKNode a => IsSKEmitterNode a where
  toSKEmitterNode :: a -> Id SKEmitterNode

instance IsSKEmitterNode (Id SKEmitterNode) where
  toSKEmitterNode = unsafeCastId

instance IsNSObject (Id SKEmitterNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKEmitterNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKEmitterNode) where
  toSKNode = unsafeCastId

-- ---------- SKFieldNode ----------

-- | Phantom type for @SKFieldNode@.
data SKFieldNode

instance IsObjCObject (Id SKFieldNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKFieldNode"

class IsSKNode a => IsSKFieldNode a where
  toSKFieldNode :: a -> Id SKFieldNode

instance IsSKFieldNode (Id SKFieldNode) where
  toSKFieldNode = unsafeCastId

instance IsNSObject (Id SKFieldNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKFieldNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKFieldNode) where
  toSKNode = unsafeCastId

-- ---------- SKLabelNode ----------

-- | A node that displays a text label with a given font.
-- 
-- Phantom type for @SKLabelNode@.
data SKLabelNode

instance IsObjCObject (Id SKLabelNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKLabelNode"

class IsSKNode a => IsSKLabelNode a where
  toSKLabelNode :: a -> Id SKLabelNode

instance IsSKLabelNode (Id SKLabelNode) where
  toSKLabelNode = unsafeCastId

instance IsNSObject (Id SKLabelNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKLabelNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKLabelNode) where
  toSKNode = unsafeCastId

-- ---------- SKLightNode ----------

-- | Phantom type for @SKLightNode@.
data SKLightNode

instance IsObjCObject (Id SKLightNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKLightNode"

class IsSKNode a => IsSKLightNode a where
  toSKLightNode :: a -> Id SKLightNode

instance IsSKLightNode (Id SKLightNode) where
  toSKLightNode = unsafeCastId

instance IsNSObject (Id SKLightNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKLightNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKLightNode) where
  toSKNode = unsafeCastId

-- ---------- SKReferenceNode ----------

-- | Phantom type for @SKReferenceNode@.
data SKReferenceNode

instance IsObjCObject (Id SKReferenceNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKReferenceNode"

class IsSKNode a => IsSKReferenceNode a where
  toSKReferenceNode :: a -> Id SKReferenceNode

instance IsSKReferenceNode (Id SKReferenceNode) where
  toSKReferenceNode = unsafeCastId

instance IsNSObject (Id SKReferenceNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKReferenceNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKReferenceNode) where
  toSKNode = unsafeCastId

-- ---------- SKShapeNode ----------

-- | A SpriteKit Node used to stroke or fill a shape. CGPaths are used to supply the path.
--
-- See CGPath reference pages for details on how to construct a CGPath.
-- 
-- Phantom type for @SKShapeNode@.
data SKShapeNode

instance IsObjCObject (Id SKShapeNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKShapeNode"

class IsSKNode a => IsSKShapeNode a where
  toSKShapeNode :: a -> Id SKShapeNode

instance IsSKShapeNode (Id SKShapeNode) where
  toSKShapeNode = unsafeCastId

instance IsNSObject (Id SKShapeNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKShapeNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKShapeNode) where
  toSKNode = unsafeCastId

-- ---------- SKSpriteNode ----------

-- | A Sprite is a textured 2D node. It can be placed, rotated, scaled and animated like any other node except it draws a textured rectangle specified by the bounds and anchor point.
--
-- Sprites are used to define quad primitives with color and/or textures applied to them. See wiki for a definition of a Sprite.
-- 
-- Phantom type for @SKSpriteNode@.
data SKSpriteNode

instance IsObjCObject (Id SKSpriteNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKSpriteNode"

class IsSKNode a => IsSKSpriteNode a where
  toSKSpriteNode :: a -> Id SKSpriteNode

instance IsSKSpriteNode (Id SKSpriteNode) where
  toSKSpriteNode = unsafeCastId

instance IsNSObject (Id SKSpriteNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKSpriteNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKSpriteNode) where
  toSKNode = unsafeCastId

-- ---------- SKTileMapNode ----------

-- | A SpriteKit node used to render a 2D array of textured sprites. Uses SKTileSet to determine what textures it can use to render. Separate tile map nodes can be layered on top of one another to achieve various effects, such as parallax scrolling.
-- 
-- Phantom type for @SKTileMapNode@.
data SKTileMapNode

instance IsObjCObject (Id SKTileMapNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKTileMapNode"

class IsSKNode a => IsSKTileMapNode a where
  toSKTileMapNode :: a -> Id SKTileMapNode

instance IsSKTileMapNode (Id SKTileMapNode) where
  toSKTileMapNode = unsafeCastId

instance IsNSObject (Id SKTileMapNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKTileMapNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKTileMapNode) where
  toSKNode = unsafeCastId

-- ---------- SKTransformNode ----------

-- | An SKTransformNode can be applied a 3D rotation that will affect the visual aspect of its children. The physics and constraints of the children will behave as if none of them were transformed.
-- 
-- Phantom type for @SKTransformNode@.
data SKTransformNode

instance IsObjCObject (Id SKTransformNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKTransformNode"

class IsSKNode a => IsSKTransformNode a where
  toSKTransformNode :: a -> Id SKTransformNode

instance IsSKTransformNode (Id SKTransformNode) where
  toSKTransformNode = unsafeCastId

instance IsNSObject (Id SKTransformNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKTransformNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKTransformNode) where
  toSKNode = unsafeCastId

-- ---------- SKVideoNode ----------

-- | Phantom type for @SKVideoNode@.
data SKVideoNode

instance IsObjCObject (Id SKVideoNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKVideoNode"

class IsSKNode a => IsSKVideoNode a where
  toSKVideoNode :: a -> Id SKVideoNode

instance IsSKVideoNode (Id SKVideoNode) where
  toSKVideoNode = unsafeCastId

instance IsNSObject (Id SKVideoNode) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKVideoNode) where
  toNSResponder = unsafeCastId

instance IsSKNode (Id SKVideoNode) where
  toSKNode = unsafeCastId

-- ---------- SKScene ----------

-- | A scene is the root node of your content. It is used to display SpriteKit content on an SKView.
--
-- See: SKView
-- 
-- Phantom type for @SKScene@.
data SKScene

instance IsObjCObject (Id SKScene) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SKScene"

class IsSKEffectNode a => IsSKScene a where
  toSKScene :: a -> Id SKScene

instance IsSKScene (Id SKScene) where
  toSKScene = unsafeCastId

instance IsNSObject (Id SKScene) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SKScene) where
  toNSResponder = unsafeCastId

instance IsSKEffectNode (Id SKScene) where
  toSKEffectNode = unsafeCastId

instance IsSKNode (Id SKScene) where
  toSKNode = unsafeCastId
