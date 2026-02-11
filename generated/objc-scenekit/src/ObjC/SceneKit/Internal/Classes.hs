{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.SceneKit.Internal.Classes (
    module ObjC.SceneKit.Internal.Classes,
    module ObjC.AVFAudio.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.Metal.Internal.Classes,
    module ObjC.QuartzCore.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFAudio.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Metal.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- ---------- SCNAction ----------

-- | Phantom type for @SCNAction@.
data SCNAction

instance IsObjCObject (Id SCNAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNAction"

class IsNSObject a => IsSCNAction a where
  toSCNAction :: a -> Id SCNAction

instance IsSCNAction (Id SCNAction) where
  toSCNAction = unsafeCastId

instance IsNSObject (Id SCNAction) where
  toNSObject = unsafeCastId

-- ---------- SCNAnimation ----------

-- | SCNAnimation represents an animation that targets a specific key path.
-- 
-- Phantom type for @SCNAnimation@.
data SCNAnimation

instance IsObjCObject (Id SCNAnimation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNAnimation"

class IsNSObject a => IsSCNAnimation a where
  toSCNAnimation :: a -> Id SCNAnimation

instance IsSCNAnimation (Id SCNAnimation) where
  toSCNAnimation = unsafeCastId

instance IsNSObject (Id SCNAnimation) where
  toNSObject = unsafeCastId

-- ---------- SCNAnimationEvent ----------

-- | SCNAnimationEvent encapsulates a block to trigger at a specific time.
-- 
-- Phantom type for @SCNAnimationEvent@.
data SCNAnimationEvent

instance IsObjCObject (Id SCNAnimationEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNAnimationEvent"

class IsNSObject a => IsSCNAnimationEvent a where
  toSCNAnimationEvent :: a -> Id SCNAnimationEvent

instance IsSCNAnimationEvent (Id SCNAnimationEvent) where
  toSCNAnimationEvent = unsafeCastId

instance IsNSObject (Id SCNAnimationEvent) where
  toNSObject = unsafeCastId

-- ---------- SCNAnimationPlayer ----------

-- | SCNAnimationPlayer let you control when and how to play and blend an animation
-- 
-- Phantom type for @SCNAnimationPlayer@.
data SCNAnimationPlayer

instance IsObjCObject (Id SCNAnimationPlayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNAnimationPlayer"

class IsNSObject a => IsSCNAnimationPlayer a where
  toSCNAnimationPlayer :: a -> Id SCNAnimationPlayer

instance IsSCNAnimationPlayer (Id SCNAnimationPlayer) where
  toSCNAnimationPlayer = unsafeCastId

instance IsNSObject (Id SCNAnimationPlayer) where
  toNSObject = unsafeCastId

-- ---------- SCNAudioPlayer ----------

-- | Phantom type for @SCNAudioPlayer@.
data SCNAudioPlayer

instance IsObjCObject (Id SCNAudioPlayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNAudioPlayer"

class IsNSObject a => IsSCNAudioPlayer a where
  toSCNAudioPlayer :: a -> Id SCNAudioPlayer

instance IsSCNAudioPlayer (Id SCNAudioPlayer) where
  toSCNAudioPlayer = unsafeCastId

instance IsNSObject (Id SCNAudioPlayer) where
  toNSObject = unsafeCastId

-- ---------- SCNAudioSource ----------

-- | SCNAudioSource
--
-- The SCNAudioSource class represents an audio source that can be added to a SCNNode.
-- 
-- Phantom type for @SCNAudioSource@.
data SCNAudioSource

instance IsObjCObject (Id SCNAudioSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNAudioSource"

class IsNSObject a => IsSCNAudioSource a where
  toSCNAudioSource :: a -> Id SCNAudioSource

instance IsSCNAudioSource (Id SCNAudioSource) where
  toSCNAudioSource = unsafeCastId

instance IsNSObject (Id SCNAudioSource) where
  toNSObject = unsafeCastId

-- ---------- SCNCamera ----------

-- | SCNCamera
--
-- SCNCamera represents a camera that can be attached to a SCNNode.
--
-- A node with a camera can be used as a point of view to visualize a 3D scene.
-- 
-- Phantom type for @SCNCamera@.
data SCNCamera

instance IsObjCObject (Id SCNCamera) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNCamera"

class IsNSObject a => IsSCNCamera a where
  toSCNCamera :: a -> Id SCNCamera

instance IsSCNCamera (Id SCNCamera) where
  toSCNCamera = unsafeCastId

instance IsNSObject (Id SCNCamera) where
  toNSObject = unsafeCastId

-- ---------- SCNCameraController ----------

-- | Phantom type for @SCNCameraController@.
data SCNCameraController

instance IsObjCObject (Id SCNCameraController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNCameraController"

class IsNSObject a => IsSCNCameraController a where
  toSCNCameraController :: a -> Id SCNCameraController

instance IsSCNCameraController (Id SCNCameraController) where
  toSCNCameraController = unsafeCastId

instance IsNSObject (Id SCNCameraController) where
  toNSObject = unsafeCastId

-- ---------- SCNConstraint ----------

-- | SCNConstraint
--
-- A SCNConstraint is an abstract class that represents a single constraint that can be applied to a node.
-- 
-- Phantom type for @SCNConstraint@.
data SCNConstraint

instance IsObjCObject (Id SCNConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNConstraint"

class IsNSObject a => IsSCNConstraint a where
  toSCNConstraint :: a -> Id SCNConstraint

instance IsSCNConstraint (Id SCNConstraint) where
  toSCNConstraint = unsafeCastId

instance IsNSObject (Id SCNConstraint) where
  toNSObject = unsafeCastId

-- ---------- SCNGeometry ----------

-- | SCNGeometry
--
-- SCNGeometry is an abstract class that represents the geometry that can be attached to a SCNNode.
-- 
-- Phantom type for @SCNGeometry@.
data SCNGeometry

instance IsObjCObject (Id SCNGeometry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNGeometry"

class IsNSObject a => IsSCNGeometry a where
  toSCNGeometry :: a -> Id SCNGeometry

instance IsSCNGeometry (Id SCNGeometry) where
  toSCNGeometry = unsafeCastId

instance IsNSObject (Id SCNGeometry) where
  toNSObject = unsafeCastId

-- ---------- SCNGeometryElement ----------

-- | SCNGeometryElement
--
-- A geometry element describes how vertices from a geometry source are connected together.
-- 
-- Phantom type for @SCNGeometryElement@.
data SCNGeometryElement

instance IsObjCObject (Id SCNGeometryElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNGeometryElement"

class IsNSObject a => IsSCNGeometryElement a where
  toSCNGeometryElement :: a -> Id SCNGeometryElement

instance IsSCNGeometryElement (Id SCNGeometryElement) where
  toSCNGeometryElement = unsafeCastId

instance IsNSObject (Id SCNGeometryElement) where
  toNSObject = unsafeCastId

-- ---------- SCNGeometrySource ----------

-- | SCNGeometrySource
--
-- A geometry source contains geometry data for a specific semantic. The data format is described by properties.
-- 
-- Phantom type for @SCNGeometrySource@.
data SCNGeometrySource

instance IsObjCObject (Id SCNGeometrySource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNGeometrySource"

class IsNSObject a => IsSCNGeometrySource a where
  toSCNGeometrySource :: a -> Id SCNGeometrySource

instance IsSCNGeometrySource (Id SCNGeometrySource) where
  toSCNGeometrySource = unsafeCastId

instance IsNSObject (Id SCNGeometrySource) where
  toNSObject = unsafeCastId

-- ---------- SCNGeometryTessellator ----------

-- | SCNGeometryTessellator
--
-- A geometry tessellator describes how a more detailed surface is calculated from the geometry's initial surface.
-- 
-- Phantom type for @SCNGeometryTessellator@.
data SCNGeometryTessellator

instance IsObjCObject (Id SCNGeometryTessellator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNGeometryTessellator"

class IsNSObject a => IsSCNGeometryTessellator a where
  toSCNGeometryTessellator :: a -> Id SCNGeometryTessellator

instance IsSCNGeometryTessellator (Id SCNGeometryTessellator) where
  toSCNGeometryTessellator = unsafeCastId

instance IsNSObject (Id SCNGeometryTessellator) where
  toNSObject = unsafeCastId

-- ---------- SCNHitTestResult ----------

-- | SCNHitTestResult
--
-- Results returned by the hit-test methods.
-- 
-- Phantom type for @SCNHitTestResult@.
data SCNHitTestResult

instance IsObjCObject (Id SCNHitTestResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNHitTestResult"

class IsNSObject a => IsSCNHitTestResult a where
  toSCNHitTestResult :: a -> Id SCNHitTestResult

instance IsSCNHitTestResult (Id SCNHitTestResult) where
  toSCNHitTestResult = unsafeCastId

instance IsNSObject (Id SCNHitTestResult) where
  toNSObject = unsafeCastId

-- ---------- SCNLevelOfDetail ----------

-- | SCNLevelOfDetail
--
-- SCNLevelOfDetail represents a level of detail of a geometry.
-- 
-- Phantom type for @SCNLevelOfDetail@.
data SCNLevelOfDetail

instance IsObjCObject (Id SCNLevelOfDetail) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNLevelOfDetail"

class IsNSObject a => IsSCNLevelOfDetail a where
  toSCNLevelOfDetail :: a -> Id SCNLevelOfDetail

instance IsSCNLevelOfDetail (Id SCNLevelOfDetail) where
  toSCNLevelOfDetail = unsafeCastId

instance IsNSObject (Id SCNLevelOfDetail) where
  toNSObject = unsafeCastId

-- ---------- SCNLight ----------

-- | SCNLight
--
-- SCNLight represents a light that can be attached to a SCNNode.
-- 
-- Phantom type for @SCNLight@.
data SCNLight

instance IsObjCObject (Id SCNLight) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNLight"

class IsNSObject a => IsSCNLight a where
  toSCNLight :: a -> Id SCNLight

instance IsSCNLight (Id SCNLight) where
  toSCNLight = unsafeCastId

instance IsNSObject (Id SCNLight) where
  toNSObject = unsafeCastId

-- ---------- SCNMaterial ----------

-- | SCNMaterial
--
-- A SCNMaterial determines how a geometry is rendered. It encapsulates the colors and textures that define the appearance of 3d geometries.
-- 
-- Phantom type for @SCNMaterial@.
data SCNMaterial

instance IsObjCObject (Id SCNMaterial) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNMaterial"

class IsNSObject a => IsSCNMaterial a where
  toSCNMaterial :: a -> Id SCNMaterial

instance IsSCNMaterial (Id SCNMaterial) where
  toSCNMaterial = unsafeCastId

instance IsNSObject (Id SCNMaterial) where
  toNSObject = unsafeCastId

-- ---------- SCNMaterialProperty ----------

-- | SCNMaterialProperty
--
-- The contents of a SCNMaterial slot
--
-- This can be used to specify the various properties of SCNMaterial slots such as diffuse, ambient, etc.
-- 
-- Phantom type for @SCNMaterialProperty@.
data SCNMaterialProperty

instance IsObjCObject (Id SCNMaterialProperty) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNMaterialProperty"

class IsNSObject a => IsSCNMaterialProperty a where
  toSCNMaterialProperty :: a -> Id SCNMaterialProperty

instance IsSCNMaterialProperty (Id SCNMaterialProperty) where
  toSCNMaterialProperty = unsafeCastId

instance IsNSObject (Id SCNMaterialProperty) where
  toNSObject = unsafeCastId

-- ---------- SCNMorpher ----------

-- | SCNMorpher
--
-- SCNMorpher controls the deformation of morphed geometries
-- 
-- Phantom type for @SCNMorpher@.
data SCNMorpher

instance IsObjCObject (Id SCNMorpher) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNMorpher"

class IsNSObject a => IsSCNMorpher a where
  toSCNMorpher :: a -> Id SCNMorpher

instance IsSCNMorpher (Id SCNMorpher) where
  toSCNMorpher = unsafeCastId

instance IsNSObject (Id SCNMorpher) where
  toNSObject = unsafeCastId

-- ---------- SCNNode ----------

-- | SCNNode
--
-- SCNNode is the model class for node-tree objects.
--
-- It encapsulates the position, rotations, and other transforms of a node, which define a coordinate system.		     The coordinate systems of all the sub-nodes are relative to the one of their parent node.
-- 
-- Phantom type for @SCNNode@.
data SCNNode

instance IsObjCObject (Id SCNNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNNode"

class IsNSObject a => IsSCNNode a where
  toSCNNode :: a -> Id SCNNode

instance IsSCNNode (Id SCNNode) where
  toSCNNode = unsafeCastId

instance IsNSObject (Id SCNNode) where
  toNSObject = unsafeCastId

-- ---------- SCNParticlePropertyController ----------

-- | SCNParticlePropertyController
--
-- The SCNParticlePropertyController class controls the variation over time or over distance of a particle property.
-- 
-- Phantom type for @SCNParticlePropertyController@.
data SCNParticlePropertyController

instance IsObjCObject (Id SCNParticlePropertyController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNParticlePropertyController"

class IsNSObject a => IsSCNParticlePropertyController a where
  toSCNParticlePropertyController :: a -> Id SCNParticlePropertyController

instance IsSCNParticlePropertyController (Id SCNParticlePropertyController) where
  toSCNParticlePropertyController = unsafeCastId

instance IsNSObject (Id SCNParticlePropertyController) where
  toNSObject = unsafeCastId

-- ---------- SCNParticleSystem ----------

-- | SCNParticleSystem
--
-- The SCNParticleSystem class represents a system of particles.
-- 
-- Phantom type for @SCNParticleSystem@.
data SCNParticleSystem

instance IsObjCObject (Id SCNParticleSystem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNParticleSystem"

class IsNSObject a => IsSCNParticleSystem a where
  toSCNParticleSystem :: a -> Id SCNParticleSystem

instance IsSCNParticleSystem (Id SCNParticleSystem) where
  toSCNParticleSystem = unsafeCastId

instance IsNSObject (Id SCNParticleSystem) where
  toNSObject = unsafeCastId

-- ---------- SCNPhysicsBehavior ----------

-- | SCNPhysicsBehavior
--
-- SCNPhysicsBehavior is an abstract class that represents a behavior in the physics world.
-- 
-- Phantom type for @SCNPhysicsBehavior@.
data SCNPhysicsBehavior

instance IsObjCObject (Id SCNPhysicsBehavior) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPhysicsBehavior"

class IsNSObject a => IsSCNPhysicsBehavior a where
  toSCNPhysicsBehavior :: a -> Id SCNPhysicsBehavior

instance IsSCNPhysicsBehavior (Id SCNPhysicsBehavior) where
  toSCNPhysicsBehavior = unsafeCastId

instance IsNSObject (Id SCNPhysicsBehavior) where
  toNSObject = unsafeCastId

-- ---------- SCNPhysicsBody ----------

-- | SCNPhysicsBody
--
-- The SCNPhysicsBody class describes the physics properties (such as mass, friction...) of a node.
-- 
-- Phantom type for @SCNPhysicsBody@.
data SCNPhysicsBody

instance IsObjCObject (Id SCNPhysicsBody) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPhysicsBody"

class IsNSObject a => IsSCNPhysicsBody a where
  toSCNPhysicsBody :: a -> Id SCNPhysicsBody

instance IsSCNPhysicsBody (Id SCNPhysicsBody) where
  toSCNPhysicsBody = unsafeCastId

instance IsNSObject (Id SCNPhysicsBody) where
  toNSObject = unsafeCastId

-- ---------- SCNPhysicsContact ----------

-- | SCNPhysicsContact
--
-- SCNPhysicsContact contains information about a physics contact.
-- 
-- Phantom type for @SCNPhysicsContact@.
data SCNPhysicsContact

instance IsObjCObject (Id SCNPhysicsContact) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPhysicsContact"

class IsNSObject a => IsSCNPhysicsContact a where
  toSCNPhysicsContact :: a -> Id SCNPhysicsContact

instance IsSCNPhysicsContact (Id SCNPhysicsContact) where
  toSCNPhysicsContact = unsafeCastId

instance IsNSObject (Id SCNPhysicsContact) where
  toNSObject = unsafeCastId

-- ---------- SCNPhysicsField ----------

-- | SCNPhysicsField
--
-- SCNPhysicsField is an abstract class that describes a force field that applies in the physics world.
-- 
-- Phantom type for @SCNPhysicsField@.
data SCNPhysicsField

instance IsObjCObject (Id SCNPhysicsField) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPhysicsField"

class IsNSObject a => IsSCNPhysicsField a where
  toSCNPhysicsField :: a -> Id SCNPhysicsField

instance IsSCNPhysicsField (Id SCNPhysicsField) where
  toSCNPhysicsField = unsafeCastId

instance IsNSObject (Id SCNPhysicsField) where
  toNSObject = unsafeCastId

-- ---------- SCNPhysicsShape ----------

-- | SCNPhysicsShape
--
-- SCNPhysicsShape represents the shape of a physics body.
-- 
-- Phantom type for @SCNPhysicsShape@.
data SCNPhysicsShape

instance IsObjCObject (Id SCNPhysicsShape) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPhysicsShape"

class IsNSObject a => IsSCNPhysicsShape a where
  toSCNPhysicsShape :: a -> Id SCNPhysicsShape

instance IsSCNPhysicsShape (Id SCNPhysicsShape) where
  toSCNPhysicsShape = unsafeCastId

instance IsNSObject (Id SCNPhysicsShape) where
  toNSObject = unsafeCastId

-- ---------- SCNPhysicsVehicleWheel ----------

-- | SCNPhysicsVehicleWheel
--
-- SCNPhysicsVehicleWheel represents a wheel that can be attached to a SCNPhysicsVehicle instance.
-- 
-- Phantom type for @SCNPhysicsVehicleWheel@.
data SCNPhysicsVehicleWheel

instance IsObjCObject (Id SCNPhysicsVehicleWheel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPhysicsVehicleWheel"

class IsNSObject a => IsSCNPhysicsVehicleWheel a where
  toSCNPhysicsVehicleWheel :: a -> Id SCNPhysicsVehicleWheel

instance IsSCNPhysicsVehicleWheel (Id SCNPhysicsVehicleWheel) where
  toSCNPhysicsVehicleWheel = unsafeCastId

instance IsNSObject (Id SCNPhysicsVehicleWheel) where
  toNSObject = unsafeCastId

-- ---------- SCNPhysicsWorld ----------

-- | SCNPhysicsWorld
--
-- The SCNPhysicsWorld class describes and allows to control the physics simulation of a 3d scene.
--
-- The SCNPhysicsWorld class should not be allocated directly but retrieved from the SCNScene class using the physicsWorld property.
-- 
-- Phantom type for @SCNPhysicsWorld@.
data SCNPhysicsWorld

instance IsObjCObject (Id SCNPhysicsWorld) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPhysicsWorld"

class IsNSObject a => IsSCNPhysicsWorld a where
  toSCNPhysicsWorld :: a -> Id SCNPhysicsWorld

instance IsSCNPhysicsWorld (Id SCNPhysicsWorld) where
  toSCNPhysicsWorld = unsafeCastId

instance IsNSObject (Id SCNPhysicsWorld) where
  toNSObject = unsafeCastId

-- ---------- SCNProgram ----------

-- | SCNProgram
--
-- A SCNProgram lets you specify custom shaders to use when rendering materials.
-- 
-- Phantom type for @SCNProgram@.
data SCNProgram

instance IsObjCObject (Id SCNProgram) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNProgram"

class IsNSObject a => IsSCNProgram a where
  toSCNProgram :: a -> Id SCNProgram

instance IsSCNProgram (Id SCNProgram) where
  toSCNProgram = unsafeCastId

instance IsNSObject (Id SCNProgram) where
  toNSObject = unsafeCastId

-- ---------- SCNRenderer ----------

-- | SCNRenderer
--
-- SCNRenderer lets you use the SceneKit renderer in an OpenGL context or Metal render pass descriptor of your own.
-- 
-- Phantom type for @SCNRenderer@.
data SCNRenderer

instance IsObjCObject (Id SCNRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNRenderer"

class IsNSObject a => IsSCNRenderer a where
  toSCNRenderer :: a -> Id SCNRenderer

instance IsSCNRenderer (Id SCNRenderer) where
  toSCNRenderer = unsafeCastId

instance IsNSObject (Id SCNRenderer) where
  toNSObject = unsafeCastId

-- ---------- SCNScene ----------

-- | SCNScene
--
-- SCNScene is the class that describes a 3d scene. It encapsulates a node hierarchy.
-- 
-- Phantom type for @SCNScene@.
data SCNScene

instance IsObjCObject (Id SCNScene) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNScene"

class IsNSObject a => IsSCNScene a where
  toSCNScene :: a -> Id SCNScene

instance IsSCNScene (Id SCNScene) where
  toSCNScene = unsafeCastId

instance IsNSObject (Id SCNScene) where
  toNSObject = unsafeCastId

-- ---------- SCNSceneSource ----------

-- | SCNSceneSource
--
-- SCNSceneSource objects, abstract the data-reading task. A scene source can read scene data from a URL or a NSData object. After creating a SCNSceneSource object for the appropriate source, you can obtain scenes using SCNSceneSource methods.
-- 
-- Phantom type for @SCNSceneSource@.
data SCNSceneSource

instance IsObjCObject (Id SCNSceneSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNSceneSource"

class IsNSObject a => IsSCNSceneSource a where
  toSCNSceneSource :: a -> Id SCNSceneSource

instance IsSCNSceneSource (Id SCNSceneSource) where
  toSCNSceneSource = unsafeCastId

instance IsNSObject (Id SCNSceneSource) where
  toNSObject = unsafeCastId

-- ---------- SCNSkinner ----------

-- | SCNSkinner
--
-- SCNSkinner controls the deformation of skinned geometries
-- 
-- Phantom type for @SCNSkinner@.
data SCNSkinner

instance IsObjCObject (Id SCNSkinner) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNSkinner"

class IsNSObject a => IsSCNSkinner a where
  toSCNSkinner :: a -> Id SCNSkinner

instance IsSCNSkinner (Id SCNSkinner) where
  toSCNSkinner = unsafeCastId

instance IsNSObject (Id SCNSkinner) where
  toNSObject = unsafeCastId

-- ---------- SCNTechnique ----------

-- | SCNTechnique
--
-- SCNTechnique represents a rendering process that may require multiple passes.
--
-- A technique is generally initialized from a Property List file. It can be set to any object that conforms to the SCNTechniqueSupport protocol.
-- 
-- Phantom type for @SCNTechnique@.
data SCNTechnique

instance IsObjCObject (Id SCNTechnique) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNTechnique"

class IsNSObject a => IsSCNTechnique a where
  toSCNTechnique :: a -> Id SCNTechnique

instance IsSCNTechnique (Id SCNTechnique) where
  toSCNTechnique = unsafeCastId

instance IsNSObject (Id SCNTechnique) where
  toNSObject = unsafeCastId

-- ---------- SCNTimingFunction ----------

-- | Phantom type for @SCNTimingFunction@.
data SCNTimingFunction

instance IsObjCObject (Id SCNTimingFunction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNTimingFunction"

class IsNSObject a => IsSCNTimingFunction a where
  toSCNTimingFunction :: a -> Id SCNTimingFunction

instance IsSCNTimingFunction (Id SCNTimingFunction) where
  toSCNTimingFunction = unsafeCastId

instance IsNSObject (Id SCNTimingFunction) where
  toNSObject = unsafeCastId

-- ---------- SCNTransaction ----------

-- | Phantom type for @SCNTransaction@.
data SCNTransaction

instance IsObjCObject (Id SCNTransaction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNTransaction"

class IsNSObject a => IsSCNTransaction a where
  toSCNTransaction :: a -> Id SCNTransaction

instance IsSCNTransaction (Id SCNTransaction) where
  toSCNTransaction = unsafeCastId

instance IsNSObject (Id SCNTransaction) where
  toNSObject = unsafeCastId

-- ---------- SCNAccelerationConstraint ----------

-- | SCNAccelerationConstraint
--
-- A SCNAccelerationConstraint caps the acceleration and velocity of a node
-- 
-- Phantom type for @SCNAccelerationConstraint@.
data SCNAccelerationConstraint

instance IsObjCObject (Id SCNAccelerationConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNAccelerationConstraint"

class IsSCNConstraint a => IsSCNAccelerationConstraint a where
  toSCNAccelerationConstraint :: a -> Id SCNAccelerationConstraint

instance IsSCNAccelerationConstraint (Id SCNAccelerationConstraint) where
  toSCNAccelerationConstraint = unsafeCastId

instance IsNSObject (Id SCNAccelerationConstraint) where
  toNSObject = unsafeCastId

instance IsSCNConstraint (Id SCNAccelerationConstraint) where
  toSCNConstraint = unsafeCastId

-- ---------- SCNAvoidOccluderConstraint ----------

-- | SCNAvoidOccluderConstraint
--
-- A SCNAvoidOccluderConstraint constraints place the receiver at a position that prevent nodes with the specified category to occlude the target.
--
-- The target node and it's children are ignored as potential occluders.
-- 
-- Phantom type for @SCNAvoidOccluderConstraint@.
data SCNAvoidOccluderConstraint

instance IsObjCObject (Id SCNAvoidOccluderConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNAvoidOccluderConstraint"

class IsSCNConstraint a => IsSCNAvoidOccluderConstraint a where
  toSCNAvoidOccluderConstraint :: a -> Id SCNAvoidOccluderConstraint

instance IsSCNAvoidOccluderConstraint (Id SCNAvoidOccluderConstraint) where
  toSCNAvoidOccluderConstraint = unsafeCastId

instance IsNSObject (Id SCNAvoidOccluderConstraint) where
  toNSObject = unsafeCastId

instance IsSCNConstraint (Id SCNAvoidOccluderConstraint) where
  toSCNConstraint = unsafeCastId

-- ---------- SCNBillboardConstraint ----------

-- | Phantom type for @SCNBillboardConstraint@.
data SCNBillboardConstraint

instance IsObjCObject (Id SCNBillboardConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNBillboardConstraint"

class IsSCNConstraint a => IsSCNBillboardConstraint a where
  toSCNBillboardConstraint :: a -> Id SCNBillboardConstraint

instance IsSCNBillboardConstraint (Id SCNBillboardConstraint) where
  toSCNBillboardConstraint = unsafeCastId

instance IsNSObject (Id SCNBillboardConstraint) where
  toNSObject = unsafeCastId

instance IsSCNConstraint (Id SCNBillboardConstraint) where
  toSCNConstraint = unsafeCastId

-- ---------- SCNDistanceConstraint ----------

-- | SCNDistanceConstraint
--
-- A SCNDistanceConstraint ensure a minimum/maximum distance with a target node.
-- 
-- Phantom type for @SCNDistanceConstraint@.
data SCNDistanceConstraint

instance IsObjCObject (Id SCNDistanceConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNDistanceConstraint"

class IsSCNConstraint a => IsSCNDistanceConstraint a where
  toSCNDistanceConstraint :: a -> Id SCNDistanceConstraint

instance IsSCNDistanceConstraint (Id SCNDistanceConstraint) where
  toSCNDistanceConstraint = unsafeCastId

instance IsNSObject (Id SCNDistanceConstraint) where
  toNSObject = unsafeCastId

instance IsSCNConstraint (Id SCNDistanceConstraint) where
  toSCNConstraint = unsafeCastId

-- ---------- SCNIKConstraint ----------

-- | SCNIKConstraint
--
-- A SCNIKConstraint applies an inverse kinematics constraint
-- 
-- Phantom type for @SCNIKConstraint@.
data SCNIKConstraint

instance IsObjCObject (Id SCNIKConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNIKConstraint"

class IsSCNConstraint a => IsSCNIKConstraint a where
  toSCNIKConstraint :: a -> Id SCNIKConstraint

instance IsSCNIKConstraint (Id SCNIKConstraint) where
  toSCNIKConstraint = unsafeCastId

instance IsNSObject (Id SCNIKConstraint) where
  toNSObject = unsafeCastId

instance IsSCNConstraint (Id SCNIKConstraint) where
  toSCNConstraint = unsafeCastId

-- ---------- SCNLookAtConstraint ----------

-- | SCNLookAtConstraint
--
-- A SCNLookAtConstraint applies on a node's orientation so that it always look at another node.
-- 
-- Phantom type for @SCNLookAtConstraint@.
data SCNLookAtConstraint

instance IsObjCObject (Id SCNLookAtConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNLookAtConstraint"

class IsSCNConstraint a => IsSCNLookAtConstraint a where
  toSCNLookAtConstraint :: a -> Id SCNLookAtConstraint

instance IsSCNLookAtConstraint (Id SCNLookAtConstraint) where
  toSCNLookAtConstraint = unsafeCastId

instance IsNSObject (Id SCNLookAtConstraint) where
  toNSObject = unsafeCastId

instance IsSCNConstraint (Id SCNLookAtConstraint) where
  toSCNConstraint = unsafeCastId

-- ---------- SCNReplicatorConstraint ----------

-- | SCNReplicatorConstraint
--
-- A SCNReplicatorConstraint replicates the position/orientation/scale of a target node
-- 
-- Phantom type for @SCNReplicatorConstraint@.
data SCNReplicatorConstraint

instance IsObjCObject (Id SCNReplicatorConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNReplicatorConstraint"

class IsSCNConstraint a => IsSCNReplicatorConstraint a where
  toSCNReplicatorConstraint :: a -> Id SCNReplicatorConstraint

instance IsSCNReplicatorConstraint (Id SCNReplicatorConstraint) where
  toSCNReplicatorConstraint = unsafeCastId

instance IsNSObject (Id SCNReplicatorConstraint) where
  toNSObject = unsafeCastId

instance IsSCNConstraint (Id SCNReplicatorConstraint) where
  toSCNConstraint = unsafeCastId

-- ---------- SCNSliderConstraint ----------

-- | SCNSliderConstraint
--
-- A SCNSliderConstraint constraint makes a node to collide and slide against a category of nodes
-- 
-- Phantom type for @SCNSliderConstraint@.
data SCNSliderConstraint

instance IsObjCObject (Id SCNSliderConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNSliderConstraint"

class IsSCNConstraint a => IsSCNSliderConstraint a where
  toSCNSliderConstraint :: a -> Id SCNSliderConstraint

instance IsSCNSliderConstraint (Id SCNSliderConstraint) where
  toSCNSliderConstraint = unsafeCastId

instance IsNSObject (Id SCNSliderConstraint) where
  toNSObject = unsafeCastId

instance IsSCNConstraint (Id SCNSliderConstraint) where
  toSCNConstraint = unsafeCastId

-- ---------- SCNTransformConstraint ----------

-- | SCNTransformConstraint
--
-- A SCNTransformConstraint applies on the transform of a node via a custom block.
-- 
-- Phantom type for @SCNTransformConstraint@.
data SCNTransformConstraint

instance IsObjCObject (Id SCNTransformConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNTransformConstraint"

class IsSCNConstraint a => IsSCNTransformConstraint a where
  toSCNTransformConstraint :: a -> Id SCNTransformConstraint

instance IsSCNTransformConstraint (Id SCNTransformConstraint) where
  toSCNTransformConstraint = unsafeCastId

instance IsNSObject (Id SCNTransformConstraint) where
  toNSObject = unsafeCastId

instance IsSCNConstraint (Id SCNTransformConstraint) where
  toSCNConstraint = unsafeCastId

-- ---------- SCNBox ----------

-- | SCNBox
--
-- SCNBox represents a box with rectangular sides and optional chamfers.
-- 
-- Phantom type for @SCNBox@.
data SCNBox

instance IsObjCObject (Id SCNBox) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNBox"

class IsSCNGeometry a => IsSCNBox a where
  toSCNBox :: a -> Id SCNBox

instance IsSCNBox (Id SCNBox) where
  toSCNBox = unsafeCastId

instance IsNSObject (Id SCNBox) where
  toNSObject = unsafeCastId

instance IsSCNGeometry (Id SCNBox) where
  toSCNGeometry = unsafeCastId

-- ---------- SCNCapsule ----------

-- | SCNCapsule
--
-- SCNCapsule represents a capsule with controllable height and cap radius.
-- 
-- Phantom type for @SCNCapsule@.
data SCNCapsule

instance IsObjCObject (Id SCNCapsule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNCapsule"

class IsSCNGeometry a => IsSCNCapsule a where
  toSCNCapsule :: a -> Id SCNCapsule

instance IsSCNCapsule (Id SCNCapsule) where
  toSCNCapsule = unsafeCastId

instance IsNSObject (Id SCNCapsule) where
  toNSObject = unsafeCastId

instance IsSCNGeometry (Id SCNCapsule) where
  toSCNGeometry = unsafeCastId

-- ---------- SCNCone ----------

-- | SCNCone
--
-- SCNCone represents a cone with controllable height, top radius and bottom radius.
-- 
-- Phantom type for @SCNCone@.
data SCNCone

instance IsObjCObject (Id SCNCone) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNCone"

class IsSCNGeometry a => IsSCNCone a where
  toSCNCone :: a -> Id SCNCone

instance IsSCNCone (Id SCNCone) where
  toSCNCone = unsafeCastId

instance IsNSObject (Id SCNCone) where
  toNSObject = unsafeCastId

instance IsSCNGeometry (Id SCNCone) where
  toSCNGeometry = unsafeCastId

-- ---------- SCNCylinder ----------

-- | SCNCylinder
--
-- SCNCylinder represents a cylinder with controllable height and radius.
-- 
-- Phantom type for @SCNCylinder@.
data SCNCylinder

instance IsObjCObject (Id SCNCylinder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNCylinder"

class IsSCNGeometry a => IsSCNCylinder a where
  toSCNCylinder :: a -> Id SCNCylinder

instance IsSCNCylinder (Id SCNCylinder) where
  toSCNCylinder = unsafeCastId

instance IsNSObject (Id SCNCylinder) where
  toNSObject = unsafeCastId

instance IsSCNGeometry (Id SCNCylinder) where
  toSCNGeometry = unsafeCastId

-- ---------- SCNFloor ----------

-- | SCNFloor
--
-- SCNFloor represents an infinite plane geometry.
-- 
-- Phantom type for @SCNFloor@.
data SCNFloor

instance IsObjCObject (Id SCNFloor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNFloor"

class IsSCNGeometry a => IsSCNFloor a where
  toSCNFloor :: a -> Id SCNFloor

instance IsSCNFloor (Id SCNFloor) where
  toSCNFloor = unsafeCastId

instance IsNSObject (Id SCNFloor) where
  toNSObject = unsafeCastId

instance IsSCNGeometry (Id SCNFloor) where
  toSCNGeometry = unsafeCastId

-- ---------- SCNPlane ----------

-- | SCNPlane
--
-- SCNPlane represents a rectangle with controllable width and height. The plane has one visible side.
-- 
-- Phantom type for @SCNPlane@.
data SCNPlane

instance IsObjCObject (Id SCNPlane) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPlane"

class IsSCNGeometry a => IsSCNPlane a where
  toSCNPlane :: a -> Id SCNPlane

instance IsSCNPlane (Id SCNPlane) where
  toSCNPlane = unsafeCastId

instance IsNSObject (Id SCNPlane) where
  toNSObject = unsafeCastId

instance IsSCNGeometry (Id SCNPlane) where
  toSCNGeometry = unsafeCastId

-- ---------- SCNPyramid ----------

-- | SCNPyramid
--
-- SCNPyramid represents a right pyramid with a rectangular base.
-- 
-- Phantom type for @SCNPyramid@.
data SCNPyramid

instance IsObjCObject (Id SCNPyramid) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPyramid"

class IsSCNGeometry a => IsSCNPyramid a where
  toSCNPyramid :: a -> Id SCNPyramid

instance IsSCNPyramid (Id SCNPyramid) where
  toSCNPyramid = unsafeCastId

instance IsNSObject (Id SCNPyramid) where
  toNSObject = unsafeCastId

instance IsSCNGeometry (Id SCNPyramid) where
  toSCNGeometry = unsafeCastId

-- ---------- SCNShape ----------

-- | SCNShape
--
-- SCNShape represents a 2D shape (cubic Bezier spline) than can be extruded.
-- 
-- Phantom type for @SCNShape@.
data SCNShape

instance IsObjCObject (Id SCNShape) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNShape"

class IsSCNGeometry a => IsSCNShape a where
  toSCNShape :: a -> Id SCNShape

instance IsSCNShape (Id SCNShape) where
  toSCNShape = unsafeCastId

instance IsNSObject (Id SCNShape) where
  toNSObject = unsafeCastId

instance IsSCNGeometry (Id SCNShape) where
  toSCNGeometry = unsafeCastId

-- ---------- SCNSphere ----------

-- | SCNSphere
--
-- SCNSphere represents a sphere with controllable radius
-- 
-- Phantom type for @SCNSphere@.
data SCNSphere

instance IsObjCObject (Id SCNSphere) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNSphere"

class IsSCNGeometry a => IsSCNSphere a where
  toSCNSphere :: a -> Id SCNSphere

instance IsSCNSphere (Id SCNSphere) where
  toSCNSphere = unsafeCastId

instance IsNSObject (Id SCNSphere) where
  toNSObject = unsafeCastId

instance IsSCNGeometry (Id SCNSphere) where
  toSCNGeometry = unsafeCastId

-- ---------- SCNText ----------

-- | SCNText
--
-- SCNText represents a block of text that has been extruded
-- 
-- Phantom type for @SCNText@.
data SCNText

instance IsObjCObject (Id SCNText) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNText"

class IsSCNGeometry a => IsSCNText a where
  toSCNText :: a -> Id SCNText

instance IsSCNText (Id SCNText) where
  toSCNText = unsafeCastId

instance IsNSObject (Id SCNText) where
  toNSObject = unsafeCastId

instance IsSCNGeometry (Id SCNText) where
  toSCNGeometry = unsafeCastId

-- ---------- SCNTorus ----------

-- | SCNTorus
--
-- SCNTorus represents a torus with controllable ring radius and pipe radius.
-- 
-- Phantom type for @SCNTorus@.
data SCNTorus

instance IsObjCObject (Id SCNTorus) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNTorus"

class IsSCNGeometry a => IsSCNTorus a where
  toSCNTorus :: a -> Id SCNTorus

instance IsSCNTorus (Id SCNTorus) where
  toSCNTorus = unsafeCastId

instance IsNSObject (Id SCNTorus) where
  toNSObject = unsafeCastId

instance IsSCNGeometry (Id SCNTorus) where
  toSCNGeometry = unsafeCastId

-- ---------- SCNTube ----------

-- | SCNTube
--
-- SCNTube represents a tube with controllable height, inner radius and outer radius.
-- 
-- Phantom type for @SCNTube@.
data SCNTube

instance IsObjCObject (Id SCNTube) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNTube"

class IsSCNGeometry a => IsSCNTube a where
  toSCNTube :: a -> Id SCNTube

instance IsSCNTube (Id SCNTube) where
  toSCNTube = unsafeCastId

instance IsNSObject (Id SCNTube) where
  toNSObject = unsafeCastId

instance IsSCNGeometry (Id SCNTube) where
  toSCNGeometry = unsafeCastId

-- ---------- SCNReferenceNode ----------

-- | SCNReferenceNode
--
-- Node that references an external file.
-- 
-- Phantom type for @SCNReferenceNode@.
data SCNReferenceNode

instance IsObjCObject (Id SCNReferenceNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNReferenceNode"

class IsSCNNode a => IsSCNReferenceNode a where
  toSCNReferenceNode :: a -> Id SCNReferenceNode

instance IsSCNReferenceNode (Id SCNReferenceNode) where
  toSCNReferenceNode = unsafeCastId

instance IsNSObject (Id SCNReferenceNode) where
  toNSObject = unsafeCastId

instance IsSCNNode (Id SCNReferenceNode) where
  toSCNNode = unsafeCastId

-- ---------- SCNPhysicsBallSocketJoint ----------

-- | SCNPhysicsBallSocketJoint
--
-- SCNPhysicsBallSocketJoint makes two bodies to move like they are connected by a ball-and-socket joint (i.e it allows rotations around all axes).
-- 
-- Phantom type for @SCNPhysicsBallSocketJoint@.
data SCNPhysicsBallSocketJoint

instance IsObjCObject (Id SCNPhysicsBallSocketJoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPhysicsBallSocketJoint"

class IsSCNPhysicsBehavior a => IsSCNPhysicsBallSocketJoint a where
  toSCNPhysicsBallSocketJoint :: a -> Id SCNPhysicsBallSocketJoint

instance IsSCNPhysicsBallSocketJoint (Id SCNPhysicsBallSocketJoint) where
  toSCNPhysicsBallSocketJoint = unsafeCastId

instance IsNSObject (Id SCNPhysicsBallSocketJoint) where
  toNSObject = unsafeCastId

instance IsSCNPhysicsBehavior (Id SCNPhysicsBallSocketJoint) where
  toSCNPhysicsBehavior = unsafeCastId

-- ---------- SCNPhysicsConeTwistJoint ----------

-- | SCNPhysicsConeTwistJoint
-- 
-- Phantom type for @SCNPhysicsConeTwistJoint@.
data SCNPhysicsConeTwistJoint

instance IsObjCObject (Id SCNPhysicsConeTwistJoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPhysicsConeTwistJoint"

class IsSCNPhysicsBehavior a => IsSCNPhysicsConeTwistJoint a where
  toSCNPhysicsConeTwistJoint :: a -> Id SCNPhysicsConeTwistJoint

instance IsSCNPhysicsConeTwistJoint (Id SCNPhysicsConeTwistJoint) where
  toSCNPhysicsConeTwistJoint = unsafeCastId

instance IsNSObject (Id SCNPhysicsConeTwistJoint) where
  toNSObject = unsafeCastId

instance IsSCNPhysicsBehavior (Id SCNPhysicsConeTwistJoint) where
  toSCNPhysicsBehavior = unsafeCastId

-- ---------- SCNPhysicsHingeJoint ----------

-- | SCNPhysicsHingeJoint
--
-- SCNPhysicsHingeJoint makes two bodies to move like they are connected by a hinge. It is for example suitable for doors, chains...
-- 
-- Phantom type for @SCNPhysicsHingeJoint@.
data SCNPhysicsHingeJoint

instance IsObjCObject (Id SCNPhysicsHingeJoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPhysicsHingeJoint"

class IsSCNPhysicsBehavior a => IsSCNPhysicsHingeJoint a where
  toSCNPhysicsHingeJoint :: a -> Id SCNPhysicsHingeJoint

instance IsSCNPhysicsHingeJoint (Id SCNPhysicsHingeJoint) where
  toSCNPhysicsHingeJoint = unsafeCastId

instance IsNSObject (Id SCNPhysicsHingeJoint) where
  toNSObject = unsafeCastId

instance IsSCNPhysicsBehavior (Id SCNPhysicsHingeJoint) where
  toSCNPhysicsBehavior = unsafeCastId

-- ---------- SCNPhysicsSliderJoint ----------

-- | SCNPhysicsSliderJoint
--
-- SCNPhysicsSliderJoint provides a linear sliding joint between two bodies.
-- 
-- Phantom type for @SCNPhysicsSliderJoint@.
data SCNPhysicsSliderJoint

instance IsObjCObject (Id SCNPhysicsSliderJoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPhysicsSliderJoint"

class IsSCNPhysicsBehavior a => IsSCNPhysicsSliderJoint a where
  toSCNPhysicsSliderJoint :: a -> Id SCNPhysicsSliderJoint

instance IsSCNPhysicsSliderJoint (Id SCNPhysicsSliderJoint) where
  toSCNPhysicsSliderJoint = unsafeCastId

instance IsNSObject (Id SCNPhysicsSliderJoint) where
  toNSObject = unsafeCastId

instance IsSCNPhysicsBehavior (Id SCNPhysicsSliderJoint) where
  toSCNPhysicsBehavior = unsafeCastId

-- ---------- SCNPhysicsVehicle ----------

-- | SCNPhysicsVehicle
--
-- SCNPhysicsVehicle provides a vehicle behavior.
-- 
-- Phantom type for @SCNPhysicsVehicle@.
data SCNPhysicsVehicle

instance IsObjCObject (Id SCNPhysicsVehicle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNPhysicsVehicle"

class IsSCNPhysicsBehavior a => IsSCNPhysicsVehicle a where
  toSCNPhysicsVehicle :: a -> Id SCNPhysicsVehicle

instance IsSCNPhysicsVehicle (Id SCNPhysicsVehicle) where
  toSCNPhysicsVehicle = unsafeCastId

instance IsNSObject (Id SCNPhysicsVehicle) where
  toNSObject = unsafeCastId

instance IsSCNPhysicsBehavior (Id SCNPhysicsVehicle) where
  toSCNPhysicsBehavior = unsafeCastId

-- ---------- SCNLayer ----------

-- | SCNLayer
--
-- A SCNLayer is a layer that can display a SCNScene.
-- 
-- Phantom type for @SCNLayer@.
data SCNLayer

instance IsObjCObject (Id SCNLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNLayer"

class IsCAOpenGLLayer a => IsSCNLayer a where
  toSCNLayer :: a -> Id SCNLayer

instance IsSCNLayer (Id SCNLayer) where
  toSCNLayer = unsafeCastId

instance IsCALayer (Id SCNLayer) where
  toCALayer = unsafeCastId

instance IsCAOpenGLLayer (Id SCNLayer) where
  toCAOpenGLLayer = unsafeCastId

instance IsNSObject (Id SCNLayer) where
  toNSObject = unsafeCastId

-- ---------- SCNView ----------

-- | SCNView
--
-- A SCNView is a subclass of NSView that can display a SCNScene
-- 
-- Phantom type for @SCNView@.
data SCNView

instance IsObjCObject (Id SCNView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SCNView"

class IsNSView a => IsSCNView a where
  toSCNView :: a -> Id SCNView

instance IsSCNView (Id SCNView) where
  toSCNView = unsafeCastId

instance IsNSObject (Id SCNView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SCNView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id SCNView) where
  toNSView = unsafeCastId
