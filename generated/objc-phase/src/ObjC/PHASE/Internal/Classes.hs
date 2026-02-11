{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.PHASE.Internal.Classes (
    module ObjC.PHASE.Internal.Classes,
    module ObjC.AVFAudio.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.ModelIO.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.ModelIO.Internal.Classes

-- ---------- PHASEAsset ----------

-- | ***********************************************************************************************
--
-- PHASEAsset
--
-- An object that represents a registered asset in the asset registry.
-- 
-- Phantom type for @PHASEAsset@.
data PHASEAsset

instance IsObjCObject (Id PHASEAsset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEAsset"

class IsNSObject a => IsPHASEAsset a where
  toPHASEAsset :: a -> Id PHASEAsset

instance IsPHASEAsset (Id PHASEAsset) where
  toPHASEAsset = unsafeCastId

instance IsNSObject (Id PHASEAsset) where
  toNSObject = unsafeCastId

-- ---------- PHASEAssetRegistry ----------

-- | *************************************************************************************************
--
-- PHASEAssetRegistry
--
-- Asset registry
-- 
-- Phantom type for @PHASEAssetRegistry@.
data PHASEAssetRegistry

instance IsObjCObject (Id PHASEAssetRegistry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEAssetRegistry"

class IsNSObject a => IsPHASEAssetRegistry a where
  toPHASEAssetRegistry :: a -> Id PHASEAssetRegistry

instance IsPHASEAssetRegistry (Id PHASEAssetRegistry) where
  toPHASEAssetRegistry = unsafeCastId

instance IsNSObject (Id PHASEAssetRegistry) where
  toNSObject = unsafeCastId

-- ---------- PHASECardioidDirectivityModelSubbandParameters ----------

-- | *************************************************************************************************
--
-- PHASECardioidDirectivityModelSubbandParameters
--
-- Cardioid directivity model subband parameters.
-- 
-- Phantom type for @PHASECardioidDirectivityModelSubbandParameters@.
data PHASECardioidDirectivityModelSubbandParameters

instance IsObjCObject (Id PHASECardioidDirectivityModelSubbandParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASECardioidDirectivityModelSubbandParameters"

class IsNSObject a => IsPHASECardioidDirectivityModelSubbandParameters a where
  toPHASECardioidDirectivityModelSubbandParameters :: a -> Id PHASECardioidDirectivityModelSubbandParameters

instance IsPHASECardioidDirectivityModelSubbandParameters (Id PHASECardioidDirectivityModelSubbandParameters) where
  toPHASECardioidDirectivityModelSubbandParameters = unsafeCastId

instance IsNSObject (Id PHASECardioidDirectivityModelSubbandParameters) where
  toNSObject = unsafeCastId

-- ---------- PHASEConeDirectivityModelSubbandParameters ----------

-- | *************************************************************************************************
--
-- PHASEConeDirectivityModelSubbandParameters
--
-- Cone directivity model subband parameters.
-- 
-- Phantom type for @PHASEConeDirectivityModelSubbandParameters@.
data PHASEConeDirectivityModelSubbandParameters

instance IsObjCObject (Id PHASEConeDirectivityModelSubbandParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEConeDirectivityModelSubbandParameters"

class IsNSObject a => IsPHASEConeDirectivityModelSubbandParameters a where
  toPHASEConeDirectivityModelSubbandParameters :: a -> Id PHASEConeDirectivityModelSubbandParameters

instance IsPHASEConeDirectivityModelSubbandParameters (Id PHASEConeDirectivityModelSubbandParameters) where
  toPHASEConeDirectivityModelSubbandParameters = unsafeCastId

instance IsNSObject (Id PHASEConeDirectivityModelSubbandParameters) where
  toNSObject = unsafeCastId

-- ---------- PHASEDefinition ----------

-- | *************************************************************************************************
--
-- PHASEDefinition
--
-- The base class for a definition.
--
-- Contains an identifer that uniquely represents this definition.
-- 
-- Phantom type for @PHASEDefinition@.
data PHASEDefinition

instance IsObjCObject (Id PHASEDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEDefinition"

class IsNSObject a => IsPHASEDefinition a where
  toPHASEDefinition :: a -> Id PHASEDefinition

instance IsPHASEDefinition (Id PHASEDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsNSObject (Id PHASEDefinition) where
  toNSObject = unsafeCastId

-- ---------- PHASEDirectivityModelParameters ----------

-- | *************************************************************************************************
--
-- PHASEDirectivityModelParameters
--
-- Directivity model parameters.
-- 
-- Phantom type for @PHASEDirectivityModelParameters@.
data PHASEDirectivityModelParameters

instance IsObjCObject (Id PHASEDirectivityModelParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEDirectivityModelParameters"

class IsNSObject a => IsPHASEDirectivityModelParameters a where
  toPHASEDirectivityModelParameters :: a -> Id PHASEDirectivityModelParameters

instance IsPHASEDirectivityModelParameters (Id PHASEDirectivityModelParameters) where
  toPHASEDirectivityModelParameters = unsafeCastId

instance IsNSObject (Id PHASEDirectivityModelParameters) where
  toNSObject = unsafeCastId

-- ---------- PHASEDistanceModelFadeOutParameters ----------

-- | *************************************************************************************************
--
-- PHASEDistanceModelFadeOutParameters
--
-- Distance model fade out parameters.
-- 
-- Phantom type for @PHASEDistanceModelFadeOutParameters@.
data PHASEDistanceModelFadeOutParameters

instance IsObjCObject (Id PHASEDistanceModelFadeOutParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEDistanceModelFadeOutParameters"

class IsNSObject a => IsPHASEDistanceModelFadeOutParameters a where
  toPHASEDistanceModelFadeOutParameters :: a -> Id PHASEDistanceModelFadeOutParameters

instance IsPHASEDistanceModelFadeOutParameters (Id PHASEDistanceModelFadeOutParameters) where
  toPHASEDistanceModelFadeOutParameters = unsafeCastId

instance IsNSObject (Id PHASEDistanceModelFadeOutParameters) where
  toNSObject = unsafeCastId

-- ---------- PHASEDistanceModelParameters ----------

-- | *************************************************************************************************
--
-- PHASEDistanceModelParameters
--
-- Distance model parameters.
-- 
-- Phantom type for @PHASEDistanceModelParameters@.
data PHASEDistanceModelParameters

instance IsObjCObject (Id PHASEDistanceModelParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEDistanceModelParameters"

class IsNSObject a => IsPHASEDistanceModelParameters a where
  toPHASEDistanceModelParameters :: a -> Id PHASEDistanceModelParameters

instance IsPHASEDistanceModelParameters (Id PHASEDistanceModelParameters) where
  toPHASEDistanceModelParameters = unsafeCastId

instance IsNSObject (Id PHASEDistanceModelParameters) where
  toNSObject = unsafeCastId

-- ---------- PHASEDucker ----------

-- | *************************************************************************************************
--
-- PHASEDucker
--
-- A PHASEDucker is used to describe ducking behavior across different groups.
-- 
-- Phantom type for @PHASEDucker@.
data PHASEDucker

instance IsObjCObject (Id PHASEDucker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEDucker"

class IsNSObject a => IsPHASEDucker a where
  toPHASEDucker :: a -> Id PHASEDucker

instance IsPHASEDucker (Id PHASEDucker) where
  toPHASEDucker = unsafeCastId

instance IsNSObject (Id PHASEDucker) where
  toNSObject = unsafeCastId

-- ---------- PHASEEngine ----------

-- | *************************************************************************************************
--
-- PHASEEngine
--
-- PHASE engine instance.
-- 
-- Phantom type for @PHASEEngine@.
data PHASEEngine

instance IsObjCObject (Id PHASEEngine) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEEngine"

class IsNSObject a => IsPHASEEngine a where
  toPHASEEngine :: a -> Id PHASEEngine

instance IsPHASEEngine (Id PHASEEngine) where
  toPHASEEngine = unsafeCastId

instance IsNSObject (Id PHASEEngine) where
  toNSObject = unsafeCastId

-- ---------- PHASEEnvelope ----------

-- | *************************************************************************************************
--
-- PHASEEnvelope
--
-- A segmented envelope.
-- 
-- Phantom type for @PHASEEnvelope@.
data PHASEEnvelope

instance IsObjCObject (Id PHASEEnvelope) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEEnvelope"

class IsNSObject a => IsPHASEEnvelope a where
  toPHASEEnvelope :: a -> Id PHASEEnvelope

instance IsPHASEEnvelope (Id PHASEEnvelope) where
  toPHASEEnvelope = unsafeCastId

instance IsNSObject (Id PHASEEnvelope) where
  toNSObject = unsafeCastId

-- ---------- PHASEEnvelopeSegment ----------

-- | *************************************************************************************************
--
-- PHASEEnvelopeSegment
--
-- An envelope segment defined by an end point and a curve type.
--
-- Envelope segments do 'not' contain a start point.        We do this so we can connect envelope segments together end to end and gaurantee continuity along the x and y axes.
-- 
-- Phantom type for @PHASEEnvelopeSegment@.
data PHASEEnvelopeSegment

instance IsObjCObject (Id PHASEEnvelopeSegment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEEnvelopeSegment"

class IsNSObject a => IsPHASEEnvelopeSegment a where
  toPHASEEnvelopeSegment :: a -> Id PHASEEnvelopeSegment

instance IsPHASEEnvelopeSegment (Id PHASEEnvelopeSegment) where
  toPHASEEnvelopeSegment = unsafeCastId

instance IsNSObject (Id PHASEEnvelopeSegment) where
  toNSObject = unsafeCastId

-- ---------- PHASEGroup ----------

-- | *************************************************************************************************
--
-- PHASEGroup
--
-- A PHASEGroup allows clients to group generator nodes for shared processing.        Clients can set the gain and playback rate, as well as mute and solo the generator nodes in a group.
-- 
-- Phantom type for @PHASEGroup@.
data PHASEGroup

instance IsObjCObject (Id PHASEGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEGroup"

class IsNSObject a => IsPHASEGroup a where
  toPHASEGroup :: a -> Id PHASEGroup

instance IsPHASEGroup (Id PHASEGroup) where
  toPHASEGroup = unsafeCastId

instance IsNSObject (Id PHASEGroup) where
  toNSObject = unsafeCastId

-- ---------- PHASEGroupPreset ----------

-- | *************************************************************************************************
--
-- PHASEGroupPreset
--
-- A PHASEGroupPreset holds a collection of PHASEGroupPresetSetting objects and other parameters to be applied all at once during playback.
--
-- Initialize beforehand, and use activate or deactivate to switch to the new preset during playback.        Activating a preset will automatically deactivate the current one.
-- 
-- Phantom type for @PHASEGroupPreset@.
data PHASEGroupPreset

instance IsObjCObject (Id PHASEGroupPreset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEGroupPreset"

class IsNSObject a => IsPHASEGroupPreset a where
  toPHASEGroupPreset :: a -> Id PHASEGroupPreset

instance IsPHASEGroupPreset (Id PHASEGroupPreset) where
  toPHASEGroupPreset = unsafeCastId

instance IsNSObject (Id PHASEGroupPreset) where
  toNSObject = unsafeCastId

-- ---------- PHASEGroupPresetSetting ----------

-- | *************************************************************************************************
--
-- PHASEGroupPresetSetting
--
-- A PHASEGroupPresetSetting is an object that holds settings that can be applied to a PHASEGroup object.
--
-- These can be either be manually created and added to a PHASEGroupPreset object, or created inline using PHASEGroupPreset addGroup.
-- 
-- Phantom type for @PHASEGroupPresetSetting@.
data PHASEGroupPresetSetting

instance IsObjCObject (Id PHASEGroupPresetSetting) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEGroupPresetSetting"

class IsNSObject a => IsPHASEGroupPresetSetting a where
  toPHASEGroupPresetSetting :: a -> Id PHASEGroupPresetSetting

instance IsPHASEGroupPresetSetting (Id PHASEGroupPresetSetting) where
  toPHASEGroupPresetSetting = unsafeCastId

instance IsNSObject (Id PHASEGroupPresetSetting) where
  toNSObject = unsafeCastId

-- ---------- PHASEMaterial ----------

-- | *************************************************************************************************
--
-- PHASEMaterial
--
-- A PHASEMaterial describes the acoustic properties of a material.
-- 
-- Phantom type for @PHASEMaterial@.
data PHASEMaterial

instance IsObjCObject (Id PHASEMaterial) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEMaterial"

class IsNSObject a => IsPHASEMaterial a where
  toPHASEMaterial :: a -> Id PHASEMaterial

instance IsPHASEMaterial (Id PHASEMaterial) where
  toPHASEMaterial = unsafeCastId

instance IsNSObject (Id PHASEMaterial) where
  toNSObject = unsafeCastId

-- ---------- PHASEMedium ----------

-- | *************************************************************************************************
--
-- PHASEMedium
--
-- A PHASEMedium describes the acoustic properties of a medium.
-- 
-- Phantom type for @PHASEMedium@.
data PHASEMedium

instance IsObjCObject (Id PHASEMedium) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEMedium"

class IsNSObject a => IsPHASEMedium a where
  toPHASEMedium :: a -> Id PHASEMedium

instance IsPHASEMedium (Id PHASEMedium) where
  toPHASEMedium = unsafeCastId

instance IsNSObject (Id PHASEMedium) where
  toNSObject = unsafeCastId

-- ---------- PHASEMetaParameter ----------

-- | *************************************************************************************************
--
-- PHASEMetaParameter
--
-- A generic object that represents an active metaparameter in the system
-- 
-- Phantom type for @PHASEMetaParameter@.
data PHASEMetaParameter

instance IsObjCObject (Id PHASEMetaParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEMetaParameter"

class IsNSObject a => IsPHASEMetaParameter a where
  toPHASEMetaParameter :: a -> Id PHASEMetaParameter

instance IsPHASEMetaParameter (Id PHASEMetaParameter) where
  toPHASEMetaParameter = unsafeCastId

instance IsNSObject (Id PHASEMetaParameter) where
  toNSObject = unsafeCastId

-- ---------- PHASEMixer ----------

-- | *************************************************************************************************
--
-- PHASEMixer
--
-- A generic object the represents an active mixer in the system
-- 
-- Phantom type for @PHASEMixer@.
data PHASEMixer

instance IsObjCObject (Id PHASEMixer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEMixer"

class IsNSObject a => IsPHASEMixer a where
  toPHASEMixer :: a -> Id PHASEMixer

instance IsPHASEMixer (Id PHASEMixer) where
  toPHASEMixer = unsafeCastId

instance IsNSObject (Id PHASEMixer) where
  toNSObject = unsafeCastId

-- ---------- PHASEMixerParameters ----------

-- | *************************************************************************************************
--
-- PHASEMixerParameters
--
-- An object that holds runtime parameters for mixers when creating PHASESoundEvents.
-- 
-- Phantom type for @PHASEMixerParameters@.
data PHASEMixerParameters

instance IsObjCObject (Id PHASEMixerParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEMixerParameters"

class IsNSObject a => IsPHASEMixerParameters a where
  toPHASEMixerParameters :: a -> Id PHASEMixerParameters

instance IsPHASEMixerParameters (Id PHASEMixerParameters) where
  toPHASEMixerParameters = unsafeCastId

instance IsNSObject (Id PHASEMixerParameters) where
  toNSObject = unsafeCastId

-- ---------- PHASENumericPair ----------

-- | *************************************************************************************************
--
-- PHASENumericPair
--
-- A numeric pair.
-- 
-- Phantom type for @PHASENumericPair@.
data PHASENumericPair

instance IsObjCObject (Id PHASENumericPair) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASENumericPair"

class IsNSObject a => IsPHASENumericPair a where
  toPHASENumericPair :: a -> Id PHASENumericPair

instance IsPHASENumericPair (Id PHASENumericPair) where
  toPHASENumericPair = unsafeCastId

instance IsNSObject (Id PHASENumericPair) where
  toNSObject = unsafeCastId

-- ---------- PHASEObject ----------

-- | *************************************************************************************************
--
-- PHASEObject
--
-- A PHASEObject represents a 3D object in the engine, which can be organized into a hierarchy with relative transforms.
-- 
-- Phantom type for @PHASEObject@.
data PHASEObject

instance IsObjCObject (Id PHASEObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEObject"

class IsNSObject a => IsPHASEObject a where
  toPHASEObject :: a -> Id PHASEObject

instance IsPHASEObject (Id PHASEObject) where
  toPHASEObject = unsafeCastId

instance IsNSObject (Id PHASEObject) where
  toNSObject = unsafeCastId

-- ---------- PHASEShape ----------

-- | *************************************************************************************************
--
-- PHASEShape
--
-- The physical representation of an object within the simulated acoustic scene.
-- 
-- Phantom type for @PHASEShape@.
data PHASEShape

instance IsObjCObject (Id PHASEShape) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEShape"

class IsNSObject a => IsPHASEShape a where
  toPHASEShape :: a -> Id PHASEShape

instance IsPHASEShape (Id PHASEShape) where
  toPHASEShape = unsafeCastId

instance IsNSObject (Id PHASEShape) where
  toNSObject = unsafeCastId

-- ---------- PHASEShapeElement ----------

-- | *************************************************************************************************
--
-- PHASEShapeElement
--
-- A single element within a shape. The attached material defines its acoustical properties.
-- 
-- Phantom type for @PHASEShapeElement@.
data PHASEShapeElement

instance IsObjCObject (Id PHASEShapeElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEShapeElement"

class IsNSObject a => IsPHASEShapeElement a where
  toPHASEShapeElement :: a -> Id PHASEShapeElement

instance IsPHASEShapeElement (Id PHASEShapeElement) where
  toPHASEShapeElement = unsafeCastId

instance IsNSObject (Id PHASEShapeElement) where
  toNSObject = unsafeCastId

-- ---------- PHASESoundEvent ----------

-- | *************************************************************************************************
--
-- PHASESoundEvent
--
-- A PHASESoundEvent is an object that represents a playable sound event in the PHASE system.
-- 
-- Phantom type for @PHASESoundEvent@.
data PHASESoundEvent

instance IsObjCObject (Id PHASESoundEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASESoundEvent"

class IsNSObject a => IsPHASESoundEvent a where
  toPHASESoundEvent :: a -> Id PHASESoundEvent

instance IsPHASESoundEvent (Id PHASESoundEvent) where
  toPHASESoundEvent = unsafeCastId

instance IsNSObject (Id PHASESoundEvent) where
  toNSObject = unsafeCastId

-- ---------- PHASESpatialPipeline ----------

-- | *************************************************************************************************
--
-- PHASESpatialPipeline
--
-- Spatial Pipeline.
-- 
-- Phantom type for @PHASESpatialPipeline@.
data PHASESpatialPipeline

instance IsObjCObject (Id PHASESpatialPipeline) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASESpatialPipeline"

class IsNSObject a => IsPHASESpatialPipeline a where
  toPHASESpatialPipeline :: a -> Id PHASESpatialPipeline

instance IsPHASESpatialPipeline (Id PHASESpatialPipeline) where
  toPHASESpatialPipeline = unsafeCastId

instance IsNSObject (Id PHASESpatialPipeline) where
  toNSObject = unsafeCastId

-- ---------- PHASESpatialPipelineEntry ----------

-- | *************************************************************************************************
--
-- PHASESpatialPipelineEntry
--
-- Spatial Pipeline Entry.
-- 
-- Phantom type for @PHASESpatialPipelineEntry@.
data PHASESpatialPipelineEntry

instance IsObjCObject (Id PHASESpatialPipelineEntry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASESpatialPipelineEntry"

class IsNSObject a => IsPHASESpatialPipelineEntry a where
  toPHASESpatialPipelineEntry :: a -> Id PHASESpatialPipelineEntry

instance IsPHASESpatialPipelineEntry (Id PHASESpatialPipelineEntry) where
  toPHASESpatialPipelineEntry = unsafeCastId

instance IsNSObject (Id PHASESpatialPipelineEntry) where
  toNSObject = unsafeCastId

-- ---------- PHASEStreamNode ----------

-- | *************************************************************************************************
--
-- PHASEStreamNode
--
-- The base class for stream nodes, exposing common elements.
-- 
-- Phantom type for @PHASEStreamNode@.
data PHASEStreamNode

instance IsObjCObject (Id PHASEStreamNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEStreamNode"

class IsNSObject a => IsPHASEStreamNode a where
  toPHASEStreamNode :: a -> Id PHASEStreamNode

instance IsPHASEStreamNode (Id PHASEStreamNode) where
  toPHASEStreamNode = unsafeCastId

instance IsNSObject (Id PHASEStreamNode) where
  toNSObject = unsafeCastId

-- ---------- PHASEGlobalMetaParameterAsset ----------

-- | *************************************************************************************************
--
-- PHASEGlobalMetaParameterAsset
--
-- An object that represents a registered global metaparameter asset in the asset registry.
-- 
-- Phantom type for @PHASEGlobalMetaParameterAsset@.
data PHASEGlobalMetaParameterAsset

instance IsObjCObject (Id PHASEGlobalMetaParameterAsset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEGlobalMetaParameterAsset"

class IsPHASEAsset a => IsPHASEGlobalMetaParameterAsset a where
  toPHASEGlobalMetaParameterAsset :: a -> Id PHASEGlobalMetaParameterAsset

instance IsPHASEGlobalMetaParameterAsset (Id PHASEGlobalMetaParameterAsset) where
  toPHASEGlobalMetaParameterAsset = unsafeCastId

instance IsNSObject (Id PHASEGlobalMetaParameterAsset) where
  toNSObject = unsafeCastId

instance IsPHASEAsset (Id PHASEGlobalMetaParameterAsset) where
  toPHASEAsset = unsafeCastId

-- ---------- PHASESoundAsset ----------

-- | *************************************************************************************************
--
-- PHASESoundAsset
--
-- An object that represents a registered sound asset in the asset registry.
-- 
-- Phantom type for @PHASESoundAsset@.
data PHASESoundAsset

instance IsObjCObject (Id PHASESoundAsset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASESoundAsset"

class IsPHASEAsset a => IsPHASESoundAsset a where
  toPHASESoundAsset :: a -> Id PHASESoundAsset

instance IsPHASESoundAsset (Id PHASESoundAsset) where
  toPHASESoundAsset = unsafeCastId

instance IsNSObject (Id PHASESoundAsset) where
  toNSObject = unsafeCastId

instance IsPHASEAsset (Id PHASESoundAsset) where
  toPHASEAsset = unsafeCastId

-- ---------- PHASESoundEventNodeAsset ----------

-- | *************************************************************************************************
--
-- PHASESoundEventNodeAsset
--
-- An object that represents a registered sound event asset in the asset registry.
-- 
-- Phantom type for @PHASESoundEventNodeAsset@.
data PHASESoundEventNodeAsset

instance IsObjCObject (Id PHASESoundEventNodeAsset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASESoundEventNodeAsset"

class IsPHASEAsset a => IsPHASESoundEventNodeAsset a where
  toPHASESoundEventNodeAsset :: a -> Id PHASESoundEventNodeAsset

instance IsPHASESoundEventNodeAsset (Id PHASESoundEventNodeAsset) where
  toPHASESoundEventNodeAsset = unsafeCastId

instance IsNSObject (Id PHASESoundEventNodeAsset) where
  toNSObject = unsafeCastId

instance IsPHASEAsset (Id PHASESoundEventNodeAsset) where
  toPHASEAsset = unsafeCastId

-- ---------- PHASEMetaParameterDefinition ----------

-- | *************************************************************************************************
--
-- PHASEMetaParameterDefinition
--
-- A base object for metaparameter definitions
-- 
-- Phantom type for @PHASEMetaParameterDefinition@.
data PHASEMetaParameterDefinition

instance IsObjCObject (Id PHASEMetaParameterDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEMetaParameterDefinition"

class IsPHASEDefinition a => IsPHASEMetaParameterDefinition a where
  toPHASEMetaParameterDefinition :: a -> Id PHASEMetaParameterDefinition

instance IsPHASEMetaParameterDefinition (Id PHASEMetaParameterDefinition) where
  toPHASEMetaParameterDefinition = unsafeCastId

instance IsNSObject (Id PHASEMetaParameterDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASEMetaParameterDefinition) where
  toPHASEDefinition = unsafeCastId

-- ---------- PHASEMixerDefinition ----------

-- | *************************************************************************************************
--
-- PHASEMixerDefinition
--
-- The base class for a mixer definition.
--
-- Mixer definitions control how audio will be rendered to the output in PHASE.
-- 
-- Phantom type for @PHASEMixerDefinition@.
data PHASEMixerDefinition

instance IsObjCObject (Id PHASEMixerDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEMixerDefinition"

class IsPHASEDefinition a => IsPHASEMixerDefinition a where
  toPHASEMixerDefinition :: a -> Id PHASEMixerDefinition

instance IsPHASEMixerDefinition (Id PHASEMixerDefinition) where
  toPHASEMixerDefinition = unsafeCastId

instance IsNSObject (Id PHASEMixerDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASEMixerDefinition) where
  toPHASEDefinition = unsafeCastId

-- ---------- PHASESoundEventNodeDefinition ----------

-- | *************************************************************************************************
--
-- PHASESoundEventNodeDefinition
--
-- The base class for a sound event node definition.
--
-- Sound event nodes are a hierarchical collection of objects that either generate or control playback of audio content in PHASE.        Generator nodes produce audio. They are always leaf nodes in a node hierarchy. These include samplers and stream nodes.        Control nodes set the logic for how generator nodes are selected, mixed and parameterized before downstream mixer processing.        Control nodes are always parent nodes, and can be organized into hierarchies for complex sound design scenarios.
-- 
-- Phantom type for @PHASESoundEventNodeDefinition@.
data PHASESoundEventNodeDefinition

instance IsObjCObject (Id PHASESoundEventNodeDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASESoundEventNodeDefinition"

class IsPHASEDefinition a => IsPHASESoundEventNodeDefinition a where
  toPHASESoundEventNodeDefinition :: a -> Id PHASESoundEventNodeDefinition

instance IsPHASESoundEventNodeDefinition (Id PHASESoundEventNodeDefinition) where
  toPHASESoundEventNodeDefinition = unsafeCastId

instance IsNSObject (Id PHASESoundEventNodeDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASESoundEventNodeDefinition) where
  toPHASEDefinition = unsafeCastId

-- ---------- PHASECardioidDirectivityModelParameters ----------

-- | *************************************************************************************************
--
-- PHASECardioidDirectivityModelParameters
--
-- Cardioid directivity model parameters.
-- 
-- Phantom type for @PHASECardioidDirectivityModelParameters@.
data PHASECardioidDirectivityModelParameters

instance IsObjCObject (Id PHASECardioidDirectivityModelParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASECardioidDirectivityModelParameters"

class IsPHASEDirectivityModelParameters a => IsPHASECardioidDirectivityModelParameters a where
  toPHASECardioidDirectivityModelParameters :: a -> Id PHASECardioidDirectivityModelParameters

instance IsPHASECardioidDirectivityModelParameters (Id PHASECardioidDirectivityModelParameters) where
  toPHASECardioidDirectivityModelParameters = unsafeCastId

instance IsNSObject (Id PHASECardioidDirectivityModelParameters) where
  toNSObject = unsafeCastId

instance IsPHASEDirectivityModelParameters (Id PHASECardioidDirectivityModelParameters) where
  toPHASEDirectivityModelParameters = unsafeCastId

-- ---------- PHASEConeDirectivityModelParameters ----------

-- | *************************************************************************************************
--
-- PHASEConeDirectivityModelParameters
--
-- Cone directivity model parameters.
-- 
-- Phantom type for @PHASEConeDirectivityModelParameters@.
data PHASEConeDirectivityModelParameters

instance IsObjCObject (Id PHASEConeDirectivityModelParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEConeDirectivityModelParameters"

class IsPHASEDirectivityModelParameters a => IsPHASEConeDirectivityModelParameters a where
  toPHASEConeDirectivityModelParameters :: a -> Id PHASEConeDirectivityModelParameters

instance IsPHASEConeDirectivityModelParameters (Id PHASEConeDirectivityModelParameters) where
  toPHASEConeDirectivityModelParameters = unsafeCastId

instance IsNSObject (Id PHASEConeDirectivityModelParameters) where
  toNSObject = unsafeCastId

instance IsPHASEDirectivityModelParameters (Id PHASEConeDirectivityModelParameters) where
  toPHASEDirectivityModelParameters = unsafeCastId

-- ---------- PHASEEnvelopeDistanceModelParameters ----------

-- | *************************************************************************************************
--
-- PHASEEnvelopeDistanceModelParameters
--
-- Envelope distance model parameters.
--
-- Envelope-driven attenuation over distance.
-- 
-- Phantom type for @PHASEEnvelopeDistanceModelParameters@.
data PHASEEnvelopeDistanceModelParameters

instance IsObjCObject (Id PHASEEnvelopeDistanceModelParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEEnvelopeDistanceModelParameters"

class IsPHASEDistanceModelParameters a => IsPHASEEnvelopeDistanceModelParameters a where
  toPHASEEnvelopeDistanceModelParameters :: a -> Id PHASEEnvelopeDistanceModelParameters

instance IsPHASEEnvelopeDistanceModelParameters (Id PHASEEnvelopeDistanceModelParameters) where
  toPHASEEnvelopeDistanceModelParameters = unsafeCastId

instance IsNSObject (Id PHASEEnvelopeDistanceModelParameters) where
  toNSObject = unsafeCastId

instance IsPHASEDistanceModelParameters (Id PHASEEnvelopeDistanceModelParameters) where
  toPHASEDistanceModelParameters = unsafeCastId

-- ---------- PHASEGeometricSpreadingDistanceModelParameters ----------

-- | *************************************************************************************************
--
-- PHASEGeometricSpreadingDistanceModelParameters
--
-- Geometric spreading distance model parameters.
--
-- Standard geometric spreading loss as a function of geometry and distance.
-- 
-- Phantom type for @PHASEGeometricSpreadingDistanceModelParameters@.
data PHASEGeometricSpreadingDistanceModelParameters

instance IsObjCObject (Id PHASEGeometricSpreadingDistanceModelParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEGeometricSpreadingDistanceModelParameters"

class IsPHASEDistanceModelParameters a => IsPHASEGeometricSpreadingDistanceModelParameters a where
  toPHASEGeometricSpreadingDistanceModelParameters :: a -> Id PHASEGeometricSpreadingDistanceModelParameters

instance IsPHASEGeometricSpreadingDistanceModelParameters (Id PHASEGeometricSpreadingDistanceModelParameters) where
  toPHASEGeometricSpreadingDistanceModelParameters = unsafeCastId

instance IsNSObject (Id PHASEGeometricSpreadingDistanceModelParameters) where
  toNSObject = unsafeCastId

instance IsPHASEDistanceModelParameters (Id PHASEGeometricSpreadingDistanceModelParameters) where
  toPHASEDistanceModelParameters = unsafeCastId

-- ---------- PHASENumberMetaParameter ----------

-- | *************************************************************************************************
--
-- PHASENumberMetaParameter
--
-- An object that represents an active numeric metaparameter in the system
-- 
-- Phantom type for @PHASENumberMetaParameter@.
data PHASENumberMetaParameter

instance IsObjCObject (Id PHASENumberMetaParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASENumberMetaParameter"

class IsPHASEMetaParameter a => IsPHASENumberMetaParameter a where
  toPHASENumberMetaParameter :: a -> Id PHASENumberMetaParameter

instance IsPHASENumberMetaParameter (Id PHASENumberMetaParameter) where
  toPHASENumberMetaParameter = unsafeCastId

instance IsNSObject (Id PHASENumberMetaParameter) where
  toNSObject = unsafeCastId

instance IsPHASEMetaParameter (Id PHASENumberMetaParameter) where
  toPHASEMetaParameter = unsafeCastId

-- ---------- PHASEStringMetaParameter ----------

-- | *************************************************************************************************
--
-- PHASEStringMetaParameter
--
-- An object that represents an active string metaparameter in the system
-- 
-- Phantom type for @PHASEStringMetaParameter@.
data PHASEStringMetaParameter

instance IsObjCObject (Id PHASEStringMetaParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEStringMetaParameter"

class IsPHASEMetaParameter a => IsPHASEStringMetaParameter a where
  toPHASEStringMetaParameter :: a -> Id PHASEStringMetaParameter

instance IsPHASEStringMetaParameter (Id PHASEStringMetaParameter) where
  toPHASEStringMetaParameter = unsafeCastId

instance IsNSObject (Id PHASEStringMetaParameter) where
  toNSObject = unsafeCastId

instance IsPHASEMetaParameter (Id PHASEStringMetaParameter) where
  toPHASEMetaParameter = unsafeCastId

-- ---------- PHASEListener ----------

-- | *************************************************************************************************
--
-- PHASEListener
--
-- A PHASEListener represents the listener's point of view within the simulated acoustic scene.
-- 
-- Phantom type for @PHASEListener@.
data PHASEListener

instance IsObjCObject (Id PHASEListener) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEListener"

class IsPHASEObject a => IsPHASEListener a where
  toPHASEListener :: a -> Id PHASEListener

instance IsPHASEListener (Id PHASEListener) where
  toPHASEListener = unsafeCastId

instance IsNSObject (Id PHASEListener) where
  toNSObject = unsafeCastId

instance IsPHASEObject (Id PHASEListener) where
  toPHASEObject = unsafeCastId

-- ---------- PHASEOccluder ----------

-- | *************************************************************************************************
--
-- PHASEOccluder
--
-- A PHASEOccluder represents a shape (with associated materials) that can affect sound transmission within the simulated acoustic scene.
-- 
-- Phantom type for @PHASEOccluder@.
data PHASEOccluder

instance IsObjCObject (Id PHASEOccluder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEOccluder"

class IsPHASEObject a => IsPHASEOccluder a where
  toPHASEOccluder :: a -> Id PHASEOccluder

instance IsPHASEOccluder (Id PHASEOccluder) where
  toPHASEOccluder = unsafeCastId

instance IsNSObject (Id PHASEOccluder) where
  toNSObject = unsafeCastId

instance IsPHASEObject (Id PHASEOccluder) where
  toPHASEObject = unsafeCastId

-- ---------- PHASESource ----------

-- | *************************************************************************************************
--
-- PHASESource
--
-- A PHASESource represents where sound originates within the simulated acoustic scene.
--
-- PHASE supports both point sources and volumetric sources.        A point source simulates the sound from a point in space.        A volumetric source simulates the sound from a shape.
-- 
-- Phantom type for @PHASESource@.
data PHASESource

instance IsObjCObject (Id PHASESource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASESource"

class IsPHASEObject a => IsPHASESource a where
  toPHASESource :: a -> Id PHASESource

instance IsPHASESource (Id PHASESource) where
  toPHASESource = unsafeCastId

instance IsNSObject (Id PHASESource) where
  toNSObject = unsafeCastId

instance IsPHASEObject (Id PHASESource) where
  toPHASEObject = unsafeCastId

-- ---------- PHASEPullStreamNode ----------

-- | *************************************************************************************************
--
-- PHASEPullStreamNode
--
-- An object for addessing an instance of a stream in an executing sound event
-- 
-- Phantom type for @PHASEPullStreamNode@.
data PHASEPullStreamNode

instance IsObjCObject (Id PHASEPullStreamNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEPullStreamNode"

class IsPHASEStreamNode a => IsPHASEPullStreamNode a where
  toPHASEPullStreamNode :: a -> Id PHASEPullStreamNode

instance IsPHASEPullStreamNode (Id PHASEPullStreamNode) where
  toPHASEPullStreamNode = unsafeCastId

instance IsNSObject (Id PHASEPullStreamNode) where
  toNSObject = unsafeCastId

instance IsPHASEStreamNode (Id PHASEPullStreamNode) where
  toPHASEStreamNode = unsafeCastId

-- ---------- PHASEPushStreamNode ----------

-- | *************************************************************************************************
--
-- PHASEPushStreamNode
--
-- An object for addessing an instance of a stream in an executing sound event
-- 
-- Phantom type for @PHASEPushStreamNode@.
data PHASEPushStreamNode

instance IsObjCObject (Id PHASEPushStreamNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEPushStreamNode"

class IsPHASEStreamNode a => IsPHASEPushStreamNode a where
  toPHASEPushStreamNode :: a -> Id PHASEPushStreamNode

instance IsPHASEPushStreamNode (Id PHASEPushStreamNode) where
  toPHASEPushStreamNode = unsafeCastId

instance IsNSObject (Id PHASEPushStreamNode) where
  toNSObject = unsafeCastId

instance IsPHASEStreamNode (Id PHASEPushStreamNode) where
  toPHASEStreamNode = unsafeCastId

-- ---------- PHASENumberMetaParameterDefinition ----------

-- | *************************************************************************************************
--
-- PHASENumberMetaParameterDefinition
--
-- A metaparameter that has a numeric value
-- 
-- Phantom type for @PHASENumberMetaParameterDefinition@.
data PHASENumberMetaParameterDefinition

instance IsObjCObject (Id PHASENumberMetaParameterDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASENumberMetaParameterDefinition"

class IsPHASEMetaParameterDefinition a => IsPHASENumberMetaParameterDefinition a where
  toPHASENumberMetaParameterDefinition :: a -> Id PHASENumberMetaParameterDefinition

instance IsPHASENumberMetaParameterDefinition (Id PHASENumberMetaParameterDefinition) where
  toPHASENumberMetaParameterDefinition = unsafeCastId

instance IsNSObject (Id PHASENumberMetaParameterDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASENumberMetaParameterDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASEMetaParameterDefinition (Id PHASENumberMetaParameterDefinition) where
  toPHASEMetaParameterDefinition = unsafeCastId

-- ---------- PHASEStringMetaParameterDefinition ----------

-- | *************************************************************************************************
--
-- PHASEStringMetaParameterDefinition
--
-- A Metaparameter that has a string value
-- 
-- Phantom type for @PHASEStringMetaParameterDefinition@.
data PHASEStringMetaParameterDefinition

instance IsObjCObject (Id PHASEStringMetaParameterDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEStringMetaParameterDefinition"

class IsPHASEMetaParameterDefinition a => IsPHASEStringMetaParameterDefinition a where
  toPHASEStringMetaParameterDefinition :: a -> Id PHASEStringMetaParameterDefinition

instance IsPHASEStringMetaParameterDefinition (Id PHASEStringMetaParameterDefinition) where
  toPHASEStringMetaParameterDefinition = unsafeCastId

instance IsNSObject (Id PHASEStringMetaParameterDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASEStringMetaParameterDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASEMetaParameterDefinition (Id PHASEStringMetaParameterDefinition) where
  toPHASEMetaParameterDefinition = unsafeCastId

-- ---------- PHASEAmbientMixerDefinition ----------

-- | *************************************************************************************************
--
-- PHASEAmbientMixerDefinition
--
-- Ambient mixer definition.
--
-- Ambient mixers render audio with spatialization but without environmental effects.        Use ambient mixers for content that isn't being simulated in the environment,        but should still sound like it's coming from somewhere out in space.
--
-- Note: Ambient mixers do not support distance modeling or directivity modeling.        Clients can however set the orientation at initialization time.
-- 
-- Phantom type for @PHASEAmbientMixerDefinition@.
data PHASEAmbientMixerDefinition

instance IsObjCObject (Id PHASEAmbientMixerDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEAmbientMixerDefinition"

class IsPHASEMixerDefinition a => IsPHASEAmbientMixerDefinition a where
  toPHASEAmbientMixerDefinition :: a -> Id PHASEAmbientMixerDefinition

instance IsPHASEAmbientMixerDefinition (Id PHASEAmbientMixerDefinition) where
  toPHASEAmbientMixerDefinition = unsafeCastId

instance IsNSObject (Id PHASEAmbientMixerDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASEAmbientMixerDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASEMixerDefinition (Id PHASEAmbientMixerDefinition) where
  toPHASEMixerDefinition = unsafeCastId

-- ---------- PHASEChannelMixerDefinition ----------

-- | *************************************************************************************************
--
-- PHASEChannelMixerDefinition
--
-- Channel mixer definition.
--
-- Channel mixers render audio without spatialization or environmental effects.        Use channel mixers for regular stem-based content that needs be rendered directly to the output device, such as stereo music        or center channel narrative dialogue.
-- 
-- Phantom type for @PHASEChannelMixerDefinition@.
data PHASEChannelMixerDefinition

instance IsObjCObject (Id PHASEChannelMixerDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEChannelMixerDefinition"

class IsPHASEMixerDefinition a => IsPHASEChannelMixerDefinition a where
  toPHASEChannelMixerDefinition :: a -> Id PHASEChannelMixerDefinition

instance IsPHASEChannelMixerDefinition (Id PHASEChannelMixerDefinition) where
  toPHASEChannelMixerDefinition = unsafeCastId

instance IsNSObject (Id PHASEChannelMixerDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASEChannelMixerDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASEMixerDefinition (Id PHASEChannelMixerDefinition) where
  toPHASEMixerDefinition = unsafeCastId

-- ---------- PHASESpatialMixerDefinition ----------

-- | *************************************************************************************************
--
-- PHASESpatialMixerDefinition
--
-- Spatial mixer definition.
--
-- Spatial mixers render audio with spatialization and environmental effects.
-- 
-- Phantom type for @PHASESpatialMixerDefinition@.
data PHASESpatialMixerDefinition

instance IsObjCObject (Id PHASESpatialMixerDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASESpatialMixerDefinition"

class IsPHASEMixerDefinition a => IsPHASESpatialMixerDefinition a where
  toPHASESpatialMixerDefinition :: a -> Id PHASESpatialMixerDefinition

instance IsPHASESpatialMixerDefinition (Id PHASESpatialMixerDefinition) where
  toPHASESpatialMixerDefinition = unsafeCastId

instance IsNSObject (Id PHASESpatialMixerDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASESpatialMixerDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASEMixerDefinition (Id PHASESpatialMixerDefinition) where
  toPHASEMixerDefinition = unsafeCastId

-- ---------- PHASEBlendNodeDefinition ----------

-- | *************************************************************************************************
--
-- PHASEBlendNodeDefinition
--
-- An object for defining a blend sound event node when building a sound event.
--
-- A blend node blends between its children based on a numeric parameter.
-- 
-- Phantom type for @PHASEBlendNodeDefinition@.
data PHASEBlendNodeDefinition

instance IsObjCObject (Id PHASEBlendNodeDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEBlendNodeDefinition"

class IsPHASESoundEventNodeDefinition a => IsPHASEBlendNodeDefinition a where
  toPHASEBlendNodeDefinition :: a -> Id PHASEBlendNodeDefinition

instance IsPHASEBlendNodeDefinition (Id PHASEBlendNodeDefinition) where
  toPHASEBlendNodeDefinition = unsafeCastId

instance IsNSObject (Id PHASEBlendNodeDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASEBlendNodeDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASESoundEventNodeDefinition (Id PHASEBlendNodeDefinition) where
  toPHASESoundEventNodeDefinition = unsafeCastId

-- ---------- PHASEContainerNodeDefinition ----------

-- | *************************************************************************************************
--
-- PHASEContainerNodeDefinition
--
-- An object for defining a container sound event node when building a sound event.
--
-- A container node plays back all its children at once.
-- 
-- Phantom type for @PHASEContainerNodeDefinition@.
data PHASEContainerNodeDefinition

instance IsObjCObject (Id PHASEContainerNodeDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEContainerNodeDefinition"

class IsPHASESoundEventNodeDefinition a => IsPHASEContainerNodeDefinition a where
  toPHASEContainerNodeDefinition :: a -> Id PHASEContainerNodeDefinition

instance IsPHASEContainerNodeDefinition (Id PHASEContainerNodeDefinition) where
  toPHASEContainerNodeDefinition = unsafeCastId

instance IsNSObject (Id PHASEContainerNodeDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASEContainerNodeDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASESoundEventNodeDefinition (Id PHASEContainerNodeDefinition) where
  toPHASESoundEventNodeDefinition = unsafeCastId

-- ---------- PHASEGeneratorNodeDefinition ----------

-- | *************************************************************************************************
--
-- PHASEGeneratorNodeDefinition
--
-- An object for defining a generator node when building a sound event.
-- 
-- Phantom type for @PHASEGeneratorNodeDefinition@.
data PHASEGeneratorNodeDefinition

instance IsObjCObject (Id PHASEGeneratorNodeDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEGeneratorNodeDefinition"

class IsPHASESoundEventNodeDefinition a => IsPHASEGeneratorNodeDefinition a where
  toPHASEGeneratorNodeDefinition :: a -> Id PHASEGeneratorNodeDefinition

instance IsPHASEGeneratorNodeDefinition (Id PHASEGeneratorNodeDefinition) where
  toPHASEGeneratorNodeDefinition = unsafeCastId

instance IsNSObject (Id PHASEGeneratorNodeDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASEGeneratorNodeDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASESoundEventNodeDefinition (Id PHASEGeneratorNodeDefinition) where
  toPHASESoundEventNodeDefinition = unsafeCastId

-- ---------- PHASERandomNodeDefinition ----------

-- | *************************************************************************************************
--
-- PHASERandomNodeDefinition
--
-- An object for defining a random sound event node when building a sound event.
--
-- A random node selects one of its children based on a weighted random choice.
-- 
-- Phantom type for @PHASERandomNodeDefinition@.
data PHASERandomNodeDefinition

instance IsObjCObject (Id PHASERandomNodeDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASERandomNodeDefinition"

class IsPHASESoundEventNodeDefinition a => IsPHASERandomNodeDefinition a where
  toPHASERandomNodeDefinition :: a -> Id PHASERandomNodeDefinition

instance IsPHASERandomNodeDefinition (Id PHASERandomNodeDefinition) where
  toPHASERandomNodeDefinition = unsafeCastId

instance IsNSObject (Id PHASERandomNodeDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASERandomNodeDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASESoundEventNodeDefinition (Id PHASERandomNodeDefinition) where
  toPHASESoundEventNodeDefinition = unsafeCastId

-- ---------- PHASESwitchNodeDefinition ----------

-- | *************************************************************************************************
--
-- PHASESwitchNodeDefinition
--
-- An object for defining a switch sound event node when building a sound event.
--
-- A switch node switches between its children based on a string parameter.
-- 
-- Phantom type for @PHASESwitchNodeDefinition@.
data PHASESwitchNodeDefinition

instance IsObjCObject (Id PHASESwitchNodeDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASESwitchNodeDefinition"

class IsPHASESoundEventNodeDefinition a => IsPHASESwitchNodeDefinition a where
  toPHASESwitchNodeDefinition :: a -> Id PHASESwitchNodeDefinition

instance IsPHASESwitchNodeDefinition (Id PHASESwitchNodeDefinition) where
  toPHASESwitchNodeDefinition = unsafeCastId

instance IsNSObject (Id PHASESwitchNodeDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASESwitchNodeDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASESoundEventNodeDefinition (Id PHASESwitchNodeDefinition) where
  toPHASESoundEventNodeDefinition = unsafeCastId

-- ---------- PHASEMappedMetaParameterDefinition ----------

-- | *************************************************************************************************
--
-- PHASEMappedMetaParameterDefinition
--
-- An object to define a Mapped Metaparameter when building an sound event.
-- 
-- Phantom type for @PHASEMappedMetaParameterDefinition@.
data PHASEMappedMetaParameterDefinition

instance IsObjCObject (Id PHASEMappedMetaParameterDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEMappedMetaParameterDefinition"

class IsPHASENumberMetaParameterDefinition a => IsPHASEMappedMetaParameterDefinition a where
  toPHASEMappedMetaParameterDefinition :: a -> Id PHASEMappedMetaParameterDefinition

instance IsPHASEMappedMetaParameterDefinition (Id PHASEMappedMetaParameterDefinition) where
  toPHASEMappedMetaParameterDefinition = unsafeCastId

instance IsNSObject (Id PHASEMappedMetaParameterDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASEMappedMetaParameterDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASEMetaParameterDefinition (Id PHASEMappedMetaParameterDefinition) where
  toPHASEMetaParameterDefinition = unsafeCastId

instance IsPHASENumberMetaParameterDefinition (Id PHASEMappedMetaParameterDefinition) where
  toPHASENumberMetaParameterDefinition = unsafeCastId

-- ---------- PHASEPullStreamNodeDefinition ----------

-- | *************************************************************************************************
--
-- PHASEPullStreamNodeDefinition
--
-- An object for defining a pull stream sound event node when building a sound event.
-- 
-- Phantom type for @PHASEPullStreamNodeDefinition@.
data PHASEPullStreamNodeDefinition

instance IsObjCObject (Id PHASEPullStreamNodeDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEPullStreamNodeDefinition"

class IsPHASEGeneratorNodeDefinition a => IsPHASEPullStreamNodeDefinition a where
  toPHASEPullStreamNodeDefinition :: a -> Id PHASEPullStreamNodeDefinition

instance IsPHASEPullStreamNodeDefinition (Id PHASEPullStreamNodeDefinition) where
  toPHASEPullStreamNodeDefinition = unsafeCastId

instance IsNSObject (Id PHASEPullStreamNodeDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASEPullStreamNodeDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASEGeneratorNodeDefinition (Id PHASEPullStreamNodeDefinition) where
  toPHASEGeneratorNodeDefinition = unsafeCastId

instance IsPHASESoundEventNodeDefinition (Id PHASEPullStreamNodeDefinition) where
  toPHASESoundEventNodeDefinition = unsafeCastId

-- ---------- PHASEPushStreamNodeDefinition ----------

-- | *************************************************************************************************
--
-- PHASEPushStreamNodeDefinition
--
-- An object for defining a push stream sound event node when building a sound event.
-- 
-- Phantom type for @PHASEPushStreamNodeDefinition@.
data PHASEPushStreamNodeDefinition

instance IsObjCObject (Id PHASEPushStreamNodeDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASEPushStreamNodeDefinition"

class IsPHASEGeneratorNodeDefinition a => IsPHASEPushStreamNodeDefinition a where
  toPHASEPushStreamNodeDefinition :: a -> Id PHASEPushStreamNodeDefinition

instance IsPHASEPushStreamNodeDefinition (Id PHASEPushStreamNodeDefinition) where
  toPHASEPushStreamNodeDefinition = unsafeCastId

instance IsNSObject (Id PHASEPushStreamNodeDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASEPushStreamNodeDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASEGeneratorNodeDefinition (Id PHASEPushStreamNodeDefinition) where
  toPHASEGeneratorNodeDefinition = unsafeCastId

instance IsPHASESoundEventNodeDefinition (Id PHASEPushStreamNodeDefinition) where
  toPHASESoundEventNodeDefinition = unsafeCastId

-- ---------- PHASESamplerNodeDefinition ----------

-- | *************************************************************************************************
--
-- PHASESamplerNodeDefinition
--
-- Sampler node definition.
--
-- Sampler nodes play back registered sound assets.
-- 
-- Phantom type for @PHASESamplerNodeDefinition@.
data PHASESamplerNodeDefinition

instance IsObjCObject (Id PHASESamplerNodeDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHASESamplerNodeDefinition"

class IsPHASEGeneratorNodeDefinition a => IsPHASESamplerNodeDefinition a where
  toPHASESamplerNodeDefinition :: a -> Id PHASESamplerNodeDefinition

instance IsPHASESamplerNodeDefinition (Id PHASESamplerNodeDefinition) where
  toPHASESamplerNodeDefinition = unsafeCastId

instance IsNSObject (Id PHASESamplerNodeDefinition) where
  toNSObject = unsafeCastId

instance IsPHASEDefinition (Id PHASESamplerNodeDefinition) where
  toPHASEDefinition = unsafeCastId

instance IsPHASEGeneratorNodeDefinition (Id PHASESamplerNodeDefinition) where
  toPHASEGeneratorNodeDefinition = unsafeCastId

instance IsPHASESoundEventNodeDefinition (Id PHASESamplerNodeDefinition) where
  toPHASESoundEventNodeDefinition = unsafeCastId
