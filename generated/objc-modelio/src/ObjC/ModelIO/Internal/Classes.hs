{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ModelIO.Internal.Classes (
    module ObjC.ModelIO.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- MDLAnimatedValue ----------

-- | Phantom type for @MDLAnimatedValue@.
data MDLAnimatedValue

instance IsObjCObject (Id MDLAnimatedValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLAnimatedValue"

class IsNSObject a => IsMDLAnimatedValue a where
  toMDLAnimatedValue :: a -> Id MDLAnimatedValue

instance IsMDLAnimatedValue (Id MDLAnimatedValue) where
  toMDLAnimatedValue = unsafeCastId

instance IsNSObject (Id MDLAnimatedValue) where
  toNSObject = unsafeCastId

-- ---------- MDLAnimationBindComponent ----------

-- | Phantom type for @MDLAnimationBindComponent@.
data MDLAnimationBindComponent

instance IsObjCObject (Id MDLAnimationBindComponent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLAnimationBindComponent"

class IsNSObject a => IsMDLAnimationBindComponent a where
  toMDLAnimationBindComponent :: a -> Id MDLAnimationBindComponent

instance IsMDLAnimationBindComponent (Id MDLAnimationBindComponent) where
  toMDLAnimationBindComponent = unsafeCastId

instance IsNSObject (Id MDLAnimationBindComponent) where
  toNSObject = unsafeCastId

-- ---------- MDLAsset ----------

-- | MDLAsset
--
-- An MDLAsset represents the contents of a model file.
--
-- Each asset contains a collection of hierarchies of objects, where              each object in the asset is the top level of a hierarchy. Objects             include transforms, lights, cameras, and meshes.
--
-- MDLAssets are typically instantiated from NSURLs that refer to a model resource.
--
-- The model resource may represented timed information, for example, a series of  mesh morphs. If the asset is timed, then the framerate will be non-zero, and the  firstFrame and lastFrame properties will indicate the range for which sample  data exists. Samples before or after that range will be clamped. Some model  resource representations allow continuous sampling, others are discrete. In the  discrete case, if a requested sample time is not on a discrete boundary the  returned sample will be the sample exactly on the sample time, or if no such is  available, the immediately preceding sample. If no time is specified for a  sample, the first data will be returned.
--
-- An asset's bounding box can be queried without traversing the hierarchy of  objects.
--
-- Fast enumeration of an MDLAsset iterates the top level objects contained within.
-- 
-- Phantom type for @MDLAsset@.
data MDLAsset

instance IsObjCObject (Id MDLAsset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLAsset"

class IsNSObject a => IsMDLAsset a where
  toMDLAsset :: a -> Id MDLAsset

instance IsMDLAsset (Id MDLAsset) where
  toMDLAsset = unsafeCastId

instance IsNSObject (Id MDLAsset) where
  toNSObject = unsafeCastId

-- ---------- MDLBundleAssetResolver ----------

-- | Phantom type for @MDLBundleAssetResolver@.
data MDLBundleAssetResolver

instance IsObjCObject (Id MDLBundleAssetResolver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLBundleAssetResolver"

class IsNSObject a => IsMDLBundleAssetResolver a where
  toMDLBundleAssetResolver :: a -> Id MDLBundleAssetResolver

instance IsMDLBundleAssetResolver (Id MDLBundleAssetResolver) where
  toMDLBundleAssetResolver = unsafeCastId

instance IsNSObject (Id MDLBundleAssetResolver) where
  toNSObject = unsafeCastId

-- ---------- MDLMaterial ----------

-- | Phantom type for @MDLMaterial@.
data MDLMaterial

instance IsObjCObject (Id MDLMaterial) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLMaterial"

class IsNSObject a => IsMDLMaterial a where
  toMDLMaterial :: a -> Id MDLMaterial

instance IsMDLMaterial (Id MDLMaterial) where
  toMDLMaterial = unsafeCastId

instance IsNSObject (Id MDLMaterial) where
  toNSObject = unsafeCastId

-- ---------- MDLMaterialProperty ----------

-- | If a color is encoded in a floatN property, it is to be interpreted as  a Rec 709 color.
-- 
-- Phantom type for @MDLMaterialProperty@.
data MDLMaterialProperty

instance IsObjCObject (Id MDLMaterialProperty) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLMaterialProperty"

class IsNSObject a => IsMDLMaterialProperty a where
  toMDLMaterialProperty :: a -> Id MDLMaterialProperty

instance IsMDLMaterialProperty (Id MDLMaterialProperty) where
  toMDLMaterialProperty = unsafeCastId

instance IsNSObject (Id MDLMaterialProperty) where
  toNSObject = unsafeCastId

-- ---------- MDLMaterialPropertyConnection ----------

-- | Phantom type for @MDLMaterialPropertyConnection@.
data MDLMaterialPropertyConnection

instance IsObjCObject (Id MDLMaterialPropertyConnection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLMaterialPropertyConnection"

class IsNSObject a => IsMDLMaterialPropertyConnection a where
  toMDLMaterialPropertyConnection :: a -> Id MDLMaterialPropertyConnection

instance IsMDLMaterialPropertyConnection (Id MDLMaterialPropertyConnection) where
  toMDLMaterialPropertyConnection = unsafeCastId

instance IsNSObject (Id MDLMaterialPropertyConnection) where
  toNSObject = unsafeCastId

-- ---------- MDLMaterialPropertyNode ----------

-- | Phantom type for @MDLMaterialPropertyNode@.
data MDLMaterialPropertyNode

instance IsObjCObject (Id MDLMaterialPropertyNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLMaterialPropertyNode"

class IsNSObject a => IsMDLMaterialPropertyNode a where
  toMDLMaterialPropertyNode :: a -> Id MDLMaterialPropertyNode

instance IsMDLMaterialPropertyNode (Id MDLMaterialPropertyNode) where
  toMDLMaterialPropertyNode = unsafeCastId

instance IsNSObject (Id MDLMaterialPropertyNode) where
  toNSObject = unsafeCastId

-- ---------- MDLMatrix4x4Array ----------

-- | Phantom type for @MDLMatrix4x4Array@.
data MDLMatrix4x4Array

instance IsObjCObject (Id MDLMatrix4x4Array) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLMatrix4x4Array"

class IsNSObject a => IsMDLMatrix4x4Array a where
  toMDLMatrix4x4Array :: a -> Id MDLMatrix4x4Array

instance IsMDLMatrix4x4Array (Id MDLMatrix4x4Array) where
  toMDLMatrix4x4Array = unsafeCastId

instance IsNSObject (Id MDLMatrix4x4Array) where
  toNSObject = unsafeCastId

-- ---------- MDLMeshBufferData ----------

-- | MDLMeshBufferData
--
-- A CPU memory backed mesh buffer
-- 
-- Phantom type for @MDLMeshBufferData@.
data MDLMeshBufferData

instance IsObjCObject (Id MDLMeshBufferData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLMeshBufferData"

class IsNSObject a => IsMDLMeshBufferData a where
  toMDLMeshBufferData :: a -> Id MDLMeshBufferData

instance IsMDLMeshBufferData (Id MDLMeshBufferData) where
  toMDLMeshBufferData = unsafeCastId

instance IsNSObject (Id MDLMeshBufferData) where
  toNSObject = unsafeCastId

-- ---------- MDLMeshBufferDataAllocator ----------

-- | An allocator to use when backing with an NSData is appropriate.
-- 
-- Phantom type for @MDLMeshBufferDataAllocator@.
data MDLMeshBufferDataAllocator

instance IsObjCObject (Id MDLMeshBufferDataAllocator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLMeshBufferDataAllocator"

class IsNSObject a => IsMDLMeshBufferDataAllocator a where
  toMDLMeshBufferDataAllocator :: a -> Id MDLMeshBufferDataAllocator

instance IsMDLMeshBufferDataAllocator (Id MDLMeshBufferDataAllocator) where
  toMDLMeshBufferDataAllocator = unsafeCastId

instance IsNSObject (Id MDLMeshBufferDataAllocator) where
  toNSObject = unsafeCastId

-- ---------- MDLMeshBufferMap ----------

-- | MDLMeshBufferMap
--
-- Represents a reference to memory of a mapped MeshBuffer
-- 
-- Phantom type for @MDLMeshBufferMap@.
data MDLMeshBufferMap

instance IsObjCObject (Id MDLMeshBufferMap) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLMeshBufferMap"

class IsNSObject a => IsMDLMeshBufferMap a where
  toMDLMeshBufferMap :: a -> Id MDLMeshBufferMap

instance IsMDLMeshBufferMap (Id MDLMeshBufferMap) where
  toMDLMeshBufferMap = unsafeCastId

instance IsNSObject (Id MDLMeshBufferMap) where
  toNSObject = unsafeCastId

-- ---------- MDLMeshBufferZoneDefault ----------

-- | A default zone that can be use for convenience
-- 
-- Phantom type for @MDLMeshBufferZoneDefault@.
data MDLMeshBufferZoneDefault

instance IsObjCObject (Id MDLMeshBufferZoneDefault) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLMeshBufferZoneDefault"

class IsNSObject a => IsMDLMeshBufferZoneDefault a where
  toMDLMeshBufferZoneDefault :: a -> Id MDLMeshBufferZoneDefault

instance IsMDLMeshBufferZoneDefault (Id MDLMeshBufferZoneDefault) where
  toMDLMeshBufferZoneDefault = unsafeCastId

instance IsNSObject (Id MDLMeshBufferZoneDefault) where
  toNSObject = unsafeCastId

-- ---------- MDLObject ----------

-- | MDLObject
--
-- Base class for object in a ModelIO asset hierarchy
--
-- Includes transformation and bounds info, links to parent and             children in the hierachy
-- 
-- Phantom type for @MDLObject@.
data MDLObject

instance IsObjCObject (Id MDLObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLObject"

class IsNSObject a => IsMDLObject a where
  toMDLObject :: a -> Id MDLObject

instance IsMDLObject (Id MDLObject) where
  toMDLObject = unsafeCastId

instance IsNSObject (Id MDLObject) where
  toNSObject = unsafeCastId

-- ---------- MDLObjectContainer ----------

-- | MDLObjectContainer
--
-- Default container object
--
-- Subclass the object container to support custom containers. Such              custom containers might reference in memory representations, offline              databases, and so on.
-- 
-- Phantom type for @MDLObjectContainer@.
data MDLObjectContainer

instance IsObjCObject (Id MDLObjectContainer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLObjectContainer"

class IsNSObject a => IsMDLObjectContainer a where
  toMDLObjectContainer :: a -> Id MDLObjectContainer

instance IsMDLObjectContainer (Id MDLObjectContainer) where
  toMDLObjectContainer = unsafeCastId

instance IsNSObject (Id MDLObjectContainer) where
  toNSObject = unsafeCastId

-- ---------- MDLPathAssetResolver ----------

-- | MDLPathAssetResolver
--
-- The path asset resolver searches for referenced files by prepending path.
--
-- Path should resolve to a well formed URI. A file system path might take the form "file:///path/to/all/assets/
--
-- A trailing slash is automatically appended to path if not provided.
-- 
-- Phantom type for @MDLPathAssetResolver@.
data MDLPathAssetResolver

instance IsObjCObject (Id MDLPathAssetResolver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLPathAssetResolver"

class IsNSObject a => IsMDLPathAssetResolver a where
  toMDLPathAssetResolver :: a -> Id MDLPathAssetResolver

instance IsMDLPathAssetResolver (Id MDLPathAssetResolver) where
  toMDLPathAssetResolver = unsafeCastId

instance IsNSObject (Id MDLPathAssetResolver) where
  toNSObject = unsafeCastId

-- ---------- MDLRelativeAssetResolver ----------

-- | MDLRelativeAssetResolver
--
-- The relative asset resolver searches for referenced files by checking the location of the asset for sibling files satisfying the requested name.
-- 
-- Phantom type for @MDLRelativeAssetResolver@.
data MDLRelativeAssetResolver

instance IsObjCObject (Id MDLRelativeAssetResolver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLRelativeAssetResolver"

class IsNSObject a => IsMDLRelativeAssetResolver a where
  toMDLRelativeAssetResolver :: a -> Id MDLRelativeAssetResolver

instance IsMDLRelativeAssetResolver (Id MDLRelativeAssetResolver) where
  toMDLRelativeAssetResolver = unsafeCastId

instance IsNSObject (Id MDLRelativeAssetResolver) where
  toNSObject = unsafeCastId

-- ---------- MDLScatteringFunction ----------

-- | The base scattering function is Lambertian, with a Blinn-Phong specular response. Specular power for Blinn-Phong can be derived from the roughness property using  an approximation.
-- 
-- Phantom type for @MDLScatteringFunction@.
data MDLScatteringFunction

instance IsObjCObject (Id MDLScatteringFunction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLScatteringFunction"

class IsNSObject a => IsMDLScatteringFunction a where
  toMDLScatteringFunction :: a -> Id MDLScatteringFunction

instance IsMDLScatteringFunction (Id MDLScatteringFunction) where
  toMDLScatteringFunction = unsafeCastId

instance IsNSObject (Id MDLScatteringFunction) where
  toNSObject = unsafeCastId

-- ---------- MDLSubmesh ----------

-- | MDLSubmesh
--
-- A drawable subset of an MDLMesh, with its own material
-- 
-- Phantom type for @MDLSubmesh@.
data MDLSubmesh

instance IsObjCObject (Id MDLSubmesh) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLSubmesh"

class IsNSObject a => IsMDLSubmesh a where
  toMDLSubmesh :: a -> Id MDLSubmesh

instance IsMDLSubmesh (Id MDLSubmesh) where
  toMDLSubmesh = unsafeCastId

instance IsNSObject (Id MDLSubmesh) where
  toNSObject = unsafeCastId

-- ---------- MDLSubmeshTopology ----------

-- | Phantom type for @MDLSubmeshTopology@.
data MDLSubmeshTopology

instance IsObjCObject (Id MDLSubmeshTopology) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLSubmeshTopology"

class IsNSObject a => IsMDLSubmeshTopology a where
  toMDLSubmeshTopology :: a -> Id MDLSubmeshTopology

instance IsMDLSubmeshTopology (Id MDLSubmeshTopology) where
  toMDLSubmeshTopology = unsafeCastId

instance IsNSObject (Id MDLSubmeshTopology) where
  toNSObject = unsafeCastId

-- ---------- MDLTexture ----------

-- | MDLTexture  a description of texels provided by a texture object.
--
-- A texture optionally generates or loads texels             through an access to the data property, or one of the other              properties, otherwise the texture object is a lightweight descriptor              only.
--
-- data
--
-- Texel data that will exist when referenced; it may or may not exist            before
--
-- dimensions
--
-- texel width and height of the texture
--
-- rowStride
--
-- The number of bytes from the first texel in a row to the first texel            in the next row. A rowStride of zero indicates that interleaved x,y            addressing of texels is not possible. This might be the case if the           texture was compressed in some manner, for example.
--
-- channelCount
--
-- The number of channels incoded in a single texel. For example, an RGB            texture has 3 channels. All channels must have the same encoding.
--
-- channelEncoding
--
-- The encoding of a channel in a single texel.
--
-- isCube
--
-- The texture encodes a cube map. If YES, then the layout of the cube            map is deduced as a vertical strip if dimension.y is six times            dimension.x. Other layouts are possible in the future.
-- 
-- Phantom type for @MDLTexture@.
data MDLTexture

instance IsObjCObject (Id MDLTexture) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLTexture"

class IsNSObject a => IsMDLTexture a where
  toMDLTexture :: a -> Id MDLTexture

instance IsMDLTexture (Id MDLTexture) where
  toMDLTexture = unsafeCastId

instance IsNSObject (Id MDLTexture) where
  toNSObject = unsafeCastId

-- ---------- MDLTextureFilter ----------

-- | Phantom type for @MDLTextureFilter@.
data MDLTextureFilter

instance IsObjCObject (Id MDLTextureFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLTextureFilter"

class IsNSObject a => IsMDLTextureFilter a where
  toMDLTextureFilter :: a -> Id MDLTextureFilter

instance IsMDLTextureFilter (Id MDLTextureFilter) where
  toMDLTextureFilter = unsafeCastId

instance IsNSObject (Id MDLTextureFilter) where
  toNSObject = unsafeCastId

-- ---------- MDLTextureSampler ----------

-- | Phantom type for @MDLTextureSampler@.
data MDLTextureSampler

instance IsObjCObject (Id MDLTextureSampler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLTextureSampler"

class IsNSObject a => IsMDLTextureSampler a where
  toMDLTextureSampler :: a -> Id MDLTextureSampler

instance IsMDLTextureSampler (Id MDLTextureSampler) where
  toMDLTextureSampler = unsafeCastId

instance IsNSObject (Id MDLTextureSampler) where
  toNSObject = unsafeCastId

-- ---------- MDLTransform ----------

-- | Concrete implementation of <MDLTransformComponent>. For more complex transform components create a class that conforms to  <MDLTransformComponent>.
--
-- Setting any of scale, translation, or rotation individually will  set the matrix property, and clear any timing information.
-- 
-- Phantom type for @MDLTransform@.
data MDLTransform

instance IsObjCObject (Id MDLTransform) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLTransform"

class IsNSObject a => IsMDLTransform a where
  toMDLTransform :: a -> Id MDLTransform

instance IsMDLTransform (Id MDLTransform) where
  toMDLTransform = unsafeCastId

instance IsNSObject (Id MDLTransform) where
  toNSObject = unsafeCastId

-- ---------- MDLTransformMatrixOp ----------

-- | Phantom type for @MDLTransformMatrixOp@.
data MDLTransformMatrixOp

instance IsObjCObject (Id MDLTransformMatrixOp) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLTransformMatrixOp"

class IsNSObject a => IsMDLTransformMatrixOp a where
  toMDLTransformMatrixOp :: a -> Id MDLTransformMatrixOp

instance IsMDLTransformMatrixOp (Id MDLTransformMatrixOp) where
  toMDLTransformMatrixOp = unsafeCastId

instance IsNSObject (Id MDLTransformMatrixOp) where
  toNSObject = unsafeCastId

-- ---------- MDLTransformOrientOp ----------

-- | Phantom type for @MDLTransformOrientOp@.
data MDLTransformOrientOp

instance IsObjCObject (Id MDLTransformOrientOp) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLTransformOrientOp"

class IsNSObject a => IsMDLTransformOrientOp a where
  toMDLTransformOrientOp :: a -> Id MDLTransformOrientOp

instance IsMDLTransformOrientOp (Id MDLTransformOrientOp) where
  toMDLTransformOrientOp = unsafeCastId

instance IsNSObject (Id MDLTransformOrientOp) where
  toNSObject = unsafeCastId

-- ---------- MDLTransformRotateOp ----------

-- | Phantom type for @MDLTransformRotateOp@.
data MDLTransformRotateOp

instance IsObjCObject (Id MDLTransformRotateOp) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLTransformRotateOp"

class IsNSObject a => IsMDLTransformRotateOp a where
  toMDLTransformRotateOp :: a -> Id MDLTransformRotateOp

instance IsMDLTransformRotateOp (Id MDLTransformRotateOp) where
  toMDLTransformRotateOp = unsafeCastId

instance IsNSObject (Id MDLTransformRotateOp) where
  toNSObject = unsafeCastId

-- ---------- MDLTransformRotateXOp ----------

-- | Phantom type for @MDLTransformRotateXOp@.
data MDLTransformRotateXOp

instance IsObjCObject (Id MDLTransformRotateXOp) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLTransformRotateXOp"

class IsNSObject a => IsMDLTransformRotateXOp a where
  toMDLTransformRotateXOp :: a -> Id MDLTransformRotateXOp

instance IsMDLTransformRotateXOp (Id MDLTransformRotateXOp) where
  toMDLTransformRotateXOp = unsafeCastId

instance IsNSObject (Id MDLTransformRotateXOp) where
  toNSObject = unsafeCastId

-- ---------- MDLTransformRotateYOp ----------

-- | Phantom type for @MDLTransformRotateYOp@.
data MDLTransformRotateYOp

instance IsObjCObject (Id MDLTransformRotateYOp) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLTransformRotateYOp"

class IsNSObject a => IsMDLTransformRotateYOp a where
  toMDLTransformRotateYOp :: a -> Id MDLTransformRotateYOp

instance IsMDLTransformRotateYOp (Id MDLTransformRotateYOp) where
  toMDLTransformRotateYOp = unsafeCastId

instance IsNSObject (Id MDLTransformRotateYOp) where
  toNSObject = unsafeCastId

-- ---------- MDLTransformRotateZOp ----------

-- | Phantom type for @MDLTransformRotateZOp@.
data MDLTransformRotateZOp

instance IsObjCObject (Id MDLTransformRotateZOp) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLTransformRotateZOp"

class IsNSObject a => IsMDLTransformRotateZOp a where
  toMDLTransformRotateZOp :: a -> Id MDLTransformRotateZOp

instance IsMDLTransformRotateZOp (Id MDLTransformRotateZOp) where
  toMDLTransformRotateZOp = unsafeCastId

instance IsNSObject (Id MDLTransformRotateZOp) where
  toNSObject = unsafeCastId

-- ---------- MDLTransformScaleOp ----------

-- | Phantom type for @MDLTransformScaleOp@.
data MDLTransformScaleOp

instance IsObjCObject (Id MDLTransformScaleOp) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLTransformScaleOp"

class IsNSObject a => IsMDLTransformScaleOp a where
  toMDLTransformScaleOp :: a -> Id MDLTransformScaleOp

instance IsMDLTransformScaleOp (Id MDLTransformScaleOp) where
  toMDLTransformScaleOp = unsafeCastId

instance IsNSObject (Id MDLTransformScaleOp) where
  toNSObject = unsafeCastId

-- ---------- MDLTransformStack ----------

-- | Phantom type for @MDLTransformStack@.
data MDLTransformStack

instance IsObjCObject (Id MDLTransformStack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLTransformStack"

class IsNSObject a => IsMDLTransformStack a where
  toMDLTransformStack :: a -> Id MDLTransformStack

instance IsMDLTransformStack (Id MDLTransformStack) where
  toMDLTransformStack = unsafeCastId

instance IsNSObject (Id MDLTransformStack) where
  toNSObject = unsafeCastId

-- ---------- MDLTransformTranslateOp ----------

-- | Phantom type for @MDLTransformTranslateOp@.
data MDLTransformTranslateOp

instance IsObjCObject (Id MDLTransformTranslateOp) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLTransformTranslateOp"

class IsNSObject a => IsMDLTransformTranslateOp a where
  toMDLTransformTranslateOp :: a -> Id MDLTransformTranslateOp

instance IsMDLTransformTranslateOp (Id MDLTransformTranslateOp) where
  toMDLTransformTranslateOp = unsafeCastId

instance IsNSObject (Id MDLTransformTranslateOp) where
  toNSObject = unsafeCastId

-- ---------- MDLUtility ----------

-- | Phantom type for @MDLUtility@.
data MDLUtility

instance IsObjCObject (Id MDLUtility) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLUtility"

class IsNSObject a => IsMDLUtility a where
  toMDLUtility :: a -> Id MDLUtility

instance IsMDLUtility (Id MDLUtility) where
  toMDLUtility = unsafeCastId

instance IsNSObject (Id MDLUtility) where
  toNSObject = unsafeCastId

-- ---------- MDLVertexAttribute ----------

-- | MDLVertexAttribute
--
-- Structure with properties of a vertex attribute
-- 
-- Phantom type for @MDLVertexAttribute@.
data MDLVertexAttribute

instance IsObjCObject (Id MDLVertexAttribute) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLVertexAttribute"

class IsNSObject a => IsMDLVertexAttribute a where
  toMDLVertexAttribute :: a -> Id MDLVertexAttribute

instance IsMDLVertexAttribute (Id MDLVertexAttribute) where
  toMDLVertexAttribute = unsafeCastId

instance IsNSObject (Id MDLVertexAttribute) where
  toNSObject = unsafeCastId

-- ---------- MDLVertexAttributeData ----------

-- | MDLVertexAttributeData
--
-- convenience object to quickly access vertex attribute data
--
-- created by MDLMesh's vertexAttributeData selector             Setting values on this object has no effect on the             underlying objects.
-- 
-- Phantom type for @MDLVertexAttributeData@.
data MDLVertexAttributeData

instance IsObjCObject (Id MDLVertexAttributeData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLVertexAttributeData"

class IsNSObject a => IsMDLVertexAttributeData a where
  toMDLVertexAttributeData :: a -> Id MDLVertexAttributeData

instance IsMDLVertexAttributeData (Id MDLVertexAttributeData) where
  toMDLVertexAttributeData = unsafeCastId

instance IsNSObject (Id MDLVertexAttributeData) where
  toNSObject = unsafeCastId

-- ---------- MDLVertexBufferLayout ----------

-- | MDLVertexBufferLayout
--
-- Describes a vertex buffer's layout
-- 
-- Phantom type for @MDLVertexBufferLayout@.
data MDLVertexBufferLayout

instance IsObjCObject (Id MDLVertexBufferLayout) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLVertexBufferLayout"

class IsNSObject a => IsMDLVertexBufferLayout a where
  toMDLVertexBufferLayout :: a -> Id MDLVertexBufferLayout

instance IsMDLVertexBufferLayout (Id MDLVertexBufferLayout) where
  toMDLVertexBufferLayout = unsafeCastId

instance IsNSObject (Id MDLVertexBufferLayout) where
  toNSObject = unsafeCastId

-- ---------- MDLVertexDescriptor ----------

-- | MDLVertexDescriptor
--
-- Describes the layout of vertex buffers in MDLMesh objects
--
-- This object is a property of MDLMesh describing the current state of attributes and buffer layouts of the vertex buffers in the mesh. This must be  immutable otherwise even small changes could cause the buffers to be out of sync  with the layout described here.
--
-- Designed to be very similar to MTLVertexDescriptor to ease creation of one from  the other
-- 
-- Phantom type for @MDLVertexDescriptor@.
data MDLVertexDescriptor

instance IsObjCObject (Id MDLVertexDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLVertexDescriptor"

class IsNSObject a => IsMDLVertexDescriptor a where
  toMDLVertexDescriptor :: a -> Id MDLVertexDescriptor

instance IsMDLVertexDescriptor (Id MDLVertexDescriptor) where
  toMDLVertexDescriptor = unsafeCastId

instance IsNSObject (Id MDLVertexDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MDLAnimatedMatrix4x4 ----------

-- | Phantom type for @MDLAnimatedMatrix4x4@.
data MDLAnimatedMatrix4x4

instance IsObjCObject (Id MDLAnimatedMatrix4x4) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLAnimatedMatrix4x4"

class IsMDLAnimatedValue a => IsMDLAnimatedMatrix4x4 a where
  toMDLAnimatedMatrix4x4 :: a -> Id MDLAnimatedMatrix4x4

instance IsMDLAnimatedMatrix4x4 (Id MDLAnimatedMatrix4x4) where
  toMDLAnimatedMatrix4x4 = unsafeCastId

instance IsMDLAnimatedValue (Id MDLAnimatedMatrix4x4) where
  toMDLAnimatedValue = unsafeCastId

instance IsNSObject (Id MDLAnimatedMatrix4x4) where
  toNSObject = unsafeCastId

-- ---------- MDLAnimatedQuaternion ----------

-- | Phantom type for @MDLAnimatedQuaternion@.
data MDLAnimatedQuaternion

instance IsObjCObject (Id MDLAnimatedQuaternion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLAnimatedQuaternion"

class IsMDLAnimatedValue a => IsMDLAnimatedQuaternion a where
  toMDLAnimatedQuaternion :: a -> Id MDLAnimatedQuaternion

instance IsMDLAnimatedQuaternion (Id MDLAnimatedQuaternion) where
  toMDLAnimatedQuaternion = unsafeCastId

instance IsMDLAnimatedValue (Id MDLAnimatedQuaternion) where
  toMDLAnimatedValue = unsafeCastId

instance IsNSObject (Id MDLAnimatedQuaternion) where
  toNSObject = unsafeCastId

-- ---------- MDLAnimatedQuaternionArray ----------

-- | Phantom type for @MDLAnimatedQuaternionArray@.
data MDLAnimatedQuaternionArray

instance IsObjCObject (Id MDLAnimatedQuaternionArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLAnimatedQuaternionArray"

class IsMDLAnimatedValue a => IsMDLAnimatedQuaternionArray a where
  toMDLAnimatedQuaternionArray :: a -> Id MDLAnimatedQuaternionArray

instance IsMDLAnimatedQuaternionArray (Id MDLAnimatedQuaternionArray) where
  toMDLAnimatedQuaternionArray = unsafeCastId

instance IsMDLAnimatedValue (Id MDLAnimatedQuaternionArray) where
  toMDLAnimatedValue = unsafeCastId

instance IsNSObject (Id MDLAnimatedQuaternionArray) where
  toNSObject = unsafeCastId

-- ---------- MDLAnimatedScalar ----------

-- | AUTO-GENERATED FROM CodeGen.h
-- 
-- Phantom type for @MDLAnimatedScalar@.
data MDLAnimatedScalar

instance IsObjCObject (Id MDLAnimatedScalar) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLAnimatedScalar"

class IsMDLAnimatedValue a => IsMDLAnimatedScalar a where
  toMDLAnimatedScalar :: a -> Id MDLAnimatedScalar

instance IsMDLAnimatedScalar (Id MDLAnimatedScalar) where
  toMDLAnimatedScalar = unsafeCastId

instance IsMDLAnimatedValue (Id MDLAnimatedScalar) where
  toMDLAnimatedValue = unsafeCastId

instance IsNSObject (Id MDLAnimatedScalar) where
  toNSObject = unsafeCastId

-- ---------- MDLAnimatedScalarArray ----------

-- | AUTO-GENERATED FROM CodeGenArray.h
-- 
-- Phantom type for @MDLAnimatedScalarArray@.
data MDLAnimatedScalarArray

instance IsObjCObject (Id MDLAnimatedScalarArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLAnimatedScalarArray"

class IsMDLAnimatedValue a => IsMDLAnimatedScalarArray a where
  toMDLAnimatedScalarArray :: a -> Id MDLAnimatedScalarArray

instance IsMDLAnimatedScalarArray (Id MDLAnimatedScalarArray) where
  toMDLAnimatedScalarArray = unsafeCastId

instance IsMDLAnimatedValue (Id MDLAnimatedScalarArray) where
  toMDLAnimatedValue = unsafeCastId

instance IsNSObject (Id MDLAnimatedScalarArray) where
  toNSObject = unsafeCastId

-- ---------- MDLAnimatedVector2 ----------

-- | Phantom type for @MDLAnimatedVector2@.
data MDLAnimatedVector2

instance IsObjCObject (Id MDLAnimatedVector2) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLAnimatedVector2"

class IsMDLAnimatedValue a => IsMDLAnimatedVector2 a where
  toMDLAnimatedVector2 :: a -> Id MDLAnimatedVector2

instance IsMDLAnimatedVector2 (Id MDLAnimatedVector2) where
  toMDLAnimatedVector2 = unsafeCastId

instance IsMDLAnimatedValue (Id MDLAnimatedVector2) where
  toMDLAnimatedValue = unsafeCastId

instance IsNSObject (Id MDLAnimatedVector2) where
  toNSObject = unsafeCastId

-- ---------- MDLAnimatedVector3 ----------

-- | Phantom type for @MDLAnimatedVector3@.
data MDLAnimatedVector3

instance IsObjCObject (Id MDLAnimatedVector3) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLAnimatedVector3"

class IsMDLAnimatedValue a => IsMDLAnimatedVector3 a where
  toMDLAnimatedVector3 :: a -> Id MDLAnimatedVector3

instance IsMDLAnimatedVector3 (Id MDLAnimatedVector3) where
  toMDLAnimatedVector3 = unsafeCastId

instance IsMDLAnimatedValue (Id MDLAnimatedVector3) where
  toMDLAnimatedValue = unsafeCastId

instance IsNSObject (Id MDLAnimatedVector3) where
  toNSObject = unsafeCastId

-- ---------- MDLAnimatedVector3Array ----------

-- | Phantom type for @MDLAnimatedVector3Array@.
data MDLAnimatedVector3Array

instance IsObjCObject (Id MDLAnimatedVector3Array) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLAnimatedVector3Array"

class IsMDLAnimatedValue a => IsMDLAnimatedVector3Array a where
  toMDLAnimatedVector3Array :: a -> Id MDLAnimatedVector3Array

instance IsMDLAnimatedVector3Array (Id MDLAnimatedVector3Array) where
  toMDLAnimatedVector3Array = unsafeCastId

instance IsMDLAnimatedValue (Id MDLAnimatedVector3Array) where
  toMDLAnimatedValue = unsafeCastId

instance IsNSObject (Id MDLAnimatedVector3Array) where
  toNSObject = unsafeCastId

-- ---------- MDLAnimatedVector4 ----------

-- | Phantom type for @MDLAnimatedVector4@.
data MDLAnimatedVector4

instance IsObjCObject (Id MDLAnimatedVector4) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLAnimatedVector4"

class IsMDLAnimatedValue a => IsMDLAnimatedVector4 a where
  toMDLAnimatedVector4 :: a -> Id MDLAnimatedVector4

instance IsMDLAnimatedVector4 (Id MDLAnimatedVector4) where
  toMDLAnimatedVector4 = unsafeCastId

instance IsMDLAnimatedValue (Id MDLAnimatedVector4) where
  toMDLAnimatedValue = unsafeCastId

instance IsNSObject (Id MDLAnimatedVector4) where
  toNSObject = unsafeCastId

-- ---------- MDLMaterialPropertyGraph ----------

-- | inputs and outputs will contain all of the inputs and outputs             external to the graph, which are all the inputs and outputs not             internally connected to something
-- 
-- Phantom type for @MDLMaterialPropertyGraph@.
data MDLMaterialPropertyGraph

instance IsObjCObject (Id MDLMaterialPropertyGraph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLMaterialPropertyGraph"

class IsMDLMaterialPropertyNode a => IsMDLMaterialPropertyGraph a where
  toMDLMaterialPropertyGraph :: a -> Id MDLMaterialPropertyGraph

instance IsMDLMaterialPropertyGraph (Id MDLMaterialPropertyGraph) where
  toMDLMaterialPropertyGraph = unsafeCastId

instance IsMDLMaterialPropertyNode (Id MDLMaterialPropertyGraph) where
  toMDLMaterialPropertyNode = unsafeCastId

instance IsNSObject (Id MDLMaterialPropertyGraph) where
  toNSObject = unsafeCastId

-- ---------- MDLCamera ----------

-- | Phantom type for @MDLCamera@.
data MDLCamera

instance IsObjCObject (Id MDLCamera) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLCamera"

class IsMDLObject a => IsMDLCamera a where
  toMDLCamera :: a -> Id MDLCamera

instance IsMDLCamera (Id MDLCamera) where
  toMDLCamera = unsafeCastId

instance IsMDLObject (Id MDLCamera) where
  toMDLObject = unsafeCastId

instance IsNSObject (Id MDLCamera) where
  toNSObject = unsafeCastId

-- ---------- MDLLight ----------

-- | Phantom type for @MDLLight@.
data MDLLight

instance IsObjCObject (Id MDLLight) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLLight"

class IsMDLObject a => IsMDLLight a where
  toMDLLight :: a -> Id MDLLight

instance IsMDLLight (Id MDLLight) where
  toMDLLight = unsafeCastId

instance IsMDLObject (Id MDLLight) where
  toMDLObject = unsafeCastId

instance IsNSObject (Id MDLLight) where
  toNSObject = unsafeCastId

-- ---------- MDLMesh ----------

-- | MDLMesh
--
-- A vertex buffer with info to interpret vertex data
--
-- Includes a collection of submeshs which have indexbuffer and             material information
-- 
-- Phantom type for @MDLMesh@.
data MDLMesh

instance IsObjCObject (Id MDLMesh) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLMesh"

class IsMDLObject a => IsMDLMesh a where
  toMDLMesh :: a -> Id MDLMesh

instance IsMDLMesh (Id MDLMesh) where
  toMDLMesh = unsafeCastId

instance IsMDLObject (Id MDLMesh) where
  toMDLObject = unsafeCastId

instance IsNSObject (Id MDLMesh) where
  toNSObject = unsafeCastId

-- ---------- MDLPackedJointAnimation ----------

-- | Phantom type for @MDLPackedJointAnimation@.
data MDLPackedJointAnimation

instance IsObjCObject (Id MDLPackedJointAnimation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLPackedJointAnimation"

class IsMDLObject a => IsMDLPackedJointAnimation a where
  toMDLPackedJointAnimation :: a -> Id MDLPackedJointAnimation

instance IsMDLPackedJointAnimation (Id MDLPackedJointAnimation) where
  toMDLPackedJointAnimation = unsafeCastId

instance IsMDLObject (Id MDLPackedJointAnimation) where
  toMDLObject = unsafeCastId

instance IsNSObject (Id MDLPackedJointAnimation) where
  toNSObject = unsafeCastId

-- ---------- MDLSkeleton ----------

-- | Phantom type for @MDLSkeleton@.
data MDLSkeleton

instance IsObjCObject (Id MDLSkeleton) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLSkeleton"

class IsMDLObject a => IsMDLSkeleton a where
  toMDLSkeleton :: a -> Id MDLSkeleton

instance IsMDLSkeleton (Id MDLSkeleton) where
  toMDLSkeleton = unsafeCastId

instance IsMDLObject (Id MDLSkeleton) where
  toMDLObject = unsafeCastId

instance IsNSObject (Id MDLSkeleton) where
  toNSObject = unsafeCastId

-- ---------- MDLVoxelArray ----------

-- | MDLVoxelArray
--
-- Voxel data represented on a three dimensional grid. Voxel data can          include voxels considered to be on the surface of an object, and a           series of shells on the outside and inside of the surface.
-- 
-- Phantom type for @MDLVoxelArray@.
data MDLVoxelArray

instance IsObjCObject (Id MDLVoxelArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLVoxelArray"

class IsMDLObject a => IsMDLVoxelArray a where
  toMDLVoxelArray :: a -> Id MDLVoxelArray

instance IsMDLVoxelArray (Id MDLVoxelArray) where
  toMDLVoxelArray = unsafeCastId

instance IsMDLObject (Id MDLVoxelArray) where
  toMDLObject = unsafeCastId

instance IsNSObject (Id MDLVoxelArray) where
  toNSObject = unsafeCastId

-- ---------- MDLPhysicallyPlausibleScatteringFunction ----------

-- | Phantom type for @MDLPhysicallyPlausibleScatteringFunction@.
data MDLPhysicallyPlausibleScatteringFunction

instance IsObjCObject (Id MDLPhysicallyPlausibleScatteringFunction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLPhysicallyPlausibleScatteringFunction"

class IsMDLScatteringFunction a => IsMDLPhysicallyPlausibleScatteringFunction a where
  toMDLPhysicallyPlausibleScatteringFunction :: a -> Id MDLPhysicallyPlausibleScatteringFunction

instance IsMDLPhysicallyPlausibleScatteringFunction (Id MDLPhysicallyPlausibleScatteringFunction) where
  toMDLPhysicallyPlausibleScatteringFunction = unsafeCastId

instance IsMDLScatteringFunction (Id MDLPhysicallyPlausibleScatteringFunction) where
  toMDLScatteringFunction = unsafeCastId

instance IsNSObject (Id MDLPhysicallyPlausibleScatteringFunction) where
  toNSObject = unsafeCastId

-- ---------- MDLCheckerboardTexture ----------

-- | MDLCheckerboardTexture A two color checkboard with a certain number of divisions
--
-- the texture will be created if data is referenced, otherwise, this             object is merely a description
-- 
-- Phantom type for @MDLCheckerboardTexture@.
data MDLCheckerboardTexture

instance IsObjCObject (Id MDLCheckerboardTexture) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLCheckerboardTexture"

class IsMDLTexture a => IsMDLCheckerboardTexture a where
  toMDLCheckerboardTexture :: a -> Id MDLCheckerboardTexture

instance IsMDLCheckerboardTexture (Id MDLCheckerboardTexture) where
  toMDLCheckerboardTexture = unsafeCastId

instance IsMDLTexture (Id MDLCheckerboardTexture) where
  toMDLTexture = unsafeCastId

instance IsNSObject (Id MDLCheckerboardTexture) where
  toNSObject = unsafeCastId

-- ---------- MDLColorSwatchTexture ----------

-- | Phantom type for @MDLColorSwatchTexture@.
data MDLColorSwatchTexture

instance IsObjCObject (Id MDLColorSwatchTexture) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLColorSwatchTexture"

class IsMDLTexture a => IsMDLColorSwatchTexture a where
  toMDLColorSwatchTexture :: a -> Id MDLColorSwatchTexture

instance IsMDLColorSwatchTexture (Id MDLColorSwatchTexture) where
  toMDLColorSwatchTexture = unsafeCastId

instance IsMDLTexture (Id MDLColorSwatchTexture) where
  toMDLTexture = unsafeCastId

instance IsNSObject (Id MDLColorSwatchTexture) where
  toNSObject = unsafeCastId

-- ---------- MDLNoiseTexture ----------

-- | MDLNoiseTexture  a noise texture containing vector or scalar noise
--
-- the texture will be created if data is referenced, otherwise, this object is merely a description
-- 
-- Phantom type for @MDLNoiseTexture@.
data MDLNoiseTexture

instance IsObjCObject (Id MDLNoiseTexture) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLNoiseTexture"

class IsMDLTexture a => IsMDLNoiseTexture a where
  toMDLNoiseTexture :: a -> Id MDLNoiseTexture

instance IsMDLNoiseTexture (Id MDLNoiseTexture) where
  toMDLNoiseTexture = unsafeCastId

instance IsMDLTexture (Id MDLNoiseTexture) where
  toMDLTexture = unsafeCastId

instance IsNSObject (Id MDLNoiseTexture) where
  toNSObject = unsafeCastId

-- ---------- MDLNormalMapTexture ----------

-- | Phantom type for @MDLNormalMapTexture@.
data MDLNormalMapTexture

instance IsObjCObject (Id MDLNormalMapTexture) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLNormalMapTexture"

class IsMDLTexture a => IsMDLNormalMapTexture a where
  toMDLNormalMapTexture :: a -> Id MDLNormalMapTexture

instance IsMDLNormalMapTexture (Id MDLNormalMapTexture) where
  toMDLNormalMapTexture = unsafeCastId

instance IsMDLTexture (Id MDLNormalMapTexture) where
  toMDLTexture = unsafeCastId

instance IsNSObject (Id MDLNormalMapTexture) where
  toNSObject = unsafeCastId

-- ---------- MDLSkyCubeTexture ----------

-- | MDLSkyCubeTexture  A physically realistic sky as a cube texture
--
-- sunElevation A value of zero is at the zenith, 0.5 is at the horizon,
--
-- 1.0 is at the nadir. Use in conjunction with turbidity to give a dawn,            dusk, or noon look.
--
-- turbidity A value of zero simulates the effect of a clear sky, the sun
--
-- will impart very little color to the sky. A value of one simulates a           great deal of dust and moisture in the sky, and will cause the sun's           color to spread across the atmosphere.
--
-- upperAtmosphereScattering A value of zero will give very dusky colors,
--
-- a value of one will give noon-ish saturated colors.
--
-- groundAlbedo controls the amount of light that bounces back up into
--
-- the sky from the ground. A value of zero will yield a clear sky, a           value of one will reduce the contrast of the sky, making it a bit foggy.
--
-- horizonElevation If the lower half of the environment is being replaced
--
-- by a color, horizonElevation is angle, in radians, below which the           replacement should occur. Negative values are below the horizon.
--
-- groundColor If this value is set, the environment will be replaced with
--
-- the color below the horizonElevation value blended with the w factor up to           Pi/2.0 past the horizon.           (e.g. w = 0.0 groundColor is applied immediatly on the horizon with no blend                 w = Pi/2 groundColor is linearly applied all the way to the south pole)           NOTE: To maintain default behavior a simple length(groundColor) != 0 is used to determine                 if we want to set the ground color (e.g. black and blended immediatly                 on the horizon use (0.0, 0.0, 0.0, 0.0000001))           4 component treats the first 3 components as color and w as blend factor           3 component treats the first 3 components as color and 0 as blend factor           2 component treats the first component as greyscale color and y as blend factor           1 component treats the scalar component as greyscale color and 0 as blend factor
--
-- gamma Modifies the amount of gamma correction applied during
--
-- tone mapping.
--
-- exposure Modifies the exposure applied during tone mapping.
--
-- brighness Modifies the brightness of the image during tone mapping.
--
-- contrast Modifies the contrast of the image during tone mapping.
--
-- saturation Modifes the saturation of the image during tone mapping.
--
-- highDynamicRangeCompression values below the x component of this value
--
-- are not compressed during tone mapping. Values between the x component           and y component are compressed to the maximum brightness value during           tone mapping. Values above the limit are clamped.
--
-- the texture will be created if data is referenced, otherwise, this object is merely a description. All parameters have legal values between zero and one.
-- 
-- Phantom type for @MDLSkyCubeTexture@.
data MDLSkyCubeTexture

instance IsObjCObject (Id MDLSkyCubeTexture) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLSkyCubeTexture"

class IsMDLTexture a => IsMDLSkyCubeTexture a where
  toMDLSkyCubeTexture :: a -> Id MDLSkyCubeTexture

instance IsMDLSkyCubeTexture (Id MDLSkyCubeTexture) where
  toMDLSkyCubeTexture = unsafeCastId

instance IsMDLTexture (Id MDLSkyCubeTexture) where
  toMDLTexture = unsafeCastId

instance IsNSObject (Id MDLSkyCubeTexture) where
  toNSObject = unsafeCastId

-- ---------- MDLURLTexture ----------

-- | MDLURLTexture  a texture provider initialized with a URL or file path.
--
-- if any of the properties of the texture, such as data, are referenced,             then the texture may be loaded, otherwise, the MDLURLTexture is merely             a lightweight reference to something that could be loaded
-- 
-- Phantom type for @MDLURLTexture@.
data MDLURLTexture

instance IsObjCObject (Id MDLURLTexture) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLURLTexture"

class IsMDLTexture a => IsMDLURLTexture a where
  toMDLURLTexture :: a -> Id MDLURLTexture

instance IsMDLURLTexture (Id MDLURLTexture) where
  toMDLURLTexture = unsafeCastId

instance IsMDLTexture (Id MDLURLTexture) where
  toMDLTexture = unsafeCastId

instance IsNSObject (Id MDLURLTexture) where
  toNSObject = unsafeCastId

-- ---------- MDLStereoscopicCamera ----------

-- | Phantom type for @MDLStereoscopicCamera@.
data MDLStereoscopicCamera

instance IsObjCObject (Id MDLStereoscopicCamera) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLStereoscopicCamera"

class IsMDLCamera a => IsMDLStereoscopicCamera a where
  toMDLStereoscopicCamera :: a -> Id MDLStereoscopicCamera

instance IsMDLStereoscopicCamera (Id MDLStereoscopicCamera) where
  toMDLStereoscopicCamera = unsafeCastId

instance IsMDLCamera (Id MDLStereoscopicCamera) where
  toMDLCamera = unsafeCastId

instance IsMDLObject (Id MDLStereoscopicCamera) where
  toMDLObject = unsafeCastId

instance IsNSObject (Id MDLStereoscopicCamera) where
  toNSObject = unsafeCastId

-- ---------- MDLLightProbe ----------

-- | Phantom type for @MDLLightProbe@.
data MDLLightProbe

instance IsObjCObject (Id MDLLightProbe) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLLightProbe"

class IsMDLLight a => IsMDLLightProbe a where
  toMDLLightProbe :: a -> Id MDLLightProbe

instance IsMDLLightProbe (Id MDLLightProbe) where
  toMDLLightProbe = unsafeCastId

instance IsMDLLight (Id MDLLightProbe) where
  toMDLLight = unsafeCastId

instance IsMDLObject (Id MDLLightProbe) where
  toMDLObject = unsafeCastId

instance IsNSObject (Id MDLLightProbe) where
  toNSObject = unsafeCastId

-- ---------- MDLPhysicallyPlausibleLight ----------

-- | MDLPhysicallyPlausibleLight
--
-- A light with characteristics representing plausible real world lights
--
-- color The color of the light.
--
-- lumens The brightness of the light.
--
-- innerConeAngle Within this cone, light is at maximum brightness. Units are degrees.
--
-- outerConeAngle Between the inner cone angle and the outer, light
--
-- quadratically attenuates to zero.
--
-- attenuationStartDistance. Within the attenuation start distance, the
--
-- light is maximally bright.
--
-- attenuationEndDistance. Beyond this distance, there is no light.
-- 
-- Phantom type for @MDLPhysicallyPlausibleLight@.
data MDLPhysicallyPlausibleLight

instance IsObjCObject (Id MDLPhysicallyPlausibleLight) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLPhysicallyPlausibleLight"

class IsMDLLight a => IsMDLPhysicallyPlausibleLight a where
  toMDLPhysicallyPlausibleLight :: a -> Id MDLPhysicallyPlausibleLight

instance IsMDLPhysicallyPlausibleLight (Id MDLPhysicallyPlausibleLight) where
  toMDLPhysicallyPlausibleLight = unsafeCastId

instance IsMDLLight (Id MDLPhysicallyPlausibleLight) where
  toMDLLight = unsafeCastId

instance IsMDLObject (Id MDLPhysicallyPlausibleLight) where
  toMDLObject = unsafeCastId

instance IsNSObject (Id MDLPhysicallyPlausibleLight) where
  toNSObject = unsafeCastId

-- ---------- MDLAreaLight ----------

-- | Phantom type for @MDLAreaLight@.
data MDLAreaLight

instance IsObjCObject (Id MDLAreaLight) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLAreaLight"

class IsMDLPhysicallyPlausibleLight a => IsMDLAreaLight a where
  toMDLAreaLight :: a -> Id MDLAreaLight

instance IsMDLAreaLight (Id MDLAreaLight) where
  toMDLAreaLight = unsafeCastId

instance IsMDLLight (Id MDLAreaLight) where
  toMDLLight = unsafeCastId

instance IsMDLObject (Id MDLAreaLight) where
  toMDLObject = unsafeCastId

instance IsMDLPhysicallyPlausibleLight (Id MDLAreaLight) where
  toMDLPhysicallyPlausibleLight = unsafeCastId

instance IsNSObject (Id MDLAreaLight) where
  toNSObject = unsafeCastId

-- ---------- MDLPhotometricLight ----------

-- | MDLPhotometricLight
--
-- A light created from measurements at various angles.
--
-- lightCubeMap A cube map that can be sampled at various directions to
--
-- learn the intensity of the light in that direction.
--
-- sphericalHarmonicsLevel The value generateSphericalHarmonicsFromLight:
--
-- used to calculate the spherical harmonics coefficients
--
-- sphericalHarmonicsCoefficients The spherical harmonic coefficiencts
--
-- calculated by generateSphericalHarmonicsFromLight:
-- 
-- Phantom type for @MDLPhotometricLight@.
data MDLPhotometricLight

instance IsObjCObject (Id MDLPhotometricLight) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MDLPhotometricLight"

class IsMDLPhysicallyPlausibleLight a => IsMDLPhotometricLight a where
  toMDLPhotometricLight :: a -> Id MDLPhotometricLight

instance IsMDLPhotometricLight (Id MDLPhotometricLight) where
  toMDLPhotometricLight = unsafeCastId

instance IsMDLLight (Id MDLPhotometricLight) where
  toMDLLight = unsafeCastId

instance IsMDLObject (Id MDLPhotometricLight) where
  toMDLObject = unsafeCastId

instance IsMDLPhysicallyPlausibleLight (Id MDLPhotometricLight) where
  toMDLPhysicallyPlausibleLight = unsafeCastId

instance IsNSObject (Id MDLPhotometricLight) where
  toNSObject = unsafeCastId
