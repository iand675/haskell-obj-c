{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLMesh
--
-- A vertex buffer with info to interpret vertex data
--
-- Includes a collection of submeshs which have indexbuffer and             material information
--
-- Generated bindings for @MDLMesh@.
module ObjC.ModelIO.MDLMesh
  ( MDLMesh
  , IsMDLMesh(..)
  , initWithBufferAllocator
  , initWithVertexBuffer_vertexCount_descriptor_submeshes
  , initWithVertexBuffers_vertexCount_descriptor_submeshes
  , vertexAttributeDataForAttributeNamed
  , vertexAttributeDataForAttributeNamed_asFormat
  , generateAmbientOcclusionTextureWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed_materialPropertyNamed
  , generateAmbientOcclusionVertexColorsWithRaysPerSample_attenuationFactor_objectsToConsider_vertexAttributeNamed
  , generateAmbientOcclusionVertexColorsWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed
  , generateLightMapTextureWithQuality_lightsToConsider_objectsToConsider_vertexAttributeNamed_materialPropertyNamed
  , generateLightMapVertexColorsWithLightsToConsider_objectsToConsider_vertexAttributeNamed
  , initMeshBySubdividingMesh_submeshIndex_subdivisionLevels_allocator
  , newIcosahedronWithRadius_inwardNormals_geometryType_allocator
  , newIcosahedronWithRadius_inwardNormals_allocator
  , newSubdividedMesh_submeshIndex_subdivisionLevels
  , addAttributeWithName_format
  , addAttributeWithName_format_type_data_stride
  , addAttributeWithName_format_type_data_stride_time
  , addNormalsWithAttributeNamed_creaseThreshold
  , addTangentBasisForTextureCoordinateAttributeNamed_tangentAttributeNamed_bitangentAttributeNamed
  , addTangentBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamed
  , addOrthTanBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamed
  , addUnwrappedTextureCoordinatesForAttributeNamed
  , flipTextureCoordinatesInAttributeNamed
  , makeVerticesUnique
  , makeVerticesUniqueAndReturnError
  , replaceAttributeNamed_withData
  , updateAttributeNamed_withData
  , removeAttributeNamed
  , vertexDescriptor
  , setVertexDescriptor
  , vertexCount
  , setVertexCount
  , vertexBuffers
  , setVertexBuffers
  , submeshes
  , setSubmeshes
  , allocator
  , initWithBufferAllocatorSelector
  , initWithVertexBuffer_vertexCount_descriptor_submeshesSelector
  , initWithVertexBuffers_vertexCount_descriptor_submeshesSelector
  , vertexAttributeDataForAttributeNamedSelector
  , vertexAttributeDataForAttributeNamed_asFormatSelector
  , generateAmbientOcclusionTextureWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector
  , generateAmbientOcclusionVertexColorsWithRaysPerSample_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector
  , generateAmbientOcclusionVertexColorsWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector
  , generateLightMapTextureWithQuality_lightsToConsider_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector
  , generateLightMapVertexColorsWithLightsToConsider_objectsToConsider_vertexAttributeNamedSelector
  , initMeshBySubdividingMesh_submeshIndex_subdivisionLevels_allocatorSelector
  , newIcosahedronWithRadius_inwardNormals_geometryType_allocatorSelector
  , newIcosahedronWithRadius_inwardNormals_allocatorSelector
  , newSubdividedMesh_submeshIndex_subdivisionLevelsSelector
  , addAttributeWithName_formatSelector
  , addAttributeWithName_format_type_data_strideSelector
  , addAttributeWithName_format_type_data_stride_timeSelector
  , addNormalsWithAttributeNamed_creaseThresholdSelector
  , addTangentBasisForTextureCoordinateAttributeNamed_tangentAttributeNamed_bitangentAttributeNamedSelector
  , addTangentBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector
  , addOrthTanBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector
  , addUnwrappedTextureCoordinatesForAttributeNamedSelector
  , flipTextureCoordinatesInAttributeNamedSelector
  , makeVerticesUniqueSelector
  , makeVerticesUniqueAndReturnErrorSelector
  , replaceAttributeNamed_withDataSelector
  , updateAttributeNamed_withDataSelector
  , removeAttributeNamedSelector
  , vertexDescriptorSelector
  , setVertexDescriptorSelector
  , vertexCountSelector
  , setVertexCountSelector
  , vertexBuffersSelector
  , setVertexBuffersSelector
  , submeshesSelector
  , setSubmeshesSelector
  , allocatorSelector

  -- * Enum types
  , MDLGeometryType(MDLGeometryType)
  , pattern MDLGeometryTypePoints
  , pattern MDLGeometryTypeLines
  , pattern MDLGeometryTypeTriangles
  , pattern MDLGeometryTypeTriangleStrips
  , pattern MDLGeometryTypeQuads
  , pattern MDLGeometryTypeVariableTopology
  , MDLVertexFormat(MDLVertexFormat)
  , pattern MDLVertexFormatInvalid
  , pattern MDLVertexFormatPackedBit
  , pattern MDLVertexFormatUCharBits
  , pattern MDLVertexFormatCharBits
  , pattern MDLVertexFormatUCharNormalizedBits
  , pattern MDLVertexFormatCharNormalizedBits
  , pattern MDLVertexFormatUShortBits
  , pattern MDLVertexFormatShortBits
  , pattern MDLVertexFormatUShortNormalizedBits
  , pattern MDLVertexFormatShortNormalizedBits
  , pattern MDLVertexFormatUIntBits
  , pattern MDLVertexFormatIntBits
  , pattern MDLVertexFormatHalfBits
  , pattern MDLVertexFormatFloatBits
  , pattern MDLVertexFormatUChar
  , pattern MDLVertexFormatUChar2
  , pattern MDLVertexFormatUChar3
  , pattern MDLVertexFormatUChar4
  , pattern MDLVertexFormatChar
  , pattern MDLVertexFormatChar2
  , pattern MDLVertexFormatChar3
  , pattern MDLVertexFormatChar4
  , pattern MDLVertexFormatUCharNormalized
  , pattern MDLVertexFormatUChar2Normalized
  , pattern MDLVertexFormatUChar3Normalized
  , pattern MDLVertexFormatUChar4Normalized
  , pattern MDLVertexFormatCharNormalized
  , pattern MDLVertexFormatChar2Normalized
  , pattern MDLVertexFormatChar3Normalized
  , pattern MDLVertexFormatChar4Normalized
  , pattern MDLVertexFormatUShort
  , pattern MDLVertexFormatUShort2
  , pattern MDLVertexFormatUShort3
  , pattern MDLVertexFormatUShort4
  , pattern MDLVertexFormatShort
  , pattern MDLVertexFormatShort2
  , pattern MDLVertexFormatShort3
  , pattern MDLVertexFormatShort4
  , pattern MDLVertexFormatUShortNormalized
  , pattern MDLVertexFormatUShort2Normalized
  , pattern MDLVertexFormatUShort3Normalized
  , pattern MDLVertexFormatUShort4Normalized
  , pattern MDLVertexFormatShortNormalized
  , pattern MDLVertexFormatShort2Normalized
  , pattern MDLVertexFormatShort3Normalized
  , pattern MDLVertexFormatShort4Normalized
  , pattern MDLVertexFormatUInt
  , pattern MDLVertexFormatUInt2
  , pattern MDLVertexFormatUInt3
  , pattern MDLVertexFormatUInt4
  , pattern MDLVertexFormatInt
  , pattern MDLVertexFormatInt2
  , pattern MDLVertexFormatInt3
  , pattern MDLVertexFormatInt4
  , pattern MDLVertexFormatHalf
  , pattern MDLVertexFormatHalf2
  , pattern MDLVertexFormatHalf3
  , pattern MDLVertexFormatHalf4
  , pattern MDLVertexFormatFloat
  , pattern MDLVertexFormatFloat2
  , pattern MDLVertexFormatFloat3
  , pattern MDLVertexFormatFloat4
  , pattern MDLVertexFormatInt1010102Normalized
  , pattern MDLVertexFormatUInt1010102Normalized

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

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithAllocator:
--
-- Initialize a mesh with an allocator
--
-- Returns: An empty mesh
--
-- ObjC selector: @- initWithBufferAllocator:@
initWithBufferAllocator :: IsMDLMesh mdlMesh => mdlMesh -> RawId -> IO (Id MDLMesh)
initWithBufferAllocator mdlMesh  bufferAllocator =
    sendMsg mdlMesh (mkSelector "initWithBufferAllocator:") (retPtr retVoid) [argPtr (castPtr (unRawId bufferAllocator) :: Ptr ())] >>= ownedObject . castPtr

-- | initWithVertexBuffer:vertexCount:descriptor:submeshes:
--
-- Initialize object with a vertex buffer and a collection of submeshes
--
-- Returns: Initialized mesh or nil if descriptor's layout array does not describe         a single buffer
--
-- @vertexBuffer@ — MDLMeshBuffer object containing all vertex data for the mesh
--
-- @vertexCount@ — Number of vertices in the vertexBuffer
--
-- @descriptor@ — VertexDescriptor specifying how to interpret vertex data
--
-- @submeshes@ — Array of submeshes with index buffers referencing vertex data        and/or materials to be applied to mesh
--
-- ObjC selector: @- initWithVertexBuffer:vertexCount:descriptor:submeshes:@
initWithVertexBuffer_vertexCount_descriptor_submeshes :: (IsMDLMesh mdlMesh, IsMDLVertexDescriptor descriptor, IsNSArray submeshes) => mdlMesh -> RawId -> CULong -> descriptor -> submeshes -> IO (Id MDLMesh)
initWithVertexBuffer_vertexCount_descriptor_submeshes mdlMesh  vertexBuffer vertexCount descriptor submeshes =
  withObjCPtr descriptor $ \raw_descriptor ->
    withObjCPtr submeshes $ \raw_submeshes ->
        sendMsg mdlMesh (mkSelector "initWithVertexBuffer:vertexCount:descriptor:submeshes:") (retPtr retVoid) [argPtr (castPtr (unRawId vertexBuffer) :: Ptr ()), argCULong vertexCount, argPtr (castPtr raw_descriptor :: Ptr ()), argPtr (castPtr raw_submeshes :: Ptr ())] >>= ownedObject . castPtr

-- | initWithVertexBuffer:vertexCount:descriptor:submeshes:
--
-- Initialize object with an array of vertex buffers (Structure of           Arrays) and a collection of submeshes
--
-- Returns: Initialized mesh or nil if descriptor's layout array is incompatible         with vertexBuffers array
--
-- @vertexCount@ — Number of vertices in vertexBuffers
--
-- @descriptor@ — VertexDescriptor specifying how to interpret vertex data
--
-- @submeshes@ — Array of submeshes with index buffers referencing vertex data        and/or materials to be applied to mesh
--
-- Allows initialization with the layout of the vertexBuffers in a        structure-of-arrays form, in other words, non-interleaved vertex attributes
--
-- ObjC selector: @- initWithVertexBuffers:vertexCount:descriptor:submeshes:@
initWithVertexBuffers_vertexCount_descriptor_submeshes :: (IsMDLMesh mdlMesh, IsNSArray vertexBuffers, IsMDLVertexDescriptor descriptor, IsNSArray submeshes) => mdlMesh -> vertexBuffers -> CULong -> descriptor -> submeshes -> IO (Id MDLMesh)
initWithVertexBuffers_vertexCount_descriptor_submeshes mdlMesh  vertexBuffers vertexCount descriptor submeshes =
  withObjCPtr vertexBuffers $ \raw_vertexBuffers ->
    withObjCPtr descriptor $ \raw_descriptor ->
      withObjCPtr submeshes $ \raw_submeshes ->
          sendMsg mdlMesh (mkSelector "initWithVertexBuffers:vertexCount:descriptor:submeshes:") (retPtr retVoid) [argPtr (castPtr raw_vertexBuffers :: Ptr ()), argCULong vertexCount, argPtr (castPtr raw_descriptor :: Ptr ()), argPtr (castPtr raw_submeshes :: Ptr ())] >>= ownedObject . castPtr

-- | vertexAttributeDataForAttributeNamed:
--
-- convenience selector to get quick access to vertex attribute data
--
-- the vertex buffer will remain mapped until the MDLVertexAttributeData             is freed.
--
-- ObjC selector: @- vertexAttributeDataForAttributeNamed:@
vertexAttributeDataForAttributeNamed :: (IsMDLMesh mdlMesh, IsNSString name) => mdlMesh -> name -> IO (Id MDLVertexAttributeData)
vertexAttributeDataForAttributeNamed mdlMesh  name =
  withObjCPtr name $ \raw_name ->
      sendMsg mdlMesh (mkSelector "vertexAttributeDataForAttributeNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | vertexAttributeDataForAttributeNamed:asFormat
--
-- convenience selector to get quick access to vertex attribute data           reformatted to the requested format if necessary.
--
-- If the desired format has less elements than the source attribute             elements, excess elements will be discarded. If the desired format             has more elements than the source attribute, then the destination             elements will be set to zero.             The vertex buffer will remain mapped until the MDLVertexAttributeData             is freed.
--
-- ObjC selector: @- vertexAttributeDataForAttributeNamed:asFormat:@
vertexAttributeDataForAttributeNamed_asFormat :: (IsMDLMesh mdlMesh, IsNSString name) => mdlMesh -> name -> MDLVertexFormat -> IO (Id MDLVertexAttributeData)
vertexAttributeDataForAttributeNamed_asFormat mdlMesh  name format =
  withObjCPtr name $ \raw_name ->
      sendMsg mdlMesh (mkSelector "vertexAttributeDataForAttributeNamed:asFormat:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce format)] >>= retainedObject . castPtr

-- | generateAmbientOcclusionTextureWithQuality:
--
-- Creates an Ambient Occlusion texture, returns true upon success, false           upon failure
--
-- @bakeQuality@ — Float between 0 and 1 that defines quality of the bake process.        0 is of lower quality but bakes faster and uses less memory, where 1 is        of higher quality.
--
-- @attenuationFactor@ — Float between 0 to 1 that defines how to attenuate the        AO value. 0 doesn't change it, and at 1, all AO values are white except        if they are originally completely black. Quadratic attenuation in between.
--
-- @objectsToConsider@ — NSArray of MDLMeshes containing the objects to raytrace        against
--
-- @vertexAttributeName@ — NSString of the MDLVertexAttribute where the vertex        texture UVs will be stored. Creates it if it doesn't exist, otherwise        overwrites current values.
--
-- @materialPropertyName@ — NSString of the MDLMaterialProperty that will store        the texture in the Mesh.
--
-- Returns: Success or failure of the baking process.
--
-- ObjC selector: @- generateAmbientOcclusionTextureWithQuality:attenuationFactor:objectsToConsider:vertexAttributeNamed:materialPropertyNamed:@
generateAmbientOcclusionTextureWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed_materialPropertyNamed :: (IsMDLMesh mdlMesh, IsNSArray objectsToConsider, IsNSString vertexAttributeName, IsNSString materialPropertyName) => mdlMesh -> CFloat -> CFloat -> objectsToConsider -> vertexAttributeName -> materialPropertyName -> IO Bool
generateAmbientOcclusionTextureWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed_materialPropertyNamed mdlMesh  bakeQuality attenuationFactor objectsToConsider vertexAttributeName materialPropertyName =
  withObjCPtr objectsToConsider $ \raw_objectsToConsider ->
    withObjCPtr vertexAttributeName $ \raw_vertexAttributeName ->
      withObjCPtr materialPropertyName $ \raw_materialPropertyName ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlMesh (mkSelector "generateAmbientOcclusionTextureWithQuality:attenuationFactor:objectsToConsider:vertexAttributeNamed:materialPropertyNamed:") retCULong [argCFloat bakeQuality, argCFloat attenuationFactor, argPtr (castPtr raw_objectsToConsider :: Ptr ()), argPtr (castPtr raw_vertexAttributeName :: Ptr ()), argPtr (castPtr raw_materialPropertyName :: Ptr ())]

-- | @- generateAmbientOcclusionVertexColorsWithRaysPerSample:attenuationFactor:objectsToConsider:vertexAttributeNamed:@
generateAmbientOcclusionVertexColorsWithRaysPerSample_attenuationFactor_objectsToConsider_vertexAttributeNamed :: (IsMDLMesh mdlMesh, IsNSArray objectsToConsider, IsNSString vertexAttributeName) => mdlMesh -> CLong -> CFloat -> objectsToConsider -> vertexAttributeName -> IO Bool
generateAmbientOcclusionVertexColorsWithRaysPerSample_attenuationFactor_objectsToConsider_vertexAttributeNamed mdlMesh  raysPerSample attenuationFactor objectsToConsider vertexAttributeName =
  withObjCPtr objectsToConsider $ \raw_objectsToConsider ->
    withObjCPtr vertexAttributeName $ \raw_vertexAttributeName ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlMesh (mkSelector "generateAmbientOcclusionVertexColorsWithRaysPerSample:attenuationFactor:objectsToConsider:vertexAttributeNamed:") retCULong [argCLong raysPerSample, argCFloat attenuationFactor, argPtr (castPtr raw_objectsToConsider :: Ptr ()), argPtr (castPtr raw_vertexAttributeName :: Ptr ())]

-- | @- generateAmbientOcclusionVertexColorsWithQuality:attenuationFactor:objectsToConsider:vertexAttributeNamed:@
generateAmbientOcclusionVertexColorsWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed :: (IsMDLMesh mdlMesh, IsNSArray objectsToConsider, IsNSString vertexAttributeName) => mdlMesh -> CFloat -> CFloat -> objectsToConsider -> vertexAttributeName -> IO Bool
generateAmbientOcclusionVertexColorsWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed mdlMesh  bakeQuality attenuationFactor objectsToConsider vertexAttributeName =
  withObjCPtr objectsToConsider $ \raw_objectsToConsider ->
    withObjCPtr vertexAttributeName $ \raw_vertexAttributeName ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlMesh (mkSelector "generateAmbientOcclusionVertexColorsWithQuality:attenuationFactor:objectsToConsider:vertexAttributeNamed:") retCULong [argCFloat bakeQuality, argCFloat attenuationFactor, argPtr (castPtr raw_objectsToConsider :: Ptr ()), argPtr (castPtr raw_vertexAttributeName :: Ptr ())]

-- | @- generateLightMapTextureWithQuality:lightsToConsider:objectsToConsider:vertexAttributeNamed:materialPropertyNamed:@
generateLightMapTextureWithQuality_lightsToConsider_objectsToConsider_vertexAttributeNamed_materialPropertyNamed :: (IsMDLMesh mdlMesh, IsNSArray lightsToConsider, IsNSArray objectsToConsider, IsNSString vertexAttributeName, IsNSString materialPropertyName) => mdlMesh -> CFloat -> lightsToConsider -> objectsToConsider -> vertexAttributeName -> materialPropertyName -> IO Bool
generateLightMapTextureWithQuality_lightsToConsider_objectsToConsider_vertexAttributeNamed_materialPropertyNamed mdlMesh  bakeQuality lightsToConsider objectsToConsider vertexAttributeName materialPropertyName =
  withObjCPtr lightsToConsider $ \raw_lightsToConsider ->
    withObjCPtr objectsToConsider $ \raw_objectsToConsider ->
      withObjCPtr vertexAttributeName $ \raw_vertexAttributeName ->
        withObjCPtr materialPropertyName $ \raw_materialPropertyName ->
            fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlMesh (mkSelector "generateLightMapTextureWithQuality:lightsToConsider:objectsToConsider:vertexAttributeNamed:materialPropertyNamed:") retCULong [argCFloat bakeQuality, argPtr (castPtr raw_lightsToConsider :: Ptr ()), argPtr (castPtr raw_objectsToConsider :: Ptr ()), argPtr (castPtr raw_vertexAttributeName :: Ptr ()), argPtr (castPtr raw_materialPropertyName :: Ptr ())]

-- | @- generateLightMapVertexColorsWithLightsToConsider:objectsToConsider:vertexAttributeNamed:@
generateLightMapVertexColorsWithLightsToConsider_objectsToConsider_vertexAttributeNamed :: (IsMDLMesh mdlMesh, IsNSArray lightsToConsider, IsNSArray objectsToConsider, IsNSString vertexAttributeName) => mdlMesh -> lightsToConsider -> objectsToConsider -> vertexAttributeName -> IO Bool
generateLightMapVertexColorsWithLightsToConsider_objectsToConsider_vertexAttributeNamed mdlMesh  lightsToConsider objectsToConsider vertexAttributeName =
  withObjCPtr lightsToConsider $ \raw_lightsToConsider ->
    withObjCPtr objectsToConsider $ \raw_objectsToConsider ->
      withObjCPtr vertexAttributeName $ \raw_vertexAttributeName ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlMesh (mkSelector "generateLightMapVertexColorsWithLightsToConsider:objectsToConsider:vertexAttributeNamed:") retCULong [argPtr (castPtr raw_lightsToConsider :: Ptr ()), argPtr (castPtr raw_objectsToConsider :: Ptr ()), argPtr (castPtr raw_vertexAttributeName :: Ptr ())]

-- | initMeshBySubdividingMesh:submeshIndex:subdivisionLevels:allocator
--
-- Factory method that generates a subdivided mesh from a source mesh
--
-- @mesh@ — Mesh from which to generate a subdivided mesh
--
-- @submeshIndex@ — Index of submesh in Mesh's submesh array from which to        generate a subdivided mesh
--
-- @subdivisionLevels@ — The number of levels to subdivide mesh
--
-- Subdivision levels over four are likely to generate more triangles             than can be reasonably displayed. Index and vertex data will use             the same allocator used for the source mesh. Loading an asset             using the topology preservation flag set to YES will result in the             best subdivision results.
--
-- Returns: Returns a mesh subdivided to index level, unless subdivision is         impossible.
--
-- ObjC selector: @- initMeshBySubdividingMesh:submeshIndex:subdivisionLevels:allocator:@
initMeshBySubdividingMesh_submeshIndex_subdivisionLevels_allocator :: (IsMDLMesh mdlMesh, IsMDLMesh mesh) => mdlMesh -> mesh -> CInt -> CUInt -> RawId -> IO (Id MDLMesh)
initMeshBySubdividingMesh_submeshIndex_subdivisionLevels_allocator mdlMesh  mesh submeshIndex subdivisionLevels allocator =
  withObjCPtr mesh $ \raw_mesh ->
      sendMsg mdlMesh (mkSelector "initMeshBySubdividingMesh:submeshIndex:subdivisionLevels:allocator:") (retPtr retVoid) [argPtr (castPtr raw_mesh :: Ptr ()), argCInt submeshIndex, argCUInt subdivisionLevels, argPtr (castPtr (unRawId allocator) :: Ptr ())] >>= ownedObject . castPtr

-- | @+ newIcosahedronWithRadius:inwardNormals:geometryType:allocator:@
newIcosahedronWithRadius_inwardNormals_geometryType_allocator :: CFloat -> Bool -> MDLGeometryType -> RawId -> IO (Id MDLMesh)
newIcosahedronWithRadius_inwardNormals_geometryType_allocator radius inwardNormals geometryType allocator =
  do
    cls' <- getRequiredClass "MDLMesh"
    sendClassMsg cls' (mkSelector "newIcosahedronWithRadius:inwardNormals:geometryType:allocator:") (retPtr retVoid) [argCFloat radius, argCULong (if inwardNormals then 1 else 0), argCLong (coerce geometryType), argPtr (castPtr (unRawId allocator) :: Ptr ())] >>= ownedObject . castPtr

-- | @+ newIcosahedronWithRadius:inwardNormals:allocator:@
newIcosahedronWithRadius_inwardNormals_allocator :: CFloat -> Bool -> RawId -> IO (Id MDLMesh)
newIcosahedronWithRadius_inwardNormals_allocator radius inwardNormals allocator =
  do
    cls' <- getRequiredClass "MDLMesh"
    sendClassMsg cls' (mkSelector "newIcosahedronWithRadius:inwardNormals:allocator:") (retPtr retVoid) [argCFloat radius, argCULong (if inwardNormals then 1 else 0), argPtr (castPtr (unRawId allocator) :: Ptr ())] >>= ownedObject . castPtr

-- | @+ newSubdividedMesh:submeshIndex:subdivisionLevels:@
newSubdividedMesh_submeshIndex_subdivisionLevels :: IsMDLMesh mesh => mesh -> CULong -> CULong -> IO (Id MDLMesh)
newSubdividedMesh_submeshIndex_subdivisionLevels mesh submeshIndex subdivisionLevels =
  do
    cls' <- getRequiredClass "MDLMesh"
    withObjCPtr mesh $ \raw_mesh ->
      sendClassMsg cls' (mkSelector "newSubdividedMesh:submeshIndex:subdivisionLevels:") (retPtr retVoid) [argPtr (castPtr raw_mesh :: Ptr ()), argCULong submeshIndex, argCULong subdivisionLevels] >>= ownedObject . castPtr

-- | addAttributeWithName:format
--
-- Convenience method to add an attribute
--
-- The mesh's allocator will be used to create storage for the new             attribute.
--
-- ObjC selector: @- addAttributeWithName:format:@
addAttributeWithName_format :: (IsMDLMesh mdlMesh, IsNSString name) => mdlMesh -> name -> MDLVertexFormat -> IO ()
addAttributeWithName_format mdlMesh  name format =
  withObjCPtr name $ \raw_name ->
      sendMsg mdlMesh (mkSelector "addAttributeWithName:format:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce format)]

-- | addAttributeWithName:format:type:data:stride
--
-- Create a new vertex attribute including an associated buffer with           a copy of the supplied data, and update the vertex descriptor accordingly
--
-- @name@ — The name the attribute can be found by
--
-- @format@ — Format of the data, such as MDLVertexFormatFloat3
--
-- @type@ — The usage of the attribute, such as MDLVertexAttributePosition
--
-- @data@ — Object containing the data to be used in the new vertex buffer
--
-- @stride@ — The increment in bytes from the start of one data entry to        the next.
--
-- ObjC selector: @- addAttributeWithName:format:type:data:stride:@
addAttributeWithName_format_type_data_stride :: (IsMDLMesh mdlMesh, IsNSString name, IsNSString type_, IsNSData data_) => mdlMesh -> name -> MDLVertexFormat -> type_ -> data_ -> CLong -> IO ()
addAttributeWithName_format_type_data_stride mdlMesh  name format type_ data_ stride =
  withObjCPtr name $ \raw_name ->
    withObjCPtr type_ $ \raw_type_ ->
      withObjCPtr data_ $ \raw_data_ ->
          sendMsg mdlMesh (mkSelector "addAttributeWithName:format:type:data:stride:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce format), argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argCLong stride]

-- | addAttributeWithName:format:type:data:stride:time
--
-- Create a new vertex attribute including an associated buffer with           a copy of the supplied data, and update the vertex descriptor accordingly
--
-- @name@ — The name the attribute can be found by
--
-- @format@ — Format of the data, such as MDLVertexFormatFloat3
--
-- @type@ — The usage of the attribute, such as MDLVertexAttributePosition
--
-- @data@ — Object containing the data to be used in the new vertex buffer
--
-- @stride@ — The increment in bytes from the start of one data entry to        the next.
--
-- @time@ — The time the attribute is to be invoked at.
--
-- Adding an attribute, such as position data, at multiple times will             result in attributes being created for each of those times.             Attributes corresponding to multiple times can be retrieved from             the vertex descriptor.
--
-- ObjC selector: @- addAttributeWithName:format:type:data:stride:time:@
addAttributeWithName_format_type_data_stride_time :: (IsMDLMesh mdlMesh, IsNSString name, IsNSString type_, IsNSData data_) => mdlMesh -> name -> MDLVertexFormat -> type_ -> data_ -> CLong -> CDouble -> IO ()
addAttributeWithName_format_type_data_stride_time mdlMesh  name format type_ data_ stride time =
  withObjCPtr name $ \raw_name ->
    withObjCPtr type_ $ \raw_type_ ->
      withObjCPtr data_ $ \raw_data_ ->
          sendMsg mdlMesh (mkSelector "addAttributeWithName:format:type:data:stride:time:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce format), argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argCLong stride, argCDouble time]

-- | addNormalsWithAttributeNamed:creaseThreshold:
--
-- Calculate and add vertex normal data
--
-- @attributeName@ — Name is the attribute name of vertex normal attribute.  If nil, vertex normals        will be added with the MDLVertexAttributeNormal name string
--
-- @creaseThreshold@ — Threshold of the dot product between the 2 triangles after which                        their face normal will be smoothed out. Therefore, a threshold of 0 will                        smooth everything and a threshold of 1 won't smooth anything.
--
-- Uses the attribute named MDLVertexAttributePosition to calculate             vertex normals. If the mesh does not have an attribute with             'attributeName', it will be added, otherwise the attribute name will             be overwritten with vertex normal data. 'vertexDescriptor' will be             updated to reflect the new attribute.
--
-- ObjC selector: @- addNormalsWithAttributeNamed:creaseThreshold:@
addNormalsWithAttributeNamed_creaseThreshold :: (IsMDLMesh mdlMesh, IsNSString attributeName) => mdlMesh -> attributeName -> CFloat -> IO ()
addNormalsWithAttributeNamed_creaseThreshold mdlMesh  attributeName creaseThreshold =
  withObjCPtr attributeName $ \raw_attributeName ->
      sendMsg mdlMesh (mkSelector "addNormalsWithAttributeNamed:creaseThreshold:") retVoid [argPtr (castPtr raw_attributeName :: Ptr ()), argCFloat creaseThreshold]

-- | addTangentBasisForTextureCoordinateAttributeNamed:tangentAttributeNamed:bitangentAttributeNamed
--
-- Create a shader basis where the tangent and bitangent span the uv -> object space transform
--
-- @textureCoordinateAttributeName@ — Name of texture coordinates to use in calculations
--
-- @tangentAttributeName@ — Name of vertex tangent attribute.
--
-- @bitangentAttributeName@ — Name of vertex bitangent attribute.
--
-- Uses the attribute named MDLVertexAttributePosition and             textureCoordinateAttributeName to calculate tangent and bitangent             attributes. The mesh's vertexDescriptor will be updated to reflect             the new attributes if necessary. The basis may not be orthogonal; to gaurantee an orthogonal              tangent basis please use addOrthTanBasisForTextureCoordinateAttibuteNamed selector.
--
-- ObjC selector: @- addTangentBasisForTextureCoordinateAttributeNamed:tangentAttributeNamed:bitangentAttributeNamed:@
addTangentBasisForTextureCoordinateAttributeNamed_tangentAttributeNamed_bitangentAttributeNamed :: (IsMDLMesh mdlMesh, IsNSString textureCoordinateAttributeName, IsNSString tangentAttributeName, IsNSString bitangentAttributeName) => mdlMesh -> textureCoordinateAttributeName -> tangentAttributeName -> bitangentAttributeName -> IO ()
addTangentBasisForTextureCoordinateAttributeNamed_tangentAttributeNamed_bitangentAttributeNamed mdlMesh  textureCoordinateAttributeName tangentAttributeName bitangentAttributeName =
  withObjCPtr textureCoordinateAttributeName $ \raw_textureCoordinateAttributeName ->
    withObjCPtr tangentAttributeName $ \raw_tangentAttributeName ->
      withObjCPtr bitangentAttributeName $ \raw_bitangentAttributeName ->
          sendMsg mdlMesh (mkSelector "addTangentBasisForTextureCoordinateAttributeNamed:tangentAttributeNamed:bitangentAttributeNamed:") retVoid [argPtr (castPtr raw_textureCoordinateAttributeName :: Ptr ()), argPtr (castPtr raw_tangentAttributeName :: Ptr ()), argPtr (castPtr raw_bitangentAttributeName :: Ptr ())]

-- | addTangentBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed
--
-- Create tangents which are orthogonal to the normal
--
-- @textureCoordinateAttributeName@ — texture coordinates to use in calculations
--
-- @normalAttributeName@ — normals to use in calculations
--
-- @tangentAttributeName@ — Name of a four component vertex tangent attribute.
--
-- Uses the attribute named MDLVertexAttributePosition and             textureCoordinateAttributeName and the specified normals to calculate             tangent information. The mesh's vertexDescriptor will be updated to             reflect the new attribute if necessary.             Note that this method does NOT produce a T.w component which is used in B = (N x T) * T.w             Please use addOrthTanBasisForTextureCoordinateAttributeNamed.
--
-- ObjC selector: @- addTangentBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed:@
addTangentBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamed :: (IsMDLMesh mdlMesh, IsNSString textureCoordinateAttributeName, IsNSString normalAttributeName, IsNSString tangentAttributeName) => mdlMesh -> textureCoordinateAttributeName -> normalAttributeName -> tangentAttributeName -> IO ()
addTangentBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamed mdlMesh  textureCoordinateAttributeName normalAttributeName tangentAttributeName =
  withObjCPtr textureCoordinateAttributeName $ \raw_textureCoordinateAttributeName ->
    withObjCPtr normalAttributeName $ \raw_normalAttributeName ->
      withObjCPtr tangentAttributeName $ \raw_tangentAttributeName ->
          sendMsg mdlMesh (mkSelector "addTangentBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed:") retVoid [argPtr (castPtr raw_textureCoordinateAttributeName :: Ptr ()), argPtr (castPtr raw_normalAttributeName :: Ptr ()), argPtr (castPtr raw_tangentAttributeName :: Ptr ())]

-- | @- addOrthTanBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed:@
addOrthTanBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamed :: (IsMDLMesh mdlMesh, IsNSString textureCoordinateAttributeName, IsNSString normalAttributeName, IsNSString tangentAttributeName) => mdlMesh -> textureCoordinateAttributeName -> normalAttributeName -> tangentAttributeName -> IO ()
addOrthTanBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamed mdlMesh  textureCoordinateAttributeName normalAttributeName tangentAttributeName =
  withObjCPtr textureCoordinateAttributeName $ \raw_textureCoordinateAttributeName ->
    withObjCPtr normalAttributeName $ \raw_normalAttributeName ->
      withObjCPtr tangentAttributeName $ \raw_tangentAttributeName ->
          sendMsg mdlMesh (mkSelector "addOrthTanBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed:") retVoid [argPtr (castPtr raw_textureCoordinateAttributeName :: Ptr ()), argPtr (castPtr raw_normalAttributeName :: Ptr ()), argPtr (castPtr raw_tangentAttributeName :: Ptr ())]

-- | addTextureCoordinatesForAttributeNamed:textureCoordinateAttributeName
--
-- Creates texture coordinates by unwrapping the mesh
--
-- @textureCoordinateAttributeName@ — texture coordinates to modify or create
--
-- Uses the attribute named MDLVertexAttributePosition and if available,             the attribute named MDLVertexAttributeNormal to calculate texture coordinates
--
-- ObjC selector: @- addUnwrappedTextureCoordinatesForAttributeNamed:@
addUnwrappedTextureCoordinatesForAttributeNamed :: (IsMDLMesh mdlMesh, IsNSString textureCoordinateAttributeName) => mdlMesh -> textureCoordinateAttributeName -> IO ()
addUnwrappedTextureCoordinatesForAttributeNamed mdlMesh  textureCoordinateAttributeName =
  withObjCPtr textureCoordinateAttributeName $ \raw_textureCoordinateAttributeName ->
      sendMsg mdlMesh (mkSelector "addUnwrappedTextureCoordinatesForAttributeNamed:") retVoid [argPtr (castPtr raw_textureCoordinateAttributeName :: Ptr ())]

-- | flipTextureCoordinatesInAttributeNamed:
--
-- Flips texture coordinates by performing the operation (u,v) = (u, 1-v)
--
-- @textureCoordinateAttributeName@ — texture coordinates to modify
--
-- Many application generate model files with texture coordinate mapping             assuming a bottom left bitmap origin. It can be more convenient to             have texture coordinates corresponding to an upper left bitmap origin.             This selector will perform the flip operation if the requested texture             coordinate attribute exists on the mesh. An exception will be raised if             the attribute cannot be found
--
-- ObjC selector: @- flipTextureCoordinatesInAttributeNamed:@
flipTextureCoordinatesInAttributeNamed :: (IsMDLMesh mdlMesh, IsNSString textureCoordinateAttributeName) => mdlMesh -> textureCoordinateAttributeName -> IO ()
flipTextureCoordinatesInAttributeNamed mdlMesh  textureCoordinateAttributeName =
  withObjCPtr textureCoordinateAttributeName $ \raw_textureCoordinateAttributeName ->
      sendMsg mdlMesh (mkSelector "flipTextureCoordinatesInAttributeNamed:") retVoid [argPtr (castPtr raw_textureCoordinateAttributeName :: Ptr ())]

-- | makeVerticesUnique:
--
-- Deindexes the vertex array
--
-- If any vertices are shared on multiple faces, duplicate those             vertices so faces do not share vertices. The vertex buffer and index             buffers on submeshes may grow to accomadate any vertices added.
--
-- ObjC selector: @- makeVerticesUnique@
makeVerticesUnique :: IsMDLMesh mdlMesh => mdlMesh -> IO ()
makeVerticesUnique mdlMesh  =
    sendMsg mdlMesh (mkSelector "makeVerticesUnique") retVoid []

-- | makeVerticesUniqueAndReturnError:
--
-- Deindexes the vertex array
--
-- If any vertices are shared on multiple faces, duplicate those vertices so faces do not share vertices. The vertex buffer and index buffers on submeshes may grow to accomadate any vertices added.
--
-- ObjC selector: @- makeVerticesUniqueAndReturnError:@
makeVerticesUniqueAndReturnError :: (IsMDLMesh mdlMesh, IsNSError error_) => mdlMesh -> error_ -> IO Bool
makeVerticesUniqueAndReturnError mdlMesh  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlMesh (mkSelector "makeVerticesUniqueAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | replaceAttributeNamed:withData
--
-- replace existing attribute data with new attribute data retaining the format of the replacement data.
--
-- If the specified attribute does not already exist, it will be created.
--
-- ObjC selector: @- replaceAttributeNamed:withData:@
replaceAttributeNamed_withData :: (IsMDLMesh mdlMesh, IsNSString name, IsMDLVertexAttributeData newData) => mdlMesh -> name -> newData -> IO ()
replaceAttributeNamed_withData mdlMesh  name newData =
  withObjCPtr name $ \raw_name ->
    withObjCPtr newData $ \raw_newData ->
        sendMsg mdlMesh (mkSelector "replaceAttributeNamed:withData:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_newData :: Ptr ())]

-- | updateAttributeNamed:withData
--
-- update existing attribute data with new attribute data retaining the format of the existing data.
--
-- If the specified attribute does not already exist, it will be created with the same format as the newData.
--
-- ObjC selector: @- updateAttributeNamed:withData:@
updateAttributeNamed_withData :: (IsMDLMesh mdlMesh, IsNSString name, IsMDLVertexAttributeData newData) => mdlMesh -> name -> newData -> IO ()
updateAttributeNamed_withData mdlMesh  name newData =
  withObjCPtr name $ \raw_name ->
    withObjCPtr newData $ \raw_newData ->
        sendMsg mdlMesh (mkSelector "updateAttributeNamed:withData:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_newData :: Ptr ())]

-- | removeAttributeNamed:
--
-- remove an attribute
--
-- if the named attribute does not exist, nothing happens.
--
-- ObjC selector: @- removeAttributeNamed:@
removeAttributeNamed :: (IsMDLMesh mdlMesh, IsNSString name) => mdlMesh -> name -> IO ()
removeAttributeNamed mdlMesh  name =
  withObjCPtr name $ \raw_name ->
      sendMsg mdlMesh (mkSelector "removeAttributeNamed:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | vertexDescriptor
--
-- Immutable vertex descriptor for interpreting data in vertexBuffers
--
-- Setting this applies the new layout in 'vertexBuffers' thus is a             heavyweight operation as structured copies of almost all vertex             buffer data could be made.  Additionally, if the new vertexDescriptor             does not have an attribute in the original vertexDescriptor, that             attribute will be deleted.  If the original vertexDescriptor does             not have an attribute in the new vertexDescriptor, the data for the             added attribute set as the added attribute's initializationValue             property.
--
-- The allocator associated with each original meshbuffer is used to             reallocate the corresponding resultant meshbuffer.
--
-- ObjC selector: @- vertexDescriptor@
vertexDescriptor :: IsMDLMesh mdlMesh => mdlMesh -> IO (Id MDLVertexDescriptor)
vertexDescriptor mdlMesh  =
    sendMsg mdlMesh (mkSelector "vertexDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | vertexDescriptor
--
-- Immutable vertex descriptor for interpreting data in vertexBuffers
--
-- Setting this applies the new layout in 'vertexBuffers' thus is a             heavyweight operation as structured copies of almost all vertex             buffer data could be made.  Additionally, if the new vertexDescriptor             does not have an attribute in the original vertexDescriptor, that             attribute will be deleted.  If the original vertexDescriptor does             not have an attribute in the new vertexDescriptor, the data for the             added attribute set as the added attribute's initializationValue             property.
--
-- The allocator associated with each original meshbuffer is used to             reallocate the corresponding resultant meshbuffer.
--
-- ObjC selector: @- setVertexDescriptor:@
setVertexDescriptor :: (IsMDLMesh mdlMesh, IsMDLVertexDescriptor value) => mdlMesh -> value -> IO ()
setVertexDescriptor mdlMesh  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mdlMesh (mkSelector "setVertexDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | vertexCount
--
-- Number of vertices in the vertexBuffers
--
-- The size of vertex data in each buffer can be computed by multiplying             this value with the stride of the buffer in the vertexDescriptor's             layout
--
-- ObjC selector: @- vertexCount@
vertexCount :: IsMDLMesh mdlMesh => mdlMesh -> IO CULong
vertexCount mdlMesh  =
    sendMsg mdlMesh (mkSelector "vertexCount") retCULong []

-- | vertexCount
--
-- Number of vertices in the vertexBuffers
--
-- The size of vertex data in each buffer can be computed by multiplying             this value with the stride of the buffer in the vertexDescriptor's             layout
--
-- ObjC selector: @- setVertexCount:@
setVertexCount :: IsMDLMesh mdlMesh => mdlMesh -> CULong -> IO ()
setVertexCount mdlMesh  value =
    sendMsg mdlMesh (mkSelector "setVertexCount:") retVoid [argCULong value]

-- | vertexBuffers
--
-- Array of buffers containing vertex data
--
-- The vertex buffers in this array are indexed by the vertex descriptor.
--
-- ObjC selector: @- vertexBuffers@
vertexBuffers :: IsMDLMesh mdlMesh => mdlMesh -> IO (Id NSArray)
vertexBuffers mdlMesh  =
    sendMsg mdlMesh (mkSelector "vertexBuffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | vertexBuffers
--
-- Array of buffers containing vertex data
--
-- The vertex buffers in this array are indexed by the vertex descriptor.
--
-- ObjC selector: @- setVertexBuffers:@
setVertexBuffers :: (IsMDLMesh mdlMesh, IsNSArray value) => mdlMesh -> value -> IO ()
setVertexBuffers mdlMesh  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mdlMesh (mkSelector "setVertexBuffers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | submeshes
--
-- Array of submeshes containing an indexbuffer referencing the vertex           data and material to be applied when the mesh is rendered
--
-- ObjC selector: @- submeshes@
submeshes :: IsMDLMesh mdlMesh => mdlMesh -> IO (Id NSMutableArray)
submeshes mdlMesh  =
    sendMsg mdlMesh (mkSelector "submeshes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | submeshes
--
-- Array of submeshes containing an indexbuffer referencing the vertex           data and material to be applied when the mesh is rendered
--
-- ObjC selector: @- setSubmeshes:@
setSubmeshes :: (IsMDLMesh mdlMesh, IsNSMutableArray value) => mdlMesh -> value -> IO ()
setSubmeshes mdlMesh  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mdlMesh (mkSelector "setSubmeshes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | allocator
--
-- allocator used to allocate contained mesh buffers
--
-- ObjC selector: @- allocator@
allocator :: IsMDLMesh mdlMesh => mdlMesh -> IO RawId
allocator mdlMesh  =
    fmap (RawId . castPtr) $ sendMsg mdlMesh (mkSelector "allocator") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBufferAllocator:@
initWithBufferAllocatorSelector :: Selector
initWithBufferAllocatorSelector = mkSelector "initWithBufferAllocator:"

-- | @Selector@ for @initWithVertexBuffer:vertexCount:descriptor:submeshes:@
initWithVertexBuffer_vertexCount_descriptor_submeshesSelector :: Selector
initWithVertexBuffer_vertexCount_descriptor_submeshesSelector = mkSelector "initWithVertexBuffer:vertexCount:descriptor:submeshes:"

-- | @Selector@ for @initWithVertexBuffers:vertexCount:descriptor:submeshes:@
initWithVertexBuffers_vertexCount_descriptor_submeshesSelector :: Selector
initWithVertexBuffers_vertexCount_descriptor_submeshesSelector = mkSelector "initWithVertexBuffers:vertexCount:descriptor:submeshes:"

-- | @Selector@ for @vertexAttributeDataForAttributeNamed:@
vertexAttributeDataForAttributeNamedSelector :: Selector
vertexAttributeDataForAttributeNamedSelector = mkSelector "vertexAttributeDataForAttributeNamed:"

-- | @Selector@ for @vertexAttributeDataForAttributeNamed:asFormat:@
vertexAttributeDataForAttributeNamed_asFormatSelector :: Selector
vertexAttributeDataForAttributeNamed_asFormatSelector = mkSelector "vertexAttributeDataForAttributeNamed:asFormat:"

-- | @Selector@ for @generateAmbientOcclusionTextureWithQuality:attenuationFactor:objectsToConsider:vertexAttributeNamed:materialPropertyNamed:@
generateAmbientOcclusionTextureWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector :: Selector
generateAmbientOcclusionTextureWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector = mkSelector "generateAmbientOcclusionTextureWithQuality:attenuationFactor:objectsToConsider:vertexAttributeNamed:materialPropertyNamed:"

-- | @Selector@ for @generateAmbientOcclusionVertexColorsWithRaysPerSample:attenuationFactor:objectsToConsider:vertexAttributeNamed:@
generateAmbientOcclusionVertexColorsWithRaysPerSample_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector :: Selector
generateAmbientOcclusionVertexColorsWithRaysPerSample_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector = mkSelector "generateAmbientOcclusionVertexColorsWithRaysPerSample:attenuationFactor:objectsToConsider:vertexAttributeNamed:"

-- | @Selector@ for @generateAmbientOcclusionVertexColorsWithQuality:attenuationFactor:objectsToConsider:vertexAttributeNamed:@
generateAmbientOcclusionVertexColorsWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector :: Selector
generateAmbientOcclusionVertexColorsWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector = mkSelector "generateAmbientOcclusionVertexColorsWithQuality:attenuationFactor:objectsToConsider:vertexAttributeNamed:"

-- | @Selector@ for @generateLightMapTextureWithQuality:lightsToConsider:objectsToConsider:vertexAttributeNamed:materialPropertyNamed:@
generateLightMapTextureWithQuality_lightsToConsider_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector :: Selector
generateLightMapTextureWithQuality_lightsToConsider_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector = mkSelector "generateLightMapTextureWithQuality:lightsToConsider:objectsToConsider:vertexAttributeNamed:materialPropertyNamed:"

-- | @Selector@ for @generateLightMapVertexColorsWithLightsToConsider:objectsToConsider:vertexAttributeNamed:@
generateLightMapVertexColorsWithLightsToConsider_objectsToConsider_vertexAttributeNamedSelector :: Selector
generateLightMapVertexColorsWithLightsToConsider_objectsToConsider_vertexAttributeNamedSelector = mkSelector "generateLightMapVertexColorsWithLightsToConsider:objectsToConsider:vertexAttributeNamed:"

-- | @Selector@ for @initMeshBySubdividingMesh:submeshIndex:subdivisionLevels:allocator:@
initMeshBySubdividingMesh_submeshIndex_subdivisionLevels_allocatorSelector :: Selector
initMeshBySubdividingMesh_submeshIndex_subdivisionLevels_allocatorSelector = mkSelector "initMeshBySubdividingMesh:submeshIndex:subdivisionLevels:allocator:"

-- | @Selector@ for @newIcosahedronWithRadius:inwardNormals:geometryType:allocator:@
newIcosahedronWithRadius_inwardNormals_geometryType_allocatorSelector :: Selector
newIcosahedronWithRadius_inwardNormals_geometryType_allocatorSelector = mkSelector "newIcosahedronWithRadius:inwardNormals:geometryType:allocator:"

-- | @Selector@ for @newIcosahedronWithRadius:inwardNormals:allocator:@
newIcosahedronWithRadius_inwardNormals_allocatorSelector :: Selector
newIcosahedronWithRadius_inwardNormals_allocatorSelector = mkSelector "newIcosahedronWithRadius:inwardNormals:allocator:"

-- | @Selector@ for @newSubdividedMesh:submeshIndex:subdivisionLevels:@
newSubdividedMesh_submeshIndex_subdivisionLevelsSelector :: Selector
newSubdividedMesh_submeshIndex_subdivisionLevelsSelector = mkSelector "newSubdividedMesh:submeshIndex:subdivisionLevels:"

-- | @Selector@ for @addAttributeWithName:format:@
addAttributeWithName_formatSelector :: Selector
addAttributeWithName_formatSelector = mkSelector "addAttributeWithName:format:"

-- | @Selector@ for @addAttributeWithName:format:type:data:stride:@
addAttributeWithName_format_type_data_strideSelector :: Selector
addAttributeWithName_format_type_data_strideSelector = mkSelector "addAttributeWithName:format:type:data:stride:"

-- | @Selector@ for @addAttributeWithName:format:type:data:stride:time:@
addAttributeWithName_format_type_data_stride_timeSelector :: Selector
addAttributeWithName_format_type_data_stride_timeSelector = mkSelector "addAttributeWithName:format:type:data:stride:time:"

-- | @Selector@ for @addNormalsWithAttributeNamed:creaseThreshold:@
addNormalsWithAttributeNamed_creaseThresholdSelector :: Selector
addNormalsWithAttributeNamed_creaseThresholdSelector = mkSelector "addNormalsWithAttributeNamed:creaseThreshold:"

-- | @Selector@ for @addTangentBasisForTextureCoordinateAttributeNamed:tangentAttributeNamed:bitangentAttributeNamed:@
addTangentBasisForTextureCoordinateAttributeNamed_tangentAttributeNamed_bitangentAttributeNamedSelector :: Selector
addTangentBasisForTextureCoordinateAttributeNamed_tangentAttributeNamed_bitangentAttributeNamedSelector = mkSelector "addTangentBasisForTextureCoordinateAttributeNamed:tangentAttributeNamed:bitangentAttributeNamed:"

-- | @Selector@ for @addTangentBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed:@
addTangentBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector :: Selector
addTangentBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector = mkSelector "addTangentBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed:"

-- | @Selector@ for @addOrthTanBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed:@
addOrthTanBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector :: Selector
addOrthTanBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector = mkSelector "addOrthTanBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed:"

-- | @Selector@ for @addUnwrappedTextureCoordinatesForAttributeNamed:@
addUnwrappedTextureCoordinatesForAttributeNamedSelector :: Selector
addUnwrappedTextureCoordinatesForAttributeNamedSelector = mkSelector "addUnwrappedTextureCoordinatesForAttributeNamed:"

-- | @Selector@ for @flipTextureCoordinatesInAttributeNamed:@
flipTextureCoordinatesInAttributeNamedSelector :: Selector
flipTextureCoordinatesInAttributeNamedSelector = mkSelector "flipTextureCoordinatesInAttributeNamed:"

-- | @Selector@ for @makeVerticesUnique@
makeVerticesUniqueSelector :: Selector
makeVerticesUniqueSelector = mkSelector "makeVerticesUnique"

-- | @Selector@ for @makeVerticesUniqueAndReturnError:@
makeVerticesUniqueAndReturnErrorSelector :: Selector
makeVerticesUniqueAndReturnErrorSelector = mkSelector "makeVerticesUniqueAndReturnError:"

-- | @Selector@ for @replaceAttributeNamed:withData:@
replaceAttributeNamed_withDataSelector :: Selector
replaceAttributeNamed_withDataSelector = mkSelector "replaceAttributeNamed:withData:"

-- | @Selector@ for @updateAttributeNamed:withData:@
updateAttributeNamed_withDataSelector :: Selector
updateAttributeNamed_withDataSelector = mkSelector "updateAttributeNamed:withData:"

-- | @Selector@ for @removeAttributeNamed:@
removeAttributeNamedSelector :: Selector
removeAttributeNamedSelector = mkSelector "removeAttributeNamed:"

-- | @Selector@ for @vertexDescriptor@
vertexDescriptorSelector :: Selector
vertexDescriptorSelector = mkSelector "vertexDescriptor"

-- | @Selector@ for @setVertexDescriptor:@
setVertexDescriptorSelector :: Selector
setVertexDescriptorSelector = mkSelector "setVertexDescriptor:"

-- | @Selector@ for @vertexCount@
vertexCountSelector :: Selector
vertexCountSelector = mkSelector "vertexCount"

-- | @Selector@ for @setVertexCount:@
setVertexCountSelector :: Selector
setVertexCountSelector = mkSelector "setVertexCount:"

-- | @Selector@ for @vertexBuffers@
vertexBuffersSelector :: Selector
vertexBuffersSelector = mkSelector "vertexBuffers"

-- | @Selector@ for @setVertexBuffers:@
setVertexBuffersSelector :: Selector
setVertexBuffersSelector = mkSelector "setVertexBuffers:"

-- | @Selector@ for @submeshes@
submeshesSelector :: Selector
submeshesSelector = mkSelector "submeshes"

-- | @Selector@ for @setSubmeshes:@
setSubmeshesSelector :: Selector
setSubmeshesSelector = mkSelector "setSubmeshes:"

-- | @Selector@ for @allocator@
allocatorSelector :: Selector
allocatorSelector = mkSelector "allocator"

