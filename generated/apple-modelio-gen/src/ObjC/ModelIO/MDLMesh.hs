{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addAttributeWithName_formatSelector
  , addAttributeWithName_format_type_data_strideSelector
  , addAttributeWithName_format_type_data_stride_timeSelector
  , addNormalsWithAttributeNamed_creaseThresholdSelector
  , addOrthTanBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector
  , addTangentBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector
  , addTangentBasisForTextureCoordinateAttributeNamed_tangentAttributeNamed_bitangentAttributeNamedSelector
  , addUnwrappedTextureCoordinatesForAttributeNamedSelector
  , allocatorSelector
  , flipTextureCoordinatesInAttributeNamedSelector
  , generateAmbientOcclusionTextureWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector
  , generateAmbientOcclusionVertexColorsWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector
  , generateAmbientOcclusionVertexColorsWithRaysPerSample_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector
  , generateLightMapTextureWithQuality_lightsToConsider_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector
  , generateLightMapVertexColorsWithLightsToConsider_objectsToConsider_vertexAttributeNamedSelector
  , initMeshBySubdividingMesh_submeshIndex_subdivisionLevels_allocatorSelector
  , initWithBufferAllocatorSelector
  , initWithVertexBuffer_vertexCount_descriptor_submeshesSelector
  , initWithVertexBuffers_vertexCount_descriptor_submeshesSelector
  , makeVerticesUniqueAndReturnErrorSelector
  , makeVerticesUniqueSelector
  , newIcosahedronWithRadius_inwardNormals_allocatorSelector
  , newIcosahedronWithRadius_inwardNormals_geometryType_allocatorSelector
  , newSubdividedMesh_submeshIndex_subdivisionLevelsSelector
  , removeAttributeNamedSelector
  , replaceAttributeNamed_withDataSelector
  , setSubmeshesSelector
  , setVertexBuffersSelector
  , setVertexCountSelector
  , setVertexDescriptorSelector
  , submeshesSelector
  , updateAttributeNamed_withDataSelector
  , vertexAttributeDataForAttributeNamedSelector
  , vertexAttributeDataForAttributeNamed_asFormatSelector
  , vertexBuffersSelector
  , vertexCountSelector
  , vertexDescriptorSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithBufferAllocator mdlMesh bufferAllocator =
  sendOwnedMessage mdlMesh initWithBufferAllocatorSelector bufferAllocator

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
initWithVertexBuffer_vertexCount_descriptor_submeshes mdlMesh vertexBuffer vertexCount descriptor submeshes =
  sendOwnedMessage mdlMesh initWithVertexBuffer_vertexCount_descriptor_submeshesSelector vertexBuffer vertexCount (toMDLVertexDescriptor descriptor) (toNSArray submeshes)

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
initWithVertexBuffers_vertexCount_descriptor_submeshes mdlMesh vertexBuffers vertexCount descriptor submeshes =
  sendOwnedMessage mdlMesh initWithVertexBuffers_vertexCount_descriptor_submeshesSelector (toNSArray vertexBuffers) vertexCount (toMDLVertexDescriptor descriptor) (toNSArray submeshes)

-- | vertexAttributeDataForAttributeNamed:
--
-- convenience selector to get quick access to vertex attribute data
--
-- the vertex buffer will remain mapped until the MDLVertexAttributeData             is freed.
--
-- ObjC selector: @- vertexAttributeDataForAttributeNamed:@
vertexAttributeDataForAttributeNamed :: (IsMDLMesh mdlMesh, IsNSString name) => mdlMesh -> name -> IO (Id MDLVertexAttributeData)
vertexAttributeDataForAttributeNamed mdlMesh name =
  sendMessage mdlMesh vertexAttributeDataForAttributeNamedSelector (toNSString name)

-- | vertexAttributeDataForAttributeNamed:asFormat
--
-- convenience selector to get quick access to vertex attribute data           reformatted to the requested format if necessary.
--
-- If the desired format has less elements than the source attribute             elements, excess elements will be discarded. If the desired format             has more elements than the source attribute, then the destination             elements will be set to zero.             The vertex buffer will remain mapped until the MDLVertexAttributeData             is freed.
--
-- ObjC selector: @- vertexAttributeDataForAttributeNamed:asFormat:@
vertexAttributeDataForAttributeNamed_asFormat :: (IsMDLMesh mdlMesh, IsNSString name) => mdlMesh -> name -> MDLVertexFormat -> IO (Id MDLVertexAttributeData)
vertexAttributeDataForAttributeNamed_asFormat mdlMesh name format =
  sendMessage mdlMesh vertexAttributeDataForAttributeNamed_asFormatSelector (toNSString name) format

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
generateAmbientOcclusionTextureWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed_materialPropertyNamed mdlMesh bakeQuality attenuationFactor objectsToConsider vertexAttributeName materialPropertyName =
  sendMessage mdlMesh generateAmbientOcclusionTextureWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector bakeQuality attenuationFactor (toNSArray objectsToConsider) (toNSString vertexAttributeName) (toNSString materialPropertyName)

-- | @- generateAmbientOcclusionVertexColorsWithRaysPerSample:attenuationFactor:objectsToConsider:vertexAttributeNamed:@
generateAmbientOcclusionVertexColorsWithRaysPerSample_attenuationFactor_objectsToConsider_vertexAttributeNamed :: (IsMDLMesh mdlMesh, IsNSArray objectsToConsider, IsNSString vertexAttributeName) => mdlMesh -> CLong -> CFloat -> objectsToConsider -> vertexAttributeName -> IO Bool
generateAmbientOcclusionVertexColorsWithRaysPerSample_attenuationFactor_objectsToConsider_vertexAttributeNamed mdlMesh raysPerSample attenuationFactor objectsToConsider vertexAttributeName =
  sendMessage mdlMesh generateAmbientOcclusionVertexColorsWithRaysPerSample_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector raysPerSample attenuationFactor (toNSArray objectsToConsider) (toNSString vertexAttributeName)

-- | @- generateAmbientOcclusionVertexColorsWithQuality:attenuationFactor:objectsToConsider:vertexAttributeNamed:@
generateAmbientOcclusionVertexColorsWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed :: (IsMDLMesh mdlMesh, IsNSArray objectsToConsider, IsNSString vertexAttributeName) => mdlMesh -> CFloat -> CFloat -> objectsToConsider -> vertexAttributeName -> IO Bool
generateAmbientOcclusionVertexColorsWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed mdlMesh bakeQuality attenuationFactor objectsToConsider vertexAttributeName =
  sendMessage mdlMesh generateAmbientOcclusionVertexColorsWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector bakeQuality attenuationFactor (toNSArray objectsToConsider) (toNSString vertexAttributeName)

-- | @- generateLightMapTextureWithQuality:lightsToConsider:objectsToConsider:vertexAttributeNamed:materialPropertyNamed:@
generateLightMapTextureWithQuality_lightsToConsider_objectsToConsider_vertexAttributeNamed_materialPropertyNamed :: (IsMDLMesh mdlMesh, IsNSArray lightsToConsider, IsNSArray objectsToConsider, IsNSString vertexAttributeName, IsNSString materialPropertyName) => mdlMesh -> CFloat -> lightsToConsider -> objectsToConsider -> vertexAttributeName -> materialPropertyName -> IO Bool
generateLightMapTextureWithQuality_lightsToConsider_objectsToConsider_vertexAttributeNamed_materialPropertyNamed mdlMesh bakeQuality lightsToConsider objectsToConsider vertexAttributeName materialPropertyName =
  sendMessage mdlMesh generateLightMapTextureWithQuality_lightsToConsider_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector bakeQuality (toNSArray lightsToConsider) (toNSArray objectsToConsider) (toNSString vertexAttributeName) (toNSString materialPropertyName)

-- | @- generateLightMapVertexColorsWithLightsToConsider:objectsToConsider:vertexAttributeNamed:@
generateLightMapVertexColorsWithLightsToConsider_objectsToConsider_vertexAttributeNamed :: (IsMDLMesh mdlMesh, IsNSArray lightsToConsider, IsNSArray objectsToConsider, IsNSString vertexAttributeName) => mdlMesh -> lightsToConsider -> objectsToConsider -> vertexAttributeName -> IO Bool
generateLightMapVertexColorsWithLightsToConsider_objectsToConsider_vertexAttributeNamed mdlMesh lightsToConsider objectsToConsider vertexAttributeName =
  sendMessage mdlMesh generateLightMapVertexColorsWithLightsToConsider_objectsToConsider_vertexAttributeNamedSelector (toNSArray lightsToConsider) (toNSArray objectsToConsider) (toNSString vertexAttributeName)

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
initMeshBySubdividingMesh_submeshIndex_subdivisionLevels_allocator mdlMesh mesh submeshIndex subdivisionLevels allocator =
  sendOwnedMessage mdlMesh initMeshBySubdividingMesh_submeshIndex_subdivisionLevels_allocatorSelector (toMDLMesh mesh) submeshIndex subdivisionLevels allocator

-- | @+ newIcosahedronWithRadius:inwardNormals:geometryType:allocator:@
newIcosahedronWithRadius_inwardNormals_geometryType_allocator :: CFloat -> Bool -> MDLGeometryType -> RawId -> IO (Id MDLMesh)
newIcosahedronWithRadius_inwardNormals_geometryType_allocator radius inwardNormals geometryType allocator =
  do
    cls' <- getRequiredClass "MDLMesh"
    sendOwnedClassMessage cls' newIcosahedronWithRadius_inwardNormals_geometryType_allocatorSelector radius inwardNormals geometryType allocator

-- | @+ newIcosahedronWithRadius:inwardNormals:allocator:@
newIcosahedronWithRadius_inwardNormals_allocator :: CFloat -> Bool -> RawId -> IO (Id MDLMesh)
newIcosahedronWithRadius_inwardNormals_allocator radius inwardNormals allocator =
  do
    cls' <- getRequiredClass "MDLMesh"
    sendOwnedClassMessage cls' newIcosahedronWithRadius_inwardNormals_allocatorSelector radius inwardNormals allocator

-- | @+ newSubdividedMesh:submeshIndex:subdivisionLevels:@
newSubdividedMesh_submeshIndex_subdivisionLevels :: IsMDLMesh mesh => mesh -> CULong -> CULong -> IO (Id MDLMesh)
newSubdividedMesh_submeshIndex_subdivisionLevels mesh submeshIndex subdivisionLevels =
  do
    cls' <- getRequiredClass "MDLMesh"
    sendOwnedClassMessage cls' newSubdividedMesh_submeshIndex_subdivisionLevelsSelector (toMDLMesh mesh) submeshIndex subdivisionLevels

-- | addAttributeWithName:format
--
-- Convenience method to add an attribute
--
-- The mesh's allocator will be used to create storage for the new             attribute.
--
-- ObjC selector: @- addAttributeWithName:format:@
addAttributeWithName_format :: (IsMDLMesh mdlMesh, IsNSString name) => mdlMesh -> name -> MDLVertexFormat -> IO ()
addAttributeWithName_format mdlMesh name format =
  sendMessage mdlMesh addAttributeWithName_formatSelector (toNSString name) format

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
addAttributeWithName_format_type_data_stride mdlMesh name format type_ data_ stride =
  sendMessage mdlMesh addAttributeWithName_format_type_data_strideSelector (toNSString name) format (toNSString type_) (toNSData data_) stride

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
addAttributeWithName_format_type_data_stride_time mdlMesh name format type_ data_ stride time =
  sendMessage mdlMesh addAttributeWithName_format_type_data_stride_timeSelector (toNSString name) format (toNSString type_) (toNSData data_) stride time

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
addNormalsWithAttributeNamed_creaseThreshold mdlMesh attributeName creaseThreshold =
  sendMessage mdlMesh addNormalsWithAttributeNamed_creaseThresholdSelector (toNSString attributeName) creaseThreshold

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
addTangentBasisForTextureCoordinateAttributeNamed_tangentAttributeNamed_bitangentAttributeNamed mdlMesh textureCoordinateAttributeName tangentAttributeName bitangentAttributeName =
  sendMessage mdlMesh addTangentBasisForTextureCoordinateAttributeNamed_tangentAttributeNamed_bitangentAttributeNamedSelector (toNSString textureCoordinateAttributeName) (toNSString tangentAttributeName) (toNSString bitangentAttributeName)

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
addTangentBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamed mdlMesh textureCoordinateAttributeName normalAttributeName tangentAttributeName =
  sendMessage mdlMesh addTangentBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector (toNSString textureCoordinateAttributeName) (toNSString normalAttributeName) (toNSString tangentAttributeName)

-- | @- addOrthTanBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed:@
addOrthTanBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamed :: (IsMDLMesh mdlMesh, IsNSString textureCoordinateAttributeName, IsNSString normalAttributeName, IsNSString tangentAttributeName) => mdlMesh -> textureCoordinateAttributeName -> normalAttributeName -> tangentAttributeName -> IO ()
addOrthTanBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamed mdlMesh textureCoordinateAttributeName normalAttributeName tangentAttributeName =
  sendMessage mdlMesh addOrthTanBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector (toNSString textureCoordinateAttributeName) (toNSString normalAttributeName) (toNSString tangentAttributeName)

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
addUnwrappedTextureCoordinatesForAttributeNamed mdlMesh textureCoordinateAttributeName =
  sendMessage mdlMesh addUnwrappedTextureCoordinatesForAttributeNamedSelector (toNSString textureCoordinateAttributeName)

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
flipTextureCoordinatesInAttributeNamed mdlMesh textureCoordinateAttributeName =
  sendMessage mdlMesh flipTextureCoordinatesInAttributeNamedSelector (toNSString textureCoordinateAttributeName)

-- | makeVerticesUnique:
--
-- Deindexes the vertex array
--
-- If any vertices are shared on multiple faces, duplicate those             vertices so faces do not share vertices. The vertex buffer and index             buffers on submeshes may grow to accomadate any vertices added.
--
-- ObjC selector: @- makeVerticesUnique@
makeVerticesUnique :: IsMDLMesh mdlMesh => mdlMesh -> IO ()
makeVerticesUnique mdlMesh =
  sendMessage mdlMesh makeVerticesUniqueSelector

-- | makeVerticesUniqueAndReturnError:
--
-- Deindexes the vertex array
--
-- If any vertices are shared on multiple faces, duplicate those vertices so faces do not share vertices. The vertex buffer and index buffers on submeshes may grow to accomadate any vertices added.
--
-- ObjC selector: @- makeVerticesUniqueAndReturnError:@
makeVerticesUniqueAndReturnError :: (IsMDLMesh mdlMesh, IsNSError error_) => mdlMesh -> error_ -> IO Bool
makeVerticesUniqueAndReturnError mdlMesh error_ =
  sendMessage mdlMesh makeVerticesUniqueAndReturnErrorSelector (toNSError error_)

-- | replaceAttributeNamed:withData
--
-- replace existing attribute data with new attribute data retaining the format of the replacement data.
--
-- If the specified attribute does not already exist, it will be created.
--
-- ObjC selector: @- replaceAttributeNamed:withData:@
replaceAttributeNamed_withData :: (IsMDLMesh mdlMesh, IsNSString name, IsMDLVertexAttributeData newData) => mdlMesh -> name -> newData -> IO ()
replaceAttributeNamed_withData mdlMesh name newData =
  sendMessage mdlMesh replaceAttributeNamed_withDataSelector (toNSString name) (toMDLVertexAttributeData newData)

-- | updateAttributeNamed:withData
--
-- update existing attribute data with new attribute data retaining the format of the existing data.
--
-- If the specified attribute does not already exist, it will be created with the same format as the newData.
--
-- ObjC selector: @- updateAttributeNamed:withData:@
updateAttributeNamed_withData :: (IsMDLMesh mdlMesh, IsNSString name, IsMDLVertexAttributeData newData) => mdlMesh -> name -> newData -> IO ()
updateAttributeNamed_withData mdlMesh name newData =
  sendMessage mdlMesh updateAttributeNamed_withDataSelector (toNSString name) (toMDLVertexAttributeData newData)

-- | removeAttributeNamed:
--
-- remove an attribute
--
-- if the named attribute does not exist, nothing happens.
--
-- ObjC selector: @- removeAttributeNamed:@
removeAttributeNamed :: (IsMDLMesh mdlMesh, IsNSString name) => mdlMesh -> name -> IO ()
removeAttributeNamed mdlMesh name =
  sendMessage mdlMesh removeAttributeNamedSelector (toNSString name)

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
vertexDescriptor mdlMesh =
  sendMessage mdlMesh vertexDescriptorSelector

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
setVertexDescriptor mdlMesh value =
  sendMessage mdlMesh setVertexDescriptorSelector (toMDLVertexDescriptor value)

-- | vertexCount
--
-- Number of vertices in the vertexBuffers
--
-- The size of vertex data in each buffer can be computed by multiplying             this value with the stride of the buffer in the vertexDescriptor's             layout
--
-- ObjC selector: @- vertexCount@
vertexCount :: IsMDLMesh mdlMesh => mdlMesh -> IO CULong
vertexCount mdlMesh =
  sendMessage mdlMesh vertexCountSelector

-- | vertexCount
--
-- Number of vertices in the vertexBuffers
--
-- The size of vertex data in each buffer can be computed by multiplying             this value with the stride of the buffer in the vertexDescriptor's             layout
--
-- ObjC selector: @- setVertexCount:@
setVertexCount :: IsMDLMesh mdlMesh => mdlMesh -> CULong -> IO ()
setVertexCount mdlMesh value =
  sendMessage mdlMesh setVertexCountSelector value

-- | vertexBuffers
--
-- Array of buffers containing vertex data
--
-- The vertex buffers in this array are indexed by the vertex descriptor.
--
-- ObjC selector: @- vertexBuffers@
vertexBuffers :: IsMDLMesh mdlMesh => mdlMesh -> IO (Id NSArray)
vertexBuffers mdlMesh =
  sendMessage mdlMesh vertexBuffersSelector

-- | vertexBuffers
--
-- Array of buffers containing vertex data
--
-- The vertex buffers in this array are indexed by the vertex descriptor.
--
-- ObjC selector: @- setVertexBuffers:@
setVertexBuffers :: (IsMDLMesh mdlMesh, IsNSArray value) => mdlMesh -> value -> IO ()
setVertexBuffers mdlMesh value =
  sendMessage mdlMesh setVertexBuffersSelector (toNSArray value)

-- | submeshes
--
-- Array of submeshes containing an indexbuffer referencing the vertex           data and material to be applied when the mesh is rendered
--
-- ObjC selector: @- submeshes@
submeshes :: IsMDLMesh mdlMesh => mdlMesh -> IO (Id NSMutableArray)
submeshes mdlMesh =
  sendMessage mdlMesh submeshesSelector

-- | submeshes
--
-- Array of submeshes containing an indexbuffer referencing the vertex           data and material to be applied when the mesh is rendered
--
-- ObjC selector: @- setSubmeshes:@
setSubmeshes :: (IsMDLMesh mdlMesh, IsNSMutableArray value) => mdlMesh -> value -> IO ()
setSubmeshes mdlMesh value =
  sendMessage mdlMesh setSubmeshesSelector (toNSMutableArray value)

-- | allocator
--
-- allocator used to allocate contained mesh buffers
--
-- ObjC selector: @- allocator@
allocator :: IsMDLMesh mdlMesh => mdlMesh -> IO RawId
allocator mdlMesh =
  sendOwnedMessage mdlMesh allocatorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBufferAllocator:@
initWithBufferAllocatorSelector :: Selector '[RawId] (Id MDLMesh)
initWithBufferAllocatorSelector = mkSelector "initWithBufferAllocator:"

-- | @Selector@ for @initWithVertexBuffer:vertexCount:descriptor:submeshes:@
initWithVertexBuffer_vertexCount_descriptor_submeshesSelector :: Selector '[RawId, CULong, Id MDLVertexDescriptor, Id NSArray] (Id MDLMesh)
initWithVertexBuffer_vertexCount_descriptor_submeshesSelector = mkSelector "initWithVertexBuffer:vertexCount:descriptor:submeshes:"

-- | @Selector@ for @initWithVertexBuffers:vertexCount:descriptor:submeshes:@
initWithVertexBuffers_vertexCount_descriptor_submeshesSelector :: Selector '[Id NSArray, CULong, Id MDLVertexDescriptor, Id NSArray] (Id MDLMesh)
initWithVertexBuffers_vertexCount_descriptor_submeshesSelector = mkSelector "initWithVertexBuffers:vertexCount:descriptor:submeshes:"

-- | @Selector@ for @vertexAttributeDataForAttributeNamed:@
vertexAttributeDataForAttributeNamedSelector :: Selector '[Id NSString] (Id MDLVertexAttributeData)
vertexAttributeDataForAttributeNamedSelector = mkSelector "vertexAttributeDataForAttributeNamed:"

-- | @Selector@ for @vertexAttributeDataForAttributeNamed:asFormat:@
vertexAttributeDataForAttributeNamed_asFormatSelector :: Selector '[Id NSString, MDLVertexFormat] (Id MDLVertexAttributeData)
vertexAttributeDataForAttributeNamed_asFormatSelector = mkSelector "vertexAttributeDataForAttributeNamed:asFormat:"

-- | @Selector@ for @generateAmbientOcclusionTextureWithQuality:attenuationFactor:objectsToConsider:vertexAttributeNamed:materialPropertyNamed:@
generateAmbientOcclusionTextureWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector :: Selector '[CFloat, CFloat, Id NSArray, Id NSString, Id NSString] Bool
generateAmbientOcclusionTextureWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector = mkSelector "generateAmbientOcclusionTextureWithQuality:attenuationFactor:objectsToConsider:vertexAttributeNamed:materialPropertyNamed:"

-- | @Selector@ for @generateAmbientOcclusionVertexColorsWithRaysPerSample:attenuationFactor:objectsToConsider:vertexAttributeNamed:@
generateAmbientOcclusionVertexColorsWithRaysPerSample_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector :: Selector '[CLong, CFloat, Id NSArray, Id NSString] Bool
generateAmbientOcclusionVertexColorsWithRaysPerSample_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector = mkSelector "generateAmbientOcclusionVertexColorsWithRaysPerSample:attenuationFactor:objectsToConsider:vertexAttributeNamed:"

-- | @Selector@ for @generateAmbientOcclusionVertexColorsWithQuality:attenuationFactor:objectsToConsider:vertexAttributeNamed:@
generateAmbientOcclusionVertexColorsWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector :: Selector '[CFloat, CFloat, Id NSArray, Id NSString] Bool
generateAmbientOcclusionVertexColorsWithQuality_attenuationFactor_objectsToConsider_vertexAttributeNamedSelector = mkSelector "generateAmbientOcclusionVertexColorsWithQuality:attenuationFactor:objectsToConsider:vertexAttributeNamed:"

-- | @Selector@ for @generateLightMapTextureWithQuality:lightsToConsider:objectsToConsider:vertexAttributeNamed:materialPropertyNamed:@
generateLightMapTextureWithQuality_lightsToConsider_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector :: Selector '[CFloat, Id NSArray, Id NSArray, Id NSString, Id NSString] Bool
generateLightMapTextureWithQuality_lightsToConsider_objectsToConsider_vertexAttributeNamed_materialPropertyNamedSelector = mkSelector "generateLightMapTextureWithQuality:lightsToConsider:objectsToConsider:vertexAttributeNamed:materialPropertyNamed:"

-- | @Selector@ for @generateLightMapVertexColorsWithLightsToConsider:objectsToConsider:vertexAttributeNamed:@
generateLightMapVertexColorsWithLightsToConsider_objectsToConsider_vertexAttributeNamedSelector :: Selector '[Id NSArray, Id NSArray, Id NSString] Bool
generateLightMapVertexColorsWithLightsToConsider_objectsToConsider_vertexAttributeNamedSelector = mkSelector "generateLightMapVertexColorsWithLightsToConsider:objectsToConsider:vertexAttributeNamed:"

-- | @Selector@ for @initMeshBySubdividingMesh:submeshIndex:subdivisionLevels:allocator:@
initMeshBySubdividingMesh_submeshIndex_subdivisionLevels_allocatorSelector :: Selector '[Id MDLMesh, CInt, CUInt, RawId] (Id MDLMesh)
initMeshBySubdividingMesh_submeshIndex_subdivisionLevels_allocatorSelector = mkSelector "initMeshBySubdividingMesh:submeshIndex:subdivisionLevels:allocator:"

-- | @Selector@ for @newIcosahedronWithRadius:inwardNormals:geometryType:allocator:@
newIcosahedronWithRadius_inwardNormals_geometryType_allocatorSelector :: Selector '[CFloat, Bool, MDLGeometryType, RawId] (Id MDLMesh)
newIcosahedronWithRadius_inwardNormals_geometryType_allocatorSelector = mkSelector "newIcosahedronWithRadius:inwardNormals:geometryType:allocator:"

-- | @Selector@ for @newIcosahedronWithRadius:inwardNormals:allocator:@
newIcosahedronWithRadius_inwardNormals_allocatorSelector :: Selector '[CFloat, Bool, RawId] (Id MDLMesh)
newIcosahedronWithRadius_inwardNormals_allocatorSelector = mkSelector "newIcosahedronWithRadius:inwardNormals:allocator:"

-- | @Selector@ for @newSubdividedMesh:submeshIndex:subdivisionLevels:@
newSubdividedMesh_submeshIndex_subdivisionLevelsSelector :: Selector '[Id MDLMesh, CULong, CULong] (Id MDLMesh)
newSubdividedMesh_submeshIndex_subdivisionLevelsSelector = mkSelector "newSubdividedMesh:submeshIndex:subdivisionLevels:"

-- | @Selector@ for @addAttributeWithName:format:@
addAttributeWithName_formatSelector :: Selector '[Id NSString, MDLVertexFormat] ()
addAttributeWithName_formatSelector = mkSelector "addAttributeWithName:format:"

-- | @Selector@ for @addAttributeWithName:format:type:data:stride:@
addAttributeWithName_format_type_data_strideSelector :: Selector '[Id NSString, MDLVertexFormat, Id NSString, Id NSData, CLong] ()
addAttributeWithName_format_type_data_strideSelector = mkSelector "addAttributeWithName:format:type:data:stride:"

-- | @Selector@ for @addAttributeWithName:format:type:data:stride:time:@
addAttributeWithName_format_type_data_stride_timeSelector :: Selector '[Id NSString, MDLVertexFormat, Id NSString, Id NSData, CLong, CDouble] ()
addAttributeWithName_format_type_data_stride_timeSelector = mkSelector "addAttributeWithName:format:type:data:stride:time:"

-- | @Selector@ for @addNormalsWithAttributeNamed:creaseThreshold:@
addNormalsWithAttributeNamed_creaseThresholdSelector :: Selector '[Id NSString, CFloat] ()
addNormalsWithAttributeNamed_creaseThresholdSelector = mkSelector "addNormalsWithAttributeNamed:creaseThreshold:"

-- | @Selector@ for @addTangentBasisForTextureCoordinateAttributeNamed:tangentAttributeNamed:bitangentAttributeNamed:@
addTangentBasisForTextureCoordinateAttributeNamed_tangentAttributeNamed_bitangentAttributeNamedSelector :: Selector '[Id NSString, Id NSString, Id NSString] ()
addTangentBasisForTextureCoordinateAttributeNamed_tangentAttributeNamed_bitangentAttributeNamedSelector = mkSelector "addTangentBasisForTextureCoordinateAttributeNamed:tangentAttributeNamed:bitangentAttributeNamed:"

-- | @Selector@ for @addTangentBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed:@
addTangentBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector :: Selector '[Id NSString, Id NSString, Id NSString] ()
addTangentBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector = mkSelector "addTangentBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed:"

-- | @Selector@ for @addOrthTanBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed:@
addOrthTanBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector :: Selector '[Id NSString, Id NSString, Id NSString] ()
addOrthTanBasisForTextureCoordinateAttributeNamed_normalAttributeNamed_tangentAttributeNamedSelector = mkSelector "addOrthTanBasisForTextureCoordinateAttributeNamed:normalAttributeNamed:tangentAttributeNamed:"

-- | @Selector@ for @addUnwrappedTextureCoordinatesForAttributeNamed:@
addUnwrappedTextureCoordinatesForAttributeNamedSelector :: Selector '[Id NSString] ()
addUnwrappedTextureCoordinatesForAttributeNamedSelector = mkSelector "addUnwrappedTextureCoordinatesForAttributeNamed:"

-- | @Selector@ for @flipTextureCoordinatesInAttributeNamed:@
flipTextureCoordinatesInAttributeNamedSelector :: Selector '[Id NSString] ()
flipTextureCoordinatesInAttributeNamedSelector = mkSelector "flipTextureCoordinatesInAttributeNamed:"

-- | @Selector@ for @makeVerticesUnique@
makeVerticesUniqueSelector :: Selector '[] ()
makeVerticesUniqueSelector = mkSelector "makeVerticesUnique"

-- | @Selector@ for @makeVerticesUniqueAndReturnError:@
makeVerticesUniqueAndReturnErrorSelector :: Selector '[Id NSError] Bool
makeVerticesUniqueAndReturnErrorSelector = mkSelector "makeVerticesUniqueAndReturnError:"

-- | @Selector@ for @replaceAttributeNamed:withData:@
replaceAttributeNamed_withDataSelector :: Selector '[Id NSString, Id MDLVertexAttributeData] ()
replaceAttributeNamed_withDataSelector = mkSelector "replaceAttributeNamed:withData:"

-- | @Selector@ for @updateAttributeNamed:withData:@
updateAttributeNamed_withDataSelector :: Selector '[Id NSString, Id MDLVertexAttributeData] ()
updateAttributeNamed_withDataSelector = mkSelector "updateAttributeNamed:withData:"

-- | @Selector@ for @removeAttributeNamed:@
removeAttributeNamedSelector :: Selector '[Id NSString] ()
removeAttributeNamedSelector = mkSelector "removeAttributeNamed:"

-- | @Selector@ for @vertexDescriptor@
vertexDescriptorSelector :: Selector '[] (Id MDLVertexDescriptor)
vertexDescriptorSelector = mkSelector "vertexDescriptor"

-- | @Selector@ for @setVertexDescriptor:@
setVertexDescriptorSelector :: Selector '[Id MDLVertexDescriptor] ()
setVertexDescriptorSelector = mkSelector "setVertexDescriptor:"

-- | @Selector@ for @vertexCount@
vertexCountSelector :: Selector '[] CULong
vertexCountSelector = mkSelector "vertexCount"

-- | @Selector@ for @setVertexCount:@
setVertexCountSelector :: Selector '[CULong] ()
setVertexCountSelector = mkSelector "setVertexCount:"

-- | @Selector@ for @vertexBuffers@
vertexBuffersSelector :: Selector '[] (Id NSArray)
vertexBuffersSelector = mkSelector "vertexBuffers"

-- | @Selector@ for @setVertexBuffers:@
setVertexBuffersSelector :: Selector '[Id NSArray] ()
setVertexBuffersSelector = mkSelector "setVertexBuffers:"

-- | @Selector@ for @submeshes@
submeshesSelector :: Selector '[] (Id NSMutableArray)
submeshesSelector = mkSelector "submeshes"

-- | @Selector@ for @setSubmeshes:@
setSubmeshesSelector :: Selector '[Id NSMutableArray] ()
setSubmeshesSelector = mkSelector "setSubmeshes:"

-- | @Selector@ for @allocator@
allocatorSelector :: Selector '[] RawId
allocatorSelector = mkSelector "allocator"

