{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNGeometrySource
--
-- A geometry source contains geometry data for a specific semantic. The data format is described by properties.
--
-- Generated bindings for @SCNGeometrySource@.
module ObjC.SceneKit.SCNGeometrySource
  ( SCNGeometrySource
  , IsSCNGeometrySource(..)
  , geometrySourceWithData_semantic_vectorCount_floatComponents_componentsPerVector_bytesPerComponent_dataOffset_dataStride
  , geometrySourceWithVertices_count
  , geometrySourceWithNormals_count
  , geometrySourceWithTextureCoordinates_count
  , geometrySourceWithBuffer_vertexFormat_semantic_vertexCount_dataOffset_dataStride
  , data_
  , semantic
  , vectorCount
  , floatComponents
  , componentsPerVector
  , bytesPerComponent
  , dataOffset
  , dataStride
  , bytesPerComponentSelector
  , componentsPerVectorSelector
  , dataOffsetSelector
  , dataSelector
  , dataStrideSelector
  , floatComponentsSelector
  , geometrySourceWithBuffer_vertexFormat_semantic_vertexCount_dataOffset_dataStrideSelector
  , geometrySourceWithData_semantic_vectorCount_floatComponents_componentsPerVector_bytesPerComponent_dataOffset_dataStrideSelector
  , geometrySourceWithNormals_countSelector
  , geometrySourceWithTextureCoordinates_countSelector
  , geometrySourceWithVertices_countSelector
  , semanticSelector
  , vectorCountSelector

  -- * Enum types
  , MTLVertexFormat(MTLVertexFormat)
  , pattern MTLVertexFormatInvalid
  , pattern MTLVertexFormatUChar2
  , pattern MTLVertexFormatUChar3
  , pattern MTLVertexFormatUChar4
  , pattern MTLVertexFormatChar2
  , pattern MTLVertexFormatChar3
  , pattern MTLVertexFormatChar4
  , pattern MTLVertexFormatUChar2Normalized
  , pattern MTLVertexFormatUChar3Normalized
  , pattern MTLVertexFormatUChar4Normalized
  , pattern MTLVertexFormatChar2Normalized
  , pattern MTLVertexFormatChar3Normalized
  , pattern MTLVertexFormatChar4Normalized
  , pattern MTLVertexFormatUShort2
  , pattern MTLVertexFormatUShort3
  , pattern MTLVertexFormatUShort4
  , pattern MTLVertexFormatShort2
  , pattern MTLVertexFormatShort3
  , pattern MTLVertexFormatShort4
  , pattern MTLVertexFormatUShort2Normalized
  , pattern MTLVertexFormatUShort3Normalized
  , pattern MTLVertexFormatUShort4Normalized
  , pattern MTLVertexFormatShort2Normalized
  , pattern MTLVertexFormatShort3Normalized
  , pattern MTLVertexFormatShort4Normalized
  , pattern MTLVertexFormatHalf2
  , pattern MTLVertexFormatHalf3
  , pattern MTLVertexFormatHalf4
  , pattern MTLVertexFormatFloat
  , pattern MTLVertexFormatFloat2
  , pattern MTLVertexFormatFloat3
  , pattern MTLVertexFormatFloat4
  , pattern MTLVertexFormatInt
  , pattern MTLVertexFormatInt2
  , pattern MTLVertexFormatInt3
  , pattern MTLVertexFormatInt4
  , pattern MTLVertexFormatUInt
  , pattern MTLVertexFormatUInt2
  , pattern MTLVertexFormatUInt3
  , pattern MTLVertexFormatUInt4
  , pattern MTLVertexFormatInt1010102Normalized
  , pattern MTLVertexFormatUInt1010102Normalized
  , pattern MTLVertexFormatUChar4Normalized_BGRA
  , pattern MTLVertexFormatUChar
  , pattern MTLVertexFormatChar
  , pattern MTLVertexFormatUCharNormalized
  , pattern MTLVertexFormatCharNormalized
  , pattern MTLVertexFormatUShort
  , pattern MTLVertexFormatShort
  , pattern MTLVertexFormatUShortNormalized
  , pattern MTLVertexFormatShortNormalized
  , pattern MTLVertexFormatHalf
  , pattern MTLVertexFormatFloatRG11B10
  , pattern MTLVertexFormatFloatRGB9E5

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | geometrySourceWithData:semantic:vectorCount:floatComponents:componentsPerVector:bytesPerComponent:dataOffset:dataStride:
--
-- Creates and returns a geometry source from the given data and parameters.
--
-- @data@ — The geometry data.
--
-- @semantic@ — The semantic of the geometry source.
--
-- @vectorCount@ — The number of geometry source vectors.
--
-- @floatComponents@ — A flag that indicates if vector components are floating point values.
--
-- @componentsPerVector@ — The number of scalar components in a vector.
--
-- @bytesPerComponent@ — The number of bytes that represent a vector component.
--
-- @offset@ — The offset from the beginning of the data. In bytes.
--
-- @stride@ — The number of bytes from a vector to the next one in the data.
--
-- ObjC selector: @+ geometrySourceWithData:semantic:vectorCount:floatComponents:componentsPerVector:bytesPerComponent:dataOffset:dataStride:@
geometrySourceWithData_semantic_vectorCount_floatComponents_componentsPerVector_bytesPerComponent_dataOffset_dataStride :: (IsNSData data_, IsNSString semantic) => data_ -> semantic -> CLong -> Bool -> CLong -> CLong -> CLong -> CLong -> IO (Id SCNGeometrySource)
geometrySourceWithData_semantic_vectorCount_floatComponents_componentsPerVector_bytesPerComponent_dataOffset_dataStride data_ semantic vectorCount floatComponents componentsPerVector bytesPerComponent offset stride =
  do
    cls' <- getRequiredClass "SCNGeometrySource"
    sendClassMessage cls' geometrySourceWithData_semantic_vectorCount_floatComponents_componentsPerVector_bytesPerComponent_dataOffset_dataStrideSelector (toNSData data_) (toNSString semantic) vectorCount floatComponents componentsPerVector bytesPerComponent offset stride

-- | geometrySourceWithVertices:count:
--
-- Creates and returns a geometry source from vertices stored in a buffer of SCNVector3 values.
--
-- @vertices@ — The buffer of vertices.
--
-- @count@ — The number of vertices.
--
-- Input vertices are copied to an optimized data format. The actual format is described by the properties of the resulting instance.
--
-- ObjC selector: @+ geometrySourceWithVertices:count:@
geometrySourceWithVertices_count :: Const (Ptr SCNVector3) -> CLong -> IO (Id SCNGeometrySource)
geometrySourceWithVertices_count vertices count =
  do
    cls' <- getRequiredClass "SCNGeometrySource"
    sendClassMessage cls' geometrySourceWithVertices_countSelector vertices count

-- | geometrySourceWithNormals:count:
--
-- Creates and returns a geometry source from normals stored in a buffer of SCNVector3 values.
--
-- @normals@ — The buffer of normals.
--
-- @count@ — The number of normals.
--
-- Input normals are copied to an optimized data format. The actual format is described by the properties of the resulting instance.
--
-- ObjC selector: @+ geometrySourceWithNormals:count:@
geometrySourceWithNormals_count :: Const (Ptr SCNVector3) -> CLong -> IO (Id SCNGeometrySource)
geometrySourceWithNormals_count normals count =
  do
    cls' <- getRequiredClass "SCNGeometrySource"
    sendClassMessage cls' geometrySourceWithNormals_countSelector normals count

-- | geometrySourceWithTextureCoordinates:count:
--
-- Creates and returns a geometry source from texture coordinates stored in a buffer of CGPoint values.
--
-- @texcoord@ — The buffer of texture coordinates.
--
-- @count@ — The number of texture coordinate points.
--
-- Input texture coordinates are copied to an optimized data format. The actual format is described by the properties of the resulting instance.
--
-- ObjC selector: @+ geometrySourceWithTextureCoordinates:count:@
geometrySourceWithTextureCoordinates_count :: Const RawId -> CLong -> IO (Id SCNGeometrySource)
geometrySourceWithTextureCoordinates_count texcoord count =
  do
    cls' <- getRequiredClass "SCNGeometrySource"
    sendClassMessage cls' geometrySourceWithTextureCoordinates_countSelector texcoord count

-- | geometrySourceWithBuffer:semantic:vectorCount:floatComponents:componentsPerVector:bytesPerComponent:dataOffset:dataStride:
--
-- Creates and returns a geometry source from the given data and parameters.
--
-- @buffer@ — A Metal buffer.
--
-- @vertexFormat@ — The vertex format.
--
-- @semantic@ — The semantic of the geometry source.
--
-- @vertexCount@ — The number of vertex.
--
-- @offset@ — The offset from the beginning of the data. In bytes.
--
-- @stride@ — The number of bytes from a vector to the next one in the data.
--
-- Attempting to modify the Metal buffer outside the SCNSceneRenderer delegate callbacks is undefined. The typical usage it to modify the MTLBuffer within the willRenderScene callback, using a compute kernel or a vertex function in the user own command buffer. So something like:
--
-- - (void)renderer:(id <SCNSceneRenderer>)aRenderer willRenderScene:(SCNScene *)scene atTime:(NSTimeInterval)time {     // ask for a new command buffer     id <MTLCommandBuffer> myCommandBuffer = [aRenderer.commandQueue commandBuffer];
--
-- // get a compute command encoder     id <MTLComputeCommandEncoder> myComputeCommandEncoder = [myCommandBuffer computeCommandEncoder];
--
-- // configure the compute command encoder's pipeline state, buffer inputs etc...     //...
--
-- // dispatch the     [myComputeCommandEncoder dispatchThreadgroups:numberOfWorkingGroups threadsPerThreadgroup:numberOfThreads];     [myComputeCommandEncoder endEncoding];
--
-- [myCommandBuffer commit]; }
--
-- ObjC selector: @+ geometrySourceWithBuffer:vertexFormat:semantic:vertexCount:dataOffset:dataStride:@
geometrySourceWithBuffer_vertexFormat_semantic_vertexCount_dataOffset_dataStride :: IsNSString semantic => RawId -> MTLVertexFormat -> semantic -> CLong -> CLong -> CLong -> IO (Id SCNGeometrySource)
geometrySourceWithBuffer_vertexFormat_semantic_vertexCount_dataOffset_dataStride buffer vertexFormat semantic vertexCount offset stride =
  do
    cls' <- getRequiredClass "SCNGeometrySource"
    sendClassMessage cls' geometrySourceWithBuffer_vertexFormat_semantic_vertexCount_dataOffset_dataStrideSelector buffer vertexFormat (toNSString semantic) vertexCount offset stride

-- | data
--
-- The data for the geometry source
--
-- ObjC selector: @- data@
data_ :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO (Id NSData)
data_ scnGeometrySource =
  sendMessage scnGeometrySource dataSelector

-- | semantic
--
-- The semantic of the geometry source
--
-- ObjC selector: @- semantic@
semantic :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO (Id NSString)
semantic scnGeometrySource =
  sendMessage scnGeometrySource semanticSelector

-- | vectorCount
--
-- The number of vectors in the data.
--
-- ObjC selector: @- vectorCount@
vectorCount :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO CLong
vectorCount scnGeometrySource =
  sendMessage scnGeometrySource vectorCountSelector

-- | floatComponents
--
-- A flag that indicates if vector components are floating point values.
--
-- ObjC selector: @- floatComponents@
floatComponents :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO Bool
floatComponents scnGeometrySource =
  sendMessage scnGeometrySource floatComponentsSelector

-- | componentsPerVector
--
-- The number of scalar components in each vector.
--
-- ObjC selector: @- componentsPerVector@
componentsPerVector :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO CLong
componentsPerVector scnGeometrySource =
  sendMessage scnGeometrySource componentsPerVectorSelector

-- | bytesPerComponent
--
-- The size of a vector component in bytes.
--
-- ObjC selector: @- bytesPerComponent@
bytesPerComponent :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO CLong
bytesPerComponent scnGeometrySource =
  sendMessage scnGeometrySource bytesPerComponentSelector

-- | dataOffset
--
-- The offset from the beginning of the data. In bytes.
--
-- ObjC selector: @- dataOffset@
dataOffset :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO CLong
dataOffset scnGeometrySource =
  sendMessage scnGeometrySource dataOffsetSelector

-- | dataStride
--
-- The number of bytes from a vector to the next one in the data.
--
-- ObjC selector: @- dataStride@
dataStride :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO CLong
dataStride scnGeometrySource =
  sendMessage scnGeometrySource dataStrideSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @geometrySourceWithData:semantic:vectorCount:floatComponents:componentsPerVector:bytesPerComponent:dataOffset:dataStride:@
geometrySourceWithData_semantic_vectorCount_floatComponents_componentsPerVector_bytesPerComponent_dataOffset_dataStrideSelector :: Selector '[Id NSData, Id NSString, CLong, Bool, CLong, CLong, CLong, CLong] (Id SCNGeometrySource)
geometrySourceWithData_semantic_vectorCount_floatComponents_componentsPerVector_bytesPerComponent_dataOffset_dataStrideSelector = mkSelector "geometrySourceWithData:semantic:vectorCount:floatComponents:componentsPerVector:bytesPerComponent:dataOffset:dataStride:"

-- | @Selector@ for @geometrySourceWithVertices:count:@
geometrySourceWithVertices_countSelector :: Selector '[Const (Ptr SCNVector3), CLong] (Id SCNGeometrySource)
geometrySourceWithVertices_countSelector = mkSelector "geometrySourceWithVertices:count:"

-- | @Selector@ for @geometrySourceWithNormals:count:@
geometrySourceWithNormals_countSelector :: Selector '[Const (Ptr SCNVector3), CLong] (Id SCNGeometrySource)
geometrySourceWithNormals_countSelector = mkSelector "geometrySourceWithNormals:count:"

-- | @Selector@ for @geometrySourceWithTextureCoordinates:count:@
geometrySourceWithTextureCoordinates_countSelector :: Selector '[Const RawId, CLong] (Id SCNGeometrySource)
geometrySourceWithTextureCoordinates_countSelector = mkSelector "geometrySourceWithTextureCoordinates:count:"

-- | @Selector@ for @geometrySourceWithBuffer:vertexFormat:semantic:vertexCount:dataOffset:dataStride:@
geometrySourceWithBuffer_vertexFormat_semantic_vertexCount_dataOffset_dataStrideSelector :: Selector '[RawId, MTLVertexFormat, Id NSString, CLong, CLong, CLong] (Id SCNGeometrySource)
geometrySourceWithBuffer_vertexFormat_semantic_vertexCount_dataOffset_dataStrideSelector = mkSelector "geometrySourceWithBuffer:vertexFormat:semantic:vertexCount:dataOffset:dataStride:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @semantic@
semanticSelector :: Selector '[] (Id NSString)
semanticSelector = mkSelector "semantic"

-- | @Selector@ for @vectorCount@
vectorCountSelector :: Selector '[] CLong
vectorCountSelector = mkSelector "vectorCount"

-- | @Selector@ for @floatComponents@
floatComponentsSelector :: Selector '[] Bool
floatComponentsSelector = mkSelector "floatComponents"

-- | @Selector@ for @componentsPerVector@
componentsPerVectorSelector :: Selector '[] CLong
componentsPerVectorSelector = mkSelector "componentsPerVector"

-- | @Selector@ for @bytesPerComponent@
bytesPerComponentSelector :: Selector '[] CLong
bytesPerComponentSelector = mkSelector "bytesPerComponent"

-- | @Selector@ for @dataOffset@
dataOffsetSelector :: Selector '[] CLong
dataOffsetSelector = mkSelector "dataOffset"

-- | @Selector@ for @dataStride@
dataStrideSelector :: Selector '[] CLong
dataStrideSelector = mkSelector "dataStride"

