{-# LANGUAGE PatternSynonyms #-}
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
  , geometrySourceWithBuffer_vertexFormat_semantic_vertexCount_dataOffset_dataStride
  , data_
  , semantic
  , vectorCount
  , floatComponents
  , componentsPerVector
  , bytesPerComponent
  , dataOffset
  , dataStride
  , geometrySourceWithData_semantic_vectorCount_floatComponents_componentsPerVector_bytesPerComponent_dataOffset_dataStrideSelector
  , geometrySourceWithVertices_countSelector
  , geometrySourceWithNormals_countSelector
  , geometrySourceWithBuffer_vertexFormat_semantic_vertexCount_dataOffset_dataStrideSelector
  , dataSelector
  , semanticSelector
  , vectorCountSelector
  , floatComponentsSelector
  , componentsPerVectorSelector
  , bytesPerComponentSelector
  , dataOffsetSelector
  , dataStrideSelector

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
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr semantic $ \raw_semantic ->
        sendClassMsg cls' (mkSelector "geometrySourceWithData:semantic:vectorCount:floatComponents:componentsPerVector:bytesPerComponent:dataOffset:dataStride:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_semantic :: Ptr ()), argCLong (fromIntegral vectorCount), argCULong (if floatComponents then 1 else 0), argCLong (fromIntegral componentsPerVector), argCLong (fromIntegral bytesPerComponent), argCLong (fromIntegral offset), argCLong (fromIntegral stride)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "geometrySourceWithVertices:count:") (retPtr retVoid) [argPtr (unConst vertices), argCLong (fromIntegral count)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "geometrySourceWithNormals:count:") (retPtr retVoid) [argPtr (unConst normals), argCLong (fromIntegral count)] >>= retainedObject . castPtr

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
    withObjCPtr semantic $ \raw_semantic ->
      sendClassMsg cls' (mkSelector "geometrySourceWithBuffer:vertexFormat:semantic:vertexCount:dataOffset:dataStride:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argCULong (coerce vertexFormat), argPtr (castPtr raw_semantic :: Ptr ()), argCLong (fromIntegral vertexCount), argCLong (fromIntegral offset), argCLong (fromIntegral stride)] >>= retainedObject . castPtr

-- | data
--
-- The data for the geometry source
--
-- ObjC selector: @- data@
data_ :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO (Id NSData)
data_ scnGeometrySource  =
  sendMsg scnGeometrySource (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | semantic
--
-- The semantic of the geometry source
--
-- ObjC selector: @- semantic@
semantic :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO (Id NSString)
semantic scnGeometrySource  =
  sendMsg scnGeometrySource (mkSelector "semantic") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | vectorCount
--
-- The number of vectors in the data.
--
-- ObjC selector: @- vectorCount@
vectorCount :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO CLong
vectorCount scnGeometrySource  =
  sendMsg scnGeometrySource (mkSelector "vectorCount") retCLong []

-- | floatComponents
--
-- A flag that indicates if vector components are floating point values.
--
-- ObjC selector: @- floatComponents@
floatComponents :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO Bool
floatComponents scnGeometrySource  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnGeometrySource (mkSelector "floatComponents") retCULong []

-- | componentsPerVector
--
-- The number of scalar components in each vector.
--
-- ObjC selector: @- componentsPerVector@
componentsPerVector :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO CLong
componentsPerVector scnGeometrySource  =
  sendMsg scnGeometrySource (mkSelector "componentsPerVector") retCLong []

-- | bytesPerComponent
--
-- The size of a vector component in bytes.
--
-- ObjC selector: @- bytesPerComponent@
bytesPerComponent :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO CLong
bytesPerComponent scnGeometrySource  =
  sendMsg scnGeometrySource (mkSelector "bytesPerComponent") retCLong []

-- | dataOffset
--
-- The offset from the beginning of the data. In bytes.
--
-- ObjC selector: @- dataOffset@
dataOffset :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO CLong
dataOffset scnGeometrySource  =
  sendMsg scnGeometrySource (mkSelector "dataOffset") retCLong []

-- | dataStride
--
-- The number of bytes from a vector to the next one in the data.
--
-- ObjC selector: @- dataStride@
dataStride :: IsSCNGeometrySource scnGeometrySource => scnGeometrySource -> IO CLong
dataStride scnGeometrySource  =
  sendMsg scnGeometrySource (mkSelector "dataStride") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @geometrySourceWithData:semantic:vectorCount:floatComponents:componentsPerVector:bytesPerComponent:dataOffset:dataStride:@
geometrySourceWithData_semantic_vectorCount_floatComponents_componentsPerVector_bytesPerComponent_dataOffset_dataStrideSelector :: Selector
geometrySourceWithData_semantic_vectorCount_floatComponents_componentsPerVector_bytesPerComponent_dataOffset_dataStrideSelector = mkSelector "geometrySourceWithData:semantic:vectorCount:floatComponents:componentsPerVector:bytesPerComponent:dataOffset:dataStride:"

-- | @Selector@ for @geometrySourceWithVertices:count:@
geometrySourceWithVertices_countSelector :: Selector
geometrySourceWithVertices_countSelector = mkSelector "geometrySourceWithVertices:count:"

-- | @Selector@ for @geometrySourceWithNormals:count:@
geometrySourceWithNormals_countSelector :: Selector
geometrySourceWithNormals_countSelector = mkSelector "geometrySourceWithNormals:count:"

-- | @Selector@ for @geometrySourceWithBuffer:vertexFormat:semantic:vertexCount:dataOffset:dataStride:@
geometrySourceWithBuffer_vertexFormat_semantic_vertexCount_dataOffset_dataStrideSelector :: Selector
geometrySourceWithBuffer_vertexFormat_semantic_vertexCount_dataOffset_dataStrideSelector = mkSelector "geometrySourceWithBuffer:vertexFormat:semantic:vertexCount:dataOffset:dataStride:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @semantic@
semanticSelector :: Selector
semanticSelector = mkSelector "semantic"

-- | @Selector@ for @vectorCount@
vectorCountSelector :: Selector
vectorCountSelector = mkSelector "vectorCount"

-- | @Selector@ for @floatComponents@
floatComponentsSelector :: Selector
floatComponentsSelector = mkSelector "floatComponents"

-- | @Selector@ for @componentsPerVector@
componentsPerVectorSelector :: Selector
componentsPerVectorSelector = mkSelector "componentsPerVector"

-- | @Selector@ for @bytesPerComponent@
bytesPerComponentSelector :: Selector
bytesPerComponentSelector = mkSelector "bytesPerComponent"

-- | @Selector@ for @dataOffset@
dataOffsetSelector :: Selector
dataOffsetSelector = mkSelector "dataOffset"

-- | @Selector@ for @dataStride@
dataStrideSelector :: Selector
dataStrideSelector = mkSelector "dataStride"

