{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Descriptor for motion triangle geometry
--
-- Generated bindings for @MTLAccelerationStructureMotionTriangleGeometryDescriptor@.
module ObjC.Metal.MTLAccelerationStructureMotionTriangleGeometryDescriptor
  ( MTLAccelerationStructureMotionTriangleGeometryDescriptor
  , IsMTLAccelerationStructureMotionTriangleGeometryDescriptor(..)
  , descriptor
  , vertexBuffers
  , setVertexBuffers
  , vertexFormat
  , setVertexFormat
  , vertexStride
  , setVertexStride
  , indexBuffer
  , setIndexBuffer
  , indexBufferOffset
  , setIndexBufferOffset
  , indexType
  , setIndexType
  , triangleCount
  , setTriangleCount
  , transformationMatrixBuffer
  , setTransformationMatrixBuffer
  , transformationMatrixBufferOffset
  , setTransformationMatrixBufferOffset
  , transformationMatrixLayout
  , setTransformationMatrixLayout
  , descriptorSelector
  , indexBufferOffsetSelector
  , indexBufferSelector
  , indexTypeSelector
  , setIndexBufferOffsetSelector
  , setIndexBufferSelector
  , setIndexTypeSelector
  , setTransformationMatrixBufferOffsetSelector
  , setTransformationMatrixBufferSelector
  , setTransformationMatrixLayoutSelector
  , setTriangleCountSelector
  , setVertexBuffersSelector
  , setVertexFormatSelector
  , setVertexStrideSelector
  , transformationMatrixBufferOffsetSelector
  , transformationMatrixBufferSelector
  , transformationMatrixLayoutSelector
  , triangleCountSelector
  , vertexBuffersSelector
  , vertexFormatSelector
  , vertexStrideSelector

  -- * Enum types
  , MTLAttributeFormat(MTLAttributeFormat)
  , pattern MTLAttributeFormatInvalid
  , pattern MTLAttributeFormatUChar2
  , pattern MTLAttributeFormatUChar3
  , pattern MTLAttributeFormatUChar4
  , pattern MTLAttributeFormatChar2
  , pattern MTLAttributeFormatChar3
  , pattern MTLAttributeFormatChar4
  , pattern MTLAttributeFormatUChar2Normalized
  , pattern MTLAttributeFormatUChar3Normalized
  , pattern MTLAttributeFormatUChar4Normalized
  , pattern MTLAttributeFormatChar2Normalized
  , pattern MTLAttributeFormatChar3Normalized
  , pattern MTLAttributeFormatChar4Normalized
  , pattern MTLAttributeFormatUShort2
  , pattern MTLAttributeFormatUShort3
  , pattern MTLAttributeFormatUShort4
  , pattern MTLAttributeFormatShort2
  , pattern MTLAttributeFormatShort3
  , pattern MTLAttributeFormatShort4
  , pattern MTLAttributeFormatUShort2Normalized
  , pattern MTLAttributeFormatUShort3Normalized
  , pattern MTLAttributeFormatUShort4Normalized
  , pattern MTLAttributeFormatShort2Normalized
  , pattern MTLAttributeFormatShort3Normalized
  , pattern MTLAttributeFormatShort4Normalized
  , pattern MTLAttributeFormatHalf2
  , pattern MTLAttributeFormatHalf3
  , pattern MTLAttributeFormatHalf4
  , pattern MTLAttributeFormatFloat
  , pattern MTLAttributeFormatFloat2
  , pattern MTLAttributeFormatFloat3
  , pattern MTLAttributeFormatFloat4
  , pattern MTLAttributeFormatInt
  , pattern MTLAttributeFormatInt2
  , pattern MTLAttributeFormatInt3
  , pattern MTLAttributeFormatInt4
  , pattern MTLAttributeFormatUInt
  , pattern MTLAttributeFormatUInt2
  , pattern MTLAttributeFormatUInt3
  , pattern MTLAttributeFormatUInt4
  , pattern MTLAttributeFormatInt1010102Normalized
  , pattern MTLAttributeFormatUInt1010102Normalized
  , pattern MTLAttributeFormatUChar4Normalized_BGRA
  , pattern MTLAttributeFormatUChar
  , pattern MTLAttributeFormatChar
  , pattern MTLAttributeFormatUCharNormalized
  , pattern MTLAttributeFormatCharNormalized
  , pattern MTLAttributeFormatUShort
  , pattern MTLAttributeFormatShort
  , pattern MTLAttributeFormatUShortNormalized
  , pattern MTLAttributeFormatShortNormalized
  , pattern MTLAttributeFormatHalf
  , pattern MTLAttributeFormatFloatRG11B10
  , pattern MTLAttributeFormatFloatRGB9E5
  , MTLIndexType(MTLIndexType)
  , pattern MTLIndexTypeUInt16
  , pattern MTLIndexTypeUInt32
  , MTLMatrixLayout(MTLMatrixLayout)
  , pattern MTLMatrixLayoutColumnMajor
  , pattern MTLMatrixLayoutRowMajor

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ descriptor@
descriptor :: IO (Id MTLAccelerationStructureMotionTriangleGeometryDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLAccelerationStructureMotionTriangleGeometryDescriptor"
    sendClassMessage cls' descriptorSelector

-- | Vertex buffer containing triangle vertices similar to what MTLAccelerationStructureTriangleGeometryDescriptor has but array of the values.
--
-- ObjC selector: @- vertexBuffers@
vertexBuffers :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO (Id NSArray)
vertexBuffers mtlAccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor vertexBuffersSelector

-- | Vertex buffer containing triangle vertices similar to what MTLAccelerationStructureTriangleGeometryDescriptor has but array of the values.
--
-- ObjC selector: @- setVertexBuffers:@
setVertexBuffers :: (IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor, IsNSArray value) => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> value -> IO ()
setVertexBuffers mtlAccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor setVertexBuffersSelector (toNSArray value)

-- | Format type of the vertex buffers across all keyframes. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- vertexFormat@
vertexFormat :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO MTLAttributeFormat
vertexFormat mtlAccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor vertexFormatSelector

-- | Format type of the vertex buffers across all keyframes. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- setVertexFormat:@
setVertexFormat :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> MTLAttributeFormat -> IO ()
setVertexFormat mtlAccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor setVertexFormatSelector value

-- | Stride, in bytes, between vertices in each keyframe's vertex buffer. Must be a multiple of the vertex format data type size and must be aligned to the vertex format data type's alignment. Defaults to 0, which will result in a stride of the vertex format data size.
--
-- ObjC selector: @- vertexStride@
vertexStride :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO CULong
vertexStride mtlAccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor vertexStrideSelector

-- | Stride, in bytes, between vertices in each keyframe's vertex buffer. Must be a multiple of the vertex format data type size and must be aligned to the vertex format data type's alignment. Defaults to 0, which will result in a stride of the vertex format data size.
--
-- ObjC selector: @- setVertexStride:@
setVertexStride :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> CULong -> IO ()
setVertexStride mtlAccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor setVertexStrideSelector value

-- | Optional index buffer containing references to vertices in the vertex buffer. May be nil.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO RawId
indexBuffer mtlAccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor indexBufferSelector

-- | Optional index buffer containing references to vertices in the vertex buffer. May be nil.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> RawId -> IO ()
setIndexBuffer mtlAccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor setIndexBufferSelector value

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- indexBufferOffset@
indexBufferOffset :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO CULong
indexBufferOffset mtlAccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor indexBufferOffsetSelector

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- setIndexBufferOffset:@
setIndexBufferOffset :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> CULong -> IO ()
setIndexBufferOffset mtlAccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor setIndexBufferOffsetSelector value

-- | Index type
--
-- ObjC selector: @- indexType@
indexType :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO MTLIndexType
indexType mtlAccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor indexTypeSelector

-- | Index type
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtlAccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor setIndexTypeSelector value

-- | Number of triangles
--
-- ObjC selector: @- triangleCount@
triangleCount :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO CULong
triangleCount mtlAccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor triangleCountSelector

-- | Number of triangles
--
-- ObjC selector: @- setTriangleCount:@
setTriangleCount :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> CULong -> IO ()
setTriangleCount mtlAccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor setTriangleCountSelector value

-- | Buffer containing packed float4x3 transformation matrix. Transform is applied to the vertex data when building the acceleration structure. Input vertex buffers are not modified. The transformation matrix is applied to all keyframes' vertex data. When set to nil, transformation matrix is not applied to vertex data.
--
-- ObjC selector: @- transformationMatrixBuffer@
transformationMatrixBuffer :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO RawId
transformationMatrixBuffer mtlAccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor transformationMatrixBufferSelector

-- | Buffer containing packed float4x3 transformation matrix. Transform is applied to the vertex data when building the acceleration structure. Input vertex buffers are not modified. The transformation matrix is applied to all keyframes' vertex data. When set to nil, transformation matrix is not applied to vertex data.
--
-- ObjC selector: @- setTransformationMatrixBuffer:@
setTransformationMatrixBuffer :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> RawId -> IO ()
setTransformationMatrixBuffer mtlAccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor setTransformationMatrixBufferSelector value

-- | Transformation matrix buffer offset. Must be a multiple of 4 bytes. Defaults to 0.
--
-- ObjC selector: @- transformationMatrixBufferOffset@
transformationMatrixBufferOffset :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO CULong
transformationMatrixBufferOffset mtlAccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor transformationMatrixBufferOffsetSelector

-- | Transformation matrix buffer offset. Must be a multiple of 4 bytes. Defaults to 0.
--
-- ObjC selector: @- setTransformationMatrixBufferOffset:@
setTransformationMatrixBufferOffset :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> CULong -> IO ()
setTransformationMatrixBufferOffset mtlAccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor setTransformationMatrixBufferOffsetSelector value

-- | Matrix layout for the transformation matrix in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- transformationMatrixLayout@
transformationMatrixLayout :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO MTLMatrixLayout
transformationMatrixLayout mtlAccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor transformationMatrixLayoutSelector

-- | Matrix layout for the transformation matrix in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- setTransformationMatrixLayout:@
setTransformationMatrixLayout :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> MTLMatrixLayout -> IO ()
setTransformationMatrixLayout mtlAccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionTriangleGeometryDescriptor setTransformationMatrixLayoutSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MTLAccelerationStructureMotionTriangleGeometryDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @vertexBuffers@
vertexBuffersSelector :: Selector '[] (Id NSArray)
vertexBuffersSelector = mkSelector "vertexBuffers"

-- | @Selector@ for @setVertexBuffers:@
setVertexBuffersSelector :: Selector '[Id NSArray] ()
setVertexBuffersSelector = mkSelector "setVertexBuffers:"

-- | @Selector@ for @vertexFormat@
vertexFormatSelector :: Selector '[] MTLAttributeFormat
vertexFormatSelector = mkSelector "vertexFormat"

-- | @Selector@ for @setVertexFormat:@
setVertexFormatSelector :: Selector '[MTLAttributeFormat] ()
setVertexFormatSelector = mkSelector "setVertexFormat:"

-- | @Selector@ for @vertexStride@
vertexStrideSelector :: Selector '[] CULong
vertexStrideSelector = mkSelector "vertexStride"

-- | @Selector@ for @setVertexStride:@
setVertexStrideSelector :: Selector '[CULong] ()
setVertexStrideSelector = mkSelector "setVertexStride:"

-- | @Selector@ for @indexBuffer@
indexBufferSelector :: Selector '[] RawId
indexBufferSelector = mkSelector "indexBuffer"

-- | @Selector@ for @setIndexBuffer:@
setIndexBufferSelector :: Selector '[RawId] ()
setIndexBufferSelector = mkSelector "setIndexBuffer:"

-- | @Selector@ for @indexBufferOffset@
indexBufferOffsetSelector :: Selector '[] CULong
indexBufferOffsetSelector = mkSelector "indexBufferOffset"

-- | @Selector@ for @setIndexBufferOffset:@
setIndexBufferOffsetSelector :: Selector '[CULong] ()
setIndexBufferOffsetSelector = mkSelector "setIndexBufferOffset:"

-- | @Selector@ for @indexType@
indexTypeSelector :: Selector '[] MTLIndexType
indexTypeSelector = mkSelector "indexType"

-- | @Selector@ for @setIndexType:@
setIndexTypeSelector :: Selector '[MTLIndexType] ()
setIndexTypeSelector = mkSelector "setIndexType:"

-- | @Selector@ for @triangleCount@
triangleCountSelector :: Selector '[] CULong
triangleCountSelector = mkSelector "triangleCount"

-- | @Selector@ for @setTriangleCount:@
setTriangleCountSelector :: Selector '[CULong] ()
setTriangleCountSelector = mkSelector "setTriangleCount:"

-- | @Selector@ for @transformationMatrixBuffer@
transformationMatrixBufferSelector :: Selector '[] RawId
transformationMatrixBufferSelector = mkSelector "transformationMatrixBuffer"

-- | @Selector@ for @setTransformationMatrixBuffer:@
setTransformationMatrixBufferSelector :: Selector '[RawId] ()
setTransformationMatrixBufferSelector = mkSelector "setTransformationMatrixBuffer:"

-- | @Selector@ for @transformationMatrixBufferOffset@
transformationMatrixBufferOffsetSelector :: Selector '[] CULong
transformationMatrixBufferOffsetSelector = mkSelector "transformationMatrixBufferOffset"

-- | @Selector@ for @setTransformationMatrixBufferOffset:@
setTransformationMatrixBufferOffsetSelector :: Selector '[CULong] ()
setTransformationMatrixBufferOffsetSelector = mkSelector "setTransformationMatrixBufferOffset:"

-- | @Selector@ for @transformationMatrixLayout@
transformationMatrixLayoutSelector :: Selector '[] MTLMatrixLayout
transformationMatrixLayoutSelector = mkSelector "transformationMatrixLayout"

-- | @Selector@ for @setTransformationMatrixLayout:@
setTransformationMatrixLayoutSelector :: Selector '[MTLMatrixLayout] ()
setTransformationMatrixLayoutSelector = mkSelector "setTransformationMatrixLayout:"

