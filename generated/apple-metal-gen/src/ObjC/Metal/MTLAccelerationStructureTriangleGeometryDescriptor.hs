{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Descriptor for triangle geometry
--
-- Generated bindings for @MTLAccelerationStructureTriangleGeometryDescriptor@.
module ObjC.Metal.MTLAccelerationStructureTriangleGeometryDescriptor
  ( MTLAccelerationStructureTriangleGeometryDescriptor
  , IsMTLAccelerationStructureTriangleGeometryDescriptor(..)
  , descriptor
  , vertexBuffer
  , setVertexBuffer
  , vertexBufferOffset
  , setVertexBufferOffset
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
  , setVertexBufferOffsetSelector
  , setVertexBufferSelector
  , setVertexFormatSelector
  , setVertexStrideSelector
  , transformationMatrixBufferOffsetSelector
  , transformationMatrixBufferSelector
  , transformationMatrixLayoutSelector
  , triangleCountSelector
  , vertexBufferOffsetSelector
  , vertexBufferSelector
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
descriptor :: IO (Id MTLAccelerationStructureTriangleGeometryDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLAccelerationStructureTriangleGeometryDescriptor"
    sendClassMessage cls' descriptorSelector

-- | Vertex buffer containing triangle vertices. Each vertex position must be formatted according to the vertex format. Must not be nil.
--
-- ObjC selector: @- vertexBuffer@
vertexBuffer :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> IO RawId
vertexBuffer mtlAccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor vertexBufferSelector

-- | Vertex buffer containing triangle vertices. Each vertex position must be formatted according to the vertex format. Must not be nil.
--
-- ObjC selector: @- setVertexBuffer:@
setVertexBuffer :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> RawId -> IO ()
setVertexBuffer mtlAccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor setVertexBufferSelector value

-- | Vertex buffer offset. Must be a multiple of the vertex stride and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- vertexBufferOffset@
vertexBufferOffset :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> IO CULong
vertexBufferOffset mtlAccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor vertexBufferOffsetSelector

-- | Vertex buffer offset. Must be a multiple of the vertex stride and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setVertexBufferOffset:@
setVertexBufferOffset :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> CULong -> IO ()
setVertexBufferOffset mtlAccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor setVertexBufferOffsetSelector value

-- | Format type of the vertex buffer. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- vertexFormat@
vertexFormat :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> IO MTLAttributeFormat
vertexFormat mtlAccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor vertexFormatSelector

-- | Format type of the vertex buffer. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- setVertexFormat:@
setVertexFormat :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> MTLAttributeFormat -> IO ()
setVertexFormat mtlAccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor setVertexFormatSelector value

-- | Stride, in bytes, between vertices in the vertex buffer. Must be a multiple of the vertex format data type size and must be aligned to the vertex format data type's alignment. Defaults to 0, which will result in a stride of the vertex format data size.
--
-- ObjC selector: @- vertexStride@
vertexStride :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> IO CULong
vertexStride mtlAccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor vertexStrideSelector

-- | Stride, in bytes, between vertices in the vertex buffer. Must be a multiple of the vertex format data type size and must be aligned to the vertex format data type's alignment. Defaults to 0, which will result in a stride of the vertex format data size.
--
-- ObjC selector: @- setVertexStride:@
setVertexStride :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> CULong -> IO ()
setVertexStride mtlAccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor setVertexStrideSelector value

-- | Optional index buffer containing references to vertices in the vertex buffer. May be nil.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> IO RawId
indexBuffer mtlAccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor indexBufferSelector

-- | Optional index buffer containing references to vertices in the vertex buffer. May be nil.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> RawId -> IO ()
setIndexBuffer mtlAccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor setIndexBufferSelector value

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- indexBufferOffset@
indexBufferOffset :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> IO CULong
indexBufferOffset mtlAccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor indexBufferOffsetSelector

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- setIndexBufferOffset:@
setIndexBufferOffset :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> CULong -> IO ()
setIndexBufferOffset mtlAccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor setIndexBufferOffsetSelector value

-- | Index type
--
-- ObjC selector: @- indexType@
indexType :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> IO MTLIndexType
indexType mtlAccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor indexTypeSelector

-- | Index type
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtlAccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor setIndexTypeSelector value

-- | Number of triangles
--
-- ObjC selector: @- triangleCount@
triangleCount :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> IO CULong
triangleCount mtlAccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor triangleCountSelector

-- | Number of triangles
--
-- ObjC selector: @- setTriangleCount:@
setTriangleCount :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> CULong -> IO ()
setTriangleCount mtlAccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor setTriangleCountSelector value

-- | Buffer containing packed float4x3 transformation matrix. Transform is applied to the vertex data when building the acceleration structure. Input vertex buffers are not modified. When set to nil, transformation matrix is not applied to vertex data.
--
-- ObjC selector: @- transformationMatrixBuffer@
transformationMatrixBuffer :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> IO RawId
transformationMatrixBuffer mtlAccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor transformationMatrixBufferSelector

-- | Buffer containing packed float4x3 transformation matrix. Transform is applied to the vertex data when building the acceleration structure. Input vertex buffers are not modified. When set to nil, transformation matrix is not applied to vertex data.
--
-- ObjC selector: @- setTransformationMatrixBuffer:@
setTransformationMatrixBuffer :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> RawId -> IO ()
setTransformationMatrixBuffer mtlAccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor setTransformationMatrixBufferSelector value

-- | Transformation matrix buffer offset. Must be a multiple of 4 bytes. Defaults to 0.
--
-- ObjC selector: @- transformationMatrixBufferOffset@
transformationMatrixBufferOffset :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> IO CULong
transformationMatrixBufferOffset mtlAccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor transformationMatrixBufferOffsetSelector

-- | Transformation matrix buffer offset. Must be a multiple of 4 bytes. Defaults to 0.
--
-- ObjC selector: @- setTransformationMatrixBufferOffset:@
setTransformationMatrixBufferOffset :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> CULong -> IO ()
setTransformationMatrixBufferOffset mtlAccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor setTransformationMatrixBufferOffsetSelector value

-- | Matrix layout for the transformation matrix in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- transformationMatrixLayout@
transformationMatrixLayout :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> IO MTLMatrixLayout
transformationMatrixLayout mtlAccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor transformationMatrixLayoutSelector

-- | Matrix layout for the transformation matrix in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- setTransformationMatrixLayout:@
setTransformationMatrixLayout :: IsMTLAccelerationStructureTriangleGeometryDescriptor mtlAccelerationStructureTriangleGeometryDescriptor => mtlAccelerationStructureTriangleGeometryDescriptor -> MTLMatrixLayout -> IO ()
setTransformationMatrixLayout mtlAccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtlAccelerationStructureTriangleGeometryDescriptor setTransformationMatrixLayoutSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MTLAccelerationStructureTriangleGeometryDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @vertexBuffer@
vertexBufferSelector :: Selector '[] RawId
vertexBufferSelector = mkSelector "vertexBuffer"

-- | @Selector@ for @setVertexBuffer:@
setVertexBufferSelector :: Selector '[RawId] ()
setVertexBufferSelector = mkSelector "setVertexBuffer:"

-- | @Selector@ for @vertexBufferOffset@
vertexBufferOffsetSelector :: Selector '[] CULong
vertexBufferOffsetSelector = mkSelector "vertexBufferOffset"

-- | @Selector@ for @setVertexBufferOffset:@
setVertexBufferOffsetSelector :: Selector '[CULong] ()
setVertexBufferOffsetSelector = mkSelector "setVertexBufferOffset:"

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

