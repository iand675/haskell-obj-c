{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes triangle geometry suitable for ray tracing.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers this descriptor references when you build this acceleration structure.
--
-- Generated bindings for @MTL4AccelerationStructureTriangleGeometryDescriptor@.
module ObjC.Metal.MTL4AccelerationStructureTriangleGeometryDescriptor
  ( MTL4AccelerationStructureTriangleGeometryDescriptor
  , IsMTL4AccelerationStructureTriangleGeometryDescriptor(..)
  , vertexBuffer
  , setVertexBuffer
  , vertexFormat
  , setVertexFormat
  , vertexStride
  , setVertexStride
  , indexBuffer
  , setIndexBuffer
  , indexType
  , setIndexType
  , triangleCount
  , setTriangleCount
  , transformationMatrixBuffer
  , setTransformationMatrixBuffer
  , transformationMatrixLayout
  , setTransformationMatrixLayout
  , indexBufferSelector
  , indexTypeSelector
  , setIndexBufferSelector
  , setIndexTypeSelector
  , setTransformationMatrixBufferSelector
  , setTransformationMatrixLayoutSelector
  , setTriangleCountSelector
  , setVertexBufferSelector
  , setVertexFormatSelector
  , setVertexStrideSelector
  , transformationMatrixBufferSelector
  , transformationMatrixLayoutSelector
  , triangleCountSelector
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
import ObjC.Metal.Internal.Structs
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Associates a vertex buffer containing triangle vertices.
--
-- You are responsible for ensuring that the format of all vertex positions match the ``vertexFormat`` property, and that the buffer address for the buffer range is not zero.
--
-- ObjC selector: @- vertexBuffer@
vertexBuffer :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> IO MTL4BufferRange
vertexBuffer mtL4AccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor vertexBufferSelector

-- | Associates a vertex buffer containing triangle vertices.
--
-- You are responsible for ensuring that the format of all vertex positions match the ``vertexFormat`` property, and that the buffer address for the buffer range is not zero.
--
-- ObjC selector: @- setVertexBuffer:@
setVertexBuffer :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> MTL4BufferRange -> IO ()
setVertexBuffer mtL4AccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor setVertexBufferSelector value

-- | Describes the format of the vertices in the vertex buffer.
--
-- This property controls the format of the position attribute of the vertices the ``vertexBuffer`` references.
--
-- The format defaults to @MTLAttributeFormatFloat3@, corresponding to three packed floating point numbers.
--
-- ObjC selector: @- vertexFormat@
vertexFormat :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> IO MTLAttributeFormat
vertexFormat mtL4AccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor vertexFormatSelector

-- | Describes the format of the vertices in the vertex buffer.
--
-- This property controls the format of the position attribute of the vertices the ``vertexBuffer`` references.
--
-- The format defaults to @MTLAttributeFormatFloat3@, corresponding to three packed floating point numbers.
--
-- ObjC selector: @- setVertexFormat:@
setVertexFormat :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> MTLAttributeFormat -> IO ()
setVertexFormat mtL4AccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor setVertexFormatSelector value

-- | Sets the stride, in bytes, between vertices in the vertex buffer.
--
-- The stride you specify needs to be a multiple of the size of the vertex format you provide in the ``vertexFormat`` property. Similarly, you are responsible for ensuring this stride matches the vertex format data type's alignment.
--
-- Defaults to @0@, which signals the stride matches the size of the ``vertexFormat`` data.
--
-- ObjC selector: @- vertexStride@
vertexStride :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> IO CULong
vertexStride mtL4AccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor vertexStrideSelector

-- | Sets the stride, in bytes, between vertices in the vertex buffer.
--
-- The stride you specify needs to be a multiple of the size of the vertex format you provide in the ``vertexFormat`` property. Similarly, you are responsible for ensuring this stride matches the vertex format data type's alignment.
--
-- Defaults to @0@, which signals the stride matches the size of the ``vertexFormat`` data.
--
-- ObjC selector: @- setVertexStride:@
setVertexStride :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> CULong -> IO ()
setVertexStride mtL4AccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor setVertexStrideSelector value

-- | Sets an optional index buffer containing references to vertices in the @vertexBuffer@.
--
-- You can set this property to @0@, the default, to avoid specifying an index buffer.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> IO MTL4BufferRange
indexBuffer mtL4AccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor indexBufferSelector

-- | Sets an optional index buffer containing references to vertices in the @vertexBuffer@.
--
-- You can set this property to @0@, the default, to avoid specifying an index buffer.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> MTL4BufferRange -> IO ()
setIndexBuffer mtL4AccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor setIndexBufferSelector value

-- | Configures the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- indexType@
indexType :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> IO MTLIndexType
indexType mtL4AccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor indexTypeSelector

-- | Configures the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtL4AccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor setIndexTypeSelector value

-- | Declares the number of triangles in this geometry descriptor.
--
-- ObjC selector: @- triangleCount@
triangleCount :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> IO CULong
triangleCount mtL4AccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor triangleCountSelector

-- | Declares the number of triangles in this geometry descriptor.
--
-- ObjC selector: @- setTriangleCount:@
setTriangleCount :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> CULong -> IO ()
setTriangleCount mtL4AccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor setTriangleCountSelector value

-- | Assigns an optional reference to a buffer containing a @float4x3@ transformation matrix.
--
-- When the buffer address is non-zero, Metal applies this transform to the vertex data positions when building the acceleration structure.
--
-- Building an acceleration structure with a descriptor that specifies this property doesn't modify the contents of the input @vertexBuffer@.
--
-- ObjC selector: @- transformationMatrixBuffer@
transformationMatrixBuffer :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> IO MTL4BufferRange
transformationMatrixBuffer mtL4AccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor transformationMatrixBufferSelector

-- | Assigns an optional reference to a buffer containing a @float4x3@ transformation matrix.
--
-- When the buffer address is non-zero, Metal applies this transform to the vertex data positions when building the acceleration structure.
--
-- Building an acceleration structure with a descriptor that specifies this property doesn't modify the contents of the input @vertexBuffer@.
--
-- ObjC selector: @- setTransformationMatrixBuffer:@
setTransformationMatrixBuffer :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> MTL4BufferRange -> IO ()
setTransformationMatrixBuffer mtL4AccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor setTransformationMatrixBufferSelector value

-- | Configures the layout for the transformation matrix in the transformation matrix buffer.
--
-- You can provide matrices in column-major or row-major form, and this property allows you to control how Metal interprets them.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- transformationMatrixLayout@
transformationMatrixLayout :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> IO MTLMatrixLayout
transformationMatrixLayout mtL4AccelerationStructureTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor transformationMatrixLayoutSelector

-- | Configures the layout for the transformation matrix in the transformation matrix buffer.
--
-- You can provide matrices in column-major or row-major form, and this property allows you to control how Metal interprets them.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- setTransformationMatrixLayout:@
setTransformationMatrixLayout :: IsMTL4AccelerationStructureTriangleGeometryDescriptor mtL4AccelerationStructureTriangleGeometryDescriptor => mtL4AccelerationStructureTriangleGeometryDescriptor -> MTLMatrixLayout -> IO ()
setTransformationMatrixLayout mtL4AccelerationStructureTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureTriangleGeometryDescriptor setTransformationMatrixLayoutSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vertexBuffer@
vertexBufferSelector :: Selector '[] MTL4BufferRange
vertexBufferSelector = mkSelector "vertexBuffer"

-- | @Selector@ for @setVertexBuffer:@
setVertexBufferSelector :: Selector '[MTL4BufferRange] ()
setVertexBufferSelector = mkSelector "setVertexBuffer:"

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
indexBufferSelector :: Selector '[] MTL4BufferRange
indexBufferSelector = mkSelector "indexBuffer"

-- | @Selector@ for @setIndexBuffer:@
setIndexBufferSelector :: Selector '[MTL4BufferRange] ()
setIndexBufferSelector = mkSelector "setIndexBuffer:"

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
transformationMatrixBufferSelector :: Selector '[] MTL4BufferRange
transformationMatrixBufferSelector = mkSelector "transformationMatrixBuffer"

-- | @Selector@ for @setTransformationMatrixBuffer:@
setTransformationMatrixBufferSelector :: Selector '[MTL4BufferRange] ()
setTransformationMatrixBufferSelector = mkSelector "setTransformationMatrixBuffer:"

-- | @Selector@ for @transformationMatrixLayout@
transformationMatrixLayoutSelector :: Selector '[] MTLMatrixLayout
transformationMatrixLayoutSelector = mkSelector "transformationMatrixLayout"

-- | @Selector@ for @setTransformationMatrixLayout:@
setTransformationMatrixLayoutSelector :: Selector '[MTLMatrixLayout] ()
setTransformationMatrixLayoutSelector = mkSelector "setTransformationMatrixLayout:"

