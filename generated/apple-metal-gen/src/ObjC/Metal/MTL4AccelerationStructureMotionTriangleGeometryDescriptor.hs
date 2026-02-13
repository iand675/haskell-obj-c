{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes motion triangle geometry, suitable for motion ray tracing.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers this descriptor references when you build this acceleration structure.
--
-- Generated bindings for @MTL4AccelerationStructureMotionTriangleGeometryDescriptor@.
module ObjC.Metal.MTL4AccelerationStructureMotionTriangleGeometryDescriptor
  ( MTL4AccelerationStructureMotionTriangleGeometryDescriptor
  , IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor(..)
  , vertexBuffers
  , setVertexBuffers
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
  , setVertexBuffersSelector
  , setVertexFormatSelector
  , setVertexStrideSelector
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
import ObjC.Metal.Internal.Structs
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Assigns a buffer where each entry contains a reference to a vertex buffer.
--
-- This property references a buffer that conceptually represents an array with one entry for each keyframe in the motion animation. Each one of these entries consists of a ``MTL4BufferRange`` that, in turn, references a vertex buffer containing the vertex data for the keyframe.
--
-- You are responsible for ensuring the buffer address is not zero for the top-level buffer, as well as for all the vertex buffers it references.
--
-- ObjC selector: @- vertexBuffers@
vertexBuffers :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO MTL4BufferRange
vertexBuffers mtL4AccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor vertexBuffersSelector

-- | Assigns a buffer where each entry contains a reference to a vertex buffer.
--
-- This property references a buffer that conceptually represents an array with one entry for each keyframe in the motion animation. Each one of these entries consists of a ``MTL4BufferRange`` that, in turn, references a vertex buffer containing the vertex data for the keyframe.
--
-- You are responsible for ensuring the buffer address is not zero for the top-level buffer, as well as for all the vertex buffers it references.
--
-- ObjC selector: @- setVertexBuffers:@
setVertexBuffers :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> MTL4BufferRange -> IO ()
setVertexBuffers mtL4AccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor setVertexBuffersSelector value

-- | Defines the format of the vertices in the vertex buffers.
--
-- All keyframes share the same vertex format. Defaults to @MTLAttributeFormatFloat3@, corresponding to three packed floating point numbers.
--
-- ObjC selector: @- vertexFormat@
vertexFormat :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO MTLAttributeFormat
vertexFormat mtL4AccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor vertexFormatSelector

-- | Defines the format of the vertices in the vertex buffers.
--
-- All keyframes share the same vertex format. Defaults to @MTLAttributeFormatFloat3@, corresponding to three packed floating point numbers.
--
-- ObjC selector: @- setVertexFormat:@
setVertexFormat :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> MTLAttributeFormat -> IO ()
setVertexFormat mtL4AccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor setVertexFormatSelector value

-- | Sets the stride, in bytes, between vertices in all the vertex buffer.
--
-- All keyframes share the same vertex stride. This stride needs to be a multiple of the size of the vertex format you provide in the ``vertexFormat`` property.
--
-- Similarly, you are responsible for ensuring this stride matches the vertex format data type's alignment.
--
-- Defaults to @0@, which signals the stride matches the size of the ``vertexFormat`` data.
--
-- ObjC selector: @- vertexStride@
vertexStride :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO CULong
vertexStride mtL4AccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor vertexStrideSelector

-- | Sets the stride, in bytes, between vertices in all the vertex buffer.
--
-- All keyframes share the same vertex stride. This stride needs to be a multiple of the size of the vertex format you provide in the ``vertexFormat`` property.
--
-- Similarly, you are responsible for ensuring this stride matches the vertex format data type's alignment.
--
-- Defaults to @0@, which signals the stride matches the size of the ``vertexFormat`` data.
--
-- ObjC selector: @- setVertexStride:@
setVertexStride :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> CULong -> IO ()
setVertexStride mtL4AccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor setVertexStrideSelector value

-- | Assigns an optional index buffer containing references to vertices in the vertex buffers you reference through the vertex buffers property.
--
-- You can set this property to @0@, the default, to avoid specifying an index buffer. All keyframes share the same index buffer.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO MTL4BufferRange
indexBuffer mtL4AccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor indexBufferSelector

-- | Assigns an optional index buffer containing references to vertices in the vertex buffers you reference through the vertex buffers property.
--
-- You can set this property to @0@, the default, to avoid specifying an index buffer. All keyframes share the same index buffer.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> MTL4BufferRange -> IO ()
setIndexBuffer mtL4AccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor setIndexBufferSelector value

-- | Specifies the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- indexType@
indexType :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO MTLIndexType
indexType mtL4AccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor indexTypeSelector

-- | Specifies the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtL4AccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor setIndexTypeSelector value

-- | Declares the number of triangles in the vertex buffers that the buffer in the vertex buffers property references.
--
-- All keyframes share the same triangle count.
--
-- ObjC selector: @- triangleCount@
triangleCount :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO CULong
triangleCount mtL4AccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor triangleCountSelector

-- | Declares the number of triangles in the vertex buffers that the buffer in the vertex buffers property references.
--
-- All keyframes share the same triangle count.
--
-- ObjC selector: @- setTriangleCount:@
setTriangleCount :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> CULong -> IO ()
setTriangleCount mtL4AccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor setTriangleCountSelector value

-- | Assings an optional reference to a buffer containing a @float4x3@ transformation matrix.
--
-- When the buffer address is non-zero, Metal applies this transform to the vertex data positions when building the acceleration structure. All keyframes share the same transformation matrix.
--
-- Building an acceleration structure with a descriptor that specifies this property doesn't modify the contents of the input @vertexBuffer@.
--
-- ObjC selector: @- transformationMatrixBuffer@
transformationMatrixBuffer :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO MTL4BufferRange
transformationMatrixBuffer mtL4AccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor transformationMatrixBufferSelector

-- | Assings an optional reference to a buffer containing a @float4x3@ transformation matrix.
--
-- When the buffer address is non-zero, Metal applies this transform to the vertex data positions when building the acceleration structure. All keyframes share the same transformation matrix.
--
-- Building an acceleration structure with a descriptor that specifies this property doesn't modify the contents of the input @vertexBuffer@.
--
-- ObjC selector: @- setTransformationMatrixBuffer:@
setTransformationMatrixBuffer :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> MTL4BufferRange -> IO ()
setTransformationMatrixBuffer mtL4AccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor setTransformationMatrixBufferSelector value

-- | Configures the layout for the transformation matrix in the transformation matrix buffer.
--
-- You can provide matrices in column-major or row-major form, and this property allows you to control how Metal interprets them.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- transformationMatrixLayout@
transformationMatrixLayout :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO MTLMatrixLayout
transformationMatrixLayout mtL4AccelerationStructureMotionTriangleGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor transformationMatrixLayoutSelector

-- | Configures the layout for the transformation matrix in the transformation matrix buffer.
--
-- You can provide matrices in column-major or row-major form, and this property allows you to control how Metal interprets them.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- setTransformationMatrixLayout:@
setTransformationMatrixLayout :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> MTLMatrixLayout -> IO ()
setTransformationMatrixLayout mtL4AccelerationStructureMotionTriangleGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionTriangleGeometryDescriptor setTransformationMatrixLayoutSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vertexBuffers@
vertexBuffersSelector :: Selector '[] MTL4BufferRange
vertexBuffersSelector = mkSelector "vertexBuffers"

-- | @Selector@ for @setVertexBuffers:@
setVertexBuffersSelector :: Selector '[MTL4BufferRange] ()
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

