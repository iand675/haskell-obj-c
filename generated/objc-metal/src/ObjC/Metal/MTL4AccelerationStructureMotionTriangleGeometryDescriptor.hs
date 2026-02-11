{-# LANGUAGE PatternSynonyms #-}
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
  , vertexBuffersSelector
  , setVertexBuffersSelector
  , vertexFormatSelector
  , setVertexFormatSelector
  , vertexStrideSelector
  , setVertexStrideSelector
  , indexBufferSelector
  , setIndexBufferSelector
  , indexTypeSelector
  , setIndexTypeSelector
  , triangleCountSelector
  , setTriangleCountSelector
  , transformationMatrixBufferSelector
  , setTransformationMatrixBufferSelector
  , transformationMatrixLayoutSelector
  , setTransformationMatrixLayoutSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
vertexBuffers mtL4AccelerationStructureMotionTriangleGeometryDescriptor  =
  sendMsgStret mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "vertexBuffers") retMTL4BufferRange []

-- | Assigns a buffer where each entry contains a reference to a vertex buffer.
--
-- This property references a buffer that conceptually represents an array with one entry for each keyframe in the motion animation. Each one of these entries consists of a ``MTL4BufferRange`` that, in turn, references a vertex buffer containing the vertex data for the keyframe.
--
-- You are responsible for ensuring the buffer address is not zero for the top-level buffer, as well as for all the vertex buffers it references.
--
-- ObjC selector: @- setVertexBuffers:@
setVertexBuffers :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> MTL4BufferRange -> IO ()
setVertexBuffers mtL4AccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setVertexBuffers:") retVoid [argMTL4BufferRange value]

-- | Defines the format of the vertices in the vertex buffers.
--
-- All keyframes share the same vertex format. Defaults to @MTLAttributeFormatFloat3@, corresponding to three packed floating point numbers.
--
-- ObjC selector: @- vertexFormat@
vertexFormat :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO MTLAttributeFormat
vertexFormat mtL4AccelerationStructureMotionTriangleGeometryDescriptor  =
  fmap (coerce :: CULong -> MTLAttributeFormat) $ sendMsg mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "vertexFormat") retCULong []

-- | Defines the format of the vertices in the vertex buffers.
--
-- All keyframes share the same vertex format. Defaults to @MTLAttributeFormatFloat3@, corresponding to three packed floating point numbers.
--
-- ObjC selector: @- setVertexFormat:@
setVertexFormat :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> MTLAttributeFormat -> IO ()
setVertexFormat mtL4AccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setVertexFormat:") retVoid [argCULong (coerce value)]

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
vertexStride mtL4AccelerationStructureMotionTriangleGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "vertexStride") retCULong []

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
setVertexStride mtL4AccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setVertexStride:") retVoid [argCULong (fromIntegral value)]

-- | Assigns an optional index buffer containing references to vertices in the vertex buffers you reference through the vertex buffers property.
--
-- You can set this property to @0@, the default, to avoid specifying an index buffer. All keyframes share the same index buffer.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO MTL4BufferRange
indexBuffer mtL4AccelerationStructureMotionTriangleGeometryDescriptor  =
  sendMsgStret mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "indexBuffer") retMTL4BufferRange []

-- | Assigns an optional index buffer containing references to vertices in the vertex buffers you reference through the vertex buffers property.
--
-- You can set this property to @0@, the default, to avoid specifying an index buffer. All keyframes share the same index buffer.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> MTL4BufferRange -> IO ()
setIndexBuffer mtL4AccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setIndexBuffer:") retVoid [argMTL4BufferRange value]

-- | Specifies the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- indexType@
indexType :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO MTLIndexType
indexType mtL4AccelerationStructureMotionTriangleGeometryDescriptor  =
  fmap (coerce :: CULong -> MTLIndexType) $ sendMsg mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "indexType") retCULong []

-- | Specifies the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtL4AccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setIndexType:") retVoid [argCULong (coerce value)]

-- | Declares the number of triangles in the vertex buffers that the buffer in the vertex buffers property references.
--
-- All keyframes share the same triangle count.
--
-- ObjC selector: @- triangleCount@
triangleCount :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO CULong
triangleCount mtL4AccelerationStructureMotionTriangleGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "triangleCount") retCULong []

-- | Declares the number of triangles in the vertex buffers that the buffer in the vertex buffers property references.
--
-- All keyframes share the same triangle count.
--
-- ObjC selector: @- setTriangleCount:@
setTriangleCount :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> CULong -> IO ()
setTriangleCount mtL4AccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setTriangleCount:") retVoid [argCULong (fromIntegral value)]

-- | Assings an optional reference to a buffer containing a @float4x3@ transformation matrix.
--
-- When the buffer address is non-zero, Metal applies this transform to the vertex data positions when building the acceleration structure. All keyframes share the same transformation matrix.
--
-- Building an acceleration structure with a descriptor that specifies this property doesn't modify the contents of the input @vertexBuffer@.
--
-- ObjC selector: @- transformationMatrixBuffer@
transformationMatrixBuffer :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO MTL4BufferRange
transformationMatrixBuffer mtL4AccelerationStructureMotionTriangleGeometryDescriptor  =
  sendMsgStret mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "transformationMatrixBuffer") retMTL4BufferRange []

-- | Assings an optional reference to a buffer containing a @float4x3@ transformation matrix.
--
-- When the buffer address is non-zero, Metal applies this transform to the vertex data positions when building the acceleration structure. All keyframes share the same transformation matrix.
--
-- Building an acceleration structure with a descriptor that specifies this property doesn't modify the contents of the input @vertexBuffer@.
--
-- ObjC selector: @- setTransformationMatrixBuffer:@
setTransformationMatrixBuffer :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> MTL4BufferRange -> IO ()
setTransformationMatrixBuffer mtL4AccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setTransformationMatrixBuffer:") retVoid [argMTL4BufferRange value]

-- | Configures the layout for the transformation matrix in the transformation matrix buffer.
--
-- You can provide matrices in column-major or row-major form, and this property allows you to control how Metal interprets them.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- transformationMatrixLayout@
transformationMatrixLayout :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> IO MTLMatrixLayout
transformationMatrixLayout mtL4AccelerationStructureMotionTriangleGeometryDescriptor  =
  fmap (coerce :: CLong -> MTLMatrixLayout) $ sendMsg mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "transformationMatrixLayout") retCLong []

-- | Configures the layout for the transformation matrix in the transformation matrix buffer.
--
-- You can provide matrices in column-major or row-major form, and this property allows you to control how Metal interprets them.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- setTransformationMatrixLayout:@
setTransformationMatrixLayout :: IsMTL4AccelerationStructureMotionTriangleGeometryDescriptor mtL4AccelerationStructureMotionTriangleGeometryDescriptor => mtL4AccelerationStructureMotionTriangleGeometryDescriptor -> MTLMatrixLayout -> IO ()
setTransformationMatrixLayout mtL4AccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setTransformationMatrixLayout:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vertexBuffers@
vertexBuffersSelector :: Selector
vertexBuffersSelector = mkSelector "vertexBuffers"

-- | @Selector@ for @setVertexBuffers:@
setVertexBuffersSelector :: Selector
setVertexBuffersSelector = mkSelector "setVertexBuffers:"

-- | @Selector@ for @vertexFormat@
vertexFormatSelector :: Selector
vertexFormatSelector = mkSelector "vertexFormat"

-- | @Selector@ for @setVertexFormat:@
setVertexFormatSelector :: Selector
setVertexFormatSelector = mkSelector "setVertexFormat:"

-- | @Selector@ for @vertexStride@
vertexStrideSelector :: Selector
vertexStrideSelector = mkSelector "vertexStride"

-- | @Selector@ for @setVertexStride:@
setVertexStrideSelector :: Selector
setVertexStrideSelector = mkSelector "setVertexStride:"

-- | @Selector@ for @indexBuffer@
indexBufferSelector :: Selector
indexBufferSelector = mkSelector "indexBuffer"

-- | @Selector@ for @setIndexBuffer:@
setIndexBufferSelector :: Selector
setIndexBufferSelector = mkSelector "setIndexBuffer:"

-- | @Selector@ for @indexType@
indexTypeSelector :: Selector
indexTypeSelector = mkSelector "indexType"

-- | @Selector@ for @setIndexType:@
setIndexTypeSelector :: Selector
setIndexTypeSelector = mkSelector "setIndexType:"

-- | @Selector@ for @triangleCount@
triangleCountSelector :: Selector
triangleCountSelector = mkSelector "triangleCount"

-- | @Selector@ for @setTriangleCount:@
setTriangleCountSelector :: Selector
setTriangleCountSelector = mkSelector "setTriangleCount:"

-- | @Selector@ for @transformationMatrixBuffer@
transformationMatrixBufferSelector :: Selector
transformationMatrixBufferSelector = mkSelector "transformationMatrixBuffer"

-- | @Selector@ for @setTransformationMatrixBuffer:@
setTransformationMatrixBufferSelector :: Selector
setTransformationMatrixBufferSelector = mkSelector "setTransformationMatrixBuffer:"

-- | @Selector@ for @transformationMatrixLayout@
transformationMatrixLayoutSelector :: Selector
transformationMatrixLayoutSelector = mkSelector "transformationMatrixLayout"

-- | @Selector@ for @setTransformationMatrixLayout:@
setTransformationMatrixLayoutSelector :: Selector
setTransformationMatrixLayoutSelector = mkSelector "setTransformationMatrixLayout:"

