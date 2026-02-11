{-# LANGUAGE PatternSynonyms #-}
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
  , indexBufferOffset
  , setIndexBufferOffset
  , indexType
  , setIndexType
  , triangleCount
  , setTriangleCount
  , transformationMatrixBufferOffset
  , setTransformationMatrixBufferOffset
  , transformationMatrixLayout
  , setTransformationMatrixLayout
  , descriptorSelector
  , vertexBuffersSelector
  , setVertexBuffersSelector
  , vertexFormatSelector
  , setVertexFormatSelector
  , vertexStrideSelector
  , setVertexStrideSelector
  , indexBufferOffsetSelector
  , setIndexBufferOffsetSelector
  , indexTypeSelector
  , setIndexTypeSelector
  , triangleCountSelector
  , setTriangleCountSelector
  , transformationMatrixBufferOffsetSelector
  , setTransformationMatrixBufferOffsetSelector
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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
    sendClassMsg cls' (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Vertex buffer containing triangle vertices similar to what MTLAccelerationStructureTriangleGeometryDescriptor has but array of the values.
--
-- ObjC selector: @- vertexBuffers@
vertexBuffers :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO (Id NSArray)
vertexBuffers mtlAccelerationStructureMotionTriangleGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "vertexBuffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Vertex buffer containing triangle vertices similar to what MTLAccelerationStructureTriangleGeometryDescriptor has but array of the values.
--
-- ObjC selector: @- setVertexBuffers:@
setVertexBuffers :: (IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor, IsNSArray value) => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> value -> IO ()
setVertexBuffers mtlAccelerationStructureMotionTriangleGeometryDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setVertexBuffers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Format type of the vertex buffers across all keyframes. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- vertexFormat@
vertexFormat :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO MTLAttributeFormat
vertexFormat mtlAccelerationStructureMotionTriangleGeometryDescriptor  =
  fmap (coerce :: CULong -> MTLAttributeFormat) $ sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "vertexFormat") retCULong []

-- | Format type of the vertex buffers across all keyframes. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- setVertexFormat:@
setVertexFormat :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> MTLAttributeFormat -> IO ()
setVertexFormat mtlAccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setVertexFormat:") retVoid [argCULong (coerce value)]

-- | Stride, in bytes, between vertices in each keyframe's vertex buffer. Must be a multiple of the vertex format data type size and must be aligned to the vertex format data type's alignment. Defaults to 0, which will result in a stride of the vertex format data size.
--
-- ObjC selector: @- vertexStride@
vertexStride :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO CULong
vertexStride mtlAccelerationStructureMotionTriangleGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "vertexStride") retCULong []

-- | Stride, in bytes, between vertices in each keyframe's vertex buffer. Must be a multiple of the vertex format data type size and must be aligned to the vertex format data type's alignment. Defaults to 0, which will result in a stride of the vertex format data size.
--
-- ObjC selector: @- setVertexStride:@
setVertexStride :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> CULong -> IO ()
setVertexStride mtlAccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setVertexStride:") retVoid [argCULong (fromIntegral value)]

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- indexBufferOffset@
indexBufferOffset :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO CULong
indexBufferOffset mtlAccelerationStructureMotionTriangleGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "indexBufferOffset") retCULong []

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- setIndexBufferOffset:@
setIndexBufferOffset :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> CULong -> IO ()
setIndexBufferOffset mtlAccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setIndexBufferOffset:") retVoid [argCULong (fromIntegral value)]

-- | Index type
--
-- ObjC selector: @- indexType@
indexType :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO MTLIndexType
indexType mtlAccelerationStructureMotionTriangleGeometryDescriptor  =
  fmap (coerce :: CULong -> MTLIndexType) $ sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "indexType") retCULong []

-- | Index type
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtlAccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setIndexType:") retVoid [argCULong (coerce value)]

-- | Number of triangles
--
-- ObjC selector: @- triangleCount@
triangleCount :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO CULong
triangleCount mtlAccelerationStructureMotionTriangleGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "triangleCount") retCULong []

-- | Number of triangles
--
-- ObjC selector: @- setTriangleCount:@
setTriangleCount :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> CULong -> IO ()
setTriangleCount mtlAccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setTriangleCount:") retVoid [argCULong (fromIntegral value)]

-- | Transformation matrix buffer offset. Must be a multiple of 4 bytes. Defaults to 0.
--
-- ObjC selector: @- transformationMatrixBufferOffset@
transformationMatrixBufferOffset :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO CULong
transformationMatrixBufferOffset mtlAccelerationStructureMotionTriangleGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "transformationMatrixBufferOffset") retCULong []

-- | Transformation matrix buffer offset. Must be a multiple of 4 bytes. Defaults to 0.
--
-- ObjC selector: @- setTransformationMatrixBufferOffset:@
setTransformationMatrixBufferOffset :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> CULong -> IO ()
setTransformationMatrixBufferOffset mtlAccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setTransformationMatrixBufferOffset:") retVoid [argCULong (fromIntegral value)]

-- | Matrix layout for the transformation matrix in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- transformationMatrixLayout@
transformationMatrixLayout :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> IO MTLMatrixLayout
transformationMatrixLayout mtlAccelerationStructureMotionTriangleGeometryDescriptor  =
  fmap (coerce :: CLong -> MTLMatrixLayout) $ sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "transformationMatrixLayout") retCLong []

-- | Matrix layout for the transformation matrix in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- setTransformationMatrixLayout:@
setTransformationMatrixLayout :: IsMTLAccelerationStructureMotionTriangleGeometryDescriptor mtlAccelerationStructureMotionTriangleGeometryDescriptor => mtlAccelerationStructureMotionTriangleGeometryDescriptor -> MTLMatrixLayout -> IO ()
setTransformationMatrixLayout mtlAccelerationStructureMotionTriangleGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionTriangleGeometryDescriptor (mkSelector "setTransformationMatrixLayout:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

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

-- | @Selector@ for @indexBufferOffset@
indexBufferOffsetSelector :: Selector
indexBufferOffsetSelector = mkSelector "indexBufferOffset"

-- | @Selector@ for @setIndexBufferOffset:@
setIndexBufferOffsetSelector :: Selector
setIndexBufferOffsetSelector = mkSelector "setIndexBufferOffset:"

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

-- | @Selector@ for @transformationMatrixBufferOffset@
transformationMatrixBufferOffsetSelector :: Selector
transformationMatrixBufferOffsetSelector = mkSelector "transformationMatrixBufferOffset"

-- | @Selector@ for @setTransformationMatrixBufferOffset:@
setTransformationMatrixBufferOffsetSelector :: Selector
setTransformationMatrixBufferOffsetSelector = mkSelector "setTransformationMatrixBufferOffset:"

-- | @Selector@ for @transformationMatrixLayout@
transformationMatrixLayoutSelector :: Selector
transformationMatrixLayoutSelector = mkSelector "transformationMatrixLayout"

-- | @Selector@ for @setTransformationMatrixLayout:@
setTransformationMatrixLayoutSelector :: Selector
setTransformationMatrixLayoutSelector = mkSelector "setTransformationMatrixLayout:"

