{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Acceleration structure motion geometry descriptor describing geometry made of curve primitives
--
-- Generated bindings for @MTLAccelerationStructureMotionCurveGeometryDescriptor@.
module ObjC.Metal.MTLAccelerationStructureMotionCurveGeometryDescriptor
  ( MTLAccelerationStructureMotionCurveGeometryDescriptor
  , IsMTLAccelerationStructureMotionCurveGeometryDescriptor(..)
  , descriptor
  , controlPointBuffers
  , setControlPointBuffers
  , controlPointCount
  , setControlPointCount
  , controlPointStride
  , setControlPointStride
  , controlPointFormat
  , setControlPointFormat
  , radiusBuffers
  , setRadiusBuffers
  , radiusFormat
  , setRadiusFormat
  , radiusStride
  , setRadiusStride
  , indexBuffer
  , setIndexBuffer
  , indexBufferOffset
  , setIndexBufferOffset
  , indexType
  , setIndexType
  , segmentCount
  , setSegmentCount
  , segmentControlPointCount
  , setSegmentControlPointCount
  , curveType
  , setCurveType
  , curveBasis
  , setCurveBasis
  , curveEndCaps
  , setCurveEndCaps
  , controlPointBuffersSelector
  , controlPointCountSelector
  , controlPointFormatSelector
  , controlPointStrideSelector
  , curveBasisSelector
  , curveEndCapsSelector
  , curveTypeSelector
  , descriptorSelector
  , indexBufferOffsetSelector
  , indexBufferSelector
  , indexTypeSelector
  , radiusBuffersSelector
  , radiusFormatSelector
  , radiusStrideSelector
  , segmentControlPointCountSelector
  , segmentCountSelector
  , setControlPointBuffersSelector
  , setControlPointCountSelector
  , setControlPointFormatSelector
  , setControlPointStrideSelector
  , setCurveBasisSelector
  , setCurveEndCapsSelector
  , setCurveTypeSelector
  , setIndexBufferOffsetSelector
  , setIndexBufferSelector
  , setIndexTypeSelector
  , setRadiusBuffersSelector
  , setRadiusFormatSelector
  , setRadiusStrideSelector
  , setSegmentControlPointCountSelector
  , setSegmentCountSelector

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
  , MTLCurveBasis(MTLCurveBasis)
  , pattern MTLCurveBasisBSpline
  , pattern MTLCurveBasisCatmullRom
  , pattern MTLCurveBasisLinear
  , pattern MTLCurveBasisBezier
  , MTLCurveEndCaps(MTLCurveEndCaps)
  , pattern MTLCurveEndCapsNone
  , pattern MTLCurveEndCapsDisk
  , pattern MTLCurveEndCapsSphere
  , MTLCurveType(MTLCurveType)
  , pattern MTLCurveTypeRound
  , pattern MTLCurveTypeFlat
  , MTLIndexType(MTLIndexType)
  , pattern MTLIndexTypeUInt16
  , pattern MTLIndexTypeUInt32

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
descriptor :: IO (Id MTLAccelerationStructureMotionCurveGeometryDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLAccelerationStructureMotionCurveGeometryDescriptor"
    sendClassMessage cls' descriptorSelector

-- | Buffers containing curve control points for each keyframe. Each control point must be of the format specified by the control point format. Buffer offsets musts be multiples of the control point format's element size and must be aligned to the platform's buffer offset alignment. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- controlPointBuffers@
controlPointBuffers :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO (Id NSArray)
controlPointBuffers mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor controlPointBuffersSelector

-- | Buffers containing curve control points for each keyframe. Each control point must be of the format specified by the control point format. Buffer offsets musts be multiples of the control point format's element size and must be aligned to the platform's buffer offset alignment. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- setControlPointBuffers:@
setControlPointBuffers :: (IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor, IsNSArray value) => mtlAccelerationStructureMotionCurveGeometryDescriptor -> value -> IO ()
setControlPointBuffers mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setControlPointBuffersSelector (toNSArray value)

-- | Number of control points in the control point buffers
--
-- ObjC selector: @- controlPointCount@
controlPointCount :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
controlPointCount mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor controlPointCountSelector

-- | Number of control points in the control point buffers
--
-- ObjC selector: @- setControlPointCount:@
setControlPointCount :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setControlPointCount mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setControlPointCountSelector value

-- | Stride, in bytes, between control points in the control point buffer. Must be a multiple of the control point format's element size and must be at least the control point format's size. Defaults to 0 bytes, indicating that the control points are tightly packed.
--
-- ObjC selector: @- controlPointStride@
controlPointStride :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
controlPointStride mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor controlPointStrideSelector

-- | Stride, in bytes, between control points in the control point buffer. Must be a multiple of the control point format's element size and must be at least the control point format's size. Defaults to 0 bytes, indicating that the control points are tightly packed.
--
-- ObjC selector: @- setControlPointStride:@
setControlPointStride :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setControlPointStride mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setControlPointStrideSelector value

-- | Format of the control points in the control point buffer. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- controlPointFormat@
controlPointFormat :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO MTLAttributeFormat
controlPointFormat mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor controlPointFormatSelector

-- | Format of the control points in the control point buffer. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- setControlPointFormat:@
setControlPointFormat :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setControlPointFormat mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setControlPointFormatSelector value

-- | Buffers containing the curve radius for each control point for each keyframe. Each radius must be of the type specified by the radius format. Buffer offsets must be multiples of the radius format size and must be aligned to the platform's buffer offset alignment. Each radius must be at least zero. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- radiusBuffers@
radiusBuffers :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO (Id NSArray)
radiusBuffers mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor radiusBuffersSelector

-- | Buffers containing the curve radius for each control point for each keyframe. Each radius must be of the type specified by the radius format. Buffer offsets must be multiples of the radius format size and must be aligned to the platform's buffer offset alignment. Each radius must be at least zero. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- setRadiusBuffers:@
setRadiusBuffers :: (IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor, IsNSArray value) => mtlAccelerationStructureMotionCurveGeometryDescriptor -> value -> IO ()
setRadiusBuffers mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setRadiusBuffersSelector (toNSArray value)

-- | Format of the radii in the radius buffer. Defaults to  MTLAttributeFormatFloat.
--
-- ObjC selector: @- radiusFormat@
radiusFormat :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO MTLAttributeFormat
radiusFormat mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor radiusFormatSelector

-- | Format of the radii in the radius buffer. Defaults to  MTLAttributeFormatFloat.
--
-- ObjC selector: @- setRadiusFormat:@
setRadiusFormat :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setRadiusFormat mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setRadiusFormatSelector value

-- | Stride, in bytes, between radii in the radius buffer. Must be a multiple of 4 bytes. Defaults to 4 bytes.
--
-- ObjC selector: @- radiusStride@
radiusStride :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
radiusStride mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor radiusStrideSelector

-- | Stride, in bytes, between radii in the radius buffer. Must be a multiple of 4 bytes. Defaults to 4 bytes.
--
-- ObjC selector: @- setRadiusStride:@
setRadiusStride :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setRadiusStride mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setRadiusStrideSelector value

-- | Index buffer containing references to control points in the control point buffer. Must not be nil.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO RawId
indexBuffer mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor indexBufferSelector

-- | Index buffer containing references to control points in the control point buffer. Must not be nil.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> RawId -> IO ()
setIndexBuffer mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setIndexBufferSelector value

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- indexBufferOffset@
indexBufferOffset :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
indexBufferOffset mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor indexBufferOffsetSelector

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- setIndexBufferOffset:@
setIndexBufferOffset :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setIndexBufferOffset mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setIndexBufferOffsetSelector value

-- | Index type
--
-- ObjC selector: @- indexType@
indexType :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO MTLIndexType
indexType mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor indexTypeSelector

-- | Index type
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setIndexTypeSelector value

-- | Number of curve segments
--
-- ObjC selector: @- segmentCount@
segmentCount :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
segmentCount mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor segmentCountSelector

-- | Number of curve segments
--
-- ObjC selector: @- setSegmentCount:@
setSegmentCount :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setSegmentCount mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setSegmentCountSelector value

-- | Number of control points per curve segment. Must be 2, 3, or 4.
--
-- ObjC selector: @- segmentControlPointCount@
segmentControlPointCount :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
segmentControlPointCount mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor segmentControlPointCountSelector

-- | Number of control points per curve segment. Must be 2, 3, or 4.
--
-- ObjC selector: @- setSegmentControlPointCount:@
setSegmentControlPointCount :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setSegmentControlPointCount mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setSegmentControlPointCountSelector value

-- | Curve type. Defaults to MTLCurveTypeRound.
--
-- ObjC selector: @- curveType@
curveType :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO MTLCurveType
curveType mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor curveTypeSelector

-- | Curve type. Defaults to MTLCurveTypeRound.
--
-- ObjC selector: @- setCurveType:@
setCurveType :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> MTLCurveType -> IO ()
setCurveType mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setCurveTypeSelector value

-- | Curve basis. Defaults to MTLCurveBasisBSpline.
--
-- ObjC selector: @- curveBasis@
curveBasis :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO MTLCurveBasis
curveBasis mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor curveBasisSelector

-- | Curve basis. Defaults to MTLCurveBasisBSpline.
--
-- ObjC selector: @- setCurveBasis:@
setCurveBasis :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> MTLCurveBasis -> IO ()
setCurveBasis mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setCurveBasisSelector value

-- | Type of curve end caps. Defaults to MTLCurveEndCapsNone.
--
-- ObjC selector: @- curveEndCaps@
curveEndCaps :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO MTLCurveEndCaps
curveEndCaps mtlAccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor curveEndCapsSelector

-- | Type of curve end caps. Defaults to MTLCurveEndCapsNone.
--
-- ObjC selector: @- setCurveEndCaps:@
setCurveEndCaps :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> MTLCurveEndCaps -> IO ()
setCurveEndCaps mtlAccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionCurveGeometryDescriptor setCurveEndCapsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MTLAccelerationStructureMotionCurveGeometryDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @controlPointBuffers@
controlPointBuffersSelector :: Selector '[] (Id NSArray)
controlPointBuffersSelector = mkSelector "controlPointBuffers"

-- | @Selector@ for @setControlPointBuffers:@
setControlPointBuffersSelector :: Selector '[Id NSArray] ()
setControlPointBuffersSelector = mkSelector "setControlPointBuffers:"

-- | @Selector@ for @controlPointCount@
controlPointCountSelector :: Selector '[] CULong
controlPointCountSelector = mkSelector "controlPointCount"

-- | @Selector@ for @setControlPointCount:@
setControlPointCountSelector :: Selector '[CULong] ()
setControlPointCountSelector = mkSelector "setControlPointCount:"

-- | @Selector@ for @controlPointStride@
controlPointStrideSelector :: Selector '[] CULong
controlPointStrideSelector = mkSelector "controlPointStride"

-- | @Selector@ for @setControlPointStride:@
setControlPointStrideSelector :: Selector '[CULong] ()
setControlPointStrideSelector = mkSelector "setControlPointStride:"

-- | @Selector@ for @controlPointFormat@
controlPointFormatSelector :: Selector '[] MTLAttributeFormat
controlPointFormatSelector = mkSelector "controlPointFormat"

-- | @Selector@ for @setControlPointFormat:@
setControlPointFormatSelector :: Selector '[MTLAttributeFormat] ()
setControlPointFormatSelector = mkSelector "setControlPointFormat:"

-- | @Selector@ for @radiusBuffers@
radiusBuffersSelector :: Selector '[] (Id NSArray)
radiusBuffersSelector = mkSelector "radiusBuffers"

-- | @Selector@ for @setRadiusBuffers:@
setRadiusBuffersSelector :: Selector '[Id NSArray] ()
setRadiusBuffersSelector = mkSelector "setRadiusBuffers:"

-- | @Selector@ for @radiusFormat@
radiusFormatSelector :: Selector '[] MTLAttributeFormat
radiusFormatSelector = mkSelector "radiusFormat"

-- | @Selector@ for @setRadiusFormat:@
setRadiusFormatSelector :: Selector '[MTLAttributeFormat] ()
setRadiusFormatSelector = mkSelector "setRadiusFormat:"

-- | @Selector@ for @radiusStride@
radiusStrideSelector :: Selector '[] CULong
radiusStrideSelector = mkSelector "radiusStride"

-- | @Selector@ for @setRadiusStride:@
setRadiusStrideSelector :: Selector '[CULong] ()
setRadiusStrideSelector = mkSelector "setRadiusStride:"

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

-- | @Selector@ for @segmentCount@
segmentCountSelector :: Selector '[] CULong
segmentCountSelector = mkSelector "segmentCount"

-- | @Selector@ for @setSegmentCount:@
setSegmentCountSelector :: Selector '[CULong] ()
setSegmentCountSelector = mkSelector "setSegmentCount:"

-- | @Selector@ for @segmentControlPointCount@
segmentControlPointCountSelector :: Selector '[] CULong
segmentControlPointCountSelector = mkSelector "segmentControlPointCount"

-- | @Selector@ for @setSegmentControlPointCount:@
setSegmentControlPointCountSelector :: Selector '[CULong] ()
setSegmentControlPointCountSelector = mkSelector "setSegmentControlPointCount:"

-- | @Selector@ for @curveType@
curveTypeSelector :: Selector '[] MTLCurveType
curveTypeSelector = mkSelector "curveType"

-- | @Selector@ for @setCurveType:@
setCurveTypeSelector :: Selector '[MTLCurveType] ()
setCurveTypeSelector = mkSelector "setCurveType:"

-- | @Selector@ for @curveBasis@
curveBasisSelector :: Selector '[] MTLCurveBasis
curveBasisSelector = mkSelector "curveBasis"

-- | @Selector@ for @setCurveBasis:@
setCurveBasisSelector :: Selector '[MTLCurveBasis] ()
setCurveBasisSelector = mkSelector "setCurveBasis:"

-- | @Selector@ for @curveEndCaps@
curveEndCapsSelector :: Selector '[] MTLCurveEndCaps
curveEndCapsSelector = mkSelector "curveEndCaps"

-- | @Selector@ for @setCurveEndCaps:@
setCurveEndCapsSelector :: Selector '[MTLCurveEndCaps] ()
setCurveEndCapsSelector = mkSelector "setCurveEndCaps:"

