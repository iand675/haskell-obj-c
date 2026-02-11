{-# LANGUAGE PatternSynonyms #-}
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
  , descriptorSelector
  , controlPointBuffersSelector
  , setControlPointBuffersSelector
  , controlPointCountSelector
  , setControlPointCountSelector
  , controlPointStrideSelector
  , setControlPointStrideSelector
  , controlPointFormatSelector
  , setControlPointFormatSelector
  , radiusBuffersSelector
  , setRadiusBuffersSelector
  , radiusFormatSelector
  , setRadiusFormatSelector
  , radiusStrideSelector
  , setRadiusStrideSelector
  , indexBufferOffsetSelector
  , setIndexBufferOffsetSelector
  , indexTypeSelector
  , setIndexTypeSelector
  , segmentCountSelector
  , setSegmentCountSelector
  , segmentControlPointCountSelector
  , setSegmentControlPointCountSelector
  , curveTypeSelector
  , setCurveTypeSelector
  , curveBasisSelector
  , setCurveBasisSelector
  , curveEndCapsSelector
  , setCurveEndCapsSelector

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
descriptor :: IO (Id MTLAccelerationStructureMotionCurveGeometryDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLAccelerationStructureMotionCurveGeometryDescriptor"
    sendClassMsg cls' (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Buffers containing curve control points for each keyframe. Each control point must be of the format specified by the control point format. Buffer offsets musts be multiples of the control point format's element size and must be aligned to the platform's buffer offset alignment. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- controlPointBuffers@
controlPointBuffers :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO (Id NSArray)
controlPointBuffers mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "controlPointBuffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Buffers containing curve control points for each keyframe. Each control point must be of the format specified by the control point format. Buffer offsets musts be multiples of the control point format's element size and must be aligned to the platform's buffer offset alignment. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- setControlPointBuffers:@
setControlPointBuffers :: (IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor, IsNSArray value) => mtlAccelerationStructureMotionCurveGeometryDescriptor -> value -> IO ()
setControlPointBuffers mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setControlPointBuffers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Number of control points in the control point buffers
--
-- ObjC selector: @- controlPointCount@
controlPointCount :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
controlPointCount mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "controlPointCount") retCULong []

-- | Number of control points in the control point buffers
--
-- ObjC selector: @- setControlPointCount:@
setControlPointCount :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setControlPointCount mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setControlPointCount:") retVoid [argCULong (fromIntegral value)]

-- | Stride, in bytes, between control points in the control point buffer. Must be a multiple of the control point format's element size and must be at least the control point format's size. Defaults to 0 bytes, indicating that the control points are tightly packed.
--
-- ObjC selector: @- controlPointStride@
controlPointStride :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
controlPointStride mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "controlPointStride") retCULong []

-- | Stride, in bytes, between control points in the control point buffer. Must be a multiple of the control point format's element size and must be at least the control point format's size. Defaults to 0 bytes, indicating that the control points are tightly packed.
--
-- ObjC selector: @- setControlPointStride:@
setControlPointStride :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setControlPointStride mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setControlPointStride:") retVoid [argCULong (fromIntegral value)]

-- | Format of the control points in the control point buffer. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- controlPointFormat@
controlPointFormat :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO MTLAttributeFormat
controlPointFormat mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  fmap (coerce :: CULong -> MTLAttributeFormat) $ sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "controlPointFormat") retCULong []

-- | Format of the control points in the control point buffer. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- setControlPointFormat:@
setControlPointFormat :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setControlPointFormat mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setControlPointFormat:") retVoid [argCULong (coerce value)]

-- | Buffers containing the curve radius for each control point for each keyframe. Each radius must be of the type specified by the radius format. Buffer offsets must be multiples of the radius format size and must be aligned to the platform's buffer offset alignment. Each radius must be at least zero. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- radiusBuffers@
radiusBuffers :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO (Id NSArray)
radiusBuffers mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "radiusBuffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Buffers containing the curve radius for each control point for each keyframe. Each radius must be of the type specified by the radius format. Buffer offsets must be multiples of the radius format size and must be aligned to the platform's buffer offset alignment. Each radius must be at least zero. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- setRadiusBuffers:@
setRadiusBuffers :: (IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor, IsNSArray value) => mtlAccelerationStructureMotionCurveGeometryDescriptor -> value -> IO ()
setRadiusBuffers mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setRadiusBuffers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Format of the radii in the radius buffer. Defaults to  MTLAttributeFormatFloat.
--
-- ObjC selector: @- radiusFormat@
radiusFormat :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO MTLAttributeFormat
radiusFormat mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  fmap (coerce :: CULong -> MTLAttributeFormat) $ sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "radiusFormat") retCULong []

-- | Format of the radii in the radius buffer. Defaults to  MTLAttributeFormatFloat.
--
-- ObjC selector: @- setRadiusFormat:@
setRadiusFormat :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setRadiusFormat mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setRadiusFormat:") retVoid [argCULong (coerce value)]

-- | Stride, in bytes, between radii in the radius buffer. Must be a multiple of 4 bytes. Defaults to 4 bytes.
--
-- ObjC selector: @- radiusStride@
radiusStride :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
radiusStride mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "radiusStride") retCULong []

-- | Stride, in bytes, between radii in the radius buffer. Must be a multiple of 4 bytes. Defaults to 4 bytes.
--
-- ObjC selector: @- setRadiusStride:@
setRadiusStride :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setRadiusStride mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setRadiusStride:") retVoid [argCULong (fromIntegral value)]

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- indexBufferOffset@
indexBufferOffset :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
indexBufferOffset mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "indexBufferOffset") retCULong []

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- setIndexBufferOffset:@
setIndexBufferOffset :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setIndexBufferOffset mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setIndexBufferOffset:") retVoid [argCULong (fromIntegral value)]

-- | Index type
--
-- ObjC selector: @- indexType@
indexType :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO MTLIndexType
indexType mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  fmap (coerce :: CULong -> MTLIndexType) $ sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "indexType") retCULong []

-- | Index type
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setIndexType:") retVoid [argCULong (coerce value)]

-- | Number of curve segments
--
-- ObjC selector: @- segmentCount@
segmentCount :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
segmentCount mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "segmentCount") retCULong []

-- | Number of curve segments
--
-- ObjC selector: @- setSegmentCount:@
setSegmentCount :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setSegmentCount mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setSegmentCount:") retVoid [argCULong (fromIntegral value)]

-- | Number of control points per curve segment. Must be 2, 3, or 4.
--
-- ObjC selector: @- segmentControlPointCount@
segmentControlPointCount :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
segmentControlPointCount mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "segmentControlPointCount") retCULong []

-- | Number of control points per curve segment. Must be 2, 3, or 4.
--
-- ObjC selector: @- setSegmentControlPointCount:@
setSegmentControlPointCount :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setSegmentControlPointCount mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setSegmentControlPointCount:") retVoid [argCULong (fromIntegral value)]

-- | Curve type. Defaults to MTLCurveTypeRound.
--
-- ObjC selector: @- curveType@
curveType :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO MTLCurveType
curveType mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  fmap (coerce :: CLong -> MTLCurveType) $ sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "curveType") retCLong []

-- | Curve type. Defaults to MTLCurveTypeRound.
--
-- ObjC selector: @- setCurveType:@
setCurveType :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> MTLCurveType -> IO ()
setCurveType mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setCurveType:") retVoid [argCLong (coerce value)]

-- | Curve basis. Defaults to MTLCurveBasisBSpline.
--
-- ObjC selector: @- curveBasis@
curveBasis :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO MTLCurveBasis
curveBasis mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  fmap (coerce :: CLong -> MTLCurveBasis) $ sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "curveBasis") retCLong []

-- | Curve basis. Defaults to MTLCurveBasisBSpline.
--
-- ObjC selector: @- setCurveBasis:@
setCurveBasis :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> MTLCurveBasis -> IO ()
setCurveBasis mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setCurveBasis:") retVoid [argCLong (coerce value)]

-- | Type of curve end caps. Defaults to MTLCurveEndCapsNone.
--
-- ObjC selector: @- curveEndCaps@
curveEndCaps :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> IO MTLCurveEndCaps
curveEndCaps mtlAccelerationStructureMotionCurveGeometryDescriptor  =
  fmap (coerce :: CLong -> MTLCurveEndCaps) $ sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "curveEndCaps") retCLong []

-- | Type of curve end caps. Defaults to MTLCurveEndCapsNone.
--
-- ObjC selector: @- setCurveEndCaps:@
setCurveEndCaps :: IsMTLAccelerationStructureMotionCurveGeometryDescriptor mtlAccelerationStructureMotionCurveGeometryDescriptor => mtlAccelerationStructureMotionCurveGeometryDescriptor -> MTLCurveEndCaps -> IO ()
setCurveEndCaps mtlAccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setCurveEndCaps:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @controlPointBuffers@
controlPointBuffersSelector :: Selector
controlPointBuffersSelector = mkSelector "controlPointBuffers"

-- | @Selector@ for @setControlPointBuffers:@
setControlPointBuffersSelector :: Selector
setControlPointBuffersSelector = mkSelector "setControlPointBuffers:"

-- | @Selector@ for @controlPointCount@
controlPointCountSelector :: Selector
controlPointCountSelector = mkSelector "controlPointCount"

-- | @Selector@ for @setControlPointCount:@
setControlPointCountSelector :: Selector
setControlPointCountSelector = mkSelector "setControlPointCount:"

-- | @Selector@ for @controlPointStride@
controlPointStrideSelector :: Selector
controlPointStrideSelector = mkSelector "controlPointStride"

-- | @Selector@ for @setControlPointStride:@
setControlPointStrideSelector :: Selector
setControlPointStrideSelector = mkSelector "setControlPointStride:"

-- | @Selector@ for @controlPointFormat@
controlPointFormatSelector :: Selector
controlPointFormatSelector = mkSelector "controlPointFormat"

-- | @Selector@ for @setControlPointFormat:@
setControlPointFormatSelector :: Selector
setControlPointFormatSelector = mkSelector "setControlPointFormat:"

-- | @Selector@ for @radiusBuffers@
radiusBuffersSelector :: Selector
radiusBuffersSelector = mkSelector "radiusBuffers"

-- | @Selector@ for @setRadiusBuffers:@
setRadiusBuffersSelector :: Selector
setRadiusBuffersSelector = mkSelector "setRadiusBuffers:"

-- | @Selector@ for @radiusFormat@
radiusFormatSelector :: Selector
radiusFormatSelector = mkSelector "radiusFormat"

-- | @Selector@ for @setRadiusFormat:@
setRadiusFormatSelector :: Selector
setRadiusFormatSelector = mkSelector "setRadiusFormat:"

-- | @Selector@ for @radiusStride@
radiusStrideSelector :: Selector
radiusStrideSelector = mkSelector "radiusStride"

-- | @Selector@ for @setRadiusStride:@
setRadiusStrideSelector :: Selector
setRadiusStrideSelector = mkSelector "setRadiusStride:"

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

-- | @Selector@ for @segmentCount@
segmentCountSelector :: Selector
segmentCountSelector = mkSelector "segmentCount"

-- | @Selector@ for @setSegmentCount:@
setSegmentCountSelector :: Selector
setSegmentCountSelector = mkSelector "setSegmentCount:"

-- | @Selector@ for @segmentControlPointCount@
segmentControlPointCountSelector :: Selector
segmentControlPointCountSelector = mkSelector "segmentControlPointCount"

-- | @Selector@ for @setSegmentControlPointCount:@
setSegmentControlPointCountSelector :: Selector
setSegmentControlPointCountSelector = mkSelector "setSegmentControlPointCount:"

-- | @Selector@ for @curveType@
curveTypeSelector :: Selector
curveTypeSelector = mkSelector "curveType"

-- | @Selector@ for @setCurveType:@
setCurveTypeSelector :: Selector
setCurveTypeSelector = mkSelector "setCurveType:"

-- | @Selector@ for @curveBasis@
curveBasisSelector :: Selector
curveBasisSelector = mkSelector "curveBasis"

-- | @Selector@ for @setCurveBasis:@
setCurveBasisSelector :: Selector
setCurveBasisSelector = mkSelector "setCurveBasis:"

-- | @Selector@ for @curveEndCaps@
curveEndCapsSelector :: Selector
curveEndCapsSelector = mkSelector "curveEndCaps"

-- | @Selector@ for @setCurveEndCaps:@
setCurveEndCapsSelector :: Selector
setCurveEndCapsSelector = mkSelector "setCurveEndCaps:"

