{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Acceleration structure geometry descriptor describing geometry made of curve primitives
--
-- Generated bindings for @MTLAccelerationStructureCurveGeometryDescriptor@.
module ObjC.Metal.MTLAccelerationStructureCurveGeometryDescriptor
  ( MTLAccelerationStructureCurveGeometryDescriptor
  , IsMTLAccelerationStructureCurveGeometryDescriptor(..)
  , descriptor
  , controlPointBuffer
  , setControlPointBuffer
  , controlPointBufferOffset
  , setControlPointBufferOffset
  , controlPointCount
  , setControlPointCount
  , controlPointStride
  , setControlPointStride
  , controlPointFormat
  , setControlPointFormat
  , radiusBuffer
  , setRadiusBuffer
  , radiusBufferOffset
  , setRadiusBufferOffset
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
  , controlPointBufferOffsetSelector
  , controlPointBufferSelector
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
  , radiusBufferOffsetSelector
  , radiusBufferSelector
  , radiusFormatSelector
  , radiusStrideSelector
  , segmentControlPointCountSelector
  , segmentCountSelector
  , setControlPointBufferOffsetSelector
  , setControlPointBufferSelector
  , setControlPointCountSelector
  , setControlPointFormatSelector
  , setControlPointStrideSelector
  , setCurveBasisSelector
  , setCurveEndCapsSelector
  , setCurveTypeSelector
  , setIndexBufferOffsetSelector
  , setIndexBufferSelector
  , setIndexTypeSelector
  , setRadiusBufferOffsetSelector
  , setRadiusBufferSelector
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
descriptor :: IO (Id MTLAccelerationStructureCurveGeometryDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLAccelerationStructureCurveGeometryDescriptor"
    sendClassMessage cls' descriptorSelector

-- | Buffer containing curve control points. Each control point must be of the format specified by the control point format. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- controlPointBuffer@
controlPointBuffer :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO RawId
controlPointBuffer mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor controlPointBufferSelector

-- | Buffer containing curve control points. Each control point must be of the format specified by the control point format. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- setControlPointBuffer:@
setControlPointBuffer :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> RawId -> IO ()
setControlPointBuffer mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setControlPointBufferSelector value

-- | Control point buffer offset. Must be a multiple of the control point format's element size and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- controlPointBufferOffset@
controlPointBufferOffset :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
controlPointBufferOffset mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor controlPointBufferOffsetSelector

-- | Control point buffer offset. Must be a multiple of the control point format's element size and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setControlPointBufferOffset:@
setControlPointBufferOffset :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setControlPointBufferOffset mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setControlPointBufferOffsetSelector value

-- | Number of control points in the control point buffer
--
-- ObjC selector: @- controlPointCount@
controlPointCount :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
controlPointCount mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor controlPointCountSelector

-- | Number of control points in the control point buffer
--
-- ObjC selector: @- setControlPointCount:@
setControlPointCount :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setControlPointCount mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setControlPointCountSelector value

-- | Stride, in bytes, between control points in the control point buffer. Must be a multiple of the control point format's element size and must be at least the control point format's size. Defaults to 0 bytes, indicating that the control points are tightly packed.
--
-- ObjC selector: @- controlPointStride@
controlPointStride :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
controlPointStride mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor controlPointStrideSelector

-- | Stride, in bytes, between control points in the control point buffer. Must be a multiple of the control point format's element size and must be at least the control point format's size. Defaults to 0 bytes, indicating that the control points are tightly packed.
--
-- ObjC selector: @- setControlPointStride:@
setControlPointStride :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setControlPointStride mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setControlPointStrideSelector value

-- | Format of the control points in the control point buffer. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- controlPointFormat@
controlPointFormat :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO MTLAttributeFormat
controlPointFormat mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor controlPointFormatSelector

-- | Format of the control points in the control point buffer. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- setControlPointFormat:@
setControlPointFormat :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setControlPointFormat mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setControlPointFormatSelector value

-- | Buffer containing the curve radius for each control point. Each radius must be of the type specified by the radius format. Each radius must be at least zero. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- radiusBuffer@
radiusBuffer :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO RawId
radiusBuffer mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor radiusBufferSelector

-- | Buffer containing the curve radius for each control point. Each radius must be of the type specified by the radius format. Each radius must be at least zero. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- setRadiusBuffer:@
setRadiusBuffer :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> RawId -> IO ()
setRadiusBuffer mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setRadiusBufferSelector value

-- | Radius buffer offset. Must be a multiple of the radius format size and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- radiusBufferOffset@
radiusBufferOffset :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
radiusBufferOffset mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor radiusBufferOffsetSelector

-- | Radius buffer offset. Must be a multiple of the radius format size and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setRadiusBufferOffset:@
setRadiusBufferOffset :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setRadiusBufferOffset mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setRadiusBufferOffsetSelector value

-- | Format of the radii in the radius buffer. Defaults to  MTLAttributeFormatFloat.
--
-- ObjC selector: @- radiusFormat@
radiusFormat :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO MTLAttributeFormat
radiusFormat mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor radiusFormatSelector

-- | Format of the radii in the radius buffer. Defaults to  MTLAttributeFormatFloat.
--
-- ObjC selector: @- setRadiusFormat:@
setRadiusFormat :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setRadiusFormat mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setRadiusFormatSelector value

-- | Stride, in bytes, between radii in the radius buffer. Must be a multiple of the radius format size. Defaults to 0 bytes, indicating that the radii are tightly packed.
--
-- ObjC selector: @- radiusStride@
radiusStride :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
radiusStride mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor radiusStrideSelector

-- | Stride, in bytes, between radii in the radius buffer. Must be a multiple of the radius format size. Defaults to 0 bytes, indicating that the radii are tightly packed.
--
-- ObjC selector: @- setRadiusStride:@
setRadiusStride :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setRadiusStride mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setRadiusStrideSelector value

-- | Index buffer containing references to control points in the control point buffer. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO RawId
indexBuffer mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor indexBufferSelector

-- | Index buffer containing references to control points in the control point buffer. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> RawId -> IO ()
setIndexBuffer mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setIndexBufferSelector value

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- indexBufferOffset@
indexBufferOffset :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
indexBufferOffset mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor indexBufferOffsetSelector

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- setIndexBufferOffset:@
setIndexBufferOffset :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setIndexBufferOffset mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setIndexBufferOffsetSelector value

-- | Index type
--
-- ObjC selector: @- indexType@
indexType :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO MTLIndexType
indexType mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor indexTypeSelector

-- | Index type
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setIndexTypeSelector value

-- | Number of curve segments
--
-- ObjC selector: @- segmentCount@
segmentCount :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
segmentCount mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor segmentCountSelector

-- | Number of curve segments
--
-- ObjC selector: @- setSegmentCount:@
setSegmentCount :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setSegmentCount mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setSegmentCountSelector value

-- | Number of control points per curve segment. Must be 2, 3, or 4.
--
-- ObjC selector: @- segmentControlPointCount@
segmentControlPointCount :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
segmentControlPointCount mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor segmentControlPointCountSelector

-- | Number of control points per curve segment. Must be 2, 3, or 4.
--
-- ObjC selector: @- setSegmentControlPointCount:@
setSegmentControlPointCount :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setSegmentControlPointCount mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setSegmentControlPointCountSelector value

-- | Curve type. Defaults to MTLCurveTypeRound.
--
-- ObjC selector: @- curveType@
curveType :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO MTLCurveType
curveType mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor curveTypeSelector

-- | Curve type. Defaults to MTLCurveTypeRound.
--
-- ObjC selector: @- setCurveType:@
setCurveType :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> MTLCurveType -> IO ()
setCurveType mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setCurveTypeSelector value

-- | Curve basis. Defaults to MTLCurveBasisBSpline.
--
-- ObjC selector: @- curveBasis@
curveBasis :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO MTLCurveBasis
curveBasis mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor curveBasisSelector

-- | Curve basis. Defaults to MTLCurveBasisBSpline.
--
-- ObjC selector: @- setCurveBasis:@
setCurveBasis :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> MTLCurveBasis -> IO ()
setCurveBasis mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setCurveBasisSelector value

-- | Type of curve end caps. Defaults to MTLCurveEndCapsNone.
--
-- ObjC selector: @- curveEndCaps@
curveEndCaps :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO MTLCurveEndCaps
curveEndCaps mtlAccelerationStructureCurveGeometryDescriptor =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor curveEndCapsSelector

-- | Type of curve end caps. Defaults to MTLCurveEndCapsNone.
--
-- ObjC selector: @- setCurveEndCaps:@
setCurveEndCaps :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> MTLCurveEndCaps -> IO ()
setCurveEndCaps mtlAccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtlAccelerationStructureCurveGeometryDescriptor setCurveEndCapsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MTLAccelerationStructureCurveGeometryDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @controlPointBuffer@
controlPointBufferSelector :: Selector '[] RawId
controlPointBufferSelector = mkSelector "controlPointBuffer"

-- | @Selector@ for @setControlPointBuffer:@
setControlPointBufferSelector :: Selector '[RawId] ()
setControlPointBufferSelector = mkSelector "setControlPointBuffer:"

-- | @Selector@ for @controlPointBufferOffset@
controlPointBufferOffsetSelector :: Selector '[] CULong
controlPointBufferOffsetSelector = mkSelector "controlPointBufferOffset"

-- | @Selector@ for @setControlPointBufferOffset:@
setControlPointBufferOffsetSelector :: Selector '[CULong] ()
setControlPointBufferOffsetSelector = mkSelector "setControlPointBufferOffset:"

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

-- | @Selector@ for @radiusBuffer@
radiusBufferSelector :: Selector '[] RawId
radiusBufferSelector = mkSelector "radiusBuffer"

-- | @Selector@ for @setRadiusBuffer:@
setRadiusBufferSelector :: Selector '[RawId] ()
setRadiusBufferSelector = mkSelector "setRadiusBuffer:"

-- | @Selector@ for @radiusBufferOffset@
radiusBufferOffsetSelector :: Selector '[] CULong
radiusBufferOffsetSelector = mkSelector "radiusBufferOffset"

-- | @Selector@ for @setRadiusBufferOffset:@
setRadiusBufferOffsetSelector :: Selector '[CULong] ()
setRadiusBufferOffsetSelector = mkSelector "setRadiusBufferOffset:"

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

