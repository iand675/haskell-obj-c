{-# LANGUAGE PatternSynonyms #-}
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
  , descriptorSelector
  , controlPointBufferSelector
  , setControlPointBufferSelector
  , controlPointBufferOffsetSelector
  , setControlPointBufferOffsetSelector
  , controlPointCountSelector
  , setControlPointCountSelector
  , controlPointStrideSelector
  , setControlPointStrideSelector
  , controlPointFormatSelector
  , setControlPointFormatSelector
  , radiusBufferSelector
  , setRadiusBufferSelector
  , radiusBufferOffsetSelector
  , setRadiusBufferOffsetSelector
  , radiusFormatSelector
  , setRadiusFormatSelector
  , radiusStrideSelector
  , setRadiusStrideSelector
  , indexBufferSelector
  , setIndexBufferSelector
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
descriptor :: IO (Id MTLAccelerationStructureCurveGeometryDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLAccelerationStructureCurveGeometryDescriptor"
    sendClassMsg cls' (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Buffer containing curve control points. Each control point must be of the format specified by the control point format. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- controlPointBuffer@
controlPointBuffer :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO RawId
controlPointBuffer mtlAccelerationStructureCurveGeometryDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "controlPointBuffer") (retPtr retVoid) []

-- | Buffer containing curve control points. Each control point must be of the format specified by the control point format. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- setControlPointBuffer:@
setControlPointBuffer :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> RawId -> IO ()
setControlPointBuffer mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setControlPointBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Control point buffer offset. Must be a multiple of the control point format's element size and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- controlPointBufferOffset@
controlPointBufferOffset :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
controlPointBufferOffset mtlAccelerationStructureCurveGeometryDescriptor  =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "controlPointBufferOffset") retCULong []

-- | Control point buffer offset. Must be a multiple of the control point format's element size and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setControlPointBufferOffset:@
setControlPointBufferOffset :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setControlPointBufferOffset mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setControlPointBufferOffset:") retVoid [argCULong value]

-- | Number of control points in the control point buffer
--
-- ObjC selector: @- controlPointCount@
controlPointCount :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
controlPointCount mtlAccelerationStructureCurveGeometryDescriptor  =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "controlPointCount") retCULong []

-- | Number of control points in the control point buffer
--
-- ObjC selector: @- setControlPointCount:@
setControlPointCount :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setControlPointCount mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setControlPointCount:") retVoid [argCULong value]

-- | Stride, in bytes, between control points in the control point buffer. Must be a multiple of the control point format's element size and must be at least the control point format's size. Defaults to 0 bytes, indicating that the control points are tightly packed.
--
-- ObjC selector: @- controlPointStride@
controlPointStride :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
controlPointStride mtlAccelerationStructureCurveGeometryDescriptor  =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "controlPointStride") retCULong []

-- | Stride, in bytes, between control points in the control point buffer. Must be a multiple of the control point format's element size and must be at least the control point format's size. Defaults to 0 bytes, indicating that the control points are tightly packed.
--
-- ObjC selector: @- setControlPointStride:@
setControlPointStride :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setControlPointStride mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setControlPointStride:") retVoid [argCULong value]

-- | Format of the control points in the control point buffer. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- controlPointFormat@
controlPointFormat :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO MTLAttributeFormat
controlPointFormat mtlAccelerationStructureCurveGeometryDescriptor  =
    fmap (coerce :: CULong -> MTLAttributeFormat) $ sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "controlPointFormat") retCULong []

-- | Format of the control points in the control point buffer. Defaults to MTLAttributeFormatFloat3 (packed).
--
-- ObjC selector: @- setControlPointFormat:@
setControlPointFormat :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setControlPointFormat mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setControlPointFormat:") retVoid [argCULong (coerce value)]

-- | Buffer containing the curve radius for each control point. Each radius must be of the type specified by the radius format. Each radius must be at least zero. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- radiusBuffer@
radiusBuffer :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO RawId
radiusBuffer mtlAccelerationStructureCurveGeometryDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "radiusBuffer") (retPtr retVoid) []

-- | Buffer containing the curve radius for each control point. Each radius must be of the type specified by the radius format. Each radius must be at least zero. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- setRadiusBuffer:@
setRadiusBuffer :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> RawId -> IO ()
setRadiusBuffer mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setRadiusBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Radius buffer offset. Must be a multiple of the radius format size and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- radiusBufferOffset@
radiusBufferOffset :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
radiusBufferOffset mtlAccelerationStructureCurveGeometryDescriptor  =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "radiusBufferOffset") retCULong []

-- | Radius buffer offset. Must be a multiple of the radius format size and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setRadiusBufferOffset:@
setRadiusBufferOffset :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setRadiusBufferOffset mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setRadiusBufferOffset:") retVoid [argCULong value]

-- | Format of the radii in the radius buffer. Defaults to  MTLAttributeFormatFloat.
--
-- ObjC selector: @- radiusFormat@
radiusFormat :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO MTLAttributeFormat
radiusFormat mtlAccelerationStructureCurveGeometryDescriptor  =
    fmap (coerce :: CULong -> MTLAttributeFormat) $ sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "radiusFormat") retCULong []

-- | Format of the radii in the radius buffer. Defaults to  MTLAttributeFormatFloat.
--
-- ObjC selector: @- setRadiusFormat:@
setRadiusFormat :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setRadiusFormat mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setRadiusFormat:") retVoid [argCULong (coerce value)]

-- | Stride, in bytes, between radii in the radius buffer. Must be a multiple of the radius format size. Defaults to 0 bytes, indicating that the radii are tightly packed.
--
-- ObjC selector: @- radiusStride@
radiusStride :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
radiusStride mtlAccelerationStructureCurveGeometryDescriptor  =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "radiusStride") retCULong []

-- | Stride, in bytes, between radii in the radius buffer. Must be a multiple of the radius format size. Defaults to 0 bytes, indicating that the radii are tightly packed.
--
-- ObjC selector: @- setRadiusStride:@
setRadiusStride :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setRadiusStride mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setRadiusStride:") retVoid [argCULong value]

-- | Index buffer containing references to control points in the control point buffer. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO RawId
indexBuffer mtlAccelerationStructureCurveGeometryDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "indexBuffer") (retPtr retVoid) []

-- | Index buffer containing references to control points in the control point buffer. Must not be nil when the acceleration structure is built.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> RawId -> IO ()
setIndexBuffer mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setIndexBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- indexBufferOffset@
indexBufferOffset :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
indexBufferOffset mtlAccelerationStructureCurveGeometryDescriptor  =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "indexBufferOffset") retCULong []

-- | Index buffer offset. Must be a multiple of the index data type size and must be aligned to both the index data type's alignment and the platform's buffer offset alignment.
--
-- ObjC selector: @- setIndexBufferOffset:@
setIndexBufferOffset :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setIndexBufferOffset mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setIndexBufferOffset:") retVoid [argCULong value]

-- | Index type
--
-- ObjC selector: @- indexType@
indexType :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO MTLIndexType
indexType mtlAccelerationStructureCurveGeometryDescriptor  =
    fmap (coerce :: CULong -> MTLIndexType) $ sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "indexType") retCULong []

-- | Index type
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setIndexType:") retVoid [argCULong (coerce value)]

-- | Number of curve segments
--
-- ObjC selector: @- segmentCount@
segmentCount :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
segmentCount mtlAccelerationStructureCurveGeometryDescriptor  =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "segmentCount") retCULong []

-- | Number of curve segments
--
-- ObjC selector: @- setSegmentCount:@
setSegmentCount :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setSegmentCount mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setSegmentCount:") retVoid [argCULong value]

-- | Number of control points per curve segment. Must be 2, 3, or 4.
--
-- ObjC selector: @- segmentControlPointCount@
segmentControlPointCount :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO CULong
segmentControlPointCount mtlAccelerationStructureCurveGeometryDescriptor  =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "segmentControlPointCount") retCULong []

-- | Number of control points per curve segment. Must be 2, 3, or 4.
--
-- ObjC selector: @- setSegmentControlPointCount:@
setSegmentControlPointCount :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setSegmentControlPointCount mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setSegmentControlPointCount:") retVoid [argCULong value]

-- | Curve type. Defaults to MTLCurveTypeRound.
--
-- ObjC selector: @- curveType@
curveType :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO MTLCurveType
curveType mtlAccelerationStructureCurveGeometryDescriptor  =
    fmap (coerce :: CLong -> MTLCurveType) $ sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "curveType") retCLong []

-- | Curve type. Defaults to MTLCurveTypeRound.
--
-- ObjC selector: @- setCurveType:@
setCurveType :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> MTLCurveType -> IO ()
setCurveType mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setCurveType:") retVoid [argCLong (coerce value)]

-- | Curve basis. Defaults to MTLCurveBasisBSpline.
--
-- ObjC selector: @- curveBasis@
curveBasis :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO MTLCurveBasis
curveBasis mtlAccelerationStructureCurveGeometryDescriptor  =
    fmap (coerce :: CLong -> MTLCurveBasis) $ sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "curveBasis") retCLong []

-- | Curve basis. Defaults to MTLCurveBasisBSpline.
--
-- ObjC selector: @- setCurveBasis:@
setCurveBasis :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> MTLCurveBasis -> IO ()
setCurveBasis mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setCurveBasis:") retVoid [argCLong (coerce value)]

-- | Type of curve end caps. Defaults to MTLCurveEndCapsNone.
--
-- ObjC selector: @- curveEndCaps@
curveEndCaps :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> IO MTLCurveEndCaps
curveEndCaps mtlAccelerationStructureCurveGeometryDescriptor  =
    fmap (coerce :: CLong -> MTLCurveEndCaps) $ sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "curveEndCaps") retCLong []

-- | Type of curve end caps. Defaults to MTLCurveEndCapsNone.
--
-- ObjC selector: @- setCurveEndCaps:@
setCurveEndCaps :: IsMTLAccelerationStructureCurveGeometryDescriptor mtlAccelerationStructureCurveGeometryDescriptor => mtlAccelerationStructureCurveGeometryDescriptor -> MTLCurveEndCaps -> IO ()
setCurveEndCaps mtlAccelerationStructureCurveGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureCurveGeometryDescriptor (mkSelector "setCurveEndCaps:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @controlPointBuffer@
controlPointBufferSelector :: Selector
controlPointBufferSelector = mkSelector "controlPointBuffer"

-- | @Selector@ for @setControlPointBuffer:@
setControlPointBufferSelector :: Selector
setControlPointBufferSelector = mkSelector "setControlPointBuffer:"

-- | @Selector@ for @controlPointBufferOffset@
controlPointBufferOffsetSelector :: Selector
controlPointBufferOffsetSelector = mkSelector "controlPointBufferOffset"

-- | @Selector@ for @setControlPointBufferOffset:@
setControlPointBufferOffsetSelector :: Selector
setControlPointBufferOffsetSelector = mkSelector "setControlPointBufferOffset:"

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

-- | @Selector@ for @radiusBuffer@
radiusBufferSelector :: Selector
radiusBufferSelector = mkSelector "radiusBuffer"

-- | @Selector@ for @setRadiusBuffer:@
setRadiusBufferSelector :: Selector
setRadiusBufferSelector = mkSelector "setRadiusBuffer:"

-- | @Selector@ for @radiusBufferOffset@
radiusBufferOffsetSelector :: Selector
radiusBufferOffsetSelector = mkSelector "radiusBufferOffset"

-- | @Selector@ for @setRadiusBufferOffset:@
setRadiusBufferOffsetSelector :: Selector
setRadiusBufferOffsetSelector = mkSelector "setRadiusBufferOffset:"

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

-- | @Selector@ for @indexBuffer@
indexBufferSelector :: Selector
indexBufferSelector = mkSelector "indexBuffer"

-- | @Selector@ for @setIndexBuffer:@
setIndexBufferSelector :: Selector
setIndexBufferSelector = mkSelector "setIndexBuffer:"

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

