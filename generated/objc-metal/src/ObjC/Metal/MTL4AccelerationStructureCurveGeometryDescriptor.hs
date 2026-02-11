{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes curve geometry suitable for ray tracing.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers this descriptor references when you build this acceleration structure.
--
-- Generated bindings for @MTL4AccelerationStructureCurveGeometryDescriptor@.
module ObjC.Metal.MTL4AccelerationStructureCurveGeometryDescriptor
  ( MTL4AccelerationStructureCurveGeometryDescriptor
  , IsMTL4AccelerationStructureCurveGeometryDescriptor(..)
  , controlPointBuffer
  , setControlPointBuffer
  , controlPointCount
  , setControlPointCount
  , controlPointStride
  , setControlPointStride
  , controlPointFormat
  , setControlPointFormat
  , radiusBuffer
  , setRadiusBuffer
  , radiusFormat
  , setRadiusFormat
  , radiusStride
  , setRadiusStride
  , indexBuffer
  , setIndexBuffer
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
  , controlPointBufferSelector
  , setControlPointBufferSelector
  , controlPointCountSelector
  , setControlPointCountSelector
  , controlPointStrideSelector
  , setControlPointStrideSelector
  , controlPointFormatSelector
  , setControlPointFormatSelector
  , radiusBufferSelector
  , setRadiusBufferSelector
  , radiusFormatSelector
  , setRadiusFormatSelector
  , radiusStrideSelector
  , setRadiusStrideSelector
  , indexBufferSelector
  , setIndexBufferSelector
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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Structs
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | References a buffer containing curve control points.
--
-- Control points are interpolated according to the basis function you specify in ``curveBasis``.
--
-- You are responsible for ensuring each control is in a format matching the control point format ``controlPointFormat`` specifies, as well as ensuring that the buffer address of the range is not zero.
--
-- ObjC selector: @- controlPointBuffer@
controlPointBuffer :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTL4BufferRange
controlPointBuffer mtL4AccelerationStructureCurveGeometryDescriptor  =
  sendMsgStret mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "controlPointBuffer") retMTL4BufferRange []

-- | References a buffer containing curve control points.
--
-- Control points are interpolated according to the basis function you specify in ``curveBasis``.
--
-- You are responsible for ensuring each control is in a format matching the control point format ``controlPointFormat`` specifies, as well as ensuring that the buffer address of the range is not zero.
--
-- ObjC selector: @- setControlPointBuffer:@
setControlPointBuffer :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTL4BufferRange -> IO ()
setControlPointBuffer mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setControlPointBuffer:") retVoid [argMTL4BufferRange value]

-- | Declares the number of control points in the control point buffer.
--
-- ObjC selector: @- controlPointCount@
controlPointCount :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO CULong
controlPointCount mtL4AccelerationStructureCurveGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "controlPointCount") retCULong []

-- | Declares the number of control points in the control point buffer.
--
-- ObjC selector: @- setControlPointCount:@
setControlPointCount :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setControlPointCount mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setControlPointCount:") retVoid [argCULong (fromIntegral value)]

-- | Sets the stride, in bytes, between control points in the control point buffer the control point buffer references.
--
-- You are responsible for ensuring this stride is a multiple of the control point format's element size, and at a minimum exactly the control point format's size.
--
-- This property defaults to @0@, indicating that the control points are tightly-packed.
--
-- ObjC selector: @- controlPointStride@
controlPointStride :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO CULong
controlPointStride mtL4AccelerationStructureCurveGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "controlPointStride") retCULong []

-- | Sets the stride, in bytes, between control points in the control point buffer the control point buffer references.
--
-- You are responsible for ensuring this stride is a multiple of the control point format's element size, and at a minimum exactly the control point format's size.
--
-- This property defaults to @0@, indicating that the control points are tightly-packed.
--
-- ObjC selector: @- setControlPointStride:@
setControlPointStride :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setControlPointStride mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setControlPointStride:") retVoid [argCULong (fromIntegral value)]

-- | Declares the format of the control points the control point buffer references.
--
-- Defaults to @MTLAttributeFormatFloat3@, representing 3 floating point values tightly packed.
--
-- ObjC selector: @- controlPointFormat@
controlPointFormat :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTLAttributeFormat
controlPointFormat mtL4AccelerationStructureCurveGeometryDescriptor  =
  fmap (coerce :: CULong -> MTLAttributeFormat) $ sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "controlPointFormat") retCULong []

-- | Declares the format of the control points the control point buffer references.
--
-- Defaults to @MTLAttributeFormatFloat3@, representing 3 floating point values tightly packed.
--
-- ObjC selector: @- setControlPointFormat:@
setControlPointFormat :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setControlPointFormat mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setControlPointFormat:") retVoid [argCULong (coerce value)]

-- | Assigns a reference to a buffer containing the curve radius for each control point.
--
-- Metal interpolates curve radii according to the basis function you specify via ``curveBasis``.
--
-- You are responsible for ensuring the type of each radius matches the type property ``radiusFormat`` specifies, that each radius is at least zero, and that the buffer address of the range is not zero.
--
-- ObjC selector: @- radiusBuffer@
radiusBuffer :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTL4BufferRange
radiusBuffer mtL4AccelerationStructureCurveGeometryDescriptor  =
  sendMsgStret mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "radiusBuffer") retMTL4BufferRange []

-- | Assigns a reference to a buffer containing the curve radius for each control point.
--
-- Metal interpolates curve radii according to the basis function you specify via ``curveBasis``.
--
-- You are responsible for ensuring the type of each radius matches the type property ``radiusFormat`` specifies, that each radius is at least zero, and that the buffer address of the range is not zero.
--
-- ObjC selector: @- setRadiusBuffer:@
setRadiusBuffer :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTL4BufferRange -> IO ()
setRadiusBuffer mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setRadiusBuffer:") retVoid [argMTL4BufferRange value]

-- | Declares the format of the radii in the radius buffer.
--
-- Defaults to  @MTLAttributeFormatFloat@.
--
-- ObjC selector: @- radiusFormat@
radiusFormat :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTLAttributeFormat
radiusFormat mtL4AccelerationStructureCurveGeometryDescriptor  =
  fmap (coerce :: CULong -> MTLAttributeFormat) $ sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "radiusFormat") retCULong []

-- | Declares the format of the radii in the radius buffer.
--
-- Defaults to  @MTLAttributeFormatFloat@.
--
-- ObjC selector: @- setRadiusFormat:@
setRadiusFormat :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setRadiusFormat mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setRadiusFormat:") retVoid [argCULong (coerce value)]

-- | Configures the stride, in bytes, between radii in the radius buffer.
--
-- You are responsible for ensuring this property is set to a multiple of the size corresponding to the ``radiusFormat``.
--
-- This property defaults to @0@ bytes, indicating that the radii are tightly packed.
--
-- ObjC selector: @- radiusStride@
radiusStride :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO CULong
radiusStride mtL4AccelerationStructureCurveGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "radiusStride") retCULong []

-- | Configures the stride, in bytes, between radii in the radius buffer.
--
-- You are responsible for ensuring this property is set to a multiple of the size corresponding to the ``radiusFormat``.
--
-- This property defaults to @0@ bytes, indicating that the radii are tightly packed.
--
-- ObjC selector: @- setRadiusStride:@
setRadiusStride :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setRadiusStride mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setRadiusStride:") retVoid [argCULong (fromIntegral value)]

-- | Assigns an optional index buffer containing references to control points in the control point buffer.
--
-- Each index represents the first control point of a curve segment. You are responsible for ensuring the buffer address of the range is not zero.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTL4BufferRange
indexBuffer mtL4AccelerationStructureCurveGeometryDescriptor  =
  sendMsgStret mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "indexBuffer") retMTL4BufferRange []

-- | Assigns an optional index buffer containing references to control points in the control point buffer.
--
-- Each index represents the first control point of a curve segment. You are responsible for ensuring the buffer address of the range is not zero.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTL4BufferRange -> IO ()
setIndexBuffer mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setIndexBuffer:") retVoid [argMTL4BufferRange value]

-- | Specifies the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- indexType@
indexType :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTLIndexType
indexType mtL4AccelerationStructureCurveGeometryDescriptor  =
  fmap (coerce :: CULong -> MTLIndexType) $ sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "indexType") retCULong []

-- | Specifies the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setIndexType:") retVoid [argCULong (coerce value)]

-- | Declares the number of curve segments.
--
-- ObjC selector: @- segmentCount@
segmentCount :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO CULong
segmentCount mtL4AccelerationStructureCurveGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "segmentCount") retCULong []

-- | Declares the number of curve segments.
--
-- ObjC selector: @- setSegmentCount:@
setSegmentCount :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setSegmentCount mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setSegmentCount:") retVoid [argCULong (fromIntegral value)]

-- | Declares the number of control points per curve segment.
--
-- Valid values for this property are @2@, @3@, or @4@.
--
-- ObjC selector: @- segmentControlPointCount@
segmentControlPointCount :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO CULong
segmentControlPointCount mtL4AccelerationStructureCurveGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "segmentControlPointCount") retCULong []

-- | Declares the number of control points per curve segment.
--
-- Valid values for this property are @2@, @3@, or @4@.
--
-- ObjC selector: @- setSegmentControlPointCount:@
setSegmentControlPointCount :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setSegmentControlPointCount mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setSegmentControlPointCount:") retVoid [argCULong (fromIntegral value)]

-- | Controls the curve type.
--
-- Defaults to @MTLCurveTypeRound@.
--
-- ObjC selector: @- curveType@
curveType :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTLCurveType
curveType mtL4AccelerationStructureCurveGeometryDescriptor  =
  fmap (coerce :: CLong -> MTLCurveType) $ sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "curveType") retCLong []

-- | Controls the curve type.
--
-- Defaults to @MTLCurveTypeRound@.
--
-- ObjC selector: @- setCurveType:@
setCurveType :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTLCurveType -> IO ()
setCurveType mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setCurveType:") retVoid [argCLong (coerce value)]

-- | Controls the curve basis function, determining how Metal interpolates the control points.
--
-- Defaults to @MTLCurveBasisBSpline@.
--
-- ObjC selector: @- curveBasis@
curveBasis :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTLCurveBasis
curveBasis mtL4AccelerationStructureCurveGeometryDescriptor  =
  fmap (coerce :: CLong -> MTLCurveBasis) $ sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "curveBasis") retCLong []

-- | Controls the curve basis function, determining how Metal interpolates the control points.
--
-- Defaults to @MTLCurveBasisBSpline@.
--
-- ObjC selector: @- setCurveBasis:@
setCurveBasis :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTLCurveBasis -> IO ()
setCurveBasis mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setCurveBasis:") retVoid [argCLong (coerce value)]

-- | Sets the type of curve end caps.
--
-- Defaults to @MTLCurveEndCapsNone@.
--
-- ObjC selector: @- curveEndCaps@
curveEndCaps :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTLCurveEndCaps
curveEndCaps mtL4AccelerationStructureCurveGeometryDescriptor  =
  fmap (coerce :: CLong -> MTLCurveEndCaps) $ sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "curveEndCaps") retCLong []

-- | Sets the type of curve end caps.
--
-- Defaults to @MTLCurveEndCapsNone@.
--
-- ObjC selector: @- setCurveEndCaps:@
setCurveEndCaps :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTLCurveEndCaps -> IO ()
setCurveEndCaps mtL4AccelerationStructureCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureCurveGeometryDescriptor (mkSelector "setCurveEndCaps:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @controlPointBuffer@
controlPointBufferSelector :: Selector
controlPointBufferSelector = mkSelector "controlPointBuffer"

-- | @Selector@ for @setControlPointBuffer:@
setControlPointBufferSelector :: Selector
setControlPointBufferSelector = mkSelector "setControlPointBuffer:"

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

