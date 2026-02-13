{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , controlPointCountSelector
  , controlPointFormatSelector
  , controlPointStrideSelector
  , curveBasisSelector
  , curveEndCapsSelector
  , curveTypeSelector
  , indexBufferSelector
  , indexTypeSelector
  , radiusBufferSelector
  , radiusFormatSelector
  , radiusStrideSelector
  , segmentControlPointCountSelector
  , segmentCountSelector
  , setControlPointBufferSelector
  , setControlPointCountSelector
  , setControlPointFormatSelector
  , setControlPointStrideSelector
  , setCurveBasisSelector
  , setCurveEndCapsSelector
  , setCurveTypeSelector
  , setIndexBufferSelector
  , setIndexTypeSelector
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
controlPointBuffer mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor controlPointBufferSelector

-- | References a buffer containing curve control points.
--
-- Control points are interpolated according to the basis function you specify in ``curveBasis``.
--
-- You are responsible for ensuring each control is in a format matching the control point format ``controlPointFormat`` specifies, as well as ensuring that the buffer address of the range is not zero.
--
-- ObjC selector: @- setControlPointBuffer:@
setControlPointBuffer :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTL4BufferRange -> IO ()
setControlPointBuffer mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setControlPointBufferSelector value

-- | Declares the number of control points in the control point buffer.
--
-- ObjC selector: @- controlPointCount@
controlPointCount :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO CULong
controlPointCount mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor controlPointCountSelector

-- | Declares the number of control points in the control point buffer.
--
-- ObjC selector: @- setControlPointCount:@
setControlPointCount :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setControlPointCount mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setControlPointCountSelector value

-- | Sets the stride, in bytes, between control points in the control point buffer the control point buffer references.
--
-- You are responsible for ensuring this stride is a multiple of the control point format's element size, and at a minimum exactly the control point format's size.
--
-- This property defaults to @0@, indicating that the control points are tightly-packed.
--
-- ObjC selector: @- controlPointStride@
controlPointStride :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO CULong
controlPointStride mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor controlPointStrideSelector

-- | Sets the stride, in bytes, between control points in the control point buffer the control point buffer references.
--
-- You are responsible for ensuring this stride is a multiple of the control point format's element size, and at a minimum exactly the control point format's size.
--
-- This property defaults to @0@, indicating that the control points are tightly-packed.
--
-- ObjC selector: @- setControlPointStride:@
setControlPointStride :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setControlPointStride mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setControlPointStrideSelector value

-- | Declares the format of the control points the control point buffer references.
--
-- Defaults to @MTLAttributeFormatFloat3@, representing 3 floating point values tightly packed.
--
-- ObjC selector: @- controlPointFormat@
controlPointFormat :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTLAttributeFormat
controlPointFormat mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor controlPointFormatSelector

-- | Declares the format of the control points the control point buffer references.
--
-- Defaults to @MTLAttributeFormatFloat3@, representing 3 floating point values tightly packed.
--
-- ObjC selector: @- setControlPointFormat:@
setControlPointFormat :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setControlPointFormat mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setControlPointFormatSelector value

-- | Assigns a reference to a buffer containing the curve radius for each control point.
--
-- Metal interpolates curve radii according to the basis function you specify via ``curveBasis``.
--
-- You are responsible for ensuring the type of each radius matches the type property ``radiusFormat`` specifies, that each radius is at least zero, and that the buffer address of the range is not zero.
--
-- ObjC selector: @- radiusBuffer@
radiusBuffer :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTL4BufferRange
radiusBuffer mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor radiusBufferSelector

-- | Assigns a reference to a buffer containing the curve radius for each control point.
--
-- Metal interpolates curve radii according to the basis function you specify via ``curveBasis``.
--
-- You are responsible for ensuring the type of each radius matches the type property ``radiusFormat`` specifies, that each radius is at least zero, and that the buffer address of the range is not zero.
--
-- ObjC selector: @- setRadiusBuffer:@
setRadiusBuffer :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTL4BufferRange -> IO ()
setRadiusBuffer mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setRadiusBufferSelector value

-- | Declares the format of the radii in the radius buffer.
--
-- Defaults to  @MTLAttributeFormatFloat@.
--
-- ObjC selector: @- radiusFormat@
radiusFormat :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTLAttributeFormat
radiusFormat mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor radiusFormatSelector

-- | Declares the format of the radii in the radius buffer.
--
-- Defaults to  @MTLAttributeFormatFloat@.
--
-- ObjC selector: @- setRadiusFormat:@
setRadiusFormat :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setRadiusFormat mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setRadiusFormatSelector value

-- | Configures the stride, in bytes, between radii in the radius buffer.
--
-- You are responsible for ensuring this property is set to a multiple of the size corresponding to the ``radiusFormat``.
--
-- This property defaults to @0@ bytes, indicating that the radii are tightly packed.
--
-- ObjC selector: @- radiusStride@
radiusStride :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO CULong
radiusStride mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor radiusStrideSelector

-- | Configures the stride, in bytes, between radii in the radius buffer.
--
-- You are responsible for ensuring this property is set to a multiple of the size corresponding to the ``radiusFormat``.
--
-- This property defaults to @0@ bytes, indicating that the radii are tightly packed.
--
-- ObjC selector: @- setRadiusStride:@
setRadiusStride :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setRadiusStride mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setRadiusStrideSelector value

-- | Assigns an optional index buffer containing references to control points in the control point buffer.
--
-- Each index represents the first control point of a curve segment. You are responsible for ensuring the buffer address of the range is not zero.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTL4BufferRange
indexBuffer mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor indexBufferSelector

-- | Assigns an optional index buffer containing references to control points in the control point buffer.
--
-- Each index represents the first control point of a curve segment. You are responsible for ensuring the buffer address of the range is not zero.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTL4BufferRange -> IO ()
setIndexBuffer mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setIndexBufferSelector value

-- | Specifies the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- indexType@
indexType :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTLIndexType
indexType mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor indexTypeSelector

-- | Specifies the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setIndexTypeSelector value

-- | Declares the number of curve segments.
--
-- ObjC selector: @- segmentCount@
segmentCount :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO CULong
segmentCount mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor segmentCountSelector

-- | Declares the number of curve segments.
--
-- ObjC selector: @- setSegmentCount:@
setSegmentCount :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setSegmentCount mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setSegmentCountSelector value

-- | Declares the number of control points per curve segment.
--
-- Valid values for this property are @2@, @3@, or @4@.
--
-- ObjC selector: @- segmentControlPointCount@
segmentControlPointCount :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO CULong
segmentControlPointCount mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor segmentControlPointCountSelector

-- | Declares the number of control points per curve segment.
--
-- Valid values for this property are @2@, @3@, or @4@.
--
-- ObjC selector: @- setSegmentControlPointCount:@
setSegmentControlPointCount :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> CULong -> IO ()
setSegmentControlPointCount mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setSegmentControlPointCountSelector value

-- | Controls the curve type.
--
-- Defaults to @MTLCurveTypeRound@.
--
-- ObjC selector: @- curveType@
curveType :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTLCurveType
curveType mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor curveTypeSelector

-- | Controls the curve type.
--
-- Defaults to @MTLCurveTypeRound@.
--
-- ObjC selector: @- setCurveType:@
setCurveType :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTLCurveType -> IO ()
setCurveType mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setCurveTypeSelector value

-- | Controls the curve basis function, determining how Metal interpolates the control points.
--
-- Defaults to @MTLCurveBasisBSpline@.
--
-- ObjC selector: @- curveBasis@
curveBasis :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTLCurveBasis
curveBasis mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor curveBasisSelector

-- | Controls the curve basis function, determining how Metal interpolates the control points.
--
-- Defaults to @MTLCurveBasisBSpline@.
--
-- ObjC selector: @- setCurveBasis:@
setCurveBasis :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTLCurveBasis -> IO ()
setCurveBasis mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setCurveBasisSelector value

-- | Sets the type of curve end caps.
--
-- Defaults to @MTLCurveEndCapsNone@.
--
-- ObjC selector: @- curveEndCaps@
curveEndCaps :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> IO MTLCurveEndCaps
curveEndCaps mtL4AccelerationStructureCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor curveEndCapsSelector

-- | Sets the type of curve end caps.
--
-- Defaults to @MTLCurveEndCapsNone@.
--
-- ObjC selector: @- setCurveEndCaps:@
setCurveEndCaps :: IsMTL4AccelerationStructureCurveGeometryDescriptor mtL4AccelerationStructureCurveGeometryDescriptor => mtL4AccelerationStructureCurveGeometryDescriptor -> MTLCurveEndCaps -> IO ()
setCurveEndCaps mtL4AccelerationStructureCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureCurveGeometryDescriptor setCurveEndCapsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @controlPointBuffer@
controlPointBufferSelector :: Selector '[] MTL4BufferRange
controlPointBufferSelector = mkSelector "controlPointBuffer"

-- | @Selector@ for @setControlPointBuffer:@
setControlPointBufferSelector :: Selector '[MTL4BufferRange] ()
setControlPointBufferSelector = mkSelector "setControlPointBuffer:"

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
radiusBufferSelector :: Selector '[] MTL4BufferRange
radiusBufferSelector = mkSelector "radiusBuffer"

-- | @Selector@ for @setRadiusBuffer:@
setRadiusBufferSelector :: Selector '[MTL4BufferRange] ()
setRadiusBufferSelector = mkSelector "setRadiusBuffer:"

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

