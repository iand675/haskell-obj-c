{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes motion curve geometry, suitable for motion ray tracing.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers this descriptor references when you build this acceleration structure.
--
-- Generated bindings for @MTL4AccelerationStructureMotionCurveGeometryDescriptor@.
module ObjC.Metal.MTL4AccelerationStructureMotionCurveGeometryDescriptor
  ( MTL4AccelerationStructureMotionCurveGeometryDescriptor
  , IsMTL4AccelerationStructureMotionCurveGeometryDescriptor(..)
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
import ObjC.Metal.Internal.Structs
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Assigns a reference to a buffer where each entry contains a reference to a buffer of control points.
--
-- This property references a buffer that conceptually represents an array with one entry for each keyframe in the motion animation. Each one of these entries consists of a ``MTL4BufferRange`` that, in turn, references a buffer containing the control points corresponding to the keyframe.
--
-- You are responsible for ensuring the buffer address is not zero for the top-level buffer, as well as for all the vertex buffers it references.
--
-- ObjC selector: @- controlPointBuffers@
controlPointBuffers :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTL4BufferRange
controlPointBuffers mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor controlPointBuffersSelector

-- | Assigns a reference to a buffer where each entry contains a reference to a buffer of control points.
--
-- This property references a buffer that conceptually represents an array with one entry for each keyframe in the motion animation. Each one of these entries consists of a ``MTL4BufferRange`` that, in turn, references a buffer containing the control points corresponding to the keyframe.
--
-- You are responsible for ensuring the buffer address is not zero for the top-level buffer, as well as for all the vertex buffers it references.
--
-- ObjC selector: @- setControlPointBuffers:@
setControlPointBuffers :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTL4BufferRange -> IO ()
setControlPointBuffers mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setControlPointBuffersSelector value

-- | Specifies the number of control points in the buffers the control point buffers reference.
--
-- All keyframes have the same number of control points.
--
-- ObjC selector: @- controlPointCount@
controlPointCount :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
controlPointCount mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor controlPointCountSelector

-- | Specifies the number of control points in the buffers the control point buffers reference.
--
-- All keyframes have the same number of control points.
--
-- ObjC selector: @- setControlPointCount:@
setControlPointCount :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setControlPointCount mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setControlPointCountSelector value

-- | Sets the stride, in bytes, between control points in the control point buffer.
--
-- All keyframes share the same control point stride.
--
-- You are responsible for ensuring this stride is a multiple of the control point format's element size, and at a minimum exactly the control point format's size.
--
-- This property defaults to @0@, indicating that the control points are tightly-packed.
--
-- ObjC selector: @- controlPointStride@
controlPointStride :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
controlPointStride mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor controlPointStrideSelector

-- | Sets the stride, in bytes, between control points in the control point buffer.
--
-- All keyframes share the same control point stride.
--
-- You are responsible for ensuring this stride is a multiple of the control point format's element size, and at a minimum exactly the control point format's size.
--
-- This property defaults to @0@, indicating that the control points are tightly-packed.
--
-- ObjC selector: @- setControlPointStride:@
setControlPointStride :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setControlPointStride mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setControlPointStrideSelector value

-- | Declares the format of the control points in the buffers that the control point buffers reference.
--
-- All keyframes share the same control point format. Defaults to @MTLAttributeFormatFloat3@, representing 3 floating point values tightly packed.
--
-- ObjC selector: @- controlPointFormat@
controlPointFormat :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTLAttributeFormat
controlPointFormat mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor controlPointFormatSelector

-- | Declares the format of the control points in the buffers that the control point buffers reference.
--
-- All keyframes share the same control point format. Defaults to @MTLAttributeFormatFloat3@, representing 3 floating point values tightly packed.
--
-- ObjC selector: @- setControlPointFormat:@
setControlPointFormat :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setControlPointFormat mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setControlPointFormatSelector value

-- | Assigns a reference to a buffer containing, in turn, references to curve radii buffers.
--
-- This property references a buffer that conceptually represents an array with one entry for each keyframe in the motion animation. Each one of these entries consists of a ``MTL4BufferRange`` that, in turn, references a buffer containing the radii corresponding to the keyframe.
--
-- Metal interpolates curve radii according to the basis function you specify via ``curveBasis``.
--
-- You are responsible for ensuring the type of each radius matches the type property ``radiusFormat`` specifies, that each radius is at least zero, and that the buffer address of the top-level buffer, as well as of buffer it references, is not zero.
--
-- ObjC selector: @- radiusBuffers@
radiusBuffers :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTL4BufferRange
radiusBuffers mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor radiusBuffersSelector

-- | Assigns a reference to a buffer containing, in turn, references to curve radii buffers.
--
-- This property references a buffer that conceptually represents an array with one entry for each keyframe in the motion animation. Each one of these entries consists of a ``MTL4BufferRange`` that, in turn, references a buffer containing the radii corresponding to the keyframe.
--
-- Metal interpolates curve radii according to the basis function you specify via ``curveBasis``.
--
-- You are responsible for ensuring the type of each radius matches the type property ``radiusFormat`` specifies, that each radius is at least zero, and that the buffer address of the top-level buffer, as well as of buffer it references, is not zero.
--
-- ObjC selector: @- setRadiusBuffers:@
setRadiusBuffers :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTL4BufferRange -> IO ()
setRadiusBuffers mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setRadiusBuffersSelector value

-- | Sets the format of the radii in the radius buffer.
--
-- Defaults to  @MTLAttributeFormatFloat@. All keyframes share the same radius format.
--
-- ObjC selector: @- radiusFormat@
radiusFormat :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTLAttributeFormat
radiusFormat mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor radiusFormatSelector

-- | Sets the format of the radii in the radius buffer.
--
-- Defaults to  @MTLAttributeFormatFloat@. All keyframes share the same radius format.
--
-- ObjC selector: @- setRadiusFormat:@
setRadiusFormat :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setRadiusFormat mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setRadiusFormatSelector value

-- | Sets the stride, in bytes, between radii in the radius buffer.
--
-- You are responsible for ensuring this property is set to a multiple of the size corresponding to the ``radiusFormat``. All keyframes share the same radius stride.
--
-- This property defaults to @0@ bytes, indicating that the radii are tightly packed.
--
-- ObjC selector: @- radiusStride@
radiusStride :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
radiusStride mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor radiusStrideSelector

-- | Sets the stride, in bytes, between radii in the radius buffer.
--
-- You are responsible for ensuring this property is set to a multiple of the size corresponding to the ``radiusFormat``. All keyframes share the same radius stride.
--
-- This property defaults to @0@ bytes, indicating that the radii are tightly packed.
--
-- ObjC selector: @- setRadiusStride:@
setRadiusStride :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setRadiusStride mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setRadiusStrideSelector value

-- | Assigns an optional index buffer containing references to control points in the control point buffers.
--
-- All keyframes share the same index buffer, with each index representing the first control point of a curve segment.
--
-- You are responsible for ensuring the buffer address of the range is not zero.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTL4BufferRange
indexBuffer mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor indexBufferSelector

-- | Assigns an optional index buffer containing references to control points in the control point buffers.
--
-- All keyframes share the same index buffer, with each index representing the first control point of a curve segment.
--
-- You are responsible for ensuring the buffer address of the range is not zero.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTL4BufferRange -> IO ()
setIndexBuffer mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setIndexBufferSelector value

-- | Configures the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- indexType@
indexType :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTLIndexType
indexType mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor indexTypeSelector

-- | Configures the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setIndexTypeSelector value

-- | Declares the number of curve segments.
--
-- All keyframes have the same number of curve segments.
--
-- ObjC selector: @- segmentCount@
segmentCount :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
segmentCount mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor segmentCountSelector

-- | Declares the number of curve segments.
--
-- All keyframes have the same number of curve segments.
--
-- ObjC selector: @- setSegmentCount:@
setSegmentCount :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setSegmentCount mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setSegmentCountSelector value

-- | Controls the number of control points per curve segment.
--
-- Valid values for this property are @2@, @3@, or @4@. All keyframes have the same number of control points per curve segment.
--
-- ObjC selector: @- segmentControlPointCount@
segmentControlPointCount :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
segmentControlPointCount mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor segmentControlPointCountSelector

-- | Controls the number of control points per curve segment.
--
-- Valid values for this property are @2@, @3@, or @4@. All keyframes have the same number of control points per curve segment.
--
-- ObjC selector: @- setSegmentControlPointCount:@
setSegmentControlPointCount :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setSegmentControlPointCount mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setSegmentControlPointCountSelector value

-- | Controls the curve type.
--
-- Defaults to @MTLCurveTypeRound@. All keyframes share the same curve type.
--
-- ObjC selector: @- curveType@
curveType :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTLCurveType
curveType mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor curveTypeSelector

-- | Controls the curve type.
--
-- Defaults to @MTLCurveTypeRound@. All keyframes share the same curve type.
--
-- ObjC selector: @- setCurveType:@
setCurveType :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTLCurveType -> IO ()
setCurveType mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setCurveTypeSelector value

-- | Sets the curve basis function, determining how Metal interpolates the control points.
--
-- Defaults to @MTLCurveBasisBSpline@. All keyframes share the same curve basis function.
--
-- ObjC selector: @- curveBasis@
curveBasis :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTLCurveBasis
curveBasis mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor curveBasisSelector

-- | Sets the curve basis function, determining how Metal interpolates the control points.
--
-- Defaults to @MTLCurveBasisBSpline@. All keyframes share the same curve basis function.
--
-- ObjC selector: @- setCurveBasis:@
setCurveBasis :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTLCurveBasis -> IO ()
setCurveBasis mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setCurveBasisSelector value

-- | Configures the type of curve end caps.
--
-- Defaults to @MTLCurveEndCapsNone@. All keyframes share the same end cap type.
--
-- ObjC selector: @- curveEndCaps@
curveEndCaps :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTLCurveEndCaps
curveEndCaps mtL4AccelerationStructureMotionCurveGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor curveEndCapsSelector

-- | Configures the type of curve end caps.
--
-- Defaults to @MTLCurveEndCapsNone@. All keyframes share the same end cap type.
--
-- ObjC selector: @- setCurveEndCaps:@
setCurveEndCaps :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTLCurveEndCaps -> IO ()
setCurveEndCaps mtL4AccelerationStructureMotionCurveGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionCurveGeometryDescriptor setCurveEndCapsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @controlPointBuffers@
controlPointBuffersSelector :: Selector '[] MTL4BufferRange
controlPointBuffersSelector = mkSelector "controlPointBuffers"

-- | @Selector@ for @setControlPointBuffers:@
setControlPointBuffersSelector :: Selector '[MTL4BufferRange] ()
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
radiusBuffersSelector :: Selector '[] MTL4BufferRange
radiusBuffersSelector = mkSelector "radiusBuffers"

-- | @Selector@ for @setRadiusBuffers:@
setRadiusBuffersSelector :: Selector '[MTL4BufferRange] ()
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

