{-# LANGUAGE PatternSynonyms #-}
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

-- | Assigns a reference to a buffer where each entry contains a reference to a buffer of control points.
--
-- This property references a buffer that conceptually represents an array with one entry for each keyframe in the motion animation. Each one of these entries consists of a ``MTL4BufferRange`` that, in turn, references a buffer containing the control points corresponding to the keyframe.
--
-- You are responsible for ensuring the buffer address is not zero for the top-level buffer, as well as for all the vertex buffers it references.
--
-- ObjC selector: @- controlPointBuffers@
controlPointBuffers :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTL4BufferRange
controlPointBuffers mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsgStret mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "controlPointBuffers") retMTL4BufferRange []

-- | Assigns a reference to a buffer where each entry contains a reference to a buffer of control points.
--
-- This property references a buffer that conceptually represents an array with one entry for each keyframe in the motion animation. Each one of these entries consists of a ``MTL4BufferRange`` that, in turn, references a buffer containing the control points corresponding to the keyframe.
--
-- You are responsible for ensuring the buffer address is not zero for the top-level buffer, as well as for all the vertex buffers it references.
--
-- ObjC selector: @- setControlPointBuffers:@
setControlPointBuffers :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTL4BufferRange -> IO ()
setControlPointBuffers mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setControlPointBuffers:") retVoid [argMTL4BufferRange value]

-- | Specifies the number of control points in the buffers the control point buffers reference.
--
-- All keyframes have the same number of control points.
--
-- ObjC selector: @- controlPointCount@
controlPointCount :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
controlPointCount mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "controlPointCount") retCULong []

-- | Specifies the number of control points in the buffers the control point buffers reference.
--
-- All keyframes have the same number of control points.
--
-- ObjC selector: @- setControlPointCount:@
setControlPointCount :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setControlPointCount mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setControlPointCount:") retVoid [argCULong (fromIntegral value)]

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
controlPointStride mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "controlPointStride") retCULong []

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
setControlPointStride mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setControlPointStride:") retVoid [argCULong (fromIntegral value)]

-- | Declares the format of the control points in the buffers that the control point buffers reference.
--
-- All keyframes share the same control point format. Defaults to @MTLAttributeFormatFloat3@, representing 3 floating point values tightly packed.
--
-- ObjC selector: @- controlPointFormat@
controlPointFormat :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTLAttributeFormat
controlPointFormat mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  fmap (coerce :: CULong -> MTLAttributeFormat) $ sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "controlPointFormat") retCULong []

-- | Declares the format of the control points in the buffers that the control point buffers reference.
--
-- All keyframes share the same control point format. Defaults to @MTLAttributeFormatFloat3@, representing 3 floating point values tightly packed.
--
-- ObjC selector: @- setControlPointFormat:@
setControlPointFormat :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setControlPointFormat mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setControlPointFormat:") retVoid [argCULong (coerce value)]

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
radiusBuffers mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsgStret mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "radiusBuffers") retMTL4BufferRange []

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
setRadiusBuffers mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setRadiusBuffers:") retVoid [argMTL4BufferRange value]

-- | Sets the format of the radii in the radius buffer.
--
-- Defaults to  @MTLAttributeFormatFloat@. All keyframes share the same radius format.
--
-- ObjC selector: @- radiusFormat@
radiusFormat :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTLAttributeFormat
radiusFormat mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  fmap (coerce :: CULong -> MTLAttributeFormat) $ sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "radiusFormat") retCULong []

-- | Sets the format of the radii in the radius buffer.
--
-- Defaults to  @MTLAttributeFormatFloat@. All keyframes share the same radius format.
--
-- ObjC selector: @- setRadiusFormat:@
setRadiusFormat :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTLAttributeFormat -> IO ()
setRadiusFormat mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setRadiusFormat:") retVoid [argCULong (coerce value)]

-- | Sets the stride, in bytes, between radii in the radius buffer.
--
-- You are responsible for ensuring this property is set to a multiple of the size corresponding to the ``radiusFormat``. All keyframes share the same radius stride.
--
-- This property defaults to @0@ bytes, indicating that the radii are tightly packed.
--
-- ObjC selector: @- radiusStride@
radiusStride :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
radiusStride mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "radiusStride") retCULong []

-- | Sets the stride, in bytes, between radii in the radius buffer.
--
-- You are responsible for ensuring this property is set to a multiple of the size corresponding to the ``radiusFormat``. All keyframes share the same radius stride.
--
-- This property defaults to @0@ bytes, indicating that the radii are tightly packed.
--
-- ObjC selector: @- setRadiusStride:@
setRadiusStride :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setRadiusStride mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setRadiusStride:") retVoid [argCULong (fromIntegral value)]

-- | Assigns an optional index buffer containing references to control points in the control point buffers.
--
-- All keyframes share the same index buffer, with each index representing the first control point of a curve segment.
--
-- You are responsible for ensuring the buffer address of the range is not zero.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTL4BufferRange
indexBuffer mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsgStret mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "indexBuffer") retMTL4BufferRange []

-- | Assigns an optional index buffer containing references to control points in the control point buffers.
--
-- All keyframes share the same index buffer, with each index representing the first control point of a curve segment.
--
-- You are responsible for ensuring the buffer address of the range is not zero.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTL4BufferRange -> IO ()
setIndexBuffer mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setIndexBuffer:") retVoid [argMTL4BufferRange value]

-- | Configures the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- indexType@
indexType :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTLIndexType
indexType mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  fmap (coerce :: CULong -> MTLIndexType) $ sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "indexType") retCULong []

-- | Configures the size of the indices the @indexBuffer@ contains, which is typically either 16 or 32-bits for each index.
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTLIndexType -> IO ()
setIndexType mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setIndexType:") retVoid [argCULong (coerce value)]

-- | Declares the number of curve segments.
--
-- All keyframes have the same number of curve segments.
--
-- ObjC selector: @- segmentCount@
segmentCount :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
segmentCount mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "segmentCount") retCULong []

-- | Declares the number of curve segments.
--
-- All keyframes have the same number of curve segments.
--
-- ObjC selector: @- setSegmentCount:@
setSegmentCount :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setSegmentCount mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setSegmentCount:") retVoid [argCULong (fromIntegral value)]

-- | Controls the number of control points per curve segment.
--
-- Valid values for this property are @2@, @3@, or @4@. All keyframes have the same number of control points per curve segment.
--
-- ObjC selector: @- segmentControlPointCount@
segmentControlPointCount :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO CULong
segmentControlPointCount mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "segmentControlPointCount") retCULong []

-- | Controls the number of control points per curve segment.
--
-- Valid values for this property are @2@, @3@, or @4@. All keyframes have the same number of control points per curve segment.
--
-- ObjC selector: @- setSegmentControlPointCount:@
setSegmentControlPointCount :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> CULong -> IO ()
setSegmentControlPointCount mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setSegmentControlPointCount:") retVoid [argCULong (fromIntegral value)]

-- | Controls the curve type.
--
-- Defaults to @MTLCurveTypeRound@. All keyframes share the same curve type.
--
-- ObjC selector: @- curveType@
curveType :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTLCurveType
curveType mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  fmap (coerce :: CLong -> MTLCurveType) $ sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "curveType") retCLong []

-- | Controls the curve type.
--
-- Defaults to @MTLCurveTypeRound@. All keyframes share the same curve type.
--
-- ObjC selector: @- setCurveType:@
setCurveType :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTLCurveType -> IO ()
setCurveType mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setCurveType:") retVoid [argCLong (coerce value)]

-- | Sets the curve basis function, determining how Metal interpolates the control points.
--
-- Defaults to @MTLCurveBasisBSpline@. All keyframes share the same curve basis function.
--
-- ObjC selector: @- curveBasis@
curveBasis :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTLCurveBasis
curveBasis mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  fmap (coerce :: CLong -> MTLCurveBasis) $ sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "curveBasis") retCLong []

-- | Sets the curve basis function, determining how Metal interpolates the control points.
--
-- Defaults to @MTLCurveBasisBSpline@. All keyframes share the same curve basis function.
--
-- ObjC selector: @- setCurveBasis:@
setCurveBasis :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTLCurveBasis -> IO ()
setCurveBasis mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setCurveBasis:") retVoid [argCLong (coerce value)]

-- | Configures the type of curve end caps.
--
-- Defaults to @MTLCurveEndCapsNone@. All keyframes share the same end cap type.
--
-- ObjC selector: @- curveEndCaps@
curveEndCaps :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> IO MTLCurveEndCaps
curveEndCaps mtL4AccelerationStructureMotionCurveGeometryDescriptor  =
  fmap (coerce :: CLong -> MTLCurveEndCaps) $ sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "curveEndCaps") retCLong []

-- | Configures the type of curve end caps.
--
-- Defaults to @MTLCurveEndCapsNone@. All keyframes share the same end cap type.
--
-- ObjC selector: @- setCurveEndCaps:@
setCurveEndCaps :: IsMTL4AccelerationStructureMotionCurveGeometryDescriptor mtL4AccelerationStructureMotionCurveGeometryDescriptor => mtL4AccelerationStructureMotionCurveGeometryDescriptor -> MTLCurveEndCaps -> IO ()
setCurveEndCaps mtL4AccelerationStructureMotionCurveGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionCurveGeometryDescriptor (mkSelector "setCurveEndCaps:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

