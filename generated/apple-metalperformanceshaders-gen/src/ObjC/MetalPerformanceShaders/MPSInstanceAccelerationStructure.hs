{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An acceleration structure built over instances of other acceleration structures
--
-- Instancing can be used to reduce memory usage in scenes that contain many copies of the same object(s) or to combine multiple acceleration structures such as a static and dynamic acceleration structure into a two-level instance hierarchy.
--
-- The typical pattern for creating an instance acceleration structure is as follows. First, create individual bottom-level acceleration structures. Then assign these acceleration structures to the accelerationStructures property of an instance acceleration structure.
--
-- All of the acceleration structures in the instance hierarchy must share the same MPSAccelerationStructureGroup. Furthermore, all of the bottom-level acceleration structures must share the same vertex buffer, index buffer, etc. although they may have different offsets within those buffers.
--
-- MPSAccelerationStructureGroup *group = nil;
-- group = [[MPSAccelerationStructureGroup alloc] initWithDevice:device];
--
-- MPSInstanceAccelerationStructure *instanceAccel = nil;
-- instanceAccel = [[MPSInstanceAccelerationStructure alloc] initWithGroup:group];
--
-- NSMutableArray *accelerationStructures = [NSMutableArray array];
-- instanceAccel.accelerationStructures = accelerationStructures;
--
-- instanceAccel.instanceCount = instanceCount;
--
-- for (ObjectType *objectType in objectTypes) {
-- MPSTriangleAccelerationStructure *triAccel = nil;
-- triAccel = [[MPSTriangleAccelerationStructure alloc] initWithGroup:group];
--
-- triAccel.vertexBuffer = objectType.vertexBuffer;
-- triAccel.vertexBufferOffset = objectType.vertexBufferOffset;
-- triAccel.triangleCount = objectType.triangleCount;
--
-- [triAccel rebuild];
--
-- [accelerationStructures addObject:triAccel];
-- }
--
-- Next, create a buffer containing the acceleration structure index for each instance, and another acceleration structure containing the transformation matrix for each instance:
--
-- NSUInteger instanceBufferLength = sizeof(uint32_t) * instanceCount;
--
-- id <MTLBuffer> instanceBuffer =
-- [device newBufferWithLength:instanceBufferLength
-- options:MTLResourceStorageModeManaged];
--
-- memcpy(instanceBuffer.contents, instances,
-- instanceBufferLength);
-- [instanceBuffer
-- didModifyRange:NSMakeRange(0, instanceBufferLength)];
--
-- instanceAccel.instanceBuffer = instanceBuffer;
--
-- // Similar for transformation matrix buffer
--
-- Finally, rebuild the instance acceleration structure:
--
-- [instanceAccel rebuild];
--
-- Refitting and Rebuilding Bottom-Level Acceleration Structures: when a bottom level acceleration structure is rebuild or refit, its' bounding box may change. Therefore, the instance acceleration structure also needs to be rebuilt or refit.
--
-- Copying and Serializing Instance Acceleration Structures: When an instance acceleration structure is copied or serialized, the bottom level acceleration structures are not copied or serialized. These must be copied or serialized along with the instance acceleration structure and assigned to the new instance acceleration structure. This also applies to buffer properties such as the instance buffer, transformation buffer, etc.
--
-- Performance Guidelines:
--
-- - Use instancing to reduce memory usage: if there are many copies of the same object(s) in       a scene, using instances of the same object can reduce memory usage and acceleration       structure build time. Rebuilding or refitting the top level acceleration structure can       also be much faster than rebuilding a large single level acceleration structure.
--
-- - Consider flattening your instance hierarchy into a single acceleration structure if the       increased memory usage and acceleration structure build time are not a concern.       Intersecting a two level acceleration structure can have a significant performance cost so       only use it when necessary. Which technique to use depends on the scene and use case. For       example, in a rendering application, it may be best to use an instance hierarchy for       interactive scene editing and preview and flattening the instance hierarchy for the final       render. For smaller scenes, it may also be sufficient to refit a flattened acceleration       structure rather than rebuilding an instance hierarchy.
--
-- - If there is only a single object in the scene, intersect its acceleration structure       directly instead of using an instance hierarchy.
--
-- - Consider dividing objects into static and dynamic acceleration structures. If dynamic       objects require the acceleration structure to be rebuilt frequently, create a high quality       static acceleration structure and a lower quality but faster to build dynamic acceleration       structure. These two acceleration structures can then be combined with a two level       acceleration structure. Use MPSTransformTypeIdentity to reduce the overhead of this       technique. Whether this technique is more efficient than rebuilding the entire       acceleration structure depends on the scene.
--
-- See MPSAccelerationStructure for more information
--
-- Generated bindings for @MPSInstanceAccelerationStructure@.
module ObjC.MetalPerformanceShaders.MPSInstanceAccelerationStructure
  ( MPSInstanceAccelerationStructure
  , IsMPSInstanceAccelerationStructure(..)
  , accelerationStructures
  , setAccelerationStructures
  , instanceBuffer
  , setInstanceBuffer
  , instanceBufferOffset
  , setInstanceBufferOffset
  , transformBuffer
  , setTransformBuffer
  , transformBufferOffset
  , setTransformBufferOffset
  , transformType
  , setTransformType
  , maskBuffer
  , setMaskBuffer
  , maskBufferOffset
  , setMaskBufferOffset
  , instanceCount
  , setInstanceCount
  , accelerationStructuresSelector
  , setAccelerationStructuresSelector
  , instanceBufferSelector
  , setInstanceBufferSelector
  , instanceBufferOffsetSelector
  , setInstanceBufferOffsetSelector
  , transformBufferSelector
  , setTransformBufferSelector
  , transformBufferOffsetSelector
  , setTransformBufferOffsetSelector
  , transformTypeSelector
  , setTransformTypeSelector
  , maskBufferSelector
  , setMaskBufferSelector
  , maskBufferOffsetSelector
  , setMaskBufferOffsetSelector
  , instanceCountSelector
  , setInstanceCountSelector

  -- * Enum types
  , MPSTransformType(MPSTransformType)
  , pattern MPSTransformTypeFloat4x4
  , pattern MPSTransformTypeIdentity

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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Acceleration structures available for use in this instance acceleration structure. Each instance must provide an index into this array in the instance buffer as well as a transformation matrix in the transform buffer. All acceleration structures must share a single vertex buffer, optional index buffer, and optional mask buffer, though they may have different offsets within each buffer, and all acceleration structures must share the same acceleration structure group. If a polygon acceleration structure is rebuilt or refit, the instance acceleration structure must subsequently be rebuilt or refit.
--
-- ObjC selector: @- accelerationStructures@
accelerationStructures :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO (Id NSArray)
accelerationStructures mpsInstanceAccelerationStructure  =
    sendMsg mpsInstanceAccelerationStructure (mkSelector "accelerationStructures") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Acceleration structures available for use in this instance acceleration structure. Each instance must provide an index into this array in the instance buffer as well as a transformation matrix in the transform buffer. All acceleration structures must share a single vertex buffer, optional index buffer, and optional mask buffer, though they may have different offsets within each buffer, and all acceleration structures must share the same acceleration structure group. If a polygon acceleration structure is rebuilt or refit, the instance acceleration structure must subsequently be rebuilt or refit.
--
-- ObjC selector: @- setAccelerationStructures:@
setAccelerationStructures :: (IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure, IsNSArray value) => mpsInstanceAccelerationStructure -> value -> IO ()
setAccelerationStructures mpsInstanceAccelerationStructure  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mpsInstanceAccelerationStructure (mkSelector "setAccelerationStructures:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Buffer containing the 32 bit unsigned integer index into the acceleration structure array for each instance
--
-- ObjC selector: @- instanceBuffer@
instanceBuffer :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO RawId
instanceBuffer mpsInstanceAccelerationStructure  =
    fmap (RawId . castPtr) $ sendMsg mpsInstanceAccelerationStructure (mkSelector "instanceBuffer") (retPtr retVoid) []

-- | Buffer containing the 32 bit unsigned integer index into the acceleration structure array for each instance
--
-- ObjC selector: @- setInstanceBuffer:@
setInstanceBuffer :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> RawId -> IO ()
setInstanceBuffer mpsInstanceAccelerationStructure  value =
    sendMsg mpsInstanceAccelerationStructure (mkSelector "setInstanceBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Offset, in bytes, into the instance buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- instanceBufferOffset@
instanceBufferOffset :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO CULong
instanceBufferOffset mpsInstanceAccelerationStructure  =
    sendMsg mpsInstanceAccelerationStructure (mkSelector "instanceBufferOffset") retCULong []

-- | Offset, in bytes, into the instance buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- setInstanceBufferOffset:@
setInstanceBufferOffset :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> CULong -> IO ()
setInstanceBufferOffset mpsInstanceAccelerationStructure  value =
    sendMsg mpsInstanceAccelerationStructure (mkSelector "setInstanceBufferOffset:") retVoid [argCULong value]

-- | Buffer containing one column major matrix_float4x4 transformation matrix per instance
--
-- ObjC selector: @- transformBuffer@
transformBuffer :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO RawId
transformBuffer mpsInstanceAccelerationStructure  =
    fmap (RawId . castPtr) $ sendMsg mpsInstanceAccelerationStructure (mkSelector "transformBuffer") (retPtr retVoid) []

-- | Buffer containing one column major matrix_float4x4 transformation matrix per instance
--
-- ObjC selector: @- setTransformBuffer:@
setTransformBuffer :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> RawId -> IO ()
setTransformBuffer mpsInstanceAccelerationStructure  value =
    sendMsg mpsInstanceAccelerationStructure (mkSelector "setTransformBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Offset, in bytes, into the transform buffer. Defaults to 0 bytes. Must be aligned to the stride of the transform type.
--
-- ObjC selector: @- transformBufferOffset@
transformBufferOffset :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO CULong
transformBufferOffset mpsInstanceAccelerationStructure  =
    sendMsg mpsInstanceAccelerationStructure (mkSelector "transformBufferOffset") retCULong []

-- | Offset, in bytes, into the transform buffer. Defaults to 0 bytes. Must be aligned to the stride of the transform type.
--
-- ObjC selector: @- setTransformBufferOffset:@
setTransformBufferOffset :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> CULong -> IO ()
setTransformBufferOffset mpsInstanceAccelerationStructure  value =
    sendMsg mpsInstanceAccelerationStructure (mkSelector "setTransformBufferOffset:") retVoid [argCULong value]

-- | Instance transform type. Defaults to MPSTransformTypeFloat4x4. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- transformType@
transformType :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO MPSTransformType
transformType mpsInstanceAccelerationStructure  =
    fmap (coerce :: CULong -> MPSTransformType) $ sendMsg mpsInstanceAccelerationStructure (mkSelector "transformType") retCULong []

-- | Instance transform type. Defaults to MPSTransformTypeFloat4x4. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- setTransformType:@
setTransformType :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> MPSTransformType -> IO ()
setTransformType mpsInstanceAccelerationStructure  value =
    sendMsg mpsInstanceAccelerationStructure (mkSelector "setTransformType:") retVoid [argCULong (coerce value)]

-- | Mask buffer containing one uint32_t mask per instance. May be nil.
--
-- ObjC selector: @- maskBuffer@
maskBuffer :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO RawId
maskBuffer mpsInstanceAccelerationStructure  =
    fmap (RawId . castPtr) $ sendMsg mpsInstanceAccelerationStructure (mkSelector "maskBuffer") (retPtr retVoid) []

-- | Mask buffer containing one uint32_t mask per instance. May be nil.
--
-- ObjC selector: @- setMaskBuffer:@
setMaskBuffer :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> RawId -> IO ()
setMaskBuffer mpsInstanceAccelerationStructure  value =
    sendMsg mpsInstanceAccelerationStructure (mkSelector "setMaskBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Offset, in bytes, into the mask buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- maskBufferOffset@
maskBufferOffset :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO CULong
maskBufferOffset mpsInstanceAccelerationStructure  =
    sendMsg mpsInstanceAccelerationStructure (mkSelector "maskBufferOffset") retCULong []

-- | Offset, in bytes, into the mask buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- setMaskBufferOffset:@
setMaskBufferOffset :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> CULong -> IO ()
setMaskBufferOffset mpsInstanceAccelerationStructure  value =
    sendMsg mpsInstanceAccelerationStructure (mkSelector "setMaskBufferOffset:") retVoid [argCULong value]

-- | Number of instances. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- instanceCount@
instanceCount :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO CULong
instanceCount mpsInstanceAccelerationStructure  =
    sendMsg mpsInstanceAccelerationStructure (mkSelector "instanceCount") retCULong []

-- | Number of instances. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- setInstanceCount:@
setInstanceCount :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> CULong -> IO ()
setInstanceCount mpsInstanceAccelerationStructure  value =
    sendMsg mpsInstanceAccelerationStructure (mkSelector "setInstanceCount:") retVoid [argCULong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accelerationStructures@
accelerationStructuresSelector :: Selector
accelerationStructuresSelector = mkSelector "accelerationStructures"

-- | @Selector@ for @setAccelerationStructures:@
setAccelerationStructuresSelector :: Selector
setAccelerationStructuresSelector = mkSelector "setAccelerationStructures:"

-- | @Selector@ for @instanceBuffer@
instanceBufferSelector :: Selector
instanceBufferSelector = mkSelector "instanceBuffer"

-- | @Selector@ for @setInstanceBuffer:@
setInstanceBufferSelector :: Selector
setInstanceBufferSelector = mkSelector "setInstanceBuffer:"

-- | @Selector@ for @instanceBufferOffset@
instanceBufferOffsetSelector :: Selector
instanceBufferOffsetSelector = mkSelector "instanceBufferOffset"

-- | @Selector@ for @setInstanceBufferOffset:@
setInstanceBufferOffsetSelector :: Selector
setInstanceBufferOffsetSelector = mkSelector "setInstanceBufferOffset:"

-- | @Selector@ for @transformBuffer@
transformBufferSelector :: Selector
transformBufferSelector = mkSelector "transformBuffer"

-- | @Selector@ for @setTransformBuffer:@
setTransformBufferSelector :: Selector
setTransformBufferSelector = mkSelector "setTransformBuffer:"

-- | @Selector@ for @transformBufferOffset@
transformBufferOffsetSelector :: Selector
transformBufferOffsetSelector = mkSelector "transformBufferOffset"

-- | @Selector@ for @setTransformBufferOffset:@
setTransformBufferOffsetSelector :: Selector
setTransformBufferOffsetSelector = mkSelector "setTransformBufferOffset:"

-- | @Selector@ for @transformType@
transformTypeSelector :: Selector
transformTypeSelector = mkSelector "transformType"

-- | @Selector@ for @setTransformType:@
setTransformTypeSelector :: Selector
setTransformTypeSelector = mkSelector "setTransformType:"

-- | @Selector@ for @maskBuffer@
maskBufferSelector :: Selector
maskBufferSelector = mkSelector "maskBuffer"

-- | @Selector@ for @setMaskBuffer:@
setMaskBufferSelector :: Selector
setMaskBufferSelector = mkSelector "setMaskBuffer:"

-- | @Selector@ for @maskBufferOffset@
maskBufferOffsetSelector :: Selector
maskBufferOffsetSelector = mkSelector "maskBufferOffset"

-- | @Selector@ for @setMaskBufferOffset:@
setMaskBufferOffsetSelector :: Selector
setMaskBufferOffsetSelector = mkSelector "setMaskBufferOffset:"

-- | @Selector@ for @instanceCount@
instanceCountSelector :: Selector
instanceCountSelector = mkSelector "instanceCount"

-- | @Selector@ for @setInstanceCount:@
setInstanceCountSelector :: Selector
setInstanceCountSelector = mkSelector "setInstanceCount:"

