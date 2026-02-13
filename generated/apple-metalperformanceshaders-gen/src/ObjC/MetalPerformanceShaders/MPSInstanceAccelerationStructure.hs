{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , instanceBufferOffsetSelector
  , instanceBufferSelector
  , instanceCountSelector
  , maskBufferOffsetSelector
  , maskBufferSelector
  , setAccelerationStructuresSelector
  , setInstanceBufferOffsetSelector
  , setInstanceBufferSelector
  , setInstanceCountSelector
  , setMaskBufferOffsetSelector
  , setMaskBufferSelector
  , setTransformBufferOffsetSelector
  , setTransformBufferSelector
  , setTransformTypeSelector
  , transformBufferOffsetSelector
  , transformBufferSelector
  , transformTypeSelector

  -- * Enum types
  , MPSTransformType(MPSTransformType)
  , pattern MPSTransformTypeFloat4x4
  , pattern MPSTransformTypeIdentity

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Acceleration structures available for use in this instance acceleration structure. Each instance must provide an index into this array in the instance buffer as well as a transformation matrix in the transform buffer. All acceleration structures must share a single vertex buffer, optional index buffer, and optional mask buffer, though they may have different offsets within each buffer, and all acceleration structures must share the same acceleration structure group. If a polygon acceleration structure is rebuilt or refit, the instance acceleration structure must subsequently be rebuilt or refit.
--
-- ObjC selector: @- accelerationStructures@
accelerationStructures :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO (Id NSArray)
accelerationStructures mpsInstanceAccelerationStructure =
  sendMessage mpsInstanceAccelerationStructure accelerationStructuresSelector

-- | Acceleration structures available for use in this instance acceleration structure. Each instance must provide an index into this array in the instance buffer as well as a transformation matrix in the transform buffer. All acceleration structures must share a single vertex buffer, optional index buffer, and optional mask buffer, though they may have different offsets within each buffer, and all acceleration structures must share the same acceleration structure group. If a polygon acceleration structure is rebuilt or refit, the instance acceleration structure must subsequently be rebuilt or refit.
--
-- ObjC selector: @- setAccelerationStructures:@
setAccelerationStructures :: (IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure, IsNSArray value) => mpsInstanceAccelerationStructure -> value -> IO ()
setAccelerationStructures mpsInstanceAccelerationStructure value =
  sendMessage mpsInstanceAccelerationStructure setAccelerationStructuresSelector (toNSArray value)

-- | Buffer containing the 32 bit unsigned integer index into the acceleration structure array for each instance
--
-- ObjC selector: @- instanceBuffer@
instanceBuffer :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO RawId
instanceBuffer mpsInstanceAccelerationStructure =
  sendMessage mpsInstanceAccelerationStructure instanceBufferSelector

-- | Buffer containing the 32 bit unsigned integer index into the acceleration structure array for each instance
--
-- ObjC selector: @- setInstanceBuffer:@
setInstanceBuffer :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> RawId -> IO ()
setInstanceBuffer mpsInstanceAccelerationStructure value =
  sendMessage mpsInstanceAccelerationStructure setInstanceBufferSelector value

-- | Offset, in bytes, into the instance buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- instanceBufferOffset@
instanceBufferOffset :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO CULong
instanceBufferOffset mpsInstanceAccelerationStructure =
  sendMessage mpsInstanceAccelerationStructure instanceBufferOffsetSelector

-- | Offset, in bytes, into the instance buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- setInstanceBufferOffset:@
setInstanceBufferOffset :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> CULong -> IO ()
setInstanceBufferOffset mpsInstanceAccelerationStructure value =
  sendMessage mpsInstanceAccelerationStructure setInstanceBufferOffsetSelector value

-- | Buffer containing one column major matrix_float4x4 transformation matrix per instance
--
-- ObjC selector: @- transformBuffer@
transformBuffer :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO RawId
transformBuffer mpsInstanceAccelerationStructure =
  sendMessage mpsInstanceAccelerationStructure transformBufferSelector

-- | Buffer containing one column major matrix_float4x4 transformation matrix per instance
--
-- ObjC selector: @- setTransformBuffer:@
setTransformBuffer :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> RawId -> IO ()
setTransformBuffer mpsInstanceAccelerationStructure value =
  sendMessage mpsInstanceAccelerationStructure setTransformBufferSelector value

-- | Offset, in bytes, into the transform buffer. Defaults to 0 bytes. Must be aligned to the stride of the transform type.
--
-- ObjC selector: @- transformBufferOffset@
transformBufferOffset :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO CULong
transformBufferOffset mpsInstanceAccelerationStructure =
  sendMessage mpsInstanceAccelerationStructure transformBufferOffsetSelector

-- | Offset, in bytes, into the transform buffer. Defaults to 0 bytes. Must be aligned to the stride of the transform type.
--
-- ObjC selector: @- setTransformBufferOffset:@
setTransformBufferOffset :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> CULong -> IO ()
setTransformBufferOffset mpsInstanceAccelerationStructure value =
  sendMessage mpsInstanceAccelerationStructure setTransformBufferOffsetSelector value

-- | Instance transform type. Defaults to MPSTransformTypeFloat4x4. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- transformType@
transformType :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO MPSTransformType
transformType mpsInstanceAccelerationStructure =
  sendMessage mpsInstanceAccelerationStructure transformTypeSelector

-- | Instance transform type. Defaults to MPSTransformTypeFloat4x4. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- setTransformType:@
setTransformType :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> MPSTransformType -> IO ()
setTransformType mpsInstanceAccelerationStructure value =
  sendMessage mpsInstanceAccelerationStructure setTransformTypeSelector value

-- | Mask buffer containing one uint32_t mask per instance. May be nil.
--
-- ObjC selector: @- maskBuffer@
maskBuffer :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO RawId
maskBuffer mpsInstanceAccelerationStructure =
  sendMessage mpsInstanceAccelerationStructure maskBufferSelector

-- | Mask buffer containing one uint32_t mask per instance. May be nil.
--
-- ObjC selector: @- setMaskBuffer:@
setMaskBuffer :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> RawId -> IO ()
setMaskBuffer mpsInstanceAccelerationStructure value =
  sendMessage mpsInstanceAccelerationStructure setMaskBufferSelector value

-- | Offset, in bytes, into the mask buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- maskBufferOffset@
maskBufferOffset :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO CULong
maskBufferOffset mpsInstanceAccelerationStructure =
  sendMessage mpsInstanceAccelerationStructure maskBufferOffsetSelector

-- | Offset, in bytes, into the mask buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- setMaskBufferOffset:@
setMaskBufferOffset :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> CULong -> IO ()
setMaskBufferOffset mpsInstanceAccelerationStructure value =
  sendMessage mpsInstanceAccelerationStructure setMaskBufferOffsetSelector value

-- | Number of instances. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- instanceCount@
instanceCount :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> IO CULong
instanceCount mpsInstanceAccelerationStructure =
  sendMessage mpsInstanceAccelerationStructure instanceCountSelector

-- | Number of instances. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- setInstanceCount:@
setInstanceCount :: IsMPSInstanceAccelerationStructure mpsInstanceAccelerationStructure => mpsInstanceAccelerationStructure -> CULong -> IO ()
setInstanceCount mpsInstanceAccelerationStructure value =
  sendMessage mpsInstanceAccelerationStructure setInstanceCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accelerationStructures@
accelerationStructuresSelector :: Selector '[] (Id NSArray)
accelerationStructuresSelector = mkSelector "accelerationStructures"

-- | @Selector@ for @setAccelerationStructures:@
setAccelerationStructuresSelector :: Selector '[Id NSArray] ()
setAccelerationStructuresSelector = mkSelector "setAccelerationStructures:"

-- | @Selector@ for @instanceBuffer@
instanceBufferSelector :: Selector '[] RawId
instanceBufferSelector = mkSelector "instanceBuffer"

-- | @Selector@ for @setInstanceBuffer:@
setInstanceBufferSelector :: Selector '[RawId] ()
setInstanceBufferSelector = mkSelector "setInstanceBuffer:"

-- | @Selector@ for @instanceBufferOffset@
instanceBufferOffsetSelector :: Selector '[] CULong
instanceBufferOffsetSelector = mkSelector "instanceBufferOffset"

-- | @Selector@ for @setInstanceBufferOffset:@
setInstanceBufferOffsetSelector :: Selector '[CULong] ()
setInstanceBufferOffsetSelector = mkSelector "setInstanceBufferOffset:"

-- | @Selector@ for @transformBuffer@
transformBufferSelector :: Selector '[] RawId
transformBufferSelector = mkSelector "transformBuffer"

-- | @Selector@ for @setTransformBuffer:@
setTransformBufferSelector :: Selector '[RawId] ()
setTransformBufferSelector = mkSelector "setTransformBuffer:"

-- | @Selector@ for @transformBufferOffset@
transformBufferOffsetSelector :: Selector '[] CULong
transformBufferOffsetSelector = mkSelector "transformBufferOffset"

-- | @Selector@ for @setTransformBufferOffset:@
setTransformBufferOffsetSelector :: Selector '[CULong] ()
setTransformBufferOffsetSelector = mkSelector "setTransformBufferOffset:"

-- | @Selector@ for @transformType@
transformTypeSelector :: Selector '[] MPSTransformType
transformTypeSelector = mkSelector "transformType"

-- | @Selector@ for @setTransformType:@
setTransformTypeSelector :: Selector '[MPSTransformType] ()
setTransformTypeSelector = mkSelector "setTransformType:"

-- | @Selector@ for @maskBuffer@
maskBufferSelector :: Selector '[] RawId
maskBufferSelector = mkSelector "maskBuffer"

-- | @Selector@ for @setMaskBuffer:@
setMaskBufferSelector :: Selector '[RawId] ()
setMaskBufferSelector = mkSelector "setMaskBuffer:"

-- | @Selector@ for @maskBufferOffset@
maskBufferOffsetSelector :: Selector '[] CULong
maskBufferOffsetSelector = mkSelector "maskBufferOffset"

-- | @Selector@ for @setMaskBufferOffset:@
setMaskBufferOffsetSelector :: Selector '[CULong] ()
setMaskBufferOffsetSelector = mkSelector "setMaskBufferOffset:"

-- | @Selector@ for @instanceCount@
instanceCountSelector :: Selector '[] CULong
instanceCountSelector = mkSelector "instanceCount"

-- | @Selector@ for @setInstanceCount:@
setInstanceCountSelector :: Selector '[CULong] ()
setInstanceCountSelector = mkSelector "setInstanceCount:"

