{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A data structure built over geometry used to accelerate ray tracing
--
-- Do not use this base class directly. Use one of the derived classes instead. The general pattern for creating an acceleration structure is as follows. First, create the acceleration structure:
--
-- MPSTriangleAccelerationStructure *accelerationStructure = nil;
-- accelerationStructure = [[MPSTriangleAccelerationStructure alloc] initWithDevice:device];
--
-- Then, assign values to the acceleration structure's properties:
--
-- accelerationStructure.vertexBuffer = vertexBuffer;
-- accelerationStructure.triangleCount = triangleCount;
--
-- Finally, the acceleration structure must be built:
--
-- [accelerationStructure rebuild];
--
-- The acceleration structure can then be used to encode ray intersection tests with an MPSRayIntersector:
--
-- [raytracer encodeIntersectionToCommandBuffer:commandBuffer
-- intersectionType:MPSIntersectionTypeNearest
-- rayBuffer:rayBuffer
-- rayBufferOffset:0
-- intersectionBuffer:intersectionBuffer
-- intersectionBufferOffset:0
-- rayCount:rayCount
-- accelerationStructure:accelerationStructure];
--
-- Asynchronous Acceleration Structure Builds: Rebuilding an acceleration structure is an expensive operation. Note that there is also a method to rebuild the acceleration structure asynchronously to avoid blocking the main thread.
--
-- [accelerationStructure rebuildWithCompletionHandler:^(MPSAccelerationStructure *accel) {
-- // Kick off ray intersection work
-- }];
--
-- Streaming Geometry Updates: It is generally safe to change buffer properties such as the vertex buffer after intersection tests have been encoded into a command buffer, but the contents of those buffers cannot be safely changed by the CPU until the command buffer has finished executing on the GPU. It is also not safe to rebuild the acceleration structure until the command buffer has completed.
--
-- If the CPU needs to stream geometry updates to the GPU, ensure the vertex and other buffers are double or triple buffered.
--
-- #define MAX_ASYNC_OPERATIONS 3
--
-- // Initialization:
--
-- // Create a semaphore with the maximum number of asynchronous operations in flight
-- dispatch_semaphore_t asyncOperationSemaphore = dispatch_semaphore_create(MAX_ASYNC_OPERATIONS);
--
-- // Create an acceleration structure for each vertex buffer range
-- NSMutableArray *accelerationStructures = [NSMutableArray array];
--
-- NSUInteger vertexBufferLength = sizeof(float3) * vertexCount * MAX_ASYNC_OPERATIONS;
-- id <MTLBuffer> vertexBuffer = [device newBufferWithLength:vertexBufferLength
-- options:MTLResourceStorageModeManaged];
--
-- for (NSUInteger i = 0; i < MAX_ASYNC_OPERATIONS; i++) {
-- MPSTriangleAccelerationStructure *accel = nil;
-- accel = [[MPSTriangleAccelerationStructure alloc] initWithDevice:device];
--
-- // Configure acceleration structure
-- accel.vertexBuffer = vertexBuffer;
-- accel.vertexBufferOffset = i * sizeof(float3) * vertexCount;
--
-- [accelerationStructures addObject:accel];
-- }
--
-- NSUInteger asyncOperationIndex = 0;
--
-- // Encode intersection testing:
--
-- // Wait until there is a free acceleration structure
-- dispatch_semaphore_wait(asyncOperationSemaphore, DISPATCH_TIME_FOREVER);
--
-- MPSTriangleAccelerationStructure *accel = accelerationStructures[asyncOperationIndex];
-- asyncOperationIndex = (asyncOperationIndex + 1) % MAX_ASYNC_OPERATIONS;
--
-- float3 *vertices = (float3 *)((uint8_t *)vertexBuffer.contents + accel.vertexBufferOffset);
-- // Update vertices
-- MPSDidModifyRange(vertexBuffer, NSMakeRange(accel.vertexBufferOffset, sizeof(float3) * vertexCount));
--
-- // Rebuild the acceleration structure
-- [accel rebuild];
--
-- // Encode actual intersection work
-- [raytracer encodeIntersectionToCommandBuffer:commandBuffer
-- intersectionType:MPSIntersectionTypeNearest
-- rayBuffer:rayBuffer
-- rayBufferOffset:rayBufferOffset
-- intersectionBuffer:intersectionBuffer
-- intersectionBufferOffset:intersectionBufferOffset
-- rayCount:rayCount
-- accelerationStructure:accel];
--
-- // Register a completion handler to run when the GPU finishes executing
-- [commandBuffer addCompletedHandler:^(id <MTLCommandBuffer> commandBuffer) {
-- Intersection *intersections = (Intersection *)((uint8_t *)intersectionBuffer.contents +
-- intersectionBufferOffset);
--
-- // Process intersections
--
-- // Signal that the acceleration structure is now available for reuse
-- dispatch_semaphore_signal(asyncOperationSemaphore);
-- }];
--
-- // Commit the command buffer to allow the GPU to start executing
-- [commandBuffer commit];
--
-- Refitting acceleration structures: If geometry has only moved slightly and not added or removed from the scene, it can be much faster to refit the existing topology of an acceleration structure to the new geometry than to rebuild the acceleration structure from scratch. Refitting can also be pipelined with other GPU work such as intersection testing. If the geometry is transformed entirely on the GPU, it is not necessary to use double or triple buffering. For example:
--
-- id <MTLCommandBuffer> commandBuffer = [commandQueue commandBuffer];
--
-- id <MTLComputeCommandEncoder> encoder = [commandBuffer computeCommandEncoder];
--
-- [encoder setBuffer:untransformedVertexBuffer offset:0 atIndex:0];
--
-- [encoder setBuffer:accelerationStructure.vertexBuffer
-- offset:accelerationStructure.vertexBufferOffset
-- atIndex:1];
--
-- [encoder setBuffer:transformationMatrices offset:0 atIndex:2];
--
-- [encoder setComputePipelineState:transformVerticesPipeline];
--
-- [encoder dispatchThreads:MTLSizeMake(accelerationStructure.triangleCount * 3, 1, 1)
-- threadsPerThreadgroup:MTLSizeMake(64, 1, 1)];
--
-- [encoder endEncoding];
--
-- [accelerationStructure encodeRefitToCommandBuffer:commandBuffer];
--
-- [commandBuffer commit];
--
-- Serializing Acceleration Structures: Instead of rebuilding acceleration structures from scratch they can be built offline, serialized, and reloaded at runtime using the NSSecureCoding protocol:
--
-- // Build time:
-- NSError *error = nil;
-- NSData *data = [NSKeyedArchiver archivedDataWithRootObject:accel
-- requiringSecureCoding:YES
-- error:&error];
--
-- if (!data)
-- NSLog(@"Error archiving MPSAccelerationStructure: %@",
-- error.localizedDescription);
--
-- // Runtime:
-- MPSTriangleAccelerationStructure *accel;
-- accel = [NSKeyedUnarchiver unarchivedObjectOfClass:[MPSTriangleAccelerationStructure class]
-- fromData:data
-- error:&error];
--
-- if (!accel)
-- NSLog(@"Error unarchiving MPSAccelerationStructure: %@",
-- error.localizedDescription);
--
-- Copying Acceleration Structures: Acceleration structures can be copied using the NSCopying protocol, even to a different Metal device. This can be used for multi-GPU raytracing. Buffer properties are not copied to the new acceleration structure. These buffers must instead be copied to the new Metal device and assigned to the new acceleration structure. For example:
--
-- MPSTriangleAccelerationStructure *copy = [accelerationStructure copyWithZone:nil
-- device:newDevice];
--
-- copy.vertexBuffer = [self copyBuffer:accelerationStructure.vertexBuffer
-- withDevice:newDevice];
--
-- Performance Guidelines:
--
-- - Provide accurate acceleration structure hints: if an acceleration structure does not       require support for refitting, a higher quality construction algorithm can be used.       However, if an acceleration structure must be rebuilt frequently, a lower quality       but higher performance construction algorithm can be used.
--
-- - Consider refitting existing acceleration structures rather than rebuilding them from       scratch. This is typically much faster and can result in a reasonably high quality       tree if the geometry has not been modified dramatically. Refitting can also be pipelined       with other GPU work. If objects have been added to or removed from the scene, it is       typically necessary to rebuild the acceleration structure rather than refit it.
--
-- - Rebuild acceleration structures asynchronously when possible to avoid blocking the main       thread. Consider presenting a UI indicating that work is happening in the background while       allowing the user to consider interacting with your application.
--
-- - If you need to mix intersection testing with acceleration structure builds (e.g. if the       user is interactively editing the scene while rendering or if objects are moving       significantly) consider allocating two independent acceleration structures that refer to       two copies of the scene data. Then, asynchronously rebuild one acceleration structure       while the other one is used for rendering. Once the rebuild has completed, swap the       acceleration structures. The intermediate frames could be filled by refitting the       rendering acceleration structure until the rebuilt acceleration structure is ready.
--
-- - When running in Xcode, disable "Enable Backtrace Recording" in your scheme settings.       Enabling this setting can significantly increase acceleration structure build time.
--
-- - Consider using quadrilaterals instead of triangles to represent your geometry.       The cost of intersecting a quadrilateral is typically less than the cost of intersecting       two triangles, so quadrilaterals can improve performance. Quadrilaterals also typically       require 30-40% less memory than triangles including vertex data and internal buffers       allocated by the acceleration structure. Whether quadrilaterals improve or hurt       performance can depend on the geometry and ray distribution, so you should choose       whichever performs better for your application.
--
-- Thread Safety: MPSAccelerationStructures are generally not thread safe. Changing properties and rebuilding acceleration structures from multiple threads result in undefined behavior. However, it is safe to encode intersection tests with a single acceleration structure from multiple threads as long as each thread uses its own MPSRayIntersector.
--
-- Generated bindings for @MPSAccelerationStructure@.
module ObjC.MetalPerformanceShaders.MPSAccelerationStructure
  ( MPSAccelerationStructure
  , IsMPSAccelerationStructure(..)
  , init_
  , initWithDevice
  , initWithCoder_device
  , initWithGroup
  , initWithCoder_group
  , rebuild
  , rebuildWithCompletionHandler
  , encodeRefitToCommandBuffer
  , copyWithZone_device
  , copyWithZone_group
  , encodeWithCoder
  , group
  , status
  , usage
  , setUsage
  , copyWithZone_deviceSelector
  , copyWithZone_groupSelector
  , encodeRefitToCommandBufferSelector
  , encodeWithCoderSelector
  , groupSelector
  , initSelector
  , initWithCoder_deviceSelector
  , initWithCoder_groupSelector
  , initWithDeviceSelector
  , initWithGroupSelector
  , rebuildSelector
  , rebuildWithCompletionHandlerSelector
  , setUsageSelector
  , statusSelector
  , usageSelector

  -- * Enum types
  , MPSAccelerationStructureStatus(MPSAccelerationStructureStatus)
  , pattern MPSAccelerationStructureStatusUnbuilt
  , pattern MPSAccelerationStructureStatusBuilt
  , MPSAccelerationStructureUsage(MPSAccelerationStructureUsage)
  , pattern MPSAccelerationStructureUsageNone
  , pattern MPSAccelerationStructureUsageRefit
  , pattern MPSAccelerationStructureUsageFrequentRebuild
  , pattern MPSAccelerationStructureUsagePreferGPUBuild
  , pattern MPSAccelerationStructureUsagePreferCPUBuild

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Structs
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMPSAccelerationStructure mpsAccelerationStructure => mpsAccelerationStructure -> IO (Id MPSAccelerationStructure)
init_ mpsAccelerationStructure =
  sendOwnedMessage mpsAccelerationStructure initSelector

-- | Initialize the acceleration structure with a Metal device
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSAccelerationStructure mpsAccelerationStructure => mpsAccelerationStructure -> RawId -> IO (Id MPSAccelerationStructure)
initWithDevice mpsAccelerationStructure device =
  sendOwnedMessage mpsAccelerationStructure initWithDeviceSelector device

-- | Initialize the acceleration structure with an NSCoder and a Metal device. Buffer properties such as the vertex buffer, instance buffer, etc. are set to nil. Encode and decode these buffers along with the acceleration structure instead.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSAccelerationStructure mpsAccelerationStructure, IsNSCoder aDecoder) => mpsAccelerationStructure -> aDecoder -> RawId -> IO (Id MPSAccelerationStructure)
initWithCoder_device mpsAccelerationStructure aDecoder device =
  sendOwnedMessage mpsAccelerationStructure initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Initialize the acceleration structure with an acceleration structure group, if the acceleration structure will be used in an instance hierarchy.
--
-- The Metal device is determined from the acceleration structure group. All acceleration structures in the instance hierarchy must share the same group.
--
-- ObjC selector: @- initWithGroup:@
initWithGroup :: (IsMPSAccelerationStructure mpsAccelerationStructure, IsMPSAccelerationStructureGroup group) => mpsAccelerationStructure -> group -> IO (Id MPSAccelerationStructure)
initWithGroup mpsAccelerationStructure group =
  sendOwnedMessage mpsAccelerationStructure initWithGroupSelector (toMPSAccelerationStructureGroup group)

-- | Initialize the acceleration structure with an NSCoder and an acceleration structure group, if the acceleration structure will be used in an instance hierarchy. All acceleration structures in the instance hierarchy must share the same group. Buffer properties such as the vertex buffer, instance buffer, etc. are set to nil. Encode and decode these buffers along with the acceleration structure instead.
--
-- ObjC selector: @- initWithCoder:group:@
initWithCoder_group :: (IsMPSAccelerationStructure mpsAccelerationStructure, IsNSCoder aDecoder, IsMPSAccelerationStructureGroup group) => mpsAccelerationStructure -> aDecoder -> group -> IO (Id MPSAccelerationStructure)
initWithCoder_group mpsAccelerationStructure aDecoder group =
  sendOwnedMessage mpsAccelerationStructure initWithCoder_groupSelector (toNSCoder aDecoder) (toMPSAccelerationStructureGroup group)

-- | Rebuild the acceleration structure
--
-- This method must be called before any intersection tests can be scheduled with this acceleration structure. Before calling this method, fill out the properties of the acceleration structure such as vertex buffer, instance buffer, etc. The acceleration structure should be rebuilt when its contents (e.g. vertices in a triangle acceleration structure) have been modified significantly and must be rebuilt when properties such as triangle count, vertex stride, etc. have changed. When the contents of the acceleration structure have only been modified slightly, it may be cheaper to refit the acceleration structure instead.
--
-- This method blocks until the acceleration structure has been rebuilt. Until the rebuild has completed, the acceleration structure cannot be copied, encoded with NSSecureCoding, rebuilt, or refit. Before this method can be called, any pending GPU writes to the vertex buffer, index buffer, etc. must be completed (and, for managed buffers, synchronized). Any prior intersection tests must also be completed before the acceleration structure can be rebuilt.
--
-- ObjC selector: @- rebuild@
rebuild :: IsMPSAccelerationStructure mpsAccelerationStructure => mpsAccelerationStructure -> IO ()
rebuild mpsAccelerationStructure =
  sendMessage mpsAccelerationStructure rebuildSelector

-- | Rebuild the acceleration structure asynchronously
--
-- This method must be called before any intersection tests can be scheduled with this acceleration structure. Before calling this method, fill out the properties of the acceleration structure such as vertex buffer, instance buffer, etc. The acceleration structure should be rebuilt when its contents (e.g. vertices in a triangle acceleration structure) have been modified significantly and must be rebuilt when properties such as triangle count, vertex stride, etc. have changed. When the contents of the acceleration structure have only been modified slightly, it may be cheaper to refit the acceleration structure instead.
--
-- Until the rebuild has completed, the acceleration structure cannot be copied, encoded with NSSecureCoding, rebuilt, or refit. Before this method can be called, any pending GPU writes to the vertex buffer, index buffer, etc. must be completed (and, for managed buffers, synchronized). Any prior intersection tests must also be completed before the acceleration structure can be rebuilt.
--
-- ObjC selector: @- rebuildWithCompletionHandler:@
rebuildWithCompletionHandler :: IsMPSAccelerationStructure mpsAccelerationStructure => mpsAccelerationStructure -> Ptr () -> IO ()
rebuildWithCompletionHandler mpsAccelerationStructure completionHandler =
  sendMessage mpsAccelerationStructure rebuildWithCompletionHandlerSelector completionHandler

-- | Refit the existing acceleration structure to new data
--
-- This method is used to refit the acceleration structure to new vertex data, index data, instance data, etc. while preserving the existing acceleration structure topology. This is typically much faster than a full rebuild of the acceleration structure. Refitting can also be pipelined with other GPU work such as ray intersection.
--
-- Until the command buffer has completed, the acceleration structure cannot be copied, encoded with NSSecureCoding, or rebuilt. Changes to properties such as the triangle count or instance count might not be reflected. These changes require that the acceleration structure be rebuilt instead. The acceleration structure must be rebuilt at least once before this method can be called.
--
-- ObjC selector: @- encodeRefitToCommandBuffer:@
encodeRefitToCommandBuffer :: IsMPSAccelerationStructure mpsAccelerationStructure => mpsAccelerationStructure -> RawId -> IO ()
encodeRefitToCommandBuffer mpsAccelerationStructure commandBuffer =
  sendMessage mpsAccelerationStructure encodeRefitToCommandBufferSelector commandBuffer

-- | Create a a copy of this acceleration structure
--
-- The acceleration structure may be copied to a different Metal device. Buffer properties of the acceleration structure such as the vertex buffer, instance, buffer, etc. are set to nil. Copy these buffers to the new Metal device and assign them to the new acceleration structure instead. Do not copy the acceleration structure until any prior refit or rebuild operations have completed.
--
-- @zone@ — This parameter is ignored. Memory zones are no longer used by Objective-C.
--
-- @device@ — New Metal device
--
-- ObjC selector: @- copyWithZone:device:@
copyWithZone_device :: IsMPSAccelerationStructure mpsAccelerationStructure => mpsAccelerationStructure -> Ptr () -> RawId -> IO (Id MPSAccelerationStructure)
copyWithZone_device mpsAccelerationStructure zone device =
  sendOwnedMessage mpsAccelerationStructure copyWithZone_deviceSelector zone device

-- | Create a a copy of this acceleration structure
--
-- The acceleration structure may be copied with a different acceleration structure group. Buffer properties of the acceleration structure such as the vertex buffer, instance buffer, etc. are set to nil. Copy these buffers with the new Metal device and assign them to the new acceleration structure instead. Do not copy the acceleration structure until any prior refit or rebuild operations have completed.
--
-- @zone@ — This parameter is ignored. Memory zones are no longer used by Objective-C.
--
-- @group@ — New acceleration structure group
--
-- ObjC selector: @- copyWithZone:group:@
copyWithZone_group :: (IsMPSAccelerationStructure mpsAccelerationStructure, IsMPSAccelerationStructureGroup group) => mpsAccelerationStructure -> Ptr () -> group -> IO (Id MPSAccelerationStructure)
copyWithZone_group mpsAccelerationStructure zone group =
  sendOwnedMessage mpsAccelerationStructure copyWithZone_groupSelector zone (toMPSAccelerationStructureGroup group)

-- | Encode the acceleration structure with an NSCoder
--
-- Buffer properties such as the vertex buffer, index buffer, etc. are not be encoded. Encode and decode these buffers along with the acceleration structure instead. Do not encode the acceleration structure until any prior refit or rebuild operations have completed.
--
-- @coder@ — An archiver object
--
-- ObjC selector: @- encodeWithCoder:@
encodeWithCoder :: (IsMPSAccelerationStructure mpsAccelerationStructure, IsNSCoder coder) => mpsAccelerationStructure -> coder -> IO ()
encodeWithCoder mpsAccelerationStructure coder =
  sendMessage mpsAccelerationStructure encodeWithCoderSelector (toNSCoder coder)

-- | The group this acceleration structure was created with
--
-- ObjC selector: @- group@
group :: IsMPSAccelerationStructure mpsAccelerationStructure => mpsAccelerationStructure -> IO (Id MPSAccelerationStructureGroup)
group mpsAccelerationStructure =
  sendMessage mpsAccelerationStructure groupSelector

-- | Status indicating whether the acceleration structure has finished building
--
-- ObjC selector: @- status@
status :: IsMPSAccelerationStructure mpsAccelerationStructure => mpsAccelerationStructure -> IO MPSAccelerationStructureStatus
status mpsAccelerationStructure =
  sendMessage mpsAccelerationStructure statusSelector

-- | Acceleration structure usage options. Changes to this property require rebuilding the acceleration structure. Defaults to MPSAccelerationStructureUsageNone.
--
-- ObjC selector: @- usage@
usage :: IsMPSAccelerationStructure mpsAccelerationStructure => mpsAccelerationStructure -> IO MPSAccelerationStructureUsage
usage mpsAccelerationStructure =
  sendMessage mpsAccelerationStructure usageSelector

-- | Acceleration structure usage options. Changes to this property require rebuilding the acceleration structure. Defaults to MPSAccelerationStructureUsageNone.
--
-- ObjC selector: @- setUsage:@
setUsage :: IsMPSAccelerationStructure mpsAccelerationStructure => mpsAccelerationStructure -> MPSAccelerationStructureUsage -> IO ()
setUsage mpsAccelerationStructure value =
  sendMessage mpsAccelerationStructure setUsageSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSAccelerationStructure)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSAccelerationStructure)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSAccelerationStructure)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithGroup:@
initWithGroupSelector :: Selector '[Id MPSAccelerationStructureGroup] (Id MPSAccelerationStructure)
initWithGroupSelector = mkSelector "initWithGroup:"

-- | @Selector@ for @initWithCoder:group:@
initWithCoder_groupSelector :: Selector '[Id NSCoder, Id MPSAccelerationStructureGroup] (Id MPSAccelerationStructure)
initWithCoder_groupSelector = mkSelector "initWithCoder:group:"

-- | @Selector@ for @rebuild@
rebuildSelector :: Selector '[] ()
rebuildSelector = mkSelector "rebuild"

-- | @Selector@ for @rebuildWithCompletionHandler:@
rebuildWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
rebuildWithCompletionHandlerSelector = mkSelector "rebuildWithCompletionHandler:"

-- | @Selector@ for @encodeRefitToCommandBuffer:@
encodeRefitToCommandBufferSelector :: Selector '[RawId] ()
encodeRefitToCommandBufferSelector = mkSelector "encodeRefitToCommandBuffer:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector '[Ptr (), RawId] (Id MPSAccelerationStructure)
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @copyWithZone:group:@
copyWithZone_groupSelector :: Selector '[Ptr (), Id MPSAccelerationStructureGroup] (Id MPSAccelerationStructure)
copyWithZone_groupSelector = mkSelector "copyWithZone:group:"

-- | @Selector@ for @encodeWithCoder:@
encodeWithCoderSelector :: Selector '[Id NSCoder] ()
encodeWithCoderSelector = mkSelector "encodeWithCoder:"

-- | @Selector@ for @group@
groupSelector :: Selector '[] (Id MPSAccelerationStructureGroup)
groupSelector = mkSelector "group"

-- | @Selector@ for @status@
statusSelector :: Selector '[] MPSAccelerationStructureStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @usage@
usageSelector :: Selector '[] MPSAccelerationStructureUsage
usageSelector = mkSelector "usage"

-- | @Selector@ for @setUsage:@
setUsageSelector :: Selector '[MPSAccelerationStructureUsage] ()
setUsageSelector = mkSelector "setUsage:"

