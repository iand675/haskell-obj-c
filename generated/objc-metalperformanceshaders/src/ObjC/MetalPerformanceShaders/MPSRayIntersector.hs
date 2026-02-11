{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSRayIntersector
--
-- Performs intersection tests between rays and the geometry in an MPSAccelerationStructure
--
-- An MPSRayIntersector is used to schedule intersection tests between rays and geometry into an MTLCommandBuffer. First, create a raytracer with a Metal device. Then, configure the properties of the raytracer:
--
-- id <MTLDevice> device = MTLCreateSystemDefaultDevice();
-- id <MTLCommandQueue> commandQueue = [device newCommandQueue];
--
-- MPSRayIntersector *raytracer = [[MPSRayIntersector alloc] initWithDevice:device];
--
-- // Configure raytracer properties
--
-- Before scheduling intersection tests, an MPSAccelerationStructure must be created. The acceleration structure is built over geometry and is used to accelerate intersection testing. For example, to create a triangle acceleration structure, allocate an MPSTriangleAccelerationStructure object. Then, configure the properties of the acceleration structure. For example, triangle acceleration structures require a vertex buffer and a triangle count:
--
-- MPSTriangleAccelerationStructure *accelerationStructure =
-- [[MPSTriangleAccelerationStructure alloc] initWithDevice:device];
--
-- accelerationStructure.vertexBuffer = vertexBuffer;
-- accelerationStructure.triangleCount = triangleCount;
--
-- Acceleration structures must be built at least once before they are used for intersection testing, and must be rebuilt when the geometry changes. Rebuilding an acceleration structure is a time consuming operation, so an asynchronous version of this method is also available.
--
-- [accelerationStructure rebuild];
--
-- The raytracer is then used to schedule intersection tests into an MTLCommandBuffer. Rays are provided in batches through a Metal buffer, and intersection results are returned through another Metal buffer in the same order, one intersection per ray.
--
-- There are several choices of ray data type controlled by the rayDataType property. The default ray data type is MPSRayOriginDirection, which includes just the ray origin direction. The other data types add support for minimum and maximum intersection distances and ray masks. These data types are available in the Metal Shading Language by including the MetalPerformanceShaders/MetalPerformanceShaders.h header. Additional application specific per-ray data can also be appended to the end of the ray data type using the rayStride property. This data will be ignored by the intersector.
--
-- If the rays were generated on the CPU:
--
-- typedef MPSRayOriginDirection Ray;
--
-- // Create a buffer to hold the rays
-- id <MTLBuffer> rayBuffer = [device newBufferWithLength:sizeof(Ray) * rayCount options:0];
--
-- // Copy the rays into the ray buffer
-- memcpy(rayBuffer.contents, rays, sizeof(Ray) * rayCount);
--
-- // Create a buffer to hold the intersections
-- id <MTLBuffer> intersectionBuffer = [device newBufferWithLength:sizeof(Intersection) * rayCount
-- options:0];
--
-- It can be useful to prevent certain rays from participating in intersection testing. For example: rays which have bounced out of the scene in previous intersection tests. It may be more efficient to do this by compacting the ray buffer so that threads with invalid rays are not left idle during intersection testing. However, it can be more convenient to disable the ray in place. This can be done by setting most fields to invalid values. For example, setting the maximum distance to a negative value, setting the mask to zero, setting the direction to the zero vector, etc.
--
-- Finally, the intersection testing is encoded into an MTLCommandBuffer. There are two intersection types. The "nearest" intersection type returns the closest intersection along each ray. The "any" intersection type returns immediately when the first intersection is found. The "any" intersection type is useful for determining whether a point is visible from another point for, e.g., shadow rays or ambient occlusion rays and is typically much faster than the "nearest" intersection type.
--
-- id <MTLCommandBuffer> commandBuffer = [commandQueue commandBuffer];
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
-- [commandBuffer commit];
--
-- The intersection results are not available until the command buffer has finished executing on the GPU. It is not safe for the CPU to write or read the contents of the ray buffer, intersection buffer, vertex buffer, etc. until the command buffer has finished executing. Use the waitUntilCompleted or addCompletedHandler methods of the MTLCommandBuffer to block the CPU until the GPU has finished executing. Then retrieve the intersection results from the intersection buffer:
--
-- typedef MPSIntersectionDistancePrimitiveIndexCoordinates Intersection;
--
-- [commandBuffer waitUntilCompleted];
--
-- Intersection *intersections = (Intersection *)intersectionBuffer.contents;
--
-- There are also several choices of intersection data type controlled by the intersectionDataType property. The default intersection data type is MPSIntersectionDistancePrimitiveIndexCoordinates, which includes the intersection distance, primitive index, and barycentric coordinates. The other data types remove the primitive index or barycentric coordinates, which can be used to reduce the memory and memory bandwidth usage of the intersection buffer. These data types are available in the Metal Shading Language by including the MetalPerformanceShaders/MetalPerformanceShaders.h header.
--
-- The intersection distance field is positive when an intersection has been found and negative when there is no intersection. When using the "nearest" intersection type, the intersection point is the ray origin plus the ray direction multiplied by the intersection distance. The other fields are not valid if there is no intersection. Only the intersection distance field is valid for the "any" intersection type, and the distance is either a negative or positive value to indicate an intersection or miss. It does not necessarily contain the actual intersection distance when using the "any" intersection type.
--
-- Asynchronous Raytracing: Copying rays and intersections to and from the CPU is expensive. Furthermore, generating rays and consuming intersections on the CPU causes the CPU and GPU to block each other. If the CPU must generate rays and consume intersections, it is better to add an asynchronous completion handler to the MTLCommandBuffer. The CPU can then proceed to do other useful work and will be notified when the GPU has finished executing. Use double or triple buffered ray and intersection buffers to avoid race conditions such as the CPU overwriting data the GPU may be reading. Then the CPU can safely write to one range of the buffer while the GPU reads from another range of the buffer. Once the GPU is done  executing, the CPU and GPU can advance to the next range of the buffer. This method can be implemented using a completion handler and a semaphore:
--
-- #define MAX_ASYNC_OPERATIONS 3
--
-- // Initialization:
--
-- // Create a semaphore with the maximum number of asynchronous operations in flight
-- dispatch_semaphore_t asyncOperationSemaphore = dispatch_semaphore_create(MAX_ASYNC_OPERATIONS);
--
-- // Create a ray and intersection buffer large enough for the maximum number of operations
-- id <MTLBuffer> rayBuffer =
-- [device newBufferWithLength:sizeof(Ray) * rayCount * MAX_ASYNC_OPERATIONS
-- options:0];
--
-- id <MTLBuffer> intersectionBuffer =
-- [device newBufferWithLength:sizeof(Intersection) * rayCount * MAX_ASYNC_OPERATIONS
-- options:0];
--
-- NSUInteger asyncOperationIndex = 0;
--
-- // Encode intersection testing:
--
-- // Wait until there is a free buffer range
-- dispatch_semaphore_wait(asyncOperationSemaphore, DISPATCH_TIME_FOREVER);
--
-- // Copy rays into ray buffer
-- NSUInteger rayBufferOffset = sizeof(Ray) * rayCount * asyncOperationIndex;
-- NSUInteger intersectionBufferOffset = sizeof(Intersection) * rayCount * asyncOperationIndex;
--
-- memcpy((uint8_t *)rayBuffer.contents + rayBufferOffset, rays, sizeof(Ray) * rayCount);
--
-- // Advance the async operation index
-- asyncOperationIndex = (asyncOperationIndex + 1) % MAX_ASYNC_OPERATIONS;
--
-- // Create a command buffer
-- id <MTLCommandBuffer> commandBuffer = [commandQueue commandBuffer];
--
-- // Encode actual intersection work
-- [raytracer encodeIntersectionToCommandBuffer:commandBuffer
-- intersectionType:MPSIntersectionTypeNearest
-- rayBuffer:rayBuffer
-- rayBufferOffset:rayBufferOffset
-- intersectionBuffer:intersectionBuffer
-- intersectionBufferOffset:intersectionBufferOffset
-- rayCount:rayCount
-- accelerationStructure:accelerationStructure];
--
-- // Register a completion handler to run when the GPU finishes executing
-- [commandBuffer addCompletedHandler:^(id <MTLCommandBuffer> commandBuffer) {
-- Intersection *intersections = (Intersection *)((uint8_t *)intersectionBuffer.contents +
-- intersectionBufferOffset);
--
-- // Process intersections
--
-- // Signal that the ray and intersection buffer ranges are now available for reuse
-- dispatch_semaphore_signal(asyncOperationSemaphore);
-- }];
--
-- // Commit the command buffer to allow the GPU to start executing
-- [commandBuffer commit];
--
-- GPU Driven Raytracing: Pipelining CPU and GPU work with asynchronous raytracing is better than allowing the CPU and GPU block each other, but it is even better to produce rays and consume intersections entirely on the GPU. This avoids the need to copy rays and intersections to and from the GPU and avoids any kind of CPU/GPU synchronization. To do this, encode compute kernels before and after intersection testing. By processing rays in parallel, the compute kernels may also be able to generate and consume rays faster than the CPU. The ray generation kernel typically produces rays according to some camera model, and the intersection consumption kernel typically updates the output buffer or texture according to some shading model.
--
-- Since the rays and intersections will never leave the GPU, store them in private Metal buffers that are allocated in GPU memory rather than system memory. Because the ray generation, intersection testing, and intersection consumption kernels are pipelined on the GPU, there is no need to double or triple buffer the ray or intersection buffers, which saves memory.
--
-- id <MTLBuffer> rayBuffer =
-- [device newBufferWithLength:sizeof(Ray) * rayCount
-- options:MTLResourceStorageModePrivate];
-- id <MTLBuffer> intersectionBuffer =
-- [device newBufferWithLength:sizeof(Intersection) * rayCount
-- options:MTLResourceStorageModePrivate];
--
-- id <MTLCommandBuffer> commandBuffer = [commandQueue commandBuffer];
--
-- // Generate rays
-- id <MTLComputeCommandEncoder> encoder = [commandBuffer computeCommandEncoder];
--
-- [encoder setBuffer:rayBuffer offset:0 atIndex:0];
-- [encoder setBytes:&uniformData length:sizeof(uniformData) atIndex:1];
--
-- [encoder setComputePipelineState:cameraPipeline];
--
-- [encoder dispatchThreads:MTLSizeMake(rayCount, 1, 1)
-- threadsPerThreadgroup:MTLSizeMake(64, 1, 1)];
--
-- [encoder endEncoding];
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
-- // Perform shading at intersections and update framebuffer texture
-- encoder = [commandBuffer computeCommandEncoder];
--
-- [encoder setBuffer:rayBuffer offset:0 atIndex:0];
-- [encoder setBuffer:intersectionBuffer offset:0 atIndex:1];
-- [encoder setBytes:&uniformData length:sizeof(uniformData) atIndex:2];
--
-- [encoder setTexture:framebufferTexture atIndex:0];
--
-- [encoder setComputePipelineState:shadingPipeline];
--
-- [encoder dispatchThreads:MTLSizeMake(rayCount, 1, 1)
-- threadsPerThreadgroup:MTLSizeMake(64, 1, 1)];
--
-- [encoder endEncoding];
--
-- [commandBuffer commit];
--
-- Note that the intersection consumption kernel can in turn produce new rays that can be passed back to the MPSRayIntersector. This technique can be used to implement iterative techniques such as progressive path tracing without leaving the GPU. For example, the shading kernel in the example above could produce both a secondary ray that will be passed back to the raytracer in the next iteration as well as a shadow ray that will be used to sample the direct lighting. A final kernel can consume the shadow ray intersections to accumulate lighting contributions into the framebuffer.
--
-- There is an alternative version of the intersection test encoding method that does not accept a literal ray count. The ray count is instead fetched indirectly by the GPU. For example, this can be combined with a parallel reduction on the GPU to compact the ray buffer after each iteration as rays bounce out of the scene or are absorbed. Alternatively, setting the maximum distance of a ray to a negative number indicates that the ray has become inactive and causes the raytracer to ignore the ray.
--
-- [raytracer encodeIntersectionToCommandBuffer:commandBuffer
-- intersectionType:MPSIntersectionTypeNearest
-- rayBuffer:rayBuffer
-- rayBufferOffset:0
-- intersectionBuffer:intersectionBuffer
-- intersectionBufferOffset:0
-- rayCountBuffer:rayCountBuffer
-- rayCountBufferOffset:0
-- accelerationStructure:accelerationStructure];
--
-- Multi-GPU Raytracing: to implement multi-GPU raytracing, create the MPSRayIntersector and MPSAccelerationStructure objects first with one Metal device and copy them to the other Metal device(s). The raytracing process can then proceed independently on each GPU. For example, divide the output image into tiles or slices that are rendered independently. Then composite finished tiles or slices back together on one GPU and present the output image to the screen. The workload should be distributed across GPUs according to their performance to avoid a more powerful GPU idly waiting for a less powerful GPU to finish.
--
-- Acceleration Structure Serialization: MPSAccelerationStructure objects can be serialized and deserialized using the NSSecureCoding protocol. This can be used to build acceleration structures offline and reload them at runtime rather than building them from scratch.
--
-- Performance Guidelines:
--
-- - For vertex buffers, ray buffers, intersection buffers, etc., use private or managed       buffers rather than shared buffers when possible on discrete memory GPU architectures as       they are much faster than fetching data over the PCIe bus. If the CPU only writes once       to a ray buffer once and reads once from the intersection buffer, then a shared buffer may       be acceptable and avoids extra copies to and from the GPU. However, it is generally       preferable to generate and consume rays and intersections on the GPU instead, in which       case a private buffer should be used. Vertex data is typically static and reused many       times so it should be stored in private or managed buffers.
--
-- - If the CPU must generate and consume rays and intersections, use double or triple       buffering as described above. This avoids the CPU and GPU mutually blocking each other.
--
-- - In general, disable any unused features such as ray masks, backface culling,       etc. Enabling extra features increases the number of instructions and register usage of       the ray intersection kernel(s), reducing intersection performance. For example, it may be       more efficient to compute barycentric coordinates in your intersection consumption       kernel rather getting them from the raytracer. Use of an index buffer may also reduce       performance, so consider disabling the index buffer if there is enough memory available.
--
-- - Try to submit rays in large batches. This amortizes the costs involved in dispatching       work to the GPU and also allows the GPU to perform more effective latency hiding.       Use the recommendedMinimumRayBatchSizeForRayCount method to get an estimate of the       minimum recommended ray batch size. For this reason, small images or sample counts       may not perform as well as large images or sample counts. Note, however, that submitting       rays in very large batches can reduce the responsiveness of the system because the GPU       will be busy for long periods. Experiment to find a balance between raytracing throughput       and system responsiveness.
--
-- - When possible, organize rays within a batch for spatial locality. Rays that originate       at nearby points or are oriented in similar directions tend to access the same       locations in memory and can therefore make more effective use of the GPU's caches.       For example, the camera rays associated with nearby pixels in the output image will likely       originate at the same point and travel in very similar directions. Therefore, divide the       output image into small tiles (e.g., 8x8). Rather than laying out all of the rays in the       ray buffer in scanline order, first lay out the ray in scanline order within each tile,       then lay out the tiles in scanline order or according to some space filling curve.
--
-- - If CPU encode time is an issue, disable Metal API validation and enable       MPSKernelOptionsSkipAPIValidation.
--
-- - Choose the minimal ray and intersection data types for your use case. Loading and storing       extra values such as ray masks or primitive indices can reduce raytracing performance, so       use a simpler data type if they are not needed. For example, camera rays typically have no       need for a maximum distance field, while shadow rays do.
--
-- - Use MPSIntersectionTestTypeAny when possible: this is typically much faster than       MPSIntersectionTestTypeNearest and can be used when you only need to check for       binary visibility between two points such as shadow and ambient occlusion rays. Combine       this with MPSRayDataTypeDistance to minimize memory bandwidth usage.
--
-- - Try to keep the geometry, textures, ray buffers, etc. within the Metal device's       recommended working set size. Paging data into GPU memory can significantly reduce       raytracing performance.
--
-- - Changes to MPSRayIntersector properties can trigger internal pipeline compilations when       intersection tests are next encoded. If you need to avoid hitches due to pipeline       compilation, encode a small ray intersection with each raytracer configuration you will       use at encode-time. This creates and caches the corresponding pipelines.
--
-- - Disable rays which should not participate in intersection testing. This can be done either       by compacting the ray buffer such that it only contains valid rays, or by setting fields       of the ray struct to invalid values. For example, setting the maximum distance to a       negative value, setting the mask to zero, setting the direction to the zero vector, etc.       In particular, rays should NOT be disabled using schemes such as moving their origin       outside the scene. These rays will still partially traverse the acceleration structure,       potentially evicting data from the cache which could have been used by valid rays. Note       that it is preferable to provide only valid rays so that threads are not left idle if       their rays are found to be invalid, but it can be convenient to disable rays in place in       the ray buffer.
--
-- See MPSAccelerationStructure and MPSInstanceAccelerationStructure for more performance guidelines.
--
-- Thread Safety: MPSRayIntersectors are generally not thread safe: changing properties and encoding intersection tests from multiple threads result in undefined behavior. Instead, multiple threads should copy or create their own MPSRayIntersectors.
--
-- Generated bindings for @MPSRayIntersector@.
module ObjC.MetalPerformanceShaders.MPSRayIntersector
  ( MPSRayIntersector
  , IsMPSRayIntersector(..)
  , init_
  , initWithDevice
  , initWithCoder_device
  , copyWithZone_device
  , recommendedMinimumRayBatchSizeForRayCount
  , encodeWithCoder
  , encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_intersectionBuffer_intersectionBufferOffset_rayCount_accelerationStructure
  , encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_intersectionBuffer_intersectionBufferOffset_rayCountBuffer_rayCountBufferOffset_accelerationStructure
  , encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_rayIndexBuffer_rayIndexBufferOffset_intersectionBuffer_intersectionBufferOffset_rayIndexCount_accelerationStructure
  , encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_rayIndexBuffer_rayIndexBufferOffset_intersectionBuffer_intersectionBufferOffset_rayIndexCountBuffer_rayIndexCountBufferOffset_accelerationStructure
  , encodeIntersectionToCommandBuffer_intersectionType_rayTexture_intersectionTexture_accelerationStructure
  , cullMode
  , setCullMode
  , frontFacingWinding
  , setFrontFacingWinding
  , triangleIntersectionTestType
  , setTriangleIntersectionTestType
  , boundingBoxIntersectionTestType
  , setBoundingBoxIntersectionTestType
  , rayMaskOptions
  , setRayMaskOptions
  , rayMaskOperator
  , setRayMaskOperator
  , rayStride
  , setRayStride
  , intersectionStride
  , setIntersectionStride
  , rayDataType
  , setRayDataType
  , intersectionDataType
  , setIntersectionDataType
  , rayIndexDataType
  , setRayIndexDataType
  , rayMask
  , setRayMask
  , initSelector
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , copyWithZone_deviceSelector
  , recommendedMinimumRayBatchSizeForRayCountSelector
  , encodeWithCoderSelector
  , encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_intersectionBuffer_intersectionBufferOffset_rayCount_accelerationStructureSelector
  , encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_intersectionBuffer_intersectionBufferOffset_rayCountBuffer_rayCountBufferOffset_accelerationStructureSelector
  , encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_rayIndexBuffer_rayIndexBufferOffset_intersectionBuffer_intersectionBufferOffset_rayIndexCount_accelerationStructureSelector
  , encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_rayIndexBuffer_rayIndexBufferOffset_intersectionBuffer_intersectionBufferOffset_rayIndexCountBuffer_rayIndexCountBufferOffset_accelerationStructureSelector
  , encodeIntersectionToCommandBuffer_intersectionType_rayTexture_intersectionTexture_accelerationStructureSelector
  , cullModeSelector
  , setCullModeSelector
  , frontFacingWindingSelector
  , setFrontFacingWindingSelector
  , triangleIntersectionTestTypeSelector
  , setTriangleIntersectionTestTypeSelector
  , boundingBoxIntersectionTestTypeSelector
  , setBoundingBoxIntersectionTestTypeSelector
  , rayMaskOptionsSelector
  , setRayMaskOptionsSelector
  , rayMaskOperatorSelector
  , setRayMaskOperatorSelector
  , rayStrideSelector
  , setRayStrideSelector
  , intersectionStrideSelector
  , setIntersectionStrideSelector
  , rayDataTypeSelector
  , setRayDataTypeSelector
  , intersectionDataTypeSelector
  , setIntersectionDataTypeSelector
  , rayIndexDataTypeSelector
  , setRayIndexDataTypeSelector
  , rayMaskSelector
  , setRayMaskSelector

  -- * Enum types
  , MPSBoundingBoxIntersectionTestType(MPSBoundingBoxIntersectionTestType)
  , pattern MPSBoundingBoxIntersectionTestTypeDefault
  , pattern MPSBoundingBoxIntersectionTestTypeAxisAligned
  , pattern MPSBoundingBoxIntersectionTestTypeFast
  , MPSDataType(MPSDataType)
  , pattern MPSDataTypeInvalid
  , pattern MPSDataTypeFloatBit
  , pattern MPSDataTypeFloat32
  , pattern MPSDataTypeFloat16
  , pattern MPSDataTypeComplexBit
  , pattern MPSDataTypeComplexFloat32
  , pattern MPSDataTypeComplexFloat16
  , pattern MPSDataTypeSignedBit
  , pattern MPSDataTypeIntBit
  , pattern MPSDataTypeInt2
  , pattern MPSDataTypeInt4
  , pattern MPSDataTypeInt8
  , pattern MPSDataTypeInt16
  , pattern MPSDataTypeInt32
  , pattern MPSDataTypeInt64
  , pattern MPSDataTypeUInt2
  , pattern MPSDataTypeUInt4
  , pattern MPSDataTypeUInt8
  , pattern MPSDataTypeUInt16
  , pattern MPSDataTypeUInt32
  , pattern MPSDataTypeUInt64
  , pattern MPSDataTypeAlternateEncodingBit
  , pattern MPSDataTypeBool
  , pattern MPSDataTypeBFloat16
  , pattern MPSDataTypeNormalizedBit
  , pattern MPSDataTypeUnorm1
  , pattern MPSDataTypeUnorm8
  , MPSIntersectionDataType(MPSIntersectionDataType)
  , pattern MPSIntersectionDataTypeDistance
  , pattern MPSIntersectionDataTypeDistancePrimitiveIndex
  , pattern MPSIntersectionDataTypeDistancePrimitiveIndexCoordinates
  , pattern MPSIntersectionDataTypeDistancePrimitiveIndexInstanceIndex
  , pattern MPSIntersectionDataTypeDistancePrimitiveIndexInstanceIndexCoordinates
  , pattern MPSIntersectionDataTypeDistancePrimitiveIndexBufferIndex
  , pattern MPSIntersectionDataTypeDistancePrimitiveIndexBufferIndexCoordinates
  , pattern MPSIntersectionDataTypeDistancePrimitiveIndexBufferIndexInstanceIndex
  , pattern MPSIntersectionDataTypeDistancePrimitiveIndexBufferIndexInstanceIndexCoordinates
  , MPSIntersectionType(MPSIntersectionType)
  , pattern MPSIntersectionTypeNearest
  , pattern MPSIntersectionTypeAny
  , MPSRayDataType(MPSRayDataType)
  , pattern MPSRayDataTypeOriginDirection
  , pattern MPSRayDataTypeOriginMinDistanceDirectionMaxDistance
  , pattern MPSRayDataTypeOriginMaskDirectionMaxDistance
  , pattern MPSRayDataTypePackedOriginDirection
  , MPSRayMaskOperator(MPSRayMaskOperator)
  , pattern MPSRayMaskOperatorAnd
  , pattern MPSRayMaskOperatorNotAnd
  , pattern MPSRayMaskOperatorOr
  , pattern MPSRayMaskOperatorNotOr
  , pattern MPSRayMaskOperatorXor
  , pattern MPSRayMaskOperatorNotXor
  , pattern MPSRayMaskOperatorLessThan
  , pattern MPSRayMaskOperatorLessThanOrEqualTo
  , pattern MPSRayMaskOperatorGreaterThan
  , pattern MPSRayMaskOperatorGreaterThanOrEqualTo
  , pattern MPSRayMaskOperatorEqual
  , pattern MPSRayMaskOperatorNotEqual
  , MPSRayMaskOptions(MPSRayMaskOptions)
  , pattern MPSRayMaskOptionNone
  , pattern MPSRayMaskOptionPrimitive
  , pattern MPSRayMaskOptionInstance
  , MPSTriangleIntersectionTestType(MPSTriangleIntersectionTestType)
  , pattern MPSTriangleIntersectionTestTypeDefault
  , pattern MPSTriangleIntersectionTestTypeWatertight
  , MTLCullMode(MTLCullMode)
  , pattern MTLCullModeNone
  , pattern MTLCullModeFront
  , pattern MTLCullModeBack
  , MTLWinding(MTLWinding)
  , pattern MTLWindingClockwise
  , pattern MTLWindingCounterClockwise

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
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> IO (Id MPSRayIntersector)
init_ mpsRayIntersector  =
  sendMsg mpsRayIntersector (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initialize the raytracer with a Metal device
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> RawId -> IO (Id MPSRayIntersector)
initWithDevice mpsRayIntersector  device =
  sendMsg mpsRayIntersector (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize the raytracer with an NSCoder and a Metal device
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSRayIntersector mpsRayIntersector, IsNSCoder aDecoder) => mpsRayIntersector -> aDecoder -> RawId -> IO (Id MPSRayIntersector)
initWithCoder_device mpsRayIntersector  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsRayIntersector (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Copy the raytracer with a Metal device
--
-- @zone@ — The NSZone in which to allocate the object
--
-- @device@ — The Metal device for the new MPSRayIntersector
--
-- Returns: A pointer to a copy of this MPSRayIntersector
--
-- ObjC selector: @- copyWithZone:device:@
copyWithZone_device :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> Ptr () -> RawId -> IO (Id MPSRayIntersector)
copyWithZone_device mpsRayIntersector  zone device =
  sendMsg mpsRayIntersector (mkSelector "copyWithZone:device:") (retPtr retVoid) [argPtr zone, argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Get the recommended minimum number of rays to submit for intersection in one batch
--
-- In order to keep the system responsive, and to limit the amount of memory allocated to ray and intersection buffers, it may be desirable to divide the rays to be intersected against an acceleration structure into smaller batches. However, submitting too few rays in a batch reduces GPU utilization and performance. This method provides a recommended minimum number of rays to submit in any given batch. For example, for a 1920x1080 image, this method may recommend that the image be divided into 512x512 tiles. The actual recommendation varies per device and total ray count.
--
-- @rayCount@ — The total number of rays to be submitted
--
-- Returns: The recommended minimum ray batch size
--
-- ObjC selector: @- recommendedMinimumRayBatchSizeForRayCount:@
recommendedMinimumRayBatchSizeForRayCount :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> CULong -> IO CULong
recommendedMinimumRayBatchSizeForRayCount mpsRayIntersector  rayCount =
  sendMsg mpsRayIntersector (mkSelector "recommendedMinimumRayBatchSizeForRayCount:") retCULong [argCULong (fromIntegral rayCount)]

-- | @- encodeWithCoder:@
encodeWithCoder :: (IsMPSRayIntersector mpsRayIntersector, IsNSCoder coder) => mpsRayIntersector -> coder -> IO ()
encodeWithCoder mpsRayIntersector  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg mpsRayIntersector (mkSelector "encodeWithCoder:") retVoid [argPtr (castPtr raw_coder :: Ptr ())]

-- | Schedule intersection tests between rays and an acceleration structure
--
-- @commandBuffer@ — Command buffer to schedule intersection testing in
--
-- @intersectionType@ — Which type of intersection to test for
--
-- @rayBuffer@ — Buffer containing rays to intersect against the acceleration                                 structure. The ray data type is defined by the rayDataType                                 and rayStride properties.
--
-- @rayBufferOffset@ — Offset, in bytes, into the ray buffer. Must be a multiple of                                 the ray stride.
--
-- @intersectionBuffer@ — Buffer to store intersection in. Intersections are stored in                                 the same order as the ray buffer, one intersection per ray.                                 The intersection data type is defined by the                                 intersectionDataType and intersectionStride properties.
--
-- @intersectionBufferOffset@ — Offset, in bytes, into the intersection buffer. Must be a                                 multiple of the intersection stride.
--
-- @rayCount@ — Number of rays
--
-- @accelerationStructure@ — Acceleration structure to test against
--
-- ObjC selector: @- encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:intersectionBuffer:intersectionBufferOffset:rayCount:accelerationStructure:@
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_intersectionBuffer_intersectionBufferOffset_rayCount_accelerationStructure :: (IsMPSRayIntersector mpsRayIntersector, IsMPSAccelerationStructure accelerationStructure) => mpsRayIntersector -> RawId -> MPSIntersectionType -> RawId -> CULong -> RawId -> CULong -> CULong -> accelerationStructure -> IO ()
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_intersectionBuffer_intersectionBufferOffset_rayCount_accelerationStructure mpsRayIntersector  commandBuffer intersectionType rayBuffer rayBufferOffset intersectionBuffer intersectionBufferOffset rayCount accelerationStructure =
withObjCPtr accelerationStructure $ \raw_accelerationStructure ->
    sendMsg mpsRayIntersector (mkSelector "encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:intersectionBuffer:intersectionBufferOffset:rayCount:accelerationStructure:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argCULong (coerce intersectionType), argPtr (castPtr (unRawId rayBuffer) :: Ptr ()), argCULong (fromIntegral rayBufferOffset), argPtr (castPtr (unRawId intersectionBuffer) :: Ptr ()), argCULong (fromIntegral intersectionBufferOffset), argCULong (fromIntegral rayCount), argPtr (castPtr raw_accelerationStructure :: Ptr ())]

-- | Schedule intersection tests between rays and an acceleration structure with a ray count provided in a buffer
--
-- @commandBuffer@ — Command buffer to schedule intersection testing in
--
-- @intersectionType@ — Which type of intersection to test for
--
-- @rayBuffer@ — Buffer containing rays to intersect against the acceleration                                 structure. The ray data type is defined by the rayDataType                                 and rayStride properties.
--
-- @rayBufferOffset@ — Offset, in bytes, into the ray buffer. Must be a multiple of                                 the ray stride.
--
-- @intersectionBuffer@ — Buffer to store intersection in. Intersections are stored in                                 the same order as the ray buffer, one intersection per ray.                                 The intersection data type is defined by the                                 intersectionDataType and intersectionStride properties.
--
-- @intersectionBufferOffset@ — Offset, in bytes, into the intersection buffer. Must be a                                 multiple of the intersection stride.
--
-- @rayCountBuffer@ — Buffer containing number of rays as a 32 bit unsigned integer
--
-- @rayCountBufferOffset@ — Offset, in bytes, into the ray count buffer. Must be a multiple                                 of 4 bytes.
--
-- @accelerationStructure@ — Acceleration structure to test against
--
-- ObjC selector: @- encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:intersectionBuffer:intersectionBufferOffset:rayCountBuffer:rayCountBufferOffset:accelerationStructure:@
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_intersectionBuffer_intersectionBufferOffset_rayCountBuffer_rayCountBufferOffset_accelerationStructure :: (IsMPSRayIntersector mpsRayIntersector, IsMPSAccelerationStructure accelerationStructure) => mpsRayIntersector -> RawId -> MPSIntersectionType -> RawId -> CULong -> RawId -> CULong -> RawId -> CULong -> accelerationStructure -> IO ()
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_intersectionBuffer_intersectionBufferOffset_rayCountBuffer_rayCountBufferOffset_accelerationStructure mpsRayIntersector  commandBuffer intersectionType rayBuffer rayBufferOffset intersectionBuffer intersectionBufferOffset rayCountBuffer rayCountBufferOffset accelerationStructure =
withObjCPtr accelerationStructure $ \raw_accelerationStructure ->
    sendMsg mpsRayIntersector (mkSelector "encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:intersectionBuffer:intersectionBufferOffset:rayCountBuffer:rayCountBufferOffset:accelerationStructure:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argCULong (coerce intersectionType), argPtr (castPtr (unRawId rayBuffer) :: Ptr ()), argCULong (fromIntegral rayBufferOffset), argPtr (castPtr (unRawId intersectionBuffer) :: Ptr ()), argCULong (fromIntegral intersectionBufferOffset), argPtr (castPtr (unRawId rayCountBuffer) :: Ptr ()), argCULong (fromIntegral rayCountBufferOffset), argPtr (castPtr raw_accelerationStructure :: Ptr ())]

-- | Schedule intersection tests between rays and an acceleration structure
--
-- @commandBuffer@ — Command buffer to schedule intersection testing in
--
-- @intersectionType@ — Which type of intersection to test for
--
-- @rayBuffer@ — Buffer containing rays to intersect against the acceleration                                 structure. The ray data type is defined by the rayDataType                                 and rayStride properties.
--
-- @rayBufferOffset@ — Offset, in bytes, into the ray buffer. Must be a multiple of                                 the ray stride.
--
-- @rayIndexBuffer@ — Buffer containing ray indices. Each index references a ray in                                 the ray buffer. The ray index data type is controlled by the                                 rayIndexDataType property.
--
-- @rayIndexBufferOffset@ — Offset, in bytes, into the ray index buffer. Must be a multiple                                 of the stride of the ray index type.
--
-- @intersectionBuffer@ — Buffer to store intersection in. Intersections are stored in                                 the same order as the ray buffer, one intersection per ray.                                 The intersection data type is defined by the                                 intersectionDataType and intersectionStride properties.
--
-- @intersectionBufferOffset@ — Offset, in bytes, into the intersection buffer. Must be a                                 multiple of the intersection stride.
--
-- @rayIndexCount@ — Number of ray indices
--
-- @accelerationStructure@ — Acceleration structure to test against
--
-- ObjC selector: @- encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:rayIndexBuffer:rayIndexBufferOffset:intersectionBuffer:intersectionBufferOffset:rayIndexCount:accelerationStructure:@
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_rayIndexBuffer_rayIndexBufferOffset_intersectionBuffer_intersectionBufferOffset_rayIndexCount_accelerationStructure :: (IsMPSRayIntersector mpsRayIntersector, IsMPSAccelerationStructure accelerationStructure) => mpsRayIntersector -> RawId -> MPSIntersectionType -> RawId -> CULong -> RawId -> CULong -> RawId -> CULong -> CULong -> accelerationStructure -> IO ()
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_rayIndexBuffer_rayIndexBufferOffset_intersectionBuffer_intersectionBufferOffset_rayIndexCount_accelerationStructure mpsRayIntersector  commandBuffer intersectionType rayBuffer rayBufferOffset rayIndexBuffer rayIndexBufferOffset intersectionBuffer intersectionBufferOffset rayIndexCount accelerationStructure =
withObjCPtr accelerationStructure $ \raw_accelerationStructure ->
    sendMsg mpsRayIntersector (mkSelector "encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:rayIndexBuffer:rayIndexBufferOffset:intersectionBuffer:intersectionBufferOffset:rayIndexCount:accelerationStructure:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argCULong (coerce intersectionType), argPtr (castPtr (unRawId rayBuffer) :: Ptr ()), argCULong (fromIntegral rayBufferOffset), argPtr (castPtr (unRawId rayIndexBuffer) :: Ptr ()), argCULong (fromIntegral rayIndexBufferOffset), argPtr (castPtr (unRawId intersectionBuffer) :: Ptr ()), argCULong (fromIntegral intersectionBufferOffset), argCULong (fromIntegral rayIndexCount), argPtr (castPtr raw_accelerationStructure :: Ptr ())]

-- | Schedule intersection tests between rays and an acceleration structure with a ray count provided in a buffer
--
-- @commandBuffer@ — Command buffer to schedule intersection testing in
--
-- @intersectionType@ — Which type of intersection to test for
--
-- @rayBuffer@ — Buffer containing rays to intersect against the acceleration                                  structure. The ray data type is defined by the rayDataType                                  and rayStride properties.
--
-- @rayBufferOffset@ — Offset, in bytes, into the ray buffer. Must be a multiple of                                  the ray stride.
--
-- @rayIndexBuffer@ — Buffer containing ray indices. Each index references a ray in                                  the ray buffer. The ray index data type is controlled by the                                  rayIndexDataType property.
--
-- @rayIndexBufferOffset@ — Offset, in bytes, into the ray index buffer. Must be a multiple                                  of the stride of the ray index type.
--
-- @intersectionBuffer@ — Buffer to store intersection in. Intersections are stored in                                  the same order as the ray buffer, one intersection per ray.                                  The intersection data type is defined by the                                  intersectionDataType and intersectionStride properties.
--
-- @intersectionBufferOffset@ — Offset, in bytes, into the intersection buffer. Must be a                                  multiple of the intersection stride.
--
-- @rayIndexCountBuffer@ — Buffer containing number of rays as a 32 bit unsigned integer
--
-- @rayIndexCountBufferOffset@ — Offset, in bytes, into the ray count buffer. Must be a multiple                                  of 4 bytes.
--
-- @accelerationStructure@ — Acceleration structure to test against
--
-- ObjC selector: @- encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:rayIndexBuffer:rayIndexBufferOffset:intersectionBuffer:intersectionBufferOffset:rayIndexCountBuffer:rayIndexCountBufferOffset:accelerationStructure:@
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_rayIndexBuffer_rayIndexBufferOffset_intersectionBuffer_intersectionBufferOffset_rayIndexCountBuffer_rayIndexCountBufferOffset_accelerationStructure :: (IsMPSRayIntersector mpsRayIntersector, IsMPSAccelerationStructure accelerationStructure) => mpsRayIntersector -> RawId -> MPSIntersectionType -> RawId -> CULong -> RawId -> CULong -> RawId -> CULong -> RawId -> CULong -> accelerationStructure -> IO ()
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_rayIndexBuffer_rayIndexBufferOffset_intersectionBuffer_intersectionBufferOffset_rayIndexCountBuffer_rayIndexCountBufferOffset_accelerationStructure mpsRayIntersector  commandBuffer intersectionType rayBuffer rayBufferOffset rayIndexBuffer rayIndexBufferOffset intersectionBuffer intersectionBufferOffset rayIndexCountBuffer rayIndexCountBufferOffset accelerationStructure =
withObjCPtr accelerationStructure $ \raw_accelerationStructure ->
    sendMsg mpsRayIntersector (mkSelector "encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:rayIndexBuffer:rayIndexBufferOffset:intersectionBuffer:intersectionBufferOffset:rayIndexCountBuffer:rayIndexCountBufferOffset:accelerationStructure:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argCULong (coerce intersectionType), argPtr (castPtr (unRawId rayBuffer) :: Ptr ()), argCULong (fromIntegral rayBufferOffset), argPtr (castPtr (unRawId rayIndexBuffer) :: Ptr ()), argCULong (fromIntegral rayIndexBufferOffset), argPtr (castPtr (unRawId intersectionBuffer) :: Ptr ()), argCULong (fromIntegral intersectionBufferOffset), argPtr (castPtr (unRawId rayIndexCountBuffer) :: Ptr ()), argCULong (fromIntegral rayIndexCountBufferOffset), argPtr (castPtr raw_accelerationStructure :: Ptr ())]

-- | Schedule intersection tests between rays and an acceleration structure, where rays and loaded from a texture and intersections are stored into a texture.
--
-- This is convenient for hybrid rendering applications which produce ray data from a fragment shader. The ray and intersection texture must be 2D array textures. Ray data must be packed into consecutive channels and slices of the ray texture. Intersection data will be packed the same way. The ray and intersection data types are defined by the rayDataType and intersectionDataType properties. The rayStride and intersectionStride properties are ignored. Channels and slices beyond the required number are ignored when reading from the ray texture. Channels and slices beyond the required number are undefined when writing to the intersection texture.
--
-- For example, if the ray data type is MPSRayDataTypeOriginMaskDirectionMaxDistance, the ray texture must have pixel format MTLPixelFormatRGBA32Float and at least two array slices, packed as follows:
--
-- tex.write(float4(ray.position, as_type<float>(ray.mask)), pixel, 0); // slice 0
-- tex.write(float4(ray.direction, ray.maxDistance), pixel, 1);         // slice 1
-- @end
--
-- If the intersection data type is MPSIntersectionDataTypeDistance, the intersection texture may
-- have pixel format MTLPixelFormatR32Float with just a single channel and one array slice, and
-- should be unpacked as follows:
--
-- @code
-- float distance = tex.read(pixel, 0).x;
-- @end
--
-- On the other hand, if the intersection data type is
-- MPSIntersectionDistancePrimitiveIndexInstanceIndexCoordinates, the intersection texture must
-- have pixel format MTLPixelFormatRGBA32Float and at least two slices:
--
-- @code
-- float3 f0 = tex.read(pixel, 0);
--
-- float distance = f0.x;
-- unsigned int primitiveIndex = as_type<unsigned int>(f0.y);
-- unsigned int instanceIndex = as_type<unsigned int>(f0.z);
-- // w component is padding for this intersection data type
--
-- float2 coordinates = tex.read(pixel, 1).xy;
-- @end
--
-- @param commandBuffer            Command buffer to schedule intersection testing in
-- @param intersectionType         Which type of intersection to test for
-- @param rayTexture               A 2D array texture containing rays to intersect against the
-- acceleration structure. The ray data type is defined by the
-- rayDataType property.
-- @param intersectionTexture      Texture to store intersection in. Intersections are stored in
-- the same position as the ray texture, one intersection per ray.
-- The intersection data type is defined by the
-- intersectionDataType property.
-- @param accelerationStructure    Acceleration structure to test against
--
-- ObjC selector: @- encodeIntersectionToCommandBuffer:intersectionType:rayTexture:intersectionTexture:accelerationStructure:@
encodeIntersectionToCommandBuffer_intersectionType_rayTexture_intersectionTexture_accelerationStructure :: (IsMPSRayIntersector mpsRayIntersector, IsMPSAccelerationStructure accelerationStructure) => mpsRayIntersector -> RawId -> MPSIntersectionType -> RawId -> RawId -> accelerationStructure -> IO ()
encodeIntersectionToCommandBuffer_intersectionType_rayTexture_intersectionTexture_accelerationStructure mpsRayIntersector  commandBuffer intersectionType rayTexture intersectionTexture accelerationStructure =
withObjCPtr accelerationStructure $ \raw_accelerationStructure ->
    sendMsg mpsRayIntersector (mkSelector "encodeIntersectionToCommandBuffer:intersectionType:rayTexture:intersectionTexture:accelerationStructure:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argCULong (coerce intersectionType), argPtr (castPtr (unRawId rayTexture) :: Ptr ()), argPtr (castPtr (unRawId intersectionTexture) :: Ptr ()), argPtr (castPtr raw_accelerationStructure :: Ptr ())]

-- | Whether to ignore intersections between rays and back-facing or front-facing triangles or quadrilaterals. Defaults to MTLCullModeNone.
--
-- A triangle or quadrilateral is back-facing if its normal points in the same direction as a ray and front-facing if its normal points in the opposite direction as a ray. If the cull mode is set to MTLCullModeBack, then back-facing triangles and quadrilaterals will be ignored. If the cull mode is set to MTLCullModeFront, then front-facing triangles and quadrilaterals will be ignored. Otherwise, if the cull mode is set to MTLCullModeNone, no triangles or quadrilaterals will be ignored. The front and back faces can be swapped using the frontFacingWinding property.
--
-- Backface culling is necessary for some scenes but can reduce raytracing performance.
--
-- ObjC selector: @- cullMode@
cullMode :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> IO MTLCullMode
cullMode mpsRayIntersector  =
  fmap (coerce :: CULong -> MTLCullMode) $ sendMsg mpsRayIntersector (mkSelector "cullMode") retCULong []

-- | Whether to ignore intersections between rays and back-facing or front-facing triangles or quadrilaterals. Defaults to MTLCullModeNone.
--
-- A triangle or quadrilateral is back-facing if its normal points in the same direction as a ray and front-facing if its normal points in the opposite direction as a ray. If the cull mode is set to MTLCullModeBack, then back-facing triangles and quadrilaterals will be ignored. If the cull mode is set to MTLCullModeFront, then front-facing triangles and quadrilaterals will be ignored. Otherwise, if the cull mode is set to MTLCullModeNone, no triangles or quadrilaterals will be ignored. The front and back faces can be swapped using the frontFacingWinding property.
--
-- Backface culling is necessary for some scenes but can reduce raytracing performance.
--
-- ObjC selector: @- setCullMode:@
setCullMode :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> MTLCullMode -> IO ()
setCullMode mpsRayIntersector  value =
  sendMsg mpsRayIntersector (mkSelector "setCullMode:") retVoid [argCULong (coerce value)]

-- | Winding order used to determine which direction a triangle or quadrilateral's normal points when back face or front face culling is enabled. Defaults to MTLWindingClockwise.
--
-- If the front face winding is set to MTLWindingClockwise, the triangle or quadrilateral normal is considered to point towards the direction where the vertices are in clockwise order when viewed from that direction. Otherwise, if the front facing winding is set to MTLWindingCounterClockwise, the triangle or quadrilateral normal is considered to point in the opposite direction.
--
-- ObjC selector: @- frontFacingWinding@
frontFacingWinding :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> IO MTLWinding
frontFacingWinding mpsRayIntersector  =
  fmap (coerce :: CULong -> MTLWinding) $ sendMsg mpsRayIntersector (mkSelector "frontFacingWinding") retCULong []

-- | Winding order used to determine which direction a triangle or quadrilateral's normal points when back face or front face culling is enabled. Defaults to MTLWindingClockwise.
--
-- If the front face winding is set to MTLWindingClockwise, the triangle or quadrilateral normal is considered to point towards the direction where the vertices are in clockwise order when viewed from that direction. Otherwise, if the front facing winding is set to MTLWindingCounterClockwise, the triangle or quadrilateral normal is considered to point in the opposite direction.
--
-- ObjC selector: @- setFrontFacingWinding:@
setFrontFacingWinding :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> MTLWinding -> IO ()
setFrontFacingWinding mpsRayIntersector  value =
  sendMsg mpsRayIntersector (mkSelector "setFrontFacingWinding:") retVoid [argCULong (coerce value)]

-- | Ray/triangle intersection test type. Defaults to MPSTriangleIntersectionTestTypeDefault. Quads are broken into two triangles for intersection testing, so this property also applies to quadrilateral intersections.
--
-- ObjC selector: @- triangleIntersectionTestType@
triangleIntersectionTestType :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> IO MPSTriangleIntersectionTestType
triangleIntersectionTestType mpsRayIntersector  =
  fmap (coerce :: CULong -> MPSTriangleIntersectionTestType) $ sendMsg mpsRayIntersector (mkSelector "triangleIntersectionTestType") retCULong []

-- | Ray/triangle intersection test type. Defaults to MPSTriangleIntersectionTestTypeDefault. Quads are broken into two triangles for intersection testing, so this property also applies to quadrilateral intersections.
--
-- ObjC selector: @- setTriangleIntersectionTestType:@
setTriangleIntersectionTestType :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> MPSTriangleIntersectionTestType -> IO ()
setTriangleIntersectionTestType mpsRayIntersector  value =
  sendMsg mpsRayIntersector (mkSelector "setTriangleIntersectionTestType:") retVoid [argCULong (coerce value)]

-- | Ray/bounding box intersection test type. Defaults to MPSBoundingBoxIntersectionTestTypeDefault.
--
-- ObjC selector: @- boundingBoxIntersectionTestType@
boundingBoxIntersectionTestType :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> IO MPSBoundingBoxIntersectionTestType
boundingBoxIntersectionTestType mpsRayIntersector  =
  fmap (coerce :: CULong -> MPSBoundingBoxIntersectionTestType) $ sendMsg mpsRayIntersector (mkSelector "boundingBoxIntersectionTestType") retCULong []

-- | Ray/bounding box intersection test type. Defaults to MPSBoundingBoxIntersectionTestTypeDefault.
--
-- ObjC selector: @- setBoundingBoxIntersectionTestType:@
setBoundingBoxIntersectionTestType :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> MPSBoundingBoxIntersectionTestType -> IO ()
setBoundingBoxIntersectionTestType mpsRayIntersector  value =
  sendMsg mpsRayIntersector (mkSelector "setBoundingBoxIntersectionTestType:") retVoid [argCULong (coerce value)]

-- | Whether to enable primitive and instance masks. Defaults to MPSRayMaskOptionNone.
--
-- If MPSRayMaskOptionPrimitive or MPSRayMaskOptionInstance is enabled, each ray and primitive and/or instance is associated with a 32 bit unsigned integer mask. Before checking for intersection between a ray and a primitive or instance, the corresponding masks are compared using the ray mask operator defined by the rayMaskOperator property. If the result is zero, the intersection is skipped.
--
-- This can be used to make certain primitives or instances invisible to certain rays. For example, objects can be grouped into layers and their visibility can be toggled by modifying the ray masks rather than removing the objects from the scene and rebuilding the acceleration structure. Alternatively, certain objects can be prevented from casting shadows by making them invisible to shadow rays.
--
-- Enabling this option may reduce raytracing performance.
--
-- ObjC selector: @- rayMaskOptions@
rayMaskOptions :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> IO MPSRayMaskOptions
rayMaskOptions mpsRayIntersector  =
  fmap (coerce :: CULong -> MPSRayMaskOptions) $ sendMsg mpsRayIntersector (mkSelector "rayMaskOptions") retCULong []

-- | Whether to enable primitive and instance masks. Defaults to MPSRayMaskOptionNone.
--
-- If MPSRayMaskOptionPrimitive or MPSRayMaskOptionInstance is enabled, each ray and primitive and/or instance is associated with a 32 bit unsigned integer mask. Before checking for intersection between a ray and a primitive or instance, the corresponding masks are compared using the ray mask operator defined by the rayMaskOperator property. If the result is zero, the intersection is skipped.
--
-- This can be used to make certain primitives or instances invisible to certain rays. For example, objects can be grouped into layers and their visibility can be toggled by modifying the ray masks rather than removing the objects from the scene and rebuilding the acceleration structure. Alternatively, certain objects can be prevented from casting shadows by making them invisible to shadow rays.
--
-- Enabling this option may reduce raytracing performance.
--
-- ObjC selector: @- setRayMaskOptions:@
setRayMaskOptions :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> MPSRayMaskOptions -> IO ()
setRayMaskOptions mpsRayIntersector  value =
  sendMsg mpsRayIntersector (mkSelector "setRayMaskOptions:") retVoid [argCULong (coerce value)]

-- | The operator to apply to determine whether to accept an intersection between a ray and a primitive or instance. Defaults to MPSRayMaskOperatorAnd.
--
-- ObjC selector: @- rayMaskOperator@
rayMaskOperator :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> IO MPSRayMaskOperator
rayMaskOperator mpsRayIntersector  =
  fmap (coerce :: CULong -> MPSRayMaskOperator) $ sendMsg mpsRayIntersector (mkSelector "rayMaskOperator") retCULong []

-- | The operator to apply to determine whether to accept an intersection between a ray and a primitive or instance. Defaults to MPSRayMaskOperatorAnd.
--
-- ObjC selector: @- setRayMaskOperator:@
setRayMaskOperator :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> MPSRayMaskOperator -> IO ()
setRayMaskOperator mpsRayIntersector  value =
  sendMsg mpsRayIntersector (mkSelector "setRayMaskOperator:") retVoid [argCULong (coerce value)]

-- | Offset, in bytes, between consecutive rays in the ray buffer. Defaults to 0, indicating that the rays are packed according to their natural aligned size.
--
-- This can be used to skip past any additional per-ray data that may be stored alongside the MPSRay struct such as the current radiance along the ray or the source pixel coordinates. Must be aligned to the alignment of the ray data type.
--
-- ObjC selector: @- rayStride@
rayStride :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> IO CULong
rayStride mpsRayIntersector  =
  sendMsg mpsRayIntersector (mkSelector "rayStride") retCULong []

-- | Offset, in bytes, between consecutive rays in the ray buffer. Defaults to 0, indicating that the rays are packed according to their natural aligned size.
--
-- This can be used to skip past any additional per-ray data that may be stored alongside the MPSRay struct such as the current radiance along the ray or the source pixel coordinates. Must be aligned to the alignment of the ray data type.
--
-- ObjC selector: @- setRayStride:@
setRayStride :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> CULong -> IO ()
setRayStride mpsRayIntersector  value =
  sendMsg mpsRayIntersector (mkSelector "setRayStride:") retVoid [argCULong (fromIntegral value)]

-- | Offset, in bytes, between consecutive intersections in the intersection buffer. Defaults to 0, indicating that the intersections are packed according to their natural aligned size.
--
-- This can be used to skip past any additional per-intersection that which may be stored alongside the MPSRayIntersection struct such as the surface normal at the point of intersection. Must be aligned to the alignment of the intersection data type.
--
-- ObjC selector: @- intersectionStride@
intersectionStride :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> IO CULong
intersectionStride mpsRayIntersector  =
  sendMsg mpsRayIntersector (mkSelector "intersectionStride") retCULong []

-- | Offset, in bytes, between consecutive intersections in the intersection buffer. Defaults to 0, indicating that the intersections are packed according to their natural aligned size.
--
-- This can be used to skip past any additional per-intersection that which may be stored alongside the MPSRayIntersection struct such as the surface normal at the point of intersection. Must be aligned to the alignment of the intersection data type.
--
-- ObjC selector: @- setIntersectionStride:@
setIntersectionStride :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> CULong -> IO ()
setIntersectionStride mpsRayIntersector  value =
  sendMsg mpsRayIntersector (mkSelector "setIntersectionStride:") retVoid [argCULong (fromIntegral value)]

-- | Ray data type. Defaults to MPSRayDataTypeOriginDirection.
--
-- ObjC selector: @- rayDataType@
rayDataType :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> IO MPSRayDataType
rayDataType mpsRayIntersector  =
  fmap (coerce :: CULong -> MPSRayDataType) $ sendMsg mpsRayIntersector (mkSelector "rayDataType") retCULong []

-- | Ray data type. Defaults to MPSRayDataTypeOriginDirection.
--
-- ObjC selector: @- setRayDataType:@
setRayDataType :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> MPSRayDataType -> IO ()
setRayDataType mpsRayIntersector  value =
  sendMsg mpsRayIntersector (mkSelector "setRayDataType:") retVoid [argCULong (coerce value)]

-- | Intersection data type. Defaults to MPSIntersectionDataTypeDistancePrimitiveIndexCoordinates.
--
-- ObjC selector: @- intersectionDataType@
intersectionDataType :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> IO MPSIntersectionDataType
intersectionDataType mpsRayIntersector  =
  fmap (coerce :: CULong -> MPSIntersectionDataType) $ sendMsg mpsRayIntersector (mkSelector "intersectionDataType") retCULong []

-- | Intersection data type. Defaults to MPSIntersectionDataTypeDistancePrimitiveIndexCoordinates.
--
-- ObjC selector: @- setIntersectionDataType:@
setIntersectionDataType :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> MPSIntersectionDataType -> IO ()
setIntersectionDataType mpsRayIntersector  value =
  sendMsg mpsRayIntersector (mkSelector "setIntersectionDataType:") retVoid [argCULong (coerce value)]

-- | Ray index data type. Defaults to MPSDataTypeUInt32. Only MPSDataTypeUInt16 and MPSDataTypeUInt32 are supported.
--
-- ObjC selector: @- rayIndexDataType@
rayIndexDataType :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> IO MPSDataType
rayIndexDataType mpsRayIntersector  =
  fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsRayIntersector (mkSelector "rayIndexDataType") retCUInt []

-- | Ray index data type. Defaults to MPSDataTypeUInt32. Only MPSDataTypeUInt16 and MPSDataTypeUInt32 are supported.
--
-- ObjC selector: @- setRayIndexDataType:@
setRayIndexDataType :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> MPSDataType -> IO ()
setRayIndexDataType mpsRayIntersector  value =
  sendMsg mpsRayIntersector (mkSelector "setRayIndexDataType:") retVoid [argCUInt (coerce value)]

-- | Global ray mask. Defaults to 0xFFFFFFFF. This value will be logically AND-ed with the per-ray mask if the ray data type contains a mask.
--
-- ObjC selector: @- rayMask@
rayMask :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> IO CUInt
rayMask mpsRayIntersector  =
  sendMsg mpsRayIntersector (mkSelector "rayMask") retCUInt []

-- | Global ray mask. Defaults to 0xFFFFFFFF. This value will be logically AND-ed with the per-ray mask if the ray data type contains a mask.
--
-- ObjC selector: @- setRayMask:@
setRayMask :: IsMPSRayIntersector mpsRayIntersector => mpsRayIntersector -> CUInt -> IO ()
setRayMask mpsRayIntersector  value =
  sendMsg mpsRayIntersector (mkSelector "setRayMask:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @recommendedMinimumRayBatchSizeForRayCount:@
recommendedMinimumRayBatchSizeForRayCountSelector :: Selector
recommendedMinimumRayBatchSizeForRayCountSelector = mkSelector "recommendedMinimumRayBatchSizeForRayCount:"

-- | @Selector@ for @encodeWithCoder:@
encodeWithCoderSelector :: Selector
encodeWithCoderSelector = mkSelector "encodeWithCoder:"

-- | @Selector@ for @encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:intersectionBuffer:intersectionBufferOffset:rayCount:accelerationStructure:@
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_intersectionBuffer_intersectionBufferOffset_rayCount_accelerationStructureSelector :: Selector
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_intersectionBuffer_intersectionBufferOffset_rayCount_accelerationStructureSelector = mkSelector "encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:intersectionBuffer:intersectionBufferOffset:rayCount:accelerationStructure:"

-- | @Selector@ for @encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:intersectionBuffer:intersectionBufferOffset:rayCountBuffer:rayCountBufferOffset:accelerationStructure:@
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_intersectionBuffer_intersectionBufferOffset_rayCountBuffer_rayCountBufferOffset_accelerationStructureSelector :: Selector
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_intersectionBuffer_intersectionBufferOffset_rayCountBuffer_rayCountBufferOffset_accelerationStructureSelector = mkSelector "encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:intersectionBuffer:intersectionBufferOffset:rayCountBuffer:rayCountBufferOffset:accelerationStructure:"

-- | @Selector@ for @encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:rayIndexBuffer:rayIndexBufferOffset:intersectionBuffer:intersectionBufferOffset:rayIndexCount:accelerationStructure:@
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_rayIndexBuffer_rayIndexBufferOffset_intersectionBuffer_intersectionBufferOffset_rayIndexCount_accelerationStructureSelector :: Selector
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_rayIndexBuffer_rayIndexBufferOffset_intersectionBuffer_intersectionBufferOffset_rayIndexCount_accelerationStructureSelector = mkSelector "encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:rayIndexBuffer:rayIndexBufferOffset:intersectionBuffer:intersectionBufferOffset:rayIndexCount:accelerationStructure:"

-- | @Selector@ for @encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:rayIndexBuffer:rayIndexBufferOffset:intersectionBuffer:intersectionBufferOffset:rayIndexCountBuffer:rayIndexCountBufferOffset:accelerationStructure:@
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_rayIndexBuffer_rayIndexBufferOffset_intersectionBuffer_intersectionBufferOffset_rayIndexCountBuffer_rayIndexCountBufferOffset_accelerationStructureSelector :: Selector
encodeIntersectionToCommandBuffer_intersectionType_rayBuffer_rayBufferOffset_rayIndexBuffer_rayIndexBufferOffset_intersectionBuffer_intersectionBufferOffset_rayIndexCountBuffer_rayIndexCountBufferOffset_accelerationStructureSelector = mkSelector "encodeIntersectionToCommandBuffer:intersectionType:rayBuffer:rayBufferOffset:rayIndexBuffer:rayIndexBufferOffset:intersectionBuffer:intersectionBufferOffset:rayIndexCountBuffer:rayIndexCountBufferOffset:accelerationStructure:"

-- | @Selector@ for @encodeIntersectionToCommandBuffer:intersectionType:rayTexture:intersectionTexture:accelerationStructure:@
encodeIntersectionToCommandBuffer_intersectionType_rayTexture_intersectionTexture_accelerationStructureSelector :: Selector
encodeIntersectionToCommandBuffer_intersectionType_rayTexture_intersectionTexture_accelerationStructureSelector = mkSelector "encodeIntersectionToCommandBuffer:intersectionType:rayTexture:intersectionTexture:accelerationStructure:"

-- | @Selector@ for @cullMode@
cullModeSelector :: Selector
cullModeSelector = mkSelector "cullMode"

-- | @Selector@ for @setCullMode:@
setCullModeSelector :: Selector
setCullModeSelector = mkSelector "setCullMode:"

-- | @Selector@ for @frontFacingWinding@
frontFacingWindingSelector :: Selector
frontFacingWindingSelector = mkSelector "frontFacingWinding"

-- | @Selector@ for @setFrontFacingWinding:@
setFrontFacingWindingSelector :: Selector
setFrontFacingWindingSelector = mkSelector "setFrontFacingWinding:"

-- | @Selector@ for @triangleIntersectionTestType@
triangleIntersectionTestTypeSelector :: Selector
triangleIntersectionTestTypeSelector = mkSelector "triangleIntersectionTestType"

-- | @Selector@ for @setTriangleIntersectionTestType:@
setTriangleIntersectionTestTypeSelector :: Selector
setTriangleIntersectionTestTypeSelector = mkSelector "setTriangleIntersectionTestType:"

-- | @Selector@ for @boundingBoxIntersectionTestType@
boundingBoxIntersectionTestTypeSelector :: Selector
boundingBoxIntersectionTestTypeSelector = mkSelector "boundingBoxIntersectionTestType"

-- | @Selector@ for @setBoundingBoxIntersectionTestType:@
setBoundingBoxIntersectionTestTypeSelector :: Selector
setBoundingBoxIntersectionTestTypeSelector = mkSelector "setBoundingBoxIntersectionTestType:"

-- | @Selector@ for @rayMaskOptions@
rayMaskOptionsSelector :: Selector
rayMaskOptionsSelector = mkSelector "rayMaskOptions"

-- | @Selector@ for @setRayMaskOptions:@
setRayMaskOptionsSelector :: Selector
setRayMaskOptionsSelector = mkSelector "setRayMaskOptions:"

-- | @Selector@ for @rayMaskOperator@
rayMaskOperatorSelector :: Selector
rayMaskOperatorSelector = mkSelector "rayMaskOperator"

-- | @Selector@ for @setRayMaskOperator:@
setRayMaskOperatorSelector :: Selector
setRayMaskOperatorSelector = mkSelector "setRayMaskOperator:"

-- | @Selector@ for @rayStride@
rayStrideSelector :: Selector
rayStrideSelector = mkSelector "rayStride"

-- | @Selector@ for @setRayStride:@
setRayStrideSelector :: Selector
setRayStrideSelector = mkSelector "setRayStride:"

-- | @Selector@ for @intersectionStride@
intersectionStrideSelector :: Selector
intersectionStrideSelector = mkSelector "intersectionStride"

-- | @Selector@ for @setIntersectionStride:@
setIntersectionStrideSelector :: Selector
setIntersectionStrideSelector = mkSelector "setIntersectionStride:"

-- | @Selector@ for @rayDataType@
rayDataTypeSelector :: Selector
rayDataTypeSelector = mkSelector "rayDataType"

-- | @Selector@ for @setRayDataType:@
setRayDataTypeSelector :: Selector
setRayDataTypeSelector = mkSelector "setRayDataType:"

-- | @Selector@ for @intersectionDataType@
intersectionDataTypeSelector :: Selector
intersectionDataTypeSelector = mkSelector "intersectionDataType"

-- | @Selector@ for @setIntersectionDataType:@
setIntersectionDataTypeSelector :: Selector
setIntersectionDataTypeSelector = mkSelector "setIntersectionDataType:"

-- | @Selector@ for @rayIndexDataType@
rayIndexDataTypeSelector :: Selector
rayIndexDataTypeSelector = mkSelector "rayIndexDataType"

-- | @Selector@ for @setRayIndexDataType:@
setRayIndexDataTypeSelector :: Selector
setRayIndexDataTypeSelector = mkSelector "setRayIndexDataType:"

-- | @Selector@ for @rayMask@
rayMaskSelector :: Selector
rayMaskSelector = mkSelector "rayMask"

-- | @Selector@ for @setRayMask:@
setRayMaskSelector :: Selector
setRayMaskSelector = mkSelector "setRayMask:"

