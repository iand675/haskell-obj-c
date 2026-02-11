{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSState
--
-- This depends on Metal Framework
--
-- A semi-opaque data container for large storage in MPS CNN filters
--
-- Some MPS CNN kernels produce additional information beyond a                  MPSImage. These may be pooling indices where the result came from,                  convolution weights, or other information not contained in the                  usual MPSImage result from a MPSCNNKernel. A MPSState object                   typically contains one or more expensive MTLResources such as                   textures or buffers to store this information.  It provides a                   base class with interfaces for managing this storage. Child                   classes may add additional functionality specific to their                  contents.
--
-- Some MPSState objects are temporary. Temporary state objects,                   like MPSTemporaryImages and Matrices, are for very short lived storage,                  perhaps just a few lines of code within the scope of a single                  MTLCommandBuffer.  They are very efficient for storage, as several                  temporary objects can share the same memory over the course of a                   MTLCommandBuffer. This can improve both memory usage and time spent                  in the kernel wiring down memory and such. You may find that some                   large CNN tasks can not be computed without them, as non-temporary                  storage would simply take up too much memory.
--
-- In exchange, the lifetime of the underlying storage in temporary                   MPSState objects needs to be carefully managed. ARC often waits                   until the end of scope to release objects. Temporary storage often                   needs to be released sooner than that. Consequently the lifetime of                   the data in the underlying MTLResources is managed by a readCount                   property. Each time a MPSCNNKernel reads a temporary MPSState object                  the readCount is automatically decremented. When it reaches zero, the                  underlying storage is recycled for use by other MPS temporary objects,                  and the data is becomes undefined.  If you need to consume the data                  multiple times, you should set the readCount to a larger number to                   prevent the data from becomming undefined.  You may set the readCount                  to 0 yourself to return the storage to MPS, if for any reason, you                   realize that the MPSState object will no longer be used.
--
-- The contents of a temporary MPSState object are only valid from                   creation to the time the readCount reaches 0. The data is only valid                   for the MTLCommandBuffer on which it was created.  Non-temporary                  MPSState objects are valid on any MTLCommandBuffer on the same                  device until they are released.
--
-- Finally, temporary MPSState objects are complicated to use with blit encoders.                  Your application should assume that the temporary MPSState is backed by a MTLHeap,                  and consequently needs a MTLFence to ensure that compute command encoders and other                  encoders do not trip over one another with heap based memory. MPS will almost never                  use a blit encoder for this reason. If you do need one, then you will need to make                  a new compute encoder to block on whatever previous compute encoder last used the                  heap block. (MPS will not tell you who previously used the heap block. That encoder                  is almost certainly long dead anyway.) If concurrent encoders are involved, then a                  barrier might be needed. Within that compute encoder, you will call -updateFence.                  End the compute encoder, make a blit encoder wait for the fence, do the blit, update                  a new fence, then make a new compute encoder, wait for the second fence, then you                  can continue. Possibly the second do-nothing compute encoder needs to be ended so                  MPS can be called. Frankly, we don't bother with blit encoders and just write a compute                  operation for copy / clear as needed, or better yet find a way to eliminate the                  clear / copy pass so we don't have to pay for it. Your application needs to use                  temporary MPSStates and MPSTemporaryImages. Memory costs skyrocket, otherwise.                  It is the blit encoder that is hopefully optional. Note: the most common use of a                  blit encoder, -synchronizeResource: can not encounter this problem because temporary                  images and states live in GPU private memory and can not be read by the CPU.
--
-- Generated bindings for @MPSState@.
module ObjC.MetalPerformanceShaders.MPSState
  ( MPSState
  , IsMPSState(..)
  , temporaryStateWithCommandBuffer_bufferSize
  , temporaryStateWithCommandBuffer_textureDescriptor
  , temporaryStateWithCommandBuffer
  , initWithDevice_bufferSize
  , initWithDevice_textureDescriptor
  , initWithResource
  , init_
  , initWithDevice_resourceList
  , temporaryStateWithCommandBuffer_resourceList
  , initWithResources
  , resourceAtIndex_allocateMemory
  , bufferSizeAtIndex
  , resourceTypeAtIndex
  , synchronizeOnCommandBuffer
  , resourceSize
  , destinationImageDescriptorForSourceImages_sourceStates_forKernel_suggestedDescriptor
  , resourceCount
  , readCount
  , setReadCount
  , isTemporary
  , label
  , setLabel
  , temporaryStateWithCommandBuffer_bufferSizeSelector
  , temporaryStateWithCommandBuffer_textureDescriptorSelector
  , temporaryStateWithCommandBufferSelector
  , initWithDevice_bufferSizeSelector
  , initWithDevice_textureDescriptorSelector
  , initWithResourceSelector
  , initSelector
  , initWithDevice_resourceListSelector
  , temporaryStateWithCommandBuffer_resourceListSelector
  , initWithResourcesSelector
  , resourceAtIndex_allocateMemorySelector
  , bufferSizeAtIndexSelector
  , resourceTypeAtIndexSelector
  , synchronizeOnCommandBufferSelector
  , resourceSizeSelector
  , destinationImageDescriptorForSourceImages_sourceStates_forKernel_suggestedDescriptorSelector
  , resourceCountSelector
  , readCountSelector
  , setReadCountSelector
  , isTemporarySelector
  , labelSelector
  , setLabelSelector

  -- * Enum types
  , MPSStateResourceType(MPSStateResourceType)
  , pattern MPSStateResourceTypeNone
  , pattern MPSStateResourceTypeBuffer
  , pattern MPSStateResourceTypeTexture

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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Structs
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.Metal.Internal.Classes

-- | Create a MPSState holding a temporary MTLBuffer
--
-- @cmdBuf@ — The command buffer against which the temporary resource is allocated
--
-- @bufferSize@ — The size of the buffer in bytes
--
-- ObjC selector: @+ temporaryStateWithCommandBuffer:bufferSize:@
temporaryStateWithCommandBuffer_bufferSize :: RawId -> CULong -> IO (Id MPSState)
temporaryStateWithCommandBuffer_bufferSize cmdBuf bufferSize =
  do
    cls' <- getRequiredClass "MPSState"
    sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:bufferSize:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argCULong (fromIntegral bufferSize)] >>= retainedObject . castPtr

-- | Create a MPSState holding a temporary MTLTexture
--
-- @cmdBuf@ — The command buffer against which the temporary resource is allocated
--
-- @descriptor@ — A descriptor for the new temporary texture
--
-- ObjC selector: @+ temporaryStateWithCommandBuffer:textureDescriptor:@
temporaryStateWithCommandBuffer_textureDescriptor :: IsMTLTextureDescriptor descriptor => RawId -> descriptor -> IO (Id MPSState)
temporaryStateWithCommandBuffer_textureDescriptor cmdBuf descriptor =
  do
    cls' <- getRequiredClass "MPSState"
    withObjCPtr descriptor $ \raw_descriptor ->
      sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:textureDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new autoreleased temporary state object without underlying resource
--
-- @cmdBuf@ — The command buffer with which the temporary resource is associated
--
-- ObjC selector: @+ temporaryStateWithCommandBuffer:@
temporaryStateWithCommandBuffer :: RawId -> IO (Id MPSState)
temporaryStateWithCommandBuffer cmdBuf =
  do
    cls' <- getRequiredClass "MPSState"
    sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithDevice:bufferSize:@
initWithDevice_bufferSize :: IsMPSState mpsState => mpsState -> RawId -> CULong -> IO (Id MPSState)
initWithDevice_bufferSize mpsState  device bufferSize =
  sendMsg mpsState (mkSelector "initWithDevice:bufferSize:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral bufferSize)] >>= ownedObject . castPtr

-- | @- initWithDevice:textureDescriptor:@
initWithDevice_textureDescriptor :: (IsMPSState mpsState, IsMTLTextureDescriptor descriptor) => mpsState -> RawId -> descriptor -> IO (Id MPSState)
initWithDevice_textureDescriptor mpsState  device descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpsState (mkSelector "initWithDevice:textureDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Create a MPSState with a non-temporary MTLResource
--
-- @resource@ — A MTLBuffer or MTLTexture. May be nil.
--
-- ObjC selector: @- initWithResource:@
initWithResource :: IsMPSState mpsState => mpsState -> RawId -> IO (Id MPSState)
initWithResource mpsState  resource =
  sendMsg mpsState (mkSelector "initWithResource:") (retPtr retVoid) [argPtr (castPtr (unRawId resource) :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPSState mpsState => mpsState -> IO (Id MPSState)
init_ mpsState  =
  sendMsg mpsState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initialize a non-temporary state to hold a number of textures and buffers
--
-- The allocation of each resource will be deferred  until it is needed.                  This occurs when -resource or -resourceAtIndex: is called.
--
-- @resourceList@ — The list of resources to create.
--
-- ObjC selector: @- initWithDevice:resourceList:@
initWithDevice_resourceList :: (IsMPSState mpsState, IsMPSStateResourceList resourceList) => mpsState -> RawId -> resourceList -> IO (Id MPSState)
initWithDevice_resourceList mpsState  device resourceList =
withObjCPtr resourceList $ \raw_resourceList ->
    sendMsg mpsState (mkSelector "initWithDevice:resourceList:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_resourceList :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize a temporary state to hold a number of textures and buffers
--
-- The textures occur first in sequence
--
-- ObjC selector: @+ temporaryStateWithCommandBuffer:resourceList:@
temporaryStateWithCommandBuffer_resourceList :: IsMPSStateResourceList resourceList => RawId -> resourceList -> IO (Id MPSState)
temporaryStateWithCommandBuffer_resourceList commandBuffer resourceList =
  do
    cls' <- getRequiredClass "MPSState"
    withObjCPtr resourceList $ \raw_resourceList ->
      sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:resourceList:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_resourceList :: Ptr ())] >>= retainedObject . castPtr

-- | Create a state object with a list of MTLResources
--
-- Because MPS prefers deferred allocation of resources                  your application should use -initWithTextures:bufferSizes:bufferCount:                  whenever possible. This method is useful for cases when the                  MTLResources must be initialized by the CPU.
--
-- ObjC selector: @- initWithResources:@
initWithResources :: (IsMPSState mpsState, IsNSArray resources) => mpsState -> resources -> IO (Id MPSState)
initWithResources mpsState  resources =
withObjCPtr resources $ \raw_resources ->
    sendMsg mpsState (mkSelector "initWithResources:") (retPtr retVoid) [argPtr (castPtr raw_resources :: Ptr ())] >>= ownedObject . castPtr

-- | Get the MTLResource at the indicated index
--
-- By convention, except where otherwise documented, the MTLResources               held by the MPSState are private to the MPSState object, owned               by the MPSState subclass author. If the MPSState subclass               author is MPS, then the identity (e.g. texture vs. buffer)               and information contained in the resource should be considered               implementation dependent. It may change by operating system               version or device. If you are the author of the subclass then it               is for your own use, and MPS will not look at it, except perhaps               so as to pass it to a custom kernel.  Otherwise, the method is made               available to facilitate debugging and to allow you to write your own               state objects. Provide accessors to read this information               in a defined format.
--
-- @index@ — The index of the MTLResource to retrieve
--
-- @allocateMemory@ — It is very important to avoid allocating memory to hold               MTLResources until it is absolutely necessary, especially when working               with temporary MPSStates. When allocateMemory is set to NO and the               resource has not yet been allocated, nil will be returned instead.               If you just need information about the resource such as buffer size               or MTLTexture properties, but not the resource itself, please use               -bufferSizeAtIndex: or -textureInfoAtIndex: instead, as these will               not force the creation of the MTLResource.
--
-- ObjC selector: @- resourceAtIndex:allocateMemory:@
resourceAtIndex_allocateMemory :: IsMPSState mpsState => mpsState -> CULong -> Bool -> IO RawId
resourceAtIndex_allocateMemory mpsState  index allocateMemory =
  fmap (RawId . castPtr) $ sendMsg mpsState (mkSelector "resourceAtIndex:allocateMemory:") (retPtr retVoid) [argCULong (fromIntegral index), argCULong (if allocateMemory then 1 else 0)]

-- | Return the buffer size of the MTLBuffer at index or 0 if it is not a MTLBuffer
--
-- Does not force allocation of the MTLResource
--
-- ObjC selector: @- bufferSizeAtIndex:@
bufferSizeAtIndex :: IsMPSState mpsState => mpsState -> CULong -> IO CULong
bufferSizeAtIndex mpsState  index =
  sendMsg mpsState (mkSelector "bufferSizeAtIndex:") retCULong [argCULong (fromIntegral index)]

-- | Return YES if the resource at index is a buffer
--
-- Does not force allocation of the MTLResource
--
-- ObjC selector: @- resourceTypeAtIndex:@
resourceTypeAtIndex :: IsMPSState mpsState => mpsState -> CULong -> IO MPSStateResourceType
resourceTypeAtIndex mpsState  index =
  fmap (coerce :: CULong -> MPSStateResourceType) $ sendMsg mpsState (mkSelector "resourceTypeAtIndex:") retCULong [argCULong (fromIntegral index)]

-- | Flush any copy of MTLResources held by the state from the device's caches, and invalidate any CPU caches if needed.
--
-- This will call [id <MTLBlitEncoder> synchronizeResource: ] on the state's MTLResources.              This is necessary for all MTLStorageModeManaged resources. For other resources, including temporary              resources (these are all MTLStorageModePrivate), nothing is done.
--
-- @commandBuffer@ — The commandbuffer on which to synchronize
--
-- ObjC selector: @- synchronizeOnCommandBuffer:@
synchronizeOnCommandBuffer :: IsMPSState mpsState => mpsState -> RawId -> IO ()
synchronizeOnCommandBuffer mpsState  commandBuffer =
  sendMsg mpsState (mkSelector "synchronizeOnCommandBuffer:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ())]

-- | Get the number of bytes used to allocate underyling MTLResources
--
-- This is the size of the backing store of underlying MTLResources.                  It does not include all storage used by the object, for example                  the storage used to hold the MPSState instantiation and MTLTexture                  or MTLBuffer is not included. It only measures the size of the                  allocation used to hold the texels in the texture or bytes in the                  buffer. This value is subject to change between different devices                  and operating systems.
--
-- Except when -initWithResource: is used, most MPSStates are allocated                  without a backing store. The backing store is allocated lazily when                  it is needed, typically when the .texture property is called.                  Consequently, in most cases, it should be inexpensive to make                  a MPSImage to see how much memory it will need, and release it                  if it is too large.
--
-- This method may fail in certain circumstances, such as when the                  MPSImage is created with -initWithTexture:featureChannels:, in                  which case 0 will be returned.
--
-- ObjC selector: @- resourceSize@
resourceSize :: IsMPSState mpsState => mpsState -> IO CULong
resourceSize mpsState  =
  sendMsg mpsState (mkSelector "resourceSize") retCULong []

-- | Determine padding and sizing of result images
--
-- A MPSState has the opportunity to reconfigure the MPSImageDescriptor                  used to create the filter result state and set the MPSKernel.offset                  to the correct value.  By default, the MPSState does not modify the                  descriptor.
--
-- There is a order of operations defined for who may update the descriptor:
--
-- 1) Default padding code runs based on the MPSNNPaddingMethod in                          the MPSCNNKernel.padding. This creates the descriptor and                          picks a starting value for the MPSCNNKernel.offset.                      2) MPSStates are called in order to apply this function and update                          the offset.                      3) The MPSNNPadding custom padding method of the same name is called.                      4)
--
-- Some code that may prove helpful:
--
-- const int centeringPolicy = 0;  // When kernelSize is even: 0 pad bottom right. 1 pad top left.    Centers the kernel for even sized kernels.
--
-- typedef enum Style{
-- StyleValidOnly = -1,
-- StyleSame = 0,
-- StyleFull = 1
-- }Style;
--
-- // Typical destination size in one dimension for forward filters (most filters)
-- static int DestSize( int sourceSize, int stride, int filterWindowSize, Style style ){
-- sourceSize += style * (filterWindowSize - 1);       // adjust how many pixels we are allowed to read
-- return (sourceSize + stride - 1) / stride;          // sourceSize / stride, round up
-- }
--
-- // Typical destination size in one dimension for reverse filters (e.g. convolution transpose)
-- static int DestSizeReverse( int sourceSize, int stride, int filterWindowSize, Style style ){
-- return (sourceSize-1) * stride +        // center tap for the last N-1 results. Take stride into account
-- 1 +                             // center tap for the first result
-- style * (filterWindowSize-1);   // add or subtract (or ignore) the filter extent
-- }
--
-- // Find the MPSOffset in one dimension
-- static int Offset( int sourceSize, int stride, int filterWindowSize, Style style ){
-- // The correction needed to adjust from position of left edge to center per MPSOffset definition
-- int correction = filterWindowSize / 2;
--
-- // exit if all we want is to start consuming pixels at the left edge of the image.
-- if( 0 )
-- return correction;
--
-- // Center the area consumed in the source image:
-- // Calculate the size of the destination image
-- int destSize = DestSize( sourceSize, stride, filterWindowSize, style ); // use DestSizeReverse here instead as appropriate
--
-- // calculate extent of pixels we need to read in source to populate the destination
-- int readSize = (destSize-1) * stride + filterWindowSize;
--
-- // calculate number of missing pixels in source
-- int extraSize = readSize - sourceSize;
--
-- // number of missing pixels on left side
-- int leftExtraPixels = (extraSize + centeringPolicy) / 2;
--
-- // account for the fact that the offset is based on the center pixel, not the left edge
-- return correction - leftExtraPixels;
-- }
--
-- @sourceImages@ — The list of source images to be used
--
-- @sourceStates@ — The list of source states to be used
--
-- @kernel@ — The MPSKernel the padding method will be applied to. Set the kernel.offset
--
-- @inDescriptor@ — MPS will prepare a starting guess based on the padding policy (exclusive of                                      MPSNNPaddingMethodCustom) set for the object. You should adjust the offset                                      and image size accordingly. It is on an autoreleasepool.
--
-- Returns: The MPSImageDescriptor to use to make a MPSImage to capture the results from the filter.                  The MPSImageDescriptor is assumed to be on an autoreleasepool. Your method must also set the                  kernel.offset property.
--
-- ObjC selector: @- destinationImageDescriptorForSourceImages:sourceStates:forKernel:suggestedDescriptor:@
destinationImageDescriptorForSourceImages_sourceStates_forKernel_suggestedDescriptor :: (IsMPSState mpsState, IsNSArray sourceImages, IsNSArray sourceStates, IsMPSKernel kernel, IsMPSImageDescriptor inDescriptor) => mpsState -> sourceImages -> sourceStates -> kernel -> inDescriptor -> IO (Id MPSImageDescriptor)
destinationImageDescriptorForSourceImages_sourceStates_forKernel_suggestedDescriptor mpsState  sourceImages sourceStates kernel inDescriptor =
withObjCPtr sourceImages $ \raw_sourceImages ->
  withObjCPtr sourceStates $ \raw_sourceStates ->
    withObjCPtr kernel $ \raw_kernel ->
      withObjCPtr inDescriptor $ \raw_inDescriptor ->
          sendMsg mpsState (mkSelector "destinationImageDescriptorForSourceImages:sourceStates:forKernel:suggestedDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_sourceImages :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_kernel :: Ptr ()), argPtr (castPtr raw_inDescriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Return the number of MTLResource objects held by the state
--
-- ObjC selector: @- resourceCount@
resourceCount :: IsMPSState mpsState => mpsState -> IO CULong
resourceCount mpsState  =
  sendMsg mpsState (mkSelector "resourceCount") retCULong []

-- | @- readCount@
readCount :: IsMPSState mpsState => mpsState -> IO CULong
readCount mpsState  =
  sendMsg mpsState (mkSelector "readCount") retCULong []

-- | @- setReadCount:@
setReadCount :: IsMPSState mpsState => mpsState -> CULong -> IO ()
setReadCount mpsState  value =
  sendMsg mpsState (mkSelector "setReadCount:") retVoid [argCULong (fromIntegral value)]

-- | @- isTemporary@
isTemporary :: IsMPSState mpsState => mpsState -> IO Bool
isTemporary mpsState  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsState (mkSelector "isTemporary") retCULong []

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- label@
label :: IsMPSState mpsState => mpsState -> IO (Id NSString)
label mpsState  =
  sendMsg mpsState (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMPSState mpsState, IsNSString value) => mpsState -> value -> IO ()
setLabel mpsState  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpsState (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @temporaryStateWithCommandBuffer:bufferSize:@
temporaryStateWithCommandBuffer_bufferSizeSelector :: Selector
temporaryStateWithCommandBuffer_bufferSizeSelector = mkSelector "temporaryStateWithCommandBuffer:bufferSize:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:textureDescriptor:@
temporaryStateWithCommandBuffer_textureDescriptorSelector :: Selector
temporaryStateWithCommandBuffer_textureDescriptorSelector = mkSelector "temporaryStateWithCommandBuffer:textureDescriptor:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:@
temporaryStateWithCommandBufferSelector :: Selector
temporaryStateWithCommandBufferSelector = mkSelector "temporaryStateWithCommandBuffer:"

-- | @Selector@ for @initWithDevice:bufferSize:@
initWithDevice_bufferSizeSelector :: Selector
initWithDevice_bufferSizeSelector = mkSelector "initWithDevice:bufferSize:"

-- | @Selector@ for @initWithDevice:textureDescriptor:@
initWithDevice_textureDescriptorSelector :: Selector
initWithDevice_textureDescriptorSelector = mkSelector "initWithDevice:textureDescriptor:"

-- | @Selector@ for @initWithResource:@
initWithResourceSelector :: Selector
initWithResourceSelector = mkSelector "initWithResource:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDevice:resourceList:@
initWithDevice_resourceListSelector :: Selector
initWithDevice_resourceListSelector = mkSelector "initWithDevice:resourceList:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:resourceList:@
temporaryStateWithCommandBuffer_resourceListSelector :: Selector
temporaryStateWithCommandBuffer_resourceListSelector = mkSelector "temporaryStateWithCommandBuffer:resourceList:"

-- | @Selector@ for @initWithResources:@
initWithResourcesSelector :: Selector
initWithResourcesSelector = mkSelector "initWithResources:"

-- | @Selector@ for @resourceAtIndex:allocateMemory:@
resourceAtIndex_allocateMemorySelector :: Selector
resourceAtIndex_allocateMemorySelector = mkSelector "resourceAtIndex:allocateMemory:"

-- | @Selector@ for @bufferSizeAtIndex:@
bufferSizeAtIndexSelector :: Selector
bufferSizeAtIndexSelector = mkSelector "bufferSizeAtIndex:"

-- | @Selector@ for @resourceTypeAtIndex:@
resourceTypeAtIndexSelector :: Selector
resourceTypeAtIndexSelector = mkSelector "resourceTypeAtIndex:"

-- | @Selector@ for @synchronizeOnCommandBuffer:@
synchronizeOnCommandBufferSelector :: Selector
synchronizeOnCommandBufferSelector = mkSelector "synchronizeOnCommandBuffer:"

-- | @Selector@ for @resourceSize@
resourceSizeSelector :: Selector
resourceSizeSelector = mkSelector "resourceSize"

-- | @Selector@ for @destinationImageDescriptorForSourceImages:sourceStates:forKernel:suggestedDescriptor:@
destinationImageDescriptorForSourceImages_sourceStates_forKernel_suggestedDescriptorSelector :: Selector
destinationImageDescriptorForSourceImages_sourceStates_forKernel_suggestedDescriptorSelector = mkSelector "destinationImageDescriptorForSourceImages:sourceStates:forKernel:suggestedDescriptor:"

-- | @Selector@ for @resourceCount@
resourceCountSelector :: Selector
resourceCountSelector = mkSelector "resourceCount"

-- | @Selector@ for @readCount@
readCountSelector :: Selector
readCountSelector = mkSelector "readCount"

-- | @Selector@ for @setReadCount:@
setReadCountSelector :: Selector
setReadCountSelector = mkSelector "setReadCount:"

-- | @Selector@ for @isTemporary@
isTemporarySelector :: Selector
isTemporarySelector = mkSelector "isTemporary"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

