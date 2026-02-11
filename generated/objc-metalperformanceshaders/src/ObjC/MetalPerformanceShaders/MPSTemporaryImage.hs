{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSTemporaryImage
--
-- MPSImage
--
-- MPSTemporaryImages are for MPSImages with short lifetimes.
--
-- What is temporary memory? It is memory, plain and simple. Analogy: If we              use an app as an analogy for a command buffer, then "Regular memory"              (such as what backs a MPSImage or the typical MTLTexture) would be memory               that you allocate at launch and never free. Temporary memory would be memory               that you free when you are done with it so it can be used for something               else as needed later in your app.  You /could/ write your app to allocate               everything you will ever need up front, but this is very inefficient and               quite frankly a pain to plan out in advance. You don't do it for your app,              so why would you do it for your command buffers?
--
-- Welcome to the 1970's! We have added a heap.
--
-- Unsurprisingly, MPSTemporaryImages can provide for profound reduction in              the the amount of memory used by your application.  Like malloc, MPS               maintains a heap of memory usable in a command buffer. Over the lifetime               of a command buffer, the same piece of memory may be reused many times.               This means that each time the same memory is reused, it aliases with previous              uses. If we aren't careful, we might find that needed data is overwritten              by successive allocations. However, this is no different than accessing freed              memory only to discover it doesn't contain what you thought it did anymore,               so you should be able to keep out of trouble by following a few simple rules,              like with malloc.
--
-- To this end, we added some restrictions to help you out and get a bit more               performance. Some comments are appended in parentheses below to extend the              analogy of command buffer = program:
--
-- - The textures are MTLStorageModePrivate. You can not, for example, use                [MTLTexture getBytes...] or [MTLTexture replaceRegion...] with them.                 MPSTemporaryImages are strictly read and written by the GPU. (There is                protected memory to prevent other processes from overwriting your heap.)
--
-- - The temporary image may be used only on a single MTLCommandBuffer.                This limits the chronology to a single linear time stream. (The heap                is specific to just one command buffer. There are no mutexes to                coordinate timing of simultaneous access by multiple GPUs. Nor are we                likely to like them if there were. So, we disallow it.)
--
-- - The readCount property must be managed correctly. Please see                the description of the readCount property for full details. (The readCount                is a reference count for the block of memory that holds your data. The                usual undefined behaviors apply to reading data that has been released.                We assert when we can to prevent that from happening accidentally,                just as a program might segfault. The readCount counts procedural users                 of the object -- MPSKernel.encode... calls that read the MPSTemporaryImage.                 As each reads from it, the readCount is automatically decremented. The                 texture data will be freed in typical usage at the right time as the                 readCount reaches zero, typically with little user involvement other                than to set the readCount up front. We did examine using the main MPSTemporaryImage                reference count for this instead so that ARC would do work for you automatically.                Alas, ARC destroys things at end of scope rather than promptly, sometimes resulting                in greatly increased memory usage. These allocations are large! So, we                 use this method instead.)
--
-- Since MPSTemporaryImages can only be used with a single MTLCommandBuffer,              and can not be used off the GPU, they generally should not be kept               around past the completion of the MTLCommandBuffer. The lifetime of              MPSTemporaryImages is expected to be typically extremely short, perhaps               only a few lines of code. Like malloc, it is intended to be fairly cheap               to make MPSTemporaryImages and throw them away. Please do so.
--
-- To keep the lifetime of the underlying texture allocation as short as               possible, the underlying texture is not allocated until the first time              the MPSTemporaryImage is used by a MPSCNNKernel or the .texture property              is read. The readCount property serves to limit the lifetime on the              other end.
--
-- You may use the MPSTemporaryImage.texture with MPSUnaryImageKernel -encode... methods,              iff featureChannels <= 4 and the MTLTexture conforms to requirements of that MPSKernel.              There is no locking mechanism provided to prevent a MTLTexture returned               from the .texture property from becoming invalid when the readCount reaches 0.
--
-- Finally, MPSTemporaryImages are complicated to use with blit encoders.              Your application should assume that the MPSTemporaryImage is backed by a MTLHeap,              and consequently needs a MTLFence to ensure that compute command encoders and other              encoders do not trip over one another with heap based memory. MPS will almost never              use a blit encoder for this reason. If you do need one, then you will need to make              a new compute encoder to block on whatever previous compute encoder last used the              heap block. (MPS will not tell you who previously used the heap block. That encoder              is almost certainly long dead anyway.) If concurrent encoders are involved, then a              barrier might be needed. Within that compute encoder, you will call -updateFence.              End the compute encoder, make a blit encoder wait for the fence, do the blit, update              a new fence, then make a new compute encoder, wait for the second fence, then you              can continue. Possibly the second do-nothing compute encoder needs to be ended so              MPS can be called. Frankly, we don't bother with blit encoders and just write a compute              operation for copy / clear as needed, or better yet find a way to eliminate the              clear / copy pass so we don't have to pay for it. Note: the most common use of a              blit encoder, -synchronizeResource: can not encounter this problem because              MPSTemporaryImages live in GPU private memory and can not be read by the CPU.
--
-- MPSTemporaryImages can otherwise be used wherever MPSImages are used.
--
-- Generated bindings for @MPSTemporaryImage@.
module ObjC.MetalPerformanceShaders.MPSTemporaryImage
  ( MPSTemporaryImage
  , IsMPSTemporaryImage(..)
  , defaultAllocator
  , temporaryImageWithCommandBuffer_imageDescriptor
  , temporaryImageWithCommandBuffer_textureDescriptor
  , temporaryImageWithCommandBuffer_textureDescriptor_featureChannels
  , prefetchStorageWithCommandBuffer_imageDescriptorList
  , initWithTexture_featureChannels
  , initWithDevice_imageDescriptor
  , readCount
  , setReadCount
  , defaultAllocatorSelector
  , temporaryImageWithCommandBuffer_imageDescriptorSelector
  , temporaryImageWithCommandBuffer_textureDescriptorSelector
  , temporaryImageWithCommandBuffer_textureDescriptor_featureChannelsSelector
  , prefetchStorageWithCommandBuffer_imageDescriptorListSelector
  , initWithTexture_featureChannelsSelector
  , initWithDevice_imageDescriptorSelector
  , readCountSelector
  , setReadCountSelector


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
import ObjC.Foundation.Internal.Classes
import ObjC.Metal.Internal.Classes

-- | Get a well known MPSImageAllocator that makes MPSTemporaryImages
--
-- ObjC selector: @+ defaultAllocator@
defaultAllocator :: IO RawId
defaultAllocator  =
  do
    cls' <- getRequiredClass "MPSTemporaryImage"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "defaultAllocator") (retPtr retVoid) []

-- | Initialize a MPSTemporaryImage for use on a MTLCommandBuffer
--
-- @commandBuffer@ — The MTLCommandBuffer on which the MPSTemporaryImage will be exclusively used
--
-- @imageDescriptor@ — A valid imageDescriptor describing the MPSImage format to create.
--
-- Returns: A valid MPSTemporaryImage.  The object will be released when the command buffer              is committed. The underlying texture will become invalid before this time              due to the action of the readCount property.
--
-- ObjC selector: @+ temporaryImageWithCommandBuffer:imageDescriptor:@
temporaryImageWithCommandBuffer_imageDescriptor :: RawId -> Const (Id MPSImageDescriptor) -> IO (Id MPSTemporaryImage)
temporaryImageWithCommandBuffer_imageDescriptor commandBuffer imageDescriptor =
  do
    cls' <- getRequiredClass "MPSTemporaryImage"
    withObjCPtr imageDescriptor $ \raw_imageDescriptor ->
      sendClassMsg cls' (mkSelector "temporaryImageWithCommandBuffer:imageDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_imageDescriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Low level interface for creating a MPSTemporaryImage using a MTLTextureDescriptor
--
-- This function provides access to MTLPixelFormats not typically covered by -initForCommandBuffer:imageDescriptor:                  The feature channels will be inferred from the MTLPixelFormat without changing the width.                   The following restrictions apply:
--
-- MTLTextureType must be MTLTextureType2D or MTLTextureType2DArray                      MTLTextureUsage must contain at least one of MTLTextureUsageShaderRead, MTLTextureUsageShaderWrite                      MTLStorageMode must be MTLStorageModePrivate                      depth must be 1
--
-- @commandBuffer@ — The command buffer on which the MPSTemporaryImage may be used
--
-- @textureDescriptor@ — A texture descriptor describing the MPSTemporaryImage texture
--
-- Returns: A valid MPSTemporaryImage.  The object will be released when the command buffer              is committed. The underlying texture will become invalid before this time              due to the action of the readCount property.
--
-- ObjC selector: @+ temporaryImageWithCommandBuffer:textureDescriptor:@
temporaryImageWithCommandBuffer_textureDescriptor :: RawId -> Const (Id MTLTextureDescriptor) -> IO (Id MPSTemporaryImage)
temporaryImageWithCommandBuffer_textureDescriptor commandBuffer textureDescriptor =
  do
    cls' <- getRequiredClass "MPSTemporaryImage"
    withObjCPtr textureDescriptor $ \raw_textureDescriptor ->
      sendClassMsg cls' (mkSelector "temporaryImageWithCommandBuffer:textureDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_textureDescriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Low level interface for creating a MPSTemporaryImage using a MTLTextureDescriptor
--
-- This function provides access to MTLPixelFormats not typically covered by -initForCommandBuffer:imageDescriptor:                  The number of images will be inferred from number of slices in the descriptor.arrayLength and                  the number of feature channels.
--
-- The following restrictions apply:
--
-- MTLTextureType must be MTLTextureType2D or MTLTextureType2DArray                      MTLTextureUsage must contain at least one of MTLTextureUsageShaderRead, MTLTextureUsageShaderWrite                      MTLStorageMode must be MTLStorageModePrivate
--
-- @commandBuffer@ — The command buffer on which the MPSTemporaryImage may be used
--
-- @textureDescriptor@ — A texture descriptor describing the MPSTemporaryImage texture
--
-- Returns: A valid MPSTemporaryImage.  The object will be released when the command buffer              is committed. The underlying texture will become invalid before this time              due to the action of the readCount property.
--
-- ObjC selector: @+ temporaryImageWithCommandBuffer:textureDescriptor:featureChannels:@
temporaryImageWithCommandBuffer_textureDescriptor_featureChannels :: RawId -> Const (Id MTLTextureDescriptor) -> CULong -> IO (Id MPSTemporaryImage)
temporaryImageWithCommandBuffer_textureDescriptor_featureChannels commandBuffer textureDescriptor featureChannels =
  do
    cls' <- getRequiredClass "MPSTemporaryImage"
    withObjCPtr textureDescriptor $ \raw_textureDescriptor ->
      sendClassMsg cls' (mkSelector "temporaryImageWithCommandBuffer:textureDescriptor:featureChannels:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_textureDescriptor :: Ptr ()), argCULong (fromIntegral featureChannels)] >>= retainedObject . castPtr

-- | Help MPS decide which allocations to make ahead of time
--
-- The texture cache that underlies the MPSTemporaryImage can automatically allocate new storage as                  needed as you create new temporary images.  However, sometimes a more global view of what you                  plan to make is useful for maximizing memory reuse to get the most efficient operation.                  This class method hints to the cache what the list of images will be.
--
-- It is never necessary to call this method. It is purely a performance and memory optimization.
--
-- This method makes a conservative estimate of memory required and may not fully                  cover temporary memory needs, resulting in additional allocations later that could                  encounter pathological behavior, if they are too small. If the full extent and timing of the                  workload is known in advance, it is recommended that MPSHintTemporaryMemoryHighWaterMark() be                  used instead.
--
-- @commandBuffer@ — The command buffer on which the MPSTemporaryImages will be used
--
-- @descriptorList@ — A NSArray of MPSImageDescriptors, indicating images that will be created
--
-- ObjC selector: @+ prefetchStorageWithCommandBuffer:imageDescriptorList:@
prefetchStorageWithCommandBuffer_imageDescriptorList :: IsNSArray descriptorList => RawId -> descriptorList -> IO ()
prefetchStorageWithCommandBuffer_imageDescriptorList commandBuffer descriptorList =
  do
    cls' <- getRequiredClass "MPSTemporaryImage"
    withObjCPtr descriptorList $ \raw_descriptorList ->
      sendClassMsg cls' (mkSelector "prefetchStorageWithCommandBuffer:imageDescriptorList:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_descriptorList :: Ptr ())]

-- | Unavailable. Use temporaryImageForCommandBuffer:textureDescriptor: or -temporaryImageForCommandBuffer:imageDescriptor: instead.
--
-- ObjC selector: @- initWithTexture:featureChannels:@
initWithTexture_featureChannels :: IsMPSTemporaryImage mpsTemporaryImage => mpsTemporaryImage -> RawId -> CULong -> IO (Id MPSTemporaryImage)
initWithTexture_featureChannels mpsTemporaryImage  texture featureChannels =
  sendMsg mpsTemporaryImage (mkSelector "initWithTexture:featureChannels:") (retPtr retVoid) [argPtr (castPtr (unRawId texture) :: Ptr ()), argCULong (fromIntegral featureChannels)] >>= ownedObject . castPtr

-- | Unavailable. Use itemporaryImageForCommandBuffer:textureDescriptor: instead.
--
-- ObjC selector: @- initWithDevice:imageDescriptor:@
initWithDevice_imageDescriptor :: IsMPSTemporaryImage mpsTemporaryImage => mpsTemporaryImage -> RawId -> Const (Id MPSImageDescriptor) -> IO (Id MPSTemporaryImage)
initWithDevice_imageDescriptor mpsTemporaryImage  device imageDescriptor =
withObjCPtr imageDescriptor $ \raw_imageDescriptor ->
    sendMsg mpsTemporaryImage (mkSelector "initWithDevice:imageDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_imageDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | @- readCount@
readCount :: IsMPSTemporaryImage mpsTemporaryImage => mpsTemporaryImage -> IO CULong
readCount mpsTemporaryImage  =
  sendMsg mpsTemporaryImage (mkSelector "readCount") retCULong []

-- | @- setReadCount:@
setReadCount :: IsMPSTemporaryImage mpsTemporaryImage => mpsTemporaryImage -> CULong -> IO ()
setReadCount mpsTemporaryImage  value =
  sendMsg mpsTemporaryImage (mkSelector "setReadCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultAllocator@
defaultAllocatorSelector :: Selector
defaultAllocatorSelector = mkSelector "defaultAllocator"

-- | @Selector@ for @temporaryImageWithCommandBuffer:imageDescriptor:@
temporaryImageWithCommandBuffer_imageDescriptorSelector :: Selector
temporaryImageWithCommandBuffer_imageDescriptorSelector = mkSelector "temporaryImageWithCommandBuffer:imageDescriptor:"

-- | @Selector@ for @temporaryImageWithCommandBuffer:textureDescriptor:@
temporaryImageWithCommandBuffer_textureDescriptorSelector :: Selector
temporaryImageWithCommandBuffer_textureDescriptorSelector = mkSelector "temporaryImageWithCommandBuffer:textureDescriptor:"

-- | @Selector@ for @temporaryImageWithCommandBuffer:textureDescriptor:featureChannels:@
temporaryImageWithCommandBuffer_textureDescriptor_featureChannelsSelector :: Selector
temporaryImageWithCommandBuffer_textureDescriptor_featureChannelsSelector = mkSelector "temporaryImageWithCommandBuffer:textureDescriptor:featureChannels:"

-- | @Selector@ for @prefetchStorageWithCommandBuffer:imageDescriptorList:@
prefetchStorageWithCommandBuffer_imageDescriptorListSelector :: Selector
prefetchStorageWithCommandBuffer_imageDescriptorListSelector = mkSelector "prefetchStorageWithCommandBuffer:imageDescriptorList:"

-- | @Selector@ for @initWithTexture:featureChannels:@
initWithTexture_featureChannelsSelector :: Selector
initWithTexture_featureChannelsSelector = mkSelector "initWithTexture:featureChannels:"

-- | @Selector@ for @initWithDevice:imageDescriptor:@
initWithDevice_imageDescriptorSelector :: Selector
initWithDevice_imageDescriptorSelector = mkSelector "initWithDevice:imageDescriptor:"

-- | @Selector@ for @readCount@
readCountSelector :: Selector
readCountSelector = mkSelector "readCount"

-- | @Selector@ for @setReadCount:@
setReadCountSelector :: Selector
setReadCountSelector = mkSelector "setReadCount:"

