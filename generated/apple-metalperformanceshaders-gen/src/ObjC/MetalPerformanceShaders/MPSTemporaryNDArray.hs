{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSTemporaryNDArray
--
-- A MPSNDArray that uses command buffer specific memory to store the array data
--
-- Temporary memory is command buffer specific memory, and is useful for MPSNDArray allocations              with limited lifetime within a single command buffer. Typically, most MPSNDArrays that              are not read or written to by the CPU or needed in other command buffers should be              MPSTemporaryNDArray. This will greatly reduce time spent allocating new memory, reduce memory usage              and help improve memory locality.
--
-- Generated bindings for @MPSTemporaryNDArray@.
module ObjC.MetalPerformanceShaders.MPSTemporaryNDArray
  ( MPSTemporaryNDArray
  , IsMPSTemporaryNDArray(..)
  , defaultAllocator
  , temporaryNDArrayWithCommandBuffer_descriptor
  , initWithDevice_descriptor
  , readCount
  , setReadCount
  , defaultAllocatorSelector
  , initWithDevice_descriptorSelector
  , readCountSelector
  , setReadCountSelector
  , temporaryNDArrayWithCommandBuffer_descriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Get a well known <MPSNDArrayAllocator> that makes temporary MTLBuffers
--
-- ObjC selector: @+ defaultAllocator@
defaultAllocator :: IO RawId
defaultAllocator  =
  do
    cls' <- getRequiredClass "MPSTemporaryNDArray"
    sendClassMessage cls' defaultAllocatorSelector

-- | Initialize a MPSTemporaryNDArray for use on a MTLCommandBuffer
--
-- @commandBuffer@ — The MTLCommandBuffer on which the MPSTemporaryNDArray will be exclusively used
--
-- @descriptor@ — A valid MPSNDArrayDescriptor describing the MPSNDArray format to create
--
-- Returns: A valid MPSTemporaryNDArray.  The object is not managed by a NSAutoreleasePool. The object will be              released when the command buffer is committed. The underlying buffer will become invalid before              this time due to the action of the readCount property.  Please read and understand the use of              the readCount property before using this object.
--
-- ObjC selector: @+ temporaryNDArrayWithCommandBuffer:descriptor:@
temporaryNDArrayWithCommandBuffer_descriptor :: IsMPSNDArrayDescriptor descriptor => RawId -> descriptor -> IO (Id MPSTemporaryNDArray)
temporaryNDArrayWithCommandBuffer_descriptor commandBuffer descriptor =
  do
    cls' <- getRequiredClass "MPSTemporaryNDArray"
    sendClassMessage cls' temporaryNDArrayWithCommandBuffer_descriptorSelector commandBuffer (toMPSNDArrayDescriptor descriptor)

-- | Please use temporaryNDArrayWithCommandBuffer:descriptor: instead
--
-- ObjC selector: @- initWithDevice:descriptor:@
initWithDevice_descriptor :: (IsMPSTemporaryNDArray mpsTemporaryNDArray, IsMPSNDArrayDescriptor descriptor) => mpsTemporaryNDArray -> RawId -> descriptor -> IO (Id MPSTemporaryNDArray)
initWithDevice_descriptor mpsTemporaryNDArray device descriptor =
  sendOwnedMessage mpsTemporaryNDArray initWithDevice_descriptorSelector device (toMPSNDArrayDescriptor descriptor)

-- | The number of times a temporary MPSNDArray may be read by a MPSNDArray... kernel                  before its contents become undefined.
--
-- MPSTemporaryNDArrays must release their underlying buffers for reuse                  immediately after last use. So as to facilitate *prompt* convenient                  memory recycling, each time a MPSTemporaryNDArray is read by a                  MPSNDArray... -encode... method, its readCount is automatically                  decremented. When the readCount reaches 0, the underlying buffer is                  automatically made available for reuse to MPS for its own needs and for                  other MPSTemporaryNDArrays prior to return from the -encode.. function.                  The contents of the buffer become undefined at this time.
--
-- By default, the readCount is initialized to 1, indicating a MPSNDArray that                  may be overwritten any number of times, but read only once.
--
-- You may change the readCount as desired to allow MPSNDArrayKernels to read                  the MPSTemporaryNDArray additional times. However, it is an error to change                  the readCount once it is zero. It is an error to read or write to a                  MPSTemporaryNDArray with a zero readCount. You may set the readCount to 0                  yourself to cause the underlying buffer to be returned to MPS. Writing                  to a MPSTemporaryNDArray does not adjust the readCount.
--
-- The Metal API Validation layer will assert if a MPSTemporaryNDArray is                  deallocated with non-zero readCount to help identify cases when resources                  are not returned promptly.
--
-- ObjC selector: @- readCount@
readCount :: IsMPSTemporaryNDArray mpsTemporaryNDArray => mpsTemporaryNDArray -> IO CULong
readCount mpsTemporaryNDArray =
  sendMessage mpsTemporaryNDArray readCountSelector

-- | The number of times a temporary MPSNDArray may be read by a MPSNDArray... kernel                  before its contents become undefined.
--
-- MPSTemporaryNDArrays must release their underlying buffers for reuse                  immediately after last use. So as to facilitate *prompt* convenient                  memory recycling, each time a MPSTemporaryNDArray is read by a                  MPSNDArray... -encode... method, its readCount is automatically                  decremented. When the readCount reaches 0, the underlying buffer is                  automatically made available for reuse to MPS for its own needs and for                  other MPSTemporaryNDArrays prior to return from the -encode.. function.                  The contents of the buffer become undefined at this time.
--
-- By default, the readCount is initialized to 1, indicating a MPSNDArray that                  may be overwritten any number of times, but read only once.
--
-- You may change the readCount as desired to allow MPSNDArrayKernels to read                  the MPSTemporaryNDArray additional times. However, it is an error to change                  the readCount once it is zero. It is an error to read or write to a                  MPSTemporaryNDArray with a zero readCount. You may set the readCount to 0                  yourself to cause the underlying buffer to be returned to MPS. Writing                  to a MPSTemporaryNDArray does not adjust the readCount.
--
-- The Metal API Validation layer will assert if a MPSTemporaryNDArray is                  deallocated with non-zero readCount to help identify cases when resources                  are not returned promptly.
--
-- ObjC selector: @- setReadCount:@
setReadCount :: IsMPSTemporaryNDArray mpsTemporaryNDArray => mpsTemporaryNDArray -> CULong -> IO ()
setReadCount mpsTemporaryNDArray value =
  sendMessage mpsTemporaryNDArray setReadCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultAllocator@
defaultAllocatorSelector :: Selector '[] RawId
defaultAllocatorSelector = mkSelector "defaultAllocator"

-- | @Selector@ for @temporaryNDArrayWithCommandBuffer:descriptor:@
temporaryNDArrayWithCommandBuffer_descriptorSelector :: Selector '[RawId, Id MPSNDArrayDescriptor] (Id MPSTemporaryNDArray)
temporaryNDArrayWithCommandBuffer_descriptorSelector = mkSelector "temporaryNDArrayWithCommandBuffer:descriptor:"

-- | @Selector@ for @initWithDevice:descriptor:@
initWithDevice_descriptorSelector :: Selector '[RawId, Id MPSNDArrayDescriptor] (Id MPSTemporaryNDArray)
initWithDevice_descriptorSelector = mkSelector "initWithDevice:descriptor:"

-- | @Selector@ for @readCount@
readCountSelector :: Selector '[] CULong
readCountSelector = mkSelector "readCount"

-- | @Selector@ for @setReadCount:@
setReadCountSelector :: Selector '[CULong] ()
setReadCountSelector = mkSelector "setReadCount:"

