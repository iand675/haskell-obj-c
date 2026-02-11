{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A MPSVector allocated on GPU private memory.
--
-- It may alias one or more other MPSTemporaryVector objects. Undesired data destruction              due to aliasing is avoided using the readCount property.
--
-- Generated bindings for @MPSTemporaryVector@.
module ObjC.MetalPerformanceShaders.MPSTemporaryVector
  ( MPSTemporaryVector
  , IsMPSTemporaryVector(..)
  , temporaryVectorWithCommandBuffer_descriptor
  , prefetchStorageWithCommandBuffer_descriptorList
  , initWithBuffer_descriptor
  , readCount
  , setReadCount
  , temporaryVectorWithCommandBuffer_descriptorSelector
  , prefetchStorageWithCommandBuffer_descriptorListSelector
  , initWithBuffer_descriptorSelector
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

-- | Initialize a MPSTemporaryVector for use on a MTLCommandBuffer
--
-- @commandBuffer@ — The MTLCommandBuffer on which the MPSTemporaryMatrix will be exclusively used
--
-- @descriptor@ — A valid MPSVectorDescriptor describing the MPSVector format to create
--
-- Returns: A valid MPSTemporaryVector.  The object is not managed by a NSAutoreleasePool. The object will be              released when the command buffer is committed. The underlying buffer will become invalid before              this time due to the action of the readCount property.  Please read and understand the use of              the readCount property before using this object.
--
-- ObjC selector: @+ temporaryVectorWithCommandBuffer:descriptor:@
temporaryVectorWithCommandBuffer_descriptor :: IsMPSVectorDescriptor descriptor => RawId -> descriptor -> IO (Id MPSTemporaryVector)
temporaryVectorWithCommandBuffer_descriptor commandBuffer descriptor =
  do
    cls' <- getRequiredClass "MPSTemporaryVector"
    withObjCPtr descriptor $ \raw_descriptor ->
      sendClassMsg cls' (mkSelector "temporaryVectorWithCommandBuffer:descriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Help MPS decide which allocations to make ahead of time
--
-- The buffer cache that underlies the MPSTemporaryVector can automatically allocate new storage as                  needed as you create new temporary vectors.  However, sometimes a more global view of what you                  plan to make is useful for maximizing memory reuse to get the most efficient operation.                  This class method hints to the cache what the list of matrices will be.
--
-- It is never necessary to call this method. It is purely a performance and memory optimization.
--
-- @commandBuffer@ — The command buffer on which the MPSTemporaryVector will be used
--
-- @descriptorList@ — A NSArray of MPSVectorDescriptor objects, indicating vectors that will be created
--
-- ObjC selector: @+ prefetchStorageWithCommandBuffer:descriptorList:@
prefetchStorageWithCommandBuffer_descriptorList :: IsNSArray descriptorList => RawId -> descriptorList -> IO ()
prefetchStorageWithCommandBuffer_descriptorList commandBuffer descriptorList =
  do
    cls' <- getRequiredClass "MPSTemporaryVector"
    withObjCPtr descriptorList $ \raw_descriptorList ->
      sendClassMsg cls' (mkSelector "prefetchStorageWithCommandBuffer:descriptorList:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_descriptorList :: Ptr ())]

-- | *** unavailable
--
-- ObjC selector: @- initWithBuffer:descriptor:@
initWithBuffer_descriptor :: (IsMPSTemporaryVector mpsTemporaryVector, IsMPSVectorDescriptor descriptor) => mpsTemporaryVector -> RawId -> descriptor -> IO (Id MPSTemporaryVector)
initWithBuffer_descriptor mpsTemporaryVector  buffer descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpsTemporaryVector (mkSelector "initWithBuffer:descriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | The number of times a temporary vector may be read by a MPSMatrix... kernel                  before its contents become undefined.
--
-- MPSTemporaryVector objects must release their underlying buffers for reuse                  immediately after last use. So as to facilitate *prompt* convenient                  memory recycling, each time a MPSTemporaryVector is read by a                  MPSMatrix... -encode... method, its readCount is automatically                  decremented. When the readCount reaches 0, the underlying buffer is                  automatically made available for reuse to MPS for its own needs and for                  other MPSTemporaryVector objects prior to return from the -encode.. function.                  The contents of the buffer become undefined at this time.
--
-- By default, the readCount is initialized to 1, indicating a matrix that                  may be overwritten any number of times, but read only once.
--
-- You may change the readCount as desired to allow MPSMatrix kernels to read                  the MPSTemporaryVector additional times. However, it is an error to change                  the readCount once it is zero. It is an error to read or write to a                  MPSTemporaryVector with a zero readCount. You may set the readCount to 0                  yourself to cause the underlying buffer to be returned to MPS. Writing                  to a MPSTemporaryVector does not adjust the readCount.
--
-- The Metal API Validation layer will assert if a MPSTemporaryVector is                  deallocated with non-zero readCount to help identify cases when resources                  are not returned promptly.
--
-- ObjC selector: @- readCount@
readCount :: IsMPSTemporaryVector mpsTemporaryVector => mpsTemporaryVector -> IO CULong
readCount mpsTemporaryVector  =
  sendMsg mpsTemporaryVector (mkSelector "readCount") retCULong []

-- | The number of times a temporary vector may be read by a MPSMatrix... kernel                  before its contents become undefined.
--
-- MPSTemporaryVector objects must release their underlying buffers for reuse                  immediately after last use. So as to facilitate *prompt* convenient                  memory recycling, each time a MPSTemporaryVector is read by a                  MPSMatrix... -encode... method, its readCount is automatically                  decremented. When the readCount reaches 0, the underlying buffer is                  automatically made available for reuse to MPS for its own needs and for                  other MPSTemporaryVector objects prior to return from the -encode.. function.                  The contents of the buffer become undefined at this time.
--
-- By default, the readCount is initialized to 1, indicating a matrix that                  may be overwritten any number of times, but read only once.
--
-- You may change the readCount as desired to allow MPSMatrix kernels to read                  the MPSTemporaryVector additional times. However, it is an error to change                  the readCount once it is zero. It is an error to read or write to a                  MPSTemporaryVector with a zero readCount. You may set the readCount to 0                  yourself to cause the underlying buffer to be returned to MPS. Writing                  to a MPSTemporaryVector does not adjust the readCount.
--
-- The Metal API Validation layer will assert if a MPSTemporaryVector is                  deallocated with non-zero readCount to help identify cases when resources                  are not returned promptly.
--
-- ObjC selector: @- setReadCount:@
setReadCount :: IsMPSTemporaryVector mpsTemporaryVector => mpsTemporaryVector -> CULong -> IO ()
setReadCount mpsTemporaryVector  value =
  sendMsg mpsTemporaryVector (mkSelector "setReadCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @temporaryVectorWithCommandBuffer:descriptor:@
temporaryVectorWithCommandBuffer_descriptorSelector :: Selector
temporaryVectorWithCommandBuffer_descriptorSelector = mkSelector "temporaryVectorWithCommandBuffer:descriptor:"

-- | @Selector@ for @prefetchStorageWithCommandBuffer:descriptorList:@
prefetchStorageWithCommandBuffer_descriptorListSelector :: Selector
prefetchStorageWithCommandBuffer_descriptorListSelector = mkSelector "prefetchStorageWithCommandBuffer:descriptorList:"

-- | @Selector@ for @initWithBuffer:descriptor:@
initWithBuffer_descriptorSelector :: Selector
initWithBuffer_descriptorSelector = mkSelector "initWithBuffer:descriptor:"

-- | @Selector@ for @readCount@
readCountSelector :: Selector
readCountSelector = mkSelector "readCount"

-- | @Selector@ for @setReadCount:@
setReadCountSelector :: Selector
setReadCountSelector = mkSelector "setReadCount:"

