{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A MPSMatrix allocated on GPU private memory.
--
-- It may alias one or more other MPSTemporaryMatrices. Undesired data destruction              due to aliasing is avoided using the readCount property.
--
-- Generated bindings for @MPSTemporaryMatrix@.
module ObjC.MetalPerformanceShaders.MPSTemporaryMatrix
  ( MPSTemporaryMatrix
  , IsMPSTemporaryMatrix(..)
  , temporaryMatrixWithCommandBuffer_matrixDescriptor
  , prefetchStorageWithCommandBuffer_matrixDescriptorList
  , initWithBuffer_descriptor
  , readCount
  , setReadCount
  , temporaryMatrixWithCommandBuffer_matrixDescriptorSelector
  , prefetchStorageWithCommandBuffer_matrixDescriptorListSelector
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

-- | Initialize a MPSTemporaryMatrix for use on a MTLCommandBuffer
--
-- @commandBuffer@ — The MTLCommandBuffer on which the MPSTemporaryMatrix will be exclusively used
--
-- @matrixDescriptor@ — A valid MPSMatrixDescriptor describing the MPSMatrix format to create
--
-- Returns: A valid MPSTemporaryMatrix.  The object is not managed by a NSAutoreleasePool. The object will be              released when the command buffer is committed. The underlying buffer will become invalid before              this time due to the action of the readCount property.  Please read and understand the use of              the readCount property before using this object.
--
-- ObjC selector: @+ temporaryMatrixWithCommandBuffer:matrixDescriptor:@
temporaryMatrixWithCommandBuffer_matrixDescriptor :: IsMPSMatrixDescriptor matrixDescriptor => RawId -> matrixDescriptor -> IO (Id MPSTemporaryMatrix)
temporaryMatrixWithCommandBuffer_matrixDescriptor commandBuffer matrixDescriptor =
  do
    cls' <- getRequiredClass "MPSTemporaryMatrix"
    withObjCPtr matrixDescriptor $ \raw_matrixDescriptor ->
      sendClassMsg cls' (mkSelector "temporaryMatrixWithCommandBuffer:matrixDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_matrixDescriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Help MPS decide which allocations to make ahead of time
--
-- The buffer cache that underlies the MPSTemporaryMatrix can automatically allocate new storage as                  needed as you create new temporary matrices.  However, sometimes a more global view of what you                  plan to make is useful for maximizing memory reuse to get the most efficient operation.                  This class method hints to the cache what the list of matrices will be.
--
-- It is never necessary to call this method. It is purely a performance and memory optimization.
--
-- @commandBuffer@ — The command buffer on which the MPSTemporaryMatrix will be used
--
-- @descriptorList@ — A NSArray of MPSMatrixDescriptor, indicating matrices that will be created
--
-- ObjC selector: @+ prefetchStorageWithCommandBuffer:matrixDescriptorList:@
prefetchStorageWithCommandBuffer_matrixDescriptorList :: IsNSArray descriptorList => RawId -> descriptorList -> IO ()
prefetchStorageWithCommandBuffer_matrixDescriptorList commandBuffer descriptorList =
  do
    cls' <- getRequiredClass "MPSTemporaryMatrix"
    withObjCPtr descriptorList $ \raw_descriptorList ->
      sendClassMsg cls' (mkSelector "prefetchStorageWithCommandBuffer:matrixDescriptorList:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_descriptorList :: Ptr ())]

-- | *** unavailable
--
-- ObjC selector: @- initWithBuffer:descriptor:@
initWithBuffer_descriptor :: (IsMPSTemporaryMatrix mpsTemporaryMatrix, IsMPSMatrixDescriptor descriptor) => mpsTemporaryMatrix -> RawId -> descriptor -> IO (Id MPSTemporaryMatrix)
initWithBuffer_descriptor mpsTemporaryMatrix  buffer descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpsTemporaryMatrix (mkSelector "initWithBuffer:descriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | The number of times a temporary matrix may be read by a MPSMatrix... kernel                  before its contents become undefined.
--
-- MPSTemporaryMatrices must release their underlying buffers for reuse                  immediately after last use. So as to facilitate *prompt* convenient                  memory recycling, each time a MPSTemporaryMatrix is read by a                  MPSMatrix... -encode... method, its readCount is automatically                  decremented. When the readCount reaches 0, the underlying buffer is                  automatically made available for reuse to MPS for its own needs and for                  other MPSTemporaryMatrices prior to return from the -encode.. function.                  The contents of the buffer become undefined at this time.
--
-- By default, the readCount is initialized to 1, indicating a matrix that                  may be overwritten any number of times, but read only once.
--
-- You may change the readCount as desired to allow MPSMatrixKernels to read                  the MPSTemporaryMatrix additional times. However, it is an error to change                  the readCount once it is zero. It is an error to read or write to a                  MPSTemporaryMatrix with a zero readCount. You may set the readCount to 0                  yourself to cause the underlying buffer to be returned to MPS. Writing                  to a MPSTemporaryMatrix does not adjust the readCount.
--
-- The Metal API Validation layer will assert if a MPSTemporaryMatrix is                  deallocated with non-zero readCount to help identify cases when resources                  are not returned promptly.
--
-- ObjC selector: @- readCount@
readCount :: IsMPSTemporaryMatrix mpsTemporaryMatrix => mpsTemporaryMatrix -> IO CULong
readCount mpsTemporaryMatrix  =
  sendMsg mpsTemporaryMatrix (mkSelector "readCount") retCULong []

-- | The number of times a temporary matrix may be read by a MPSMatrix... kernel                  before its contents become undefined.
--
-- MPSTemporaryMatrices must release their underlying buffers for reuse                  immediately after last use. So as to facilitate *prompt* convenient                  memory recycling, each time a MPSTemporaryMatrix is read by a                  MPSMatrix... -encode... method, its readCount is automatically                  decremented. When the readCount reaches 0, the underlying buffer is                  automatically made available for reuse to MPS for its own needs and for                  other MPSTemporaryMatrices prior to return from the -encode.. function.                  The contents of the buffer become undefined at this time.
--
-- By default, the readCount is initialized to 1, indicating a matrix that                  may be overwritten any number of times, but read only once.
--
-- You may change the readCount as desired to allow MPSMatrixKernels to read                  the MPSTemporaryMatrix additional times. However, it is an error to change                  the readCount once it is zero. It is an error to read or write to a                  MPSTemporaryMatrix with a zero readCount. You may set the readCount to 0                  yourself to cause the underlying buffer to be returned to MPS. Writing                  to a MPSTemporaryMatrix does not adjust the readCount.
--
-- The Metal API Validation layer will assert if a MPSTemporaryMatrix is                  deallocated with non-zero readCount to help identify cases when resources                  are not returned promptly.
--
-- ObjC selector: @- setReadCount:@
setReadCount :: IsMPSTemporaryMatrix mpsTemporaryMatrix => mpsTemporaryMatrix -> CULong -> IO ()
setReadCount mpsTemporaryMatrix  value =
  sendMsg mpsTemporaryMatrix (mkSelector "setReadCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @temporaryMatrixWithCommandBuffer:matrixDescriptor:@
temporaryMatrixWithCommandBuffer_matrixDescriptorSelector :: Selector
temporaryMatrixWithCommandBuffer_matrixDescriptorSelector = mkSelector "temporaryMatrixWithCommandBuffer:matrixDescriptor:"

-- | @Selector@ for @prefetchStorageWithCommandBuffer:matrixDescriptorList:@
prefetchStorageWithCommandBuffer_matrixDescriptorListSelector :: Selector
prefetchStorageWithCommandBuffer_matrixDescriptorListSelector = mkSelector "prefetchStorageWithCommandBuffer:matrixDescriptorList:"

-- | @Selector@ for @initWithBuffer:descriptor:@
initWithBuffer_descriptorSelector :: Selector
initWithBuffer_descriptorSelector = mkSelector "initWithBuffer:descriptor:"

-- | @Selector@ for @readCount@
readCountSelector :: Selector
readCountSelector = mkSelector "readCount"

-- | @Selector@ for @setReadCount:@
setReadCountSelector :: Selector
setReadCountSelector = mkSelector "setReadCount:"

