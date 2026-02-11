{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCommandBuffer
--
-- This depends on Metal.framework
--
-- A MPSCommandBuffer object is used to wrap an existing command buffer with MPS specific options.
--
-- A MPS kernel typically operates between a fixed set of inputs and outputs.              The MPSCommandBuffer class provides a way to add further encode-time parameters              to the encode call using the command buffer. Currently the only parameter included in the              MPSCommandBuffer that all MPS kernels support is the the predicate option,              which can be used to pre-empt the kernel from the GPU side.              NOTE: the options that contain metal resources will be referenced by this object and              therefore it is advisable to make the lifetime of this object as short as possible as is the              case for all command buffers.
--
-- Generated bindings for @MPSCommandBuffer@.
module ObjC.MetalPerformanceShaders.MPSCommandBuffer
  ( MPSCommandBuffer
  , IsMPSCommandBuffer(..)
  , commandBufferWithCommandBuffer
  , commandBufferFromCommandQueue
  , initWithCommandBuffer
  , init_
  , commitAndContinue
  , prefetchHeapForWorkloadSize
  , commandBuffer
  , rootCommandBuffer
  , predicate
  , setPredicate
  , heapProvider
  , setHeapProvider
  , commandBufferWithCommandBufferSelector
  , commandBufferFromCommandQueueSelector
  , initWithCommandBufferSelector
  , initSelector
  , commitAndContinueSelector
  , prefetchHeapForWorkloadSizeSelector
  , commandBufferSelector
  , rootCommandBufferSelector
  , predicateSelector
  , setPredicateSelector
  , heapProviderSelector
  , setHeapProviderSelector


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

-- | Initializes a MPSCommandBuffer object with given MTLCommandBuffer.              Once we create this MPSCommandBuffer, any methods utilizing it could call commitAndContinue and so the users original commandBuffer may have been committed.              Please use the rootCommandBuffer method to get the current alive underlying MTLCommandBuffer.
--
-- Returns: A pointer to the newly initialized MPSCommandBuffer object.
--
-- ObjC selector: @+ commandBufferWithCommandBuffer:@
commandBufferWithCommandBuffer :: RawId -> IO (Id MPSCommandBuffer)
commandBufferWithCommandBuffer commandBuffer =
  do
    cls' <- getRequiredClass "MPSCommandBuffer"
    sendClassMsg cls' (mkSelector "commandBufferWithCommandBuffer:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes a MPSCommandBuffer object from a given command queue.
--
-- Returns: A pointer to the newly initialized MPSCommandBuffer object.
--
-- ObjC selector: @+ commandBufferFromCommandQueue:@
commandBufferFromCommandQueue :: RawId -> IO (Id MPSCommandBuffer)
commandBufferFromCommandQueue commandQueue =
  do
    cls' <- getRequiredClass "MPSCommandBuffer"
    sendClassMsg cls' (mkSelector "commandBufferFromCommandQueue:") (retPtr retVoid) [argPtr (castPtr (unRawId commandQueue) :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes an empty MPSCommandBuffer object with given MTLCommandBuffer.              Once we create this MPSCommandBuffer, any methods utilizing it could call commitAndContinue and so the users original commandBuffer may have been committed.              Please use the rootCommandBuffer method to get the current alive underlying MTLCommandBuffer.
--
-- Returns: A pointer to the newly initialized MPSCommandBuffer object.
--
-- ObjC selector: @- initWithCommandBuffer:@
initWithCommandBuffer :: IsMPSCommandBuffer mpsCommandBuffer => mpsCommandBuffer -> RawId -> IO (Id MPSCommandBuffer)
initWithCommandBuffer mpsCommandBuffer  commandBuffer =
    sendMsg mpsCommandBuffer (mkSelector "initWithCommandBuffer:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPSCommandBuffer mpsCommandBuffer => mpsCommandBuffer -> IO (Id MPSCommandBuffer)
init_ mpsCommandBuffer  =
    sendMsg mpsCommandBuffer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Commit work encoded so far and continue with a new underlying command buffer
--
-- This method commits the underlying root MTLCommandBuffer, and makes                  a new one on the same command queue. The MPS heap is moved forward                  to the new command buffer such that temporary objects used by                  the previous command buffer can be still be used with the new one.
--
-- This provides a way to move work already encoded into consideration                  by the Metal back end sooner. For large workloads, e.g. a neural networking graph                  periodically calling -commitAndContinue may allow you to improve CPU / GPU parallelism                  without the substantial memory increases associated with double buffering.                  It will also help decrease overall latency.
--
-- Any Metal schedule or completion callbacks previously attached to this                  object will remain attached to the old command buffer and                  will fire as expected as the old command buffer is scheduled and                  completes. If your application is relying on such callbacks to coordinate                  retain / release of important objects that are needed for work encoded after                  -commitAndContinue, your application should retain these objects before                  calling commitAndContinue, and attach new release callbacks to this                  object with a new completion handler so that they persist through the                  lifetime of the new underlying command buffer. You may do this, for example                  by adding the objects to a mutable array before calling -commitAndContinue, then                  release the mutable array in a new completion callback added after -commitAndContinue.
--
-- Because -commitAndContinue commits the old command buffer then switches to a new one,                  some aspects of command buffer completion may surprise unwary developers. For example,                  -waitUntilCompleted called immediately after -commitAndContinue asks Metal to wait for                  the new command buffer to finish, not the old one. Since the new command buffer presumably                  hasn't been committed yet, it is formally a deadlock, resources may leak and Metal may                  complain. Your application should ether call -commit before -waitUntilCompleted, or                  capture the -rootCommandBuffer from before the call to -commitAndContinue and wait                  on that.  Similarly, your application should be sure to use the appropriate command buffer                  when querying the [MTLCommandBuffer status] property.
--
-- If the underlying MTLCommandBuffer also implements -commitAndContinue, then the message                  will be forwarded to that object instead. In this way, underlying predicate objects and                  other state will be preserved.
--
-- ObjC selector: @- commitAndContinue@
commitAndContinue :: IsMPSCommandBuffer mpsCommandBuffer => mpsCommandBuffer -> IO ()
commitAndContinue mpsCommandBuffer  =
    sendMsg mpsCommandBuffer (mkSelector "commitAndContinue") retVoid []

-- | Prefetch heap into the MPS command buffer heap cache.
--
-- If there is not sufficient free storage in the MPS heap for the command buffer for allocations of total size size,              pre-warm the MPS heap with a new MTLHeap allocation of sufficient size.  If this size turns out to be too small              MPS may ask for more heaps later to cover additional allocations. If heapProvider is not nil, the heapProvider              will be used.
--
-- @size@ — The minimum size of the free store needed
--
-- ObjC selector: @- prefetchHeapForWorkloadSize:@
prefetchHeapForWorkloadSize :: IsMPSCommandBuffer mpsCommandBuffer => mpsCommandBuffer -> CULong -> IO ()
prefetchHeapForWorkloadSize mpsCommandBuffer  size =
    sendMsg mpsCommandBuffer (mkSelector "prefetchHeapForWorkloadSize:") retVoid [argCULong size]

-- | commandBuffer
--
-- The Metal Command Buffer that was used to initialize this object.
--
-- ObjC selector: @- commandBuffer@
commandBuffer :: IsMPSCommandBuffer mpsCommandBuffer => mpsCommandBuffer -> IO RawId
commandBuffer mpsCommandBuffer  =
    fmap (RawId . castPtr) $ sendMsg mpsCommandBuffer (mkSelector "commandBuffer") (retPtr retVoid) []

-- | rootCommandBuffer
--
-- The base MTLCommandBuffer underlying the MPSCommandBuffer
--
-- MPSCommandBuffers may wrap other MPSCommandBuffers, in the process              creating what is in effect a stack of predicate objects that may be              pushed or popped by making new MPSCommandBuffers or by calling -commandBuffer.              In some circumstances, it is preferable to use the root command buffer,              particularly when trying to identify the command buffer that will be commited              by -commitAndContinue.
--
-- ObjC selector: @- rootCommandBuffer@
rootCommandBuffer :: IsMPSCommandBuffer mpsCommandBuffer => mpsCommandBuffer -> IO RawId
rootCommandBuffer mpsCommandBuffer  =
    fmap (RawId . castPtr) $ sendMsg mpsCommandBuffer (mkSelector "rootCommandBuffer") (retPtr retVoid) []

-- | predicate
--
-- A GPU predicate object. Default: nil.
--
-- ObjC selector: @- predicate@
predicate :: IsMPSCommandBuffer mpsCommandBuffer => mpsCommandBuffer -> IO (Id MPSPredicate)
predicate mpsCommandBuffer  =
    sendMsg mpsCommandBuffer (mkSelector "predicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | predicate
--
-- A GPU predicate object. Default: nil.
--
-- ObjC selector: @- setPredicate:@
setPredicate :: (IsMPSCommandBuffer mpsCommandBuffer, IsMPSPredicate value) => mpsCommandBuffer -> value -> IO ()
setPredicate mpsCommandBuffer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mpsCommandBuffer (mkSelector "setPredicate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | heapProvider
--
-- A application supplied object to allocate MTLHeaps for MPS
--
-- By default this is nil, which will use MPS' device level global heap cache to              allocate the heaps. This is a reasonable choice. However, it may be inefficient              if you are keeping your own MTLHeap, since there will be two pessimistically              sized free stores which may be larger than is strictly necessary, and of course              fragmentation across multiple heaps. In such cases, the problem may be solved              either by using MPS' automatically managed heap (simple) or having MPS use              your heap. The heapProvider allows you to implement the second case.  To use              the MPS heap, simply make temporary MPSImages, vectors and matrices.
--
-- If multiple MPSCommandBuffers reference the same MTLCommandBuffer, changing              the heapProvider on one will change the heap provider for all of them.
--
-- ObjC selector: @- heapProvider@
heapProvider :: IsMPSCommandBuffer mpsCommandBuffer => mpsCommandBuffer -> IO RawId
heapProvider mpsCommandBuffer  =
    fmap (RawId . castPtr) $ sendMsg mpsCommandBuffer (mkSelector "heapProvider") (retPtr retVoid) []

-- | heapProvider
--
-- A application supplied object to allocate MTLHeaps for MPS
--
-- By default this is nil, which will use MPS' device level global heap cache to              allocate the heaps. This is a reasonable choice. However, it may be inefficient              if you are keeping your own MTLHeap, since there will be two pessimistically              sized free stores which may be larger than is strictly necessary, and of course              fragmentation across multiple heaps. In such cases, the problem may be solved              either by using MPS' automatically managed heap (simple) or having MPS use              your heap. The heapProvider allows you to implement the second case.  To use              the MPS heap, simply make temporary MPSImages, vectors and matrices.
--
-- If multiple MPSCommandBuffers reference the same MTLCommandBuffer, changing              the heapProvider on one will change the heap provider for all of them.
--
-- ObjC selector: @- setHeapProvider:@
setHeapProvider :: IsMPSCommandBuffer mpsCommandBuffer => mpsCommandBuffer -> RawId -> IO ()
setHeapProvider mpsCommandBuffer  value =
    sendMsg mpsCommandBuffer (mkSelector "setHeapProvider:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @commandBufferWithCommandBuffer:@
commandBufferWithCommandBufferSelector :: Selector
commandBufferWithCommandBufferSelector = mkSelector "commandBufferWithCommandBuffer:"

-- | @Selector@ for @commandBufferFromCommandQueue:@
commandBufferFromCommandQueueSelector :: Selector
commandBufferFromCommandQueueSelector = mkSelector "commandBufferFromCommandQueue:"

-- | @Selector@ for @initWithCommandBuffer:@
initWithCommandBufferSelector :: Selector
initWithCommandBufferSelector = mkSelector "initWithCommandBuffer:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @commitAndContinue@
commitAndContinueSelector :: Selector
commitAndContinueSelector = mkSelector "commitAndContinue"

-- | @Selector@ for @prefetchHeapForWorkloadSize:@
prefetchHeapForWorkloadSizeSelector :: Selector
prefetchHeapForWorkloadSizeSelector = mkSelector "prefetchHeapForWorkloadSize:"

-- | @Selector@ for @commandBuffer@
commandBufferSelector :: Selector
commandBufferSelector = mkSelector "commandBuffer"

-- | @Selector@ for @rootCommandBuffer@
rootCommandBufferSelector :: Selector
rootCommandBufferSelector = mkSelector "rootCommandBuffer"

-- | @Selector@ for @predicate@
predicateSelector :: Selector
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @setPredicate:@
setPredicateSelector :: Selector
setPredicateSelector = mkSelector "setPredicate:"

-- | @Selector@ for @heapProvider@
heapProviderSelector :: Selector
heapProviderSelector = mkSelector "heapProvider"

-- | @Selector@ for @setHeapProvider:@
setHeapProviderSelector :: Selector
setHeapProviderSelector = mkSelector "setHeapProvider:"

