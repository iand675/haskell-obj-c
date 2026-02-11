{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLAccelerationStructureCommandEncoder@.
--
-- Usage:
--
-- @
-- delegate <- newMTLAccelerationStructureCommandEncoder defaultMTLAccelerationStructureCommandEncoderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLAccelerationStructureCommandEncoder
  ( MTLAccelerationStructureCommandEncoderOverrides(..)
  , defaultMTLAccelerationStructureCommandEncoderOverrides
  , newMTLAccelerationStructureCommandEncoder
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol MTLAccelerationStructureCommandEncoder@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLAccelerationStructureCommandEncoderOverrides = MTLAccelerationStructureCommandEncoderOverrides
  { _buildAccelerationStructure_descriptor_scratchBuffer_scratchBufferOffset :: !(Maybe (RawId -> RawId -> RawId -> Int -> IO ()))
  , _refitAccelerationStructure_descriptor_destination_scratchBuffer_scratchBufferOffset :: !(Maybe (RawId -> RawId -> RawId -> RawId -> Int -> IO ()))
  , _copyAccelerationStructure_toAccelerationStructure :: !(Maybe (RawId -> RawId -> IO ()))
  , _writeCompactedAccelerationStructureSize_toBuffer_offset :: !(Maybe (RawId -> RawId -> Int -> IO ()))
  , _copyAndCompactAccelerationStructure_toAccelerationStructure :: !(Maybe (RawId -> RawId -> IO ()))
  , _updateFence :: !(Maybe (RawId -> IO ()))
  , _waitForFence :: !(Maybe (RawId -> IO ()))
  , _useHeap :: !(Maybe (RawId -> IO ()))
  , _useHeaps_count :: !(Maybe (RawId -> Int -> IO ()))
  , _sampleCountersInBuffer_atSampleIndex_withBarrier :: !(Maybe (RawId -> Int -> Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLAccelerationStructureCommandEncoderOverrides :: MTLAccelerationStructureCommandEncoderOverrides
defaultMTLAccelerationStructureCommandEncoderOverrides = MTLAccelerationStructureCommandEncoderOverrides
  { _buildAccelerationStructure_descriptor_scratchBuffer_scratchBufferOffset = Nothing
  , _refitAccelerationStructure_descriptor_destination_scratchBuffer_scratchBufferOffset = Nothing
  , _copyAccelerationStructure_toAccelerationStructure = Nothing
  , _writeCompactedAccelerationStructureSize_toBuffer_offset = Nothing
  , _copyAndCompactAccelerationStructure_toAccelerationStructure = Nothing
  , _updateFence = Nothing
  , _waitForFence = Nothing
  , _useHeap = Nothing
  , _useHeaps_count = Nothing
  , _sampleCountersInBuffer_atSampleIndex_withBarrier = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_Q_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlAccelerationStructureCommandEncoderDelegateClass #-}
mtlAccelerationStructureCommandEncoderDelegateClass :: Class
mtlAccelerationStructureCommandEncoderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLAccelerationStructureCommandEncoder" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_buildAccelerationStructure_descriptor_scratchBuffer_scratchBufferOffset = unSelector (mkSelector "buildAccelerationStructure:descriptor:scratchBuffer:scratchBufferOffset:")
      sel_refitAccelerationStructure_descriptor_destination_scratchBuffer_scratchBufferOffset = unSelector (mkSelector "refitAccelerationStructure:descriptor:destination:scratchBuffer:scratchBufferOffset:")
      sel_copyAccelerationStructure_toAccelerationStructure = unSelector (mkSelector "copyAccelerationStructure:toAccelerationStructure:")
      sel_writeCompactedAccelerationStructureSize_toBuffer_offset = unSelector (mkSelector "writeCompactedAccelerationStructureSize:toBuffer:offset:")
      sel_copyAndCompactAccelerationStructure_toAccelerationStructure = unSelector (mkSelector "copyAndCompactAccelerationStructure:toAccelerationStructure:")
      sel_updateFence = unSelector (mkSelector "updateFence:")
      sel_waitForFence = unSelector (mkSelector "waitForFence:")
      sel_useHeap = unSelector (mkSelector "useHeap:")
      sel_useHeaps_count = unSelector (mkSelector "useHeaps:count:")
      sel_sampleCountersInBuffer_atSampleIndex_withBarrier = unSelector (mkSelector "sampleCountersInBuffer:atSampleIndex:withBarrier:")
  -- buildAccelerationStructure:descriptor:scratchBuffer:scratchBufferOffset:
  stub_0 <- wrap_at_at_at_Q_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLAccelerationStructureCommandEncoderOverrides
    case _buildAccelerationStructure_descriptor_scratchBuffer_scratchBufferOffset rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (fromIntegral arg3)
  addObjCMethod cls "buildAccelerationStructure:descriptor:scratchBuffer:scratchBufferOffset:" "v@:@@@Q" stub_0

  -- refitAccelerationStructure:descriptor:destination:scratchBuffer:scratchBufferOffset:
  stub_1 <- wrap_at_at_at_at_Q_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLAccelerationStructureCommandEncoderOverrides
    case _refitAccelerationStructure_descriptor_destination_scratchBuffer_scratchBufferOffset rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (fromIntegral arg4)
  addObjCMethod cls "refitAccelerationStructure:descriptor:destination:scratchBuffer:scratchBufferOffset:" "v@:@@@@Q" stub_1

  -- copyAccelerationStructure:toAccelerationStructure:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLAccelerationStructureCommandEncoderOverrides
    case _copyAccelerationStructure_toAccelerationStructure rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "copyAccelerationStructure:toAccelerationStructure:" "v@:@@" stub_2

  -- writeCompactedAccelerationStructureSize:toBuffer:offset:
  stub_3 <- wrap_at_at_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLAccelerationStructureCommandEncoderOverrides
    case _writeCompactedAccelerationStructureSize_toBuffer_offset rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2)
  addObjCMethod cls "writeCompactedAccelerationStructureSize:toBuffer:offset:" "v@:@@Q" stub_3

  -- copyAndCompactAccelerationStructure:toAccelerationStructure:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLAccelerationStructureCommandEncoderOverrides
    case _copyAndCompactAccelerationStructure_toAccelerationStructure rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "copyAndCompactAccelerationStructure:toAccelerationStructure:" "v@:@@" stub_4

  -- updateFence:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLAccelerationStructureCommandEncoderOverrides
    case _updateFence rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "updateFence:" "v@:@" stub_5

  -- waitForFence:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLAccelerationStructureCommandEncoderOverrides
    case _waitForFence rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "waitForFence:" "v@:@" stub_6

  -- useHeap:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLAccelerationStructureCommandEncoderOverrides
    case _useHeap rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "useHeap:" "v@:@" stub_7

  -- useHeaps:count:
  stub_8 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLAccelerationStructureCommandEncoderOverrides
    case _useHeaps_count rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "useHeaps:count:" "v@:@Q" stub_8

  -- sampleCountersInBuffer:atSampleIndex:withBarrier:
  stub_9 <- wrap_at_Q_B_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLAccelerationStructureCommandEncoderOverrides
    case _sampleCountersInBuffer_atSampleIndex_withBarrier rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (arg2 /= 0)
  addObjCMethod cls "sampleCountersInBuffer:atSampleIndex:withBarrier:" "v@:@QB" stub_9

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLAccelerationStructureCommandEncoderOverrides
    if queriedSel == sel_buildAccelerationStructure_descriptor_scratchBuffer_scratchBufferOffset then pure (maybe 0 (const 1) (_buildAccelerationStructure_descriptor_scratchBuffer_scratchBufferOffset rec_))
    else if queriedSel == sel_refitAccelerationStructure_descriptor_destination_scratchBuffer_scratchBufferOffset then pure (maybe 0 (const 1) (_refitAccelerationStructure_descriptor_destination_scratchBuffer_scratchBufferOffset rec_))
    else if queriedSel == sel_copyAccelerationStructure_toAccelerationStructure then pure (maybe 0 (const 1) (_copyAccelerationStructure_toAccelerationStructure rec_))
    else if queriedSel == sel_writeCompactedAccelerationStructureSize_toBuffer_offset then pure (maybe 0 (const 1) (_writeCompactedAccelerationStructureSize_toBuffer_offset rec_))
    else if queriedSel == sel_copyAndCompactAccelerationStructure_toAccelerationStructure then pure (maybe 0 (const 1) (_copyAndCompactAccelerationStructure_toAccelerationStructure rec_))
    else if queriedSel == sel_updateFence then pure (maybe 0 (const 1) (_updateFence rec_))
    else if queriedSel == sel_waitForFence then pure (maybe 0 (const 1) (_waitForFence rec_))
    else if queriedSel == sel_useHeap then pure (maybe 0 (const 1) (_useHeap rec_))
    else if queriedSel == sel_useHeaps_count then pure (maybe 0 (const 1) (_useHeaps_count rec_))
    else if queriedSel == sel_sampleCountersInBuffer_atSampleIndex_withBarrier then pure (maybe 0 (const 1) (_sampleCountersInBuffer_atSampleIndex_withBarrier rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newMTLAccelerationStructureCommandEncoder :: MTLAccelerationStructureCommandEncoderOverrides -> IO RawId
newMTLAccelerationStructureCommandEncoder overrides = do
  inst <- class_createInstance mtlAccelerationStructureCommandEncoderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
