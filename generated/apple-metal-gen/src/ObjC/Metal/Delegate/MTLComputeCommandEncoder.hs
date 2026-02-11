{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLComputeCommandEncoder@.
--
-- Usage:
--
-- @
-- delegate <- newMTLComputeCommandEncoder defaultMTLComputeCommandEncoderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLComputeCommandEncoder
  ( MTLComputeCommandEncoderOverrides(..)
  , defaultMTLComputeCommandEncoderOverrides
  , newMTLComputeCommandEncoder
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

-- | Overrides record for @\@protocol MTLComputeCommandEncoder@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLComputeCommandEncoderOverrides = MTLComputeCommandEncoderOverrides
  { _setComputePipelineState :: !(Maybe (RawId -> IO ()))
  , _setBuffer_offset_atIndex :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _setBufferOffset_atIndex :: !(Maybe (Int -> Int -> IO ()))
  , _setBuffer_offset_attributeStride_atIndex :: !(Maybe (RawId -> Int -> Int -> Int -> IO ()))
  , _setBufferOffset_attributeStride_atIndex :: !(Maybe (Int -> Int -> Int -> IO ()))
  , _setVisibleFunctionTable_atBufferIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _setIntersectionFunctionTable_atBufferIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _setAccelerationStructure_atBufferIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _setTexture_atIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _setSamplerState_atIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _setSamplerState_lodMinClamp_lodMaxClamp_atIndex :: !(Maybe (RawId -> Float -> Float -> Int -> IO ()))
  , _setThreadgroupMemoryLength_atIndex :: !(Maybe (Int -> Int -> IO ()))
  , _setImageblockWidth_height :: !(Maybe (Int -> Int -> IO ()))
  , _setStageInRegionWithIndirectBuffer_indirectBufferOffset :: !(Maybe (RawId -> Int -> IO ()))
  , _updateFence :: !(Maybe (RawId -> IO ()))
  , _waitForFence :: !(Maybe (RawId -> IO ()))
  , _useHeap :: !(Maybe (RawId -> IO ()))
  , _useHeaps_count :: !(Maybe (RawId -> Int -> IO ()))
  , _executeCommandsInBuffer_indirectBuffer_indirectBufferOffset :: !(Maybe (RawId -> RawId -> Int -> IO ()))
  , _memoryBarrierWithResources_count :: !(Maybe (RawId -> Int -> IO ()))
  , _sampleCountersInBuffer_atSampleIndex_withBarrier :: !(Maybe (RawId -> Int -> Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLComputeCommandEncoderOverrides :: MTLComputeCommandEncoderOverrides
defaultMTLComputeCommandEncoderOverrides = MTLComputeCommandEncoderOverrides
  { _setComputePipelineState = Nothing
  , _setBuffer_offset_atIndex = Nothing
  , _setBufferOffset_atIndex = Nothing
  , _setBuffer_offset_attributeStride_atIndex = Nothing
  , _setBufferOffset_attributeStride_atIndex = Nothing
  , _setVisibleFunctionTable_atBufferIndex = Nothing
  , _setIntersectionFunctionTable_atBufferIndex = Nothing
  , _setAccelerationStructure_atBufferIndex = Nothing
  , _setTexture_atIndex = Nothing
  , _setSamplerState_atIndex = Nothing
  , _setSamplerState_lodMinClamp_lodMaxClamp_atIndex = Nothing
  , _setThreadgroupMemoryLength_atIndex = Nothing
  , _setImageblockWidth_height = Nothing
  , _setStageInRegionWithIndirectBuffer_indirectBufferOffset = Nothing
  , _updateFence = Nothing
  , _waitForFence = Nothing
  , _useHeap = Nothing
  , _useHeaps_count = Nothing
  , _executeCommandsInBuffer_indirectBuffer_indirectBufferOffset = Nothing
  , _memoryBarrierWithResources_count = Nothing
  , _sampleCountersInBuffer_atSampleIndex_withBarrier = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_Q_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_f_f_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CFloat -> CFloat -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CFloat -> CFloat -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_Q_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlComputeCommandEncoderDelegateClass #-}
mtlComputeCommandEncoderDelegateClass :: Class
mtlComputeCommandEncoderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLComputeCommandEncoder" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_setComputePipelineState = unSelector (mkSelector "setComputePipelineState:")
      sel_setBuffer_offset_atIndex = unSelector (mkSelector "setBuffer:offset:atIndex:")
      sel_setBufferOffset_atIndex = unSelector (mkSelector "setBufferOffset:atIndex:")
      sel_setBuffer_offset_attributeStride_atIndex = unSelector (mkSelector "setBuffer:offset:attributeStride:atIndex:")
      sel_setBufferOffset_attributeStride_atIndex = unSelector (mkSelector "setBufferOffset:attributeStride:atIndex:")
      sel_setVisibleFunctionTable_atBufferIndex = unSelector (mkSelector "setVisibleFunctionTable:atBufferIndex:")
      sel_setIntersectionFunctionTable_atBufferIndex = unSelector (mkSelector "setIntersectionFunctionTable:atBufferIndex:")
      sel_setAccelerationStructure_atBufferIndex = unSelector (mkSelector "setAccelerationStructure:atBufferIndex:")
      sel_setTexture_atIndex = unSelector (mkSelector "setTexture:atIndex:")
      sel_setSamplerState_atIndex = unSelector (mkSelector "setSamplerState:atIndex:")
      sel_setSamplerState_lodMinClamp_lodMaxClamp_atIndex = unSelector (mkSelector "setSamplerState:lodMinClamp:lodMaxClamp:atIndex:")
      sel_setThreadgroupMemoryLength_atIndex = unSelector (mkSelector "setThreadgroupMemoryLength:atIndex:")
      sel_setImageblockWidth_height = unSelector (mkSelector "setImageblockWidth:height:")
      sel_setStageInRegionWithIndirectBuffer_indirectBufferOffset = unSelector (mkSelector "setStageInRegionWithIndirectBuffer:indirectBufferOffset:")
      sel_updateFence = unSelector (mkSelector "updateFence:")
      sel_waitForFence = unSelector (mkSelector "waitForFence:")
      sel_useHeap = unSelector (mkSelector "useHeap:")
      sel_useHeaps_count = unSelector (mkSelector "useHeaps:count:")
      sel_executeCommandsInBuffer_indirectBuffer_indirectBufferOffset = unSelector (mkSelector "executeCommandsInBuffer:indirectBuffer:indirectBufferOffset:")
      sel_memoryBarrierWithResources_count = unSelector (mkSelector "memoryBarrierWithResources:count:")
      sel_sampleCountersInBuffer_atSampleIndex_withBarrier = unSelector (mkSelector "sampleCountersInBuffer:atSampleIndex:withBarrier:")
  -- setComputePipelineState:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setComputePipelineState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setComputePipelineState:" "v@:@" stub_0

  -- setBuffer:offset:atIndex:
  stub_1 <- wrap_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setBuffer_offset_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "setBuffer:offset:atIndex:" "v@:@QQ" stub_1

  -- setBufferOffset:atIndex:
  stub_2 <- wrap_Q_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setBufferOffset_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1)
  addObjCMethod cls "setBufferOffset:atIndex:" "v@:QQ" stub_2

  -- setBuffer:offset:attributeStride:atIndex:
  stub_3 <- wrap_at_Q_Q_Q_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setBuffer_offset_attributeStride_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2) (fromIntegral arg3)
  addObjCMethod cls "setBuffer:offset:attributeStride:atIndex:" "v@:@QQQ" stub_3

  -- setBufferOffset:attributeStride:atIndex:
  stub_4 <- wrap_Q_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setBufferOffset_attributeStride_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "setBufferOffset:attributeStride:atIndex:" "v@:QQQ" stub_4

  -- setVisibleFunctionTable:atBufferIndex:
  stub_5 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setVisibleFunctionTable_atBufferIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setVisibleFunctionTable:atBufferIndex:" "v@:@Q" stub_5

  -- setIntersectionFunctionTable:atBufferIndex:
  stub_6 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setIntersectionFunctionTable_atBufferIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setIntersectionFunctionTable:atBufferIndex:" "v@:@Q" stub_6

  -- setAccelerationStructure:atBufferIndex:
  stub_7 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setAccelerationStructure_atBufferIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setAccelerationStructure:atBufferIndex:" "v@:@Q" stub_7

  -- setTexture:atIndex:
  stub_8 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setTexture_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setTexture:atIndex:" "v@:@Q" stub_8

  -- setSamplerState:atIndex:
  stub_9 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setSamplerState_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setSamplerState:atIndex:" "v@:@Q" stub_9

  -- setSamplerState:lodMinClamp:lodMaxClamp:atIndex:
  stub_10 <- wrap_at_f_f_Q_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setSamplerState_lodMinClamp_lodMaxClamp_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (realToFrac arg1) (realToFrac arg2) (fromIntegral arg3)
  addObjCMethod cls "setSamplerState:lodMinClamp:lodMaxClamp:atIndex:" "v@:@ffQ" stub_10

  -- setThreadgroupMemoryLength:atIndex:
  stub_11 <- wrap_Q_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setThreadgroupMemoryLength_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1)
  addObjCMethod cls "setThreadgroupMemoryLength:atIndex:" "v@:QQ" stub_11

  -- setImageblockWidth:height:
  stub_12 <- wrap_Q_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setImageblockWidth_height rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1)
  addObjCMethod cls "setImageblockWidth:height:" "v@:QQ" stub_12

  -- setStageInRegionWithIndirectBuffer:indirectBufferOffset:
  stub_13 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _setStageInRegionWithIndirectBuffer_indirectBufferOffset rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setStageInRegionWithIndirectBuffer:indirectBufferOffset:" "v@:@Q" stub_13

  -- updateFence:
  stub_14 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _updateFence rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "updateFence:" "v@:@" stub_14

  -- waitForFence:
  stub_15 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _waitForFence rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "waitForFence:" "v@:@" stub_15

  -- useHeap:
  stub_16 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _useHeap rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "useHeap:" "v@:@" stub_16

  -- useHeaps:count:
  stub_17 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _useHeaps_count rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "useHeaps:count:" "v@:@Q" stub_17

  -- executeCommandsInBuffer:indirectBuffer:indirectBufferOffset:
  stub_18 <- wrap_at_at_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _executeCommandsInBuffer_indirectBuffer_indirectBufferOffset rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2)
  addObjCMethod cls "executeCommandsInBuffer:indirectBuffer:indirectBufferOffset:" "v@:@@Q" stub_18

  -- memoryBarrierWithResources:count:
  stub_19 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _memoryBarrierWithResources_count rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "memoryBarrierWithResources:count:" "v@:@Q" stub_19

  -- sampleCountersInBuffer:atSampleIndex:withBarrier:
  stub_20 <- wrap_at_Q_B_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    case _sampleCountersInBuffer_atSampleIndex_withBarrier rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (arg2 /= 0)
  addObjCMethod cls "sampleCountersInBuffer:atSampleIndex:withBarrier:" "v@:@QB" stub_20

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLComputeCommandEncoderOverrides
    if queriedSel == sel_setComputePipelineState then pure (maybe 0 (const 1) (_setComputePipelineState rec_))
    else if queriedSel == sel_setBuffer_offset_atIndex then pure (maybe 0 (const 1) (_setBuffer_offset_atIndex rec_))
    else if queriedSel == sel_setBufferOffset_atIndex then pure (maybe 0 (const 1) (_setBufferOffset_atIndex rec_))
    else if queriedSel == sel_setBuffer_offset_attributeStride_atIndex then pure (maybe 0 (const 1) (_setBuffer_offset_attributeStride_atIndex rec_))
    else if queriedSel == sel_setBufferOffset_attributeStride_atIndex then pure (maybe 0 (const 1) (_setBufferOffset_attributeStride_atIndex rec_))
    else if queriedSel == sel_setVisibleFunctionTable_atBufferIndex then pure (maybe 0 (const 1) (_setVisibleFunctionTable_atBufferIndex rec_))
    else if queriedSel == sel_setIntersectionFunctionTable_atBufferIndex then pure (maybe 0 (const 1) (_setIntersectionFunctionTable_atBufferIndex rec_))
    else if queriedSel == sel_setAccelerationStructure_atBufferIndex then pure (maybe 0 (const 1) (_setAccelerationStructure_atBufferIndex rec_))
    else if queriedSel == sel_setTexture_atIndex then pure (maybe 0 (const 1) (_setTexture_atIndex rec_))
    else if queriedSel == sel_setSamplerState_atIndex then pure (maybe 0 (const 1) (_setSamplerState_atIndex rec_))
    else if queriedSel == sel_setSamplerState_lodMinClamp_lodMaxClamp_atIndex then pure (maybe 0 (const 1) (_setSamplerState_lodMinClamp_lodMaxClamp_atIndex rec_))
    else if queriedSel == sel_setThreadgroupMemoryLength_atIndex then pure (maybe 0 (const 1) (_setThreadgroupMemoryLength_atIndex rec_))
    else if queriedSel == sel_setImageblockWidth_height then pure (maybe 0 (const 1) (_setImageblockWidth_height rec_))
    else if queriedSel == sel_setStageInRegionWithIndirectBuffer_indirectBufferOffset then pure (maybe 0 (const 1) (_setStageInRegionWithIndirectBuffer_indirectBufferOffset rec_))
    else if queriedSel == sel_updateFence then pure (maybe 0 (const 1) (_updateFence rec_))
    else if queriedSel == sel_waitForFence then pure (maybe 0 (const 1) (_waitForFence rec_))
    else if queriedSel == sel_useHeap then pure (maybe 0 (const 1) (_useHeap rec_))
    else if queriedSel == sel_useHeaps_count then pure (maybe 0 (const 1) (_useHeaps_count rec_))
    else if queriedSel == sel_executeCommandsInBuffer_indirectBuffer_indirectBufferOffset then pure (maybe 0 (const 1) (_executeCommandsInBuffer_indirectBuffer_indirectBufferOffset rec_))
    else if queriedSel == sel_memoryBarrierWithResources_count then pure (maybe 0 (const 1) (_memoryBarrierWithResources_count rec_))
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
newMTLComputeCommandEncoder :: MTLComputeCommandEncoderOverrides -> IO RawId
newMTLComputeCommandEncoder overrides = do
  inst <- class_createInstance mtlComputeCommandEncoderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
