{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTL4ComputeCommandEncoder@.
--
-- Usage:
--
-- @
-- delegate <- newMTL4ComputeCommandEncoder defaultMTL4ComputeCommandEncoderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTL4ComputeCommandEncoder
  ( MTL4ComputeCommandEncoderOverrides(..)
  , defaultMTL4ComputeCommandEncoderOverrides
  , newMTL4ComputeCommandEncoder
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

-- | Overrides record for @\@protocol MTL4ComputeCommandEncoder@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTL4ComputeCommandEncoderOverrides = MTL4ComputeCommandEncoderOverrides
  { _setComputePipelineState :: !(Maybe (RawId -> IO ()))
  , _setThreadgroupMemoryLength_atIndex :: !(Maybe (Int -> Int -> IO ()))
  , _setImageblockWidth_height :: !(Maybe (Int -> Int -> IO ()))
  , _dispatchThreadsWithIndirectBuffer :: !(Maybe (Int -> IO ()))
  , _executeCommandsInBuffer_indirectBuffer :: !(Maybe (RawId -> Int -> IO ()))
  , _copyFromTexture_toTexture :: !(Maybe (RawId -> RawId -> IO ()))
  , _copyFromTexture_sourceSlice_sourceLevel_toTexture_destinationSlice_destinationLevel_sliceCount_levelCount :: !(Maybe (RawId -> Int -> Int -> RawId -> Int -> Int -> Int -> Int -> IO ()))
  , _copyFromBuffer_sourceOffset_toBuffer_destinationOffset_size :: !(Maybe (RawId -> Int -> RawId -> Int -> Int -> IO ()))
  , _copyFromTensor_sourceOrigin_sourceDimensions_toTensor_destinationOrigin_destinationDimensions :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()))
  , _generateMipmapsForTexture :: !(Maybe (RawId -> IO ()))
  , _optimizeContentsForGPUAccess :: !(Maybe (RawId -> IO ()))
  , _optimizeContentsForGPUAccess_slice_level :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _optimizeContentsForCPUAccess :: !(Maybe (RawId -> IO ()))
  , _optimizeContentsForCPUAccess_slice_level :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _setArgumentTable :: !(Maybe (RawId -> IO ()))
  , _copyAccelerationStructure_toAccelerationStructure :: !(Maybe (RawId -> RawId -> IO ()))
  , _copyAndCompactAccelerationStructure_toAccelerationStructure :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTL4ComputeCommandEncoderOverrides :: MTL4ComputeCommandEncoderOverrides
defaultMTL4ComputeCommandEncoderOverrides = MTL4ComputeCommandEncoderOverrides
  { _setComputePipelineState = Nothing
  , _setThreadgroupMemoryLength_atIndex = Nothing
  , _setImageblockWidth_height = Nothing
  , _dispatchThreadsWithIndirectBuffer = Nothing
  , _executeCommandsInBuffer_indirectBuffer = Nothing
  , _copyFromTexture_toTexture = Nothing
  , _copyFromTexture_sourceSlice_sourceLevel_toTexture_destinationSlice_destinationLevel_sliceCount_levelCount = Nothing
  , _copyFromBuffer_sourceOffset_toBuffer_destinationOffset_size = Nothing
  , _copyFromTensor_sourceOrigin_sourceDimensions_toTensor_destinationOrigin_destinationDimensions = Nothing
  , _generateMipmapsForTexture = Nothing
  , _optimizeContentsForGPUAccess = Nothing
  , _optimizeContentsForGPUAccess_slice_level = Nothing
  , _optimizeContentsForCPUAccess = Nothing
  , _optimizeContentsForCPUAccess_slice_level = Nothing
  , _setArgumentTable = Nothing
  , _copyAccelerationStructure_toAccelerationStructure = Nothing
  , _copyAndCompactAccelerationStructure_toAccelerationStructure = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_at_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_Q_at_Q_Q_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> Ptr ObjCObject -> CULong -> CULong -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> Ptr ObjCObject -> CULong -> CULong -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtL4ComputeCommandEncoderDelegateClass #-}
mtL4ComputeCommandEncoderDelegateClass :: Class
mtL4ComputeCommandEncoderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTL4ComputeCommandEncoder" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_setComputePipelineState = unSelector (mkSelector "setComputePipelineState:")
      sel_setThreadgroupMemoryLength_atIndex = unSelector (mkSelector "setThreadgroupMemoryLength:atIndex:")
      sel_setImageblockWidth_height = unSelector (mkSelector "setImageblockWidth:height:")
      sel_dispatchThreadsWithIndirectBuffer = unSelector (mkSelector "dispatchThreadsWithIndirectBuffer:")
      sel_executeCommandsInBuffer_indirectBuffer = unSelector (mkSelector "executeCommandsInBuffer:indirectBuffer:")
      sel_copyFromTexture_toTexture = unSelector (mkSelector "copyFromTexture:toTexture:")
      sel_copyFromTexture_sourceSlice_sourceLevel_toTexture_destinationSlice_destinationLevel_sliceCount_levelCount = unSelector (mkSelector "copyFromTexture:sourceSlice:sourceLevel:toTexture:destinationSlice:destinationLevel:sliceCount:levelCount:")
      sel_copyFromBuffer_sourceOffset_toBuffer_destinationOffset_size = unSelector (mkSelector "copyFromBuffer:sourceOffset:toBuffer:destinationOffset:size:")
      sel_copyFromTensor_sourceOrigin_sourceDimensions_toTensor_destinationOrigin_destinationDimensions = unSelector (mkSelector "copyFromTensor:sourceOrigin:sourceDimensions:toTensor:destinationOrigin:destinationDimensions:")
      sel_generateMipmapsForTexture = unSelector (mkSelector "generateMipmapsForTexture:")
      sel_optimizeContentsForGPUAccess = unSelector (mkSelector "optimizeContentsForGPUAccess:")
      sel_optimizeContentsForGPUAccess_slice_level = unSelector (mkSelector "optimizeContentsForGPUAccess:slice:level:")
      sel_optimizeContentsForCPUAccess = unSelector (mkSelector "optimizeContentsForCPUAccess:")
      sel_optimizeContentsForCPUAccess_slice_level = unSelector (mkSelector "optimizeContentsForCPUAccess:slice:level:")
      sel_setArgumentTable = unSelector (mkSelector "setArgumentTable:")
      sel_copyAccelerationStructure_toAccelerationStructure = unSelector (mkSelector "copyAccelerationStructure:toAccelerationStructure:")
      sel_copyAndCompactAccelerationStructure_toAccelerationStructure = unSelector (mkSelector "copyAndCompactAccelerationStructure:toAccelerationStructure:")
  -- setComputePipelineState:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _setComputePipelineState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setComputePipelineState:" "v@:@" stub_0

  -- setThreadgroupMemoryLength:atIndex:
  stub_1 <- wrap_Q_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _setThreadgroupMemoryLength_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1)
  addObjCMethod cls "setThreadgroupMemoryLength:atIndex:" "v@:QQ" stub_1

  -- setImageblockWidth:height:
  stub_2 <- wrap_Q_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _setImageblockWidth_height rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1)
  addObjCMethod cls "setImageblockWidth:height:" "v@:QQ" stub_2

  -- dispatchThreadsWithIndirectBuffer:
  stub_3 <- wrap_Q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _dispatchThreadsWithIndirectBuffer rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "dispatchThreadsWithIndirectBuffer:" "v@:Q" stub_3

  -- executeCommandsInBuffer:indirectBuffer:
  stub_4 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _executeCommandsInBuffer_indirectBuffer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "executeCommandsInBuffer:indirectBuffer:" "v@:@Q" stub_4

  -- copyFromTexture:toTexture:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _copyFromTexture_toTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "copyFromTexture:toTexture:" "v@:@@" stub_5

  -- copyFromTexture:sourceSlice:sourceLevel:toTexture:destinationSlice:destinationLevel:sliceCount:levelCount:
  stub_6 <- wrap_at_Q_Q_at_Q_Q_Q_Q_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _copyFromTexture_sourceSlice_sourceLevel_toTexture_destinationSlice_destinationLevel_sliceCount_levelCount rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2) (RawId arg3) (fromIntegral arg4) (fromIntegral arg5) (fromIntegral arg6) (fromIntegral arg7)
  addObjCMethod cls "copyFromTexture:sourceSlice:sourceLevel:toTexture:destinationSlice:destinationLevel:sliceCount:levelCount:" "v@:@QQ@QQQQ" stub_6

  -- copyFromBuffer:sourceOffset:toBuffer:destinationOffset:size:
  stub_7 <- wrap_at_Q_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _copyFromBuffer_sourceOffset_toBuffer_destinationOffset_size rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (RawId arg2) (fromIntegral arg3) (fromIntegral arg4)
  addObjCMethod cls "copyFromBuffer:sourceOffset:toBuffer:destinationOffset:size:" "v@:@Q@QQ" stub_7

  -- copyFromTensor:sourceOrigin:sourceDimensions:toTensor:destinationOrigin:destinationDimensions:
  stub_8 <- wrap_at_at_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 arg5 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _copyFromTensor_sourceOrigin_sourceDimensions_toTensor_destinationOrigin_destinationDimensions rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4) (RawId arg5)
  addObjCMethod cls "copyFromTensor:sourceOrigin:sourceDimensions:toTensor:destinationOrigin:destinationDimensions:" "v@:@@@@@@" stub_8

  -- generateMipmapsForTexture:
  stub_9 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _generateMipmapsForTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "generateMipmapsForTexture:" "v@:@" stub_9

  -- optimizeContentsForGPUAccess:
  stub_10 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _optimizeContentsForGPUAccess rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "optimizeContentsForGPUAccess:" "v@:@" stub_10

  -- optimizeContentsForGPUAccess:slice:level:
  stub_11 <- wrap_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _optimizeContentsForGPUAccess_slice_level rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "optimizeContentsForGPUAccess:slice:level:" "v@:@QQ" stub_11

  -- optimizeContentsForCPUAccess:
  stub_12 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _optimizeContentsForCPUAccess rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "optimizeContentsForCPUAccess:" "v@:@" stub_12

  -- optimizeContentsForCPUAccess:slice:level:
  stub_13 <- wrap_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _optimizeContentsForCPUAccess_slice_level rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "optimizeContentsForCPUAccess:slice:level:" "v@:@QQ" stub_13

  -- setArgumentTable:
  stub_14 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _setArgumentTable rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setArgumentTable:" "v@:@" stub_14

  -- copyAccelerationStructure:toAccelerationStructure:
  stub_15 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _copyAccelerationStructure_toAccelerationStructure rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "copyAccelerationStructure:toAccelerationStructure:" "v@:@@" stub_15

  -- copyAndCompactAccelerationStructure:toAccelerationStructure:
  stub_16 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    case _copyAndCompactAccelerationStructure_toAccelerationStructure rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "copyAndCompactAccelerationStructure:toAccelerationStructure:" "v@:@@" stub_16

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ComputeCommandEncoderOverrides
    if queriedSel == sel_setComputePipelineState then pure (maybe 0 (const 1) (_setComputePipelineState rec_))
    else if queriedSel == sel_setThreadgroupMemoryLength_atIndex then pure (maybe 0 (const 1) (_setThreadgroupMemoryLength_atIndex rec_))
    else if queriedSel == sel_setImageblockWidth_height then pure (maybe 0 (const 1) (_setImageblockWidth_height rec_))
    else if queriedSel == sel_dispatchThreadsWithIndirectBuffer then pure (maybe 0 (const 1) (_dispatchThreadsWithIndirectBuffer rec_))
    else if queriedSel == sel_executeCommandsInBuffer_indirectBuffer then pure (maybe 0 (const 1) (_executeCommandsInBuffer_indirectBuffer rec_))
    else if queriedSel == sel_copyFromTexture_toTexture then pure (maybe 0 (const 1) (_copyFromTexture_toTexture rec_))
    else if queriedSel == sel_copyFromTexture_sourceSlice_sourceLevel_toTexture_destinationSlice_destinationLevel_sliceCount_levelCount then pure (maybe 0 (const 1) (_copyFromTexture_sourceSlice_sourceLevel_toTexture_destinationSlice_destinationLevel_sliceCount_levelCount rec_))
    else if queriedSel == sel_copyFromBuffer_sourceOffset_toBuffer_destinationOffset_size then pure (maybe 0 (const 1) (_copyFromBuffer_sourceOffset_toBuffer_destinationOffset_size rec_))
    else if queriedSel == sel_copyFromTensor_sourceOrigin_sourceDimensions_toTensor_destinationOrigin_destinationDimensions then pure (maybe 0 (const 1) (_copyFromTensor_sourceOrigin_sourceDimensions_toTensor_destinationOrigin_destinationDimensions rec_))
    else if queriedSel == sel_generateMipmapsForTexture then pure (maybe 0 (const 1) (_generateMipmapsForTexture rec_))
    else if queriedSel == sel_optimizeContentsForGPUAccess then pure (maybe 0 (const 1) (_optimizeContentsForGPUAccess rec_))
    else if queriedSel == sel_optimizeContentsForGPUAccess_slice_level then pure (maybe 0 (const 1) (_optimizeContentsForGPUAccess_slice_level rec_))
    else if queriedSel == sel_optimizeContentsForCPUAccess then pure (maybe 0 (const 1) (_optimizeContentsForCPUAccess rec_))
    else if queriedSel == sel_optimizeContentsForCPUAccess_slice_level then pure (maybe 0 (const 1) (_optimizeContentsForCPUAccess_slice_level rec_))
    else if queriedSel == sel_setArgumentTable then pure (maybe 0 (const 1) (_setArgumentTable rec_))
    else if queriedSel == sel_copyAccelerationStructure_toAccelerationStructure then pure (maybe 0 (const 1) (_copyAccelerationStructure_toAccelerationStructure rec_))
    else if queriedSel == sel_copyAndCompactAccelerationStructure_toAccelerationStructure then pure (maybe 0 (const 1) (_copyAndCompactAccelerationStructure_toAccelerationStructure rec_))
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
newMTL4ComputeCommandEncoder :: MTL4ComputeCommandEncoderOverrides -> IO RawId
newMTL4ComputeCommandEncoder overrides = do
  inst <- class_createInstance mtL4ComputeCommandEncoderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
