{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLIndirectComputeCommand@.
--
-- Usage:
--
-- @
-- delegate <- newMTLIndirectComputeCommand defaultMTLIndirectComputeCommandOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLIndirectComputeCommand
  ( MTLIndirectComputeCommandOverrides(..)
  , defaultMTLIndirectComputeCommandOverrides
  , newMTLIndirectComputeCommand
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

-- | Overrides record for @\@protocol MTLIndirectComputeCommand@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLIndirectComputeCommandOverrides = MTLIndirectComputeCommandOverrides
  { _setComputePipelineState :: !(Maybe (RawId -> IO ()))
  , _setKernelBuffer_offset_atIndex :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _setKernelBuffer_offset_attributeStride_atIndex :: !(Maybe (RawId -> Int -> Int -> Int -> IO ()))
  , _setBarrier :: !(Maybe (IO ()))
  , _clearBarrier :: !(Maybe (IO ()))
  , _setImageblockWidth_height :: !(Maybe (Int -> Int -> IO ()))
  , _reset :: !(Maybe (IO ()))
  , _setThreadgroupMemoryLength_atIndex :: !(Maybe (Int -> Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLIndirectComputeCommandOverrides :: MTLIndirectComputeCommandOverrides
defaultMTLIndirectComputeCommandOverrides = MTLIndirectComputeCommandOverrides
  { _setComputePipelineState = Nothing
  , _setKernelBuffer_offset_atIndex = Nothing
  , _setKernelBuffer_offset_attributeStride_atIndex = Nothing
  , _setBarrier = Nothing
  , _clearBarrier = Nothing
  , _setImageblockWidth_height = Nothing
  , _reset = Nothing
  , _setThreadgroupMemoryLength_atIndex = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> CULong -> IO ()))

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
{-# NOINLINE mtlIndirectComputeCommandDelegateClass #-}
mtlIndirectComputeCommandDelegateClass :: Class
mtlIndirectComputeCommandDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLIndirectComputeCommand" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_setComputePipelineState = unSelector (mkSelector "setComputePipelineState:")
      sel_setKernelBuffer_offset_atIndex = unSelector (mkSelector "setKernelBuffer:offset:atIndex:")
      sel_setKernelBuffer_offset_attributeStride_atIndex = unSelector (mkSelector "setKernelBuffer:offset:attributeStride:atIndex:")
      sel_setBarrier = unSelector (mkSelector "setBarrier")
      sel_clearBarrier = unSelector (mkSelector "clearBarrier")
      sel_setImageblockWidth_height = unSelector (mkSelector "setImageblockWidth:height:")
      sel_reset = unSelector (mkSelector "reset")
      sel_setThreadgroupMemoryLength_atIndex = unSelector (mkSelector "setThreadgroupMemoryLength:atIndex:")
  -- setComputePipelineState:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectComputeCommandOverrides
    case _setComputePipelineState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setComputePipelineState:" "v@:@" stub_0

  -- setKernelBuffer:offset:atIndex:
  stub_1 <- wrap_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectComputeCommandOverrides
    case _setKernelBuffer_offset_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "setKernelBuffer:offset:atIndex:" "v@:@QQ" stub_1

  -- setKernelBuffer:offset:attributeStride:atIndex:
  stub_2 <- wrap_at_Q_Q_Q_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectComputeCommandOverrides
    case _setKernelBuffer_offset_attributeStride_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2) (fromIntegral arg3)
  addObjCMethod cls "setKernelBuffer:offset:attributeStride:atIndex:" "v@:@QQQ" stub_2

  -- setBarrier
  stub_3 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectComputeCommandOverrides
    case _setBarrier rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "setBarrier" "v@:" stub_3

  -- clearBarrier
  stub_4 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectComputeCommandOverrides
    case _clearBarrier rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "clearBarrier" "v@:" stub_4

  -- setImageblockWidth:height:
  stub_5 <- wrap_Q_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectComputeCommandOverrides
    case _setImageblockWidth_height rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1)
  addObjCMethod cls "setImageblockWidth:height:" "v@:QQ" stub_5

  -- reset
  stub_6 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectComputeCommandOverrides
    case _reset rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "reset" "v@:" stub_6

  -- setThreadgroupMemoryLength:atIndex:
  stub_7 <- wrap_Q_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectComputeCommandOverrides
    case _setThreadgroupMemoryLength_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1)
  addObjCMethod cls "setThreadgroupMemoryLength:atIndex:" "v@:QQ" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectComputeCommandOverrides
    if queriedSel == sel_setComputePipelineState then pure (maybe 0 (const 1) (_setComputePipelineState rec_))
    else if queriedSel == sel_setKernelBuffer_offset_atIndex then pure (maybe 0 (const 1) (_setKernelBuffer_offset_atIndex rec_))
    else if queriedSel == sel_setKernelBuffer_offset_attributeStride_atIndex then pure (maybe 0 (const 1) (_setKernelBuffer_offset_attributeStride_atIndex rec_))
    else if queriedSel == sel_setBarrier then pure (maybe 0 (const 1) (_setBarrier rec_))
    else if queriedSel == sel_clearBarrier then pure (maybe 0 (const 1) (_clearBarrier rec_))
    else if queriedSel == sel_setImageblockWidth_height then pure (maybe 0 (const 1) (_setImageblockWidth_height rec_))
    else if queriedSel == sel_reset then pure (maybe 0 (const 1) (_reset rec_))
    else if queriedSel == sel_setThreadgroupMemoryLength_atIndex then pure (maybe 0 (const 1) (_setThreadgroupMemoryLength_atIndex rec_))
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
newMTLIndirectComputeCommand :: MTLIndirectComputeCommandOverrides -> IO RawId
newMTLIndirectComputeCommand overrides = do
  inst <- class_createInstance mtlIndirectComputeCommandDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
