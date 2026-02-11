{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLIndirectRenderCommand@.
--
-- Usage:
--
-- @
-- delegate <- newMTLIndirectRenderCommand defaultMTLIndirectRenderCommandOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLIndirectRenderCommand
  ( MTLIndirectRenderCommandOverrides(..)
  , defaultMTLIndirectRenderCommandOverrides
  , newMTLIndirectRenderCommand
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

-- | Overrides record for @\@protocol MTLIndirectRenderCommand@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLIndirectRenderCommandOverrides = MTLIndirectRenderCommandOverrides
  { _setRenderPipelineState :: !(Maybe (RawId -> IO ()))
  , _setVertexBuffer_offset_atIndex :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _setFragmentBuffer_offset_atIndex :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _setVertexBuffer_offset_attributeStride_atIndex :: !(Maybe (RawId -> Int -> Int -> Int -> IO ()))
  , _drawPatches_patchStart_patchCount_patchIndexBuffer_patchIndexBufferOffset_instanceCount_baseInstance_tessellationFactorBuffer_tessellationFactorBufferOffset_tessellationFactorBufferInstanceStride :: !(Maybe (Int -> Int -> Int -> RawId -> Int -> Int -> Int -> RawId -> Int -> Int -> IO ()))
  , _drawIndexedPatches_patchStart_patchCount_patchIndexBuffer_patchIndexBufferOffset_controlPointIndexBuffer_controlPointIndexBufferOffset_instanceCount_baseInstance_tessellationFactorBuffer_tessellationFactorBufferOffset_tessellationFactorBufferInstanceStride :: !(Maybe (Int -> Int -> Int -> RawId -> Int -> RawId -> Int -> Int -> Int -> RawId -> Int -> Int -> IO ()))
  , _setObjectThreadgroupMemoryLength_atIndex :: !(Maybe (Int -> Int -> IO ()))
  , _setObjectBuffer_offset_atIndex :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _setMeshBuffer_offset_atIndex :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _setBarrier :: !(Maybe (IO ()))
  , _clearBarrier :: !(Maybe (IO ()))
  , _setDepthStencilState :: !(Maybe (RawId -> IO ()))
  , _setDepthBias_slopeScale_clamp :: !(Maybe (Float -> Float -> Float -> IO ()))
  , _reset :: !(Maybe (IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLIndirectRenderCommandOverrides :: MTLIndirectRenderCommandOverrides
defaultMTLIndirectRenderCommandOverrides = MTLIndirectRenderCommandOverrides
  { _setRenderPipelineState = Nothing
  , _setVertexBuffer_offset_atIndex = Nothing
  , _setFragmentBuffer_offset_atIndex = Nothing
  , _setVertexBuffer_offset_attributeStride_atIndex = Nothing
  , _drawPatches_patchStart_patchCount_patchIndexBuffer_patchIndexBufferOffset_instanceCount_baseInstance_tessellationFactorBuffer_tessellationFactorBufferOffset_tessellationFactorBufferInstanceStride = Nothing
  , _drawIndexedPatches_patchStart_patchCount_patchIndexBuffer_patchIndexBufferOffset_controlPointIndexBuffer_controlPointIndexBufferOffset_instanceCount_baseInstance_tessellationFactorBuffer_tessellationFactorBufferOffset_tessellationFactorBufferInstanceStride = Nothing
  , _setObjectThreadgroupMemoryLength_atIndex = Nothing
  , _setObjectBuffer_offset_atIndex = Nothing
  , _setMeshBuffer_offset_atIndex = Nothing
  , _setBarrier = Nothing
  , _clearBarrier = Nothing
  , _setDepthStencilState = Nothing
  , _setDepthBias_slopeScale_clamp = Nothing
  , _reset = Nothing
  }

foreign import ccall "wrapper"
  wrap_f_f_f_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> CFloat -> CFloat -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> CFloat -> CFloat -> IO ()))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_Q_Q_Q_at_Q_at_Q_Q_Q_at_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> CULong -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> CULong -> CULong -> CULong -> Ptr ObjCObject -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> CULong -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> CULong -> CULong -> CULong -> Ptr ObjCObject -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_Q_Q_Q_at_Q_Q_Q_at_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> CULong -> Ptr ObjCObject -> CULong -> CULong -> CULong -> Ptr ObjCObject -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> CULong -> Ptr ObjCObject -> CULong -> CULong -> CULong -> Ptr ObjCObject -> CULong -> CULong -> IO ()))

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
{-# NOINLINE mtlIndirectRenderCommandDelegateClass #-}
mtlIndirectRenderCommandDelegateClass :: Class
mtlIndirectRenderCommandDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLIndirectRenderCommand" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_setRenderPipelineState = unSelector (mkSelector "setRenderPipelineState:")
      sel_setVertexBuffer_offset_atIndex = unSelector (mkSelector "setVertexBuffer:offset:atIndex:")
      sel_setFragmentBuffer_offset_atIndex = unSelector (mkSelector "setFragmentBuffer:offset:atIndex:")
      sel_setVertexBuffer_offset_attributeStride_atIndex = unSelector (mkSelector "setVertexBuffer:offset:attributeStride:atIndex:")
      sel_drawPatches_patchStart_patchCount_patchIndexBuffer_patchIndexBufferOffset_instanceCount_baseInstance_tessellationFactorBuffer_tessellationFactorBufferOffset_tessellationFactorBufferInstanceStride = unSelector (mkSelector "drawPatches:patchStart:patchCount:patchIndexBuffer:patchIndexBufferOffset:instanceCount:baseInstance:tessellationFactorBuffer:tessellationFactorBufferOffset:tessellationFactorBufferInstanceStride:")
      sel_drawIndexedPatches_patchStart_patchCount_patchIndexBuffer_patchIndexBufferOffset_controlPointIndexBuffer_controlPointIndexBufferOffset_instanceCount_baseInstance_tessellationFactorBuffer_tessellationFactorBufferOffset_tessellationFactorBufferInstanceStride = unSelector (mkSelector "drawIndexedPatches:patchStart:patchCount:patchIndexBuffer:patchIndexBufferOffset:controlPointIndexBuffer:controlPointIndexBufferOffset:instanceCount:baseInstance:tessellationFactorBuffer:tessellationFactorBufferOffset:tessellationFactorBufferInstanceStride:")
      sel_setObjectThreadgroupMemoryLength_atIndex = unSelector (mkSelector "setObjectThreadgroupMemoryLength:atIndex:")
      sel_setObjectBuffer_offset_atIndex = unSelector (mkSelector "setObjectBuffer:offset:atIndex:")
      sel_setMeshBuffer_offset_atIndex = unSelector (mkSelector "setMeshBuffer:offset:atIndex:")
      sel_setBarrier = unSelector (mkSelector "setBarrier")
      sel_clearBarrier = unSelector (mkSelector "clearBarrier")
      sel_setDepthStencilState = unSelector (mkSelector "setDepthStencilState:")
      sel_setDepthBias_slopeScale_clamp = unSelector (mkSelector "setDepthBias:slopeScale:clamp:")
      sel_reset = unSelector (mkSelector "reset")
  -- setRenderPipelineState:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _setRenderPipelineState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setRenderPipelineState:" "v@:@" stub_0

  -- setVertexBuffer:offset:atIndex:
  stub_1 <- wrap_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _setVertexBuffer_offset_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "setVertexBuffer:offset:atIndex:" "v@:@QQ" stub_1

  -- setFragmentBuffer:offset:atIndex:
  stub_2 <- wrap_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _setFragmentBuffer_offset_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "setFragmentBuffer:offset:atIndex:" "v@:@QQ" stub_2

  -- setVertexBuffer:offset:attributeStride:atIndex:
  stub_3 <- wrap_at_Q_Q_Q_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _setVertexBuffer_offset_attributeStride_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2) (fromIntegral arg3)
  addObjCMethod cls "setVertexBuffer:offset:attributeStride:atIndex:" "v@:@QQQ" stub_3

  -- drawPatches:patchStart:patchCount:patchIndexBuffer:patchIndexBufferOffset:instanceCount:baseInstance:tessellationFactorBuffer:tessellationFactorBufferOffset:tessellationFactorBufferInstanceStride:
  stub_4 <- wrap_Q_Q_Q_at_Q_Q_Q_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _drawPatches_patchStart_patchCount_patchIndexBuffer_patchIndexBufferOffset_instanceCount_baseInstance_tessellationFactorBuffer_tessellationFactorBufferOffset_tessellationFactorBufferInstanceStride rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1) (fromIntegral arg2) (RawId arg3) (fromIntegral arg4) (fromIntegral arg5) (fromIntegral arg6) (RawId arg7) (fromIntegral arg8) (fromIntegral arg9)
  addObjCMethod cls "drawPatches:patchStart:patchCount:patchIndexBuffer:patchIndexBufferOffset:instanceCount:baseInstance:tessellationFactorBuffer:tessellationFactorBufferOffset:tessellationFactorBufferInstanceStride:" "v@:QQQ@QQQ@QQ" stub_4

  -- drawIndexedPatches:patchStart:patchCount:patchIndexBuffer:patchIndexBufferOffset:controlPointIndexBuffer:controlPointIndexBufferOffset:instanceCount:baseInstance:tessellationFactorBuffer:tessellationFactorBufferOffset:tessellationFactorBufferInstanceStride:
  stub_5 <- wrap_Q_Q_Q_at_Q_at_Q_Q_Q_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _drawIndexedPatches_patchStart_patchCount_patchIndexBuffer_patchIndexBufferOffset_controlPointIndexBuffer_controlPointIndexBufferOffset_instanceCount_baseInstance_tessellationFactorBuffer_tessellationFactorBufferOffset_tessellationFactorBufferInstanceStride rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1) (fromIntegral arg2) (RawId arg3) (fromIntegral arg4) (RawId arg5) (fromIntegral arg6) (fromIntegral arg7) (fromIntegral arg8) (RawId arg9) (fromIntegral arg10) (fromIntegral arg11)
  addObjCMethod cls "drawIndexedPatches:patchStart:patchCount:patchIndexBuffer:patchIndexBufferOffset:controlPointIndexBuffer:controlPointIndexBufferOffset:instanceCount:baseInstance:tessellationFactorBuffer:tessellationFactorBufferOffset:tessellationFactorBufferInstanceStride:" "v@:QQQ@Q@QQQ@QQ" stub_5

  -- setObjectThreadgroupMemoryLength:atIndex:
  stub_6 <- wrap_Q_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _setObjectThreadgroupMemoryLength_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1)
  addObjCMethod cls "setObjectThreadgroupMemoryLength:atIndex:" "v@:QQ" stub_6

  -- setObjectBuffer:offset:atIndex:
  stub_7 <- wrap_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _setObjectBuffer_offset_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "setObjectBuffer:offset:atIndex:" "v@:@QQ" stub_7

  -- setMeshBuffer:offset:atIndex:
  stub_8 <- wrap_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _setMeshBuffer_offset_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "setMeshBuffer:offset:atIndex:" "v@:@QQ" stub_8

  -- setBarrier
  stub_9 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _setBarrier rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "setBarrier" "v@:" stub_9

  -- clearBarrier
  stub_10 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _clearBarrier rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "clearBarrier" "v@:" stub_10

  -- setDepthStencilState:
  stub_11 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _setDepthStencilState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setDepthStencilState:" "v@:@" stub_11

  -- setDepthBias:slopeScale:clamp:
  stub_12 <- wrap_f_f_f_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _setDepthBias_slopeScale_clamp rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0) (realToFrac arg1) (realToFrac arg2)
  addObjCMethod cls "setDepthBias:slopeScale:clamp:" "v@:fff" stub_12

  -- reset
  stub_13 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    case _reset rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "reset" "v@:" stub_13

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectRenderCommandOverrides
    if queriedSel == sel_setRenderPipelineState then pure (maybe 0 (const 1) (_setRenderPipelineState rec_))
    else if queriedSel == sel_setVertexBuffer_offset_atIndex then pure (maybe 0 (const 1) (_setVertexBuffer_offset_atIndex rec_))
    else if queriedSel == sel_setFragmentBuffer_offset_atIndex then pure (maybe 0 (const 1) (_setFragmentBuffer_offset_atIndex rec_))
    else if queriedSel == sel_setVertexBuffer_offset_attributeStride_atIndex then pure (maybe 0 (const 1) (_setVertexBuffer_offset_attributeStride_atIndex rec_))
    else if queriedSel == sel_drawPatches_patchStart_patchCount_patchIndexBuffer_patchIndexBufferOffset_instanceCount_baseInstance_tessellationFactorBuffer_tessellationFactorBufferOffset_tessellationFactorBufferInstanceStride then pure (maybe 0 (const 1) (_drawPatches_patchStart_patchCount_patchIndexBuffer_patchIndexBufferOffset_instanceCount_baseInstance_tessellationFactorBuffer_tessellationFactorBufferOffset_tessellationFactorBufferInstanceStride rec_))
    else if queriedSel == sel_drawIndexedPatches_patchStart_patchCount_patchIndexBuffer_patchIndexBufferOffset_controlPointIndexBuffer_controlPointIndexBufferOffset_instanceCount_baseInstance_tessellationFactorBuffer_tessellationFactorBufferOffset_tessellationFactorBufferInstanceStride then pure (maybe 0 (const 1) (_drawIndexedPatches_patchStart_patchCount_patchIndexBuffer_patchIndexBufferOffset_controlPointIndexBuffer_controlPointIndexBufferOffset_instanceCount_baseInstance_tessellationFactorBuffer_tessellationFactorBufferOffset_tessellationFactorBufferInstanceStride rec_))
    else if queriedSel == sel_setObjectThreadgroupMemoryLength_atIndex then pure (maybe 0 (const 1) (_setObjectThreadgroupMemoryLength_atIndex rec_))
    else if queriedSel == sel_setObjectBuffer_offset_atIndex then pure (maybe 0 (const 1) (_setObjectBuffer_offset_atIndex rec_))
    else if queriedSel == sel_setMeshBuffer_offset_atIndex then pure (maybe 0 (const 1) (_setMeshBuffer_offset_atIndex rec_))
    else if queriedSel == sel_setBarrier then pure (maybe 0 (const 1) (_setBarrier rec_))
    else if queriedSel == sel_clearBarrier then pure (maybe 0 (const 1) (_clearBarrier rec_))
    else if queriedSel == sel_setDepthStencilState then pure (maybe 0 (const 1) (_setDepthStencilState rec_))
    else if queriedSel == sel_setDepthBias_slopeScale_clamp then pure (maybe 0 (const 1) (_setDepthBias_slopeScale_clamp rec_))
    else if queriedSel == sel_reset then pure (maybe 0 (const 1) (_reset rec_))
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
newMTLIndirectRenderCommand :: MTLIndirectRenderCommandOverrides -> IO RawId
newMTLIndirectRenderCommand overrides = do
  inst <- class_createInstance mtlIndirectRenderCommandDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
