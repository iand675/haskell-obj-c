{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLArgumentEncoder@.
--
-- Usage:
--
-- @
-- delegate <- newMTLArgumentEncoder defaultMTLArgumentEncoderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLArgumentEncoder
  ( MTLArgumentEncoderOverrides(..)
  , defaultMTLArgumentEncoderOverrides
  , newMTLArgumentEncoder
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

-- | Overrides record for @\@protocol MTLArgumentEncoder@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLArgumentEncoderOverrides = MTLArgumentEncoderOverrides
  { _setArgumentBuffer_offset :: !(Maybe (RawId -> Int -> IO ()))
  , _setArgumentBuffer_startOffset_arrayElement :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _setBuffer_offset_atIndex :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _setTexture_atIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _setSamplerState_atIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _setRenderPipelineState_atIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _setComputePipelineState_atIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _setIndirectCommandBuffer_atIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _setAccelerationStructure_atIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _newArgumentEncoderForBufferAtIndex :: !(Maybe (Int -> IO RawId))
  , _setVisibleFunctionTable_atIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _setIntersectionFunctionTable_atIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _setDepthStencilState_atIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _device :: !(Maybe (IO RawId))
  , _label :: !(Maybe (IO RawId))
  , _setLabel :: !(Maybe (RawId -> IO ()))
  , _encodedLength :: !(Maybe (IO Int))
  , _alignment :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLArgumentEncoderOverrides :: MTLArgumentEncoderOverrides
defaultMTLArgumentEncoderOverrides = MTLArgumentEncoderOverrides
  { _setArgumentBuffer_offset = Nothing
  , _setArgumentBuffer_startOffset_arrayElement = Nothing
  , _setBuffer_offset_atIndex = Nothing
  , _setTexture_atIndex = Nothing
  , _setSamplerState_atIndex = Nothing
  , _setRenderPipelineState_atIndex = Nothing
  , _setComputePipelineState_atIndex = Nothing
  , _setIndirectCommandBuffer_atIndex = Nothing
  , _setAccelerationStructure_atIndex = Nothing
  , _newArgumentEncoderForBufferAtIndex = Nothing
  , _setVisibleFunctionTable_atIndex = Nothing
  , _setIntersectionFunctionTable_atIndex = Nothing
  , _setDepthStencilState_atIndex = Nothing
  , _device = Nothing
  , _label = Nothing
  , _setLabel = Nothing
  , _encodedLength = Nothing
  , _alignment = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_Q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlArgumentEncoderDelegateClass #-}
mtlArgumentEncoderDelegateClass :: Class
mtlArgumentEncoderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLArgumentEncoder" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_setArgumentBuffer_offset = unSelector (mkSelector "setArgumentBuffer:offset:")
      sel_setArgumentBuffer_startOffset_arrayElement = unSelector (mkSelector "setArgumentBuffer:startOffset:arrayElement:")
      sel_setBuffer_offset_atIndex = unSelector (mkSelector "setBuffer:offset:atIndex:")
      sel_setTexture_atIndex = unSelector (mkSelector "setTexture:atIndex:")
      sel_setSamplerState_atIndex = unSelector (mkSelector "setSamplerState:atIndex:")
      sel_setRenderPipelineState_atIndex = unSelector (mkSelector "setRenderPipelineState:atIndex:")
      sel_setComputePipelineState_atIndex = unSelector (mkSelector "setComputePipelineState:atIndex:")
      sel_setIndirectCommandBuffer_atIndex = unSelector (mkSelector "setIndirectCommandBuffer:atIndex:")
      sel_setAccelerationStructure_atIndex = unSelector (mkSelector "setAccelerationStructure:atIndex:")
      sel_newArgumentEncoderForBufferAtIndex = unSelector (mkSelector "newArgumentEncoderForBufferAtIndex:")
      sel_setVisibleFunctionTable_atIndex = unSelector (mkSelector "setVisibleFunctionTable:atIndex:")
      sel_setIntersectionFunctionTable_atIndex = unSelector (mkSelector "setIntersectionFunctionTable:atIndex:")
      sel_setDepthStencilState_atIndex = unSelector (mkSelector "setDepthStencilState:atIndex:")
      sel_device = unSelector (mkSelector "device")
      sel_label = unSelector (mkSelector "label")
      sel_setLabel = unSelector (mkSelector "setLabel:")
      sel_encodedLength = unSelector (mkSelector "encodedLength")
      sel_alignment = unSelector (mkSelector "alignment")
  -- setArgumentBuffer:offset:
  stub_0 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _setArgumentBuffer_offset rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setArgumentBuffer:offset:" "v@:@Q" stub_0

  -- setArgumentBuffer:startOffset:arrayElement:
  stub_1 <- wrap_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _setArgumentBuffer_startOffset_arrayElement rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "setArgumentBuffer:startOffset:arrayElement:" "v@:@QQ" stub_1

  -- setBuffer:offset:atIndex:
  stub_2 <- wrap_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _setBuffer_offset_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "setBuffer:offset:atIndex:" "v@:@QQ" stub_2

  -- setTexture:atIndex:
  stub_3 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _setTexture_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setTexture:atIndex:" "v@:@Q" stub_3

  -- setSamplerState:atIndex:
  stub_4 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _setSamplerState_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setSamplerState:atIndex:" "v@:@Q" stub_4

  -- setRenderPipelineState:atIndex:
  stub_5 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _setRenderPipelineState_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setRenderPipelineState:atIndex:" "v@:@Q" stub_5

  -- setComputePipelineState:atIndex:
  stub_6 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _setComputePipelineState_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setComputePipelineState:atIndex:" "v@:@Q" stub_6

  -- setIndirectCommandBuffer:atIndex:
  stub_7 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _setIndirectCommandBuffer_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setIndirectCommandBuffer:atIndex:" "v@:@Q" stub_7

  -- setAccelerationStructure:atIndex:
  stub_8 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _setAccelerationStructure_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setAccelerationStructure:atIndex:" "v@:@Q" stub_8

  -- newArgumentEncoderForBufferAtIndex:
  stub_9 <- wrap_Q_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _newArgumentEncoderForBufferAtIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "newArgumentEncoderForBufferAtIndex:" "@@:Q" stub_9

  -- setVisibleFunctionTable:atIndex:
  stub_10 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _setVisibleFunctionTable_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setVisibleFunctionTable:atIndex:" "v@:@Q" stub_10

  -- setIntersectionFunctionTable:atIndex:
  stub_11 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _setIntersectionFunctionTable_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setIntersectionFunctionTable:atIndex:" "v@:@Q" stub_11

  -- setDepthStencilState:atIndex:
  stub_12 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _setDepthStencilState_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setDepthStencilState:atIndex:" "v@:@Q" stub_12

  -- device
  stub_13 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _device rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "device" "@@:" stub_13

  -- label
  stub_14 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _label rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "label" "@@:" stub_14

  -- setLabel:
  stub_15 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _setLabel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setLabel:" "v@:@" stub_15

  -- encodedLength
  stub_16 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _encodedLength rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "encodedLength" "Q@:" stub_16

  -- alignment
  stub_17 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    case _alignment rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "alignment" "Q@:" stub_17

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLArgumentEncoderOverrides
    if queriedSel == sel_setArgumentBuffer_offset then pure (maybe 0 (const 1) (_setArgumentBuffer_offset rec_))
    else if queriedSel == sel_setArgumentBuffer_startOffset_arrayElement then pure (maybe 0 (const 1) (_setArgumentBuffer_startOffset_arrayElement rec_))
    else if queriedSel == sel_setBuffer_offset_atIndex then pure (maybe 0 (const 1) (_setBuffer_offset_atIndex rec_))
    else if queriedSel == sel_setTexture_atIndex then pure (maybe 0 (const 1) (_setTexture_atIndex rec_))
    else if queriedSel == sel_setSamplerState_atIndex then pure (maybe 0 (const 1) (_setSamplerState_atIndex rec_))
    else if queriedSel == sel_setRenderPipelineState_atIndex then pure (maybe 0 (const 1) (_setRenderPipelineState_atIndex rec_))
    else if queriedSel == sel_setComputePipelineState_atIndex then pure (maybe 0 (const 1) (_setComputePipelineState_atIndex rec_))
    else if queriedSel == sel_setIndirectCommandBuffer_atIndex then pure (maybe 0 (const 1) (_setIndirectCommandBuffer_atIndex rec_))
    else if queriedSel == sel_setAccelerationStructure_atIndex then pure (maybe 0 (const 1) (_setAccelerationStructure_atIndex rec_))
    else if queriedSel == sel_newArgumentEncoderForBufferAtIndex then pure (maybe 0 (const 1) (_newArgumentEncoderForBufferAtIndex rec_))
    else if queriedSel == sel_setVisibleFunctionTable_atIndex then pure (maybe 0 (const 1) (_setVisibleFunctionTable_atIndex rec_))
    else if queriedSel == sel_setIntersectionFunctionTable_atIndex then pure (maybe 0 (const 1) (_setIntersectionFunctionTable_atIndex rec_))
    else if queriedSel == sel_setDepthStencilState_atIndex then pure (maybe 0 (const 1) (_setDepthStencilState_atIndex rec_))
    else if queriedSel == sel_device then pure (maybe 0 (const 1) (_device rec_))
    else if queriedSel == sel_label then pure (maybe 0 (const 1) (_label rec_))
    else if queriedSel == sel_setLabel then pure (maybe 0 (const 1) (_setLabel rec_))
    else if queriedSel == sel_encodedLength then pure (maybe 0 (const 1) (_encodedLength rec_))
    else if queriedSel == sel_alignment then pure (maybe 0 (const 1) (_alignment rec_))
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
newMTLArgumentEncoder :: MTLArgumentEncoderOverrides -> IO RawId
newMTLArgumentEncoder overrides = do
  inst <- class_createInstance mtlArgumentEncoderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
