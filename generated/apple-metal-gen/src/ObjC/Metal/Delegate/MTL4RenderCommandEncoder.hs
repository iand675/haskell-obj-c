{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTL4RenderCommandEncoder@.
--
-- Usage:
--
-- @
-- delegate <- newMTL4RenderCommandEncoder defaultMTL4RenderCommandEncoderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTL4RenderCommandEncoder
  ( MTL4RenderCommandEncoderOverrides(..)
  , defaultMTL4RenderCommandEncoderOverrides
  , newMTL4RenderCommandEncoder
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

-- | Overrides record for @\@protocol MTL4RenderCommandEncoder@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTL4RenderCommandEncoderOverrides = MTL4RenderCommandEncoderOverrides
  { _setColorAttachmentMap :: !(Maybe (RawId -> IO ()))
  , _setRenderPipelineState :: !(Maybe (RawId -> IO ()))
  , _setViewports_count :: !(Maybe (RawId -> Int -> IO ()))
  , _setVertexAmplificationCount_viewMappings :: !(Maybe (Int -> RawId -> IO ()))
  , _setDepthBias_slopeScale_clamp :: !(Maybe (Float -> Float -> Float -> IO ()))
  , _setDepthTestMinBound_maxBound :: !(Maybe (Float -> Float -> IO ()))
  , _setScissorRects_count :: !(Maybe (RawId -> Int -> IO ()))
  , _setBlendColorRed_green_blue_alpha :: !(Maybe (Float -> Float -> Float -> Float -> IO ()))
  , _setDepthStencilState :: !(Maybe (RawId -> IO ()))
  , _setStencilReferenceValue :: !(Maybe (Int -> IO ()))
  , _setStencilFrontReferenceValue_backReferenceValue :: !(Maybe (Int -> Int -> IO ()))
  , _executeCommandsInBuffer_indirectBuffer :: !(Maybe (RawId -> Int -> IO ()))
  , _setObjectThreadgroupMemoryLength_atIndex :: !(Maybe (Int -> Int -> IO ()))
  , _setThreadgroupMemoryLength_offset_atIndex :: !(Maybe (Int -> Int -> Int -> IO ()))
  , _tileWidth :: !(Maybe (IO Int))
  , _tileHeight :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultMTL4RenderCommandEncoderOverrides :: MTL4RenderCommandEncoderOverrides
defaultMTL4RenderCommandEncoderOverrides = MTL4RenderCommandEncoderOverrides
  { _setColorAttachmentMap = Nothing
  , _setRenderPipelineState = Nothing
  , _setViewports_count = Nothing
  , _setVertexAmplificationCount_viewMappings = Nothing
  , _setDepthBias_slopeScale_clamp = Nothing
  , _setDepthTestMinBound_maxBound = Nothing
  , _setScissorRects_count = Nothing
  , _setBlendColorRed_green_blue_alpha = Nothing
  , _setDepthStencilState = Nothing
  , _setStencilReferenceValue = Nothing
  , _setStencilFrontReferenceValue_backReferenceValue = Nothing
  , _executeCommandsInBuffer_indirectBuffer = Nothing
  , _setObjectThreadgroupMemoryLength_atIndex = Nothing
  , _setThreadgroupMemoryLength_offset_atIndex = Nothing
  , _tileWidth = Nothing
  , _tileHeight = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_Q_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_I_I_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CUInt -> CUInt -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CUInt -> CUInt -> IO ()))

foreign import ccall "wrapper"
  wrap_I_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CUInt -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CUInt -> IO ()))

foreign import ccall "wrapper"
  wrap_f_f_f_f_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> CFloat -> CFloat -> CFloat -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()))

foreign import ccall "wrapper"
  wrap_f_f_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> CFloat -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> CFloat -> IO ()))

foreign import ccall "wrapper"
  wrap_f_f_f_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> CFloat -> CFloat -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> CFloat -> CFloat -> IO ()))

foreign import ccall "wrapper"
  wrap_Q_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtL4RenderCommandEncoderDelegateClass #-}
mtL4RenderCommandEncoderDelegateClass :: Class
mtL4RenderCommandEncoderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTL4RenderCommandEncoder" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_setColorAttachmentMap = unSelector (mkSelector "setColorAttachmentMap:")
      sel_setRenderPipelineState = unSelector (mkSelector "setRenderPipelineState:")
      sel_setViewports_count = unSelector (mkSelector "setViewports:count:")
      sel_setVertexAmplificationCount_viewMappings = unSelector (mkSelector "setVertexAmplificationCount:viewMappings:")
      sel_setDepthBias_slopeScale_clamp = unSelector (mkSelector "setDepthBias:slopeScale:clamp:")
      sel_setDepthTestMinBound_maxBound = unSelector (mkSelector "setDepthTestMinBound:maxBound:")
      sel_setScissorRects_count = unSelector (mkSelector "setScissorRects:count:")
      sel_setBlendColorRed_green_blue_alpha = unSelector (mkSelector "setBlendColorRed:green:blue:alpha:")
      sel_setDepthStencilState = unSelector (mkSelector "setDepthStencilState:")
      sel_setStencilReferenceValue = unSelector (mkSelector "setStencilReferenceValue:")
      sel_setStencilFrontReferenceValue_backReferenceValue = unSelector (mkSelector "setStencilFrontReferenceValue:backReferenceValue:")
      sel_executeCommandsInBuffer_indirectBuffer = unSelector (mkSelector "executeCommandsInBuffer:indirectBuffer:")
      sel_setObjectThreadgroupMemoryLength_atIndex = unSelector (mkSelector "setObjectThreadgroupMemoryLength:atIndex:")
      sel_setThreadgroupMemoryLength_offset_atIndex = unSelector (mkSelector "setThreadgroupMemoryLength:offset:atIndex:")
      sel_tileWidth = unSelector (mkSelector "tileWidth")
      sel_tileHeight = unSelector (mkSelector "tileHeight")
  -- setColorAttachmentMap:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _setColorAttachmentMap rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setColorAttachmentMap:" "v@:@" stub_0

  -- setRenderPipelineState:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _setRenderPipelineState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setRenderPipelineState:" "v@:@" stub_1

  -- setViewports:count:
  stub_2 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _setViewports_count rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setViewports:count:" "v@:@Q" stub_2

  -- setVertexAmplificationCount:viewMappings:
  stub_3 <- wrap_Q_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _setVertexAmplificationCount_viewMappings rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (RawId arg1)
  addObjCMethod cls "setVertexAmplificationCount:viewMappings:" "v@:Q@" stub_3

  -- setDepthBias:slopeScale:clamp:
  stub_4 <- wrap_f_f_f_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _setDepthBias_slopeScale_clamp rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0) (realToFrac arg1) (realToFrac arg2)
  addObjCMethod cls "setDepthBias:slopeScale:clamp:" "v@:fff" stub_4

  -- setDepthTestMinBound:maxBound:
  stub_5 <- wrap_f_f_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _setDepthTestMinBound_maxBound rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0) (realToFrac arg1)
  addObjCMethod cls "setDepthTestMinBound:maxBound:" "v@:ff" stub_5

  -- setScissorRects:count:
  stub_6 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _setScissorRects_count rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setScissorRects:count:" "v@:@Q" stub_6

  -- setBlendColorRed:green:blue:alpha:
  stub_7 <- wrap_f_f_f_f_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _setBlendColorRed_green_blue_alpha rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0) (realToFrac arg1) (realToFrac arg2) (realToFrac arg3)
  addObjCMethod cls "setBlendColorRed:green:blue:alpha:" "v@:ffff" stub_7

  -- setDepthStencilState:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _setDepthStencilState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setDepthStencilState:" "v@:@" stub_8

  -- setStencilReferenceValue:
  stub_9 <- wrap_I_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _setStencilReferenceValue rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setStencilReferenceValue:" "v@:I" stub_9

  -- setStencilFrontReferenceValue:backReferenceValue:
  stub_10 <- wrap_I_I_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _setStencilFrontReferenceValue_backReferenceValue rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1)
  addObjCMethod cls "setStencilFrontReferenceValue:backReferenceValue:" "v@:II" stub_10

  -- executeCommandsInBuffer:indirectBuffer:
  stub_11 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _executeCommandsInBuffer_indirectBuffer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "executeCommandsInBuffer:indirectBuffer:" "v@:@Q" stub_11

  -- setObjectThreadgroupMemoryLength:atIndex:
  stub_12 <- wrap_Q_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _setObjectThreadgroupMemoryLength_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1)
  addObjCMethod cls "setObjectThreadgroupMemoryLength:atIndex:" "v@:QQ" stub_12

  -- setThreadgroupMemoryLength:offset:atIndex:
  stub_13 <- wrap_Q_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _setThreadgroupMemoryLength_offset_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "setThreadgroupMemoryLength:offset:atIndex:" "v@:QQQ" stub_13

  -- tileWidth
  stub_14 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _tileWidth rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "tileWidth" "Q@:" stub_14

  -- tileHeight
  stub_15 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    case _tileHeight rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "tileHeight" "Q@:" stub_15

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4RenderCommandEncoderOverrides
    if queriedSel == sel_setColorAttachmentMap then pure (maybe 0 (const 1) (_setColorAttachmentMap rec_))
    else if queriedSel == sel_setRenderPipelineState then pure (maybe 0 (const 1) (_setRenderPipelineState rec_))
    else if queriedSel == sel_setViewports_count then pure (maybe 0 (const 1) (_setViewports_count rec_))
    else if queriedSel == sel_setVertexAmplificationCount_viewMappings then pure (maybe 0 (const 1) (_setVertexAmplificationCount_viewMappings rec_))
    else if queriedSel == sel_setDepthBias_slopeScale_clamp then pure (maybe 0 (const 1) (_setDepthBias_slopeScale_clamp rec_))
    else if queriedSel == sel_setDepthTestMinBound_maxBound then pure (maybe 0 (const 1) (_setDepthTestMinBound_maxBound rec_))
    else if queriedSel == sel_setScissorRects_count then pure (maybe 0 (const 1) (_setScissorRects_count rec_))
    else if queriedSel == sel_setBlendColorRed_green_blue_alpha then pure (maybe 0 (const 1) (_setBlendColorRed_green_blue_alpha rec_))
    else if queriedSel == sel_setDepthStencilState then pure (maybe 0 (const 1) (_setDepthStencilState rec_))
    else if queriedSel == sel_setStencilReferenceValue then pure (maybe 0 (const 1) (_setStencilReferenceValue rec_))
    else if queriedSel == sel_setStencilFrontReferenceValue_backReferenceValue then pure (maybe 0 (const 1) (_setStencilFrontReferenceValue_backReferenceValue rec_))
    else if queriedSel == sel_executeCommandsInBuffer_indirectBuffer then pure (maybe 0 (const 1) (_executeCommandsInBuffer_indirectBuffer rec_))
    else if queriedSel == sel_setObjectThreadgroupMemoryLength_atIndex then pure (maybe 0 (const 1) (_setObjectThreadgroupMemoryLength_atIndex rec_))
    else if queriedSel == sel_setThreadgroupMemoryLength_offset_atIndex then pure (maybe 0 (const 1) (_setThreadgroupMemoryLength_offset_atIndex rec_))
    else if queriedSel == sel_tileWidth then pure (maybe 0 (const 1) (_tileWidth rec_))
    else if queriedSel == sel_tileHeight then pure (maybe 0 (const 1) (_tileHeight rec_))
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
newMTL4RenderCommandEncoder :: MTL4RenderCommandEncoderOverrides -> IO RawId
newMTL4RenderCommandEncoder overrides = do
  inst <- class_createInstance mtL4RenderCommandEncoderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
