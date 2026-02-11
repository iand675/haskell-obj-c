{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTL4CommandBuffer@.
--
-- Usage:
--
-- @
-- delegate <- newMTL4CommandBuffer defaultMTL4CommandBufferOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTL4CommandBuffer
  ( MTL4CommandBufferOverrides(..)
  , defaultMTL4CommandBufferOverrides
  , newMTL4CommandBuffer
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

-- | Overrides record for @\@protocol MTL4CommandBuffer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTL4CommandBufferOverrides = MTL4CommandBufferOverrides
  { _beginCommandBufferWithAllocator :: !(Maybe (RawId -> IO ()))
  , _beginCommandBufferWithAllocator_options :: !(Maybe (RawId -> RawId -> IO ()))
  , _endCommandBuffer :: !(Maybe (IO ()))
  , _renderCommandEncoderWithDescriptor :: !(Maybe (RawId -> IO RawId))
  , _computeCommandEncoder :: !(Maybe (IO RawId))
  , _machineLearningCommandEncoder :: !(Maybe (IO RawId))
  , _useResidencySet :: !(Maybe (RawId -> IO ()))
  , _useResidencySets_count :: !(Maybe (RawId -> Int -> IO ()))
  , _pushDebugGroup :: !(Maybe (RawId -> IO ()))
  , _popDebugGroup :: !(Maybe (IO ()))
  , _writeTimestampIntoHeap_atIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _device :: !(Maybe (IO RawId))
  , _label :: !(Maybe (IO RawId))
  , _setLabel :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTL4CommandBufferOverrides :: MTL4CommandBufferOverrides
defaultMTL4CommandBufferOverrides = MTL4CommandBufferOverrides
  { _beginCommandBufferWithAllocator = Nothing
  , _beginCommandBufferWithAllocator_options = Nothing
  , _endCommandBuffer = Nothing
  , _renderCommandEncoderWithDescriptor = Nothing
  , _computeCommandEncoder = Nothing
  , _machineLearningCommandEncoder = Nothing
  , _useResidencySet = Nothing
  , _useResidencySets_count = Nothing
  , _pushDebugGroup = Nothing
  , _popDebugGroup = Nothing
  , _writeTimestampIntoHeap_atIndex = Nothing
  , _device = Nothing
  , _label = Nothing
  , _setLabel = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtL4CommandBufferDelegateClass #-}
mtL4CommandBufferDelegateClass :: Class
mtL4CommandBufferDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTL4CommandBuffer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_beginCommandBufferWithAllocator = unSelector (mkSelector "beginCommandBufferWithAllocator:")
      sel_beginCommandBufferWithAllocator_options = unSelector (mkSelector "beginCommandBufferWithAllocator:options:")
      sel_endCommandBuffer = unSelector (mkSelector "endCommandBuffer")
      sel_renderCommandEncoderWithDescriptor = unSelector (mkSelector "renderCommandEncoderWithDescriptor:")
      sel_computeCommandEncoder = unSelector (mkSelector "computeCommandEncoder")
      sel_machineLearningCommandEncoder = unSelector (mkSelector "machineLearningCommandEncoder")
      sel_useResidencySet = unSelector (mkSelector "useResidencySet:")
      sel_useResidencySets_count = unSelector (mkSelector "useResidencySets:count:")
      sel_pushDebugGroup = unSelector (mkSelector "pushDebugGroup:")
      sel_popDebugGroup = unSelector (mkSelector "popDebugGroup")
      sel_writeTimestampIntoHeap_atIndex = unSelector (mkSelector "writeTimestampIntoHeap:atIndex:")
      sel_device = unSelector (mkSelector "device")
      sel_label = unSelector (mkSelector "label")
      sel_setLabel = unSelector (mkSelector "setLabel:")
  -- beginCommandBufferWithAllocator:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _beginCommandBufferWithAllocator rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "beginCommandBufferWithAllocator:" "v@:@" stub_0

  -- beginCommandBufferWithAllocator:options:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _beginCommandBufferWithAllocator_options rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "beginCommandBufferWithAllocator:options:" "v@:@@" stub_1

  -- endCommandBuffer
  stub_2 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _endCommandBuffer rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "endCommandBuffer" "v@:" stub_2

  -- renderCommandEncoderWithDescriptor:
  stub_3 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _renderCommandEncoderWithDescriptor rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "renderCommandEncoderWithDescriptor:" "@@:@" stub_3

  -- computeCommandEncoder
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _computeCommandEncoder rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "computeCommandEncoder" "@@:" stub_4

  -- machineLearningCommandEncoder
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _machineLearningCommandEncoder rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "machineLearningCommandEncoder" "@@:" stub_5

  -- useResidencySet:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _useResidencySet rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "useResidencySet:" "v@:@" stub_6

  -- useResidencySets:count:
  stub_7 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _useResidencySets_count rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "useResidencySets:count:" "v@:@Q" stub_7

  -- pushDebugGroup:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _pushDebugGroup rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pushDebugGroup:" "v@:@" stub_8

  -- popDebugGroup
  stub_9 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _popDebugGroup rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "popDebugGroup" "v@:" stub_9

  -- writeTimestampIntoHeap:atIndex:
  stub_10 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _writeTimestampIntoHeap_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "writeTimestampIntoHeap:atIndex:" "v@:@Q" stub_10

  -- device
  stub_11 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _device rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "device" "@@:" stub_11

  -- label
  stub_12 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _label rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "label" "@@:" stub_12

  -- setLabel:
  stub_13 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    case _setLabel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setLabel:" "v@:@" stub_13

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandBufferOverrides
    if queriedSel == sel_beginCommandBufferWithAllocator then pure (maybe 0 (const 1) (_beginCommandBufferWithAllocator rec_))
    else if queriedSel == sel_beginCommandBufferWithAllocator_options then pure (maybe 0 (const 1) (_beginCommandBufferWithAllocator_options rec_))
    else if queriedSel == sel_endCommandBuffer then pure (maybe 0 (const 1) (_endCommandBuffer rec_))
    else if queriedSel == sel_renderCommandEncoderWithDescriptor then pure (maybe 0 (const 1) (_renderCommandEncoderWithDescriptor rec_))
    else if queriedSel == sel_computeCommandEncoder then pure (maybe 0 (const 1) (_computeCommandEncoder rec_))
    else if queriedSel == sel_machineLearningCommandEncoder then pure (maybe 0 (const 1) (_machineLearningCommandEncoder rec_))
    else if queriedSel == sel_useResidencySet then pure (maybe 0 (const 1) (_useResidencySet rec_))
    else if queriedSel == sel_useResidencySets_count then pure (maybe 0 (const 1) (_useResidencySets_count rec_))
    else if queriedSel == sel_pushDebugGroup then pure (maybe 0 (const 1) (_pushDebugGroup rec_))
    else if queriedSel == sel_popDebugGroup then pure (maybe 0 (const 1) (_popDebugGroup rec_))
    else if queriedSel == sel_writeTimestampIntoHeap_atIndex then pure (maybe 0 (const 1) (_writeTimestampIntoHeap_atIndex rec_))
    else if queriedSel == sel_device then pure (maybe 0 (const 1) (_device rec_))
    else if queriedSel == sel_label then pure (maybe 0 (const 1) (_label rec_))
    else if queriedSel == sel_setLabel then pure (maybe 0 (const 1) (_setLabel rec_))
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
newMTL4CommandBuffer :: MTL4CommandBufferOverrides -> IO RawId
newMTL4CommandBuffer overrides = do
  inst <- class_createInstance mtL4CommandBufferDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
