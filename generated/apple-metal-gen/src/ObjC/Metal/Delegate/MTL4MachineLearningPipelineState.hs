{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTL4MachineLearningPipelineState@.
--
-- Usage:
--
-- @
-- delegate <- newMTL4MachineLearningPipelineState defaultMTL4MachineLearningPipelineStateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTL4MachineLearningPipelineState
  ( MTL4MachineLearningPipelineStateOverrides(..)
  , defaultMTL4MachineLearningPipelineStateOverrides
  , newMTL4MachineLearningPipelineState
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

-- | Overrides record for @\@protocol MTL4MachineLearningPipelineState@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTL4MachineLearningPipelineStateOverrides = MTL4MachineLearningPipelineStateOverrides
  { _label :: !(Maybe (IO RawId))
  , _device :: !(Maybe (IO RawId))
  , _reflection :: !(Maybe (IO RawId))
  , _intermediatesHeapSize :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultMTL4MachineLearningPipelineStateOverrides :: MTL4MachineLearningPipelineStateOverrides
defaultMTL4MachineLearningPipelineStateOverrides = MTL4MachineLearningPipelineStateOverrides
  { _label = Nothing
  , _device = Nothing
  , _reflection = Nothing
  , _intermediatesHeapSize = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtL4MachineLearningPipelineStateDelegateClass #-}
mtL4MachineLearningPipelineStateDelegateClass :: Class
mtL4MachineLearningPipelineStateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTL4MachineLearningPipelineState" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_label = unSelector (mkSelector "label")
      sel_device = unSelector (mkSelector "device")
      sel_reflection = unSelector (mkSelector "reflection")
      sel_intermediatesHeapSize = unSelector (mkSelector "intermediatesHeapSize")
  -- label
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4MachineLearningPipelineStateOverrides
    case _label rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "label" "@@:" stub_0

  -- device
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4MachineLearningPipelineStateOverrides
    case _device rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "device" "@@:" stub_1

  -- reflection
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4MachineLearningPipelineStateOverrides
    case _reflection rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "reflection" "@@:" stub_2

  -- intermediatesHeapSize
  stub_3 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4MachineLearningPipelineStateOverrides
    case _intermediatesHeapSize rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "intermediatesHeapSize" "Q@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4MachineLearningPipelineStateOverrides
    if queriedSel == sel_label then pure (maybe 0 (const 1) (_label rec_))
    else if queriedSel == sel_device then pure (maybe 0 (const 1) (_device rec_))
    else if queriedSel == sel_reflection then pure (maybe 0 (const 1) (_reflection rec_))
    else if queriedSel == sel_intermediatesHeapSize then pure (maybe 0 (const 1) (_intermediatesHeapSize rec_))
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
newMTL4MachineLearningPipelineState :: MTL4MachineLearningPipelineStateOverrides -> IO RawId
newMTL4MachineLearningPipelineState overrides = do
  inst <- class_createInstance mtL4MachineLearningPipelineStateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
