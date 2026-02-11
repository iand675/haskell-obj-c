{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MPSNNPadding@.
--
-- Usage:
--
-- @
-- delegate <- newMPSNNPadding defaultMPSNNPaddingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MetalPerformanceShaders.Delegate.MPSNNPadding
  ( MPSNNPaddingOverrides(..)
  , defaultMPSNNPaddingOverrides
  , newMPSNNPadding
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

-- | Overrides record for @\@protocol MPSNNPadding@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MPSNNPaddingOverrides = MPSNNPaddingOverrides
  { _label :: !(Maybe (IO RawId))
  , _destinationImageDescriptorForSourceImages_sourceStates_forKernel_suggestedDescriptor :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO RawId))
  , _inverse :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMPSNNPaddingOverrides :: MPSNNPaddingOverrides
defaultMPSNNPaddingOverrides = MPSNNPaddingOverrides
  { _label = Nothing
  , _destinationImageDescriptorForSourceImages_sourceStates_forKernel_suggestedDescriptor = Nothing
  , _inverse = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mpsnnPaddingDelegateClass #-}
mpsnnPaddingDelegateClass :: Class
mpsnnPaddingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMPSNNPadding" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_label = unSelector (mkSelector "label")
      sel_destinationImageDescriptorForSourceImages_sourceStates_forKernel_suggestedDescriptor = unSelector (mkSelector "destinationImageDescriptorForSourceImages:sourceStates:forKernel:suggestedDescriptor:")
      sel_inverse = unSelector (mkSelector "inverse")
  -- label
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSNNPaddingOverrides
    case _label rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "label" "@@:" stub_0

  -- destinationImageDescriptorForSourceImages:sourceStates:forKernel:suggestedDescriptor:
  stub_1 <- wrap_at_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSNNPaddingOverrides
    case _destinationImageDescriptorForSourceImages_sourceStates_forKernel_suggestedDescriptor rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "destinationImageDescriptorForSourceImages:sourceStates:forKernel:suggestedDescriptor:" "@@:@@@@" stub_1

  -- inverse
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSNNPaddingOverrides
    case _inverse rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "inverse" "@@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSNNPaddingOverrides
    if queriedSel == sel_label then pure (maybe 0 (const 1) (_label rec_))
    else if queriedSel == sel_destinationImageDescriptorForSourceImages_sourceStates_forKernel_suggestedDescriptor then pure (maybe 0 (const 1) (_destinationImageDescriptorForSourceImages_sourceStates_forKernel_suggestedDescriptor rec_))
    else if queriedSel == sel_inverse then pure (maybe 0 (const 1) (_inverse rec_))
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
newMPSNNPadding :: MPSNNPaddingOverrides -> IO RawId
newMPSNNPadding overrides = do
  inst <- class_createInstance mpsnnPaddingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
