{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MPSHeapProvider@.
--
-- Usage:
--
-- @
-- delegate <- newMPSHeapProvider defaultMPSHeapProviderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MetalPerformanceShaders.Delegate.MPSHeapProvider
  ( MPSHeapProviderOverrides(..)
  , defaultMPSHeapProviderOverrides
  , newMPSHeapProvider
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

-- | Overrides record for @\@protocol MPSHeapProvider@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MPSHeapProviderOverrides = MPSHeapProviderOverrides
  { _newHeapWithDescriptor :: !(Maybe (RawId -> IO RawId))
  , _retireHeap_cacheDelay :: !(Maybe (RawId -> Double -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMPSHeapProviderOverrides :: MPSHeapProviderOverrides
defaultMPSHeapProviderOverrides = MPSHeapProviderOverrides
  { _newHeapWithDescriptor = Nothing
  , _retireHeap_cacheDelay = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_d_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mpsHeapProviderDelegateClass #-}
mpsHeapProviderDelegateClass :: Class
mpsHeapProviderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMPSHeapProvider" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_newHeapWithDescriptor = unSelector (mkSelector "newHeapWithDescriptor:")
      sel_retireHeap_cacheDelay = unSelector (mkSelector "retireHeap:cacheDelay:")
  -- newHeapWithDescriptor:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSHeapProviderOverrides
    case _newHeapWithDescriptor rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "newHeapWithDescriptor:" "@@:@" stub_0

  -- retireHeap:cacheDelay:
  stub_1 <- wrap_at_d_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSHeapProviderOverrides
    case _retireHeap_cacheDelay rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (realToFrac arg1)
  addObjCMethod cls "retireHeap:cacheDelay:" "v@:@d" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSHeapProviderOverrides
    if queriedSel == sel_newHeapWithDescriptor then pure (maybe 0 (const 1) (_newHeapWithDescriptor rec_))
    else if queriedSel == sel_retireHeap_cacheDelay then pure (maybe 0 (const 1) (_retireHeap_cacheDelay rec_))
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
newMPSHeapProvider :: MPSHeapProviderOverrides -> IO RawId
newMPSHeapProvider overrides = do
  inst <- class_createInstance mpsHeapProviderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
