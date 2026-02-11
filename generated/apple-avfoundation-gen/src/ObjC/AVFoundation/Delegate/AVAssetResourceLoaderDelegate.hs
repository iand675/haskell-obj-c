{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVAssetResourceLoaderDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVAssetResourceLoaderDelegate defaultAVAssetResourceLoaderDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFoundation.Delegate.AVAssetResourceLoaderDelegate
  ( AVAssetResourceLoaderDelegateOverrides(..)
  , defaultAVAssetResourceLoaderDelegateOverrides
  , newAVAssetResourceLoaderDelegate
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

-- | Overrides record for @\@protocol AVAssetResourceLoaderDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVAssetResourceLoaderDelegateOverrides = AVAssetResourceLoaderDelegateOverrides
  { _resourceLoader_shouldWaitForLoadingOfRequestedResource :: !(Maybe (RawId -> RawId -> IO Bool))
  , _resourceLoader_shouldWaitForRenewalOfRequestedResource :: !(Maybe (RawId -> RawId -> IO Bool))
  , _resourceLoader_didCancelLoadingRequest :: !(Maybe (RawId -> RawId -> IO ()))
  , _resourceLoader_shouldWaitForResponseToAuthenticationChallenge :: !(Maybe (RawId -> RawId -> IO Bool))
  , _resourceLoader_didCancelAuthenticationChallenge :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVAssetResourceLoaderDelegateOverrides :: AVAssetResourceLoaderDelegateOverrides
defaultAVAssetResourceLoaderDelegateOverrides = AVAssetResourceLoaderDelegateOverrides
  { _resourceLoader_shouldWaitForLoadingOfRequestedResource = Nothing
  , _resourceLoader_shouldWaitForRenewalOfRequestedResource = Nothing
  , _resourceLoader_didCancelLoadingRequest = Nothing
  , _resourceLoader_shouldWaitForResponseToAuthenticationChallenge = Nothing
  , _resourceLoader_didCancelAuthenticationChallenge = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avAssetResourceLoaderDelegateDelegateClass #-}
avAssetResourceLoaderDelegateDelegateClass :: Class
avAssetResourceLoaderDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVAssetResourceLoaderDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_resourceLoader_shouldWaitForLoadingOfRequestedResource = unSelector (mkSelector "resourceLoader:shouldWaitForLoadingOfRequestedResource:")
      sel_resourceLoader_shouldWaitForRenewalOfRequestedResource = unSelector (mkSelector "resourceLoader:shouldWaitForRenewalOfRequestedResource:")
      sel_resourceLoader_didCancelLoadingRequest = unSelector (mkSelector "resourceLoader:didCancelLoadingRequest:")
      sel_resourceLoader_shouldWaitForResponseToAuthenticationChallenge = unSelector (mkSelector "resourceLoader:shouldWaitForResponseToAuthenticationChallenge:")
      sel_resourceLoader_didCancelAuthenticationChallenge = unSelector (mkSelector "resourceLoader:didCancelAuthenticationChallenge:")
  -- resourceLoader:shouldWaitForLoadingOfRequestedResource:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetResourceLoaderDelegateOverrides
    case _resourceLoader_shouldWaitForLoadingOfRequestedResource rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "resourceLoader:shouldWaitForLoadingOfRequestedResource:" "B@:@@" stub_0

  -- resourceLoader:shouldWaitForRenewalOfRequestedResource:
  stub_1 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetResourceLoaderDelegateOverrides
    case _resourceLoader_shouldWaitForRenewalOfRequestedResource rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "resourceLoader:shouldWaitForRenewalOfRequestedResource:" "B@:@@" stub_1

  -- resourceLoader:didCancelLoadingRequest:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetResourceLoaderDelegateOverrides
    case _resourceLoader_didCancelLoadingRequest rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "resourceLoader:didCancelLoadingRequest:" "v@:@@" stub_2

  -- resourceLoader:shouldWaitForResponseToAuthenticationChallenge:
  stub_3 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetResourceLoaderDelegateOverrides
    case _resourceLoader_shouldWaitForResponseToAuthenticationChallenge rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "resourceLoader:shouldWaitForResponseToAuthenticationChallenge:" "B@:@@" stub_3

  -- resourceLoader:didCancelAuthenticationChallenge:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetResourceLoaderDelegateOverrides
    case _resourceLoader_didCancelAuthenticationChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "resourceLoader:didCancelAuthenticationChallenge:" "v@:@@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetResourceLoaderDelegateOverrides
    if queriedSel == sel_resourceLoader_shouldWaitForLoadingOfRequestedResource then pure (maybe 0 (const 1) (_resourceLoader_shouldWaitForLoadingOfRequestedResource rec_))
    else if queriedSel == sel_resourceLoader_shouldWaitForRenewalOfRequestedResource then pure (maybe 0 (const 1) (_resourceLoader_shouldWaitForRenewalOfRequestedResource rec_))
    else if queriedSel == sel_resourceLoader_didCancelLoadingRequest then pure (maybe 0 (const 1) (_resourceLoader_didCancelLoadingRequest rec_))
    else if queriedSel == sel_resourceLoader_shouldWaitForResponseToAuthenticationChallenge then pure (maybe 0 (const 1) (_resourceLoader_shouldWaitForResponseToAuthenticationChallenge rec_))
    else if queriedSel == sel_resourceLoader_didCancelAuthenticationChallenge then pure (maybe 0 (const 1) (_resourceLoader_didCancelAuthenticationChallenge rec_))
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
newAVAssetResourceLoaderDelegate :: AVAssetResourceLoaderDelegateOverrides -> IO RawId
newAVAssetResourceLoaderDelegate overrides = do
  inst <- class_createInstance avAssetResourceLoaderDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
