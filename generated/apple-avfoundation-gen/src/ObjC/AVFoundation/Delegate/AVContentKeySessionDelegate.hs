{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVContentKeySessionDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVContentKeySessionDelegate defaultAVContentKeySessionDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFoundation.Delegate.AVContentKeySessionDelegate
  ( AVContentKeySessionDelegateOverrides(..)
  , defaultAVContentKeySessionDelegateOverrides
  , newAVContentKeySessionDelegate
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

-- | Overrides record for @\@protocol AVContentKeySessionDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVContentKeySessionDelegateOverrides = AVContentKeySessionDelegateOverrides
  { _contentKeySession_didProvideContentKeyRequest :: !(Maybe (RawId -> RawId -> IO ()))
  , _contentKeySession_didProvideRenewingContentKeyRequest :: !(Maybe (RawId -> RawId -> IO ()))
  , _contentKeySession_didProvidePersistableContentKeyRequest :: !(Maybe (RawId -> RawId -> IO ()))
  , _contentKeySession_didUpdatePersistableContentKey_forContentKeyIdentifier :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _contentKeySession_contentKeyRequest_didFailWithError :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _contentKeySession_shouldRetryContentKeyRequest_reason :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _contentKeySession_contentKeyRequestDidSucceed :: !(Maybe (RawId -> RawId -> IO ()))
  , _contentKeySessionContentProtectionSessionIdentifierDidChange :: !(Maybe (RawId -> IO ()))
  , _contentKeySessionDidGenerateExpiredSessionReport :: !(Maybe (RawId -> IO ()))
  , _contentKeySession_externalProtectionStatusDidChangeForContentKey :: !(Maybe (RawId -> RawId -> IO ()))
  , _contentKeySession_didProvideContentKeyRequests_forInitializationData :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVContentKeySessionDelegateOverrides :: AVContentKeySessionDelegateOverrides
defaultAVContentKeySessionDelegateOverrides = AVContentKeySessionDelegateOverrides
  { _contentKeySession_didProvideContentKeyRequest = Nothing
  , _contentKeySession_didProvideRenewingContentKeyRequest = Nothing
  , _contentKeySession_didProvidePersistableContentKeyRequest = Nothing
  , _contentKeySession_didUpdatePersistableContentKey_forContentKeyIdentifier = Nothing
  , _contentKeySession_contentKeyRequest_didFailWithError = Nothing
  , _contentKeySession_shouldRetryContentKeyRequest_reason = Nothing
  , _contentKeySession_contentKeyRequestDidSucceed = Nothing
  , _contentKeySessionContentProtectionSessionIdentifierDidChange = Nothing
  , _contentKeySessionDidGenerateExpiredSessionReport = Nothing
  , _contentKeySession_externalProtectionStatusDidChangeForContentKey = Nothing
  , _contentKeySession_didProvideContentKeyRequests_forInitializationData = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avContentKeySessionDelegateDelegateClass #-}
avContentKeySessionDelegateDelegateClass :: Class
avContentKeySessionDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVContentKeySessionDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_contentKeySession_didProvideContentKeyRequest = unSelector (mkSelector "contentKeySession:didProvideContentKeyRequest:")
      sel_contentKeySession_didProvideRenewingContentKeyRequest = unSelector (mkSelector "contentKeySession:didProvideRenewingContentKeyRequest:")
      sel_contentKeySession_didProvidePersistableContentKeyRequest = unSelector (mkSelector "contentKeySession:didProvidePersistableContentKeyRequest:")
      sel_contentKeySession_didUpdatePersistableContentKey_forContentKeyIdentifier = unSelector (mkSelector "contentKeySession:didUpdatePersistableContentKey:forContentKeyIdentifier:")
      sel_contentKeySession_contentKeyRequest_didFailWithError = unSelector (mkSelector "contentKeySession:contentKeyRequest:didFailWithError:")
      sel_contentKeySession_shouldRetryContentKeyRequest_reason = unSelector (mkSelector "contentKeySession:shouldRetryContentKeyRequest:reason:")
      sel_contentKeySession_contentKeyRequestDidSucceed = unSelector (mkSelector "contentKeySession:contentKeyRequestDidSucceed:")
      sel_contentKeySessionContentProtectionSessionIdentifierDidChange = unSelector (mkSelector "contentKeySessionContentProtectionSessionIdentifierDidChange:")
      sel_contentKeySessionDidGenerateExpiredSessionReport = unSelector (mkSelector "contentKeySessionDidGenerateExpiredSessionReport:")
      sel_contentKeySession_externalProtectionStatusDidChangeForContentKey = unSelector (mkSelector "contentKeySession:externalProtectionStatusDidChangeForContentKey:")
      sel_contentKeySession_didProvideContentKeyRequests_forInitializationData = unSelector (mkSelector "contentKeySession:didProvideContentKeyRequests:forInitializationData:")
  -- contentKeySession:didProvideContentKeyRequest:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeySessionDelegateOverrides
    case _contentKeySession_didProvideContentKeyRequest rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "contentKeySession:didProvideContentKeyRequest:" "v@:@@" stub_0

  -- contentKeySession:didProvideRenewingContentKeyRequest:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeySessionDelegateOverrides
    case _contentKeySession_didProvideRenewingContentKeyRequest rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "contentKeySession:didProvideRenewingContentKeyRequest:" "v@:@@" stub_1

  -- contentKeySession:didProvidePersistableContentKeyRequest:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeySessionDelegateOverrides
    case _contentKeySession_didProvidePersistableContentKeyRequest rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "contentKeySession:didProvidePersistableContentKeyRequest:" "v@:@@" stub_2

  -- contentKeySession:didUpdatePersistableContentKey:forContentKeyIdentifier:
  stub_3 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeySessionDelegateOverrides
    case _contentKeySession_didUpdatePersistableContentKey_forContentKeyIdentifier rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "contentKeySession:didUpdatePersistableContentKey:forContentKeyIdentifier:" "v@:@@@" stub_3

  -- contentKeySession:contentKeyRequest:didFailWithError:
  stub_4 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeySessionDelegateOverrides
    case _contentKeySession_contentKeyRequest_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "contentKeySession:contentKeyRequest:didFailWithError:" "v@:@@@" stub_4

  -- contentKeySession:shouldRetryContentKeyRequest:reason:
  stub_5 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeySessionDelegateOverrides
    case _contentKeySession_shouldRetryContentKeyRequest_reason rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "contentKeySession:shouldRetryContentKeyRequest:reason:" "B@:@@@" stub_5

  -- contentKeySession:contentKeyRequestDidSucceed:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeySessionDelegateOverrides
    case _contentKeySession_contentKeyRequestDidSucceed rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "contentKeySession:contentKeyRequestDidSucceed:" "v@:@@" stub_6

  -- contentKeySessionContentProtectionSessionIdentifierDidChange:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeySessionDelegateOverrides
    case _contentKeySessionContentProtectionSessionIdentifierDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "contentKeySessionContentProtectionSessionIdentifierDidChange:" "v@:@" stub_7

  -- contentKeySessionDidGenerateExpiredSessionReport:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeySessionDelegateOverrides
    case _contentKeySessionDidGenerateExpiredSessionReport rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "contentKeySessionDidGenerateExpiredSessionReport:" "v@:@" stub_8

  -- contentKeySession:externalProtectionStatusDidChangeForContentKey:
  stub_9 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeySessionDelegateOverrides
    case _contentKeySession_externalProtectionStatusDidChangeForContentKey rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "contentKeySession:externalProtectionStatusDidChangeForContentKey:" "v@:@@" stub_9

  -- contentKeySession:didProvideContentKeyRequests:forInitializationData:
  stub_10 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeySessionDelegateOverrides
    case _contentKeySession_didProvideContentKeyRequests_forInitializationData rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "contentKeySession:didProvideContentKeyRequests:forInitializationData:" "v@:@@@" stub_10

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeySessionDelegateOverrides
    if queriedSel == sel_contentKeySession_didProvideContentKeyRequest then pure (maybe 0 (const 1) (_contentKeySession_didProvideContentKeyRequest rec_))
    else if queriedSel == sel_contentKeySession_didProvideRenewingContentKeyRequest then pure (maybe 0 (const 1) (_contentKeySession_didProvideRenewingContentKeyRequest rec_))
    else if queriedSel == sel_contentKeySession_didProvidePersistableContentKeyRequest then pure (maybe 0 (const 1) (_contentKeySession_didProvidePersistableContentKeyRequest rec_))
    else if queriedSel == sel_contentKeySession_didUpdatePersistableContentKey_forContentKeyIdentifier then pure (maybe 0 (const 1) (_contentKeySession_didUpdatePersistableContentKey_forContentKeyIdentifier rec_))
    else if queriedSel == sel_contentKeySession_contentKeyRequest_didFailWithError then pure (maybe 0 (const 1) (_contentKeySession_contentKeyRequest_didFailWithError rec_))
    else if queriedSel == sel_contentKeySession_shouldRetryContentKeyRequest_reason then pure (maybe 0 (const 1) (_contentKeySession_shouldRetryContentKeyRequest_reason rec_))
    else if queriedSel == sel_contentKeySession_contentKeyRequestDidSucceed then pure (maybe 0 (const 1) (_contentKeySession_contentKeyRequestDidSucceed rec_))
    else if queriedSel == sel_contentKeySessionContentProtectionSessionIdentifierDidChange then pure (maybe 0 (const 1) (_contentKeySessionContentProtectionSessionIdentifierDidChange rec_))
    else if queriedSel == sel_contentKeySessionDidGenerateExpiredSessionReport then pure (maybe 0 (const 1) (_contentKeySessionDidGenerateExpiredSessionReport rec_))
    else if queriedSel == sel_contentKeySession_externalProtectionStatusDidChangeForContentKey then pure (maybe 0 (const 1) (_contentKeySession_externalProtectionStatusDidChangeForContentKey rec_))
    else if queriedSel == sel_contentKeySession_didProvideContentKeyRequests_forInitializationData then pure (maybe 0 (const 1) (_contentKeySession_didProvideContentKeyRequests_forInitializationData rec_))
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
newAVContentKeySessionDelegate :: AVContentKeySessionDelegateOverrides -> IO RawId
newAVContentKeySessionDelegate overrides = do
  inst <- class_createInstance avContentKeySessionDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
