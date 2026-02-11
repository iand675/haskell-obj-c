{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSURLDownloadDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSURLDownloadDelegate defaultNSURLDownloadDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSURLDownloadDelegate
  ( NSURLDownloadDelegateOverrides(..)
  , defaultNSURLDownloadDelegateOverrides
  , newNSURLDownloadDelegate
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

-- | Overrides record for @\@protocol NSURLDownloadDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSURLDownloadDelegateOverrides = NSURLDownloadDelegateOverrides
  { _downloadDidBegin :: !(Maybe (RawId -> IO ()))
  , _download_willSendRequest_redirectResponse :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _download_canAuthenticateAgainstProtectionSpace :: !(Maybe (RawId -> RawId -> IO Bool))
  , _download_didReceiveAuthenticationChallenge :: !(Maybe (RawId -> RawId -> IO ()))
  , _download_didCancelAuthenticationChallenge :: !(Maybe (RawId -> RawId -> IO ()))
  , _downloadShouldUseCredentialStorage :: !(Maybe (RawId -> IO Bool))
  , _download_didReceiveResponse :: !(Maybe (RawId -> RawId -> IO ()))
  , _download_willResumeWithResponse_fromByte :: !(Maybe (RawId -> RawId -> Int -> IO ()))
  , _download_didReceiveDataOfLength :: !(Maybe (RawId -> Int -> IO ()))
  , _download_shouldDecodeSourceDataOfMIMEType :: !(Maybe (RawId -> RawId -> IO Bool))
  , _download_decideDestinationWithSuggestedFilename :: !(Maybe (RawId -> RawId -> IO ()))
  , _download_didCreateDestination :: !(Maybe (RawId -> RawId -> IO ()))
  , _downloadDidFinish :: !(Maybe (RawId -> IO ()))
  , _download_didFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSURLDownloadDelegateOverrides :: NSURLDownloadDelegateOverrides
defaultNSURLDownloadDelegateOverrides = NSURLDownloadDelegateOverrides
  { _downloadDidBegin = Nothing
  , _download_willSendRequest_redirectResponse = Nothing
  , _download_canAuthenticateAgainstProtectionSpace = Nothing
  , _download_didReceiveAuthenticationChallenge = Nothing
  , _download_didCancelAuthenticationChallenge = Nothing
  , _downloadShouldUseCredentialStorage = Nothing
  , _download_didReceiveResponse = Nothing
  , _download_willResumeWithResponse_fromByte = Nothing
  , _download_didReceiveDataOfLength = Nothing
  , _download_shouldDecodeSourceDataOfMIMEType = Nothing
  , _download_decideDestinationWithSuggestedFilename = Nothing
  , _download_didCreateDestination = Nothing
  , _downloadDidFinish = Nothing
  , _download_didFailWithError = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsurlDownloadDelegateDelegateClass #-}
nsurlDownloadDelegateDelegateClass :: Class
nsurlDownloadDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSURLDownloadDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_downloadDidBegin = unSelector (mkSelector "downloadDidBegin:")
      sel_download_willSendRequest_redirectResponse = unSelector (mkSelector "download:willSendRequest:redirectResponse:")
      sel_download_canAuthenticateAgainstProtectionSpace = unSelector (mkSelector "download:canAuthenticateAgainstProtectionSpace:")
      sel_download_didReceiveAuthenticationChallenge = unSelector (mkSelector "download:didReceiveAuthenticationChallenge:")
      sel_download_didCancelAuthenticationChallenge = unSelector (mkSelector "download:didCancelAuthenticationChallenge:")
      sel_downloadShouldUseCredentialStorage = unSelector (mkSelector "downloadShouldUseCredentialStorage:")
      sel_download_didReceiveResponse = unSelector (mkSelector "download:didReceiveResponse:")
      sel_download_willResumeWithResponse_fromByte = unSelector (mkSelector "download:willResumeWithResponse:fromByte:")
      sel_download_didReceiveDataOfLength = unSelector (mkSelector "download:didReceiveDataOfLength:")
      sel_download_shouldDecodeSourceDataOfMIMEType = unSelector (mkSelector "download:shouldDecodeSourceDataOfMIMEType:")
      sel_download_decideDestinationWithSuggestedFilename = unSelector (mkSelector "download:decideDestinationWithSuggestedFilename:")
      sel_download_didCreateDestination = unSelector (mkSelector "download:didCreateDestination:")
      sel_downloadDidFinish = unSelector (mkSelector "downloadDidFinish:")
      sel_download_didFailWithError = unSelector (mkSelector "download:didFailWithError:")
  -- downloadDidBegin:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _downloadDidBegin rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "downloadDidBegin:" "v@:@" stub_0

  -- download:willSendRequest:redirectResponse:
  stub_1 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _download_willSendRequest_redirectResponse rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "download:willSendRequest:redirectResponse:" "@@:@@@" stub_1

  -- download:canAuthenticateAgainstProtectionSpace:
  stub_2 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _download_canAuthenticateAgainstProtectionSpace rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "download:canAuthenticateAgainstProtectionSpace:" "B@:@@" stub_2

  -- download:didReceiveAuthenticationChallenge:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _download_didReceiveAuthenticationChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "download:didReceiveAuthenticationChallenge:" "v@:@@" stub_3

  -- download:didCancelAuthenticationChallenge:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _download_didCancelAuthenticationChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "download:didCancelAuthenticationChallenge:" "v@:@@" stub_4

  -- downloadShouldUseCredentialStorage:
  stub_5 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _downloadShouldUseCredentialStorage rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "downloadShouldUseCredentialStorage:" "B@:@" stub_5

  -- download:didReceiveResponse:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _download_didReceiveResponse rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "download:didReceiveResponse:" "v@:@@" stub_6

  -- download:willResumeWithResponse:fromByte:
  stub_7 <- wrap_at_at_q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _download_willResumeWithResponse_fromByte rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2)
  addObjCMethod cls "download:willResumeWithResponse:fromByte:" "v@:@@q" stub_7

  -- download:didReceiveDataOfLength:
  stub_8 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _download_didReceiveDataOfLength rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "download:didReceiveDataOfLength:" "v@:@Q" stub_8

  -- download:shouldDecodeSourceDataOfMIMEType:
  stub_9 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _download_shouldDecodeSourceDataOfMIMEType rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "download:shouldDecodeSourceDataOfMIMEType:" "B@:@@" stub_9

  -- download:decideDestinationWithSuggestedFilename:
  stub_10 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _download_decideDestinationWithSuggestedFilename rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "download:decideDestinationWithSuggestedFilename:" "v@:@@" stub_10

  -- download:didCreateDestination:
  stub_11 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _download_didCreateDestination rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "download:didCreateDestination:" "v@:@@" stub_11

  -- downloadDidFinish:
  stub_12 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _downloadDidFinish rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "downloadDidFinish:" "v@:@" stub_12

  -- download:didFailWithError:
  stub_13 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    case _download_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "download:didFailWithError:" "v@:@@" stub_13

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLDownloadDelegateOverrides
    if queriedSel == sel_downloadDidBegin then pure (maybe 0 (const 1) (_downloadDidBegin rec_))
    else if queriedSel == sel_download_willSendRequest_redirectResponse then pure (maybe 0 (const 1) (_download_willSendRequest_redirectResponse rec_))
    else if queriedSel == sel_download_canAuthenticateAgainstProtectionSpace then pure (maybe 0 (const 1) (_download_canAuthenticateAgainstProtectionSpace rec_))
    else if queriedSel == sel_download_didReceiveAuthenticationChallenge then pure (maybe 0 (const 1) (_download_didReceiveAuthenticationChallenge rec_))
    else if queriedSel == sel_download_didCancelAuthenticationChallenge then pure (maybe 0 (const 1) (_download_didCancelAuthenticationChallenge rec_))
    else if queriedSel == sel_downloadShouldUseCredentialStorage then pure (maybe 0 (const 1) (_downloadShouldUseCredentialStorage rec_))
    else if queriedSel == sel_download_didReceiveResponse then pure (maybe 0 (const 1) (_download_didReceiveResponse rec_))
    else if queriedSel == sel_download_willResumeWithResponse_fromByte then pure (maybe 0 (const 1) (_download_willResumeWithResponse_fromByte rec_))
    else if queriedSel == sel_download_didReceiveDataOfLength then pure (maybe 0 (const 1) (_download_didReceiveDataOfLength rec_))
    else if queriedSel == sel_download_shouldDecodeSourceDataOfMIMEType then pure (maybe 0 (const 1) (_download_shouldDecodeSourceDataOfMIMEType rec_))
    else if queriedSel == sel_download_decideDestinationWithSuggestedFilename then pure (maybe 0 (const 1) (_download_decideDestinationWithSuggestedFilename rec_))
    else if queriedSel == sel_download_didCreateDestination then pure (maybe 0 (const 1) (_download_didCreateDestination rec_))
    else if queriedSel == sel_downloadDidFinish then pure (maybe 0 (const 1) (_downloadDidFinish rec_))
    else if queriedSel == sel_download_didFailWithError then pure (maybe 0 (const 1) (_download_didFailWithError rec_))
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
newNSURLDownloadDelegate :: NSURLDownloadDelegateOverrides -> IO RawId
newNSURLDownloadDelegate overrides = do
  inst <- class_createInstance nsurlDownloadDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
