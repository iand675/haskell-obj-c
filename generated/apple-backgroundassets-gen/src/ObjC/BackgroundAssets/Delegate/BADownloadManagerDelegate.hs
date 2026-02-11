{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol BADownloadManagerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newBADownloadManagerDelegate defaultBADownloadManagerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.BackgroundAssets.Delegate.BADownloadManagerDelegate
  ( BADownloadManagerDelegateOverrides(..)
  , defaultBADownloadManagerDelegateOverrides
  , newBADownloadManagerDelegate
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

-- | Overrides record for @\@protocol BADownloadManagerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data BADownloadManagerDelegateOverrides = BADownloadManagerDelegateOverrides
  { _downloadDidBegin :: !(Maybe (RawId -> IO ()))
  , _downloadDidPause :: !(Maybe (RawId -> IO ()))
  , _download_didWriteBytes_totalBytesWritten_totalBytesExpectedToWrite :: !(Maybe (RawId -> Int -> Int -> Int -> IO ()))
  , _download_failedWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _download_finishedWithFileURL :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultBADownloadManagerDelegateOverrides :: BADownloadManagerDelegateOverrides
defaultBADownloadManagerDelegateOverrides = BADownloadManagerDelegateOverrides
  { _downloadDidBegin = Nothing
  , _downloadDidPause = Nothing
  , _download_didWriteBytes_totalBytesWritten_totalBytesExpectedToWrite = Nothing
  , _download_failedWithError = Nothing
  , _download_finishedWithFileURL = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_q_q_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE baDownloadManagerDelegateDelegateClass #-}
baDownloadManagerDelegateDelegateClass :: Class
baDownloadManagerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsBADownloadManagerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_downloadDidBegin = unSelector (mkSelector "downloadDidBegin:")
      sel_downloadDidPause = unSelector (mkSelector "downloadDidPause:")
      sel_download_didWriteBytes_totalBytesWritten_totalBytesExpectedToWrite = unSelector (mkSelector "download:didWriteBytes:totalBytesWritten:totalBytesExpectedToWrite:")
      sel_download_failedWithError = unSelector (mkSelector "download:failedWithError:")
      sel_download_finishedWithFileURL = unSelector (mkSelector "download:finishedWithFileURL:")
  -- downloadDidBegin:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BADownloadManagerDelegateOverrides
    case _downloadDidBegin rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "downloadDidBegin:" "v@:@" stub_0

  -- downloadDidPause:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BADownloadManagerDelegateOverrides
    case _downloadDidPause rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "downloadDidPause:" "v@:@" stub_1

  -- download:didWriteBytes:totalBytesWritten:totalBytesExpectedToWrite:
  stub_2 <- wrap_at_q_q_q_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BADownloadManagerDelegateOverrides
    case _download_didWriteBytes_totalBytesWritten_totalBytesExpectedToWrite rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2) (fromIntegral arg3)
  addObjCMethod cls "download:didWriteBytes:totalBytesWritten:totalBytesExpectedToWrite:" "v@:@qqq" stub_2

  -- download:failedWithError:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BADownloadManagerDelegateOverrides
    case _download_failedWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "download:failedWithError:" "v@:@@" stub_3

  -- download:finishedWithFileURL:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BADownloadManagerDelegateOverrides
    case _download_finishedWithFileURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "download:finishedWithFileURL:" "v@:@@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BADownloadManagerDelegateOverrides
    if queriedSel == sel_downloadDidBegin then pure (maybe 0 (const 1) (_downloadDidBegin rec_))
    else if queriedSel == sel_downloadDidPause then pure (maybe 0 (const 1) (_downloadDidPause rec_))
    else if queriedSel == sel_download_didWriteBytes_totalBytesWritten_totalBytesExpectedToWrite then pure (maybe 0 (const 1) (_download_didWriteBytes_totalBytesWritten_totalBytesExpectedToWrite rec_))
    else if queriedSel == sel_download_failedWithError then pure (maybe 0 (const 1) (_download_failedWithError rec_))
    else if queriedSel == sel_download_finishedWithFileURL then pure (maybe 0 (const 1) (_download_finishedWithFileURL rec_))
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
newBADownloadManagerDelegate :: BADownloadManagerDelegateOverrides -> IO RawId
newBADownloadManagerDelegate overrides = do
  inst <- class_createInstance baDownloadManagerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
