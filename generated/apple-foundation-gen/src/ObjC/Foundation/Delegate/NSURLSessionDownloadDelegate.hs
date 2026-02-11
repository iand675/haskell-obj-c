{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSURLSessionDownloadDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSURLSessionDownloadDelegate defaultNSURLSessionDownloadDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSURLSessionDownloadDelegate
  ( NSURLSessionDownloadDelegateOverrides(..)
  , defaultNSURLSessionDownloadDelegateOverrides
  , newNSURLSessionDownloadDelegate
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

-- | Overrides record for @\@protocol NSURLSessionDownloadDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSURLSessionDownloadDelegateOverrides = NSURLSessionDownloadDelegateOverrides
  { _urlSession_downloadTask_didFinishDownloadingToURL :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _urlSession_downloadTask_didWriteData_totalBytesWritten_totalBytesExpectedToWrite :: !(Maybe (RawId -> RawId -> Int -> Int -> Int -> IO ()))
  , _urlSession_downloadTask_didResumeAtOffset_expectedTotalBytes :: !(Maybe (RawId -> RawId -> Int -> Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSURLSessionDownloadDelegateOverrides :: NSURLSessionDownloadDelegateOverrides
defaultNSURLSessionDownloadDelegateOverrides = NSURLSessionDownloadDelegateOverrides
  { _urlSession_downloadTask_didFinishDownloadingToURL = Nothing
  , _urlSession_downloadTask_didWriteData_totalBytesWritten_totalBytesExpectedToWrite = Nothing
  , _urlSession_downloadTask_didResumeAtOffset_expectedTotalBytes = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_q_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_q_q_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> CLong -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> CLong -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsurlSessionDownloadDelegateDelegateClass #-}
nsurlSessionDownloadDelegateDelegateClass :: Class
nsurlSessionDownloadDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSURLSessionDownloadDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_urlSession_downloadTask_didFinishDownloadingToURL = unSelector (mkSelector "URLSession:downloadTask:didFinishDownloadingToURL:")
      sel_urlSession_downloadTask_didWriteData_totalBytesWritten_totalBytesExpectedToWrite = unSelector (mkSelector "URLSession:downloadTask:didWriteData:totalBytesWritten:totalBytesExpectedToWrite:")
      sel_urlSession_downloadTask_didResumeAtOffset_expectedTotalBytes = unSelector (mkSelector "URLSession:downloadTask:didResumeAtOffset:expectedTotalBytes:")
  -- URLSession:downloadTask:didFinishDownloadingToURL:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionDownloadDelegateOverrides
    case _urlSession_downloadTask_didFinishDownloadingToURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:downloadTask:didFinishDownloadingToURL:" "v@:@@@" stub_0

  -- URLSession:downloadTask:didWriteData:totalBytesWritten:totalBytesExpectedToWrite:
  stub_1 <- wrap_at_at_q_q_q_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionDownloadDelegateOverrides
    case _urlSession_downloadTask_didWriteData_totalBytesWritten_totalBytesExpectedToWrite rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2) (fromIntegral arg3) (fromIntegral arg4)
  addObjCMethod cls "URLSession:downloadTask:didWriteData:totalBytesWritten:totalBytesExpectedToWrite:" "v@:@@qqq" stub_1

  -- URLSession:downloadTask:didResumeAtOffset:expectedTotalBytes:
  stub_2 <- wrap_at_at_q_q_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionDownloadDelegateOverrides
    case _urlSession_downloadTask_didResumeAtOffset_expectedTotalBytes rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2) (fromIntegral arg3)
  addObjCMethod cls "URLSession:downloadTask:didResumeAtOffset:expectedTotalBytes:" "v@:@@qq" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionDownloadDelegateOverrides
    if queriedSel == sel_urlSession_downloadTask_didFinishDownloadingToURL then pure (maybe 0 (const 1) (_urlSession_downloadTask_didFinishDownloadingToURL rec_))
    else if queriedSel == sel_urlSession_downloadTask_didWriteData_totalBytesWritten_totalBytesExpectedToWrite then pure (maybe 0 (const 1) (_urlSession_downloadTask_didWriteData_totalBytesWritten_totalBytesExpectedToWrite rec_))
    else if queriedSel == sel_urlSession_downloadTask_didResumeAtOffset_expectedTotalBytes then pure (maybe 0 (const 1) (_urlSession_downloadTask_didResumeAtOffset_expectedTotalBytes rec_))
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
newNSURLSessionDownloadDelegate :: NSURLSessionDownloadDelegateOverrides -> IO RawId
newNSURLSessionDownloadDelegate overrides = do
  inst <- class_createInstance nsurlSessionDownloadDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
