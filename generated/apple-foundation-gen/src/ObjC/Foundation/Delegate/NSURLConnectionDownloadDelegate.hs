{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSURLConnectionDownloadDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSURLConnectionDownloadDelegate defaultNSURLConnectionDownloadDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSURLConnectionDownloadDelegate
  ( NSURLConnectionDownloadDelegateOverrides(..)
  , defaultNSURLConnectionDownloadDelegateOverrides
  , newNSURLConnectionDownloadDelegate
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

-- | Overrides record for @\@protocol NSURLConnectionDownloadDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSURLConnectionDownloadDelegateOverrides = NSURLConnectionDownloadDelegateOverrides
  { _connection_didWriteData_totalBytesWritten_expectedTotalBytes :: !(Maybe (RawId -> Int -> Int -> Int -> IO ()))
  , _connectionDidResumeDownloading_totalBytesWritten_expectedTotalBytes :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _connectionDidFinishDownloading_destinationURL :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSURLConnectionDownloadDelegateOverrides :: NSURLConnectionDownloadDelegateOverrides
defaultNSURLConnectionDownloadDelegateOverrides = NSURLConnectionDownloadDelegateOverrides
  { _connection_didWriteData_totalBytesWritten_expectedTotalBytes = Nothing
  , _connectionDidResumeDownloading_totalBytesWritten_expectedTotalBytes = Nothing
  , _connectionDidFinishDownloading_destinationURL = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_q_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_q_q_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> CLong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsurlConnectionDownloadDelegateDelegateClass #-}
nsurlConnectionDownloadDelegateDelegateClass :: Class
nsurlConnectionDownloadDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSURLConnectionDownloadDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_connection_didWriteData_totalBytesWritten_expectedTotalBytes = unSelector (mkSelector "connection:didWriteData:totalBytesWritten:expectedTotalBytes:")
      sel_connectionDidResumeDownloading_totalBytesWritten_expectedTotalBytes = unSelector (mkSelector "connectionDidResumeDownloading:totalBytesWritten:expectedTotalBytes:")
      sel_connectionDidFinishDownloading_destinationURL = unSelector (mkSelector "connectionDidFinishDownloading:destinationURL:")
  -- connection:didWriteData:totalBytesWritten:expectedTotalBytes:
  stub_0 <- wrap_at_q_q_q_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDownloadDelegateOverrides
    case _connection_didWriteData_totalBytesWritten_expectedTotalBytes rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2) (fromIntegral arg3)
  addObjCMethod cls "connection:didWriteData:totalBytesWritten:expectedTotalBytes:" "v@:@qqq" stub_0

  -- connectionDidResumeDownloading:totalBytesWritten:expectedTotalBytes:
  stub_1 <- wrap_at_q_q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDownloadDelegateOverrides
    case _connectionDidResumeDownloading_totalBytesWritten_expectedTotalBytes rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "connectionDidResumeDownloading:totalBytesWritten:expectedTotalBytes:" "v@:@qq" stub_1

  -- connectionDidFinishDownloading:destinationURL:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDownloadDelegateOverrides
    case _connectionDidFinishDownloading_destinationURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "connectionDidFinishDownloading:destinationURL:" "v@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDownloadDelegateOverrides
    if queriedSel == sel_connection_didWriteData_totalBytesWritten_expectedTotalBytes then pure (maybe 0 (const 1) (_connection_didWriteData_totalBytesWritten_expectedTotalBytes rec_))
    else if queriedSel == sel_connectionDidResumeDownloading_totalBytesWritten_expectedTotalBytes then pure (maybe 0 (const 1) (_connectionDidResumeDownloading_totalBytesWritten_expectedTotalBytes rec_))
    else if queriedSel == sel_connectionDidFinishDownloading_destinationURL then pure (maybe 0 (const 1) (_connectionDidFinishDownloading_destinationURL rec_))
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
newNSURLConnectionDownloadDelegate :: NSURLConnectionDownloadDelegateOverrides -> IO RawId
newNSURLConnectionDownloadDelegate overrides = do
  inst <- class_createInstance nsurlConnectionDownloadDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
