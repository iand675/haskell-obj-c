{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSURLSessionTaskDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSURLSessionTaskDelegate defaultNSURLSessionTaskDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSURLSessionTaskDelegate
  ( NSURLSessionTaskDelegateOverrides(..)
  , defaultNSURLSessionTaskDelegateOverrides
  , newNSURLSessionTaskDelegate
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

-- | Overrides record for @\@protocol NSURLSessionTaskDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSURLSessionTaskDelegateOverrides = NSURLSessionTaskDelegateOverrides
  { _urlSession_didCreateTask :: !(Maybe (RawId -> RawId -> IO ()))
  , _urlSession_taskIsWaitingForConnectivity :: !(Maybe (RawId -> RawId -> IO ()))
  , _urlSession_task_didSendBodyData_totalBytesSent_totalBytesExpectedToSend :: !(Maybe (RawId -> RawId -> Int -> Int -> Int -> IO ()))
  , _urlSession_task_didReceiveInformationalResponse :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _urlSession_task_didFinishCollectingMetrics :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _urlSession_task_didCompleteWithError :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSURLSessionTaskDelegateOverrides :: NSURLSessionTaskDelegateOverrides
defaultNSURLSessionTaskDelegateOverrides = NSURLSessionTaskDelegateOverrides
  { _urlSession_didCreateTask = Nothing
  , _urlSession_taskIsWaitingForConnectivity = Nothing
  , _urlSession_task_didSendBodyData_totalBytesSent_totalBytesExpectedToSend = Nothing
  , _urlSession_task_didReceiveInformationalResponse = Nothing
  , _urlSession_task_didFinishCollectingMetrics = Nothing
  , _urlSession_task_didCompleteWithError = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_q_q_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> CLong -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> CLong -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsurlSessionTaskDelegateDelegateClass #-}
nsurlSessionTaskDelegateDelegateClass :: Class
nsurlSessionTaskDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSURLSessionTaskDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_urlSession_didCreateTask = unSelector (mkSelector "URLSession:didCreateTask:")
      sel_urlSession_taskIsWaitingForConnectivity = unSelector (mkSelector "URLSession:taskIsWaitingForConnectivity:")
      sel_urlSession_task_didSendBodyData_totalBytesSent_totalBytesExpectedToSend = unSelector (mkSelector "URLSession:task:didSendBodyData:totalBytesSent:totalBytesExpectedToSend:")
      sel_urlSession_task_didReceiveInformationalResponse = unSelector (mkSelector "URLSession:task:didReceiveInformationalResponse:")
      sel_urlSession_task_didFinishCollectingMetrics = unSelector (mkSelector "URLSession:task:didFinishCollectingMetrics:")
      sel_urlSession_task_didCompleteWithError = unSelector (mkSelector "URLSession:task:didCompleteWithError:")
  -- URLSession:didCreateTask:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionTaskDelegateOverrides
    case _urlSession_didCreateTask rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "URLSession:didCreateTask:" "v@:@@" stub_0

  -- URLSession:taskIsWaitingForConnectivity:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionTaskDelegateOverrides
    case _urlSession_taskIsWaitingForConnectivity rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "URLSession:taskIsWaitingForConnectivity:" "v@:@@" stub_1

  -- URLSession:task:didSendBodyData:totalBytesSent:totalBytesExpectedToSend:
  stub_2 <- wrap_at_at_q_q_q_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionTaskDelegateOverrides
    case _urlSession_task_didSendBodyData_totalBytesSent_totalBytesExpectedToSend rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2) (fromIntegral arg3) (fromIntegral arg4)
  addObjCMethod cls "URLSession:task:didSendBodyData:totalBytesSent:totalBytesExpectedToSend:" "v@:@@qqq" stub_2

  -- URLSession:task:didReceiveInformationalResponse:
  stub_3 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionTaskDelegateOverrides
    case _urlSession_task_didReceiveInformationalResponse rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:task:didReceiveInformationalResponse:" "v@:@@@" stub_3

  -- URLSession:task:didFinishCollectingMetrics:
  stub_4 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionTaskDelegateOverrides
    case _urlSession_task_didFinishCollectingMetrics rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:task:didFinishCollectingMetrics:" "v@:@@@" stub_4

  -- URLSession:task:didCompleteWithError:
  stub_5 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionTaskDelegateOverrides
    case _urlSession_task_didCompleteWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:task:didCompleteWithError:" "v@:@@@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionTaskDelegateOverrides
    if queriedSel == sel_urlSession_didCreateTask then pure (maybe 0 (const 1) (_urlSession_didCreateTask rec_))
    else if queriedSel == sel_urlSession_taskIsWaitingForConnectivity then pure (maybe 0 (const 1) (_urlSession_taskIsWaitingForConnectivity rec_))
    else if queriedSel == sel_urlSession_task_didSendBodyData_totalBytesSent_totalBytesExpectedToSend then pure (maybe 0 (const 1) (_urlSession_task_didSendBodyData_totalBytesSent_totalBytesExpectedToSend rec_))
    else if queriedSel == sel_urlSession_task_didReceiveInformationalResponse then pure (maybe 0 (const 1) (_urlSession_task_didReceiveInformationalResponse rec_))
    else if queriedSel == sel_urlSession_task_didFinishCollectingMetrics then pure (maybe 0 (const 1) (_urlSession_task_didFinishCollectingMetrics rec_))
    else if queriedSel == sel_urlSession_task_didCompleteWithError then pure (maybe 0 (const 1) (_urlSession_task_didCompleteWithError rec_))
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
newNSURLSessionTaskDelegate :: NSURLSessionTaskDelegateOverrides -> IO RawId
newNSURLSessionTaskDelegate overrides = do
  inst <- class_createInstance nsurlSessionTaskDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
