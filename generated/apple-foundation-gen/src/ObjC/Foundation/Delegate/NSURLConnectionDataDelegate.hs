{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSURLConnectionDataDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSURLConnectionDataDelegate defaultNSURLConnectionDataDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSURLConnectionDataDelegate
  ( NSURLConnectionDataDelegateOverrides(..)
  , defaultNSURLConnectionDataDelegateOverrides
  , newNSURLConnectionDataDelegate
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

-- | Overrides record for @\@protocol NSURLConnectionDataDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSURLConnectionDataDelegateOverrides = NSURLConnectionDataDelegateOverrides
  { _connection_willSendRequest_redirectResponse :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _connection_didReceiveResponse :: !(Maybe (RawId -> RawId -> IO ()))
  , _connection_didReceiveData :: !(Maybe (RawId -> RawId -> IO ()))
  , _connection_needNewBodyStream :: !(Maybe (RawId -> RawId -> IO RawId))
  , _connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite :: !(Maybe (RawId -> Int -> Int -> Int -> IO ()))
  , _connection_willCacheResponse :: !(Maybe (RawId -> RawId -> IO RawId))
  , _connectionDidFinishLoading :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSURLConnectionDataDelegateOverrides :: NSURLConnectionDataDelegateOverrides
defaultNSURLConnectionDataDelegateOverrides = NSURLConnectionDataDelegateOverrides
  { _connection_willSendRequest_redirectResponse = Nothing
  , _connection_didReceiveResponse = Nothing
  , _connection_didReceiveData = Nothing
  , _connection_needNewBodyStream = Nothing
  , _connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite = Nothing
  , _connection_willCacheResponse = Nothing
  , _connectionDidFinishLoading = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_q_q_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsurlConnectionDataDelegateDelegateClass #-}
nsurlConnectionDataDelegateDelegateClass :: Class
nsurlConnectionDataDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSURLConnectionDataDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_connection_willSendRequest_redirectResponse = unSelector (mkSelector "connection:willSendRequest:redirectResponse:")
      sel_connection_didReceiveResponse = unSelector (mkSelector "connection:didReceiveResponse:")
      sel_connection_didReceiveData = unSelector (mkSelector "connection:didReceiveData:")
      sel_connection_needNewBodyStream = unSelector (mkSelector "connection:needNewBodyStream:")
      sel_connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite = unSelector (mkSelector "connection:didSendBodyData:totalBytesWritten:totalBytesExpectedToWrite:")
      sel_connection_willCacheResponse = unSelector (mkSelector "connection:willCacheResponse:")
      sel_connectionDidFinishLoading = unSelector (mkSelector "connectionDidFinishLoading:")
  -- connection:willSendRequest:redirectResponse:
  stub_0 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDataDelegateOverrides
    case _connection_willSendRequest_redirectResponse rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "connection:willSendRequest:redirectResponse:" "@@:@@@" stub_0

  -- connection:didReceiveResponse:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDataDelegateOverrides
    case _connection_didReceiveResponse rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "connection:didReceiveResponse:" "v@:@@" stub_1

  -- connection:didReceiveData:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDataDelegateOverrides
    case _connection_didReceiveData rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "connection:didReceiveData:" "v@:@@" stub_2

  -- connection:needNewBodyStream:
  stub_3 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDataDelegateOverrides
    case _connection_needNewBodyStream rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "connection:needNewBodyStream:" "@@:@@" stub_3

  -- connection:didSendBodyData:totalBytesWritten:totalBytesExpectedToWrite:
  stub_4 <- wrap_at_q_q_q_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDataDelegateOverrides
    case _connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2) (fromIntegral arg3)
  addObjCMethod cls "connection:didSendBodyData:totalBytesWritten:totalBytesExpectedToWrite:" "v@:@qqq" stub_4

  -- connection:willCacheResponse:
  stub_5 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDataDelegateOverrides
    case _connection_willCacheResponse rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "connection:willCacheResponse:" "@@:@@" stub_5

  -- connectionDidFinishLoading:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDataDelegateOverrides
    case _connectionDidFinishLoading rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "connectionDidFinishLoading:" "v@:@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDataDelegateOverrides
    if queriedSel == sel_connection_willSendRequest_redirectResponse then pure (maybe 0 (const 1) (_connection_willSendRequest_redirectResponse rec_))
    else if queriedSel == sel_connection_didReceiveResponse then pure (maybe 0 (const 1) (_connection_didReceiveResponse rec_))
    else if queriedSel == sel_connection_didReceiveData then pure (maybe 0 (const 1) (_connection_didReceiveData rec_))
    else if queriedSel == sel_connection_needNewBodyStream then pure (maybe 0 (const 1) (_connection_needNewBodyStream rec_))
    else if queriedSel == sel_connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite then pure (maybe 0 (const 1) (_connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite rec_))
    else if queriedSel == sel_connection_willCacheResponse then pure (maybe 0 (const 1) (_connection_willCacheResponse rec_))
    else if queriedSel == sel_connectionDidFinishLoading then pure (maybe 0 (const 1) (_connectionDidFinishLoading rec_))
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
newNSURLConnectionDataDelegate :: NSURLConnectionDataDelegateOverrides -> IO RawId
newNSURLConnectionDataDelegate overrides = do
  inst <- class_createInstance nsurlConnectionDataDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
