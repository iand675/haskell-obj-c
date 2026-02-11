{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSURLConnectionDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSURLConnectionDelegate defaultNSURLConnectionDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSURLConnectionDelegate
  ( NSURLConnectionDelegateOverrides(..)
  , defaultNSURLConnectionDelegateOverrides
  , newNSURLConnectionDelegate
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

-- | Overrides record for @\@protocol NSURLConnectionDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSURLConnectionDelegateOverrides = NSURLConnectionDelegateOverrides
  { _connection_didFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _connectionShouldUseCredentialStorage :: !(Maybe (RawId -> IO Bool))
  , _connection_willSendRequestForAuthenticationChallenge :: !(Maybe (RawId -> RawId -> IO ()))
  , _connection_canAuthenticateAgainstProtectionSpace :: !(Maybe (RawId -> RawId -> IO Bool))
  , _connection_didReceiveAuthenticationChallenge :: !(Maybe (RawId -> RawId -> IO ()))
  , _connection_didCancelAuthenticationChallenge :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSURLConnectionDelegateOverrides :: NSURLConnectionDelegateOverrides
defaultNSURLConnectionDelegateOverrides = NSURLConnectionDelegateOverrides
  { _connection_didFailWithError = Nothing
  , _connectionShouldUseCredentialStorage = Nothing
  , _connection_willSendRequestForAuthenticationChallenge = Nothing
  , _connection_canAuthenticateAgainstProtectionSpace = Nothing
  , _connection_didReceiveAuthenticationChallenge = Nothing
  , _connection_didCancelAuthenticationChallenge = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsurlConnectionDelegateDelegateClass #-}
nsurlConnectionDelegateDelegateClass :: Class
nsurlConnectionDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSURLConnectionDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_connection_didFailWithError = unSelector (mkSelector "connection:didFailWithError:")
      sel_connectionShouldUseCredentialStorage = unSelector (mkSelector "connectionShouldUseCredentialStorage:")
      sel_connection_willSendRequestForAuthenticationChallenge = unSelector (mkSelector "connection:willSendRequestForAuthenticationChallenge:")
      sel_connection_canAuthenticateAgainstProtectionSpace = unSelector (mkSelector "connection:canAuthenticateAgainstProtectionSpace:")
      sel_connection_didReceiveAuthenticationChallenge = unSelector (mkSelector "connection:didReceiveAuthenticationChallenge:")
      sel_connection_didCancelAuthenticationChallenge = unSelector (mkSelector "connection:didCancelAuthenticationChallenge:")
  -- connection:didFailWithError:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDelegateOverrides
    case _connection_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "connection:didFailWithError:" "v@:@@" stub_0

  -- connectionShouldUseCredentialStorage:
  stub_1 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDelegateOverrides
    case _connectionShouldUseCredentialStorage rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "connectionShouldUseCredentialStorage:" "B@:@" stub_1

  -- connection:willSendRequestForAuthenticationChallenge:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDelegateOverrides
    case _connection_willSendRequestForAuthenticationChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "connection:willSendRequestForAuthenticationChallenge:" "v@:@@" stub_2

  -- connection:canAuthenticateAgainstProtectionSpace:
  stub_3 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDelegateOverrides
    case _connection_canAuthenticateAgainstProtectionSpace rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "connection:canAuthenticateAgainstProtectionSpace:" "B@:@@" stub_3

  -- connection:didReceiveAuthenticationChallenge:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDelegateOverrides
    case _connection_didReceiveAuthenticationChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "connection:didReceiveAuthenticationChallenge:" "v@:@@" stub_4

  -- connection:didCancelAuthenticationChallenge:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDelegateOverrides
    case _connection_didCancelAuthenticationChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "connection:didCancelAuthenticationChallenge:" "v@:@@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLConnectionDelegateOverrides
    if queriedSel == sel_connection_didFailWithError then pure (maybe 0 (const 1) (_connection_didFailWithError rec_))
    else if queriedSel == sel_connectionShouldUseCredentialStorage then pure (maybe 0 (const 1) (_connectionShouldUseCredentialStorage rec_))
    else if queriedSel == sel_connection_willSendRequestForAuthenticationChallenge then pure (maybe 0 (const 1) (_connection_willSendRequestForAuthenticationChallenge rec_))
    else if queriedSel == sel_connection_canAuthenticateAgainstProtectionSpace then pure (maybe 0 (const 1) (_connection_canAuthenticateAgainstProtectionSpace rec_))
    else if queriedSel == sel_connection_didReceiveAuthenticationChallenge then pure (maybe 0 (const 1) (_connection_didReceiveAuthenticationChallenge rec_))
    else if queriedSel == sel_connection_didCancelAuthenticationChallenge then pure (maybe 0 (const 1) (_connection_didCancelAuthenticationChallenge rec_))
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
newNSURLConnectionDelegate :: NSURLConnectionDelegateOverrides -> IO RawId
newNSURLConnectionDelegate overrides = do
  inst <- class_createInstance nsurlConnectionDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
