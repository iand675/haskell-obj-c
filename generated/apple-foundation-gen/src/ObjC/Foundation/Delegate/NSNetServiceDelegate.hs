{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSNetServiceDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSNetServiceDelegate defaultNSNetServiceDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSNetServiceDelegate
  ( NSNetServiceDelegateOverrides(..)
  , defaultNSNetServiceDelegateOverrides
  , newNSNetServiceDelegate
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

-- | Overrides record for @\@protocol NSNetServiceDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSNetServiceDelegateOverrides = NSNetServiceDelegateOverrides
  { _netServiceWillPublish :: !(Maybe (RawId -> IO ()))
  , _netServiceDidPublish :: !(Maybe (RawId -> IO ()))
  , _netService_didNotPublish :: !(Maybe (RawId -> RawId -> IO ()))
  , _netServiceWillResolve :: !(Maybe (RawId -> IO ()))
  , _netServiceDidResolveAddress :: !(Maybe (RawId -> IO ()))
  , _netService_didNotResolve :: !(Maybe (RawId -> RawId -> IO ()))
  , _netServiceDidStop :: !(Maybe (RawId -> IO ()))
  , _netService_didUpdateTXTRecordData :: !(Maybe (RawId -> RawId -> IO ()))
  , _netService_didAcceptConnectionWithInputStream_outputStream :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSNetServiceDelegateOverrides :: NSNetServiceDelegateOverrides
defaultNSNetServiceDelegateOverrides = NSNetServiceDelegateOverrides
  { _netServiceWillPublish = Nothing
  , _netServiceDidPublish = Nothing
  , _netService_didNotPublish = Nothing
  , _netServiceWillResolve = Nothing
  , _netServiceDidResolveAddress = Nothing
  , _netService_didNotResolve = Nothing
  , _netServiceDidStop = Nothing
  , _netService_didUpdateTXTRecordData = Nothing
  , _netService_didAcceptConnectionWithInputStream_outputStream = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsNetServiceDelegateDelegateClass #-}
nsNetServiceDelegateDelegateClass :: Class
nsNetServiceDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSNetServiceDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_netServiceWillPublish = unSelector (mkSelector "netServiceWillPublish:")
      sel_netServiceDidPublish = unSelector (mkSelector "netServiceDidPublish:")
      sel_netService_didNotPublish = unSelector (mkSelector "netService:didNotPublish:")
      sel_netServiceWillResolve = unSelector (mkSelector "netServiceWillResolve:")
      sel_netServiceDidResolveAddress = unSelector (mkSelector "netServiceDidResolveAddress:")
      sel_netService_didNotResolve = unSelector (mkSelector "netService:didNotResolve:")
      sel_netServiceDidStop = unSelector (mkSelector "netServiceDidStop:")
      sel_netService_didUpdateTXTRecordData = unSelector (mkSelector "netService:didUpdateTXTRecordData:")
      sel_netService_didAcceptConnectionWithInputStream_outputStream = unSelector (mkSelector "netService:didAcceptConnectionWithInputStream:outputStream:")
  -- netServiceWillPublish:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSNetServiceDelegateOverrides
    case _netServiceWillPublish rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "netServiceWillPublish:" "v@:@" stub_0

  -- netServiceDidPublish:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSNetServiceDelegateOverrides
    case _netServiceDidPublish rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "netServiceDidPublish:" "v@:@" stub_1

  -- netService:didNotPublish:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSNetServiceDelegateOverrides
    case _netService_didNotPublish rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "netService:didNotPublish:" "v@:@@" stub_2

  -- netServiceWillResolve:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSNetServiceDelegateOverrides
    case _netServiceWillResolve rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "netServiceWillResolve:" "v@:@" stub_3

  -- netServiceDidResolveAddress:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSNetServiceDelegateOverrides
    case _netServiceDidResolveAddress rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "netServiceDidResolveAddress:" "v@:@" stub_4

  -- netService:didNotResolve:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSNetServiceDelegateOverrides
    case _netService_didNotResolve rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "netService:didNotResolve:" "v@:@@" stub_5

  -- netServiceDidStop:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSNetServiceDelegateOverrides
    case _netServiceDidStop rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "netServiceDidStop:" "v@:@" stub_6

  -- netService:didUpdateTXTRecordData:
  stub_7 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSNetServiceDelegateOverrides
    case _netService_didUpdateTXTRecordData rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "netService:didUpdateTXTRecordData:" "v@:@@" stub_7

  -- netService:didAcceptConnectionWithInputStream:outputStream:
  stub_8 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSNetServiceDelegateOverrides
    case _netService_didAcceptConnectionWithInputStream_outputStream rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "netService:didAcceptConnectionWithInputStream:outputStream:" "v@:@@@" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSNetServiceDelegateOverrides
    if queriedSel == sel_netServiceWillPublish then pure (maybe 0 (const 1) (_netServiceWillPublish rec_))
    else if queriedSel == sel_netServiceDidPublish then pure (maybe 0 (const 1) (_netServiceDidPublish rec_))
    else if queriedSel == sel_netService_didNotPublish then pure (maybe 0 (const 1) (_netService_didNotPublish rec_))
    else if queriedSel == sel_netServiceWillResolve then pure (maybe 0 (const 1) (_netServiceWillResolve rec_))
    else if queriedSel == sel_netServiceDidResolveAddress then pure (maybe 0 (const 1) (_netServiceDidResolveAddress rec_))
    else if queriedSel == sel_netService_didNotResolve then pure (maybe 0 (const 1) (_netService_didNotResolve rec_))
    else if queriedSel == sel_netServiceDidStop then pure (maybe 0 (const 1) (_netServiceDidStop rec_))
    else if queriedSel == sel_netService_didUpdateTXTRecordData then pure (maybe 0 (const 1) (_netService_didUpdateTXTRecordData rec_))
    else if queriedSel == sel_netService_didAcceptConnectionWithInputStream_outputStream then pure (maybe 0 (const 1) (_netService_didAcceptConnectionWithInputStream_outputStream rec_))
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
newNSNetServiceDelegate :: NSNetServiceDelegateOverrides -> IO RawId
newNSNetServiceDelegate overrides = do
  inst <- class_createInstance nsNetServiceDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
