{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKSessionDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newGKSessionDelegate defaultGKSessionDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameKit.Delegate.GKSessionDelegate
  ( GKSessionDelegateOverrides(..)
  , defaultGKSessionDelegateOverrides
  , newGKSessionDelegate
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

-- | Overrides record for @\@protocol GKSessionDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKSessionDelegateOverrides = GKSessionDelegateOverrides
  { _session_didReceiveConnectionRequestFromPeer :: !(Maybe (RawId -> RawId -> IO ()))
  , _session_connectionWithPeerFailed_withError :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _session_didFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultGKSessionDelegateOverrides :: GKSessionDelegateOverrides
defaultGKSessionDelegateOverrides = GKSessionDelegateOverrides
  { _session_didReceiveConnectionRequestFromPeer = Nothing
  , _session_connectionWithPeerFailed_withError = Nothing
  , _session_didFailWithError = Nothing
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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkSessionDelegateDelegateClass #-}
gkSessionDelegateDelegateClass :: Class
gkSessionDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKSessionDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_session_didReceiveConnectionRequestFromPeer = unSelector (mkSelector "session:didReceiveConnectionRequestFromPeer:")
      sel_session_connectionWithPeerFailed_withError = unSelector (mkSelector "session:connectionWithPeerFailed:withError:")
      sel_session_didFailWithError = unSelector (mkSelector "session:didFailWithError:")
  -- session:didReceiveConnectionRequestFromPeer:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKSessionDelegateOverrides
    case _session_didReceiveConnectionRequestFromPeer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "session:didReceiveConnectionRequestFromPeer:" "v@:@@" stub_0

  -- session:connectionWithPeerFailed:withError:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKSessionDelegateOverrides
    case _session_connectionWithPeerFailed_withError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "session:connectionWithPeerFailed:withError:" "v@:@@@" stub_1

  -- session:didFailWithError:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKSessionDelegateOverrides
    case _session_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "session:didFailWithError:" "v@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKSessionDelegateOverrides
    if queriedSel == sel_session_didReceiveConnectionRequestFromPeer then pure (maybe 0 (const 1) (_session_didReceiveConnectionRequestFromPeer rec_))
    else if queriedSel == sel_session_connectionWithPeerFailed_withError then pure (maybe 0 (const 1) (_session_connectionWithPeerFailed_withError rec_))
    else if queriedSel == sel_session_didFailWithError then pure (maybe 0 (const 1) (_session_didFailWithError rec_))
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
newGKSessionDelegate :: GKSessionDelegateOverrides -> IO RawId
newGKSessionDelegate overrides = do
  inst <- class_createInstance gkSessionDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
