{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MCSessionDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newMCSessionDelegate defaultMCSessionDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MultipeerConnectivity.Delegate.MCSessionDelegate
  ( MCSessionDelegateOverrides(..)
  , defaultMCSessionDelegateOverrides
  , newMCSessionDelegate
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

-- | Overrides record for @\@protocol MCSessionDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MCSessionDelegateOverrides = MCSessionDelegateOverrides
  { _session_didReceiveData_fromPeer :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _session_didReceiveStream_withName_fromPeer :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _session_didStartReceivingResourceWithName_fromPeer_withProgress :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _session_didFinishReceivingResourceWithName_fromPeer_atURL_withError :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMCSessionDelegateOverrides :: MCSessionDelegateOverrides
defaultMCSessionDelegateOverrides = MCSessionDelegateOverrides
  { _session_didReceiveData_fromPeer = Nothing
  , _session_didReceiveStream_withName_fromPeer = Nothing
  , _session_didStartReceivingResourceWithName_fromPeer_withProgress = Nothing
  , _session_didFinishReceivingResourceWithName_fromPeer_atURL_withError = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mcSessionDelegateDelegateClass #-}
mcSessionDelegateDelegateClass :: Class
mcSessionDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMCSessionDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_session_didReceiveData_fromPeer = unSelector (mkSelector "session:didReceiveData:fromPeer:")
      sel_session_didReceiveStream_withName_fromPeer = unSelector (mkSelector "session:didReceiveStream:withName:fromPeer:")
      sel_session_didStartReceivingResourceWithName_fromPeer_withProgress = unSelector (mkSelector "session:didStartReceivingResourceWithName:fromPeer:withProgress:")
      sel_session_didFinishReceivingResourceWithName_fromPeer_atURL_withError = unSelector (mkSelector "session:didFinishReceivingResourceWithName:fromPeer:atURL:withError:")
  -- session:didReceiveData:fromPeer:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCSessionDelegateOverrides
    case _session_didReceiveData_fromPeer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "session:didReceiveData:fromPeer:" "v@:@@@" stub_0

  -- session:didReceiveStream:withName:fromPeer:
  stub_1 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCSessionDelegateOverrides
    case _session_didReceiveStream_withName_fromPeer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "session:didReceiveStream:withName:fromPeer:" "v@:@@@@" stub_1

  -- session:didStartReceivingResourceWithName:fromPeer:withProgress:
  stub_2 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCSessionDelegateOverrides
    case _session_didStartReceivingResourceWithName_fromPeer_withProgress rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "session:didStartReceivingResourceWithName:fromPeer:withProgress:" "v@:@@@@" stub_2

  -- session:didFinishReceivingResourceWithName:fromPeer:atURL:withError:
  stub_3 <- wrap_at_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCSessionDelegateOverrides
    case _session_didFinishReceivingResourceWithName_fromPeer_atURL_withError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4)
  addObjCMethod cls "session:didFinishReceivingResourceWithName:fromPeer:atURL:withError:" "v@:@@@@@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCSessionDelegateOverrides
    if queriedSel == sel_session_didReceiveData_fromPeer then pure (maybe 0 (const 1) (_session_didReceiveData_fromPeer rec_))
    else if queriedSel == sel_session_didReceiveStream_withName_fromPeer then pure (maybe 0 (const 1) (_session_didReceiveStream_withName_fromPeer rec_))
    else if queriedSel == sel_session_didStartReceivingResourceWithName_fromPeer_withProgress then pure (maybe 0 (const 1) (_session_didStartReceivingResourceWithName_fromPeer_withProgress rec_))
    else if queriedSel == sel_session_didFinishReceivingResourceWithName_fromPeer_atURL_withError then pure (maybe 0 (const 1) (_session_didFinishReceivingResourceWithName_fromPeer_atURL_withError rec_))
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
newMCSessionDelegate :: MCSessionDelegateOverrides -> IO RawId
newMCSessionDelegate overrides = do
  inst <- class_createInstance mcSessionDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
