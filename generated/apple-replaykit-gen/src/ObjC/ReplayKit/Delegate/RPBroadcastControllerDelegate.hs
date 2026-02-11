{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol RPBroadcastControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newRPBroadcastControllerDelegate defaultRPBroadcastControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ReplayKit.Delegate.RPBroadcastControllerDelegate
  ( RPBroadcastControllerDelegateOverrides(..)
  , defaultRPBroadcastControllerDelegateOverrides
  , newRPBroadcastControllerDelegate
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

-- | Overrides record for @\@protocol RPBroadcastControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data RPBroadcastControllerDelegateOverrides = RPBroadcastControllerDelegateOverrides
  { _broadcastController_didFinishWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _broadcastController_didUpdateServiceInfo :: !(Maybe (RawId -> RawId -> IO ()))
  , _broadcastController_didUpdateBroadcastURL :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultRPBroadcastControllerDelegateOverrides :: RPBroadcastControllerDelegateOverrides
defaultRPBroadcastControllerDelegateOverrides = RPBroadcastControllerDelegateOverrides
  { _broadcastController_didFinishWithError = Nothing
  , _broadcastController_didUpdateServiceInfo = Nothing
  , _broadcastController_didUpdateBroadcastURL = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE rpBroadcastControllerDelegateDelegateClass #-}
rpBroadcastControllerDelegateDelegateClass :: Class
rpBroadcastControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsRPBroadcastControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_broadcastController_didFinishWithError = unSelector (mkSelector "broadcastController:didFinishWithError:")
      sel_broadcastController_didUpdateServiceInfo = unSelector (mkSelector "broadcastController:didUpdateServiceInfo:")
      sel_broadcastController_didUpdateBroadcastURL = unSelector (mkSelector "broadcastController:didUpdateBroadcastURL:")
  -- broadcastController:didFinishWithError:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO RPBroadcastControllerDelegateOverrides
    case _broadcastController_didFinishWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "broadcastController:didFinishWithError:" "v@:@@" stub_0

  -- broadcastController:didUpdateServiceInfo:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO RPBroadcastControllerDelegateOverrides
    case _broadcastController_didUpdateServiceInfo rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "broadcastController:didUpdateServiceInfo:" "v@:@@" stub_1

  -- broadcastController:didUpdateBroadcastURL:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO RPBroadcastControllerDelegateOverrides
    case _broadcastController_didUpdateBroadcastURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "broadcastController:didUpdateBroadcastURL:" "v@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO RPBroadcastControllerDelegateOverrides
    if queriedSel == sel_broadcastController_didFinishWithError then pure (maybe 0 (const 1) (_broadcastController_didFinishWithError rec_))
    else if queriedSel == sel_broadcastController_didUpdateServiceInfo then pure (maybe 0 (const 1) (_broadcastController_didUpdateServiceInfo rec_))
    else if queriedSel == sel_broadcastController_didUpdateBroadcastURL then pure (maybe 0 (const 1) (_broadcastController_didUpdateBroadcastURL rec_))
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
newRPBroadcastControllerDelegate :: RPBroadcastControllerDelegateOverrides -> IO RawId
newRPBroadcastControllerDelegate overrides = do
  inst <- class_createInstance rpBroadcastControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
