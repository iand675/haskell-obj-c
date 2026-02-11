{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PKPushRegistryDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newPKPushRegistryDelegate defaultPKPushRegistryDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.PushKit.Delegate.PKPushRegistryDelegate
  ( PKPushRegistryDelegateOverrides(..)
  , defaultPKPushRegistryDelegateOverrides
  , newPKPushRegistryDelegate
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

-- | Overrides record for @\@protocol PKPushRegistryDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PKPushRegistryDelegateOverrides = PKPushRegistryDelegateOverrides
  { _pushRegistry_didUpdatePushCredentials_forType :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _pushRegistry_didReceiveIncomingPushWithPayload_forType :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _pushRegistry_didInvalidatePushTokenForType :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultPKPushRegistryDelegateOverrides :: PKPushRegistryDelegateOverrides
defaultPKPushRegistryDelegateOverrides = PKPushRegistryDelegateOverrides
  { _pushRegistry_didUpdatePushCredentials_forType = Nothing
  , _pushRegistry_didReceiveIncomingPushWithPayload_forType = Nothing
  , _pushRegistry_didInvalidatePushTokenForType = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE pkPushRegistryDelegateDelegateClass #-}
pkPushRegistryDelegateDelegateClass :: Class
pkPushRegistryDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPKPushRegistryDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_pushRegistry_didUpdatePushCredentials_forType = unSelector (mkSelector "pushRegistry:didUpdatePushCredentials:forType:")
      sel_pushRegistry_didReceiveIncomingPushWithPayload_forType = unSelector (mkSelector "pushRegistry:didReceiveIncomingPushWithPayload:forType:")
      sel_pushRegistry_didInvalidatePushTokenForType = unSelector (mkSelector "pushRegistry:didInvalidatePushTokenForType:")
  -- pushRegistry:didUpdatePushCredentials:forType:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPushRegistryDelegateOverrides
    case _pushRegistry_didUpdatePushCredentials_forType rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "pushRegistry:didUpdatePushCredentials:forType:" "v@:@@@" stub_0

  -- pushRegistry:didReceiveIncomingPushWithPayload:forType:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPushRegistryDelegateOverrides
    case _pushRegistry_didReceiveIncomingPushWithPayload_forType rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "pushRegistry:didReceiveIncomingPushWithPayload:forType:" "v@:@@@" stub_1

  -- pushRegistry:didInvalidatePushTokenForType:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPushRegistryDelegateOverrides
    case _pushRegistry_didInvalidatePushTokenForType rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "pushRegistry:didInvalidatePushTokenForType:" "v@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPushRegistryDelegateOverrides
    if queriedSel == sel_pushRegistry_didUpdatePushCredentials_forType then pure (maybe 0 (const 1) (_pushRegistry_didUpdatePushCredentials_forType rec_))
    else if queriedSel == sel_pushRegistry_didReceiveIncomingPushWithPayload_forType then pure (maybe 0 (const 1) (_pushRegistry_didReceiveIncomingPushWithPayload_forType rec_))
    else if queriedSel == sel_pushRegistry_didInvalidatePushTokenForType then pure (maybe 0 (const 1) (_pushRegistry_didInvalidatePushTokenForType rec_))
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
newPKPushRegistryDelegate :: PKPushRegistryDelegateOverrides -> IO RawId
newPKPushRegistryDelegate overrides = do
  inst <- class_createInstance pkPushRegistryDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
