{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol INGetRideStatusIntentHandling@.
--
-- Usage:
--
-- @
-- delegate <- newINGetRideStatusIntentHandling defaultINGetRideStatusIntentHandlingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Intents.Delegate.INGetRideStatusIntentHandling
  ( INGetRideStatusIntentHandlingOverrides(..)
  , defaultINGetRideStatusIntentHandlingOverrides
  , newINGetRideStatusIntentHandling
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

-- | Overrides record for @\@protocol INGetRideStatusIntentHandling@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data INGetRideStatusIntentHandlingOverrides = INGetRideStatusIntentHandlingOverrides
  { _startSendingUpdatesForGetRideStatus_toObserver :: !(Maybe (RawId -> RawId -> IO ()))
  , _stopSendingUpdatesForGetRideStatus :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultINGetRideStatusIntentHandlingOverrides :: INGetRideStatusIntentHandlingOverrides
defaultINGetRideStatusIntentHandlingOverrides = INGetRideStatusIntentHandlingOverrides
  { _startSendingUpdatesForGetRideStatus_toObserver = Nothing
  , _stopSendingUpdatesForGetRideStatus = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE inGetRideStatusIntentHandlingDelegateClass #-}
inGetRideStatusIntentHandlingDelegateClass :: Class
inGetRideStatusIntentHandlingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsINGetRideStatusIntentHandling" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_startSendingUpdatesForGetRideStatus_toObserver = unSelector (mkSelector "startSendingUpdatesForGetRideStatus:toObserver:")
      sel_stopSendingUpdatesForGetRideStatus = unSelector (mkSelector "stopSendingUpdatesForGetRideStatus:")
  -- startSendingUpdatesForGetRideStatus:toObserver:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INGetRideStatusIntentHandlingOverrides
    case _startSendingUpdatesForGetRideStatus_toObserver rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "startSendingUpdatesForGetRideStatus:toObserver:" "v@:@@" stub_0

  -- stopSendingUpdatesForGetRideStatus:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INGetRideStatusIntentHandlingOverrides
    case _stopSendingUpdatesForGetRideStatus rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "stopSendingUpdatesForGetRideStatus:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INGetRideStatusIntentHandlingOverrides
    if queriedSel == sel_startSendingUpdatesForGetRideStatus_toObserver then pure (maybe 0 (const 1) (_startSendingUpdatesForGetRideStatus_toObserver rec_))
    else if queriedSel == sel_stopSendingUpdatesForGetRideStatus then pure (maybe 0 (const 1) (_stopSendingUpdatesForGetRideStatus rec_))
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
newINGetRideStatusIntentHandling :: INGetRideStatusIntentHandlingOverrides -> IO RawId
newINGetRideStatusIntentHandling overrides = do
  inst <- class_createInstance inGetRideStatusIntentHandlingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
