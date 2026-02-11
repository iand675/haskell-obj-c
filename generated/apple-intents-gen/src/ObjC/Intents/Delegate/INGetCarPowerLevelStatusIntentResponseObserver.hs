{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol INGetCarPowerLevelStatusIntentResponseObserver@.
--
-- Usage:
--
-- @
-- delegate <- newINGetCarPowerLevelStatusIntentResponseObserver defaultINGetCarPowerLevelStatusIntentResponseObserverOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Intents.Delegate.INGetCarPowerLevelStatusIntentResponseObserver
  ( INGetCarPowerLevelStatusIntentResponseObserverOverrides(..)
  , defaultINGetCarPowerLevelStatusIntentResponseObserverOverrides
  , newINGetCarPowerLevelStatusIntentResponseObserver
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

-- | Overrides record for @\@protocol INGetCarPowerLevelStatusIntentResponseObserver@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data INGetCarPowerLevelStatusIntentResponseObserverOverrides = INGetCarPowerLevelStatusIntentResponseObserverOverrides
  { _getCarPowerLevelStatusResponseDidUpdate :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultINGetCarPowerLevelStatusIntentResponseObserverOverrides :: INGetCarPowerLevelStatusIntentResponseObserverOverrides
defaultINGetCarPowerLevelStatusIntentResponseObserverOverrides = INGetCarPowerLevelStatusIntentResponseObserverOverrides
  { _getCarPowerLevelStatusResponseDidUpdate = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE inGetCarPowerLevelStatusIntentResponseObserverDelegateClass #-}
inGetCarPowerLevelStatusIntentResponseObserverDelegateClass :: Class
inGetCarPowerLevelStatusIntentResponseObserverDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsINGetCarPowerLevelStatusIntentResponseObserver" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_getCarPowerLevelStatusResponseDidUpdate = unSelector (mkSelector "getCarPowerLevelStatusResponseDidUpdate:")
  -- getCarPowerLevelStatusResponseDidUpdate:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INGetCarPowerLevelStatusIntentResponseObserverOverrides
    case _getCarPowerLevelStatusResponseDidUpdate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "getCarPowerLevelStatusResponseDidUpdate:" "v@:@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INGetCarPowerLevelStatusIntentResponseObserverOverrides
    if queriedSel == sel_getCarPowerLevelStatusResponseDidUpdate then pure (maybe 0 (const 1) (_getCarPowerLevelStatusResponseDidUpdate rec_))
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
newINGetCarPowerLevelStatusIntentResponseObserver :: INGetCarPowerLevelStatusIntentResponseObserverOverrides -> IO RawId
newINGetCarPowerLevelStatusIntentResponseObserver overrides = do
  inst <- class_createInstance inGetCarPowerLevelStatusIntentResponseObserverDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
