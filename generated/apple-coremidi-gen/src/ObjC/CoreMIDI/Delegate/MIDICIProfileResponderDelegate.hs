{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MIDICIProfileResponderDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newMIDICIProfileResponderDelegate defaultMIDICIProfileResponderDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreMIDI.Delegate.MIDICIProfileResponderDelegate
  ( MIDICIProfileResponderDelegateOverrides(..)
  , defaultMIDICIProfileResponderDelegateOverrides
  , newMIDICIProfileResponderDelegate
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

-- | Overrides record for @\@protocol MIDICIProfileResponderDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MIDICIProfileResponderDelegateOverrides = MIDICIProfileResponderDelegateOverrides
  { _connectInitiator_withDeviceInfo :: !(Maybe (RawId -> RawId -> IO Bool))
  , _initiatorDisconnected :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMIDICIProfileResponderDelegateOverrides :: MIDICIProfileResponderDelegateOverrides
defaultMIDICIProfileResponderDelegateOverrides = MIDICIProfileResponderDelegateOverrides
  { _connectInitiator_withDeviceInfo = Nothing
  , _initiatorDisconnected = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE midiciProfileResponderDelegateDelegateClass #-}
midiciProfileResponderDelegateDelegateClass :: Class
midiciProfileResponderDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMIDICIProfileResponderDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_connectInitiator_withDeviceInfo = unSelector (mkSelector "connectInitiator:withDeviceInfo:")
      sel_initiatorDisconnected = unSelector (mkSelector "initiatorDisconnected:")
  -- connectInitiator:withDeviceInfo:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MIDICIProfileResponderDelegateOverrides
    case _connectInitiator_withDeviceInfo rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "connectInitiator:withDeviceInfo:" "B@:@@" stub_0

  -- initiatorDisconnected:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MIDICIProfileResponderDelegateOverrides
    case _initiatorDisconnected rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "initiatorDisconnected:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MIDICIProfileResponderDelegateOverrides
    if queriedSel == sel_connectInitiator_withDeviceInfo then pure (maybe 0 (const 1) (_connectInitiator_withDeviceInfo rec_))
    else if queriedSel == sel_initiatorDisconnected then pure (maybe 0 (const 1) (_initiatorDisconnected rec_))
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
newMIDICIProfileResponderDelegate :: MIDICIProfileResponderDelegateOverrides -> IO RawId
newMIDICIProfileResponderDelegate overrides = do
  inst <- class_createInstance midiciProfileResponderDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
