{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTRDevicePairingDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newMTRDevicePairingDelegate defaultMTRDevicePairingDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Matter.Delegate.MTRDevicePairingDelegate
  ( MTRDevicePairingDelegateOverrides(..)
  , defaultMTRDevicePairingDelegateOverrides
  , newMTRDevicePairingDelegate
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

-- | Overrides record for @\@protocol MTRDevicePairingDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTRDevicePairingDelegateOverrides = MTRDevicePairingDelegateOverrides
  { _onPairingComplete :: !(Maybe (RawId -> IO ()))
  , _onCommissioningComplete :: !(Maybe (RawId -> IO ()))
  , _onPairingDeleted :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTRDevicePairingDelegateOverrides :: MTRDevicePairingDelegateOverrides
defaultMTRDevicePairingDelegateOverrides = MTRDevicePairingDelegateOverrides
  { _onPairingComplete = Nothing
  , _onCommissioningComplete = Nothing
  , _onPairingDeleted = Nothing
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
{-# NOINLINE mtrDevicePairingDelegateDelegateClass #-}
mtrDevicePairingDelegateDelegateClass :: Class
mtrDevicePairingDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTRDevicePairingDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_onPairingComplete = unSelector (mkSelector "onPairingComplete:")
      sel_onCommissioningComplete = unSelector (mkSelector "onCommissioningComplete:")
      sel_onPairingDeleted = unSelector (mkSelector "onPairingDeleted:")
  -- onPairingComplete:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDevicePairingDelegateOverrides
    case _onPairingComplete rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "onPairingComplete:" "v@:@" stub_0

  -- onCommissioningComplete:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDevicePairingDelegateOverrides
    case _onCommissioningComplete rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "onCommissioningComplete:" "v@:@" stub_1

  -- onPairingDeleted:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDevicePairingDelegateOverrides
    case _onPairingDeleted rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "onPairingDeleted:" "v@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDevicePairingDelegateOverrides
    if queriedSel == sel_onPairingComplete then pure (maybe 0 (const 1) (_onPairingComplete rec_))
    else if queriedSel == sel_onCommissioningComplete then pure (maybe 0 (const 1) (_onCommissioningComplete rec_))
    else if queriedSel == sel_onPairingDeleted then pure (maybe 0 (const 1) (_onPairingDeleted rec_))
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
newMTRDevicePairingDelegate :: MTRDevicePairingDelegateOverrides -> IO RawId
newMTRDevicePairingDelegate overrides = do
  inst <- class_createInstance mtrDevicePairingDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
