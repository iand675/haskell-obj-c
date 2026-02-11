{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CWEventDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newCWEventDelegate defaultCWEventDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreWLAN.Delegate.CWEventDelegate
  ( CWEventDelegateOverrides(..)
  , defaultCWEventDelegateOverrides
  , newCWEventDelegate
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

-- | Overrides record for @\@protocol CWEventDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CWEventDelegateOverrides = CWEventDelegateOverrides
  { _clientConnectionInterrupted :: !(Maybe (IO ()))
  , _clientConnectionInvalidated :: !(Maybe (IO ()))
  , _powerStateDidChangeForWiFiInterfaceWithName :: !(Maybe (RawId -> IO ()))
  , _ssidDidChangeForWiFiInterfaceWithName :: !(Maybe (RawId -> IO ()))
  , _bssidDidChangeForWiFiInterfaceWithName :: !(Maybe (RawId -> IO ()))
  , _countryCodeDidChangeForWiFiInterfaceWithName :: !(Maybe (RawId -> IO ()))
  , _linkDidChangeForWiFiInterfaceWithName :: !(Maybe (RawId -> IO ()))
  , _linkQualityDidChangeForWiFiInterfaceWithName_rssi_transmitRate :: !(Maybe (RawId -> Int -> Double -> IO ()))
  , _modeDidChangeForWiFiInterfaceWithName :: !(Maybe (RawId -> IO ()))
  , _scanCacheUpdatedForWiFiInterfaceWithName :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCWEventDelegateOverrides :: CWEventDelegateOverrides
defaultCWEventDelegateOverrides = CWEventDelegateOverrides
  { _clientConnectionInterrupted = Nothing
  , _clientConnectionInvalidated = Nothing
  , _powerStateDidChangeForWiFiInterfaceWithName = Nothing
  , _ssidDidChangeForWiFiInterfaceWithName = Nothing
  , _bssidDidChangeForWiFiInterfaceWithName = Nothing
  , _countryCodeDidChangeForWiFiInterfaceWithName = Nothing
  , _linkDidChangeForWiFiInterfaceWithName = Nothing
  , _linkQualityDidChangeForWiFiInterfaceWithName_rssi_transmitRate = Nothing
  , _modeDidChangeForWiFiInterfaceWithName = Nothing
  , _scanCacheUpdatedForWiFiInterfaceWithName = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_q_d_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CDouble -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CDouble -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE cwEventDelegateDelegateClass #-}
cwEventDelegateDelegateClass :: Class
cwEventDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCWEventDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_clientConnectionInterrupted = unSelector (mkSelector "clientConnectionInterrupted")
      sel_clientConnectionInvalidated = unSelector (mkSelector "clientConnectionInvalidated")
      sel_powerStateDidChangeForWiFiInterfaceWithName = unSelector (mkSelector "powerStateDidChangeForWiFiInterfaceWithName:")
      sel_ssidDidChangeForWiFiInterfaceWithName = unSelector (mkSelector "ssidDidChangeForWiFiInterfaceWithName:")
      sel_bssidDidChangeForWiFiInterfaceWithName = unSelector (mkSelector "bssidDidChangeForWiFiInterfaceWithName:")
      sel_countryCodeDidChangeForWiFiInterfaceWithName = unSelector (mkSelector "countryCodeDidChangeForWiFiInterfaceWithName:")
      sel_linkDidChangeForWiFiInterfaceWithName = unSelector (mkSelector "linkDidChangeForWiFiInterfaceWithName:")
      sel_linkQualityDidChangeForWiFiInterfaceWithName_rssi_transmitRate = unSelector (mkSelector "linkQualityDidChangeForWiFiInterfaceWithName:rssi:transmitRate:")
      sel_modeDidChangeForWiFiInterfaceWithName = unSelector (mkSelector "modeDidChangeForWiFiInterfaceWithName:")
      sel_scanCacheUpdatedForWiFiInterfaceWithName = unSelector (mkSelector "scanCacheUpdatedForWiFiInterfaceWithName:")
  -- clientConnectionInterrupted
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CWEventDelegateOverrides
    case _clientConnectionInterrupted rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "clientConnectionInterrupted" "v@:" stub_0

  -- clientConnectionInvalidated
  stub_1 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CWEventDelegateOverrides
    case _clientConnectionInvalidated rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "clientConnectionInvalidated" "v@:" stub_1

  -- powerStateDidChangeForWiFiInterfaceWithName:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CWEventDelegateOverrides
    case _powerStateDidChangeForWiFiInterfaceWithName rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "powerStateDidChangeForWiFiInterfaceWithName:" "v@:@" stub_2

  -- ssidDidChangeForWiFiInterfaceWithName:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CWEventDelegateOverrides
    case _ssidDidChangeForWiFiInterfaceWithName rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "ssidDidChangeForWiFiInterfaceWithName:" "v@:@" stub_3

  -- bssidDidChangeForWiFiInterfaceWithName:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CWEventDelegateOverrides
    case _bssidDidChangeForWiFiInterfaceWithName rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "bssidDidChangeForWiFiInterfaceWithName:" "v@:@" stub_4

  -- countryCodeDidChangeForWiFiInterfaceWithName:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CWEventDelegateOverrides
    case _countryCodeDidChangeForWiFiInterfaceWithName rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "countryCodeDidChangeForWiFiInterfaceWithName:" "v@:@" stub_5

  -- linkDidChangeForWiFiInterfaceWithName:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CWEventDelegateOverrides
    case _linkDidChangeForWiFiInterfaceWithName rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "linkDidChangeForWiFiInterfaceWithName:" "v@:@" stub_6

  -- linkQualityDidChangeForWiFiInterfaceWithName:rssi:transmitRate:
  stub_7 <- wrap_at_q_d_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CWEventDelegateOverrides
    case _linkQualityDidChangeForWiFiInterfaceWithName_rssi_transmitRate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (realToFrac arg2)
  addObjCMethod cls "linkQualityDidChangeForWiFiInterfaceWithName:rssi:transmitRate:" "v@:@qd" stub_7

  -- modeDidChangeForWiFiInterfaceWithName:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CWEventDelegateOverrides
    case _modeDidChangeForWiFiInterfaceWithName rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "modeDidChangeForWiFiInterfaceWithName:" "v@:@" stub_8

  -- scanCacheUpdatedForWiFiInterfaceWithName:
  stub_9 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CWEventDelegateOverrides
    case _scanCacheUpdatedForWiFiInterfaceWithName rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "scanCacheUpdatedForWiFiInterfaceWithName:" "v@:@" stub_9

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CWEventDelegateOverrides
    if queriedSel == sel_clientConnectionInterrupted then pure (maybe 0 (const 1) (_clientConnectionInterrupted rec_))
    else if queriedSel == sel_clientConnectionInvalidated then pure (maybe 0 (const 1) (_clientConnectionInvalidated rec_))
    else if queriedSel == sel_powerStateDidChangeForWiFiInterfaceWithName then pure (maybe 0 (const 1) (_powerStateDidChangeForWiFiInterfaceWithName rec_))
    else if queriedSel == sel_ssidDidChangeForWiFiInterfaceWithName then pure (maybe 0 (const 1) (_ssidDidChangeForWiFiInterfaceWithName rec_))
    else if queriedSel == sel_bssidDidChangeForWiFiInterfaceWithName then pure (maybe 0 (const 1) (_bssidDidChangeForWiFiInterfaceWithName rec_))
    else if queriedSel == sel_countryCodeDidChangeForWiFiInterfaceWithName then pure (maybe 0 (const 1) (_countryCodeDidChangeForWiFiInterfaceWithName rec_))
    else if queriedSel == sel_linkDidChangeForWiFiInterfaceWithName then pure (maybe 0 (const 1) (_linkDidChangeForWiFiInterfaceWithName rec_))
    else if queriedSel == sel_linkQualityDidChangeForWiFiInterfaceWithName_rssi_transmitRate then pure (maybe 0 (const 1) (_linkQualityDidChangeForWiFiInterfaceWithName_rssi_transmitRate rec_))
    else if queriedSel == sel_modeDidChangeForWiFiInterfaceWithName then pure (maybe 0 (const 1) (_modeDidChangeForWiFiInterfaceWithName rec_))
    else if queriedSel == sel_scanCacheUpdatedForWiFiInterfaceWithName then pure (maybe 0 (const 1) (_scanCacheUpdatedForWiFiInterfaceWithName rec_))
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
newCWEventDelegate :: CWEventDelegateOverrides -> IO RawId
newCWEventDelegate overrides = do
  inst <- class_createInstance cwEventDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
