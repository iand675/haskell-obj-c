{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ICScannerDeviceDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newICScannerDeviceDelegate defaultICScannerDeviceDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ImageCaptureCore.Delegate.ICScannerDeviceDelegate
  ( ICScannerDeviceDelegateOverrides(..)
  , defaultICScannerDeviceDelegateOverrides
  , newICScannerDeviceDelegate
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

-- | Overrides record for @\@protocol ICScannerDeviceDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ICScannerDeviceDelegateOverrides = ICScannerDeviceDelegateOverrides
  { _scannerDeviceDidBecomeAvailable :: !(Maybe (RawId -> IO ()))
  , _scannerDevice_didSelectFunctionalUnit_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _scannerDevice_didScanToURL_data :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _scannerDevice_didScanToURL :: !(Maybe (RawId -> RawId -> IO ()))
  , _scannerDevice_didScanToBandData :: !(Maybe (RawId -> RawId -> IO ()))
  , _scannerDevice_didCompleteOverviewScanWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _scannerDevice_didCompleteScanWithError :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultICScannerDeviceDelegateOverrides :: ICScannerDeviceDelegateOverrides
defaultICScannerDeviceDelegateOverrides = ICScannerDeviceDelegateOverrides
  { _scannerDeviceDidBecomeAvailable = Nothing
  , _scannerDevice_didSelectFunctionalUnit_error = Nothing
  , _scannerDevice_didScanToURL_data = Nothing
  , _scannerDevice_didScanToURL = Nothing
  , _scannerDevice_didScanToBandData = Nothing
  , _scannerDevice_didCompleteOverviewScanWithError = Nothing
  , _scannerDevice_didCompleteScanWithError = Nothing
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
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE icScannerDeviceDelegateDelegateClass #-}
icScannerDeviceDelegateDelegateClass :: Class
icScannerDeviceDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsICScannerDeviceDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_scannerDeviceDidBecomeAvailable = unSelector (mkSelector "scannerDeviceDidBecomeAvailable:")
      sel_scannerDevice_didSelectFunctionalUnit_error = unSelector (mkSelector "scannerDevice:didSelectFunctionalUnit:error:")
      sel_scannerDevice_didScanToURL_data = unSelector (mkSelector "scannerDevice:didScanToURL:data:")
      sel_scannerDevice_didScanToURL = unSelector (mkSelector "scannerDevice:didScanToURL:")
      sel_scannerDevice_didScanToBandData = unSelector (mkSelector "scannerDevice:didScanToBandData:")
      sel_scannerDevice_didCompleteOverviewScanWithError = unSelector (mkSelector "scannerDevice:didCompleteOverviewScanWithError:")
      sel_scannerDevice_didCompleteScanWithError = unSelector (mkSelector "scannerDevice:didCompleteScanWithError:")
  -- scannerDeviceDidBecomeAvailable:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICScannerDeviceDelegateOverrides
    case _scannerDeviceDidBecomeAvailable rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "scannerDeviceDidBecomeAvailable:" "v@:@" stub_0

  -- scannerDevice:didSelectFunctionalUnit:error:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICScannerDeviceDelegateOverrides
    case _scannerDevice_didSelectFunctionalUnit_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "scannerDevice:didSelectFunctionalUnit:error:" "v@:@@@" stub_1

  -- scannerDevice:didScanToURL:data:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICScannerDeviceDelegateOverrides
    case _scannerDevice_didScanToURL_data rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "scannerDevice:didScanToURL:data:" "v@:@@@" stub_2

  -- scannerDevice:didScanToURL:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICScannerDeviceDelegateOverrides
    case _scannerDevice_didScanToURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "scannerDevice:didScanToURL:" "v@:@@" stub_3

  -- scannerDevice:didScanToBandData:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICScannerDeviceDelegateOverrides
    case _scannerDevice_didScanToBandData rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "scannerDevice:didScanToBandData:" "v@:@@" stub_4

  -- scannerDevice:didCompleteOverviewScanWithError:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICScannerDeviceDelegateOverrides
    case _scannerDevice_didCompleteOverviewScanWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "scannerDevice:didCompleteOverviewScanWithError:" "v@:@@" stub_5

  -- scannerDevice:didCompleteScanWithError:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICScannerDeviceDelegateOverrides
    case _scannerDevice_didCompleteScanWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "scannerDevice:didCompleteScanWithError:" "v@:@@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICScannerDeviceDelegateOverrides
    if queriedSel == sel_scannerDeviceDidBecomeAvailable then pure (maybe 0 (const 1) (_scannerDeviceDidBecomeAvailable rec_))
    else if queriedSel == sel_scannerDevice_didSelectFunctionalUnit_error then pure (maybe 0 (const 1) (_scannerDevice_didSelectFunctionalUnit_error rec_))
    else if queriedSel == sel_scannerDevice_didScanToURL_data then pure (maybe 0 (const 1) (_scannerDevice_didScanToURL_data rec_))
    else if queriedSel == sel_scannerDevice_didScanToURL then pure (maybe 0 (const 1) (_scannerDevice_didScanToURL rec_))
    else if queriedSel == sel_scannerDevice_didScanToBandData then pure (maybe 0 (const 1) (_scannerDevice_didScanToBandData rec_))
    else if queriedSel == sel_scannerDevice_didCompleteOverviewScanWithError then pure (maybe 0 (const 1) (_scannerDevice_didCompleteOverviewScanWithError rec_))
    else if queriedSel == sel_scannerDevice_didCompleteScanWithError then pure (maybe 0 (const 1) (_scannerDevice_didCompleteScanWithError rec_))
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
newICScannerDeviceDelegate :: ICScannerDeviceDelegateOverrides -> IO RawId
newICScannerDeviceDelegate overrides = do
  inst <- class_createInstance icScannerDeviceDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
