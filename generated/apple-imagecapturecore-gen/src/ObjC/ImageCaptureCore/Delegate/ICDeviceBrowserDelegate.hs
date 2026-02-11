{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ICDeviceBrowserDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newICDeviceBrowserDelegate defaultICDeviceBrowserDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ImageCaptureCore.Delegate.ICDeviceBrowserDelegate
  ( ICDeviceBrowserDelegateOverrides(..)
  , defaultICDeviceBrowserDelegateOverrides
  , newICDeviceBrowserDelegate
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

-- | Overrides record for @\@protocol ICDeviceBrowserDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ICDeviceBrowserDelegateOverrides = ICDeviceBrowserDelegateOverrides
  { _deviceBrowser_didAddDevice_moreComing :: !(Maybe (RawId -> RawId -> Bool -> IO ()))
  , _deviceBrowser_didRemoveDevice_moreGoing :: !(Maybe (RawId -> RawId -> Bool -> IO ()))
  , _deviceBrowser_deviceDidChangeName :: !(Maybe (RawId -> RawId -> IO ()))
  , _deviceBrowser_deviceDidChangeSharingState :: !(Maybe (RawId -> RawId -> IO ()))
  , _deviceBrowser_requestsSelectDevice :: !(Maybe (RawId -> RawId -> IO ()))
  , _deviceBrowserDidEnumerateLocalDevices :: !(Maybe (RawId -> IO ()))
  , _deviceBrowserWillSuspendOperations :: !(Maybe (RawId -> IO ()))
  , _deviceBrowserDidSuspendOperations :: !(Maybe (RawId -> IO ()))
  , _deviceBrowserDidCancelSuspendOperations :: !(Maybe (RawId -> IO ()))
  , _deviceBrowserDidResumeOperations :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultICDeviceBrowserDelegateOverrides :: ICDeviceBrowserDelegateOverrides
defaultICDeviceBrowserDelegateOverrides = ICDeviceBrowserDelegateOverrides
  { _deviceBrowser_didAddDevice_moreComing = Nothing
  , _deviceBrowser_didRemoveDevice_moreGoing = Nothing
  , _deviceBrowser_deviceDidChangeName = Nothing
  , _deviceBrowser_deviceDidChangeSharingState = Nothing
  , _deviceBrowser_requestsSelectDevice = Nothing
  , _deviceBrowserDidEnumerateLocalDevices = Nothing
  , _deviceBrowserWillSuspendOperations = Nothing
  , _deviceBrowserDidSuspendOperations = Nothing
  , _deviceBrowserDidCancelSuspendOperations = Nothing
  , _deviceBrowserDidResumeOperations = Nothing
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
  wrap_at_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE icDeviceBrowserDelegateDelegateClass #-}
icDeviceBrowserDelegateDelegateClass :: Class
icDeviceBrowserDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsICDeviceBrowserDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_deviceBrowser_didAddDevice_moreComing = unSelector (mkSelector "deviceBrowser:didAddDevice:moreComing:")
      sel_deviceBrowser_didRemoveDevice_moreGoing = unSelector (mkSelector "deviceBrowser:didRemoveDevice:moreGoing:")
      sel_deviceBrowser_deviceDidChangeName = unSelector (mkSelector "deviceBrowser:deviceDidChangeName:")
      sel_deviceBrowser_deviceDidChangeSharingState = unSelector (mkSelector "deviceBrowser:deviceDidChangeSharingState:")
      sel_deviceBrowser_requestsSelectDevice = unSelector (mkSelector "deviceBrowser:requestsSelectDevice:")
      sel_deviceBrowserDidEnumerateLocalDevices = unSelector (mkSelector "deviceBrowserDidEnumerateLocalDevices:")
      sel_deviceBrowserWillSuspendOperations = unSelector (mkSelector "deviceBrowserWillSuspendOperations:")
      sel_deviceBrowserDidSuspendOperations = unSelector (mkSelector "deviceBrowserDidSuspendOperations:")
      sel_deviceBrowserDidCancelSuspendOperations = unSelector (mkSelector "deviceBrowserDidCancelSuspendOperations:")
      sel_deviceBrowserDidResumeOperations = unSelector (mkSelector "deviceBrowserDidResumeOperations:")
  -- deviceBrowser:didAddDevice:moreComing:
  stub_0 <- wrap_at_at_B_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICDeviceBrowserDelegateOverrides
    case _deviceBrowser_didAddDevice_moreComing rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (arg2 /= 0)
  addObjCMethod cls "deviceBrowser:didAddDevice:moreComing:" "v@:@@B" stub_0

  -- deviceBrowser:didRemoveDevice:moreGoing:
  stub_1 <- wrap_at_at_B_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICDeviceBrowserDelegateOverrides
    case _deviceBrowser_didRemoveDevice_moreGoing rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (arg2 /= 0)
  addObjCMethod cls "deviceBrowser:didRemoveDevice:moreGoing:" "v@:@@B" stub_1

  -- deviceBrowser:deviceDidChangeName:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICDeviceBrowserDelegateOverrides
    case _deviceBrowser_deviceDidChangeName rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "deviceBrowser:deviceDidChangeName:" "v@:@@" stub_2

  -- deviceBrowser:deviceDidChangeSharingState:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICDeviceBrowserDelegateOverrides
    case _deviceBrowser_deviceDidChangeSharingState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "deviceBrowser:deviceDidChangeSharingState:" "v@:@@" stub_3

  -- deviceBrowser:requestsSelectDevice:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICDeviceBrowserDelegateOverrides
    case _deviceBrowser_requestsSelectDevice rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "deviceBrowser:requestsSelectDevice:" "v@:@@" stub_4

  -- deviceBrowserDidEnumerateLocalDevices:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICDeviceBrowserDelegateOverrides
    case _deviceBrowserDidEnumerateLocalDevices rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deviceBrowserDidEnumerateLocalDevices:" "v@:@" stub_5

  -- deviceBrowserWillSuspendOperations:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICDeviceBrowserDelegateOverrides
    case _deviceBrowserWillSuspendOperations rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deviceBrowserWillSuspendOperations:" "v@:@" stub_6

  -- deviceBrowserDidSuspendOperations:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICDeviceBrowserDelegateOverrides
    case _deviceBrowserDidSuspendOperations rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deviceBrowserDidSuspendOperations:" "v@:@" stub_7

  -- deviceBrowserDidCancelSuspendOperations:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICDeviceBrowserDelegateOverrides
    case _deviceBrowserDidCancelSuspendOperations rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deviceBrowserDidCancelSuspendOperations:" "v@:@" stub_8

  -- deviceBrowserDidResumeOperations:
  stub_9 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICDeviceBrowserDelegateOverrides
    case _deviceBrowserDidResumeOperations rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deviceBrowserDidResumeOperations:" "v@:@" stub_9

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICDeviceBrowserDelegateOverrides
    if queriedSel == sel_deviceBrowser_didAddDevice_moreComing then pure (maybe 0 (const 1) (_deviceBrowser_didAddDevice_moreComing rec_))
    else if queriedSel == sel_deviceBrowser_didRemoveDevice_moreGoing then pure (maybe 0 (const 1) (_deviceBrowser_didRemoveDevice_moreGoing rec_))
    else if queriedSel == sel_deviceBrowser_deviceDidChangeName then pure (maybe 0 (const 1) (_deviceBrowser_deviceDidChangeName rec_))
    else if queriedSel == sel_deviceBrowser_deviceDidChangeSharingState then pure (maybe 0 (const 1) (_deviceBrowser_deviceDidChangeSharingState rec_))
    else if queriedSel == sel_deviceBrowser_requestsSelectDevice then pure (maybe 0 (const 1) (_deviceBrowser_requestsSelectDevice rec_))
    else if queriedSel == sel_deviceBrowserDidEnumerateLocalDevices then pure (maybe 0 (const 1) (_deviceBrowserDidEnumerateLocalDevices rec_))
    else if queriedSel == sel_deviceBrowserWillSuspendOperations then pure (maybe 0 (const 1) (_deviceBrowserWillSuspendOperations rec_))
    else if queriedSel == sel_deviceBrowserDidSuspendOperations then pure (maybe 0 (const 1) (_deviceBrowserDidSuspendOperations rec_))
    else if queriedSel == sel_deviceBrowserDidCancelSuspendOperations then pure (maybe 0 (const 1) (_deviceBrowserDidCancelSuspendOperations rec_))
    else if queriedSel == sel_deviceBrowserDidResumeOperations then pure (maybe 0 (const 1) (_deviceBrowserDidResumeOperations rec_))
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
newICDeviceBrowserDelegate :: ICDeviceBrowserDelegateOverrides -> IO RawId
newICDeviceBrowserDelegate overrides = do
  inst <- class_createInstance icDeviceBrowserDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
