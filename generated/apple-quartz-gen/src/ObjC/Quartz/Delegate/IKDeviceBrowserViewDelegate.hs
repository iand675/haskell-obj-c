{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IKDeviceBrowserViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newIKDeviceBrowserViewDelegate defaultIKDeviceBrowserViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Quartz.Delegate.IKDeviceBrowserViewDelegate
  ( IKDeviceBrowserViewDelegateOverrides(..)
  , defaultIKDeviceBrowserViewDelegateOverrides
  , newIKDeviceBrowserViewDelegate
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

-- | Overrides record for @\@protocol IKDeviceBrowserViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IKDeviceBrowserViewDelegateOverrides = IKDeviceBrowserViewDelegateOverrides
  { _deviceBrowserView_selectionDidChange :: !(Maybe (RawId -> RawId -> IO ()))
  , _deviceBrowserView_didEncounterError :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultIKDeviceBrowserViewDelegateOverrides :: IKDeviceBrowserViewDelegateOverrides
defaultIKDeviceBrowserViewDelegateOverrides = IKDeviceBrowserViewDelegateOverrides
  { _deviceBrowserView_selectionDidChange = Nothing
  , _deviceBrowserView_didEncounterError = Nothing
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
{-# NOINLINE ikDeviceBrowserViewDelegateDelegateClass #-}
ikDeviceBrowserViewDelegateDelegateClass :: Class
ikDeviceBrowserViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIKDeviceBrowserViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_deviceBrowserView_selectionDidChange = unSelector (mkSelector "deviceBrowserView:selectionDidChange:")
      sel_deviceBrowserView_didEncounterError = unSelector (mkSelector "deviceBrowserView:didEncounterError:")
  -- deviceBrowserView:selectionDidChange:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKDeviceBrowserViewDelegateOverrides
    case _deviceBrowserView_selectionDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "deviceBrowserView:selectionDidChange:" "v@:@@" stub_0

  -- deviceBrowserView:didEncounterError:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKDeviceBrowserViewDelegateOverrides
    case _deviceBrowserView_didEncounterError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "deviceBrowserView:didEncounterError:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKDeviceBrowserViewDelegateOverrides
    if queriedSel == sel_deviceBrowserView_selectionDidChange then pure (maybe 0 (const 1) (_deviceBrowserView_selectionDidChange rec_))
    else if queriedSel == sel_deviceBrowserView_didEncounterError then pure (maybe 0 (const 1) (_deviceBrowserView_didEncounterError rec_))
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
newIKDeviceBrowserViewDelegate :: IKDeviceBrowserViewDelegateOverrides -> IO RawId
newIKDeviceBrowserViewDelegate overrides = do
  inst <- class_createInstance ikDeviceBrowserViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
