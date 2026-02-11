{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol EAWiFiUnconfiguredAccessoryBrowserDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newEAWiFiUnconfiguredAccessoryBrowserDelegate defaultEAWiFiUnconfiguredAccessoryBrowserDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ExternalAccessory.Delegate.EAWiFiUnconfiguredAccessoryBrowserDelegate
  ( EAWiFiUnconfiguredAccessoryBrowserDelegateOverrides(..)
  , defaultEAWiFiUnconfiguredAccessoryBrowserDelegateOverrides
  , newEAWiFiUnconfiguredAccessoryBrowserDelegate
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

-- | Overrides record for @\@protocol EAWiFiUnconfiguredAccessoryBrowserDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data EAWiFiUnconfiguredAccessoryBrowserDelegateOverrides = EAWiFiUnconfiguredAccessoryBrowserDelegateOverrides
  { _accessoryBrowser_didFindUnconfiguredAccessories :: !(Maybe (RawId -> RawId -> IO ()))
  , _accessoryBrowser_didRemoveUnconfiguredAccessories :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultEAWiFiUnconfiguredAccessoryBrowserDelegateOverrides :: EAWiFiUnconfiguredAccessoryBrowserDelegateOverrides
defaultEAWiFiUnconfiguredAccessoryBrowserDelegateOverrides = EAWiFiUnconfiguredAccessoryBrowserDelegateOverrides
  { _accessoryBrowser_didFindUnconfiguredAccessories = Nothing
  , _accessoryBrowser_didRemoveUnconfiguredAccessories = Nothing
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
{-# NOINLINE eaWiFiUnconfiguredAccessoryBrowserDelegateDelegateClass #-}
eaWiFiUnconfiguredAccessoryBrowserDelegateDelegateClass :: Class
eaWiFiUnconfiguredAccessoryBrowserDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsEAWiFiUnconfiguredAccessoryBrowserDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accessoryBrowser_didFindUnconfiguredAccessories = unSelector (mkSelector "accessoryBrowser:didFindUnconfiguredAccessories:")
      sel_accessoryBrowser_didRemoveUnconfiguredAccessories = unSelector (mkSelector "accessoryBrowser:didRemoveUnconfiguredAccessories:")
  -- accessoryBrowser:didFindUnconfiguredAccessories:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO EAWiFiUnconfiguredAccessoryBrowserDelegateOverrides
    case _accessoryBrowser_didFindUnconfiguredAccessories rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "accessoryBrowser:didFindUnconfiguredAccessories:" "v@:@@" stub_0

  -- accessoryBrowser:didRemoveUnconfiguredAccessories:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO EAWiFiUnconfiguredAccessoryBrowserDelegateOverrides
    case _accessoryBrowser_didRemoveUnconfiguredAccessories rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "accessoryBrowser:didRemoveUnconfiguredAccessories:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO EAWiFiUnconfiguredAccessoryBrowserDelegateOverrides
    if queriedSel == sel_accessoryBrowser_didFindUnconfiguredAccessories then pure (maybe 0 (const 1) (_accessoryBrowser_didFindUnconfiguredAccessories rec_))
    else if queriedSel == sel_accessoryBrowser_didRemoveUnconfiguredAccessories then pure (maybe 0 (const 1) (_accessoryBrowser_didRemoveUnconfiguredAccessories rec_))
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
newEAWiFiUnconfiguredAccessoryBrowserDelegate :: EAWiFiUnconfiguredAccessoryBrowserDelegateOverrides -> IO RawId
newEAWiFiUnconfiguredAccessoryBrowserDelegate overrides = do
  inst <- class_createInstance eaWiFiUnconfiguredAccessoryBrowserDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
