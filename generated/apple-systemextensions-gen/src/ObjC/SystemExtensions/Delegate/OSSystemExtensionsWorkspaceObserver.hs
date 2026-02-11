{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol OSSystemExtensionsWorkspaceObserver@.
--
-- Usage:
--
-- @
-- delegate <- newOSSystemExtensionsWorkspaceObserver defaultOSSystemExtensionsWorkspaceObserverOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SystemExtensions.Delegate.OSSystemExtensionsWorkspaceObserver
  ( OSSystemExtensionsWorkspaceObserverOverrides(..)
  , defaultOSSystemExtensionsWorkspaceObserverOverrides
  , newOSSystemExtensionsWorkspaceObserver
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

-- | Overrides record for @\@protocol OSSystemExtensionsWorkspaceObserver@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data OSSystemExtensionsWorkspaceObserverOverrides = OSSystemExtensionsWorkspaceObserverOverrides
  { _systemExtensionWillBecomeEnabled :: !(Maybe (RawId -> IO ()))
  , _systemExtensionWillBecomeDisabled :: !(Maybe (RawId -> IO ()))
  , _systemExtensionWillBecomeInactive :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultOSSystemExtensionsWorkspaceObserverOverrides :: OSSystemExtensionsWorkspaceObserverOverrides
defaultOSSystemExtensionsWorkspaceObserverOverrides = OSSystemExtensionsWorkspaceObserverOverrides
  { _systemExtensionWillBecomeEnabled = Nothing
  , _systemExtensionWillBecomeDisabled = Nothing
  , _systemExtensionWillBecomeInactive = Nothing
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
{-# NOINLINE osSystemExtensionsWorkspaceObserverDelegateClass #-}
osSystemExtensionsWorkspaceObserverDelegateClass :: Class
osSystemExtensionsWorkspaceObserverDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsOSSystemExtensionsWorkspaceObserver" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_systemExtensionWillBecomeEnabled = unSelector (mkSelector "systemExtensionWillBecomeEnabled:")
      sel_systemExtensionWillBecomeDisabled = unSelector (mkSelector "systemExtensionWillBecomeDisabled:")
      sel_systemExtensionWillBecomeInactive = unSelector (mkSelector "systemExtensionWillBecomeInactive:")
  -- systemExtensionWillBecomeEnabled:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSSystemExtensionsWorkspaceObserverOverrides
    case _systemExtensionWillBecomeEnabled rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "systemExtensionWillBecomeEnabled:" "v@:@" stub_0

  -- systemExtensionWillBecomeDisabled:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSSystemExtensionsWorkspaceObserverOverrides
    case _systemExtensionWillBecomeDisabled rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "systemExtensionWillBecomeDisabled:" "v@:@" stub_1

  -- systemExtensionWillBecomeInactive:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSSystemExtensionsWorkspaceObserverOverrides
    case _systemExtensionWillBecomeInactive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "systemExtensionWillBecomeInactive:" "v@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSSystemExtensionsWorkspaceObserverOverrides
    if queriedSel == sel_systemExtensionWillBecomeEnabled then pure (maybe 0 (const 1) (_systemExtensionWillBecomeEnabled rec_))
    else if queriedSel == sel_systemExtensionWillBecomeDisabled then pure (maybe 0 (const 1) (_systemExtensionWillBecomeDisabled rec_))
    else if queriedSel == sel_systemExtensionWillBecomeInactive then pure (maybe 0 (const 1) (_systemExtensionWillBecomeInactive rec_))
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
newOSSystemExtensionsWorkspaceObserver :: OSSystemExtensionsWorkspaceObserverOverrides -> IO RawId
newOSSystemExtensionsWorkspaceObserver overrides = do
  inst <- class_createInstance osSystemExtensionsWorkspaceObserverDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
