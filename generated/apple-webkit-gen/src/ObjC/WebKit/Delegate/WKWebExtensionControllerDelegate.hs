{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol WKWebExtensionControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newWKWebExtensionControllerDelegate defaultWKWebExtensionControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.WebKit.Delegate.WKWebExtensionControllerDelegate
  ( WKWebExtensionControllerDelegateOverrides(..)
  , defaultWKWebExtensionControllerDelegateOverrides
  , newWKWebExtensionControllerDelegate
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

-- | Overrides record for @\@protocol WKWebExtensionControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data WKWebExtensionControllerDelegateOverrides = WKWebExtensionControllerDelegateOverrides
  { _webExtensionController_openWindowsForExtensionContext :: !(Maybe (RawId -> RawId -> IO RawId))
  , _webExtensionController_focusedWindowForExtensionContext :: !(Maybe (RawId -> RawId -> IO RawId))
  , _webExtensionController_openNewWindowUsingConfiguration_forExtensionContext_completionHandler :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _webExtensionController_openNewTabUsingConfiguration_forExtensionContext_completionHandler :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _webExtensionController_promptForPermissions_inTab_forExtensionContext_completionHandler :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> IO ()))
  , _webExtensionController_promptForPermissionToAccessURLs_inTab_forExtensionContext_completionHandler :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> IO ()))
  , _webExtensionController_promptForPermissionMatchPatterns_inTab_forExtensionContext_completionHandler :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> IO ()))
  , _webExtensionController_didUpdateAction_forExtensionContext :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultWKWebExtensionControllerDelegateOverrides :: WKWebExtensionControllerDelegateOverrides
defaultWKWebExtensionControllerDelegateOverrides = WKWebExtensionControllerDelegateOverrides
  { _webExtensionController_openWindowsForExtensionContext = Nothing
  , _webExtensionController_focusedWindowForExtensionContext = Nothing
  , _webExtensionController_openNewWindowUsingConfiguration_forExtensionContext_completionHandler = Nothing
  , _webExtensionController_openNewTabUsingConfiguration_forExtensionContext_completionHandler = Nothing
  , _webExtensionController_promptForPermissions_inTab_forExtensionContext_completionHandler = Nothing
  , _webExtensionController_promptForPermissionToAccessURLs_inTab_forExtensionContext_completionHandler = Nothing
  , _webExtensionController_promptForPermissionMatchPatterns_inTab_forExtensionContext_completionHandler = Nothing
  , _webExtensionController_didUpdateAction_forExtensionContext = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE wkWebExtensionControllerDelegateDelegateClass #-}
wkWebExtensionControllerDelegateDelegateClass :: Class
wkWebExtensionControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsWKWebExtensionControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_webExtensionController_openWindowsForExtensionContext = unSelector (mkSelector "webExtensionController:openWindowsForExtensionContext:")
      sel_webExtensionController_focusedWindowForExtensionContext = unSelector (mkSelector "webExtensionController:focusedWindowForExtensionContext:")
      sel_webExtensionController_openNewWindowUsingConfiguration_forExtensionContext_completionHandler = unSelector (mkSelector "webExtensionController:openNewWindowUsingConfiguration:forExtensionContext:completionHandler:")
      sel_webExtensionController_openNewTabUsingConfiguration_forExtensionContext_completionHandler = unSelector (mkSelector "webExtensionController:openNewTabUsingConfiguration:forExtensionContext:completionHandler:")
      sel_webExtensionController_promptForPermissions_inTab_forExtensionContext_completionHandler = unSelector (mkSelector "webExtensionController:promptForPermissions:inTab:forExtensionContext:completionHandler:")
      sel_webExtensionController_promptForPermissionToAccessURLs_inTab_forExtensionContext_completionHandler = unSelector (mkSelector "webExtensionController:promptForPermissionToAccessURLs:inTab:forExtensionContext:completionHandler:")
      sel_webExtensionController_promptForPermissionMatchPatterns_inTab_forExtensionContext_completionHandler = unSelector (mkSelector "webExtensionController:promptForPermissionMatchPatterns:inTab:forExtensionContext:completionHandler:")
      sel_webExtensionController_didUpdateAction_forExtensionContext = unSelector (mkSelector "webExtensionController:didUpdateAction:forExtensionContext:")
  -- webExtensionController:openWindowsForExtensionContext:
  stub_0 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKWebExtensionControllerDelegateOverrides
    case _webExtensionController_openWindowsForExtensionContext rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "webExtensionController:openWindowsForExtensionContext:" "@@:@@" stub_0

  -- webExtensionController:focusedWindowForExtensionContext:
  stub_1 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKWebExtensionControllerDelegateOverrides
    case _webExtensionController_focusedWindowForExtensionContext rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "webExtensionController:focusedWindowForExtensionContext:" "@@:@@" stub_1

  -- webExtensionController:openNewWindowUsingConfiguration:forExtensionContext:completionHandler:
  stub_2 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKWebExtensionControllerDelegateOverrides
    case _webExtensionController_openNewWindowUsingConfiguration_forExtensionContext_completionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "webExtensionController:openNewWindowUsingConfiguration:forExtensionContext:completionHandler:" "v@:@@@@" stub_2

  -- webExtensionController:openNewTabUsingConfiguration:forExtensionContext:completionHandler:
  stub_3 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKWebExtensionControllerDelegateOverrides
    case _webExtensionController_openNewTabUsingConfiguration_forExtensionContext_completionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "webExtensionController:openNewTabUsingConfiguration:forExtensionContext:completionHandler:" "v@:@@@@" stub_3

  -- webExtensionController:promptForPermissions:inTab:forExtensionContext:completionHandler:
  stub_4 <- wrap_at_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKWebExtensionControllerDelegateOverrides
    case _webExtensionController_promptForPermissions_inTab_forExtensionContext_completionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4)
  addObjCMethod cls "webExtensionController:promptForPermissions:inTab:forExtensionContext:completionHandler:" "v@:@@@@@" stub_4

  -- webExtensionController:promptForPermissionToAccessURLs:inTab:forExtensionContext:completionHandler:
  stub_5 <- wrap_at_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKWebExtensionControllerDelegateOverrides
    case _webExtensionController_promptForPermissionToAccessURLs_inTab_forExtensionContext_completionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4)
  addObjCMethod cls "webExtensionController:promptForPermissionToAccessURLs:inTab:forExtensionContext:completionHandler:" "v@:@@@@@" stub_5

  -- webExtensionController:promptForPermissionMatchPatterns:inTab:forExtensionContext:completionHandler:
  stub_6 <- wrap_at_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKWebExtensionControllerDelegateOverrides
    case _webExtensionController_promptForPermissionMatchPatterns_inTab_forExtensionContext_completionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4)
  addObjCMethod cls "webExtensionController:promptForPermissionMatchPatterns:inTab:forExtensionContext:completionHandler:" "v@:@@@@@" stub_6

  -- webExtensionController:didUpdateAction:forExtensionContext:
  stub_7 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKWebExtensionControllerDelegateOverrides
    case _webExtensionController_didUpdateAction_forExtensionContext rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "webExtensionController:didUpdateAction:forExtensionContext:" "v@:@@@" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKWebExtensionControllerDelegateOverrides
    if queriedSel == sel_webExtensionController_openWindowsForExtensionContext then pure (maybe 0 (const 1) (_webExtensionController_openWindowsForExtensionContext rec_))
    else if queriedSel == sel_webExtensionController_focusedWindowForExtensionContext then pure (maybe 0 (const 1) (_webExtensionController_focusedWindowForExtensionContext rec_))
    else if queriedSel == sel_webExtensionController_openNewWindowUsingConfiguration_forExtensionContext_completionHandler then pure (maybe 0 (const 1) (_webExtensionController_openNewWindowUsingConfiguration_forExtensionContext_completionHandler rec_))
    else if queriedSel == sel_webExtensionController_openNewTabUsingConfiguration_forExtensionContext_completionHandler then pure (maybe 0 (const 1) (_webExtensionController_openNewTabUsingConfiguration_forExtensionContext_completionHandler rec_))
    else if queriedSel == sel_webExtensionController_promptForPermissions_inTab_forExtensionContext_completionHandler then pure (maybe 0 (const 1) (_webExtensionController_promptForPermissions_inTab_forExtensionContext_completionHandler rec_))
    else if queriedSel == sel_webExtensionController_promptForPermissionToAccessURLs_inTab_forExtensionContext_completionHandler then pure (maybe 0 (const 1) (_webExtensionController_promptForPermissionToAccessURLs_inTab_forExtensionContext_completionHandler rec_))
    else if queriedSel == sel_webExtensionController_promptForPermissionMatchPatterns_inTab_forExtensionContext_completionHandler then pure (maybe 0 (const 1) (_webExtensionController_promptForPermissionMatchPatterns_inTab_forExtensionContext_completionHandler rec_))
    else if queriedSel == sel_webExtensionController_didUpdateAction_forExtensionContext then pure (maybe 0 (const 1) (_webExtensionController_didUpdateAction_forExtensionContext rec_))
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
newWKWebExtensionControllerDelegate :: WKWebExtensionControllerDelegateOverrides -> IO RawId
newWKWebExtensionControllerDelegate overrides = do
  inst <- class_createInstance wkWebExtensionControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
