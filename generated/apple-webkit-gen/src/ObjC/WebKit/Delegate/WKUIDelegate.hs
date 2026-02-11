{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol WKUIDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newWKUIDelegate defaultWKUIDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.WebKit.Delegate.WKUIDelegate
  ( WKUIDelegateOverrides(..)
  , defaultWKUIDelegateOverrides
  , newWKUIDelegate
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

-- | Overrides record for @\@protocol WKUIDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data WKUIDelegateOverrides = WKUIDelegateOverrides
  { _webView_createWebViewWithConfiguration_forNavigationAction_windowFeatures :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO RawId))
  , _webViewDidClose :: !(Maybe (RawId -> IO ()))
  , _webView_runOpenPanelWithParameters_initiatedByFrame_completionHandler :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultWKUIDelegateOverrides :: WKUIDelegateOverrides
defaultWKUIDelegateOverrides = WKUIDelegateOverrides
  { _webView_createWebViewWithConfiguration_forNavigationAction_windowFeatures = Nothing
  , _webViewDidClose = Nothing
  , _webView_runOpenPanelWithParameters_initiatedByFrame_completionHandler = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE wkuiDelegateDelegateClass #-}
wkuiDelegateDelegateClass :: Class
wkuiDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsWKUIDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_webView_createWebViewWithConfiguration_forNavigationAction_windowFeatures = unSelector (mkSelector "webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:")
      sel_webViewDidClose = unSelector (mkSelector "webViewDidClose:")
      sel_webView_runOpenPanelWithParameters_initiatedByFrame_completionHandler = unSelector (mkSelector "webView:runOpenPanelWithParameters:initiatedByFrame:completionHandler:")
  -- webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:
  stub_0 <- wrap_at_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKUIDelegateOverrides
    case _webView_createWebViewWithConfiguration_forNavigationAction_windowFeatures rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:" "@@:@@@@" stub_0

  -- webViewDidClose:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKUIDelegateOverrides
    case _webViewDidClose rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "webViewDidClose:" "v@:@" stub_1

  -- webView:runOpenPanelWithParameters:initiatedByFrame:completionHandler:
  stub_2 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKUIDelegateOverrides
    case _webView_runOpenPanelWithParameters_initiatedByFrame_completionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "webView:runOpenPanelWithParameters:initiatedByFrame:completionHandler:" "v@:@@@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKUIDelegateOverrides
    if queriedSel == sel_webView_createWebViewWithConfiguration_forNavigationAction_windowFeatures then pure (maybe 0 (const 1) (_webView_createWebViewWithConfiguration_forNavigationAction_windowFeatures rec_))
    else if queriedSel == sel_webViewDidClose then pure (maybe 0 (const 1) (_webViewDidClose rec_))
    else if queriedSel == sel_webView_runOpenPanelWithParameters_initiatedByFrame_completionHandler then pure (maybe 0 (const 1) (_webView_runOpenPanelWithParameters_initiatedByFrame_completionHandler rec_))
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
newWKUIDelegate :: WKUIDelegateOverrides -> IO RawId
newWKUIDelegate overrides = do
  inst <- class_createInstance wkuiDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
