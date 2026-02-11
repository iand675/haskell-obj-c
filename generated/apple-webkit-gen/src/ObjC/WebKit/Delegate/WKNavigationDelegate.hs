{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol WKNavigationDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newWKNavigationDelegate defaultWKNavigationDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.WebKit.Delegate.WKNavigationDelegate
  ( WKNavigationDelegateOverrides(..)
  , defaultWKNavigationDelegateOverrides
  , newWKNavigationDelegate
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

-- | Overrides record for @\@protocol WKNavigationDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data WKNavigationDelegateOverrides = WKNavigationDelegateOverrides
  { _webView_didStartProvisionalNavigation :: !(Maybe (RawId -> RawId -> IO ()))
  , _webView_didReceiveServerRedirectForProvisionalNavigation :: !(Maybe (RawId -> RawId -> IO ()))
  , _webView_didFailProvisionalNavigation_withError :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _webView_didCommitNavigation :: !(Maybe (RawId -> RawId -> IO ()))
  , _webView_didFinishNavigation :: !(Maybe (RawId -> RawId -> IO ()))
  , _webView_didFailNavigation_withError :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _webViewWebContentProcessDidTerminate :: !(Maybe (RawId -> IO ()))
  , _webView_navigationAction_didBecomeDownload :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _webView_navigationResponse_didBecomeDownload :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultWKNavigationDelegateOverrides :: WKNavigationDelegateOverrides
defaultWKNavigationDelegateOverrides = WKNavigationDelegateOverrides
  { _webView_didStartProvisionalNavigation = Nothing
  , _webView_didReceiveServerRedirectForProvisionalNavigation = Nothing
  , _webView_didFailProvisionalNavigation_withError = Nothing
  , _webView_didCommitNavigation = Nothing
  , _webView_didFinishNavigation = Nothing
  , _webView_didFailNavigation_withError = Nothing
  , _webViewWebContentProcessDidTerminate = Nothing
  , _webView_navigationAction_didBecomeDownload = Nothing
  , _webView_navigationResponse_didBecomeDownload = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE wkNavigationDelegateDelegateClass #-}
wkNavigationDelegateDelegateClass :: Class
wkNavigationDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsWKNavigationDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_webView_didStartProvisionalNavigation = unSelector (mkSelector "webView:didStartProvisionalNavigation:")
      sel_webView_didReceiveServerRedirectForProvisionalNavigation = unSelector (mkSelector "webView:didReceiveServerRedirectForProvisionalNavigation:")
      sel_webView_didFailProvisionalNavigation_withError = unSelector (mkSelector "webView:didFailProvisionalNavigation:withError:")
      sel_webView_didCommitNavigation = unSelector (mkSelector "webView:didCommitNavigation:")
      sel_webView_didFinishNavigation = unSelector (mkSelector "webView:didFinishNavigation:")
      sel_webView_didFailNavigation_withError = unSelector (mkSelector "webView:didFailNavigation:withError:")
      sel_webViewWebContentProcessDidTerminate = unSelector (mkSelector "webViewWebContentProcessDidTerminate:")
      sel_webView_navigationAction_didBecomeDownload = unSelector (mkSelector "webView:navigationAction:didBecomeDownload:")
      sel_webView_navigationResponse_didBecomeDownload = unSelector (mkSelector "webView:navigationResponse:didBecomeDownload:")
  -- webView:didStartProvisionalNavigation:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKNavigationDelegateOverrides
    case _webView_didStartProvisionalNavigation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "webView:didStartProvisionalNavigation:" "v@:@@" stub_0

  -- webView:didReceiveServerRedirectForProvisionalNavigation:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKNavigationDelegateOverrides
    case _webView_didReceiveServerRedirectForProvisionalNavigation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "webView:didReceiveServerRedirectForProvisionalNavigation:" "v@:@@" stub_1

  -- webView:didFailProvisionalNavigation:withError:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKNavigationDelegateOverrides
    case _webView_didFailProvisionalNavigation_withError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "webView:didFailProvisionalNavigation:withError:" "v@:@@@" stub_2

  -- webView:didCommitNavigation:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKNavigationDelegateOverrides
    case _webView_didCommitNavigation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "webView:didCommitNavigation:" "v@:@@" stub_3

  -- webView:didFinishNavigation:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKNavigationDelegateOverrides
    case _webView_didFinishNavigation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "webView:didFinishNavigation:" "v@:@@" stub_4

  -- webView:didFailNavigation:withError:
  stub_5 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKNavigationDelegateOverrides
    case _webView_didFailNavigation_withError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "webView:didFailNavigation:withError:" "v@:@@@" stub_5

  -- webViewWebContentProcessDidTerminate:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKNavigationDelegateOverrides
    case _webViewWebContentProcessDidTerminate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "webViewWebContentProcessDidTerminate:" "v@:@" stub_6

  -- webView:navigationAction:didBecomeDownload:
  stub_7 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKNavigationDelegateOverrides
    case _webView_navigationAction_didBecomeDownload rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "webView:navigationAction:didBecomeDownload:" "v@:@@@" stub_7

  -- webView:navigationResponse:didBecomeDownload:
  stub_8 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKNavigationDelegateOverrides
    case _webView_navigationResponse_didBecomeDownload rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "webView:navigationResponse:didBecomeDownload:" "v@:@@@" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKNavigationDelegateOverrides
    if queriedSel == sel_webView_didStartProvisionalNavigation then pure (maybe 0 (const 1) (_webView_didStartProvisionalNavigation rec_))
    else if queriedSel == sel_webView_didReceiveServerRedirectForProvisionalNavigation then pure (maybe 0 (const 1) (_webView_didReceiveServerRedirectForProvisionalNavigation rec_))
    else if queriedSel == sel_webView_didFailProvisionalNavigation_withError then pure (maybe 0 (const 1) (_webView_didFailProvisionalNavigation_withError rec_))
    else if queriedSel == sel_webView_didCommitNavigation then pure (maybe 0 (const 1) (_webView_didCommitNavigation rec_))
    else if queriedSel == sel_webView_didFinishNavigation then pure (maybe 0 (const 1) (_webView_didFinishNavigation rec_))
    else if queriedSel == sel_webView_didFailNavigation_withError then pure (maybe 0 (const 1) (_webView_didFailNavigation_withError rec_))
    else if queriedSel == sel_webViewWebContentProcessDidTerminate then pure (maybe 0 (const 1) (_webViewWebContentProcessDidTerminate rec_))
    else if queriedSel == sel_webView_navigationAction_didBecomeDownload then pure (maybe 0 (const 1) (_webView_navigationAction_didBecomeDownload rec_))
    else if queriedSel == sel_webView_navigationResponse_didBecomeDownload then pure (maybe 0 (const 1) (_webView_navigationResponse_didBecomeDownload rec_))
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
newWKNavigationDelegate :: WKNavigationDelegateOverrides -> IO RawId
newWKNavigationDelegate overrides = do
  inst <- class_createInstance wkNavigationDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
