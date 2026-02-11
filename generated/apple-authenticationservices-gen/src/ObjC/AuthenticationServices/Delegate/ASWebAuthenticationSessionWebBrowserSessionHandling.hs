{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASWebAuthenticationSessionWebBrowserSessionHandling@.
--
-- Usage:
--
-- @
-- delegate <- newASWebAuthenticationSessionWebBrowserSessionHandling defaultASWebAuthenticationSessionWebBrowserSessionHandlingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASWebAuthenticationSessionWebBrowserSessionHandling
  ( ASWebAuthenticationSessionWebBrowserSessionHandlingOverrides(..)
  , defaultASWebAuthenticationSessionWebBrowserSessionHandlingOverrides
  , newASWebAuthenticationSessionWebBrowserSessionHandling
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

-- | Overrides record for @\@protocol ASWebAuthenticationSessionWebBrowserSessionHandling@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASWebAuthenticationSessionWebBrowserSessionHandlingOverrides = ASWebAuthenticationSessionWebBrowserSessionHandlingOverrides
  { _beginHandlingWebAuthenticationSessionRequest :: !(Maybe (RawId -> IO ()))
  , _cancelWebAuthenticationSessionRequest :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultASWebAuthenticationSessionWebBrowserSessionHandlingOverrides :: ASWebAuthenticationSessionWebBrowserSessionHandlingOverrides
defaultASWebAuthenticationSessionWebBrowserSessionHandlingOverrides = ASWebAuthenticationSessionWebBrowserSessionHandlingOverrides
  { _beginHandlingWebAuthenticationSessionRequest = Nothing
  , _cancelWebAuthenticationSessionRequest = Nothing
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
{-# NOINLINE asWebAuthenticationSessionWebBrowserSessionHandlingDelegateClass #-}
asWebAuthenticationSessionWebBrowserSessionHandlingDelegateClass :: Class
asWebAuthenticationSessionWebBrowserSessionHandlingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASWebAuthenticationSessionWebBrowserSessionHandling" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_beginHandlingWebAuthenticationSessionRequest = unSelector (mkSelector "beginHandlingWebAuthenticationSessionRequest:")
      sel_cancelWebAuthenticationSessionRequest = unSelector (mkSelector "cancelWebAuthenticationSessionRequest:")
  -- beginHandlingWebAuthenticationSessionRequest:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASWebAuthenticationSessionWebBrowserSessionHandlingOverrides
    case _beginHandlingWebAuthenticationSessionRequest rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "beginHandlingWebAuthenticationSessionRequest:" "v@:@" stub_0

  -- cancelWebAuthenticationSessionRequest:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASWebAuthenticationSessionWebBrowserSessionHandlingOverrides
    case _cancelWebAuthenticationSessionRequest rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "cancelWebAuthenticationSessionRequest:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASWebAuthenticationSessionWebBrowserSessionHandlingOverrides
    if queriedSel == sel_beginHandlingWebAuthenticationSessionRequest then pure (maybe 0 (const 1) (_beginHandlingWebAuthenticationSessionRequest rec_))
    else if queriedSel == sel_cancelWebAuthenticationSessionRequest then pure (maybe 0 (const 1) (_cancelWebAuthenticationSessionRequest rec_))
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
newASWebAuthenticationSessionWebBrowserSessionHandling :: ASWebAuthenticationSessionWebBrowserSessionHandlingOverrides -> IO RawId
newASWebAuthenticationSessionWebBrowserSessionHandling overrides = do
  inst <- class_createInstance asWebAuthenticationSessionWebBrowserSessionHandlingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
