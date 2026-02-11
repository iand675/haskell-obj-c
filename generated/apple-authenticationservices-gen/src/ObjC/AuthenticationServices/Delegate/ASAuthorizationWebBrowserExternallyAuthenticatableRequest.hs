{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASAuthorizationWebBrowserExternallyAuthenticatableRequest@.
--
-- Usage:
--
-- @
-- delegate <- newASAuthorizationWebBrowserExternallyAuthenticatableRequest defaultASAuthorizationWebBrowserExternallyAuthenticatableRequestOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASAuthorizationWebBrowserExternallyAuthenticatableRequest
  ( ASAuthorizationWebBrowserExternallyAuthenticatableRequestOverrides(..)
  , defaultASAuthorizationWebBrowserExternallyAuthenticatableRequestOverrides
  , newASAuthorizationWebBrowserExternallyAuthenticatableRequest
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

-- | Overrides record for @\@protocol ASAuthorizationWebBrowserExternallyAuthenticatableRequest@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASAuthorizationWebBrowserExternallyAuthenticatableRequestOverrides = ASAuthorizationWebBrowserExternallyAuthenticatableRequestOverrides
  { _authenticatedContext :: !(Maybe (IO RawId))
  , _setAuthenticatedContext :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultASAuthorizationWebBrowserExternallyAuthenticatableRequestOverrides :: ASAuthorizationWebBrowserExternallyAuthenticatableRequestOverrides
defaultASAuthorizationWebBrowserExternallyAuthenticatableRequestOverrides = ASAuthorizationWebBrowserExternallyAuthenticatableRequestOverrides
  { _authenticatedContext = Nothing
  , _setAuthenticatedContext = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE asAuthorizationWebBrowserExternallyAuthenticatableRequestDelegateClass #-}
asAuthorizationWebBrowserExternallyAuthenticatableRequestDelegateClass :: Class
asAuthorizationWebBrowserExternallyAuthenticatableRequestDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASAuthorizationWebBrowserExternallyAuthenticatableRequest" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_authenticatedContext = unSelector (mkSelector "authenticatedContext")
      sel_setAuthenticatedContext = unSelector (mkSelector "setAuthenticatedContext:")
  -- authenticatedContext
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationWebBrowserExternallyAuthenticatableRequestOverrides
    case _authenticatedContext rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "authenticatedContext" "@@:" stub_0

  -- setAuthenticatedContext:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationWebBrowserExternallyAuthenticatableRequestOverrides
    case _setAuthenticatedContext rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAuthenticatedContext:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationWebBrowserExternallyAuthenticatableRequestOverrides
    if queriedSel == sel_authenticatedContext then pure (maybe 0 (const 1) (_authenticatedContext rec_))
    else if queriedSel == sel_setAuthenticatedContext then pure (maybe 0 (const 1) (_setAuthenticatedContext rec_))
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
newASAuthorizationWebBrowserExternallyAuthenticatableRequest :: ASAuthorizationWebBrowserExternallyAuthenticatableRequestOverrides -> IO RawId
newASAuthorizationWebBrowserExternallyAuthenticatableRequest overrides = do
  inst <- class_createInstance asAuthorizationWebBrowserExternallyAuthenticatableRequestDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
