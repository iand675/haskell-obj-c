{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASWebAuthenticationSessionRequestDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newASWebAuthenticationSessionRequestDelegate defaultASWebAuthenticationSessionRequestDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASWebAuthenticationSessionRequestDelegate
  ( ASWebAuthenticationSessionRequestDelegateOverrides(..)
  , defaultASWebAuthenticationSessionRequestDelegateOverrides
  , newASWebAuthenticationSessionRequestDelegate
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

-- | Overrides record for @\@protocol ASWebAuthenticationSessionRequestDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASWebAuthenticationSessionRequestDelegateOverrides = ASWebAuthenticationSessionRequestDelegateOverrides
  { _authenticationSessionRequest_didCompleteWithCallbackURL :: !(Maybe (RawId -> RawId -> IO ()))
  , _authenticationSessionRequest_didCancelWithError :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultASWebAuthenticationSessionRequestDelegateOverrides :: ASWebAuthenticationSessionRequestDelegateOverrides
defaultASWebAuthenticationSessionRequestDelegateOverrides = ASWebAuthenticationSessionRequestDelegateOverrides
  { _authenticationSessionRequest_didCompleteWithCallbackURL = Nothing
  , _authenticationSessionRequest_didCancelWithError = Nothing
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
{-# NOINLINE asWebAuthenticationSessionRequestDelegateDelegateClass #-}
asWebAuthenticationSessionRequestDelegateDelegateClass :: Class
asWebAuthenticationSessionRequestDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASWebAuthenticationSessionRequestDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_authenticationSessionRequest_didCompleteWithCallbackURL = unSelector (mkSelector "authenticationSessionRequest:didCompleteWithCallbackURL:")
      sel_authenticationSessionRequest_didCancelWithError = unSelector (mkSelector "authenticationSessionRequest:didCancelWithError:")
  -- authenticationSessionRequest:didCompleteWithCallbackURL:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASWebAuthenticationSessionRequestDelegateOverrides
    case _authenticationSessionRequest_didCompleteWithCallbackURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "authenticationSessionRequest:didCompleteWithCallbackURL:" "v@:@@" stub_0

  -- authenticationSessionRequest:didCancelWithError:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASWebAuthenticationSessionRequestDelegateOverrides
    case _authenticationSessionRequest_didCancelWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "authenticationSessionRequest:didCancelWithError:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASWebAuthenticationSessionRequestDelegateOverrides
    if queriedSel == sel_authenticationSessionRequest_didCompleteWithCallbackURL then pure (maybe 0 (const 1) (_authenticationSessionRequest_didCompleteWithCallbackURL rec_))
    else if queriedSel == sel_authenticationSessionRequest_didCancelWithError then pure (maybe 0 (const 1) (_authenticationSessionRequest_didCancelWithError rec_))
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
newASWebAuthenticationSessionRequestDelegate :: ASWebAuthenticationSessionRequestDelegateOverrides -> IO RawId
newASWebAuthenticationSessionRequestDelegate overrides = do
  inst <- class_createInstance asWebAuthenticationSessionRequestDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
