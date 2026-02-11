{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASAuthorizationControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newASAuthorizationControllerDelegate defaultASAuthorizationControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASAuthorizationControllerDelegate
  ( ASAuthorizationControllerDelegateOverrides(..)
  , defaultASAuthorizationControllerDelegateOverrides
  , newASAuthorizationControllerDelegate
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

-- | Overrides record for @\@protocol ASAuthorizationControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASAuthorizationControllerDelegateOverrides = ASAuthorizationControllerDelegateOverrides
  { _authorizationController_didCompleteWithAuthorization :: !(Maybe (RawId -> RawId -> IO ()))
  , _authorizationController_didCompleteWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _authorizationController_didCompleteWithCustomMethod :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultASAuthorizationControllerDelegateOverrides :: ASAuthorizationControllerDelegateOverrides
defaultASAuthorizationControllerDelegateOverrides = ASAuthorizationControllerDelegateOverrides
  { _authorizationController_didCompleteWithAuthorization = Nothing
  , _authorizationController_didCompleteWithError = Nothing
  , _authorizationController_didCompleteWithCustomMethod = Nothing
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
{-# NOINLINE asAuthorizationControllerDelegateDelegateClass #-}
asAuthorizationControllerDelegateDelegateClass :: Class
asAuthorizationControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASAuthorizationControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_authorizationController_didCompleteWithAuthorization = unSelector (mkSelector "authorizationController:didCompleteWithAuthorization:")
      sel_authorizationController_didCompleteWithError = unSelector (mkSelector "authorizationController:didCompleteWithError:")
      sel_authorizationController_didCompleteWithCustomMethod = unSelector (mkSelector "authorizationController:didCompleteWithCustomMethod:")
  -- authorizationController:didCompleteWithAuthorization:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationControllerDelegateOverrides
    case _authorizationController_didCompleteWithAuthorization rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "authorizationController:didCompleteWithAuthorization:" "v@:@@" stub_0

  -- authorizationController:didCompleteWithError:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationControllerDelegateOverrides
    case _authorizationController_didCompleteWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "authorizationController:didCompleteWithError:" "v@:@@" stub_1

  -- authorizationController:didCompleteWithCustomMethod:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationControllerDelegateOverrides
    case _authorizationController_didCompleteWithCustomMethod rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "authorizationController:didCompleteWithCustomMethod:" "v@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationControllerDelegateOverrides
    if queriedSel == sel_authorizationController_didCompleteWithAuthorization then pure (maybe 0 (const 1) (_authorizationController_didCompleteWithAuthorization rec_))
    else if queriedSel == sel_authorizationController_didCompleteWithError then pure (maybe 0 (const 1) (_authorizationController_didCompleteWithError rec_))
    else if queriedSel == sel_authorizationController_didCompleteWithCustomMethod then pure (maybe 0 (const 1) (_authorizationController_didCompleteWithCustomMethod rec_))
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
newASAuthorizationControllerDelegate :: ASAuthorizationControllerDelegateOverrides -> IO RawId
newASAuthorizationControllerDelegate overrides = do
  inst <- class_createInstance asAuthorizationControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
