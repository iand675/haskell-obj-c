{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASAuthorizationControllerPresentationContextProviding@.
--
-- Usage:
--
-- @
-- delegate <- newASAuthorizationControllerPresentationContextProviding defaultASAuthorizationControllerPresentationContextProvidingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASAuthorizationControllerPresentationContextProviding
  ( ASAuthorizationControllerPresentationContextProvidingOverrides(..)
  , defaultASAuthorizationControllerPresentationContextProvidingOverrides
  , newASAuthorizationControllerPresentationContextProviding
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

-- | Overrides record for @\@protocol ASAuthorizationControllerPresentationContextProviding@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASAuthorizationControllerPresentationContextProvidingOverrides = ASAuthorizationControllerPresentationContextProvidingOverrides
  { _presentationAnchorForAuthorizationController :: !(Maybe (RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultASAuthorizationControllerPresentationContextProvidingOverrides :: ASAuthorizationControllerPresentationContextProvidingOverrides
defaultASAuthorizationControllerPresentationContextProvidingOverrides = ASAuthorizationControllerPresentationContextProvidingOverrides
  { _presentationAnchorForAuthorizationController = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE asAuthorizationControllerPresentationContextProvidingDelegateClass #-}
asAuthorizationControllerPresentationContextProvidingDelegateClass :: Class
asAuthorizationControllerPresentationContextProvidingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASAuthorizationControllerPresentationContextProviding" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_presentationAnchorForAuthorizationController = unSelector (mkSelector "presentationAnchorForAuthorizationController:")
  -- presentationAnchorForAuthorizationController:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationControllerPresentationContextProvidingOverrides
    case _presentationAnchorForAuthorizationController rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "presentationAnchorForAuthorizationController:" "@@:@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationControllerPresentationContextProvidingOverrides
    if queriedSel == sel_presentationAnchorForAuthorizationController then pure (maybe 0 (const 1) (_presentationAnchorForAuthorizationController rec_))
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
newASAuthorizationControllerPresentationContextProviding :: ASAuthorizationControllerPresentationContextProvidingOverrides -> IO RawId
newASAuthorizationControllerPresentationContextProviding overrides = do
  inst <- class_createInstance asAuthorizationControllerPresentationContextProvidingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
