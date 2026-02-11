{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequest@.
--
-- Usage:
--
-- @
-- delegate <- newASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequest defaultASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequest
  ( ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides(..)
  , defaultASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides
  , newASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequest
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

-- | Overrides record for @\@protocol ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequest@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides = ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides
  { _clientData :: !(Maybe (IO RawId))
  , _excludedCredentials :: !(Maybe (IO RawId))
  , _setExcludedCredentials :: !(Maybe (RawId -> IO ()))
  , _shouldShowHybridTransport :: !(Maybe (IO Bool))
  , _setShouldShowHybridTransport :: !(Maybe (Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides :: ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides
defaultASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides = ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides
  { _clientData = Nothing
  , _excludedCredentials = Nothing
  , _setExcludedCredentials = Nothing
  , _shouldShowHybridTransport = Nothing
  , _setShouldShowHybridTransport = Nothing
  }

foreign import ccall "wrapper"
  wrap_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

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
{-# NOINLINE asAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestDelegateClass #-}
asAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestDelegateClass :: Class
asAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequest" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_clientData = unSelector (mkSelector "clientData")
      sel_excludedCredentials = unSelector (mkSelector "excludedCredentials")
      sel_setExcludedCredentials = unSelector (mkSelector "setExcludedCredentials:")
      sel_shouldShowHybridTransport = unSelector (mkSelector "shouldShowHybridTransport")
      sel_setShouldShowHybridTransport = unSelector (mkSelector "setShouldShowHybridTransport:")
  -- clientData
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides
    case _clientData rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "clientData" "@@:" stub_0

  -- excludedCredentials
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides
    case _excludedCredentials rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "excludedCredentials" "@@:" stub_1

  -- setExcludedCredentials:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides
    case _setExcludedCredentials rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setExcludedCredentials:" "v@:@" stub_2

  -- shouldShowHybridTransport
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides
    case _shouldShowHybridTransport rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldShowHybridTransport" "B@:" stub_3

  -- setShouldShowHybridTransport:
  stub_4 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides
    case _setShouldShowHybridTransport rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setShouldShowHybridTransport:" "v@:B" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides
    if queriedSel == sel_clientData then pure (maybe 0 (const 1) (_clientData rec_))
    else if queriedSel == sel_excludedCredentials then pure (maybe 0 (const 1) (_excludedCredentials rec_))
    else if queriedSel == sel_setExcludedCredentials then pure (maybe 0 (const 1) (_setExcludedCredentials rec_))
    else if queriedSel == sel_shouldShowHybridTransport then pure (maybe 0 (const 1) (_shouldShowHybridTransport rec_))
    else if queriedSel == sel_setShouldShowHybridTransport then pure (maybe 0 (const 1) (_setShouldShowHybridTransport rec_))
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
newASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequest :: ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestOverrides -> IO RawId
newASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequest overrides = do
  inst <- class_createInstance asAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequestDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
