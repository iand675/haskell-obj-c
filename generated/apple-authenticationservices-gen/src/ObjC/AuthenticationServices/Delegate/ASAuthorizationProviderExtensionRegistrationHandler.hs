{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASAuthorizationProviderExtensionRegistrationHandler@.
--
-- Usage:
--
-- @
-- delegate <- newASAuthorizationProviderExtensionRegistrationHandler defaultASAuthorizationProviderExtensionRegistrationHandlerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASAuthorizationProviderExtensionRegistrationHandler
  ( ASAuthorizationProviderExtensionRegistrationHandlerOverrides(..)
  , defaultASAuthorizationProviderExtensionRegistrationHandlerOverrides
  , newASAuthorizationProviderExtensionRegistrationHandler
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

-- | Overrides record for @\@protocol ASAuthorizationProviderExtensionRegistrationHandler@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASAuthorizationProviderExtensionRegistrationHandlerOverrides = ASAuthorizationProviderExtensionRegistrationHandlerOverrides
  { _registrationDidComplete :: !(Maybe (IO ()))
  , _registrationDidCancel :: !(Maybe (IO ()))
  , _displayNamesForGroups_loginManager_completion :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _supportedDeviceSigningAlgorithms :: !(Maybe (IO RawId))
  , _supportedDeviceEncryptionAlgorithms :: !(Maybe (IO RawId))
  , _supportedUserSecureEnclaveKeySigningAlgorithms :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultASAuthorizationProviderExtensionRegistrationHandlerOverrides :: ASAuthorizationProviderExtensionRegistrationHandlerOverrides
defaultASAuthorizationProviderExtensionRegistrationHandlerOverrides = ASAuthorizationProviderExtensionRegistrationHandlerOverrides
  { _registrationDidComplete = Nothing
  , _registrationDidCancel = Nothing
  , _displayNamesForGroups_loginManager_completion = Nothing
  , _supportedDeviceSigningAlgorithms = Nothing
  , _supportedDeviceEncryptionAlgorithms = Nothing
  , _supportedUserSecureEnclaveKeySigningAlgorithms = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE asAuthorizationProviderExtensionRegistrationHandlerDelegateClass #-}
asAuthorizationProviderExtensionRegistrationHandlerDelegateClass :: Class
asAuthorizationProviderExtensionRegistrationHandlerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASAuthorizationProviderExtensionRegistrationHandler" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_registrationDidComplete = unSelector (mkSelector "registrationDidComplete")
      sel_registrationDidCancel = unSelector (mkSelector "registrationDidCancel")
      sel_displayNamesForGroups_loginManager_completion = unSelector (mkSelector "displayNamesForGroups:loginManager:completion:")
      sel_supportedDeviceSigningAlgorithms = unSelector (mkSelector "supportedDeviceSigningAlgorithms")
      sel_supportedDeviceEncryptionAlgorithms = unSelector (mkSelector "supportedDeviceEncryptionAlgorithms")
      sel_supportedUserSecureEnclaveKeySigningAlgorithms = unSelector (mkSelector "supportedUserSecureEnclaveKeySigningAlgorithms")
  -- registrationDidComplete
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationProviderExtensionRegistrationHandlerOverrides
    case _registrationDidComplete rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "registrationDidComplete" "v@:" stub_0

  -- registrationDidCancel
  stub_1 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationProviderExtensionRegistrationHandlerOverrides
    case _registrationDidCancel rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "registrationDidCancel" "v@:" stub_1

  -- displayNamesForGroups:loginManager:completion:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationProviderExtensionRegistrationHandlerOverrides
    case _displayNamesForGroups_loginManager_completion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "displayNamesForGroups:loginManager:completion:" "v@:@@@" stub_2

  -- supportedDeviceSigningAlgorithms
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationProviderExtensionRegistrationHandlerOverrides
    case _supportedDeviceSigningAlgorithms rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "supportedDeviceSigningAlgorithms" "@@:" stub_3

  -- supportedDeviceEncryptionAlgorithms
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationProviderExtensionRegistrationHandlerOverrides
    case _supportedDeviceEncryptionAlgorithms rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "supportedDeviceEncryptionAlgorithms" "@@:" stub_4

  -- supportedUserSecureEnclaveKeySigningAlgorithms
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationProviderExtensionRegistrationHandlerOverrides
    case _supportedUserSecureEnclaveKeySigningAlgorithms rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "supportedUserSecureEnclaveKeySigningAlgorithms" "@@:" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationProviderExtensionRegistrationHandlerOverrides
    if queriedSel == sel_registrationDidComplete then pure (maybe 0 (const 1) (_registrationDidComplete rec_))
    else if queriedSel == sel_registrationDidCancel then pure (maybe 0 (const 1) (_registrationDidCancel rec_))
    else if queriedSel == sel_displayNamesForGroups_loginManager_completion then pure (maybe 0 (const 1) (_displayNamesForGroups_loginManager_completion rec_))
    else if queriedSel == sel_supportedDeviceSigningAlgorithms then pure (maybe 0 (const 1) (_supportedDeviceSigningAlgorithms rec_))
    else if queriedSel == sel_supportedDeviceEncryptionAlgorithms then pure (maybe 0 (const 1) (_supportedDeviceEncryptionAlgorithms rec_))
    else if queriedSel == sel_supportedUserSecureEnclaveKeySigningAlgorithms then pure (maybe 0 (const 1) (_supportedUserSecureEnclaveKeySigningAlgorithms rec_))
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
newASAuthorizationProviderExtensionRegistrationHandler :: ASAuthorizationProviderExtensionRegistrationHandlerOverrides -> IO RawId
newASAuthorizationProviderExtensionRegistrationHandler overrides = do
  inst <- class_createInstance asAuthorizationProviderExtensionRegistrationHandlerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
