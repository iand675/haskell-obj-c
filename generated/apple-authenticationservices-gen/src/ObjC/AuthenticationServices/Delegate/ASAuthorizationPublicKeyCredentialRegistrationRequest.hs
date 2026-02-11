{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASAuthorizationPublicKeyCredentialRegistrationRequest@.
--
-- Usage:
--
-- @
-- delegate <- newASAuthorizationPublicKeyCredentialRegistrationRequest defaultASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASAuthorizationPublicKeyCredentialRegistrationRequest
  ( ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides(..)
  , defaultASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
  , newASAuthorizationPublicKeyCredentialRegistrationRequest
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

-- | Overrides record for @\@protocol ASAuthorizationPublicKeyCredentialRegistrationRequest@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides = ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
  { _relyingPartyIdentifier :: !(Maybe (IO RawId))
  , _userID :: !(Maybe (IO RawId))
  , _setUserID :: !(Maybe (RawId -> IO ()))
  , _name :: !(Maybe (IO RawId))
  , _setName :: !(Maybe (RawId -> IO ()))
  , _displayName :: !(Maybe (IO RawId))
  , _setDisplayName :: !(Maybe (RawId -> IO ()))
  , _challenge :: !(Maybe (IO RawId))
  , _setChallenge :: !(Maybe (RawId -> IO ()))
  , _userVerificationPreference :: !(Maybe (IO RawId))
  , _setUserVerificationPreference :: !(Maybe (RawId -> IO ()))
  , _attestationPreference :: !(Maybe (IO RawId))
  , _setAttestationPreference :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultASAuthorizationPublicKeyCredentialRegistrationRequestOverrides :: ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
defaultASAuthorizationPublicKeyCredentialRegistrationRequestOverrides = ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
  { _relyingPartyIdentifier = Nothing
  , _userID = Nothing
  , _setUserID = Nothing
  , _name = Nothing
  , _setName = Nothing
  , _displayName = Nothing
  , _setDisplayName = Nothing
  , _challenge = Nothing
  , _setChallenge = Nothing
  , _userVerificationPreference = Nothing
  , _setUserVerificationPreference = Nothing
  , _attestationPreference = Nothing
  , _setAttestationPreference = Nothing
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
{-# NOINLINE asAuthorizationPublicKeyCredentialRegistrationRequestDelegateClass #-}
asAuthorizationPublicKeyCredentialRegistrationRequestDelegateClass :: Class
asAuthorizationPublicKeyCredentialRegistrationRequestDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASAuthorizationPublicKeyCredentialRegistrationRequest" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_relyingPartyIdentifier = unSelector (mkSelector "relyingPartyIdentifier")
      sel_userID = unSelector (mkSelector "userID")
      sel_setUserID = unSelector (mkSelector "setUserID:")
      sel_name = unSelector (mkSelector "name")
      sel_setName = unSelector (mkSelector "setName:")
      sel_displayName = unSelector (mkSelector "displayName")
      sel_setDisplayName = unSelector (mkSelector "setDisplayName:")
      sel_challenge = unSelector (mkSelector "challenge")
      sel_setChallenge = unSelector (mkSelector "setChallenge:")
      sel_userVerificationPreference = unSelector (mkSelector "userVerificationPreference")
      sel_setUserVerificationPreference = unSelector (mkSelector "setUserVerificationPreference:")
      sel_attestationPreference = unSelector (mkSelector "attestationPreference")
      sel_setAttestationPreference = unSelector (mkSelector "setAttestationPreference:")
  -- relyingPartyIdentifier
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    case _relyingPartyIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "relyingPartyIdentifier" "@@:" stub_0

  -- userID
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    case _userID rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "userID" "@@:" stub_1

  -- setUserID:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    case _setUserID rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setUserID:" "v@:@" stub_2

  -- name
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    case _name rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "name" "@@:" stub_3

  -- setName:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    case _setName rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setName:" "v@:@" stub_4

  -- displayName
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    case _displayName rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "displayName" "@@:" stub_5

  -- setDisplayName:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    case _setDisplayName rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setDisplayName:" "v@:@" stub_6

  -- challenge
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    case _challenge rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "challenge" "@@:" stub_7

  -- setChallenge:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    case _setChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setChallenge:" "v@:@" stub_8

  -- userVerificationPreference
  stub_9 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    case _userVerificationPreference rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "userVerificationPreference" "@@:" stub_9

  -- setUserVerificationPreference:
  stub_10 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    case _setUserVerificationPreference rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setUserVerificationPreference:" "v@:@" stub_10

  -- attestationPreference
  stub_11 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    case _attestationPreference rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "attestationPreference" "@@:" stub_11

  -- setAttestationPreference:
  stub_12 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    case _setAttestationPreference rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAttestationPreference:" "v@:@" stub_12

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides
    if queriedSel == sel_relyingPartyIdentifier then pure (maybe 0 (const 1) (_relyingPartyIdentifier rec_))
    else if queriedSel == sel_userID then pure (maybe 0 (const 1) (_userID rec_))
    else if queriedSel == sel_setUserID then pure (maybe 0 (const 1) (_setUserID rec_))
    else if queriedSel == sel_name then pure (maybe 0 (const 1) (_name rec_))
    else if queriedSel == sel_setName then pure (maybe 0 (const 1) (_setName rec_))
    else if queriedSel == sel_displayName then pure (maybe 0 (const 1) (_displayName rec_))
    else if queriedSel == sel_setDisplayName then pure (maybe 0 (const 1) (_setDisplayName rec_))
    else if queriedSel == sel_challenge then pure (maybe 0 (const 1) (_challenge rec_))
    else if queriedSel == sel_setChallenge then pure (maybe 0 (const 1) (_setChallenge rec_))
    else if queriedSel == sel_userVerificationPreference then pure (maybe 0 (const 1) (_userVerificationPreference rec_))
    else if queriedSel == sel_setUserVerificationPreference then pure (maybe 0 (const 1) (_setUserVerificationPreference rec_))
    else if queriedSel == sel_attestationPreference then pure (maybe 0 (const 1) (_attestationPreference rec_))
    else if queriedSel == sel_setAttestationPreference then pure (maybe 0 (const 1) (_setAttestationPreference rec_))
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
newASAuthorizationPublicKeyCredentialRegistrationRequest :: ASAuthorizationPublicKeyCredentialRegistrationRequestOverrides -> IO RawId
newASAuthorizationPublicKeyCredentialRegistrationRequest overrides = do
  inst <- class_createInstance asAuthorizationPublicKeyCredentialRegistrationRequestDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
