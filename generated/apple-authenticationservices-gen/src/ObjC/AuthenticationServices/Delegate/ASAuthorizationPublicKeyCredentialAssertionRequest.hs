{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASAuthorizationPublicKeyCredentialAssertionRequest@.
--
-- Usage:
--
-- @
-- delegate <- newASAuthorizationPublicKeyCredentialAssertionRequest defaultASAuthorizationPublicKeyCredentialAssertionRequestOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASAuthorizationPublicKeyCredentialAssertionRequest
  ( ASAuthorizationPublicKeyCredentialAssertionRequestOverrides(..)
  , defaultASAuthorizationPublicKeyCredentialAssertionRequestOverrides
  , newASAuthorizationPublicKeyCredentialAssertionRequest
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

-- | Overrides record for @\@protocol ASAuthorizationPublicKeyCredentialAssertionRequest@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASAuthorizationPublicKeyCredentialAssertionRequestOverrides = ASAuthorizationPublicKeyCredentialAssertionRequestOverrides
  { _challenge :: !(Maybe (IO RawId))
  , _setChallenge :: !(Maybe (RawId -> IO ()))
  , _relyingPartyIdentifier :: !(Maybe (IO RawId))
  , _setRelyingPartyIdentifier :: !(Maybe (RawId -> IO ()))
  , _allowedCredentials :: !(Maybe (IO RawId))
  , _setAllowedCredentials :: !(Maybe (RawId -> IO ()))
  , _userVerificationPreference :: !(Maybe (IO RawId))
  , _setUserVerificationPreference :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultASAuthorizationPublicKeyCredentialAssertionRequestOverrides :: ASAuthorizationPublicKeyCredentialAssertionRequestOverrides
defaultASAuthorizationPublicKeyCredentialAssertionRequestOverrides = ASAuthorizationPublicKeyCredentialAssertionRequestOverrides
  { _challenge = Nothing
  , _setChallenge = Nothing
  , _relyingPartyIdentifier = Nothing
  , _setRelyingPartyIdentifier = Nothing
  , _allowedCredentials = Nothing
  , _setAllowedCredentials = Nothing
  , _userVerificationPreference = Nothing
  , _setUserVerificationPreference = Nothing
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
{-# NOINLINE asAuthorizationPublicKeyCredentialAssertionRequestDelegateClass #-}
asAuthorizationPublicKeyCredentialAssertionRequestDelegateClass :: Class
asAuthorizationPublicKeyCredentialAssertionRequestDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASAuthorizationPublicKeyCredentialAssertionRequest" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_challenge = unSelector (mkSelector "challenge")
      sel_setChallenge = unSelector (mkSelector "setChallenge:")
      sel_relyingPartyIdentifier = unSelector (mkSelector "relyingPartyIdentifier")
      sel_setRelyingPartyIdentifier = unSelector (mkSelector "setRelyingPartyIdentifier:")
      sel_allowedCredentials = unSelector (mkSelector "allowedCredentials")
      sel_setAllowedCredentials = unSelector (mkSelector "setAllowedCredentials:")
      sel_userVerificationPreference = unSelector (mkSelector "userVerificationPreference")
      sel_setUserVerificationPreference = unSelector (mkSelector "setUserVerificationPreference:")
  -- challenge
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialAssertionRequestOverrides
    case _challenge rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "challenge" "@@:" stub_0

  -- setChallenge:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialAssertionRequestOverrides
    case _setChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setChallenge:" "v@:@" stub_1

  -- relyingPartyIdentifier
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialAssertionRequestOverrides
    case _relyingPartyIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "relyingPartyIdentifier" "@@:" stub_2

  -- setRelyingPartyIdentifier:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialAssertionRequestOverrides
    case _setRelyingPartyIdentifier rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setRelyingPartyIdentifier:" "v@:@" stub_3

  -- allowedCredentials
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialAssertionRequestOverrides
    case _allowedCredentials rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "allowedCredentials" "@@:" stub_4

  -- setAllowedCredentials:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialAssertionRequestOverrides
    case _setAllowedCredentials rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAllowedCredentials:" "v@:@" stub_5

  -- userVerificationPreference
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialAssertionRequestOverrides
    case _userVerificationPreference rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "userVerificationPreference" "@@:" stub_6

  -- setUserVerificationPreference:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialAssertionRequestOverrides
    case _setUserVerificationPreference rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setUserVerificationPreference:" "v@:@" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialAssertionRequestOverrides
    if queriedSel == sel_challenge then pure (maybe 0 (const 1) (_challenge rec_))
    else if queriedSel == sel_setChallenge then pure (maybe 0 (const 1) (_setChallenge rec_))
    else if queriedSel == sel_relyingPartyIdentifier then pure (maybe 0 (const 1) (_relyingPartyIdentifier rec_))
    else if queriedSel == sel_setRelyingPartyIdentifier then pure (maybe 0 (const 1) (_setRelyingPartyIdentifier rec_))
    else if queriedSel == sel_allowedCredentials then pure (maybe 0 (const 1) (_allowedCredentials rec_))
    else if queriedSel == sel_setAllowedCredentials then pure (maybe 0 (const 1) (_setAllowedCredentials rec_))
    else if queriedSel == sel_userVerificationPreference then pure (maybe 0 (const 1) (_userVerificationPreference rec_))
    else if queriedSel == sel_setUserVerificationPreference then pure (maybe 0 (const 1) (_setUserVerificationPreference rec_))
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
newASAuthorizationPublicKeyCredentialAssertionRequest :: ASAuthorizationPublicKeyCredentialAssertionRequestOverrides -> IO RawId
newASAuthorizationPublicKeyCredentialAssertionRequest overrides = do
  inst <- class_createInstance asAuthorizationPublicKeyCredentialAssertionRequestDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
