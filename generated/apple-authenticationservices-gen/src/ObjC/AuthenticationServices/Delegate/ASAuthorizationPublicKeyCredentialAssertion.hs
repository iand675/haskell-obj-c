{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASAuthorizationPublicKeyCredentialAssertion@.
--
-- Usage:
--
-- @
-- delegate <- newASAuthorizationPublicKeyCredentialAssertion defaultASAuthorizationPublicKeyCredentialAssertionOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASAuthorizationPublicKeyCredentialAssertion
  ( ASAuthorizationPublicKeyCredentialAssertionOverrides(..)
  , defaultASAuthorizationPublicKeyCredentialAssertionOverrides
  , newASAuthorizationPublicKeyCredentialAssertion
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

-- | Overrides record for @\@protocol ASAuthorizationPublicKeyCredentialAssertion@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASAuthorizationPublicKeyCredentialAssertionOverrides = ASAuthorizationPublicKeyCredentialAssertionOverrides
  { _rawAuthenticatorData :: !(Maybe (IO RawId))
  , _userID :: !(Maybe (IO RawId))
  , _signature :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultASAuthorizationPublicKeyCredentialAssertionOverrides :: ASAuthorizationPublicKeyCredentialAssertionOverrides
defaultASAuthorizationPublicKeyCredentialAssertionOverrides = ASAuthorizationPublicKeyCredentialAssertionOverrides
  { _rawAuthenticatorData = Nothing
  , _userID = Nothing
  , _signature = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE asAuthorizationPublicKeyCredentialAssertionDelegateClass #-}
asAuthorizationPublicKeyCredentialAssertionDelegateClass :: Class
asAuthorizationPublicKeyCredentialAssertionDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASAuthorizationPublicKeyCredentialAssertion" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_rawAuthenticatorData = unSelector (mkSelector "rawAuthenticatorData")
      sel_userID = unSelector (mkSelector "userID")
      sel_signature = unSelector (mkSelector "signature")
  -- rawAuthenticatorData
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialAssertionOverrides
    case _rawAuthenticatorData rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "rawAuthenticatorData" "@@:" stub_0

  -- userID
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialAssertionOverrides
    case _userID rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "userID" "@@:" stub_1

  -- signature
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialAssertionOverrides
    case _signature rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "signature" "@@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationPublicKeyCredentialAssertionOverrides
    if queriedSel == sel_rawAuthenticatorData then pure (maybe 0 (const 1) (_rawAuthenticatorData rec_))
    else if queriedSel == sel_userID then pure (maybe 0 (const 1) (_userID rec_))
    else if queriedSel == sel_signature then pure (maybe 0 (const 1) (_signature rec_))
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
newASAuthorizationPublicKeyCredentialAssertion :: ASAuthorizationPublicKeyCredentialAssertionOverrides -> IO RawId
newASAuthorizationPublicKeyCredentialAssertion overrides = do
  inst <- class_createInstance asAuthorizationPublicKeyCredentialAssertionDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
