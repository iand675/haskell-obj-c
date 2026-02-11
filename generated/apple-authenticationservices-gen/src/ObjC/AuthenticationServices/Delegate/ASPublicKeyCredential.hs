{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASPublicKeyCredential@.
--
-- Usage:
--
-- @
-- delegate <- newASPublicKeyCredential defaultASPublicKeyCredentialOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASPublicKeyCredential
  ( ASPublicKeyCredentialOverrides(..)
  , defaultASPublicKeyCredentialOverrides
  , newASPublicKeyCredential
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

-- | Overrides record for @\@protocol ASPublicKeyCredential@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASPublicKeyCredentialOverrides = ASPublicKeyCredentialOverrides
  { _rawClientDataJSON :: !(Maybe (IO RawId))
  , _credentialID :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultASPublicKeyCredentialOverrides :: ASPublicKeyCredentialOverrides
defaultASPublicKeyCredentialOverrides = ASPublicKeyCredentialOverrides
  { _rawClientDataJSON = Nothing
  , _credentialID = Nothing
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
{-# NOINLINE asPublicKeyCredentialDelegateClass #-}
asPublicKeyCredentialDelegateClass :: Class
asPublicKeyCredentialDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASPublicKeyCredential" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_rawClientDataJSON = unSelector (mkSelector "rawClientDataJSON")
      sel_credentialID = unSelector (mkSelector "credentialID")
  -- rawClientDataJSON
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASPublicKeyCredentialOverrides
    case _rawClientDataJSON rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "rawClientDataJSON" "@@:" stub_0

  -- credentialID
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASPublicKeyCredentialOverrides
    case _credentialID rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "credentialID" "@@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASPublicKeyCredentialOverrides
    if queriedSel == sel_rawClientDataJSON then pure (maybe 0 (const 1) (_rawClientDataJSON rec_))
    else if queriedSel == sel_credentialID then pure (maybe 0 (const 1) (_credentialID rec_))
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
newASPublicKeyCredential :: ASPublicKeyCredentialOverrides -> IO RawId
newASPublicKeyCredential overrides = do
  inst <- class_createInstance asPublicKeyCredentialDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
