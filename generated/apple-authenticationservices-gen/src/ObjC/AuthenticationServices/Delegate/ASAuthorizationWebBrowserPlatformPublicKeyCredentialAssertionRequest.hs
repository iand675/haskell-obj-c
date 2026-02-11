{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequest@.
--
-- Usage:
--
-- @
-- delegate <- newASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequest defaultASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequest
  ( ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides(..)
  , defaultASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides
  , newASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequest
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

-- | Overrides record for @\@protocol ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequest@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides = ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides
  { _clientData :: !(Maybe (IO RawId))
  , _shouldShowHybridTransport :: !(Maybe (IO Bool))
  , _setShouldShowHybridTransport :: !(Maybe (Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides :: ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides
defaultASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides = ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides
  { _clientData = Nothing
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
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE asAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestDelegateClass #-}
asAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestDelegateClass :: Class
asAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequest" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_clientData = unSelector (mkSelector "clientData")
      sel_shouldShowHybridTransport = unSelector (mkSelector "shouldShowHybridTransport")
      sel_setShouldShowHybridTransport = unSelector (mkSelector "setShouldShowHybridTransport:")
  -- clientData
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides
    case _clientData rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "clientData" "@@:" stub_0

  -- shouldShowHybridTransport
  stub_1 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides
    case _shouldShowHybridTransport rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldShowHybridTransport" "B@:" stub_1

  -- setShouldShowHybridTransport:
  stub_2 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides
    case _setShouldShowHybridTransport rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setShouldShowHybridTransport:" "v@:B" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides
    if queriedSel == sel_clientData then pure (maybe 0 (const 1) (_clientData rec_))
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
newASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequest :: ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestOverrides -> IO RawId
newASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequest overrides = do
  inst <- class_createInstance asAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequestDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
