{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASAuthorizationProviderExtensionAuthorizationRequestHandler@.
--
-- Usage:
--
-- @
-- delegate <- newASAuthorizationProviderExtensionAuthorizationRequestHandler defaultASAuthorizationProviderExtensionAuthorizationRequestHandlerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASAuthorizationProviderExtensionAuthorizationRequestHandler
  ( ASAuthorizationProviderExtensionAuthorizationRequestHandlerOverrides(..)
  , defaultASAuthorizationProviderExtensionAuthorizationRequestHandlerOverrides
  , newASAuthorizationProviderExtensionAuthorizationRequestHandler
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

-- | Overrides record for @\@protocol ASAuthorizationProviderExtensionAuthorizationRequestHandler@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASAuthorizationProviderExtensionAuthorizationRequestHandlerOverrides = ASAuthorizationProviderExtensionAuthorizationRequestHandlerOverrides
  { _beginAuthorizationWithRequest :: !(Maybe (RawId -> IO ()))
  , _cancelAuthorizationWithRequest :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultASAuthorizationProviderExtensionAuthorizationRequestHandlerOverrides :: ASAuthorizationProviderExtensionAuthorizationRequestHandlerOverrides
defaultASAuthorizationProviderExtensionAuthorizationRequestHandlerOverrides = ASAuthorizationProviderExtensionAuthorizationRequestHandlerOverrides
  { _beginAuthorizationWithRequest = Nothing
  , _cancelAuthorizationWithRequest = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE asAuthorizationProviderExtensionAuthorizationRequestHandlerDelegateClass #-}
asAuthorizationProviderExtensionAuthorizationRequestHandlerDelegateClass :: Class
asAuthorizationProviderExtensionAuthorizationRequestHandlerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASAuthorizationProviderExtensionAuthorizationRequestHandler" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_beginAuthorizationWithRequest = unSelector (mkSelector "beginAuthorizationWithRequest:")
      sel_cancelAuthorizationWithRequest = unSelector (mkSelector "cancelAuthorizationWithRequest:")
  -- beginAuthorizationWithRequest:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationProviderExtensionAuthorizationRequestHandlerOverrides
    case _beginAuthorizationWithRequest rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "beginAuthorizationWithRequest:" "v@:@" stub_0

  -- cancelAuthorizationWithRequest:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationProviderExtensionAuthorizationRequestHandlerOverrides
    case _cancelAuthorizationWithRequest rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "cancelAuthorizationWithRequest:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAuthorizationProviderExtensionAuthorizationRequestHandlerOverrides
    if queriedSel == sel_beginAuthorizationWithRequest then pure (maybe 0 (const 1) (_beginAuthorizationWithRequest rec_))
    else if queriedSel == sel_cancelAuthorizationWithRequest then pure (maybe 0 (const 1) (_cancelAuthorizationWithRequest rec_))
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
newASAuthorizationProviderExtensionAuthorizationRequestHandler :: ASAuthorizationProviderExtensionAuthorizationRequestHandlerOverrides -> IO RawId
newASAuthorizationProviderExtensionAuthorizationRequestHandler overrides = do
  inst <- class_createInstance asAuthorizationProviderExtensionAuthorizationRequestHandlerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
