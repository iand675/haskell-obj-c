{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASWebAuthenticationPresentationContextProviding@.
--
-- Usage:
--
-- @
-- delegate <- newASWebAuthenticationPresentationContextProviding defaultASWebAuthenticationPresentationContextProvidingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASWebAuthenticationPresentationContextProviding
  ( ASWebAuthenticationPresentationContextProvidingOverrides(..)
  , defaultASWebAuthenticationPresentationContextProvidingOverrides
  , newASWebAuthenticationPresentationContextProviding
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

-- | Overrides record for @\@protocol ASWebAuthenticationPresentationContextProviding@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASWebAuthenticationPresentationContextProvidingOverrides = ASWebAuthenticationPresentationContextProvidingOverrides
  { _presentationAnchorForWebAuthenticationSession :: !(Maybe (RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultASWebAuthenticationPresentationContextProvidingOverrides :: ASWebAuthenticationPresentationContextProvidingOverrides
defaultASWebAuthenticationPresentationContextProvidingOverrides = ASWebAuthenticationPresentationContextProvidingOverrides
  { _presentationAnchorForWebAuthenticationSession = Nothing
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
{-# NOINLINE asWebAuthenticationPresentationContextProvidingDelegateClass #-}
asWebAuthenticationPresentationContextProvidingDelegateClass :: Class
asWebAuthenticationPresentationContextProvidingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASWebAuthenticationPresentationContextProviding" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_presentationAnchorForWebAuthenticationSession = unSelector (mkSelector "presentationAnchorForWebAuthenticationSession:")
  -- presentationAnchorForWebAuthenticationSession:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASWebAuthenticationPresentationContextProvidingOverrides
    case _presentationAnchorForWebAuthenticationSession rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "presentationAnchorForWebAuthenticationSession:" "@@:@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASWebAuthenticationPresentationContextProvidingOverrides
    if queriedSel == sel_presentationAnchorForWebAuthenticationSession then pure (maybe 0 (const 1) (_presentationAnchorForWebAuthenticationSession rec_))
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
newASWebAuthenticationPresentationContextProviding :: ASWebAuthenticationPresentationContextProvidingOverrides -> IO RawId
newASWebAuthenticationPresentationContextProviding overrides = do
  inst <- class_createInstance asWebAuthenticationPresentationContextProvidingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
