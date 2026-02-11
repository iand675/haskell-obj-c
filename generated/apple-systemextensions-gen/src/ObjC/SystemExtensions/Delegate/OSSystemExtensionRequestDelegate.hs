{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol OSSystemExtensionRequestDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newOSSystemExtensionRequestDelegate defaultOSSystemExtensionRequestDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SystemExtensions.Delegate.OSSystemExtensionRequestDelegate
  ( OSSystemExtensionRequestDelegateOverrides(..)
  , defaultOSSystemExtensionRequestDelegateOverrides
  , newOSSystemExtensionRequestDelegate
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

-- | Overrides record for @\@protocol OSSystemExtensionRequestDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data OSSystemExtensionRequestDelegateOverrides = OSSystemExtensionRequestDelegateOverrides
  { _requestNeedsUserApproval :: !(Maybe (RawId -> IO ()))
  , _request_didFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _request_foundProperties :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultOSSystemExtensionRequestDelegateOverrides :: OSSystemExtensionRequestDelegateOverrides
defaultOSSystemExtensionRequestDelegateOverrides = OSSystemExtensionRequestDelegateOverrides
  { _requestNeedsUserApproval = Nothing
  , _request_didFailWithError = Nothing
  , _request_foundProperties = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE osSystemExtensionRequestDelegateDelegateClass #-}
osSystemExtensionRequestDelegateDelegateClass :: Class
osSystemExtensionRequestDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsOSSystemExtensionRequestDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_requestNeedsUserApproval = unSelector (mkSelector "requestNeedsUserApproval:")
      sel_request_didFailWithError = unSelector (mkSelector "request:didFailWithError:")
      sel_request_foundProperties = unSelector (mkSelector "request:foundProperties:")
  -- requestNeedsUserApproval:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSSystemExtensionRequestDelegateOverrides
    case _requestNeedsUserApproval rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "requestNeedsUserApproval:" "v@:@" stub_0

  -- request:didFailWithError:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSSystemExtensionRequestDelegateOverrides
    case _request_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "request:didFailWithError:" "v@:@@" stub_1

  -- request:foundProperties:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSSystemExtensionRequestDelegateOverrides
    case _request_foundProperties rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "request:foundProperties:" "v@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSSystemExtensionRequestDelegateOverrides
    if queriedSel == sel_requestNeedsUserApproval then pure (maybe 0 (const 1) (_requestNeedsUserApproval rec_))
    else if queriedSel == sel_request_didFailWithError then pure (maybe 0 (const 1) (_request_didFailWithError rec_))
    else if queriedSel == sel_request_foundProperties then pure (maybe 0 (const 1) (_request_foundProperties rec_))
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
newOSSystemExtensionRequestDelegate :: OSSystemExtensionRequestDelegateOverrides -> IO RawId
newOSSystemExtensionRequestDelegate overrides = do
  inst <- class_createInstance osSystemExtensionRequestDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
