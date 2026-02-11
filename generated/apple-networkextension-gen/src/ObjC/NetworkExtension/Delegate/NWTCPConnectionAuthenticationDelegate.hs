{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NWTCPConnectionAuthenticationDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNWTCPConnectionAuthenticationDelegate defaultNWTCPConnectionAuthenticationDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.NetworkExtension.Delegate.NWTCPConnectionAuthenticationDelegate
  ( NWTCPConnectionAuthenticationDelegateOverrides(..)
  , defaultNWTCPConnectionAuthenticationDelegateOverrides
  , newNWTCPConnectionAuthenticationDelegate
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

-- | Overrides record for @\@protocol NWTCPConnectionAuthenticationDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NWTCPConnectionAuthenticationDelegateOverrides = NWTCPConnectionAuthenticationDelegateOverrides
  { _shouldProvideIdentityForConnection :: !(Maybe (RawId -> IO Bool))
  , _provideIdentityForConnection_completionHandler :: !(Maybe (RawId -> RawId -> IO ()))
  , _shouldEvaluateTrustForConnection :: !(Maybe (RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNWTCPConnectionAuthenticationDelegateOverrides :: NWTCPConnectionAuthenticationDelegateOverrides
defaultNWTCPConnectionAuthenticationDelegateOverrides = NWTCPConnectionAuthenticationDelegateOverrides
  { _shouldProvideIdentityForConnection = Nothing
  , _provideIdentityForConnection_completionHandler = Nothing
  , _shouldEvaluateTrustForConnection = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nwtcpConnectionAuthenticationDelegateDelegateClass #-}
nwtcpConnectionAuthenticationDelegateDelegateClass :: Class
nwtcpConnectionAuthenticationDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNWTCPConnectionAuthenticationDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_shouldProvideIdentityForConnection = unSelector (mkSelector "shouldProvideIdentityForConnection:")
      sel_provideIdentityForConnection_completionHandler = unSelector (mkSelector "provideIdentityForConnection:completionHandler:")
      sel_shouldEvaluateTrustForConnection = unSelector (mkSelector "shouldEvaluateTrustForConnection:")
  -- shouldProvideIdentityForConnection:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NWTCPConnectionAuthenticationDelegateOverrides
    case _shouldProvideIdentityForConnection rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldProvideIdentityForConnection:" "B@:@" stub_0

  -- provideIdentityForConnection:completionHandler:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NWTCPConnectionAuthenticationDelegateOverrides
    case _provideIdentityForConnection_completionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "provideIdentityForConnection:completionHandler:" "v@:@@" stub_1

  -- shouldEvaluateTrustForConnection:
  stub_2 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NWTCPConnectionAuthenticationDelegateOverrides
    case _shouldEvaluateTrustForConnection rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldEvaluateTrustForConnection:" "B@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NWTCPConnectionAuthenticationDelegateOverrides
    if queriedSel == sel_shouldProvideIdentityForConnection then pure (maybe 0 (const 1) (_shouldProvideIdentityForConnection rec_))
    else if queriedSel == sel_provideIdentityForConnection_completionHandler then pure (maybe 0 (const 1) (_provideIdentityForConnection_completionHandler rec_))
    else if queriedSel == sel_shouldEvaluateTrustForConnection then pure (maybe 0 (const 1) (_shouldEvaluateTrustForConnection rec_))
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
newNWTCPConnectionAuthenticationDelegate :: NWTCPConnectionAuthenticationDelegateOverrides -> IO RawId
newNWTCPConnectionAuthenticationDelegate overrides = do
  inst <- class_createInstance nwtcpConnectionAuthenticationDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
