{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSURLAuthenticationChallengeSender@.
--
-- Usage:
--
-- @
-- delegate <- newNSURLAuthenticationChallengeSender defaultNSURLAuthenticationChallengeSenderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSURLAuthenticationChallengeSender
  ( NSURLAuthenticationChallengeSenderOverrides(..)
  , defaultNSURLAuthenticationChallengeSenderOverrides
  , newNSURLAuthenticationChallengeSender
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

-- | Overrides record for @\@protocol NSURLAuthenticationChallengeSender@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSURLAuthenticationChallengeSenderOverrides = NSURLAuthenticationChallengeSenderOverrides
  { _useCredential_forAuthenticationChallenge :: !(Maybe (RawId -> RawId -> IO ()))
  , _continueWithoutCredentialForAuthenticationChallenge :: !(Maybe (RawId -> IO ()))
  , _cancelAuthenticationChallenge :: !(Maybe (RawId -> IO ()))
  , _performDefaultHandlingForAuthenticationChallenge :: !(Maybe (RawId -> IO ()))
  , _rejectProtectionSpaceAndContinueWithChallenge :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSURLAuthenticationChallengeSenderOverrides :: NSURLAuthenticationChallengeSenderOverrides
defaultNSURLAuthenticationChallengeSenderOverrides = NSURLAuthenticationChallengeSenderOverrides
  { _useCredential_forAuthenticationChallenge = Nothing
  , _continueWithoutCredentialForAuthenticationChallenge = Nothing
  , _cancelAuthenticationChallenge = Nothing
  , _performDefaultHandlingForAuthenticationChallenge = Nothing
  , _rejectProtectionSpaceAndContinueWithChallenge = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsurlAuthenticationChallengeSenderDelegateClass #-}
nsurlAuthenticationChallengeSenderDelegateClass :: Class
nsurlAuthenticationChallengeSenderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSURLAuthenticationChallengeSender" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_useCredential_forAuthenticationChallenge = unSelector (mkSelector "useCredential:forAuthenticationChallenge:")
      sel_continueWithoutCredentialForAuthenticationChallenge = unSelector (mkSelector "continueWithoutCredentialForAuthenticationChallenge:")
      sel_cancelAuthenticationChallenge = unSelector (mkSelector "cancelAuthenticationChallenge:")
      sel_performDefaultHandlingForAuthenticationChallenge = unSelector (mkSelector "performDefaultHandlingForAuthenticationChallenge:")
      sel_rejectProtectionSpaceAndContinueWithChallenge = unSelector (mkSelector "rejectProtectionSpaceAndContinueWithChallenge:")
  -- useCredential:forAuthenticationChallenge:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLAuthenticationChallengeSenderOverrides
    case _useCredential_forAuthenticationChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "useCredential:forAuthenticationChallenge:" "v@:@@" stub_0

  -- continueWithoutCredentialForAuthenticationChallenge:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLAuthenticationChallengeSenderOverrides
    case _continueWithoutCredentialForAuthenticationChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "continueWithoutCredentialForAuthenticationChallenge:" "v@:@" stub_1

  -- cancelAuthenticationChallenge:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLAuthenticationChallengeSenderOverrides
    case _cancelAuthenticationChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "cancelAuthenticationChallenge:" "v@:@" stub_2

  -- performDefaultHandlingForAuthenticationChallenge:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLAuthenticationChallengeSenderOverrides
    case _performDefaultHandlingForAuthenticationChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "performDefaultHandlingForAuthenticationChallenge:" "v@:@" stub_3

  -- rejectProtectionSpaceAndContinueWithChallenge:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLAuthenticationChallengeSenderOverrides
    case _rejectProtectionSpaceAndContinueWithChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "rejectProtectionSpaceAndContinueWithChallenge:" "v@:@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLAuthenticationChallengeSenderOverrides
    if queriedSel == sel_useCredential_forAuthenticationChallenge then pure (maybe 0 (const 1) (_useCredential_forAuthenticationChallenge rec_))
    else if queriedSel == sel_continueWithoutCredentialForAuthenticationChallenge then pure (maybe 0 (const 1) (_continueWithoutCredentialForAuthenticationChallenge rec_))
    else if queriedSel == sel_cancelAuthenticationChallenge then pure (maybe 0 (const 1) (_cancelAuthenticationChallenge rec_))
    else if queriedSel == sel_performDefaultHandlingForAuthenticationChallenge then pure (maybe 0 (const 1) (_performDefaultHandlingForAuthenticationChallenge rec_))
    else if queriedSel == sel_rejectProtectionSpaceAndContinueWithChallenge then pure (maybe 0 (const 1) (_rejectProtectionSpaceAndContinueWithChallenge rec_))
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
newNSURLAuthenticationChallengeSender :: NSURLAuthenticationChallengeSenderOverrides -> IO RawId
newNSURLAuthenticationChallengeSender overrides = do
  inst <- class_createInstance nsurlAuthenticationChallengeSenderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
