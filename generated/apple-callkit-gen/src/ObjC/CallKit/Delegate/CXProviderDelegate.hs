{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CXProviderDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newCXProviderDelegate defaultCXProviderDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CallKit.Delegate.CXProviderDelegate
  ( CXProviderDelegateOverrides(..)
  , defaultCXProviderDelegateOverrides
  , newCXProviderDelegate
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

-- | Overrides record for @\@protocol CXProviderDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CXProviderDelegateOverrides = CXProviderDelegateOverrides
  { _providerDidReset :: !(Maybe (RawId -> IO ()))
  , _providerDidBegin :: !(Maybe (RawId -> IO ()))
  , _provider_executeTransaction :: !(Maybe (RawId -> RawId -> IO Bool))
  , _provider_performStartCallAction :: !(Maybe (RawId -> RawId -> IO ()))
  , _provider_performAnswerCallAction :: !(Maybe (RawId -> RawId -> IO ()))
  , _provider_performEndCallAction :: !(Maybe (RawId -> RawId -> IO ()))
  , _provider_performSetHeldCallAction :: !(Maybe (RawId -> RawId -> IO ()))
  , _provider_performSetMutedCallAction :: !(Maybe (RawId -> RawId -> IO ()))
  , _provider_performSetGroupCallAction :: !(Maybe (RawId -> RawId -> IO ()))
  , _provider_performPlayDTMFCallAction :: !(Maybe (RawId -> RawId -> IO ()))
  , _provider_timedOutPerformingAction :: !(Maybe (RawId -> RawId -> IO ()))
  , _provider_didActivateAudioSession :: !(Maybe (RawId -> RawId -> IO ()))
  , _provider_didDeactivateAudioSession :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCXProviderDelegateOverrides :: CXProviderDelegateOverrides
defaultCXProviderDelegateOverrides = CXProviderDelegateOverrides
  { _providerDidReset = Nothing
  , _providerDidBegin = Nothing
  , _provider_executeTransaction = Nothing
  , _provider_performStartCallAction = Nothing
  , _provider_performAnswerCallAction = Nothing
  , _provider_performEndCallAction = Nothing
  , _provider_performSetHeldCallAction = Nothing
  , _provider_performSetMutedCallAction = Nothing
  , _provider_performSetGroupCallAction = Nothing
  , _provider_performPlayDTMFCallAction = Nothing
  , _provider_timedOutPerformingAction = Nothing
  , _provider_didActivateAudioSession = Nothing
  , _provider_didDeactivateAudioSession = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE cxProviderDelegateDelegateClass #-}
cxProviderDelegateDelegateClass :: Class
cxProviderDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCXProviderDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_providerDidReset = unSelector (mkSelector "providerDidReset:")
      sel_providerDidBegin = unSelector (mkSelector "providerDidBegin:")
      sel_provider_executeTransaction = unSelector (mkSelector "provider:executeTransaction:")
      sel_provider_performStartCallAction = unSelector (mkSelector "provider:performStartCallAction:")
      sel_provider_performAnswerCallAction = unSelector (mkSelector "provider:performAnswerCallAction:")
      sel_provider_performEndCallAction = unSelector (mkSelector "provider:performEndCallAction:")
      sel_provider_performSetHeldCallAction = unSelector (mkSelector "provider:performSetHeldCallAction:")
      sel_provider_performSetMutedCallAction = unSelector (mkSelector "provider:performSetMutedCallAction:")
      sel_provider_performSetGroupCallAction = unSelector (mkSelector "provider:performSetGroupCallAction:")
      sel_provider_performPlayDTMFCallAction = unSelector (mkSelector "provider:performPlayDTMFCallAction:")
      sel_provider_timedOutPerformingAction = unSelector (mkSelector "provider:timedOutPerformingAction:")
      sel_provider_didActivateAudioSession = unSelector (mkSelector "provider:didActivateAudioSession:")
      sel_provider_didDeactivateAudioSession = unSelector (mkSelector "provider:didDeactivateAudioSession:")
  -- providerDidReset:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    case _providerDidReset rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "providerDidReset:" "v@:@" stub_0

  -- providerDidBegin:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    case _providerDidBegin rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "providerDidBegin:" "v@:@" stub_1

  -- provider:executeTransaction:
  stub_2 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    case _provider_executeTransaction rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "provider:executeTransaction:" "B@:@@" stub_2

  -- provider:performStartCallAction:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    case _provider_performStartCallAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "provider:performStartCallAction:" "v@:@@" stub_3

  -- provider:performAnswerCallAction:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    case _provider_performAnswerCallAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "provider:performAnswerCallAction:" "v@:@@" stub_4

  -- provider:performEndCallAction:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    case _provider_performEndCallAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "provider:performEndCallAction:" "v@:@@" stub_5

  -- provider:performSetHeldCallAction:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    case _provider_performSetHeldCallAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "provider:performSetHeldCallAction:" "v@:@@" stub_6

  -- provider:performSetMutedCallAction:
  stub_7 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    case _provider_performSetMutedCallAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "provider:performSetMutedCallAction:" "v@:@@" stub_7

  -- provider:performSetGroupCallAction:
  stub_8 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    case _provider_performSetGroupCallAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "provider:performSetGroupCallAction:" "v@:@@" stub_8

  -- provider:performPlayDTMFCallAction:
  stub_9 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    case _provider_performPlayDTMFCallAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "provider:performPlayDTMFCallAction:" "v@:@@" stub_9

  -- provider:timedOutPerformingAction:
  stub_10 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    case _provider_timedOutPerformingAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "provider:timedOutPerformingAction:" "v@:@@" stub_10

  -- provider:didActivateAudioSession:
  stub_11 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    case _provider_didActivateAudioSession rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "provider:didActivateAudioSession:" "v@:@@" stub_11

  -- provider:didDeactivateAudioSession:
  stub_12 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    case _provider_didDeactivateAudioSession rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "provider:didDeactivateAudioSession:" "v@:@@" stub_12

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXProviderDelegateOverrides
    if queriedSel == sel_providerDidReset then pure (maybe 0 (const 1) (_providerDidReset rec_))
    else if queriedSel == sel_providerDidBegin then pure (maybe 0 (const 1) (_providerDidBegin rec_))
    else if queriedSel == sel_provider_executeTransaction then pure (maybe 0 (const 1) (_provider_executeTransaction rec_))
    else if queriedSel == sel_provider_performStartCallAction then pure (maybe 0 (const 1) (_provider_performStartCallAction rec_))
    else if queriedSel == sel_provider_performAnswerCallAction then pure (maybe 0 (const 1) (_provider_performAnswerCallAction rec_))
    else if queriedSel == sel_provider_performEndCallAction then pure (maybe 0 (const 1) (_provider_performEndCallAction rec_))
    else if queriedSel == sel_provider_performSetHeldCallAction then pure (maybe 0 (const 1) (_provider_performSetHeldCallAction rec_))
    else if queriedSel == sel_provider_performSetMutedCallAction then pure (maybe 0 (const 1) (_provider_performSetMutedCallAction rec_))
    else if queriedSel == sel_provider_performSetGroupCallAction then pure (maybe 0 (const 1) (_provider_performSetGroupCallAction rec_))
    else if queriedSel == sel_provider_performPlayDTMFCallAction then pure (maybe 0 (const 1) (_provider_performPlayDTMFCallAction rec_))
    else if queriedSel == sel_provider_timedOutPerformingAction then pure (maybe 0 (const 1) (_provider_timedOutPerformingAction rec_))
    else if queriedSel == sel_provider_didActivateAudioSession then pure (maybe 0 (const 1) (_provider_didActivateAudioSession rec_))
    else if queriedSel == sel_provider_didDeactivateAudioSession then pure (maybe 0 (const 1) (_provider_didDeactivateAudioSession rec_))
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
newCXProviderDelegate :: CXProviderDelegateOverrides -> IO RawId
newCXProviderDelegate overrides = do
  inst <- class_createInstance cxProviderDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
