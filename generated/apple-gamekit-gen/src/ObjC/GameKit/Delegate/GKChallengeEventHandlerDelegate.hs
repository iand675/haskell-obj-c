{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKChallengeEventHandlerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newGKChallengeEventHandlerDelegate defaultGKChallengeEventHandlerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameKit.Delegate.GKChallengeEventHandlerDelegate
  ( GKChallengeEventHandlerDelegateOverrides(..)
  , defaultGKChallengeEventHandlerDelegateOverrides
  , newGKChallengeEventHandlerDelegate
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

-- | Overrides record for @\@protocol GKChallengeEventHandlerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKChallengeEventHandlerDelegateOverrides = GKChallengeEventHandlerDelegateOverrides
  { _localPlayerDidSelectChallenge :: !(Maybe (RawId -> IO ()))
  , _shouldShowBannerForLocallyReceivedChallenge :: !(Maybe (RawId -> IO Bool))
  , _localPlayerDidReceiveChallenge :: !(Maybe (RawId -> IO ()))
  , _shouldShowBannerForLocallyCompletedChallenge :: !(Maybe (RawId -> IO Bool))
  , _localPlayerDidCompleteChallenge :: !(Maybe (RawId -> IO ()))
  , _shouldShowBannerForRemotelyCompletedChallenge :: !(Maybe (RawId -> IO Bool))
  , _remotePlayerDidCompleteChallenge :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultGKChallengeEventHandlerDelegateOverrides :: GKChallengeEventHandlerDelegateOverrides
defaultGKChallengeEventHandlerDelegateOverrides = GKChallengeEventHandlerDelegateOverrides
  { _localPlayerDidSelectChallenge = Nothing
  , _shouldShowBannerForLocallyReceivedChallenge = Nothing
  , _localPlayerDidReceiveChallenge = Nothing
  , _shouldShowBannerForLocallyCompletedChallenge = Nothing
  , _localPlayerDidCompleteChallenge = Nothing
  , _shouldShowBannerForRemotelyCompletedChallenge = Nothing
  , _remotePlayerDidCompleteChallenge = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkChallengeEventHandlerDelegateDelegateClass #-}
gkChallengeEventHandlerDelegateDelegateClass :: Class
gkChallengeEventHandlerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKChallengeEventHandlerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_localPlayerDidSelectChallenge = unSelector (mkSelector "localPlayerDidSelectChallenge:")
      sel_shouldShowBannerForLocallyReceivedChallenge = unSelector (mkSelector "shouldShowBannerForLocallyReceivedChallenge:")
      sel_localPlayerDidReceiveChallenge = unSelector (mkSelector "localPlayerDidReceiveChallenge:")
      sel_shouldShowBannerForLocallyCompletedChallenge = unSelector (mkSelector "shouldShowBannerForLocallyCompletedChallenge:")
      sel_localPlayerDidCompleteChallenge = unSelector (mkSelector "localPlayerDidCompleteChallenge:")
      sel_shouldShowBannerForRemotelyCompletedChallenge = unSelector (mkSelector "shouldShowBannerForRemotelyCompletedChallenge:")
      sel_remotePlayerDidCompleteChallenge = unSelector (mkSelector "remotePlayerDidCompleteChallenge:")
  -- localPlayerDidSelectChallenge:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKChallengeEventHandlerDelegateOverrides
    case _localPlayerDidSelectChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "localPlayerDidSelectChallenge:" "v@:@" stub_0

  -- shouldShowBannerForLocallyReceivedChallenge:
  stub_1 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKChallengeEventHandlerDelegateOverrides
    case _shouldShowBannerForLocallyReceivedChallenge rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldShowBannerForLocallyReceivedChallenge:" "B@:@" stub_1

  -- localPlayerDidReceiveChallenge:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKChallengeEventHandlerDelegateOverrides
    case _localPlayerDidReceiveChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "localPlayerDidReceiveChallenge:" "v@:@" stub_2

  -- shouldShowBannerForLocallyCompletedChallenge:
  stub_3 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKChallengeEventHandlerDelegateOverrides
    case _shouldShowBannerForLocallyCompletedChallenge rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldShowBannerForLocallyCompletedChallenge:" "B@:@" stub_3

  -- localPlayerDidCompleteChallenge:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKChallengeEventHandlerDelegateOverrides
    case _localPlayerDidCompleteChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "localPlayerDidCompleteChallenge:" "v@:@" stub_4

  -- shouldShowBannerForRemotelyCompletedChallenge:
  stub_5 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKChallengeEventHandlerDelegateOverrides
    case _shouldShowBannerForRemotelyCompletedChallenge rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldShowBannerForRemotelyCompletedChallenge:" "B@:@" stub_5

  -- remotePlayerDidCompleteChallenge:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKChallengeEventHandlerDelegateOverrides
    case _remotePlayerDidCompleteChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "remotePlayerDidCompleteChallenge:" "v@:@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKChallengeEventHandlerDelegateOverrides
    if queriedSel == sel_localPlayerDidSelectChallenge then pure (maybe 0 (const 1) (_localPlayerDidSelectChallenge rec_))
    else if queriedSel == sel_shouldShowBannerForLocallyReceivedChallenge then pure (maybe 0 (const 1) (_shouldShowBannerForLocallyReceivedChallenge rec_))
    else if queriedSel == sel_localPlayerDidReceiveChallenge then pure (maybe 0 (const 1) (_localPlayerDidReceiveChallenge rec_))
    else if queriedSel == sel_shouldShowBannerForLocallyCompletedChallenge then pure (maybe 0 (const 1) (_shouldShowBannerForLocallyCompletedChallenge rec_))
    else if queriedSel == sel_localPlayerDidCompleteChallenge then pure (maybe 0 (const 1) (_localPlayerDidCompleteChallenge rec_))
    else if queriedSel == sel_shouldShowBannerForRemotelyCompletedChallenge then pure (maybe 0 (const 1) (_shouldShowBannerForRemotelyCompletedChallenge rec_))
    else if queriedSel == sel_remotePlayerDidCompleteChallenge then pure (maybe 0 (const 1) (_remotePlayerDidCompleteChallenge rec_))
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
newGKChallengeEventHandlerDelegate :: GKChallengeEventHandlerDelegateOverrides -> IO RawId
newGKChallengeEventHandlerDelegate overrides = do
  inst <- class_createInstance gkChallengeEventHandlerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
