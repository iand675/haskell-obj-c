{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKMatchmakerViewControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newGKMatchmakerViewControllerDelegate defaultGKMatchmakerViewControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameKit.Delegate.GKMatchmakerViewControllerDelegate
  ( GKMatchmakerViewControllerDelegateOverrides(..)
  , defaultGKMatchmakerViewControllerDelegateOverrides
  , newGKMatchmakerViewControllerDelegate
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

-- | Overrides record for @\@protocol GKMatchmakerViewControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKMatchmakerViewControllerDelegateOverrides = GKMatchmakerViewControllerDelegateOverrides
  { _matchmakerViewControllerWasCancelled :: !(Maybe (RawId -> IO ()))
  , _matchmakerViewController_didFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _matchmakerViewController_didFindMatch :: !(Maybe (RawId -> RawId -> IO ()))
  , _matchmakerViewController_didFindHostedPlayers :: !(Maybe (RawId -> RawId -> IO ()))
  , _matchmakerViewController_hostedPlayerDidAccept :: !(Maybe (RawId -> RawId -> IO ()))
  , _matchmakerViewController_didFindPlayers :: !(Maybe (RawId -> RawId -> IO ()))
  , _matchmakerViewController_didReceiveAcceptFromHostedPlayer :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultGKMatchmakerViewControllerDelegateOverrides :: GKMatchmakerViewControllerDelegateOverrides
defaultGKMatchmakerViewControllerDelegateOverrides = GKMatchmakerViewControllerDelegateOverrides
  { _matchmakerViewControllerWasCancelled = Nothing
  , _matchmakerViewController_didFailWithError = Nothing
  , _matchmakerViewController_didFindMatch = Nothing
  , _matchmakerViewController_didFindHostedPlayers = Nothing
  , _matchmakerViewController_hostedPlayerDidAccept = Nothing
  , _matchmakerViewController_didFindPlayers = Nothing
  , _matchmakerViewController_didReceiveAcceptFromHostedPlayer = Nothing
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
{-# NOINLINE gkMatchmakerViewControllerDelegateDelegateClass #-}
gkMatchmakerViewControllerDelegateDelegateClass :: Class
gkMatchmakerViewControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKMatchmakerViewControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_matchmakerViewControllerWasCancelled = unSelector (mkSelector "matchmakerViewControllerWasCancelled:")
      sel_matchmakerViewController_didFailWithError = unSelector (mkSelector "matchmakerViewController:didFailWithError:")
      sel_matchmakerViewController_didFindMatch = unSelector (mkSelector "matchmakerViewController:didFindMatch:")
      sel_matchmakerViewController_didFindHostedPlayers = unSelector (mkSelector "matchmakerViewController:didFindHostedPlayers:")
      sel_matchmakerViewController_hostedPlayerDidAccept = unSelector (mkSelector "matchmakerViewController:hostedPlayerDidAccept:")
      sel_matchmakerViewController_didFindPlayers = unSelector (mkSelector "matchmakerViewController:didFindPlayers:")
      sel_matchmakerViewController_didReceiveAcceptFromHostedPlayer = unSelector (mkSelector "matchmakerViewController:didReceiveAcceptFromHostedPlayer:")
  -- matchmakerViewControllerWasCancelled:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchmakerViewControllerDelegateOverrides
    case _matchmakerViewControllerWasCancelled rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "matchmakerViewControllerWasCancelled:" "v@:@" stub_0

  -- matchmakerViewController:didFailWithError:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchmakerViewControllerDelegateOverrides
    case _matchmakerViewController_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "matchmakerViewController:didFailWithError:" "v@:@@" stub_1

  -- matchmakerViewController:didFindMatch:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchmakerViewControllerDelegateOverrides
    case _matchmakerViewController_didFindMatch rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "matchmakerViewController:didFindMatch:" "v@:@@" stub_2

  -- matchmakerViewController:didFindHostedPlayers:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchmakerViewControllerDelegateOverrides
    case _matchmakerViewController_didFindHostedPlayers rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "matchmakerViewController:didFindHostedPlayers:" "v@:@@" stub_3

  -- matchmakerViewController:hostedPlayerDidAccept:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchmakerViewControllerDelegateOverrides
    case _matchmakerViewController_hostedPlayerDidAccept rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "matchmakerViewController:hostedPlayerDidAccept:" "v@:@@" stub_4

  -- matchmakerViewController:didFindPlayers:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchmakerViewControllerDelegateOverrides
    case _matchmakerViewController_didFindPlayers rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "matchmakerViewController:didFindPlayers:" "v@:@@" stub_5

  -- matchmakerViewController:didReceiveAcceptFromHostedPlayer:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchmakerViewControllerDelegateOverrides
    case _matchmakerViewController_didReceiveAcceptFromHostedPlayer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "matchmakerViewController:didReceiveAcceptFromHostedPlayer:" "v@:@@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchmakerViewControllerDelegateOverrides
    if queriedSel == sel_matchmakerViewControllerWasCancelled then pure (maybe 0 (const 1) (_matchmakerViewControllerWasCancelled rec_))
    else if queriedSel == sel_matchmakerViewController_didFailWithError then pure (maybe 0 (const 1) (_matchmakerViewController_didFailWithError rec_))
    else if queriedSel == sel_matchmakerViewController_didFindMatch then pure (maybe 0 (const 1) (_matchmakerViewController_didFindMatch rec_))
    else if queriedSel == sel_matchmakerViewController_didFindHostedPlayers then pure (maybe 0 (const 1) (_matchmakerViewController_didFindHostedPlayers rec_))
    else if queriedSel == sel_matchmakerViewController_hostedPlayerDidAccept then pure (maybe 0 (const 1) (_matchmakerViewController_hostedPlayerDidAccept rec_))
    else if queriedSel == sel_matchmakerViewController_didFindPlayers then pure (maybe 0 (const 1) (_matchmakerViewController_didFindPlayers rec_))
    else if queriedSel == sel_matchmakerViewController_didReceiveAcceptFromHostedPlayer then pure (maybe 0 (const 1) (_matchmakerViewController_didReceiveAcceptFromHostedPlayer rec_))
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
newGKMatchmakerViewControllerDelegate :: GKMatchmakerViewControllerDelegateOverrides -> IO RawId
newGKMatchmakerViewControllerDelegate overrides = do
  inst <- class_createInstance gkMatchmakerViewControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
