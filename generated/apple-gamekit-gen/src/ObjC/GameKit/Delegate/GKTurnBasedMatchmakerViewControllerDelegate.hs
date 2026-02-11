{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKTurnBasedMatchmakerViewControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newGKTurnBasedMatchmakerViewControllerDelegate defaultGKTurnBasedMatchmakerViewControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameKit.Delegate.GKTurnBasedMatchmakerViewControllerDelegate
  ( GKTurnBasedMatchmakerViewControllerDelegateOverrides(..)
  , defaultGKTurnBasedMatchmakerViewControllerDelegateOverrides
  , newGKTurnBasedMatchmakerViewControllerDelegate
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

-- | Overrides record for @\@protocol GKTurnBasedMatchmakerViewControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKTurnBasedMatchmakerViewControllerDelegateOverrides = GKTurnBasedMatchmakerViewControllerDelegateOverrides
  { _turnBasedMatchmakerViewControllerWasCancelled :: !(Maybe (RawId -> IO ()))
  , _turnBasedMatchmakerViewController_didFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _turnBasedMatchmakerViewController_didFindMatch :: !(Maybe (RawId -> RawId -> IO ()))
  , _turnBasedMatchmakerViewController_playerQuitForMatch :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultGKTurnBasedMatchmakerViewControllerDelegateOverrides :: GKTurnBasedMatchmakerViewControllerDelegateOverrides
defaultGKTurnBasedMatchmakerViewControllerDelegateOverrides = GKTurnBasedMatchmakerViewControllerDelegateOverrides
  { _turnBasedMatchmakerViewControllerWasCancelled = Nothing
  , _turnBasedMatchmakerViewController_didFailWithError = Nothing
  , _turnBasedMatchmakerViewController_didFindMatch = Nothing
  , _turnBasedMatchmakerViewController_playerQuitForMatch = Nothing
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
{-# NOINLINE gkTurnBasedMatchmakerViewControllerDelegateDelegateClass #-}
gkTurnBasedMatchmakerViewControllerDelegateDelegateClass :: Class
gkTurnBasedMatchmakerViewControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKTurnBasedMatchmakerViewControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_turnBasedMatchmakerViewControllerWasCancelled = unSelector (mkSelector "turnBasedMatchmakerViewControllerWasCancelled:")
      sel_turnBasedMatchmakerViewController_didFailWithError = unSelector (mkSelector "turnBasedMatchmakerViewController:didFailWithError:")
      sel_turnBasedMatchmakerViewController_didFindMatch = unSelector (mkSelector "turnBasedMatchmakerViewController:didFindMatch:")
      sel_turnBasedMatchmakerViewController_playerQuitForMatch = unSelector (mkSelector "turnBasedMatchmakerViewController:playerQuitForMatch:")
  -- turnBasedMatchmakerViewControllerWasCancelled:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedMatchmakerViewControllerDelegateOverrides
    case _turnBasedMatchmakerViewControllerWasCancelled rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "turnBasedMatchmakerViewControllerWasCancelled:" "v@:@" stub_0

  -- turnBasedMatchmakerViewController:didFailWithError:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedMatchmakerViewControllerDelegateOverrides
    case _turnBasedMatchmakerViewController_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "turnBasedMatchmakerViewController:didFailWithError:" "v@:@@" stub_1

  -- turnBasedMatchmakerViewController:didFindMatch:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedMatchmakerViewControllerDelegateOverrides
    case _turnBasedMatchmakerViewController_didFindMatch rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "turnBasedMatchmakerViewController:didFindMatch:" "v@:@@" stub_2

  -- turnBasedMatchmakerViewController:playerQuitForMatch:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedMatchmakerViewControllerDelegateOverrides
    case _turnBasedMatchmakerViewController_playerQuitForMatch rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "turnBasedMatchmakerViewController:playerQuitForMatch:" "v@:@@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedMatchmakerViewControllerDelegateOverrides
    if queriedSel == sel_turnBasedMatchmakerViewControllerWasCancelled then pure (maybe 0 (const 1) (_turnBasedMatchmakerViewControllerWasCancelled rec_))
    else if queriedSel == sel_turnBasedMatchmakerViewController_didFailWithError then pure (maybe 0 (const 1) (_turnBasedMatchmakerViewController_didFailWithError rec_))
    else if queriedSel == sel_turnBasedMatchmakerViewController_didFindMatch then pure (maybe 0 (const 1) (_turnBasedMatchmakerViewController_didFindMatch rec_))
    else if queriedSel == sel_turnBasedMatchmakerViewController_playerQuitForMatch then pure (maybe 0 (const 1) (_turnBasedMatchmakerViewController_playerQuitForMatch rec_))
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
newGKTurnBasedMatchmakerViewControllerDelegate :: GKTurnBasedMatchmakerViewControllerDelegateOverrides -> IO RawId
newGKTurnBasedMatchmakerViewControllerDelegate overrides = do
  inst <- class_createInstance gkTurnBasedMatchmakerViewControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
