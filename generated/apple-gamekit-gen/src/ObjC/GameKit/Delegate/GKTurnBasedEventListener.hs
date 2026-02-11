{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKTurnBasedEventListener@.
--
-- Usage:
--
-- @
-- delegate <- newGKTurnBasedEventListener defaultGKTurnBasedEventListenerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameKit.Delegate.GKTurnBasedEventListener
  ( GKTurnBasedEventListenerOverrides(..)
  , defaultGKTurnBasedEventListenerOverrides
  , newGKTurnBasedEventListener
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

-- | Overrides record for @\@protocol GKTurnBasedEventListener@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKTurnBasedEventListenerOverrides = GKTurnBasedEventListenerOverrides
  { _player_didRequestMatchWithOtherPlayers :: !(Maybe (RawId -> RawId -> IO ()))
  , _player_receivedTurnEventForMatch_didBecomeActive :: !(Maybe (RawId -> RawId -> Bool -> IO ()))
  , _player_matchEnded :: !(Maybe (RawId -> RawId -> IO ()))
  , _player_receivedExchangeRequest_forMatch :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _player_receivedExchangeCancellation_forMatch :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _player_receivedExchangeReplies_forCompletedExchange_forMatch :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _player_wantsToQuitMatch :: !(Maybe (RawId -> RawId -> IO ()))
  , _player_didRequestMatchWithPlayers :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultGKTurnBasedEventListenerOverrides :: GKTurnBasedEventListenerOverrides
defaultGKTurnBasedEventListenerOverrides = GKTurnBasedEventListenerOverrides
  { _player_didRequestMatchWithOtherPlayers = Nothing
  , _player_receivedTurnEventForMatch_didBecomeActive = Nothing
  , _player_matchEnded = Nothing
  , _player_receivedExchangeRequest_forMatch = Nothing
  , _player_receivedExchangeCancellation_forMatch = Nothing
  , _player_receivedExchangeReplies_forCompletedExchange_forMatch = Nothing
  , _player_wantsToQuitMatch = Nothing
  , _player_didRequestMatchWithPlayers = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkTurnBasedEventListenerDelegateClass #-}
gkTurnBasedEventListenerDelegateClass :: Class
gkTurnBasedEventListenerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKTurnBasedEventListener" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_player_didRequestMatchWithOtherPlayers = unSelector (mkSelector "player:didRequestMatchWithOtherPlayers:")
      sel_player_receivedTurnEventForMatch_didBecomeActive = unSelector (mkSelector "player:receivedTurnEventForMatch:didBecomeActive:")
      sel_player_matchEnded = unSelector (mkSelector "player:matchEnded:")
      sel_player_receivedExchangeRequest_forMatch = unSelector (mkSelector "player:receivedExchangeRequest:forMatch:")
      sel_player_receivedExchangeCancellation_forMatch = unSelector (mkSelector "player:receivedExchangeCancellation:forMatch:")
      sel_player_receivedExchangeReplies_forCompletedExchange_forMatch = unSelector (mkSelector "player:receivedExchangeReplies:forCompletedExchange:forMatch:")
      sel_player_wantsToQuitMatch = unSelector (mkSelector "player:wantsToQuitMatch:")
      sel_player_didRequestMatchWithPlayers = unSelector (mkSelector "player:didRequestMatchWithPlayers:")
  -- player:didRequestMatchWithOtherPlayers:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventListenerOverrides
    case _player_didRequestMatchWithOtherPlayers rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "player:didRequestMatchWithOtherPlayers:" "v@:@@" stub_0

  -- player:receivedTurnEventForMatch:didBecomeActive:
  stub_1 <- wrap_at_at_B_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventListenerOverrides
    case _player_receivedTurnEventForMatch_didBecomeActive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (arg2 /= 0)
  addObjCMethod cls "player:receivedTurnEventForMatch:didBecomeActive:" "v@:@@B" stub_1

  -- player:matchEnded:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventListenerOverrides
    case _player_matchEnded rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "player:matchEnded:" "v@:@@" stub_2

  -- player:receivedExchangeRequest:forMatch:
  stub_3 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventListenerOverrides
    case _player_receivedExchangeRequest_forMatch rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "player:receivedExchangeRequest:forMatch:" "v@:@@@" stub_3

  -- player:receivedExchangeCancellation:forMatch:
  stub_4 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventListenerOverrides
    case _player_receivedExchangeCancellation_forMatch rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "player:receivedExchangeCancellation:forMatch:" "v@:@@@" stub_4

  -- player:receivedExchangeReplies:forCompletedExchange:forMatch:
  stub_5 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventListenerOverrides
    case _player_receivedExchangeReplies_forCompletedExchange_forMatch rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "player:receivedExchangeReplies:forCompletedExchange:forMatch:" "v@:@@@@" stub_5

  -- player:wantsToQuitMatch:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventListenerOverrides
    case _player_wantsToQuitMatch rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "player:wantsToQuitMatch:" "v@:@@" stub_6

  -- player:didRequestMatchWithPlayers:
  stub_7 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventListenerOverrides
    case _player_didRequestMatchWithPlayers rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "player:didRequestMatchWithPlayers:" "v@:@@" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventListenerOverrides
    if queriedSel == sel_player_didRequestMatchWithOtherPlayers then pure (maybe 0 (const 1) (_player_didRequestMatchWithOtherPlayers rec_))
    else if queriedSel == sel_player_receivedTurnEventForMatch_didBecomeActive then pure (maybe 0 (const 1) (_player_receivedTurnEventForMatch_didBecomeActive rec_))
    else if queriedSel == sel_player_matchEnded then pure (maybe 0 (const 1) (_player_matchEnded rec_))
    else if queriedSel == sel_player_receivedExchangeRequest_forMatch then pure (maybe 0 (const 1) (_player_receivedExchangeRequest_forMatch rec_))
    else if queriedSel == sel_player_receivedExchangeCancellation_forMatch then pure (maybe 0 (const 1) (_player_receivedExchangeCancellation_forMatch rec_))
    else if queriedSel == sel_player_receivedExchangeReplies_forCompletedExchange_forMatch then pure (maybe 0 (const 1) (_player_receivedExchangeReplies_forCompletedExchange_forMatch rec_))
    else if queriedSel == sel_player_wantsToQuitMatch then pure (maybe 0 (const 1) (_player_wantsToQuitMatch rec_))
    else if queriedSel == sel_player_didRequestMatchWithPlayers then pure (maybe 0 (const 1) (_player_didRequestMatchWithPlayers rec_))
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
newGKTurnBasedEventListener :: GKTurnBasedEventListenerOverrides -> IO RawId
newGKTurnBasedEventListener overrides = do
  inst <- class_createInstance gkTurnBasedEventListenerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
