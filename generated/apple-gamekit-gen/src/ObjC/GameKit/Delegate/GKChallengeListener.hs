{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKChallengeListener@.
--
-- Usage:
--
-- @
-- delegate <- newGKChallengeListener defaultGKChallengeListenerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameKit.Delegate.GKChallengeListener
  ( GKChallengeListenerOverrides(..)
  , defaultGKChallengeListenerOverrides
  , newGKChallengeListener
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

-- | Overrides record for @\@protocol GKChallengeListener@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKChallengeListenerOverrides = GKChallengeListenerOverrides
  { _player_wantsToPlayChallenge :: !(Maybe (RawId -> RawId -> IO ()))
  , _player_didReceiveChallenge :: !(Maybe (RawId -> RawId -> IO ()))
  , _player_didCompleteChallenge_issuedByFriend :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _player_issuedChallengeWasCompleted_byFriend :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultGKChallengeListenerOverrides :: GKChallengeListenerOverrides
defaultGKChallengeListenerOverrides = GKChallengeListenerOverrides
  { _player_wantsToPlayChallenge = Nothing
  , _player_didReceiveChallenge = Nothing
  , _player_didCompleteChallenge_issuedByFriend = Nothing
  , _player_issuedChallengeWasCompleted_byFriend = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkChallengeListenerDelegateClass #-}
gkChallengeListenerDelegateClass :: Class
gkChallengeListenerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKChallengeListener" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_player_wantsToPlayChallenge = unSelector (mkSelector "player:wantsToPlayChallenge:")
      sel_player_didReceiveChallenge = unSelector (mkSelector "player:didReceiveChallenge:")
      sel_player_didCompleteChallenge_issuedByFriend = unSelector (mkSelector "player:didCompleteChallenge:issuedByFriend:")
      sel_player_issuedChallengeWasCompleted_byFriend = unSelector (mkSelector "player:issuedChallengeWasCompleted:byFriend:")
  -- player:wantsToPlayChallenge:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKChallengeListenerOverrides
    case _player_wantsToPlayChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "player:wantsToPlayChallenge:" "v@:@@" stub_0

  -- player:didReceiveChallenge:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKChallengeListenerOverrides
    case _player_didReceiveChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "player:didReceiveChallenge:" "v@:@@" stub_1

  -- player:didCompleteChallenge:issuedByFriend:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKChallengeListenerOverrides
    case _player_didCompleteChallenge_issuedByFriend rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "player:didCompleteChallenge:issuedByFriend:" "v@:@@@" stub_2

  -- player:issuedChallengeWasCompleted:byFriend:
  stub_3 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKChallengeListenerOverrides
    case _player_issuedChallengeWasCompleted_byFriend rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "player:issuedChallengeWasCompleted:byFriend:" "v@:@@@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKChallengeListenerOverrides
    if queriedSel == sel_player_wantsToPlayChallenge then pure (maybe 0 (const 1) (_player_wantsToPlayChallenge rec_))
    else if queriedSel == sel_player_didReceiveChallenge then pure (maybe 0 (const 1) (_player_didReceiveChallenge rec_))
    else if queriedSel == sel_player_didCompleteChallenge_issuedByFriend then pure (maybe 0 (const 1) (_player_didCompleteChallenge_issuedByFriend rec_))
    else if queriedSel == sel_player_issuedChallengeWasCompleted_byFriend then pure (maybe 0 (const 1) (_player_issuedChallengeWasCompleted_byFriend rec_))
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
newGKChallengeListener :: GKChallengeListenerOverrides -> IO RawId
newGKChallengeListener overrides = do
  inst <- class_createInstance gkChallengeListenerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
