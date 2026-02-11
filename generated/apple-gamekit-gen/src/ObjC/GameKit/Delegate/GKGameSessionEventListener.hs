{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKGameSessionEventListener@.
--
-- Usage:
--
-- @
-- delegate <- newGKGameSessionEventListener defaultGKGameSessionEventListenerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameKit.Delegate.GKGameSessionEventListener
  ( GKGameSessionEventListenerOverrides(..)
  , defaultGKGameSessionEventListenerOverrides
  , newGKGameSessionEventListener
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

-- | Overrides record for @\@protocol GKGameSessionEventListener@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKGameSessionEventListenerOverrides = GKGameSessionEventListenerOverrides
  { _session_didAddPlayer :: !(Maybe (RawId -> RawId -> IO ()))
  , _session_didRemovePlayer :: !(Maybe (RawId -> RawId -> IO ()))
  , _session_player_didSaveData :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _session_didReceiveData_fromPlayer :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _session_didReceiveMessage_withData_fromPlayer :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultGKGameSessionEventListenerOverrides :: GKGameSessionEventListenerOverrides
defaultGKGameSessionEventListenerOverrides = GKGameSessionEventListenerOverrides
  { _session_didAddPlayer = Nothing
  , _session_didRemovePlayer = Nothing
  , _session_player_didSaveData = Nothing
  , _session_didReceiveData_fromPlayer = Nothing
  , _session_didReceiveMessage_withData_fromPlayer = Nothing
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
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkGameSessionEventListenerDelegateClass #-}
gkGameSessionEventListenerDelegateClass :: Class
gkGameSessionEventListenerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKGameSessionEventListener" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_session_didAddPlayer = unSelector (mkSelector "session:didAddPlayer:")
      sel_session_didRemovePlayer = unSelector (mkSelector "session:didRemovePlayer:")
      sel_session_player_didSaveData = unSelector (mkSelector "session:player:didSaveData:")
      sel_session_didReceiveData_fromPlayer = unSelector (mkSelector "session:didReceiveData:fromPlayer:")
      sel_session_didReceiveMessage_withData_fromPlayer = unSelector (mkSelector "session:didReceiveMessage:withData:fromPlayer:")
  -- session:didAddPlayer:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameSessionEventListenerOverrides
    case _session_didAddPlayer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "session:didAddPlayer:" "v@:@@" stub_0

  -- session:didRemovePlayer:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameSessionEventListenerOverrides
    case _session_didRemovePlayer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "session:didRemovePlayer:" "v@:@@" stub_1

  -- session:player:didSaveData:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameSessionEventListenerOverrides
    case _session_player_didSaveData rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "session:player:didSaveData:" "v@:@@@" stub_2

  -- session:didReceiveData:fromPlayer:
  stub_3 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameSessionEventListenerOverrides
    case _session_didReceiveData_fromPlayer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "session:didReceiveData:fromPlayer:" "v@:@@@" stub_3

  -- session:didReceiveMessage:withData:fromPlayer:
  stub_4 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameSessionEventListenerOverrides
    case _session_didReceiveMessage_withData_fromPlayer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "session:didReceiveMessage:withData:fromPlayer:" "v@:@@@@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameSessionEventListenerOverrides
    if queriedSel == sel_session_didAddPlayer then pure (maybe 0 (const 1) (_session_didAddPlayer rec_))
    else if queriedSel == sel_session_didRemovePlayer then pure (maybe 0 (const 1) (_session_didRemovePlayer rec_))
    else if queriedSel == sel_session_player_didSaveData then pure (maybe 0 (const 1) (_session_player_didSaveData rec_))
    else if queriedSel == sel_session_didReceiveData_fromPlayer then pure (maybe 0 (const 1) (_session_didReceiveData_fromPlayer rec_))
    else if queriedSel == sel_session_didReceiveMessage_withData_fromPlayer then pure (maybe 0 (const 1) (_session_didReceiveMessage_withData_fromPlayer rec_))
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
newGKGameSessionEventListener :: GKGameSessionEventListenerOverrides -> IO RawId
newGKGameSessionEventListener overrides = do
  inst <- class_createInstance gkGameSessionEventListenerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
