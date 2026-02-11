{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKVoiceChatClient@.
--
-- Usage:
--
-- @
-- delegate <- newGKVoiceChatClient defaultGKVoiceChatClientOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameKit.Delegate.GKVoiceChatClient
  ( GKVoiceChatClientOverrides(..)
  , defaultGKVoiceChatClientOverrides
  , newGKVoiceChatClient
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

-- | Overrides record for @\@protocol GKVoiceChatClient@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKVoiceChatClientOverrides = GKVoiceChatClientOverrides
  { _voiceChatService_sendData_toParticipantID :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _participantID :: !(Maybe (IO RawId))
  , _voiceChatService_sendRealTimeData_toParticipantID :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _voiceChatService_didStartWithParticipantID :: !(Maybe (RawId -> RawId -> IO ()))
  , _voiceChatService_didNotStartWithParticipantID_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _voiceChatService_didStopWithParticipantID_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _voiceChatService_didReceiveInvitationFromParticipantID_callID :: !(Maybe (RawId -> RawId -> Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultGKVoiceChatClientOverrides :: GKVoiceChatClientOverrides
defaultGKVoiceChatClientOverrides = GKVoiceChatClientOverrides
  { _voiceChatService_sendData_toParticipantID = Nothing
  , _participantID = Nothing
  , _voiceChatService_sendRealTimeData_toParticipantID = Nothing
  , _voiceChatService_didStartWithParticipantID = Nothing
  , _voiceChatService_didNotStartWithParticipantID_error = Nothing
  , _voiceChatService_didStopWithParticipantID_error = Nothing
  , _voiceChatService_didReceiveInvitationFromParticipantID_callID = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkVoiceChatClientDelegateClass #-}
gkVoiceChatClientDelegateClass :: Class
gkVoiceChatClientDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKVoiceChatClient" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_voiceChatService_sendData_toParticipantID = unSelector (mkSelector "voiceChatService:sendData:toParticipantID:")
      sel_participantID = unSelector (mkSelector "participantID")
      sel_voiceChatService_sendRealTimeData_toParticipantID = unSelector (mkSelector "voiceChatService:sendRealTimeData:toParticipantID:")
      sel_voiceChatService_didStartWithParticipantID = unSelector (mkSelector "voiceChatService:didStartWithParticipantID:")
      sel_voiceChatService_didNotStartWithParticipantID_error = unSelector (mkSelector "voiceChatService:didNotStartWithParticipantID:error:")
      sel_voiceChatService_didStopWithParticipantID_error = unSelector (mkSelector "voiceChatService:didStopWithParticipantID:error:")
      sel_voiceChatService_didReceiveInvitationFromParticipantID_callID = unSelector (mkSelector "voiceChatService:didReceiveInvitationFromParticipantID:callID:")
  -- voiceChatService:sendData:toParticipantID:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKVoiceChatClientOverrides
    case _voiceChatService_sendData_toParticipantID rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "voiceChatService:sendData:toParticipantID:" "v@:@@@" stub_0

  -- participantID
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKVoiceChatClientOverrides
    case _participantID rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "participantID" "@@:" stub_1

  -- voiceChatService:sendRealTimeData:toParticipantID:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKVoiceChatClientOverrides
    case _voiceChatService_sendRealTimeData_toParticipantID rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "voiceChatService:sendRealTimeData:toParticipantID:" "v@:@@@" stub_2

  -- voiceChatService:didStartWithParticipantID:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKVoiceChatClientOverrides
    case _voiceChatService_didStartWithParticipantID rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "voiceChatService:didStartWithParticipantID:" "v@:@@" stub_3

  -- voiceChatService:didNotStartWithParticipantID:error:
  stub_4 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKVoiceChatClientOverrides
    case _voiceChatService_didNotStartWithParticipantID_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "voiceChatService:didNotStartWithParticipantID:error:" "v@:@@@" stub_4

  -- voiceChatService:didStopWithParticipantID:error:
  stub_5 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKVoiceChatClientOverrides
    case _voiceChatService_didStopWithParticipantID_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "voiceChatService:didStopWithParticipantID:error:" "v@:@@@" stub_5

  -- voiceChatService:didReceiveInvitationFromParticipantID:callID:
  stub_6 <- wrap_at_at_q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKVoiceChatClientOverrides
    case _voiceChatService_didReceiveInvitationFromParticipantID_callID rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2)
  addObjCMethod cls "voiceChatService:didReceiveInvitationFromParticipantID:callID:" "v@:@@q" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKVoiceChatClientOverrides
    if queriedSel == sel_voiceChatService_sendData_toParticipantID then pure (maybe 0 (const 1) (_voiceChatService_sendData_toParticipantID rec_))
    else if queriedSel == sel_participantID then pure (maybe 0 (const 1) (_participantID rec_))
    else if queriedSel == sel_voiceChatService_sendRealTimeData_toParticipantID then pure (maybe 0 (const 1) (_voiceChatService_sendRealTimeData_toParticipantID rec_))
    else if queriedSel == sel_voiceChatService_didStartWithParticipantID then pure (maybe 0 (const 1) (_voiceChatService_didStartWithParticipantID rec_))
    else if queriedSel == sel_voiceChatService_didNotStartWithParticipantID_error then pure (maybe 0 (const 1) (_voiceChatService_didNotStartWithParticipantID_error rec_))
    else if queriedSel == sel_voiceChatService_didStopWithParticipantID_error then pure (maybe 0 (const 1) (_voiceChatService_didStopWithParticipantID_error rec_))
    else if queriedSel == sel_voiceChatService_didReceiveInvitationFromParticipantID_callID then pure (maybe 0 (const 1) (_voiceChatService_didReceiveInvitationFromParticipantID_callID rec_))
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
newGKVoiceChatClient :: GKVoiceChatClientOverrides -> IO RawId
newGKVoiceChatClient overrides = do
  inst <- class_createInstance gkVoiceChatClientDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
