{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The GKSession handles networking between peers for a game, which includes establishing and maintaining connections over a game network, and network data transport.
--
-- This a not a Game Center feature. To support Game Center and online play, see GKMatch.
--
-- Generated bindings for @GKSession@.
module ObjC.GameKit.GKSession
  ( GKSession
  , IsGKSession(..)
  , initWithSessionID_displayName_sessionMode
  , displayNameForPeer
  , sendData_toPeers_withDataMode_error
  , sendDataToAllPeers_withDataMode_error
  , setDataReceiveHandler_withContext
  , connectToPeer_withTimeout
  , cancelConnectToPeer
  , acceptConnectionFromPeer_error
  , denyConnectionFromPeer
  , disconnectPeerFromAllPeers
  , disconnectFromAllPeers
  , peersWithConnectionState
  , sessionID
  , displayName
  , sessionMode
  , peerID
  , available
  , setAvailable
  , disconnectTimeout
  , setDisconnectTimeout
  , initWithSessionID_displayName_sessionModeSelector
  , displayNameForPeerSelector
  , sendData_toPeers_withDataMode_errorSelector
  , sendDataToAllPeers_withDataMode_errorSelector
  , setDataReceiveHandler_withContextSelector
  , connectToPeer_withTimeoutSelector
  , cancelConnectToPeerSelector
  , acceptConnectionFromPeer_errorSelector
  , denyConnectionFromPeerSelector
  , disconnectPeerFromAllPeersSelector
  , disconnectFromAllPeersSelector
  , peersWithConnectionStateSelector
  , sessionIDSelector
  , displayNameSelector
  , sessionModeSelector
  , peerIDSelector
  , availableSelector
  , setAvailableSelector
  , disconnectTimeoutSelector
  , setDisconnectTimeoutSelector

  -- * Enum types
  , GKPeerConnectionState(GKPeerConnectionState)
  , pattern GKPeerStateAvailable
  , pattern GKPeerStateUnavailable
  , pattern GKPeerStateConnected
  , pattern GKPeerStateDisconnected
  , pattern GKPeerStateConnecting
  , pattern GKPeerStateConnectedRelay
  , GKSendDataMode(GKSendDataMode)
  , pattern GKSendDataReliable
  , pattern GKSendDataUnreliable
  , GKSessionMode(GKSessionMode)
  , pattern GKSessionModeServer
  , pattern GKSessionModeClient
  , pattern GKSessionModePeer

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithSessionID:displayName:sessionMode:@
initWithSessionID_displayName_sessionMode :: (IsGKSession gkSession, IsNSString sessionID, IsNSString name) => gkSession -> sessionID -> name -> GKSessionMode -> IO RawId
initWithSessionID_displayName_sessionMode gkSession  sessionID name mode =
withObjCPtr sessionID $ \raw_sessionID ->
  withObjCPtr name $ \raw_name ->
      fmap (RawId . castPtr) $ sendMsg gkSession (mkSelector "initWithSessionID:displayName:sessionMode:") (retPtr retVoid) [argPtr (castPtr raw_sessionID :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argCInt (coerce mode)]

-- | Return the application chosen name of a specific peer
--
-- ObjC selector: @- displayNameForPeer:@
displayNameForPeer :: (IsGKSession gkSession, IsNSString peerID) => gkSession -> peerID -> IO (Id NSString)
displayNameForPeer gkSession  peerID =
withObjCPtr peerID $ \raw_peerID ->
    sendMsg gkSession (mkSelector "displayNameForPeer:") (retPtr retVoid) [argPtr (castPtr raw_peerID :: Ptr ())] >>= retainedObject . castPtr

-- | Asynchronous delivery of data to one or more peers.  Returns YES if delivery started, NO if unable to start sending, and error will be set.  Delivery will be reliable or unreliable as set by mode.
--
-- ObjC selector: @- sendData:toPeers:withDataMode:error:@
sendData_toPeers_withDataMode_error :: (IsGKSession gkSession, IsNSData data_, IsNSArray peers, IsNSError error_) => gkSession -> data_ -> peers -> GKSendDataMode -> error_ -> IO Bool
sendData_toPeers_withDataMode_error gkSession  data_ peers mode error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr peers $ \raw_peers ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkSession (mkSelector "sendData:toPeers:withDataMode:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_peers :: Ptr ()), argCInt (coerce mode), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Asynchronous delivery to all peers.  Returns YES if delivery started, NO if unable to start sending, and error will be set.  Delivery will be reliable or unreliable as set by mode.
--
-- ObjC selector: @- sendDataToAllPeers:withDataMode:error:@
sendDataToAllPeers_withDataMode_error :: (IsGKSession gkSession, IsNSData data_, IsNSError error_) => gkSession -> data_ -> GKSendDataMode -> error_ -> IO Bool
sendDataToAllPeers_withDataMode_error gkSession  data_ mode error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkSession (mkSelector "sendDataToAllPeers:withDataMode:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argCInt (coerce mode), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Set the handler to receive data sent from remote peers.
--
-- ObjC selector: @- setDataReceiveHandler:withContext:@
setDataReceiveHandler_withContext :: IsGKSession gkSession => gkSession -> RawId -> Ptr () -> IO ()
setDataReceiveHandler_withContext gkSession  handler context =
  sendMsg gkSession (mkSelector "setDataReceiveHandler:withContext:") retVoid [argPtr (castPtr (unRawId handler) :: Ptr ()), argPtr context]

-- | Attempt connection to a remote peer.  Remote peer gets a callback to -session:didReceiveConnectionRequestFromPeer:.
--
-- Success results in a call to delegate -session:peer:didChangeState: GKPeerStateConnectedFailure results in a call to delegate -session:connectionWithPeerFailed:withError:
--
-- ObjC selector: @- connectToPeer:withTimeout:@
connectToPeer_withTimeout :: (IsGKSession gkSession, IsNSString peerID) => gkSession -> peerID -> CDouble -> IO ()
connectToPeer_withTimeout gkSession  peerID timeout =
withObjCPtr peerID $ \raw_peerID ->
    sendMsg gkSession (mkSelector "connectToPeer:withTimeout:") retVoid [argPtr (castPtr raw_peerID :: Ptr ()), argCDouble (fromIntegral timeout)]

-- | @- cancelConnectToPeer:@
cancelConnectToPeer :: (IsGKSession gkSession, IsNSString peerID) => gkSession -> peerID -> IO ()
cancelConnectToPeer gkSession  peerID =
withObjCPtr peerID $ \raw_peerID ->
    sendMsg gkSession (mkSelector "cancelConnectToPeer:") retVoid [argPtr (castPtr raw_peerID :: Ptr ())]

-- | Methods to accept or deny a prior connection request from -session:didReceiveConnectionRequestFromPeer:
--
-- ObjC selector: @- acceptConnectionFromPeer:error:@
acceptConnectionFromPeer_error :: (IsGKSession gkSession, IsNSString peerID, IsNSError error_) => gkSession -> peerID -> error_ -> IO Bool
acceptConnectionFromPeer_error gkSession  peerID error_ =
withObjCPtr peerID $ \raw_peerID ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkSession (mkSelector "acceptConnectionFromPeer:error:") retCULong [argPtr (castPtr raw_peerID :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- denyConnectionFromPeer:@
denyConnectionFromPeer :: (IsGKSession gkSession, IsNSString peerID) => gkSession -> peerID -> IO ()
denyConnectionFromPeer gkSession  peerID =
withObjCPtr peerID $ \raw_peerID ->
    sendMsg gkSession (mkSelector "denyConnectionFromPeer:") retVoid [argPtr (castPtr raw_peerID :: Ptr ())]

-- | Disconnect a peer from the session (the peer gets disconnected from all connected peers).
--
-- ObjC selector: @- disconnectPeerFromAllPeers:@
disconnectPeerFromAllPeers :: (IsGKSession gkSession, IsNSString peerID) => gkSession -> peerID -> IO ()
disconnectPeerFromAllPeers gkSession  peerID =
withObjCPtr peerID $ \raw_peerID ->
    sendMsg gkSession (mkSelector "disconnectPeerFromAllPeers:") retVoid [argPtr (castPtr raw_peerID :: Ptr ())]

-- | Disconnect local peer
--
-- ObjC selector: @- disconnectFromAllPeers@
disconnectFromAllPeers :: IsGKSession gkSession => gkSession -> IO ()
disconnectFromAllPeers gkSession  =
  sendMsg gkSession (mkSelector "disconnectFromAllPeers") retVoid []

-- | Returns peers according to connection state
--
-- ObjC selector: @- peersWithConnectionState:@
peersWithConnectionState :: IsGKSession gkSession => gkSession -> GKPeerConnectionState -> IO (Id NSArray)
peersWithConnectionState gkSession  state =
  sendMsg gkSession (mkSelector "peersWithConnectionState:") (retPtr retVoid) [argCInt (coerce state)] >>= retainedObject . castPtr

-- | @- sessionID@
sessionID :: IsGKSession gkSession => gkSession -> IO (Id NSString)
sessionID gkSession  =
  sendMsg gkSession (mkSelector "sessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- displayName@
displayName :: IsGKSession gkSession => gkSession -> IO (Id NSString)
displayName gkSession  =
  sendMsg gkSession (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sessionMode@
sessionMode :: IsGKSession gkSession => gkSession -> IO GKSessionMode
sessionMode gkSession  =
  fmap (coerce :: CInt -> GKSessionMode) $ sendMsg gkSession (mkSelector "sessionMode") retCInt []

-- | @- peerID@
peerID :: IsGKSession gkSession => gkSession -> IO (Id NSString)
peerID gkSession  =
  sendMsg gkSession (mkSelector "peerID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Toggle availability on the network based on session mode and search criteria.  Delegate will get a callback -session:didReceiveConnectionRequestFromPeer: when a peer attempts a connection.
--
-- ObjC selector: @- available@
available :: IsGKSession gkSession => gkSession -> IO Bool
available gkSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkSession (mkSelector "available") retCULong []

-- | Toggle availability on the network based on session mode and search criteria.  Delegate will get a callback -session:didReceiveConnectionRequestFromPeer: when a peer attempts a connection.
--
-- ObjC selector: @- setAvailable:@
setAvailable :: IsGKSession gkSession => gkSession -> Bool -> IO ()
setAvailable gkSession  value =
  sendMsg gkSession (mkSelector "setAvailable:") retVoid [argCULong (if value then 1 else 0)]

-- | The timeout for disconnecting a peer if it appears that the peer has lost connection to the game network
--
-- ObjC selector: @- disconnectTimeout@
disconnectTimeout :: IsGKSession gkSession => gkSession -> IO CDouble
disconnectTimeout gkSession  =
  sendMsg gkSession (mkSelector "disconnectTimeout") retCDouble []

-- | The timeout for disconnecting a peer if it appears that the peer has lost connection to the game network
--
-- ObjC selector: @- setDisconnectTimeout:@
setDisconnectTimeout :: IsGKSession gkSession => gkSession -> CDouble -> IO ()
setDisconnectTimeout gkSession  value =
  sendMsg gkSession (mkSelector "setDisconnectTimeout:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSessionID:displayName:sessionMode:@
initWithSessionID_displayName_sessionModeSelector :: Selector
initWithSessionID_displayName_sessionModeSelector = mkSelector "initWithSessionID:displayName:sessionMode:"

-- | @Selector@ for @displayNameForPeer:@
displayNameForPeerSelector :: Selector
displayNameForPeerSelector = mkSelector "displayNameForPeer:"

-- | @Selector@ for @sendData:toPeers:withDataMode:error:@
sendData_toPeers_withDataMode_errorSelector :: Selector
sendData_toPeers_withDataMode_errorSelector = mkSelector "sendData:toPeers:withDataMode:error:"

-- | @Selector@ for @sendDataToAllPeers:withDataMode:error:@
sendDataToAllPeers_withDataMode_errorSelector :: Selector
sendDataToAllPeers_withDataMode_errorSelector = mkSelector "sendDataToAllPeers:withDataMode:error:"

-- | @Selector@ for @setDataReceiveHandler:withContext:@
setDataReceiveHandler_withContextSelector :: Selector
setDataReceiveHandler_withContextSelector = mkSelector "setDataReceiveHandler:withContext:"

-- | @Selector@ for @connectToPeer:withTimeout:@
connectToPeer_withTimeoutSelector :: Selector
connectToPeer_withTimeoutSelector = mkSelector "connectToPeer:withTimeout:"

-- | @Selector@ for @cancelConnectToPeer:@
cancelConnectToPeerSelector :: Selector
cancelConnectToPeerSelector = mkSelector "cancelConnectToPeer:"

-- | @Selector@ for @acceptConnectionFromPeer:error:@
acceptConnectionFromPeer_errorSelector :: Selector
acceptConnectionFromPeer_errorSelector = mkSelector "acceptConnectionFromPeer:error:"

-- | @Selector@ for @denyConnectionFromPeer:@
denyConnectionFromPeerSelector :: Selector
denyConnectionFromPeerSelector = mkSelector "denyConnectionFromPeer:"

-- | @Selector@ for @disconnectPeerFromAllPeers:@
disconnectPeerFromAllPeersSelector :: Selector
disconnectPeerFromAllPeersSelector = mkSelector "disconnectPeerFromAllPeers:"

-- | @Selector@ for @disconnectFromAllPeers@
disconnectFromAllPeersSelector :: Selector
disconnectFromAllPeersSelector = mkSelector "disconnectFromAllPeers"

-- | @Selector@ for @peersWithConnectionState:@
peersWithConnectionStateSelector :: Selector
peersWithConnectionStateSelector = mkSelector "peersWithConnectionState:"

-- | @Selector@ for @sessionID@
sessionIDSelector :: Selector
sessionIDSelector = mkSelector "sessionID"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @sessionMode@
sessionModeSelector :: Selector
sessionModeSelector = mkSelector "sessionMode"

-- | @Selector@ for @peerID@
peerIDSelector :: Selector
peerIDSelector = mkSelector "peerID"

-- | @Selector@ for @available@
availableSelector :: Selector
availableSelector = mkSelector "available"

-- | @Selector@ for @setAvailable:@
setAvailableSelector :: Selector
setAvailableSelector = mkSelector "setAvailable:"

-- | @Selector@ for @disconnectTimeout@
disconnectTimeoutSelector :: Selector
disconnectTimeoutSelector = mkSelector "disconnectTimeout"

-- | @Selector@ for @setDisconnectTimeout:@
setDisconnectTimeoutSelector :: Selector
setDisconnectTimeoutSelector = mkSelector "setDisconnectTimeout:"

