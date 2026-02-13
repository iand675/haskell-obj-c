{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , sessionID
  , displayName
  , sessionMode
  , peerID
  , available
  , setAvailable
  , disconnectTimeout
  , setDisconnectTimeout
  , acceptConnectionFromPeer_errorSelector
  , availableSelector
  , cancelConnectToPeerSelector
  , connectToPeer_withTimeoutSelector
  , delegateSelector
  , denyConnectionFromPeerSelector
  , disconnectFromAllPeersSelector
  , disconnectPeerFromAllPeersSelector
  , disconnectTimeoutSelector
  , displayNameForPeerSelector
  , displayNameSelector
  , initWithSessionID_displayName_sessionModeSelector
  , peerIDSelector
  , peersWithConnectionStateSelector
  , sendDataToAllPeers_withDataMode_errorSelector
  , sendData_toPeers_withDataMode_errorSelector
  , sessionIDSelector
  , sessionModeSelector
  , setAvailableSelector
  , setDataReceiveHandler_withContextSelector
  , setDelegateSelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithSessionID:displayName:sessionMode:@
initWithSessionID_displayName_sessionMode :: (IsGKSession gkSession, IsNSString sessionID, IsNSString name) => gkSession -> sessionID -> name -> GKSessionMode -> IO RawId
initWithSessionID_displayName_sessionMode gkSession sessionID name mode =
  sendOwnedMessage gkSession initWithSessionID_displayName_sessionModeSelector (toNSString sessionID) (toNSString name) mode

-- | Return the application chosen name of a specific peer
--
-- ObjC selector: @- displayNameForPeer:@
displayNameForPeer :: (IsGKSession gkSession, IsNSString peerID) => gkSession -> peerID -> IO (Id NSString)
displayNameForPeer gkSession peerID =
  sendMessage gkSession displayNameForPeerSelector (toNSString peerID)

-- | Asynchronous delivery of data to one or more peers.  Returns YES if delivery started, NO if unable to start sending, and error will be set.  Delivery will be reliable or unreliable as set by mode.
--
-- ObjC selector: @- sendData:toPeers:withDataMode:error:@
sendData_toPeers_withDataMode_error :: (IsGKSession gkSession, IsNSData data_, IsNSArray peers, IsNSError error_) => gkSession -> data_ -> peers -> GKSendDataMode -> error_ -> IO Bool
sendData_toPeers_withDataMode_error gkSession data_ peers mode error_ =
  sendMessage gkSession sendData_toPeers_withDataMode_errorSelector (toNSData data_) (toNSArray peers) mode (toNSError error_)

-- | Asynchronous delivery to all peers.  Returns YES if delivery started, NO if unable to start sending, and error will be set.  Delivery will be reliable or unreliable as set by mode.
--
-- ObjC selector: @- sendDataToAllPeers:withDataMode:error:@
sendDataToAllPeers_withDataMode_error :: (IsGKSession gkSession, IsNSData data_, IsNSError error_) => gkSession -> data_ -> GKSendDataMode -> error_ -> IO Bool
sendDataToAllPeers_withDataMode_error gkSession data_ mode error_ =
  sendMessage gkSession sendDataToAllPeers_withDataMode_errorSelector (toNSData data_) mode (toNSError error_)

-- | Set the handler to receive data sent from remote peers.
--
-- ObjC selector: @- setDataReceiveHandler:withContext:@
setDataReceiveHandler_withContext :: IsGKSession gkSession => gkSession -> RawId -> Ptr () -> IO ()
setDataReceiveHandler_withContext gkSession handler context =
  sendMessage gkSession setDataReceiveHandler_withContextSelector handler context

-- | Attempt connection to a remote peer.  Remote peer gets a callback to -session:didReceiveConnectionRequestFromPeer:.
--
-- Success results in a call to delegate -session:peer:didChangeState: GKPeerStateConnectedFailure results in a call to delegate -session:connectionWithPeerFailed:withError:
--
-- ObjC selector: @- connectToPeer:withTimeout:@
connectToPeer_withTimeout :: (IsGKSession gkSession, IsNSString peerID) => gkSession -> peerID -> CDouble -> IO ()
connectToPeer_withTimeout gkSession peerID timeout =
  sendMessage gkSession connectToPeer_withTimeoutSelector (toNSString peerID) timeout

-- | @- cancelConnectToPeer:@
cancelConnectToPeer :: (IsGKSession gkSession, IsNSString peerID) => gkSession -> peerID -> IO ()
cancelConnectToPeer gkSession peerID =
  sendMessage gkSession cancelConnectToPeerSelector (toNSString peerID)

-- | Methods to accept or deny a prior connection request from -session:didReceiveConnectionRequestFromPeer:
--
-- ObjC selector: @- acceptConnectionFromPeer:error:@
acceptConnectionFromPeer_error :: (IsGKSession gkSession, IsNSString peerID, IsNSError error_) => gkSession -> peerID -> error_ -> IO Bool
acceptConnectionFromPeer_error gkSession peerID error_ =
  sendMessage gkSession acceptConnectionFromPeer_errorSelector (toNSString peerID) (toNSError error_)

-- | @- denyConnectionFromPeer:@
denyConnectionFromPeer :: (IsGKSession gkSession, IsNSString peerID) => gkSession -> peerID -> IO ()
denyConnectionFromPeer gkSession peerID =
  sendMessage gkSession denyConnectionFromPeerSelector (toNSString peerID)

-- | Disconnect a peer from the session (the peer gets disconnected from all connected peers).
--
-- ObjC selector: @- disconnectPeerFromAllPeers:@
disconnectPeerFromAllPeers :: (IsGKSession gkSession, IsNSString peerID) => gkSession -> peerID -> IO ()
disconnectPeerFromAllPeers gkSession peerID =
  sendMessage gkSession disconnectPeerFromAllPeersSelector (toNSString peerID)

-- | Disconnect local peer
--
-- ObjC selector: @- disconnectFromAllPeers@
disconnectFromAllPeers :: IsGKSession gkSession => gkSession -> IO ()
disconnectFromAllPeers gkSession =
  sendMessage gkSession disconnectFromAllPeersSelector

-- | Returns peers according to connection state
--
-- ObjC selector: @- peersWithConnectionState:@
peersWithConnectionState :: IsGKSession gkSession => gkSession -> GKPeerConnectionState -> IO (Id NSArray)
peersWithConnectionState gkSession state =
  sendMessage gkSession peersWithConnectionStateSelector state

-- | @- delegate@
delegate :: IsGKSession gkSession => gkSession -> IO RawId
delegate gkSession =
  sendMessage gkSession delegateSelector

-- | @- setDelegate:@
setDelegate :: IsGKSession gkSession => gkSession -> RawId -> IO ()
setDelegate gkSession value =
  sendMessage gkSession setDelegateSelector value

-- | @- sessionID@
sessionID :: IsGKSession gkSession => gkSession -> IO (Id NSString)
sessionID gkSession =
  sendMessage gkSession sessionIDSelector

-- | @- displayName@
displayName :: IsGKSession gkSession => gkSession -> IO (Id NSString)
displayName gkSession =
  sendMessage gkSession displayNameSelector

-- | @- sessionMode@
sessionMode :: IsGKSession gkSession => gkSession -> IO GKSessionMode
sessionMode gkSession =
  sendMessage gkSession sessionModeSelector

-- | @- peerID@
peerID :: IsGKSession gkSession => gkSession -> IO (Id NSString)
peerID gkSession =
  sendMessage gkSession peerIDSelector

-- | Toggle availability on the network based on session mode and search criteria.  Delegate will get a callback -session:didReceiveConnectionRequestFromPeer: when a peer attempts a connection.
--
-- ObjC selector: @- available@
available :: IsGKSession gkSession => gkSession -> IO Bool
available gkSession =
  sendMessage gkSession availableSelector

-- | Toggle availability on the network based on session mode and search criteria.  Delegate will get a callback -session:didReceiveConnectionRequestFromPeer: when a peer attempts a connection.
--
-- ObjC selector: @- setAvailable:@
setAvailable :: IsGKSession gkSession => gkSession -> Bool -> IO ()
setAvailable gkSession value =
  sendMessage gkSession setAvailableSelector value

-- | The timeout for disconnecting a peer if it appears that the peer has lost connection to the game network
--
-- ObjC selector: @- disconnectTimeout@
disconnectTimeout :: IsGKSession gkSession => gkSession -> IO CDouble
disconnectTimeout gkSession =
  sendMessage gkSession disconnectTimeoutSelector

-- | The timeout for disconnecting a peer if it appears that the peer has lost connection to the game network
--
-- ObjC selector: @- setDisconnectTimeout:@
setDisconnectTimeout :: IsGKSession gkSession => gkSession -> CDouble -> IO ()
setDisconnectTimeout gkSession value =
  sendMessage gkSession setDisconnectTimeoutSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSessionID:displayName:sessionMode:@
initWithSessionID_displayName_sessionModeSelector :: Selector '[Id NSString, Id NSString, GKSessionMode] RawId
initWithSessionID_displayName_sessionModeSelector = mkSelector "initWithSessionID:displayName:sessionMode:"

-- | @Selector@ for @displayNameForPeer:@
displayNameForPeerSelector :: Selector '[Id NSString] (Id NSString)
displayNameForPeerSelector = mkSelector "displayNameForPeer:"

-- | @Selector@ for @sendData:toPeers:withDataMode:error:@
sendData_toPeers_withDataMode_errorSelector :: Selector '[Id NSData, Id NSArray, GKSendDataMode, Id NSError] Bool
sendData_toPeers_withDataMode_errorSelector = mkSelector "sendData:toPeers:withDataMode:error:"

-- | @Selector@ for @sendDataToAllPeers:withDataMode:error:@
sendDataToAllPeers_withDataMode_errorSelector :: Selector '[Id NSData, GKSendDataMode, Id NSError] Bool
sendDataToAllPeers_withDataMode_errorSelector = mkSelector "sendDataToAllPeers:withDataMode:error:"

-- | @Selector@ for @setDataReceiveHandler:withContext:@
setDataReceiveHandler_withContextSelector :: Selector '[RawId, Ptr ()] ()
setDataReceiveHandler_withContextSelector = mkSelector "setDataReceiveHandler:withContext:"

-- | @Selector@ for @connectToPeer:withTimeout:@
connectToPeer_withTimeoutSelector :: Selector '[Id NSString, CDouble] ()
connectToPeer_withTimeoutSelector = mkSelector "connectToPeer:withTimeout:"

-- | @Selector@ for @cancelConnectToPeer:@
cancelConnectToPeerSelector :: Selector '[Id NSString] ()
cancelConnectToPeerSelector = mkSelector "cancelConnectToPeer:"

-- | @Selector@ for @acceptConnectionFromPeer:error:@
acceptConnectionFromPeer_errorSelector :: Selector '[Id NSString, Id NSError] Bool
acceptConnectionFromPeer_errorSelector = mkSelector "acceptConnectionFromPeer:error:"

-- | @Selector@ for @denyConnectionFromPeer:@
denyConnectionFromPeerSelector :: Selector '[Id NSString] ()
denyConnectionFromPeerSelector = mkSelector "denyConnectionFromPeer:"

-- | @Selector@ for @disconnectPeerFromAllPeers:@
disconnectPeerFromAllPeersSelector :: Selector '[Id NSString] ()
disconnectPeerFromAllPeersSelector = mkSelector "disconnectPeerFromAllPeers:"

-- | @Selector@ for @disconnectFromAllPeers@
disconnectFromAllPeersSelector :: Selector '[] ()
disconnectFromAllPeersSelector = mkSelector "disconnectFromAllPeers"

-- | @Selector@ for @peersWithConnectionState:@
peersWithConnectionStateSelector :: Selector '[GKPeerConnectionState] (Id NSArray)
peersWithConnectionStateSelector = mkSelector "peersWithConnectionState:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @sessionID@
sessionIDSelector :: Selector '[] (Id NSString)
sessionIDSelector = mkSelector "sessionID"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @sessionMode@
sessionModeSelector :: Selector '[] GKSessionMode
sessionModeSelector = mkSelector "sessionMode"

-- | @Selector@ for @peerID@
peerIDSelector :: Selector '[] (Id NSString)
peerIDSelector = mkSelector "peerID"

-- | @Selector@ for @available@
availableSelector :: Selector '[] Bool
availableSelector = mkSelector "available"

-- | @Selector@ for @setAvailable:@
setAvailableSelector :: Selector '[Bool] ()
setAvailableSelector = mkSelector "setAvailable:"

-- | @Selector@ for @disconnectTimeout@
disconnectTimeoutSelector :: Selector '[] CDouble
disconnectTimeoutSelector = mkSelector "disconnectTimeout"

-- | @Selector@ for @setDisconnectTimeout:@
setDisconnectTimeoutSelector :: Selector '[CDouble] ()
setDisconnectTimeoutSelector = mkSelector "setDisconnectTimeout:"

