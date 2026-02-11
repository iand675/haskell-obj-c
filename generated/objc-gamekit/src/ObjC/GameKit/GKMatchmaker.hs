{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GKMatchmaker is a singleton object to manage match creation from invites and automatching.
--
-- Generated bindings for @GKMatchmaker@.
module ObjC.GameKit.GKMatchmaker
  ( GKMatchmaker
  , IsGKMatchmaker(..)
  , sharedMatchmaker
  , matchForInvite_completionHandler
  , findMatchForRequest_withCompletionHandler
  , findMatchedPlayers_withCompletionHandler
  , addPlayersToMatch_matchRequest_completionHandler
  , cancel
  , cancelPendingInviteToPlayer
  , finishMatchmakingForMatch
  , queryPlayerGroupActivity_withCompletionHandler
  , queryActivityWithCompletionHandler
  , queryQueueActivity_withCompletionHandler
  , startBrowsingForNearbyPlayersWithHandler
  , stopBrowsingForNearbyPlayers
  , startGroupActivityWithPlayerHandler
  , stopGroupActivity
  , startBrowsingForNearbyPlayersWithReachableHandler
  , cancelInviteToPlayer
  , inviteHandler
  , setInviteHandler
  , sharedMatchmakerSelector
  , matchForInvite_completionHandlerSelector
  , findMatchForRequest_withCompletionHandlerSelector
  , findMatchedPlayers_withCompletionHandlerSelector
  , addPlayersToMatch_matchRequest_completionHandlerSelector
  , cancelSelector
  , cancelPendingInviteToPlayerSelector
  , finishMatchmakingForMatchSelector
  , queryPlayerGroupActivity_withCompletionHandlerSelector
  , queryActivityWithCompletionHandlerSelector
  , queryQueueActivity_withCompletionHandlerSelector
  , startBrowsingForNearbyPlayersWithHandlerSelector
  , stopBrowsingForNearbyPlayersSelector
  , startGroupActivityWithPlayerHandlerSelector
  , stopGroupActivitySelector
  , startBrowsingForNearbyPlayersWithReachableHandlerSelector
  , cancelInviteToPlayerSelector
  , inviteHandlerSelector
  , setInviteHandlerSelector


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
import ObjC.Foundation.Internal.Classes

-- | The shared matchmaker
--
-- ObjC selector: @+ sharedMatchmaker@
sharedMatchmaker :: IO (Id GKMatchmaker)
sharedMatchmaker  =
  do
    cls' <- getRequiredClass "GKMatchmaker"
    sendClassMsg cls' (mkSelector "sharedMatchmaker") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get a match for an accepted invite Possible reasons for error: 1. Communications failure 2. Invite cancelled
--
-- ObjC selector: @- matchForInvite:completionHandler:@
matchForInvite_completionHandler :: (IsGKMatchmaker gkMatchmaker, IsGKInvite invite) => gkMatchmaker -> invite -> Ptr () -> IO ()
matchForInvite_completionHandler gkMatchmaker  invite completionHandler =
withObjCPtr invite $ \raw_invite ->
    sendMsg gkMatchmaker (mkSelector "matchForInvite:completionHandler:") retVoid [argPtr (castPtr raw_invite :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Automatching or invites to find a peer-to-peer match for the specified request. Error will be nil on success: Possible reasons for error: 1. Communications failure 2. Unauthenticated player 3. Timeout Note that the players property on the returned GKMatch instance will only contain connected players. It will initially be empty as players are connecting. Implement the GKMatchDelegate method match:player:didChangeConnectionState: to listen for updates to the GKMatch instance's players property.
--
-- ObjC selector: @- findMatchForRequest:withCompletionHandler:@
findMatchForRequest_withCompletionHandler :: (IsGKMatchmaker gkMatchmaker, IsGKMatchRequest request) => gkMatchmaker -> request -> Ptr () -> IO ()
findMatchForRequest_withCompletionHandler gkMatchmaker  request completionHandler =
withObjCPtr request $ \raw_request ->
    sendMsg gkMatchmaker (mkSelector "findMatchForRequest:withCompletionHandler:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Automatching or invites for host-client rule-based match request.
--
-- ObjC selector: @- findMatchedPlayers:withCompletionHandler:@
findMatchedPlayers_withCompletionHandler :: (IsGKMatchmaker gkMatchmaker, IsGKMatchRequest request) => gkMatchmaker -> request -> Ptr () -> IO ()
findMatchedPlayers_withCompletionHandler gkMatchmaker  request completionHandler =
withObjCPtr request $ \raw_request ->
    sendMsg gkMatchmaker (mkSelector "findMatchedPlayers:withCompletionHandler:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Automatching or invites to add additional players to a peer-to-peer match for the specified request. Error will be nil on success: Possible reasons for error: 1. Communications failure 2. Timeout
--
-- ObjC selector: @- addPlayersToMatch:matchRequest:completionHandler:@
addPlayersToMatch_matchRequest_completionHandler :: (IsGKMatchmaker gkMatchmaker, IsGKMatch match, IsGKMatchRequest matchRequest) => gkMatchmaker -> match -> matchRequest -> Ptr () -> IO ()
addPlayersToMatch_matchRequest_completionHandler gkMatchmaker  match matchRequest completionHandler =
withObjCPtr match $ \raw_match ->
  withObjCPtr matchRequest $ \raw_matchRequest ->
      sendMsg gkMatchmaker (mkSelector "addPlayersToMatch:matchRequest:completionHandler:") retVoid [argPtr (castPtr raw_match :: Ptr ()), argPtr (castPtr raw_matchRequest :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Cancel matchmaking and any pending invites
--
-- ObjC selector: @- cancel@
cancel :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> IO ()
cancel gkMatchmaker  =
  sendMsg gkMatchmaker (mkSelector "cancel") retVoid []

-- | Cancel a pending invitation to a player
--
-- ObjC selector: @- cancelPendingInviteToPlayer:@
cancelPendingInviteToPlayer :: (IsGKMatchmaker gkMatchmaker, IsGKPlayer player) => gkMatchmaker -> player -> IO ()
cancelPendingInviteToPlayer gkMatchmaker  player =
withObjCPtr player $ \raw_player ->
    sendMsg gkMatchmaker (mkSelector "cancelPendingInviteToPlayer:") retVoid [argPtr (castPtr raw_player :: Ptr ())]

-- | Call this when finished with all programmatic P2P invites/matchmaking, for compatability with connected players using GKMatchmakerViewController.
--
-- ObjC selector: @- finishMatchmakingForMatch:@
finishMatchmakingForMatch :: (IsGKMatchmaker gkMatchmaker, IsGKMatch match) => gkMatchmaker -> match -> IO ()
finishMatchmakingForMatch gkMatchmaker  match =
withObjCPtr match $ \raw_match ->
    sendMsg gkMatchmaker (mkSelector "finishMatchmakingForMatch:") retVoid [argPtr (castPtr raw_match :: Ptr ())]

-- | Query the server for recent activity in the specified player group. A larger value indicates that a given group has seen more recent activity. Error will be nil on success. Possible reasons for error: 1. Communications failure
--
-- ObjC selector: @- queryPlayerGroupActivity:withCompletionHandler:@
queryPlayerGroupActivity_withCompletionHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> CULong -> Ptr () -> IO ()
queryPlayerGroupActivity_withCompletionHandler gkMatchmaker  playerGroup completionHandler =
  sendMsg gkMatchmaker (mkSelector "queryPlayerGroupActivity:withCompletionHandler:") retVoid [argCULong (fromIntegral playerGroup), argPtr (castPtr completionHandler :: Ptr ())]

-- | Query the server for recent activity for all the player groups of that game. Error will be nil on success. Possible reasons for error: 1. Communications failure
--
-- ObjC selector: @- queryActivityWithCompletionHandler:@
queryActivityWithCompletionHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> Ptr () -> IO ()
queryActivityWithCompletionHandler gkMatchmaker  completionHandler =
  sendMsg gkMatchmaker (mkSelector "queryActivityWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Query the server for recent activity for the specified queue.
--
-- ObjC selector: @- queryQueueActivity:withCompletionHandler:@
queryQueueActivity_withCompletionHandler :: (IsGKMatchmaker gkMatchmaker, IsNSString queueName) => gkMatchmaker -> queueName -> Ptr () -> IO ()
queryQueueActivity_withCompletionHandler gkMatchmaker  queueName completionHandler =
withObjCPtr queueName $ \raw_queueName ->
    sendMsg gkMatchmaker (mkSelector "queryQueueActivity:withCompletionHandler:") retVoid [argPtr (castPtr raw_queueName :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Start browsing for nearby players that can be invited to a match. The reachableHandler will be called for each player found with a compatible game. It may be called more than once for the same player if that player ever becomes unreachable (e.g. moves out of range). You should call stopBrowsingForNearbyPlayers when finished browsing.
--
-- ObjC selector: @- startBrowsingForNearbyPlayersWithHandler:@
startBrowsingForNearbyPlayersWithHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> Ptr () -> IO ()
startBrowsingForNearbyPlayersWithHandler gkMatchmaker  reachableHandler =
  sendMsg gkMatchmaker (mkSelector "startBrowsingForNearbyPlayersWithHandler:") retVoid [argPtr (castPtr reachableHandler :: Ptr ())]

-- | Stop browsing for nearby players.
--
-- ObjC selector: @- stopBrowsingForNearbyPlayers@
stopBrowsingForNearbyPlayers :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> IO ()
stopBrowsingForNearbyPlayers gkMatchmaker  =
  sendMsg gkMatchmaker (mkSelector "stopBrowsingForNearbyPlayers") retVoid []

-- | Activate  a  group activity by Game Center for your game, which allows people in the FaceTime call to join the local player's game. The handler will be called for each player who joined from the activity.
--
-- ObjC selector: @- startGroupActivityWithPlayerHandler:@
startGroupActivityWithPlayerHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> Ptr () -> IO ()
startGroupActivityWithPlayerHandler gkMatchmaker  handler =
  sendMsg gkMatchmaker (mkSelector "startGroupActivityWithPlayerHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | End the group activity created by Game Center for your game, which was activated by the local player.
--
-- ObjC selector: @- stopGroupActivity@
stopGroupActivity :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> IO ()
stopGroupActivity gkMatchmaker  =
  sendMsg gkMatchmaker (mkSelector "stopGroupActivity") retVoid []

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- startBrowsingForNearbyPlayersWithReachableHandler:@
startBrowsingForNearbyPlayersWithReachableHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> Ptr () -> IO ()
startBrowsingForNearbyPlayersWithReachableHandler gkMatchmaker  reachableHandler =
  sendMsg gkMatchmaker (mkSelector "startBrowsingForNearbyPlayersWithReachableHandler:") retVoid [argPtr (castPtr reachableHandler :: Ptr ())]

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- cancelInviteToPlayer:@
cancelInviteToPlayer :: (IsGKMatchmaker gkMatchmaker, IsNSString playerID) => gkMatchmaker -> playerID -> IO ()
cancelInviteToPlayer gkMatchmaker  playerID =
withObjCPtr playerID $ \raw_playerID ->
    sendMsg gkMatchmaker (mkSelector "cancelInviteToPlayer:") retVoid [argPtr (castPtr raw_playerID :: Ptr ())]

-- | @- inviteHandler@
inviteHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> IO (Ptr ())
inviteHandler gkMatchmaker  =
  fmap castPtr $ sendMsg gkMatchmaker (mkSelector "inviteHandler") (retPtr retVoid) []

-- | @- setInviteHandler:@
setInviteHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> Ptr () -> IO ()
setInviteHandler gkMatchmaker  value =
  sendMsg gkMatchmaker (mkSelector "setInviteHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedMatchmaker@
sharedMatchmakerSelector :: Selector
sharedMatchmakerSelector = mkSelector "sharedMatchmaker"

-- | @Selector@ for @matchForInvite:completionHandler:@
matchForInvite_completionHandlerSelector :: Selector
matchForInvite_completionHandlerSelector = mkSelector "matchForInvite:completionHandler:"

-- | @Selector@ for @findMatchForRequest:withCompletionHandler:@
findMatchForRequest_withCompletionHandlerSelector :: Selector
findMatchForRequest_withCompletionHandlerSelector = mkSelector "findMatchForRequest:withCompletionHandler:"

-- | @Selector@ for @findMatchedPlayers:withCompletionHandler:@
findMatchedPlayers_withCompletionHandlerSelector :: Selector
findMatchedPlayers_withCompletionHandlerSelector = mkSelector "findMatchedPlayers:withCompletionHandler:"

-- | @Selector@ for @addPlayersToMatch:matchRequest:completionHandler:@
addPlayersToMatch_matchRequest_completionHandlerSelector :: Selector
addPlayersToMatch_matchRequest_completionHandlerSelector = mkSelector "addPlayersToMatch:matchRequest:completionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @cancelPendingInviteToPlayer:@
cancelPendingInviteToPlayerSelector :: Selector
cancelPendingInviteToPlayerSelector = mkSelector "cancelPendingInviteToPlayer:"

-- | @Selector@ for @finishMatchmakingForMatch:@
finishMatchmakingForMatchSelector :: Selector
finishMatchmakingForMatchSelector = mkSelector "finishMatchmakingForMatch:"

-- | @Selector@ for @queryPlayerGroupActivity:withCompletionHandler:@
queryPlayerGroupActivity_withCompletionHandlerSelector :: Selector
queryPlayerGroupActivity_withCompletionHandlerSelector = mkSelector "queryPlayerGroupActivity:withCompletionHandler:"

-- | @Selector@ for @queryActivityWithCompletionHandler:@
queryActivityWithCompletionHandlerSelector :: Selector
queryActivityWithCompletionHandlerSelector = mkSelector "queryActivityWithCompletionHandler:"

-- | @Selector@ for @queryQueueActivity:withCompletionHandler:@
queryQueueActivity_withCompletionHandlerSelector :: Selector
queryQueueActivity_withCompletionHandlerSelector = mkSelector "queryQueueActivity:withCompletionHandler:"

-- | @Selector@ for @startBrowsingForNearbyPlayersWithHandler:@
startBrowsingForNearbyPlayersWithHandlerSelector :: Selector
startBrowsingForNearbyPlayersWithHandlerSelector = mkSelector "startBrowsingForNearbyPlayersWithHandler:"

-- | @Selector@ for @stopBrowsingForNearbyPlayers@
stopBrowsingForNearbyPlayersSelector :: Selector
stopBrowsingForNearbyPlayersSelector = mkSelector "stopBrowsingForNearbyPlayers"

-- | @Selector@ for @startGroupActivityWithPlayerHandler:@
startGroupActivityWithPlayerHandlerSelector :: Selector
startGroupActivityWithPlayerHandlerSelector = mkSelector "startGroupActivityWithPlayerHandler:"

-- | @Selector@ for @stopGroupActivity@
stopGroupActivitySelector :: Selector
stopGroupActivitySelector = mkSelector "stopGroupActivity"

-- | @Selector@ for @startBrowsingForNearbyPlayersWithReachableHandler:@
startBrowsingForNearbyPlayersWithReachableHandlerSelector :: Selector
startBrowsingForNearbyPlayersWithReachableHandlerSelector = mkSelector "startBrowsingForNearbyPlayersWithReachableHandler:"

-- | @Selector@ for @cancelInviteToPlayer:@
cancelInviteToPlayerSelector :: Selector
cancelInviteToPlayerSelector = mkSelector "cancelInviteToPlayer:"

-- | @Selector@ for @inviteHandler@
inviteHandlerSelector :: Selector
inviteHandlerSelector = mkSelector "inviteHandler"

-- | @Selector@ for @setInviteHandler:@
setInviteHandlerSelector :: Selector
setInviteHandlerSelector = mkSelector "setInviteHandler:"

