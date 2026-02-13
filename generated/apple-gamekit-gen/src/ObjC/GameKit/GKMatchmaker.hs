{-# LANGUAGE DataKinds #-}
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
  , addPlayersToMatch_matchRequest_completionHandlerSelector
  , cancelInviteToPlayerSelector
  , cancelPendingInviteToPlayerSelector
  , cancelSelector
  , findMatchForRequest_withCompletionHandlerSelector
  , findMatchedPlayers_withCompletionHandlerSelector
  , finishMatchmakingForMatchSelector
  , inviteHandlerSelector
  , matchForInvite_completionHandlerSelector
  , queryActivityWithCompletionHandlerSelector
  , queryPlayerGroupActivity_withCompletionHandlerSelector
  , queryQueueActivity_withCompletionHandlerSelector
  , setInviteHandlerSelector
  , sharedMatchmakerSelector
  , startBrowsingForNearbyPlayersWithHandlerSelector
  , startBrowsingForNearbyPlayersWithReachableHandlerSelector
  , startGroupActivityWithPlayerHandlerSelector
  , stopBrowsingForNearbyPlayersSelector
  , stopGroupActivitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' sharedMatchmakerSelector

-- | Get a match for an accepted invite Possible reasons for error: 1. Communications failure 2. Invite cancelled
--
-- ObjC selector: @- matchForInvite:completionHandler:@
matchForInvite_completionHandler :: (IsGKMatchmaker gkMatchmaker, IsGKInvite invite) => gkMatchmaker -> invite -> Ptr () -> IO ()
matchForInvite_completionHandler gkMatchmaker invite completionHandler =
  sendMessage gkMatchmaker matchForInvite_completionHandlerSelector (toGKInvite invite) completionHandler

-- | Automatching or invites to find a peer-to-peer match for the specified request. Error will be nil on success: Possible reasons for error: 1. Communications failure 2. Unauthenticated player 3. Timeout Note that the players property on the returned GKMatch instance will only contain connected players. It will initially be empty as players are connecting. Implement the GKMatchDelegate method match:player:didChangeConnectionState: to listen for updates to the GKMatch instance's players property.
--
-- ObjC selector: @- findMatchForRequest:withCompletionHandler:@
findMatchForRequest_withCompletionHandler :: (IsGKMatchmaker gkMatchmaker, IsGKMatchRequest request) => gkMatchmaker -> request -> Ptr () -> IO ()
findMatchForRequest_withCompletionHandler gkMatchmaker request completionHandler =
  sendMessage gkMatchmaker findMatchForRequest_withCompletionHandlerSelector (toGKMatchRequest request) completionHandler

-- | Automatching or invites for host-client rule-based match request.
--
-- ObjC selector: @- findMatchedPlayers:withCompletionHandler:@
findMatchedPlayers_withCompletionHandler :: (IsGKMatchmaker gkMatchmaker, IsGKMatchRequest request) => gkMatchmaker -> request -> Ptr () -> IO ()
findMatchedPlayers_withCompletionHandler gkMatchmaker request completionHandler =
  sendMessage gkMatchmaker findMatchedPlayers_withCompletionHandlerSelector (toGKMatchRequest request) completionHandler

-- | Automatching or invites to add additional players to a peer-to-peer match for the specified request. Error will be nil on success: Possible reasons for error: 1. Communications failure 2. Timeout
--
-- ObjC selector: @- addPlayersToMatch:matchRequest:completionHandler:@
addPlayersToMatch_matchRequest_completionHandler :: (IsGKMatchmaker gkMatchmaker, IsGKMatch match, IsGKMatchRequest matchRequest) => gkMatchmaker -> match -> matchRequest -> Ptr () -> IO ()
addPlayersToMatch_matchRequest_completionHandler gkMatchmaker match matchRequest completionHandler =
  sendMessage gkMatchmaker addPlayersToMatch_matchRequest_completionHandlerSelector (toGKMatch match) (toGKMatchRequest matchRequest) completionHandler

-- | Cancel matchmaking and any pending invites
--
-- ObjC selector: @- cancel@
cancel :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> IO ()
cancel gkMatchmaker =
  sendMessage gkMatchmaker cancelSelector

-- | Cancel a pending invitation to a player
--
-- ObjC selector: @- cancelPendingInviteToPlayer:@
cancelPendingInviteToPlayer :: (IsGKMatchmaker gkMatchmaker, IsGKPlayer player) => gkMatchmaker -> player -> IO ()
cancelPendingInviteToPlayer gkMatchmaker player =
  sendMessage gkMatchmaker cancelPendingInviteToPlayerSelector (toGKPlayer player)

-- | Call this when finished with all programmatic P2P invites/matchmaking, for compatability with connected players using GKMatchmakerViewController.
--
-- ObjC selector: @- finishMatchmakingForMatch:@
finishMatchmakingForMatch :: (IsGKMatchmaker gkMatchmaker, IsGKMatch match) => gkMatchmaker -> match -> IO ()
finishMatchmakingForMatch gkMatchmaker match =
  sendMessage gkMatchmaker finishMatchmakingForMatchSelector (toGKMatch match)

-- | Query the server for recent activity in the specified player group. A larger value indicates that a given group has seen more recent activity. Error will be nil on success. Possible reasons for error: 1. Communications failure
--
-- ObjC selector: @- queryPlayerGroupActivity:withCompletionHandler:@
queryPlayerGroupActivity_withCompletionHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> CULong -> Ptr () -> IO ()
queryPlayerGroupActivity_withCompletionHandler gkMatchmaker playerGroup completionHandler =
  sendMessage gkMatchmaker queryPlayerGroupActivity_withCompletionHandlerSelector playerGroup completionHandler

-- | Query the server for recent activity for all the player groups of that game. Error will be nil on success. Possible reasons for error: 1. Communications failure
--
-- ObjC selector: @- queryActivityWithCompletionHandler:@
queryActivityWithCompletionHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> Ptr () -> IO ()
queryActivityWithCompletionHandler gkMatchmaker completionHandler =
  sendMessage gkMatchmaker queryActivityWithCompletionHandlerSelector completionHandler

-- | Query the server for recent activity for the specified queue.
--
-- ObjC selector: @- queryQueueActivity:withCompletionHandler:@
queryQueueActivity_withCompletionHandler :: (IsGKMatchmaker gkMatchmaker, IsNSString queueName) => gkMatchmaker -> queueName -> Ptr () -> IO ()
queryQueueActivity_withCompletionHandler gkMatchmaker queueName completionHandler =
  sendMessage gkMatchmaker queryQueueActivity_withCompletionHandlerSelector (toNSString queueName) completionHandler

-- | Start browsing for nearby players that can be invited to a match. The reachableHandler will be called for each player found with a compatible game. It may be called more than once for the same player if that player ever becomes unreachable (e.g. moves out of range). You should call stopBrowsingForNearbyPlayers when finished browsing.
--
-- ObjC selector: @- startBrowsingForNearbyPlayersWithHandler:@
startBrowsingForNearbyPlayersWithHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> Ptr () -> IO ()
startBrowsingForNearbyPlayersWithHandler gkMatchmaker reachableHandler =
  sendMessage gkMatchmaker startBrowsingForNearbyPlayersWithHandlerSelector reachableHandler

-- | Stop browsing for nearby players.
--
-- ObjC selector: @- stopBrowsingForNearbyPlayers@
stopBrowsingForNearbyPlayers :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> IO ()
stopBrowsingForNearbyPlayers gkMatchmaker =
  sendMessage gkMatchmaker stopBrowsingForNearbyPlayersSelector

-- | Activate  a  group activity by Game Center for your game, which allows people in the FaceTime call to join the local player's game. The handler will be called for each player who joined from the activity.
--
-- ObjC selector: @- startGroupActivityWithPlayerHandler:@
startGroupActivityWithPlayerHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> Ptr () -> IO ()
startGroupActivityWithPlayerHandler gkMatchmaker handler =
  sendMessage gkMatchmaker startGroupActivityWithPlayerHandlerSelector handler

-- | End the group activity created by Game Center for your game, which was activated by the local player.
--
-- ObjC selector: @- stopGroupActivity@
stopGroupActivity :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> IO ()
stopGroupActivity gkMatchmaker =
  sendMessage gkMatchmaker stopGroupActivitySelector

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- startBrowsingForNearbyPlayersWithReachableHandler:@
startBrowsingForNearbyPlayersWithReachableHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> Ptr () -> IO ()
startBrowsingForNearbyPlayersWithReachableHandler gkMatchmaker reachableHandler =
  sendMessage gkMatchmaker startBrowsingForNearbyPlayersWithReachableHandlerSelector reachableHandler

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- cancelInviteToPlayer:@
cancelInviteToPlayer :: (IsGKMatchmaker gkMatchmaker, IsNSString playerID) => gkMatchmaker -> playerID -> IO ()
cancelInviteToPlayer gkMatchmaker playerID =
  sendMessage gkMatchmaker cancelInviteToPlayerSelector (toNSString playerID)

-- | @- inviteHandler@
inviteHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> IO (Ptr ())
inviteHandler gkMatchmaker =
  sendMessage gkMatchmaker inviteHandlerSelector

-- | @- setInviteHandler:@
setInviteHandler :: IsGKMatchmaker gkMatchmaker => gkMatchmaker -> Ptr () -> IO ()
setInviteHandler gkMatchmaker value =
  sendMessage gkMatchmaker setInviteHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedMatchmaker@
sharedMatchmakerSelector :: Selector '[] (Id GKMatchmaker)
sharedMatchmakerSelector = mkSelector "sharedMatchmaker"

-- | @Selector@ for @matchForInvite:completionHandler:@
matchForInvite_completionHandlerSelector :: Selector '[Id GKInvite, Ptr ()] ()
matchForInvite_completionHandlerSelector = mkSelector "matchForInvite:completionHandler:"

-- | @Selector@ for @findMatchForRequest:withCompletionHandler:@
findMatchForRequest_withCompletionHandlerSelector :: Selector '[Id GKMatchRequest, Ptr ()] ()
findMatchForRequest_withCompletionHandlerSelector = mkSelector "findMatchForRequest:withCompletionHandler:"

-- | @Selector@ for @findMatchedPlayers:withCompletionHandler:@
findMatchedPlayers_withCompletionHandlerSelector :: Selector '[Id GKMatchRequest, Ptr ()] ()
findMatchedPlayers_withCompletionHandlerSelector = mkSelector "findMatchedPlayers:withCompletionHandler:"

-- | @Selector@ for @addPlayersToMatch:matchRequest:completionHandler:@
addPlayersToMatch_matchRequest_completionHandlerSelector :: Selector '[Id GKMatch, Id GKMatchRequest, Ptr ()] ()
addPlayersToMatch_matchRequest_completionHandlerSelector = mkSelector "addPlayersToMatch:matchRequest:completionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @cancelPendingInviteToPlayer:@
cancelPendingInviteToPlayerSelector :: Selector '[Id GKPlayer] ()
cancelPendingInviteToPlayerSelector = mkSelector "cancelPendingInviteToPlayer:"

-- | @Selector@ for @finishMatchmakingForMatch:@
finishMatchmakingForMatchSelector :: Selector '[Id GKMatch] ()
finishMatchmakingForMatchSelector = mkSelector "finishMatchmakingForMatch:"

-- | @Selector@ for @queryPlayerGroupActivity:withCompletionHandler:@
queryPlayerGroupActivity_withCompletionHandlerSelector :: Selector '[CULong, Ptr ()] ()
queryPlayerGroupActivity_withCompletionHandlerSelector = mkSelector "queryPlayerGroupActivity:withCompletionHandler:"

-- | @Selector@ for @queryActivityWithCompletionHandler:@
queryActivityWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
queryActivityWithCompletionHandlerSelector = mkSelector "queryActivityWithCompletionHandler:"

-- | @Selector@ for @queryQueueActivity:withCompletionHandler:@
queryQueueActivity_withCompletionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
queryQueueActivity_withCompletionHandlerSelector = mkSelector "queryQueueActivity:withCompletionHandler:"

-- | @Selector@ for @startBrowsingForNearbyPlayersWithHandler:@
startBrowsingForNearbyPlayersWithHandlerSelector :: Selector '[Ptr ()] ()
startBrowsingForNearbyPlayersWithHandlerSelector = mkSelector "startBrowsingForNearbyPlayersWithHandler:"

-- | @Selector@ for @stopBrowsingForNearbyPlayers@
stopBrowsingForNearbyPlayersSelector :: Selector '[] ()
stopBrowsingForNearbyPlayersSelector = mkSelector "stopBrowsingForNearbyPlayers"

-- | @Selector@ for @startGroupActivityWithPlayerHandler:@
startGroupActivityWithPlayerHandlerSelector :: Selector '[Ptr ()] ()
startGroupActivityWithPlayerHandlerSelector = mkSelector "startGroupActivityWithPlayerHandler:"

-- | @Selector@ for @stopGroupActivity@
stopGroupActivitySelector :: Selector '[] ()
stopGroupActivitySelector = mkSelector "stopGroupActivity"

-- | @Selector@ for @startBrowsingForNearbyPlayersWithReachableHandler:@
startBrowsingForNearbyPlayersWithReachableHandlerSelector :: Selector '[Ptr ()] ()
startBrowsingForNearbyPlayersWithReachableHandlerSelector = mkSelector "startBrowsingForNearbyPlayersWithReachableHandler:"

-- | @Selector@ for @cancelInviteToPlayer:@
cancelInviteToPlayerSelector :: Selector '[Id NSString] ()
cancelInviteToPlayerSelector = mkSelector "cancelInviteToPlayer:"

-- | @Selector@ for @inviteHandler@
inviteHandlerSelector :: Selector '[] (Ptr ())
inviteHandlerSelector = mkSelector "inviteHandler"

-- | @Selector@ for @setInviteHandler:@
setInviteHandlerSelector :: Selector '[Ptr ()] ()
setInviteHandlerSelector = mkSelector "setInviteHandler:"

