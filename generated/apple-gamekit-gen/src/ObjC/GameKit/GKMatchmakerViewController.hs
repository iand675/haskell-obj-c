{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKMatchmakerViewController@.
module ObjC.GameKit.GKMatchmakerViewController
  ( GKMatchmakerViewController
  , IsGKMatchmakerViewController(..)
  , initWithMatchRequest
  , initWithInvite
  , addPlayersToMatch
  , setHostedPlayer_didConnect
  , setHostedPlayer_connected
  , setHostedPlayerReady
  , matchmakerDelegate
  , setMatchmakerDelegate
  , matchRequest
  , hosted
  , setHosted
  , matchmakingMode
  , setMatchmakingMode
  , canStartWithMinimumPlayers
  , setCanStartWithMinimumPlayers
  , defaultInvitationMessage
  , setDefaultInvitationMessage
  , addPlayersToMatchSelector
  , canStartWithMinimumPlayersSelector
  , defaultInvitationMessageSelector
  , hostedSelector
  , initWithInviteSelector
  , initWithMatchRequestSelector
  , matchRequestSelector
  , matchmakerDelegateSelector
  , matchmakingModeSelector
  , setCanStartWithMinimumPlayersSelector
  , setDefaultInvitationMessageSelector
  , setHostedPlayerReadySelector
  , setHostedPlayer_connectedSelector
  , setHostedPlayer_didConnectSelector
  , setHostedSelector
  , setMatchmakerDelegateSelector
  , setMatchmakingModeSelector

  -- * Enum types
  , GKMatchmakingMode(GKMatchmakingMode)
  , pattern GKMatchmakingModeDefault
  , pattern GKMatchmakingModeNearbyOnly
  , pattern GKMatchmakingModeAutomatchOnly
  , pattern GKMatchmakingModeInviteOnly

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize with a matchmaking request, allowing the user to send invites and/or start matchmaking
--
-- ObjC selector: @- initWithMatchRequest:@
initWithMatchRequest :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsGKMatchRequest request) => gkMatchmakerViewController -> request -> IO RawId
initWithMatchRequest gkMatchmakerViewController request =
  sendOwnedMessage gkMatchmakerViewController initWithMatchRequestSelector (toGKMatchRequest request)

-- | Initialize with an accepted invite, allowing the user to see the status of other invited players and get notified when the game starts
--
-- ObjC selector: @- initWithInvite:@
initWithInvite :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsGKInvite invite) => gkMatchmakerViewController -> invite -> IO RawId
initWithInvite gkMatchmakerViewController invite =
  sendOwnedMessage gkMatchmakerViewController initWithInviteSelector (toGKInvite invite)

-- | Add additional players (not currently connected) to an existing peer-to-peer match. Apps should elect a single device to do this, otherwise conflicts could arise resulting in unexpected connection errors.
--
-- ObjC selector: @- addPlayersToMatch:@
addPlayersToMatch :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsGKMatch match) => gkMatchmakerViewController -> match -> IO ()
addPlayersToMatch gkMatchmakerViewController match =
  sendMessage gkMatchmakerViewController addPlayersToMatchSelector (toGKMatch match)

-- | @- setHostedPlayer:didConnect:@
setHostedPlayer_didConnect :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsGKPlayer player) => gkMatchmakerViewController -> player -> Bool -> IO ()
setHostedPlayer_didConnect gkMatchmakerViewController player connected =
  sendMessage gkMatchmakerViewController setHostedPlayer_didConnectSelector (toGKPlayer player) connected

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- setHostedPlayer:connected:@
setHostedPlayer_connected :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsNSString playerID) => gkMatchmakerViewController -> playerID -> Bool -> IO ()
setHostedPlayer_connected gkMatchmakerViewController playerID connected =
  sendMessage gkMatchmakerViewController setHostedPlayer_connectedSelector (toNSString playerID) connected

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- setHostedPlayerReady:@
setHostedPlayerReady :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsNSString playerID) => gkMatchmakerViewController -> playerID -> IO ()
setHostedPlayerReady gkMatchmakerViewController playerID =
  sendMessage gkMatchmakerViewController setHostedPlayerReadySelector (toNSString playerID)

-- | @- matchmakerDelegate@
matchmakerDelegate :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> IO RawId
matchmakerDelegate gkMatchmakerViewController =
  sendMessage gkMatchmakerViewController matchmakerDelegateSelector

-- | @- setMatchmakerDelegate:@
setMatchmakerDelegate :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> RawId -> IO ()
setMatchmakerDelegate gkMatchmakerViewController value =
  sendMessage gkMatchmakerViewController setMatchmakerDelegateSelector value

-- | @- matchRequest@
matchRequest :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> IO (Id GKMatchRequest)
matchRequest gkMatchmakerViewController =
  sendMessage gkMatchmakerViewController matchRequestSelector

-- | set to YES to receive hosted (eg. not peer-to-peer) match results. Will cause the controller to return an array of players instead of a match.
--
-- ObjC selector: @- hosted@
hosted :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> IO Bool
hosted gkMatchmakerViewController =
  sendMessage gkMatchmakerViewController hostedSelector

-- | set to YES to receive hosted (eg. not peer-to-peer) match results. Will cause the controller to return an array of players instead of a match.
--
-- ObjC selector: @- setHosted:@
setHosted :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> Bool -> IO ()
setHosted gkMatchmakerViewController value =
  sendMessage gkMatchmakerViewController setHostedSelector value

-- | this controls which mode of matchmaking to support in the UI (all, nearby only, automatch only, invite only).  Throws an exeption if you can not set to the desired mode (due to restrictions)
--
-- ObjC selector: @- matchmakingMode@
matchmakingMode :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> IO GKMatchmakingMode
matchmakingMode gkMatchmakerViewController =
  sendMessage gkMatchmakerViewController matchmakingModeSelector

-- | this controls which mode of matchmaking to support in the UI (all, nearby only, automatch only, invite only).  Throws an exeption if you can not set to the desired mode (due to restrictions)
--
-- ObjC selector: @- setMatchmakingMode:@
setMatchmakingMode :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> GKMatchmakingMode -> IO ()
setMatchmakingMode gkMatchmakerViewController value =
  sendMessage gkMatchmakerViewController setMatchmakingModeSelector value

-- | A BOOL value to allow the GKMatchMakerViewController to return control to the game once the minimum number of players are connected. By default the value is NO, and the multiplayer match can only proceed after all players are connected. If the value is set to YES, then once the number of connected players is greater than or equal to minPlayers of the match request, matchmakerViewController:didFindMatch: will be called and the game can get the match instance, and update the game scene accordingly. The remaining players wil continue to connect.
--
-- ObjC selector: @- canStartWithMinimumPlayers@
canStartWithMinimumPlayers :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> IO Bool
canStartWithMinimumPlayers gkMatchmakerViewController =
  sendMessage gkMatchmakerViewController canStartWithMinimumPlayersSelector

-- | A BOOL value to allow the GKMatchMakerViewController to return control to the game once the minimum number of players are connected. By default the value is NO, and the multiplayer match can only proceed after all players are connected. If the value is set to YES, then once the number of connected players is greater than or equal to minPlayers of the match request, matchmakerViewController:didFindMatch: will be called and the game can get the match instance, and update the game scene accordingly. The remaining players wil continue to connect.
--
-- ObjC selector: @- setCanStartWithMinimumPlayers:@
setCanStartWithMinimumPlayers :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> Bool -> IO ()
setCanStartWithMinimumPlayers gkMatchmakerViewController value =
  sendMessage gkMatchmakerViewController setCanStartWithMinimumPlayersSelector value

-- | deprecated, set the message on the match request instead
--
-- ObjC selector: @- defaultInvitationMessage@
defaultInvitationMessage :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> IO (Id NSString)
defaultInvitationMessage gkMatchmakerViewController =
  sendMessage gkMatchmakerViewController defaultInvitationMessageSelector

-- | deprecated, set the message on the match request instead
--
-- ObjC selector: @- setDefaultInvitationMessage:@
setDefaultInvitationMessage :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsNSString value) => gkMatchmakerViewController -> value -> IO ()
setDefaultInvitationMessage gkMatchmakerViewController value =
  sendMessage gkMatchmakerViewController setDefaultInvitationMessageSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMatchRequest:@
initWithMatchRequestSelector :: Selector '[Id GKMatchRequest] RawId
initWithMatchRequestSelector = mkSelector "initWithMatchRequest:"

-- | @Selector@ for @initWithInvite:@
initWithInviteSelector :: Selector '[Id GKInvite] RawId
initWithInviteSelector = mkSelector "initWithInvite:"

-- | @Selector@ for @addPlayersToMatch:@
addPlayersToMatchSelector :: Selector '[Id GKMatch] ()
addPlayersToMatchSelector = mkSelector "addPlayersToMatch:"

-- | @Selector@ for @setHostedPlayer:didConnect:@
setHostedPlayer_didConnectSelector :: Selector '[Id GKPlayer, Bool] ()
setHostedPlayer_didConnectSelector = mkSelector "setHostedPlayer:didConnect:"

-- | @Selector@ for @setHostedPlayer:connected:@
setHostedPlayer_connectedSelector :: Selector '[Id NSString, Bool] ()
setHostedPlayer_connectedSelector = mkSelector "setHostedPlayer:connected:"

-- | @Selector@ for @setHostedPlayerReady:@
setHostedPlayerReadySelector :: Selector '[Id NSString] ()
setHostedPlayerReadySelector = mkSelector "setHostedPlayerReady:"

-- | @Selector@ for @matchmakerDelegate@
matchmakerDelegateSelector :: Selector '[] RawId
matchmakerDelegateSelector = mkSelector "matchmakerDelegate"

-- | @Selector@ for @setMatchmakerDelegate:@
setMatchmakerDelegateSelector :: Selector '[RawId] ()
setMatchmakerDelegateSelector = mkSelector "setMatchmakerDelegate:"

-- | @Selector@ for @matchRequest@
matchRequestSelector :: Selector '[] (Id GKMatchRequest)
matchRequestSelector = mkSelector "matchRequest"

-- | @Selector@ for @hosted@
hostedSelector :: Selector '[] Bool
hostedSelector = mkSelector "hosted"

-- | @Selector@ for @setHosted:@
setHostedSelector :: Selector '[Bool] ()
setHostedSelector = mkSelector "setHosted:"

-- | @Selector@ for @matchmakingMode@
matchmakingModeSelector :: Selector '[] GKMatchmakingMode
matchmakingModeSelector = mkSelector "matchmakingMode"

-- | @Selector@ for @setMatchmakingMode:@
setMatchmakingModeSelector :: Selector '[GKMatchmakingMode] ()
setMatchmakingModeSelector = mkSelector "setMatchmakingMode:"

-- | @Selector@ for @canStartWithMinimumPlayers@
canStartWithMinimumPlayersSelector :: Selector '[] Bool
canStartWithMinimumPlayersSelector = mkSelector "canStartWithMinimumPlayers"

-- | @Selector@ for @setCanStartWithMinimumPlayers:@
setCanStartWithMinimumPlayersSelector :: Selector '[Bool] ()
setCanStartWithMinimumPlayersSelector = mkSelector "setCanStartWithMinimumPlayers:"

-- | @Selector@ for @defaultInvitationMessage@
defaultInvitationMessageSelector :: Selector '[] (Id NSString)
defaultInvitationMessageSelector = mkSelector "defaultInvitationMessage"

-- | @Selector@ for @setDefaultInvitationMessage:@
setDefaultInvitationMessageSelector :: Selector '[Id NSString] ()
setDefaultInvitationMessageSelector = mkSelector "setDefaultInvitationMessage:"

