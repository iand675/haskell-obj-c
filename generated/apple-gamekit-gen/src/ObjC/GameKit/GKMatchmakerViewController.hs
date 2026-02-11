{-# LANGUAGE PatternSynonyms #-}
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
  , initWithMatchRequestSelector
  , initWithInviteSelector
  , addPlayersToMatchSelector
  , setHostedPlayer_didConnectSelector
  , setHostedPlayer_connectedSelector
  , setHostedPlayerReadySelector
  , matchmakerDelegateSelector
  , setMatchmakerDelegateSelector
  , matchRequestSelector
  , hostedSelector
  , setHostedSelector
  , matchmakingModeSelector
  , setMatchmakingModeSelector
  , canStartWithMinimumPlayersSelector
  , setCanStartWithMinimumPlayersSelector
  , defaultInvitationMessageSelector
  , setDefaultInvitationMessageSelector

  -- * Enum types
  , GKMatchmakingMode(GKMatchmakingMode)
  , pattern GKMatchmakingModeDefault
  , pattern GKMatchmakingModeNearbyOnly
  , pattern GKMatchmakingModeAutomatchOnly
  , pattern GKMatchmakingModeInviteOnly

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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize with a matchmaking request, allowing the user to send invites and/or start matchmaking
--
-- ObjC selector: @- initWithMatchRequest:@
initWithMatchRequest :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsGKMatchRequest request) => gkMatchmakerViewController -> request -> IO RawId
initWithMatchRequest gkMatchmakerViewController  request =
  withObjCPtr request $ \raw_request ->
      fmap (RawId . castPtr) $ sendMsg gkMatchmakerViewController (mkSelector "initWithMatchRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())]

-- | Initialize with an accepted invite, allowing the user to see the status of other invited players and get notified when the game starts
--
-- ObjC selector: @- initWithInvite:@
initWithInvite :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsGKInvite invite) => gkMatchmakerViewController -> invite -> IO RawId
initWithInvite gkMatchmakerViewController  invite =
  withObjCPtr invite $ \raw_invite ->
      fmap (RawId . castPtr) $ sendMsg gkMatchmakerViewController (mkSelector "initWithInvite:") (retPtr retVoid) [argPtr (castPtr raw_invite :: Ptr ())]

-- | Add additional players (not currently connected) to an existing peer-to-peer match. Apps should elect a single device to do this, otherwise conflicts could arise resulting in unexpected connection errors.
--
-- ObjC selector: @- addPlayersToMatch:@
addPlayersToMatch :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsGKMatch match) => gkMatchmakerViewController -> match -> IO ()
addPlayersToMatch gkMatchmakerViewController  match =
  withObjCPtr match $ \raw_match ->
      sendMsg gkMatchmakerViewController (mkSelector "addPlayersToMatch:") retVoid [argPtr (castPtr raw_match :: Ptr ())]

-- | @- setHostedPlayer:didConnect:@
setHostedPlayer_didConnect :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsGKPlayer player) => gkMatchmakerViewController -> player -> Bool -> IO ()
setHostedPlayer_didConnect gkMatchmakerViewController  player connected =
  withObjCPtr player $ \raw_player ->
      sendMsg gkMatchmakerViewController (mkSelector "setHostedPlayer:didConnect:") retVoid [argPtr (castPtr raw_player :: Ptr ()), argCULong (if connected then 1 else 0)]

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- setHostedPlayer:connected:@
setHostedPlayer_connected :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsNSString playerID) => gkMatchmakerViewController -> playerID -> Bool -> IO ()
setHostedPlayer_connected gkMatchmakerViewController  playerID connected =
  withObjCPtr playerID $ \raw_playerID ->
      sendMsg gkMatchmakerViewController (mkSelector "setHostedPlayer:connected:") retVoid [argPtr (castPtr raw_playerID :: Ptr ()), argCULong (if connected then 1 else 0)]

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- setHostedPlayerReady:@
setHostedPlayerReady :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsNSString playerID) => gkMatchmakerViewController -> playerID -> IO ()
setHostedPlayerReady gkMatchmakerViewController  playerID =
  withObjCPtr playerID $ \raw_playerID ->
      sendMsg gkMatchmakerViewController (mkSelector "setHostedPlayerReady:") retVoid [argPtr (castPtr raw_playerID :: Ptr ())]

-- | @- matchmakerDelegate@
matchmakerDelegate :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> IO RawId
matchmakerDelegate gkMatchmakerViewController  =
    fmap (RawId . castPtr) $ sendMsg gkMatchmakerViewController (mkSelector "matchmakerDelegate") (retPtr retVoid) []

-- | @- setMatchmakerDelegate:@
setMatchmakerDelegate :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> RawId -> IO ()
setMatchmakerDelegate gkMatchmakerViewController  value =
    sendMsg gkMatchmakerViewController (mkSelector "setMatchmakerDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- matchRequest@
matchRequest :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> IO (Id GKMatchRequest)
matchRequest gkMatchmakerViewController  =
    sendMsg gkMatchmakerViewController (mkSelector "matchRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | set to YES to receive hosted (eg. not peer-to-peer) match results. Will cause the controller to return an array of players instead of a match.
--
-- ObjC selector: @- hosted@
hosted :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> IO Bool
hosted gkMatchmakerViewController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkMatchmakerViewController (mkSelector "hosted") retCULong []

-- | set to YES to receive hosted (eg. not peer-to-peer) match results. Will cause the controller to return an array of players instead of a match.
--
-- ObjC selector: @- setHosted:@
setHosted :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> Bool -> IO ()
setHosted gkMatchmakerViewController  value =
    sendMsg gkMatchmakerViewController (mkSelector "setHosted:") retVoid [argCULong (if value then 1 else 0)]

-- | this controls which mode of matchmaking to support in the UI (all, nearby only, automatch only, invite only).  Throws an exeption if you can not set to the desired mode (due to restrictions)
--
-- ObjC selector: @- matchmakingMode@
matchmakingMode :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> IO GKMatchmakingMode
matchmakingMode gkMatchmakerViewController  =
    fmap (coerce :: CLong -> GKMatchmakingMode) $ sendMsg gkMatchmakerViewController (mkSelector "matchmakingMode") retCLong []

-- | this controls which mode of matchmaking to support in the UI (all, nearby only, automatch only, invite only).  Throws an exeption if you can not set to the desired mode (due to restrictions)
--
-- ObjC selector: @- setMatchmakingMode:@
setMatchmakingMode :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> GKMatchmakingMode -> IO ()
setMatchmakingMode gkMatchmakerViewController  value =
    sendMsg gkMatchmakerViewController (mkSelector "setMatchmakingMode:") retVoid [argCLong (coerce value)]

-- | A BOOL value to allow the GKMatchMakerViewController to return control to the game once the minimum number of players are connected. By default the value is NO, and the multiplayer match can only proceed after all players are connected. If the value is set to YES, then once the number of connected players is greater than or equal to minPlayers of the match request, matchmakerViewController:didFindMatch: will be called and the game can get the match instance, and update the game scene accordingly. The remaining players wil continue to connect.
--
-- ObjC selector: @- canStartWithMinimumPlayers@
canStartWithMinimumPlayers :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> IO Bool
canStartWithMinimumPlayers gkMatchmakerViewController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkMatchmakerViewController (mkSelector "canStartWithMinimumPlayers") retCULong []

-- | A BOOL value to allow the GKMatchMakerViewController to return control to the game once the minimum number of players are connected. By default the value is NO, and the multiplayer match can only proceed after all players are connected. If the value is set to YES, then once the number of connected players is greater than or equal to minPlayers of the match request, matchmakerViewController:didFindMatch: will be called and the game can get the match instance, and update the game scene accordingly. The remaining players wil continue to connect.
--
-- ObjC selector: @- setCanStartWithMinimumPlayers:@
setCanStartWithMinimumPlayers :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> Bool -> IO ()
setCanStartWithMinimumPlayers gkMatchmakerViewController  value =
    sendMsg gkMatchmakerViewController (mkSelector "setCanStartWithMinimumPlayers:") retVoid [argCULong (if value then 1 else 0)]

-- | deprecated, set the message on the match request instead
--
-- ObjC selector: @- defaultInvitationMessage@
defaultInvitationMessage :: IsGKMatchmakerViewController gkMatchmakerViewController => gkMatchmakerViewController -> IO (Id NSString)
defaultInvitationMessage gkMatchmakerViewController  =
    sendMsg gkMatchmakerViewController (mkSelector "defaultInvitationMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | deprecated, set the message on the match request instead
--
-- ObjC selector: @- setDefaultInvitationMessage:@
setDefaultInvitationMessage :: (IsGKMatchmakerViewController gkMatchmakerViewController, IsNSString value) => gkMatchmakerViewController -> value -> IO ()
setDefaultInvitationMessage gkMatchmakerViewController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg gkMatchmakerViewController (mkSelector "setDefaultInvitationMessage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMatchRequest:@
initWithMatchRequestSelector :: Selector
initWithMatchRequestSelector = mkSelector "initWithMatchRequest:"

-- | @Selector@ for @initWithInvite:@
initWithInviteSelector :: Selector
initWithInviteSelector = mkSelector "initWithInvite:"

-- | @Selector@ for @addPlayersToMatch:@
addPlayersToMatchSelector :: Selector
addPlayersToMatchSelector = mkSelector "addPlayersToMatch:"

-- | @Selector@ for @setHostedPlayer:didConnect:@
setHostedPlayer_didConnectSelector :: Selector
setHostedPlayer_didConnectSelector = mkSelector "setHostedPlayer:didConnect:"

-- | @Selector@ for @setHostedPlayer:connected:@
setHostedPlayer_connectedSelector :: Selector
setHostedPlayer_connectedSelector = mkSelector "setHostedPlayer:connected:"

-- | @Selector@ for @setHostedPlayerReady:@
setHostedPlayerReadySelector :: Selector
setHostedPlayerReadySelector = mkSelector "setHostedPlayerReady:"

-- | @Selector@ for @matchmakerDelegate@
matchmakerDelegateSelector :: Selector
matchmakerDelegateSelector = mkSelector "matchmakerDelegate"

-- | @Selector@ for @setMatchmakerDelegate:@
setMatchmakerDelegateSelector :: Selector
setMatchmakerDelegateSelector = mkSelector "setMatchmakerDelegate:"

-- | @Selector@ for @matchRequest@
matchRequestSelector :: Selector
matchRequestSelector = mkSelector "matchRequest"

-- | @Selector@ for @hosted@
hostedSelector :: Selector
hostedSelector = mkSelector "hosted"

-- | @Selector@ for @setHosted:@
setHostedSelector :: Selector
setHostedSelector = mkSelector "setHosted:"

-- | @Selector@ for @matchmakingMode@
matchmakingModeSelector :: Selector
matchmakingModeSelector = mkSelector "matchmakingMode"

-- | @Selector@ for @setMatchmakingMode:@
setMatchmakingModeSelector :: Selector
setMatchmakingModeSelector = mkSelector "setMatchmakingMode:"

-- | @Selector@ for @canStartWithMinimumPlayers@
canStartWithMinimumPlayersSelector :: Selector
canStartWithMinimumPlayersSelector = mkSelector "canStartWithMinimumPlayers"

-- | @Selector@ for @setCanStartWithMinimumPlayers:@
setCanStartWithMinimumPlayersSelector :: Selector
setCanStartWithMinimumPlayersSelector = mkSelector "setCanStartWithMinimumPlayers:"

-- | @Selector@ for @defaultInvitationMessage@
defaultInvitationMessageSelector :: Selector
defaultInvitationMessageSelector = mkSelector "defaultInvitationMessage"

-- | @Selector@ for @setDefaultInvitationMessage:@
setDefaultInvitationMessageSelector :: Selector
setDefaultInvitationMessageSelector = mkSelector "setDefaultInvitationMessage:"

