{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | View controller that provides the standard user interface for leaderboards, achievements, and challenges. Present modally from the top view controller.
--
-- Generated bindings for @GKGameCenterViewController@.
module ObjC.GameKit.GKGameCenterViewController
  ( GKGameCenterViewController
  , IsGKGameCenterViewController(..)
  , initWithState
  , initWithLeaderboardID_playerScope_timeScope
  , initWithLeaderboard_playerScope
  , initWithLeaderboardSetID
  , initWithAchievementID
  , initWithPlayer
  , viewState
  , setViewState
  , leaderboardTimeScope
  , setLeaderboardTimeScope
  , leaderboardIdentifier
  , setLeaderboardIdentifier
  , initWithStateSelector
  , initWithLeaderboardID_playerScope_timeScopeSelector
  , initWithLeaderboard_playerScopeSelector
  , initWithLeaderboardSetIDSelector
  , initWithAchievementIDSelector
  , initWithPlayerSelector
  , viewStateSelector
  , setViewStateSelector
  , leaderboardTimeScopeSelector
  , setLeaderboardTimeScopeSelector
  , leaderboardIdentifierSelector
  , setLeaderboardIdentifierSelector

  -- * Enum types
  , GKGameCenterViewControllerState(GKGameCenterViewControllerState)
  , pattern GKGameCenterViewControllerStateDefault
  , pattern GKGameCenterViewControllerStateLeaderboards
  , pattern GKGameCenterViewControllerStateAchievements
  , pattern GKGameCenterViewControllerStateChallenges
  , pattern GKGameCenterViewControllerStateLocalPlayerProfile
  , pattern GKGameCenterViewControllerStateDashboard
  , pattern GKGameCenterViewControllerStateLocalPlayerFriendsList
  , GKLeaderboardPlayerScope(GKLeaderboardPlayerScope)
  , pattern GKLeaderboardPlayerScopeGlobal
  , pattern GKLeaderboardPlayerScopeFriendsOnly
  , GKLeaderboardTimeScope(GKLeaderboardTimeScope)
  , pattern GKLeaderboardTimeScopeToday
  , pattern GKLeaderboardTimeScopeWeek
  , pattern GKLeaderboardTimeScopeAllTime

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

-- | Use this to display content associated with the specified state. For example setting the state to GKGameCenterViewControllerStateLeaderboards will display a list of leaderboard sets or leaderboards (if no sets). Setting state to GKGameCenterViewControllerStateAchievements will display a list of achievements.
--
-- ObjC selector: @- initWithState:@
initWithState :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> GKGameCenterViewControllerState -> IO (Id GKGameCenterViewController)
initWithState gkGameCenterViewController  state =
  sendMsg gkGameCenterViewController (mkSelector "initWithState:") (retPtr retVoid) [argCLong (coerce state)] >>= ownedObject . castPtr

-- | Use this to display the scores for the specified leaderboardID, player scope and time scope. The time scope is only applicable to classic leaderboards. Recurring leaderboards will always be displayed initially with the results (scores) associated with the current instance of the leaderboard.
--
-- ObjC selector: @- initWithLeaderboardID:playerScope:timeScope:@
initWithLeaderboardID_playerScope_timeScope :: (IsGKGameCenterViewController gkGameCenterViewController, IsNSString leaderboardID) => gkGameCenterViewController -> leaderboardID -> GKLeaderboardPlayerScope -> GKLeaderboardTimeScope -> IO (Id GKGameCenterViewController)
initWithLeaderboardID_playerScope_timeScope gkGameCenterViewController  leaderboardID playerScope timeScope =
withObjCPtr leaderboardID $ \raw_leaderboardID ->
    sendMsg gkGameCenterViewController (mkSelector "initWithLeaderboardID:playerScope:timeScope:") (retPtr retVoid) [argPtr (castPtr raw_leaderboardID :: Ptr ()), argCLong (coerce playerScope), argCLong (coerce timeScope)] >>= ownedObject . castPtr

-- | Use this to display the scores for the specified leaderboard and player scope. Both classic and recurring leaderboards can use this method to initialize the view with their scores.
--
-- ObjC selector: @- initWithLeaderboard:playerScope:@
initWithLeaderboard_playerScope :: (IsGKGameCenterViewController gkGameCenterViewController, IsGKLeaderboard leaderboard) => gkGameCenterViewController -> leaderboard -> GKLeaderboardPlayerScope -> IO (Id GKGameCenterViewController)
initWithLeaderboard_playerScope gkGameCenterViewController  leaderboard playerScope =
withObjCPtr leaderboard $ \raw_leaderboard ->
    sendMsg gkGameCenterViewController (mkSelector "initWithLeaderboard:playerScope:") (retPtr retVoid) [argPtr (castPtr raw_leaderboard :: Ptr ()), argCLong (coerce playerScope)] >>= ownedObject . castPtr

-- | Use this to display the leaderboard sets for the specified leaderboardSetID.
--
-- ObjC selector: @- initWithLeaderboardSetID:@
initWithLeaderboardSetID :: (IsGKGameCenterViewController gkGameCenterViewController, IsNSString leaderboardSetID) => gkGameCenterViewController -> leaderboardSetID -> IO (Id GKGameCenterViewController)
initWithLeaderboardSetID gkGameCenterViewController  leaderboardSetID =
withObjCPtr leaderboardSetID $ \raw_leaderboardSetID ->
    sendMsg gkGameCenterViewController (mkSelector "initWithLeaderboardSetID:") (retPtr retVoid) [argPtr (castPtr raw_leaderboardSetID :: Ptr ())] >>= ownedObject . castPtr

-- | Use this to display the details associated with the specified achievementID
--
-- ObjC selector: @- initWithAchievementID:@
initWithAchievementID :: (IsGKGameCenterViewController gkGameCenterViewController, IsNSString achievementID) => gkGameCenterViewController -> achievementID -> IO (Id GKGameCenterViewController)
initWithAchievementID gkGameCenterViewController  achievementID =
withObjCPtr achievementID $ \raw_achievementID ->
    sendMsg gkGameCenterViewController (mkSelector "initWithAchievementID:") (retPtr retVoid) [argPtr (castPtr raw_achievementID :: Ptr ())] >>= ownedObject . castPtr

-- | Use this to display the profile page associated with the specified player.
--
-- ObjC selector: @- initWithPlayer:@
initWithPlayer :: (IsGKGameCenterViewController gkGameCenterViewController, IsGKPlayer player) => gkGameCenterViewController -> player -> IO (Id GKGameCenterViewController)
initWithPlayer gkGameCenterViewController  player =
withObjCPtr player $ \raw_player ->
    sendMsg gkGameCenterViewController (mkSelector "initWithPlayer:") (retPtr retVoid) [argPtr (castPtr raw_player :: Ptr ())] >>= ownedObject . castPtr

-- | @- viewState@
viewState :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> IO GKGameCenterViewControllerState
viewState gkGameCenterViewController  =
  fmap (coerce :: CLong -> GKGameCenterViewControllerState) $ sendMsg gkGameCenterViewController (mkSelector "viewState") retCLong []

-- | @- setViewState:@
setViewState :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> GKGameCenterViewControllerState -> IO ()
setViewState gkGameCenterViewController  value =
  sendMsg gkGameCenterViewController (mkSelector "setViewState:") retVoid [argCLong (coerce value)]

-- | @- leaderboardTimeScope@
leaderboardTimeScope :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> IO GKLeaderboardTimeScope
leaderboardTimeScope gkGameCenterViewController  =
  fmap (coerce :: CLong -> GKLeaderboardTimeScope) $ sendMsg gkGameCenterViewController (mkSelector "leaderboardTimeScope") retCLong []

-- | @- setLeaderboardTimeScope:@
setLeaderboardTimeScope :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> GKLeaderboardTimeScope -> IO ()
setLeaderboardTimeScope gkGameCenterViewController  value =
  sendMsg gkGameCenterViewController (mkSelector "setLeaderboardTimeScope:") retVoid [argCLong (coerce value)]

-- | @- leaderboardIdentifier@
leaderboardIdentifier :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> IO (Id NSString)
leaderboardIdentifier gkGameCenterViewController  =
  sendMsg gkGameCenterViewController (mkSelector "leaderboardIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLeaderboardIdentifier:@
setLeaderboardIdentifier :: (IsGKGameCenterViewController gkGameCenterViewController, IsNSString value) => gkGameCenterViewController -> value -> IO ()
setLeaderboardIdentifier gkGameCenterViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkGameCenterViewController (mkSelector "setLeaderboardIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithState:@
initWithStateSelector :: Selector
initWithStateSelector = mkSelector "initWithState:"

-- | @Selector@ for @initWithLeaderboardID:playerScope:timeScope:@
initWithLeaderboardID_playerScope_timeScopeSelector :: Selector
initWithLeaderboardID_playerScope_timeScopeSelector = mkSelector "initWithLeaderboardID:playerScope:timeScope:"

-- | @Selector@ for @initWithLeaderboard:playerScope:@
initWithLeaderboard_playerScopeSelector :: Selector
initWithLeaderboard_playerScopeSelector = mkSelector "initWithLeaderboard:playerScope:"

-- | @Selector@ for @initWithLeaderboardSetID:@
initWithLeaderboardSetIDSelector :: Selector
initWithLeaderboardSetIDSelector = mkSelector "initWithLeaderboardSetID:"

-- | @Selector@ for @initWithAchievementID:@
initWithAchievementIDSelector :: Selector
initWithAchievementIDSelector = mkSelector "initWithAchievementID:"

-- | @Selector@ for @initWithPlayer:@
initWithPlayerSelector :: Selector
initWithPlayerSelector = mkSelector "initWithPlayer:"

-- | @Selector@ for @viewState@
viewStateSelector :: Selector
viewStateSelector = mkSelector "viewState"

-- | @Selector@ for @setViewState:@
setViewStateSelector :: Selector
setViewStateSelector = mkSelector "setViewState:"

-- | @Selector@ for @leaderboardTimeScope@
leaderboardTimeScopeSelector :: Selector
leaderboardTimeScopeSelector = mkSelector "leaderboardTimeScope"

-- | @Selector@ for @setLeaderboardTimeScope:@
setLeaderboardTimeScopeSelector :: Selector
setLeaderboardTimeScopeSelector = mkSelector "setLeaderboardTimeScope:"

-- | @Selector@ for @leaderboardIdentifier@
leaderboardIdentifierSelector :: Selector
leaderboardIdentifierSelector = mkSelector "leaderboardIdentifier"

-- | @Selector@ for @setLeaderboardIdentifier:@
setLeaderboardIdentifierSelector :: Selector
setLeaderboardIdentifierSelector = mkSelector "setLeaderboardIdentifier:"

