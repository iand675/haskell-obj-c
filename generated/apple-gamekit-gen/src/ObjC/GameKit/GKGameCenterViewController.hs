{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , leaderboardCategory
  , setLeaderboardCategory
  , gameCenterDelegate
  , setGameCenterDelegate
  , gameCenterDelegateSelector
  , initWithAchievementIDSelector
  , initWithLeaderboardID_playerScope_timeScopeSelector
  , initWithLeaderboardSetIDSelector
  , initWithLeaderboard_playerScopeSelector
  , initWithPlayerSelector
  , initWithStateSelector
  , leaderboardCategorySelector
  , leaderboardIdentifierSelector
  , leaderboardTimeScopeSelector
  , setGameCenterDelegateSelector
  , setLeaderboardCategorySelector
  , setLeaderboardIdentifierSelector
  , setLeaderboardTimeScopeSelector
  , setViewStateSelector
  , viewStateSelector

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

-- | Use this to display content associated with the specified state. For example setting the state to GKGameCenterViewControllerStateLeaderboards will display a list of leaderboard sets or leaderboards (if no sets). Setting state to GKGameCenterViewControllerStateAchievements will display a list of achievements.
--
-- ObjC selector: @- initWithState:@
initWithState :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> GKGameCenterViewControllerState -> IO (Id GKGameCenterViewController)
initWithState gkGameCenterViewController state =
  sendOwnedMessage gkGameCenterViewController initWithStateSelector state

-- | Use this to display the scores for the specified leaderboardID, player scope and time scope. The time scope is only applicable to classic leaderboards. Recurring leaderboards will always be displayed initially with the results (scores) associated with the current instance of the leaderboard.
--
-- ObjC selector: @- initWithLeaderboardID:playerScope:timeScope:@
initWithLeaderboardID_playerScope_timeScope :: (IsGKGameCenterViewController gkGameCenterViewController, IsNSString leaderboardID) => gkGameCenterViewController -> leaderboardID -> GKLeaderboardPlayerScope -> GKLeaderboardTimeScope -> IO (Id GKGameCenterViewController)
initWithLeaderboardID_playerScope_timeScope gkGameCenterViewController leaderboardID playerScope timeScope =
  sendOwnedMessage gkGameCenterViewController initWithLeaderboardID_playerScope_timeScopeSelector (toNSString leaderboardID) playerScope timeScope

-- | Use this to display the scores for the specified leaderboard and player scope. Both classic and recurring leaderboards can use this method to initialize the view with their scores.
--
-- ObjC selector: @- initWithLeaderboard:playerScope:@
initWithLeaderboard_playerScope :: (IsGKGameCenterViewController gkGameCenterViewController, IsGKLeaderboard leaderboard) => gkGameCenterViewController -> leaderboard -> GKLeaderboardPlayerScope -> IO (Id GKGameCenterViewController)
initWithLeaderboard_playerScope gkGameCenterViewController leaderboard playerScope =
  sendOwnedMessage gkGameCenterViewController initWithLeaderboard_playerScopeSelector (toGKLeaderboard leaderboard) playerScope

-- | Use this to display the leaderboard sets for the specified leaderboardSetID.
--
-- ObjC selector: @- initWithLeaderboardSetID:@
initWithLeaderboardSetID :: (IsGKGameCenterViewController gkGameCenterViewController, IsNSString leaderboardSetID) => gkGameCenterViewController -> leaderboardSetID -> IO (Id GKGameCenterViewController)
initWithLeaderboardSetID gkGameCenterViewController leaderboardSetID =
  sendOwnedMessage gkGameCenterViewController initWithLeaderboardSetIDSelector (toNSString leaderboardSetID)

-- | Use this to display the details associated with the specified achievementID
--
-- ObjC selector: @- initWithAchievementID:@
initWithAchievementID :: (IsGKGameCenterViewController gkGameCenterViewController, IsNSString achievementID) => gkGameCenterViewController -> achievementID -> IO (Id GKGameCenterViewController)
initWithAchievementID gkGameCenterViewController achievementID =
  sendOwnedMessage gkGameCenterViewController initWithAchievementIDSelector (toNSString achievementID)

-- | Use this to display the profile page associated with the specified player.
--
-- ObjC selector: @- initWithPlayer:@
initWithPlayer :: (IsGKGameCenterViewController gkGameCenterViewController, IsGKPlayer player) => gkGameCenterViewController -> player -> IO (Id GKGameCenterViewController)
initWithPlayer gkGameCenterViewController player =
  sendOwnedMessage gkGameCenterViewController initWithPlayerSelector (toGKPlayer player)

-- | @- viewState@
viewState :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> IO GKGameCenterViewControllerState
viewState gkGameCenterViewController =
  sendMessage gkGameCenterViewController viewStateSelector

-- | @- setViewState:@
setViewState :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> GKGameCenterViewControllerState -> IO ()
setViewState gkGameCenterViewController value =
  sendMessage gkGameCenterViewController setViewStateSelector value

-- | @- leaderboardTimeScope@
leaderboardTimeScope :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> IO GKLeaderboardTimeScope
leaderboardTimeScope gkGameCenterViewController =
  sendMessage gkGameCenterViewController leaderboardTimeScopeSelector

-- | @- setLeaderboardTimeScope:@
setLeaderboardTimeScope :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> GKLeaderboardTimeScope -> IO ()
setLeaderboardTimeScope gkGameCenterViewController value =
  sendMessage gkGameCenterViewController setLeaderboardTimeScopeSelector value

-- | @- leaderboardIdentifier@
leaderboardIdentifier :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> IO (Id NSString)
leaderboardIdentifier gkGameCenterViewController =
  sendMessage gkGameCenterViewController leaderboardIdentifierSelector

-- | @- setLeaderboardIdentifier:@
setLeaderboardIdentifier :: (IsGKGameCenterViewController gkGameCenterViewController, IsNSString value) => gkGameCenterViewController -> value -> IO ()
setLeaderboardIdentifier gkGameCenterViewController value =
  sendMessage gkGameCenterViewController setLeaderboardIdentifierSelector (toNSString value)

-- | @- leaderboardCategory@
leaderboardCategory :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> IO (Id NSString)
leaderboardCategory gkGameCenterViewController =
  sendMessage gkGameCenterViewController leaderboardCategorySelector

-- | @- setLeaderboardCategory:@
setLeaderboardCategory :: (IsGKGameCenterViewController gkGameCenterViewController, IsNSString value) => gkGameCenterViewController -> value -> IO ()
setLeaderboardCategory gkGameCenterViewController value =
  sendMessage gkGameCenterViewController setLeaderboardCategorySelector (toNSString value)

-- | @- gameCenterDelegate@
gameCenterDelegate :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> IO RawId
gameCenterDelegate gkGameCenterViewController =
  sendMessage gkGameCenterViewController gameCenterDelegateSelector

-- | @- setGameCenterDelegate:@
setGameCenterDelegate :: IsGKGameCenterViewController gkGameCenterViewController => gkGameCenterViewController -> RawId -> IO ()
setGameCenterDelegate gkGameCenterViewController value =
  sendMessage gkGameCenterViewController setGameCenterDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithState:@
initWithStateSelector :: Selector '[GKGameCenterViewControllerState] (Id GKGameCenterViewController)
initWithStateSelector = mkSelector "initWithState:"

-- | @Selector@ for @initWithLeaderboardID:playerScope:timeScope:@
initWithLeaderboardID_playerScope_timeScopeSelector :: Selector '[Id NSString, GKLeaderboardPlayerScope, GKLeaderboardTimeScope] (Id GKGameCenterViewController)
initWithLeaderboardID_playerScope_timeScopeSelector = mkSelector "initWithLeaderboardID:playerScope:timeScope:"

-- | @Selector@ for @initWithLeaderboard:playerScope:@
initWithLeaderboard_playerScopeSelector :: Selector '[Id GKLeaderboard, GKLeaderboardPlayerScope] (Id GKGameCenterViewController)
initWithLeaderboard_playerScopeSelector = mkSelector "initWithLeaderboard:playerScope:"

-- | @Selector@ for @initWithLeaderboardSetID:@
initWithLeaderboardSetIDSelector :: Selector '[Id NSString] (Id GKGameCenterViewController)
initWithLeaderboardSetIDSelector = mkSelector "initWithLeaderboardSetID:"

-- | @Selector@ for @initWithAchievementID:@
initWithAchievementIDSelector :: Selector '[Id NSString] (Id GKGameCenterViewController)
initWithAchievementIDSelector = mkSelector "initWithAchievementID:"

-- | @Selector@ for @initWithPlayer:@
initWithPlayerSelector :: Selector '[Id GKPlayer] (Id GKGameCenterViewController)
initWithPlayerSelector = mkSelector "initWithPlayer:"

-- | @Selector@ for @viewState@
viewStateSelector :: Selector '[] GKGameCenterViewControllerState
viewStateSelector = mkSelector "viewState"

-- | @Selector@ for @setViewState:@
setViewStateSelector :: Selector '[GKGameCenterViewControllerState] ()
setViewStateSelector = mkSelector "setViewState:"

-- | @Selector@ for @leaderboardTimeScope@
leaderboardTimeScopeSelector :: Selector '[] GKLeaderboardTimeScope
leaderboardTimeScopeSelector = mkSelector "leaderboardTimeScope"

-- | @Selector@ for @setLeaderboardTimeScope:@
setLeaderboardTimeScopeSelector :: Selector '[GKLeaderboardTimeScope] ()
setLeaderboardTimeScopeSelector = mkSelector "setLeaderboardTimeScope:"

-- | @Selector@ for @leaderboardIdentifier@
leaderboardIdentifierSelector :: Selector '[] (Id NSString)
leaderboardIdentifierSelector = mkSelector "leaderboardIdentifier"

-- | @Selector@ for @setLeaderboardIdentifier:@
setLeaderboardIdentifierSelector :: Selector '[Id NSString] ()
setLeaderboardIdentifierSelector = mkSelector "setLeaderboardIdentifier:"

-- | @Selector@ for @leaderboardCategory@
leaderboardCategorySelector :: Selector '[] (Id NSString)
leaderboardCategorySelector = mkSelector "leaderboardCategory"

-- | @Selector@ for @setLeaderboardCategory:@
setLeaderboardCategorySelector :: Selector '[Id NSString] ()
setLeaderboardCategorySelector = mkSelector "setLeaderboardCategory:"

-- | @Selector@ for @gameCenterDelegate@
gameCenterDelegateSelector :: Selector '[] RawId
gameCenterDelegateSelector = mkSelector "gameCenterDelegate"

-- | @Selector@ for @setGameCenterDelegate:@
setGameCenterDelegateSelector :: Selector '[RawId] ()
setGameCenterDelegateSelector = mkSelector "setGameCenterDelegate:"

