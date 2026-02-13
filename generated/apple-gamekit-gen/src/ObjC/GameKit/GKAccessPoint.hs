{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKAccessPoint@.
module ObjC.GameKit.GKAccessPoint
  ( GKAccessPoint
  , IsGKAccessPoint(..)
  , triggerAccessPointWithHandler
  , triggerAccessPointWithState_handler
  , triggerAccessPointWithAchievementID_handler
  , triggerAccessPointWithLeaderboardSetID_handler
  , triggerAccessPointWithLeaderboardID_playerScope_timeScope_handler
  , triggerAccessPointWithPlayer_handler
  , triggerAccessPointForPlayTogetherWithHandler
  , triggerAccessPointForChallengesWithHandler
  , triggerAccessPointWithChallengeDefinitionID_handler
  , triggerAccessPointWithGameActivityDefinitionID_handler
  , triggerAccessPointWithGameActivity_handler
  , triggerAccessPointForFriendingWithHandler
  , triggerAccessPointForArcadeWithHandler
  , shared
  , active
  , setActive
  , focused
  , setFocused
  , visible
  , isPresentingGameCenter
  , showHighlights
  , setShowHighlights
  , location
  , setLocation
  , frameInScreenCoordinates
  , parentWindow
  , setParentWindow
  , activeSelector
  , focusedSelector
  , frameInScreenCoordinatesSelector
  , isPresentingGameCenterSelector
  , locationSelector
  , parentWindowSelector
  , setActiveSelector
  , setFocusedSelector
  , setLocationSelector
  , setParentWindowSelector
  , setShowHighlightsSelector
  , sharedSelector
  , showHighlightsSelector
  , triggerAccessPointForArcadeWithHandlerSelector
  , triggerAccessPointForChallengesWithHandlerSelector
  , triggerAccessPointForFriendingWithHandlerSelector
  , triggerAccessPointForPlayTogetherWithHandlerSelector
  , triggerAccessPointWithAchievementID_handlerSelector
  , triggerAccessPointWithChallengeDefinitionID_handlerSelector
  , triggerAccessPointWithGameActivityDefinitionID_handlerSelector
  , triggerAccessPointWithGameActivity_handlerSelector
  , triggerAccessPointWithHandlerSelector
  , triggerAccessPointWithLeaderboardID_playerScope_timeScope_handlerSelector
  , triggerAccessPointWithLeaderboardSetID_handlerSelector
  , triggerAccessPointWithPlayer_handlerSelector
  , triggerAccessPointWithState_handlerSelector
  , visibleSelector

  -- * Enum types
  , GKAccessPointLocation(GKAccessPointLocation)
  , pattern GKAccessPointLocationTopLeading
  , pattern GKAccessPointLocationTopTrailing
  , pattern GKAccessPointLocationBottomLeading
  , pattern GKAccessPointLocationBottomTrailing
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
import ObjC.Foundation.Internal.Structs
import ObjC.GameKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | this lets the developer trigger the access point as if the user had touched it.  This is useful for games that use controllers or the remote on AppleTV.  the argument lets you specify a specific state (default, profile, achievements, leaderboards) for GameCenterViewController
--
-- ObjC selector: @- triggerAccessPointWithHandler:@
triggerAccessPointWithHandler :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Ptr () -> IO ()
triggerAccessPointWithHandler gkAccessPoint handler =
  sendMessage gkAccessPoint triggerAccessPointWithHandlerSelector handler

-- | @- triggerAccessPointWithState:handler:@
triggerAccessPointWithState_handler :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> GKGameCenterViewControllerState -> Ptr () -> IO ()
triggerAccessPointWithState_handler gkAccessPoint state handler =
  sendMessage gkAccessPoint triggerAccessPointWithState_handlerSelector state handler

-- | @- triggerAccessPointWithAchievementID:handler:@
triggerAccessPointWithAchievementID_handler :: (IsGKAccessPoint gkAccessPoint, IsNSString achievementID) => gkAccessPoint -> achievementID -> Ptr () -> IO ()
triggerAccessPointWithAchievementID_handler gkAccessPoint achievementID handler =
  sendMessage gkAccessPoint triggerAccessPointWithAchievementID_handlerSelector (toNSString achievementID) handler

-- | @- triggerAccessPointWithLeaderboardSetID:handler:@
triggerAccessPointWithLeaderboardSetID_handler :: (IsGKAccessPoint gkAccessPoint, IsNSString leaderboardSetID) => gkAccessPoint -> leaderboardSetID -> Ptr () -> IO ()
triggerAccessPointWithLeaderboardSetID_handler gkAccessPoint leaderboardSetID handler =
  sendMessage gkAccessPoint triggerAccessPointWithLeaderboardSetID_handlerSelector (toNSString leaderboardSetID) handler

-- | @- triggerAccessPointWithLeaderboardID:playerScope:timeScope:handler:@
triggerAccessPointWithLeaderboardID_playerScope_timeScope_handler :: (IsGKAccessPoint gkAccessPoint, IsNSString leaderboardID) => gkAccessPoint -> leaderboardID -> GKLeaderboardPlayerScope -> GKLeaderboardTimeScope -> Ptr () -> IO ()
triggerAccessPointWithLeaderboardID_playerScope_timeScope_handler gkAccessPoint leaderboardID playerScope timeScope handler =
  sendMessage gkAccessPoint triggerAccessPointWithLeaderboardID_playerScope_timeScope_handlerSelector (toNSString leaderboardID) playerScope timeScope handler

-- | @- triggerAccessPointWithPlayer:handler:@
triggerAccessPointWithPlayer_handler :: (IsGKAccessPoint gkAccessPoint, IsGKPlayer player) => gkAccessPoint -> player -> Ptr () -> IO ()
triggerAccessPointWithPlayer_handler gkAccessPoint player handler =
  sendMessage gkAccessPoint triggerAccessPointWithPlayer_handlerSelector (toGKPlayer player) handler

-- | Displays the view that allows players to engage each other with activities and challenges.
--
-- ObjC selector: @- triggerAccessPointForPlayTogetherWithHandler:@
triggerAccessPointForPlayTogetherWithHandler :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Ptr () -> IO ()
triggerAccessPointForPlayTogetherWithHandler gkAccessPoint handler =
  sendMessage gkAccessPoint triggerAccessPointForPlayTogetherWithHandlerSelector handler

-- | Displays the view that allows players to engage each other with challenges.
--
-- ObjC selector: @- triggerAccessPointForChallengesWithHandler:@
triggerAccessPointForChallengesWithHandler :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Ptr () -> IO ()
triggerAccessPointForChallengesWithHandler gkAccessPoint handler =
  sendMessage gkAccessPoint triggerAccessPointForChallengesWithHandlerSelector handler

-- | Displays the challenge creation view for the provided challenge definition ID.
--
-- ObjC selector: @- triggerAccessPointWithChallengeDefinitionID:handler:@
triggerAccessPointWithChallengeDefinitionID_handler :: (IsGKAccessPoint gkAccessPoint, IsNSString challengeDefinitionID) => gkAccessPoint -> challengeDefinitionID -> Ptr () -> IO ()
triggerAccessPointWithChallengeDefinitionID_handler gkAccessPoint challengeDefinitionID handler =
  sendMessage gkAccessPoint triggerAccessPointWithChallengeDefinitionID_handlerSelector (toNSString challengeDefinitionID) handler

-- | Displays the game activity creation view for the provided activity definition ID.
--
-- ObjC selector: @- triggerAccessPointWithGameActivityDefinitionID:handler:@
triggerAccessPointWithGameActivityDefinitionID_handler :: (IsGKAccessPoint gkAccessPoint, IsNSString gameActivityDefinitionID) => gkAccessPoint -> gameActivityDefinitionID -> Ptr () -> IO ()
triggerAccessPointWithGameActivityDefinitionID_handler gkAccessPoint gameActivityDefinitionID handler =
  sendMessage gkAccessPoint triggerAccessPointWithGameActivityDefinitionID_handlerSelector (toNSString gameActivityDefinitionID) handler

-- | Displays the game activity view for the provided activity instance.
--
-- ObjC selector: @- triggerAccessPointWithGameActivity:handler:@
triggerAccessPointWithGameActivity_handler :: (IsGKAccessPoint gkAccessPoint, IsGKGameActivity gameActivity) => gkAccessPoint -> gameActivity -> Ptr () -> IO ()
triggerAccessPointWithGameActivity_handler gkAccessPoint gameActivity handler =
  sendMessage gkAccessPoint triggerAccessPointWithGameActivity_handlerSelector (toGKGameActivity gameActivity) handler

-- | Brings up the invite friends view.
--
-- ObjC selector: @- triggerAccessPointForFriendingWithHandler:@
triggerAccessPointForFriendingWithHandler :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Ptr () -> IO ()
triggerAccessPointForFriendingWithHandler gkAccessPoint handler =
  sendMessage gkAccessPoint triggerAccessPointForFriendingWithHandlerSelector handler

-- | Brings up the Arcade dashboard.
--
-- ObjC selector: @- triggerAccessPointForArcadeWithHandler:@
triggerAccessPointForArcadeWithHandler :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Ptr () -> IO ()
triggerAccessPointForArcadeWithHandler gkAccessPoint handler =
  sendMessage gkAccessPoint triggerAccessPointForArcadeWithHandlerSelector handler

-- | @+ shared@
shared :: IO (Id GKAccessPoint)
shared  =
  do
    cls' <- getRequiredClass "GKAccessPoint"
    sendClassMessage cls' sharedSelector

-- | set this true to enable access point in your app.  Setting this will cause the access point to appear after the notification banner is presented.  If it already was presented it will appear immediately
--
-- ObjC selector: @- active@
active :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO Bool
active gkAccessPoint =
  sendMessage gkAccessPoint activeSelector

-- | set this true to enable access point in your app.  Setting this will cause the access point to appear after the notification banner is presented.  If it already was presented it will appear immediately
--
-- ObjC selector: @- setActive:@
setActive :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Bool -> IO ()
setActive gkAccessPoint value =
  sendMessage gkAccessPoint setActiveSelector value

-- | set this on tvOS to put the accessPoint into focused mode
--
-- ObjC selector: @- focused@
focused :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO Bool
focused gkAccessPoint =
  sendMessage gkAccessPoint focusedSelector

-- | set this on tvOS to put the accessPoint into focused mode
--
-- ObjC selector: @- setFocused:@
setFocused :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Bool -> IO ()
setFocused gkAccessPoint value =
  sendMessage gkAccessPoint setFocusedSelector value

-- | @- visible@
visible :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO Bool
visible gkAccessPoint =
  sendMessage gkAccessPoint visibleSelector

-- | observable property that indicates when the access point is visible.
--
-- ObjC selector: @- isPresentingGameCenter@
isPresentingGameCenter :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO Bool
isPresentingGameCenter gkAccessPoint =
  sendMessage gkAccessPoint isPresentingGameCenterSelector

-- | Set this property to true if you wish to show the highlights for most recent achievement, current rank on default leaderboard, etc
--
-- ObjC selector: @- showHighlights@
showHighlights :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO Bool
showHighlights gkAccessPoint =
  sendMessage gkAccessPoint showHighlightsSelector

-- | Set this property to true if you wish to show the highlights for most recent achievement, current rank on default leaderboard, etc
--
-- ObjC selector: @- setShowHighlights:@
setShowHighlights :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Bool -> IO ()
setShowHighlights gkAccessPoint value =
  sendMessage gkAccessPoint setShowHighlightsSelector value

-- | These properties control the placement of the widget
--
-- ObjC selector: @- location@
location :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO GKAccessPointLocation
location gkAccessPoint =
  sendMessage gkAccessPoint locationSelector

-- | These properties control the placement of the widget
--
-- ObjC selector: @- setLocation:@
setLocation :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> GKAccessPointLocation -> IO ()
setLocation gkAccessPoint value =
  sendMessage gkAccessPoint setLocationSelector value

-- | observable property that contains the current frame needed to display the widget
--
-- ObjC selector: @- frameInScreenCoordinates@
frameInScreenCoordinates :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO NSRect
frameInScreenCoordinates gkAccessPoint =
  sendMessage gkAccessPoint frameInScreenCoordinatesSelector

-- | the following is a platform specific window that you wish to have the access point in.  If not set then a best attempt will be made to choose the main window of the app.
--
-- ObjC selector: @- parentWindow@
parentWindow :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO (Id NSWindow)
parentWindow gkAccessPoint =
  sendMessage gkAccessPoint parentWindowSelector

-- | the following is a platform specific window that you wish to have the access point in.  If not set then a best attempt will be made to choose the main window of the app.
--
-- ObjC selector: @- setParentWindow:@
setParentWindow :: (IsGKAccessPoint gkAccessPoint, IsNSWindow value) => gkAccessPoint -> value -> IO ()
setParentWindow gkAccessPoint value =
  sendMessage gkAccessPoint setParentWindowSelector (toNSWindow value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @triggerAccessPointWithHandler:@
triggerAccessPointWithHandlerSelector :: Selector '[Ptr ()] ()
triggerAccessPointWithHandlerSelector = mkSelector "triggerAccessPointWithHandler:"

-- | @Selector@ for @triggerAccessPointWithState:handler:@
triggerAccessPointWithState_handlerSelector :: Selector '[GKGameCenterViewControllerState, Ptr ()] ()
triggerAccessPointWithState_handlerSelector = mkSelector "triggerAccessPointWithState:handler:"

-- | @Selector@ for @triggerAccessPointWithAchievementID:handler:@
triggerAccessPointWithAchievementID_handlerSelector :: Selector '[Id NSString, Ptr ()] ()
triggerAccessPointWithAchievementID_handlerSelector = mkSelector "triggerAccessPointWithAchievementID:handler:"

-- | @Selector@ for @triggerAccessPointWithLeaderboardSetID:handler:@
triggerAccessPointWithLeaderboardSetID_handlerSelector :: Selector '[Id NSString, Ptr ()] ()
triggerAccessPointWithLeaderboardSetID_handlerSelector = mkSelector "triggerAccessPointWithLeaderboardSetID:handler:"

-- | @Selector@ for @triggerAccessPointWithLeaderboardID:playerScope:timeScope:handler:@
triggerAccessPointWithLeaderboardID_playerScope_timeScope_handlerSelector :: Selector '[Id NSString, GKLeaderboardPlayerScope, GKLeaderboardTimeScope, Ptr ()] ()
triggerAccessPointWithLeaderboardID_playerScope_timeScope_handlerSelector = mkSelector "triggerAccessPointWithLeaderboardID:playerScope:timeScope:handler:"

-- | @Selector@ for @triggerAccessPointWithPlayer:handler:@
triggerAccessPointWithPlayer_handlerSelector :: Selector '[Id GKPlayer, Ptr ()] ()
triggerAccessPointWithPlayer_handlerSelector = mkSelector "triggerAccessPointWithPlayer:handler:"

-- | @Selector@ for @triggerAccessPointForPlayTogetherWithHandler:@
triggerAccessPointForPlayTogetherWithHandlerSelector :: Selector '[Ptr ()] ()
triggerAccessPointForPlayTogetherWithHandlerSelector = mkSelector "triggerAccessPointForPlayTogetherWithHandler:"

-- | @Selector@ for @triggerAccessPointForChallengesWithHandler:@
triggerAccessPointForChallengesWithHandlerSelector :: Selector '[Ptr ()] ()
triggerAccessPointForChallengesWithHandlerSelector = mkSelector "triggerAccessPointForChallengesWithHandler:"

-- | @Selector@ for @triggerAccessPointWithChallengeDefinitionID:handler:@
triggerAccessPointWithChallengeDefinitionID_handlerSelector :: Selector '[Id NSString, Ptr ()] ()
triggerAccessPointWithChallengeDefinitionID_handlerSelector = mkSelector "triggerAccessPointWithChallengeDefinitionID:handler:"

-- | @Selector@ for @triggerAccessPointWithGameActivityDefinitionID:handler:@
triggerAccessPointWithGameActivityDefinitionID_handlerSelector :: Selector '[Id NSString, Ptr ()] ()
triggerAccessPointWithGameActivityDefinitionID_handlerSelector = mkSelector "triggerAccessPointWithGameActivityDefinitionID:handler:"

-- | @Selector@ for @triggerAccessPointWithGameActivity:handler:@
triggerAccessPointWithGameActivity_handlerSelector :: Selector '[Id GKGameActivity, Ptr ()] ()
triggerAccessPointWithGameActivity_handlerSelector = mkSelector "triggerAccessPointWithGameActivity:handler:"

-- | @Selector@ for @triggerAccessPointForFriendingWithHandler:@
triggerAccessPointForFriendingWithHandlerSelector :: Selector '[Ptr ()] ()
triggerAccessPointForFriendingWithHandlerSelector = mkSelector "triggerAccessPointForFriendingWithHandler:"

-- | @Selector@ for @triggerAccessPointForArcadeWithHandler:@
triggerAccessPointForArcadeWithHandlerSelector :: Selector '[Ptr ()] ()
triggerAccessPointForArcadeWithHandlerSelector = mkSelector "triggerAccessPointForArcadeWithHandler:"

-- | @Selector@ for @shared@
sharedSelector :: Selector '[] (Id GKAccessPoint)
sharedSelector = mkSelector "shared"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector '[Bool] ()
setActiveSelector = mkSelector "setActive:"

-- | @Selector@ for @focused@
focusedSelector :: Selector '[] Bool
focusedSelector = mkSelector "focused"

-- | @Selector@ for @setFocused:@
setFocusedSelector :: Selector '[Bool] ()
setFocusedSelector = mkSelector "setFocused:"

-- | @Selector@ for @visible@
visibleSelector :: Selector '[] Bool
visibleSelector = mkSelector "visible"

-- | @Selector@ for @isPresentingGameCenter@
isPresentingGameCenterSelector :: Selector '[] Bool
isPresentingGameCenterSelector = mkSelector "isPresentingGameCenter"

-- | @Selector@ for @showHighlights@
showHighlightsSelector :: Selector '[] Bool
showHighlightsSelector = mkSelector "showHighlights"

-- | @Selector@ for @setShowHighlights:@
setShowHighlightsSelector :: Selector '[Bool] ()
setShowHighlightsSelector = mkSelector "setShowHighlights:"

-- | @Selector@ for @location@
locationSelector :: Selector '[] GKAccessPointLocation
locationSelector = mkSelector "location"

-- | @Selector@ for @setLocation:@
setLocationSelector :: Selector '[GKAccessPointLocation] ()
setLocationSelector = mkSelector "setLocation:"

-- | @Selector@ for @frameInScreenCoordinates@
frameInScreenCoordinatesSelector :: Selector '[] NSRect
frameInScreenCoordinatesSelector = mkSelector "frameInScreenCoordinates"

-- | @Selector@ for @parentWindow@
parentWindowSelector :: Selector '[] (Id NSWindow)
parentWindowSelector = mkSelector "parentWindow"

-- | @Selector@ for @setParentWindow:@
setParentWindowSelector :: Selector '[Id NSWindow] ()
setParentWindowSelector = mkSelector "setParentWindow:"

