{-# LANGUAGE PatternSynonyms #-}
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
  , triggerAccessPointWithHandlerSelector
  , triggerAccessPointWithState_handlerSelector
  , triggerAccessPointWithAchievementID_handlerSelector
  , triggerAccessPointWithLeaderboardSetID_handlerSelector
  , triggerAccessPointWithLeaderboardID_playerScope_timeScope_handlerSelector
  , triggerAccessPointWithPlayer_handlerSelector
  , triggerAccessPointForPlayTogetherWithHandlerSelector
  , triggerAccessPointForChallengesWithHandlerSelector
  , triggerAccessPointWithChallengeDefinitionID_handlerSelector
  , triggerAccessPointWithGameActivityDefinitionID_handlerSelector
  , triggerAccessPointWithGameActivity_handlerSelector
  , triggerAccessPointForFriendingWithHandlerSelector
  , triggerAccessPointForArcadeWithHandlerSelector
  , sharedSelector
  , activeSelector
  , setActiveSelector
  , focusedSelector
  , setFocusedSelector
  , visibleSelector
  , isPresentingGameCenterSelector
  , showHighlightsSelector
  , setShowHighlightsSelector
  , locationSelector
  , setLocationSelector
  , frameInScreenCoordinatesSelector
  , parentWindowSelector
  , setParentWindowSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
triggerAccessPointWithHandler gkAccessPoint  handler =
  sendMsg gkAccessPoint (mkSelector "triggerAccessPointWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @- triggerAccessPointWithState:handler:@
triggerAccessPointWithState_handler :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> GKGameCenterViewControllerState -> Ptr () -> IO ()
triggerAccessPointWithState_handler gkAccessPoint  state handler =
  sendMsg gkAccessPoint (mkSelector "triggerAccessPointWithState:handler:") retVoid [argCLong (coerce state), argPtr (castPtr handler :: Ptr ())]

-- | @- triggerAccessPointWithAchievementID:handler:@
triggerAccessPointWithAchievementID_handler :: (IsGKAccessPoint gkAccessPoint, IsNSString achievementID) => gkAccessPoint -> achievementID -> Ptr () -> IO ()
triggerAccessPointWithAchievementID_handler gkAccessPoint  achievementID handler =
withObjCPtr achievementID $ \raw_achievementID ->
    sendMsg gkAccessPoint (mkSelector "triggerAccessPointWithAchievementID:handler:") retVoid [argPtr (castPtr raw_achievementID :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- triggerAccessPointWithLeaderboardSetID:handler:@
triggerAccessPointWithLeaderboardSetID_handler :: (IsGKAccessPoint gkAccessPoint, IsNSString leaderboardSetID) => gkAccessPoint -> leaderboardSetID -> Ptr () -> IO ()
triggerAccessPointWithLeaderboardSetID_handler gkAccessPoint  leaderboardSetID handler =
withObjCPtr leaderboardSetID $ \raw_leaderboardSetID ->
    sendMsg gkAccessPoint (mkSelector "triggerAccessPointWithLeaderboardSetID:handler:") retVoid [argPtr (castPtr raw_leaderboardSetID :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- triggerAccessPointWithLeaderboardID:playerScope:timeScope:handler:@
triggerAccessPointWithLeaderboardID_playerScope_timeScope_handler :: (IsGKAccessPoint gkAccessPoint, IsNSString leaderboardID) => gkAccessPoint -> leaderboardID -> GKLeaderboardPlayerScope -> GKLeaderboardTimeScope -> Ptr () -> IO ()
triggerAccessPointWithLeaderboardID_playerScope_timeScope_handler gkAccessPoint  leaderboardID playerScope timeScope handler =
withObjCPtr leaderboardID $ \raw_leaderboardID ->
    sendMsg gkAccessPoint (mkSelector "triggerAccessPointWithLeaderboardID:playerScope:timeScope:handler:") retVoid [argPtr (castPtr raw_leaderboardID :: Ptr ()), argCLong (coerce playerScope), argCLong (coerce timeScope), argPtr (castPtr handler :: Ptr ())]

-- | @- triggerAccessPointWithPlayer:handler:@
triggerAccessPointWithPlayer_handler :: (IsGKAccessPoint gkAccessPoint, IsGKPlayer player) => gkAccessPoint -> player -> Ptr () -> IO ()
triggerAccessPointWithPlayer_handler gkAccessPoint  player handler =
withObjCPtr player $ \raw_player ->
    sendMsg gkAccessPoint (mkSelector "triggerAccessPointWithPlayer:handler:") retVoid [argPtr (castPtr raw_player :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Displays the view that allows players to engage each other with activities and challenges.
--
-- ObjC selector: @- triggerAccessPointForPlayTogetherWithHandler:@
triggerAccessPointForPlayTogetherWithHandler :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Ptr () -> IO ()
triggerAccessPointForPlayTogetherWithHandler gkAccessPoint  handler =
  sendMsg gkAccessPoint (mkSelector "triggerAccessPointForPlayTogetherWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | Displays the view that allows players to engage each other with challenges.
--
-- ObjC selector: @- triggerAccessPointForChallengesWithHandler:@
triggerAccessPointForChallengesWithHandler :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Ptr () -> IO ()
triggerAccessPointForChallengesWithHandler gkAccessPoint  handler =
  sendMsg gkAccessPoint (mkSelector "triggerAccessPointForChallengesWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | Displays the challenge creation view for the provided challenge definition ID.
--
-- ObjC selector: @- triggerAccessPointWithChallengeDefinitionID:handler:@
triggerAccessPointWithChallengeDefinitionID_handler :: (IsGKAccessPoint gkAccessPoint, IsNSString challengeDefinitionID) => gkAccessPoint -> challengeDefinitionID -> Ptr () -> IO ()
triggerAccessPointWithChallengeDefinitionID_handler gkAccessPoint  challengeDefinitionID handler =
withObjCPtr challengeDefinitionID $ \raw_challengeDefinitionID ->
    sendMsg gkAccessPoint (mkSelector "triggerAccessPointWithChallengeDefinitionID:handler:") retVoid [argPtr (castPtr raw_challengeDefinitionID :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Displays the game activity creation view for the provided activity definition ID.
--
-- ObjC selector: @- triggerAccessPointWithGameActivityDefinitionID:handler:@
triggerAccessPointWithGameActivityDefinitionID_handler :: (IsGKAccessPoint gkAccessPoint, IsNSString gameActivityDefinitionID) => gkAccessPoint -> gameActivityDefinitionID -> Ptr () -> IO ()
triggerAccessPointWithGameActivityDefinitionID_handler gkAccessPoint  gameActivityDefinitionID handler =
withObjCPtr gameActivityDefinitionID $ \raw_gameActivityDefinitionID ->
    sendMsg gkAccessPoint (mkSelector "triggerAccessPointWithGameActivityDefinitionID:handler:") retVoid [argPtr (castPtr raw_gameActivityDefinitionID :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Displays the game activity view for the provided activity instance.
--
-- ObjC selector: @- triggerAccessPointWithGameActivity:handler:@
triggerAccessPointWithGameActivity_handler :: (IsGKAccessPoint gkAccessPoint, IsGKGameActivity gameActivity) => gkAccessPoint -> gameActivity -> Ptr () -> IO ()
triggerAccessPointWithGameActivity_handler gkAccessPoint  gameActivity handler =
withObjCPtr gameActivity $ \raw_gameActivity ->
    sendMsg gkAccessPoint (mkSelector "triggerAccessPointWithGameActivity:handler:") retVoid [argPtr (castPtr raw_gameActivity :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Brings up the invite friends view.
--
-- ObjC selector: @- triggerAccessPointForFriendingWithHandler:@
triggerAccessPointForFriendingWithHandler :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Ptr () -> IO ()
triggerAccessPointForFriendingWithHandler gkAccessPoint  handler =
  sendMsg gkAccessPoint (mkSelector "triggerAccessPointForFriendingWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | Brings up the Arcade dashboard.
--
-- ObjC selector: @- triggerAccessPointForArcadeWithHandler:@
triggerAccessPointForArcadeWithHandler :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Ptr () -> IO ()
triggerAccessPointForArcadeWithHandler gkAccessPoint  handler =
  sendMsg gkAccessPoint (mkSelector "triggerAccessPointForArcadeWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @+ shared@
shared :: IO (Id GKAccessPoint)
shared  =
  do
    cls' <- getRequiredClass "GKAccessPoint"
    sendClassMsg cls' (mkSelector "shared") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | set this true to enable access point in your app.  Setting this will cause the access point to appear after the notification banner is presented.  If it already was presented it will appear immediately
--
-- ObjC selector: @- active@
active :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO Bool
active gkAccessPoint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkAccessPoint (mkSelector "active") retCULong []

-- | set this true to enable access point in your app.  Setting this will cause the access point to appear after the notification banner is presented.  If it already was presented it will appear immediately
--
-- ObjC selector: @- setActive:@
setActive :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Bool -> IO ()
setActive gkAccessPoint  value =
  sendMsg gkAccessPoint (mkSelector "setActive:") retVoid [argCULong (if value then 1 else 0)]

-- | set this on tvOS to put the accessPoint into focused mode
--
-- ObjC selector: @- focused@
focused :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO Bool
focused gkAccessPoint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkAccessPoint (mkSelector "focused") retCULong []

-- | set this on tvOS to put the accessPoint into focused mode
--
-- ObjC selector: @- setFocused:@
setFocused :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Bool -> IO ()
setFocused gkAccessPoint  value =
  sendMsg gkAccessPoint (mkSelector "setFocused:") retVoid [argCULong (if value then 1 else 0)]

-- | @- visible@
visible :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO Bool
visible gkAccessPoint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkAccessPoint (mkSelector "visible") retCULong []

-- | observable property that indicates when the access point is visible.
--
-- ObjC selector: @- isPresentingGameCenter@
isPresentingGameCenter :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO Bool
isPresentingGameCenter gkAccessPoint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkAccessPoint (mkSelector "isPresentingGameCenter") retCULong []

-- | Set this property to true if you wish to show the highlights for most recent achievement, current rank on default leaderboard, etc
--
-- ObjC selector: @- showHighlights@
showHighlights :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO Bool
showHighlights gkAccessPoint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkAccessPoint (mkSelector "showHighlights") retCULong []

-- | Set this property to true if you wish to show the highlights for most recent achievement, current rank on default leaderboard, etc
--
-- ObjC selector: @- setShowHighlights:@
setShowHighlights :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> Bool -> IO ()
setShowHighlights gkAccessPoint  value =
  sendMsg gkAccessPoint (mkSelector "setShowHighlights:") retVoid [argCULong (if value then 1 else 0)]

-- | These properties control the placement of the widget
--
-- ObjC selector: @- location@
location :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO GKAccessPointLocation
location gkAccessPoint  =
  fmap (coerce :: CLong -> GKAccessPointLocation) $ sendMsg gkAccessPoint (mkSelector "location") retCLong []

-- | These properties control the placement of the widget
--
-- ObjC selector: @- setLocation:@
setLocation :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> GKAccessPointLocation -> IO ()
setLocation gkAccessPoint  value =
  sendMsg gkAccessPoint (mkSelector "setLocation:") retVoid [argCLong (coerce value)]

-- | observable property that contains the current frame needed to display the widget
--
-- ObjC selector: @- frameInScreenCoordinates@
frameInScreenCoordinates :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO NSRect
frameInScreenCoordinates gkAccessPoint  =
  sendMsgStret gkAccessPoint (mkSelector "frameInScreenCoordinates") retNSRect []

-- | the following is a platform specific window that you wish to have the access point in.  If not set then a best attempt will be made to choose the main window of the app.
--
-- ObjC selector: @- parentWindow@
parentWindow :: IsGKAccessPoint gkAccessPoint => gkAccessPoint -> IO (Id NSWindow)
parentWindow gkAccessPoint  =
  sendMsg gkAccessPoint (mkSelector "parentWindow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | the following is a platform specific window that you wish to have the access point in.  If not set then a best attempt will be made to choose the main window of the app.
--
-- ObjC selector: @- setParentWindow:@
setParentWindow :: (IsGKAccessPoint gkAccessPoint, IsNSWindow value) => gkAccessPoint -> value -> IO ()
setParentWindow gkAccessPoint  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkAccessPoint (mkSelector "setParentWindow:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @triggerAccessPointWithHandler:@
triggerAccessPointWithHandlerSelector :: Selector
triggerAccessPointWithHandlerSelector = mkSelector "triggerAccessPointWithHandler:"

-- | @Selector@ for @triggerAccessPointWithState:handler:@
triggerAccessPointWithState_handlerSelector :: Selector
triggerAccessPointWithState_handlerSelector = mkSelector "triggerAccessPointWithState:handler:"

-- | @Selector@ for @triggerAccessPointWithAchievementID:handler:@
triggerAccessPointWithAchievementID_handlerSelector :: Selector
triggerAccessPointWithAchievementID_handlerSelector = mkSelector "triggerAccessPointWithAchievementID:handler:"

-- | @Selector@ for @triggerAccessPointWithLeaderboardSetID:handler:@
triggerAccessPointWithLeaderboardSetID_handlerSelector :: Selector
triggerAccessPointWithLeaderboardSetID_handlerSelector = mkSelector "triggerAccessPointWithLeaderboardSetID:handler:"

-- | @Selector@ for @triggerAccessPointWithLeaderboardID:playerScope:timeScope:handler:@
triggerAccessPointWithLeaderboardID_playerScope_timeScope_handlerSelector :: Selector
triggerAccessPointWithLeaderboardID_playerScope_timeScope_handlerSelector = mkSelector "triggerAccessPointWithLeaderboardID:playerScope:timeScope:handler:"

-- | @Selector@ for @triggerAccessPointWithPlayer:handler:@
triggerAccessPointWithPlayer_handlerSelector :: Selector
triggerAccessPointWithPlayer_handlerSelector = mkSelector "triggerAccessPointWithPlayer:handler:"

-- | @Selector@ for @triggerAccessPointForPlayTogetherWithHandler:@
triggerAccessPointForPlayTogetherWithHandlerSelector :: Selector
triggerAccessPointForPlayTogetherWithHandlerSelector = mkSelector "triggerAccessPointForPlayTogetherWithHandler:"

-- | @Selector@ for @triggerAccessPointForChallengesWithHandler:@
triggerAccessPointForChallengesWithHandlerSelector :: Selector
triggerAccessPointForChallengesWithHandlerSelector = mkSelector "triggerAccessPointForChallengesWithHandler:"

-- | @Selector@ for @triggerAccessPointWithChallengeDefinitionID:handler:@
triggerAccessPointWithChallengeDefinitionID_handlerSelector :: Selector
triggerAccessPointWithChallengeDefinitionID_handlerSelector = mkSelector "triggerAccessPointWithChallengeDefinitionID:handler:"

-- | @Selector@ for @triggerAccessPointWithGameActivityDefinitionID:handler:@
triggerAccessPointWithGameActivityDefinitionID_handlerSelector :: Selector
triggerAccessPointWithGameActivityDefinitionID_handlerSelector = mkSelector "triggerAccessPointWithGameActivityDefinitionID:handler:"

-- | @Selector@ for @triggerAccessPointWithGameActivity:handler:@
triggerAccessPointWithGameActivity_handlerSelector :: Selector
triggerAccessPointWithGameActivity_handlerSelector = mkSelector "triggerAccessPointWithGameActivity:handler:"

-- | @Selector@ for @triggerAccessPointForFriendingWithHandler:@
triggerAccessPointForFriendingWithHandlerSelector :: Selector
triggerAccessPointForFriendingWithHandlerSelector = mkSelector "triggerAccessPointForFriendingWithHandler:"

-- | @Selector@ for @triggerAccessPointForArcadeWithHandler:@
triggerAccessPointForArcadeWithHandlerSelector :: Selector
triggerAccessPointForArcadeWithHandlerSelector = mkSelector "triggerAccessPointForArcadeWithHandler:"

-- | @Selector@ for @shared@
sharedSelector :: Selector
sharedSelector = mkSelector "shared"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector
setActiveSelector = mkSelector "setActive:"

-- | @Selector@ for @focused@
focusedSelector :: Selector
focusedSelector = mkSelector "focused"

-- | @Selector@ for @setFocused:@
setFocusedSelector :: Selector
setFocusedSelector = mkSelector "setFocused:"

-- | @Selector@ for @visible@
visibleSelector :: Selector
visibleSelector = mkSelector "visible"

-- | @Selector@ for @isPresentingGameCenter@
isPresentingGameCenterSelector :: Selector
isPresentingGameCenterSelector = mkSelector "isPresentingGameCenter"

-- | @Selector@ for @showHighlights@
showHighlightsSelector :: Selector
showHighlightsSelector = mkSelector "showHighlights"

-- | @Selector@ for @setShowHighlights:@
setShowHighlightsSelector :: Selector
setShowHighlightsSelector = mkSelector "setShowHighlights:"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @setLocation:@
setLocationSelector :: Selector
setLocationSelector = mkSelector "setLocation:"

-- | @Selector@ for @frameInScreenCoordinates@
frameInScreenCoordinatesSelector :: Selector
frameInScreenCoordinatesSelector = mkSelector "frameInScreenCoordinates"

-- | @Selector@ for @parentWindow@
parentWindowSelector :: Selector
parentWindowSelector = mkSelector "parentWindow"

-- | @Selector@ for @setParentWindow:@
setParentWindowSelector :: Selector
setParentWindowSelector = mkSelector "setParentWindow:"

