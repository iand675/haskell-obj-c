{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKAchievement@.
module ObjC.GameKit.GKAchievement
  ( GKAchievement
  , IsGKAchievement(..)
  , resetAchievementsWithCompletionHandler
  , initWithIdentifier
  , initWithIdentifier_player
  , reportAchievements_withCompletionHandler
  , challengeComposeControllerWithPlayers_message_completionHandler
  , challengeComposeControllerWithMessage_players_completionHandler
  , challengeComposeControllerWithMessage_players_completion
  , issueChallengeToPlayers_message
  , reportAchievements_withEligibleChallenges_withCompletionHandler
  , initWithIdentifier_forPlayer
  , reportAchievementWithCompletionHandler
  , identifier
  , setIdentifier
  , percentComplete
  , setPercentComplete
  , completed
  , lastReportedDate
  , showsCompletionBanner
  , setShowsCompletionBanner
  , player
  , playerID
  , hidden
  , challengeComposeControllerWithMessage_players_completionHandlerSelector
  , challengeComposeControllerWithMessage_players_completionSelector
  , challengeComposeControllerWithPlayers_message_completionHandlerSelector
  , completedSelector
  , hiddenSelector
  , identifierSelector
  , initWithIdentifierSelector
  , initWithIdentifier_forPlayerSelector
  , initWithIdentifier_playerSelector
  , issueChallengeToPlayers_messageSelector
  , lastReportedDateSelector
  , percentCompleteSelector
  , playerIDSelector
  , playerSelector
  , reportAchievementWithCompletionHandlerSelector
  , reportAchievements_withCompletionHandlerSelector
  , reportAchievements_withEligibleChallenges_withCompletionHandlerSelector
  , resetAchievementsWithCompletionHandlerSelector
  , setIdentifierSelector
  , setPercentCompleteSelector
  , setShowsCompletionBannerSelector
  , showsCompletionBannerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Reset the achievements progress for the local player. All the entries for the local player are removed from the server. Error will be nil on success.Possible reasons for error: 1. Local player not authenticated 2. Communications failure
--
-- ObjC selector: @+ resetAchievementsWithCompletionHandler:@
resetAchievementsWithCompletionHandler :: Ptr () -> IO ()
resetAchievementsWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "GKAchievement"
    sendClassMessage cls' resetAchievementsWithCompletionHandlerSelector completionHandler

-- | Designated initializer
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsGKAchievement gkAchievement, IsNSString identifier) => gkAchievement -> identifier -> IO (Id GKAchievement)
initWithIdentifier gkAchievement identifier =
  sendOwnedMessage gkAchievement initWithIdentifierSelector (toNSString identifier)

-- | Initialize the achievement for a specific player. Use to submit participant achievements when ending a turn-based match.
--
-- ObjC selector: @- initWithIdentifier:player:@
initWithIdentifier_player :: (IsGKAchievement gkAchievement, IsNSString identifier, IsGKPlayer player) => gkAchievement -> identifier -> player -> IO (Id GKAchievement)
initWithIdentifier_player gkAchievement identifier player =
  sendOwnedMessage gkAchievement initWithIdentifier_playerSelector (toNSString identifier) (toGKPlayer player)

-- | Report an array of achievements to the server. Percent complete is required. Points, completed state are set based on percentComplete. isHidden is set to NO anytime this method is invoked. Date is optional. Error will be nil on success. Possible reasons for error: 1. Local player not authenticated 2. Communications failure 3. Reported Achievement does not exist
--
-- ObjC selector: @+ reportAchievements:withCompletionHandler:@
reportAchievements_withCompletionHandler :: IsNSArray achievements => achievements -> Ptr () -> IO ()
reportAchievements_withCompletionHandler achievements completionHandler =
  do
    cls' <- getRequiredClass "GKAchievement"
    sendClassMessage cls' reportAchievements_withCompletionHandlerSelector (toNSArray achievements) completionHandler

-- | * This method is obsolete. Calling this method does nothing and will return nil **
--
-- ObjC selector: @- challengeComposeControllerWithPlayers:message:completionHandler:@
challengeComposeControllerWithPlayers_message_completionHandler :: (IsGKAchievement gkAchievement, IsNSArray playerIDs, IsNSString message) => gkAchievement -> playerIDs -> message -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithPlayers_message_completionHandler gkAchievement playerIDs message completionHandler =
  sendMessage gkAchievement challengeComposeControllerWithPlayers_message_completionHandlerSelector (toNSArray playerIDs) (toNSString message) completionHandler

-- | @- challengeComposeControllerWithMessage:players:completionHandler:@
challengeComposeControllerWithMessage_players_completionHandler :: (IsGKAchievement gkAchievement, IsNSString message, IsNSArray players) => gkAchievement -> message -> players -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithMessage_players_completionHandler gkAchievement message players completionHandler =
  sendMessage gkAchievement challengeComposeControllerWithMessage_players_completionHandlerSelector (toNSString message) (toNSArray players) completionHandler

-- | @- challengeComposeControllerWithMessage:players:completion:@
challengeComposeControllerWithMessage_players_completion :: (IsGKAchievement gkAchievement, IsNSString message, IsNSArray players) => gkAchievement -> message -> players -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithMessage_players_completion gkAchievement message players completionHandler =
  sendMessage gkAchievement challengeComposeControllerWithMessage_players_completionSelector (toNSString message) (toNSArray players) completionHandler

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- issueChallengeToPlayers:message:@
issueChallengeToPlayers_message :: (IsGKAchievement gkAchievement, IsNSArray playerIDs, IsNSString message) => gkAchievement -> playerIDs -> message -> IO ()
issueChallengeToPlayers_message gkAchievement playerIDs message =
  sendMessage gkAchievement issueChallengeToPlayers_messageSelector (toNSArray playerIDs) (toNSString message)

-- | Use this alternative to reportAchievements:withCompletionHandler: to allow only certain specific challenges to be completed. Pass nil to avoid completing any challenges.
--
-- ObjC selector: @+ reportAchievements:withEligibleChallenges:withCompletionHandler:@
reportAchievements_withEligibleChallenges_withCompletionHandler :: (IsNSArray achievements, IsNSArray challenges) => achievements -> challenges -> Ptr () -> IO ()
reportAchievements_withEligibleChallenges_withCompletionHandler achievements challenges completionHandler =
  do
    cls' <- getRequiredClass "GKAchievement"
    sendClassMessage cls' reportAchievements_withEligibleChallenges_withCompletionHandlerSelector (toNSArray achievements) (toNSArray challenges) completionHandler

-- | * This method is obsolete. Calling this initializer does nothing and will return nil **
--
-- ObjC selector: @- initWithIdentifier:forPlayer:@
initWithIdentifier_forPlayer :: (IsGKAchievement gkAchievement, IsNSString identifier, IsNSString playerID) => gkAchievement -> identifier -> playerID -> IO (Id GKAchievement)
initWithIdentifier_forPlayer gkAchievement identifier playerID =
  sendOwnedMessage gkAchievement initWithIdentifier_forPlayerSelector (toNSString identifier) (toNSString playerID)

-- | @- reportAchievementWithCompletionHandler:@
reportAchievementWithCompletionHandler :: IsGKAchievement gkAchievement => gkAchievement -> Ptr () -> IO ()
reportAchievementWithCompletionHandler gkAchievement completionHandler =
  sendMessage gkAchievement reportAchievementWithCompletionHandlerSelector completionHandler

-- | Achievement identifier
--
-- ObjC selector: @- identifier@
identifier :: IsGKAchievement gkAchievement => gkAchievement -> IO (Id NSString)
identifier gkAchievement =
  sendMessage gkAchievement identifierSelector

-- | Achievement identifier
--
-- ObjC selector: @- setIdentifier:@
setIdentifier :: (IsGKAchievement gkAchievement, IsNSString value) => gkAchievement -> value -> IO ()
setIdentifier gkAchievement value =
  sendMessage gkAchievement setIdentifierSelector (toNSString value)

-- | Required, Percentage of achievement complete.
--
-- ObjC selector: @- percentComplete@
percentComplete :: IsGKAchievement gkAchievement => gkAchievement -> IO CDouble
percentComplete gkAchievement =
  sendMessage gkAchievement percentCompleteSelector

-- | Required, Percentage of achievement complete.
--
-- ObjC selector: @- setPercentComplete:@
setPercentComplete :: IsGKAchievement gkAchievement => gkAchievement -> CDouble -> IO ()
setPercentComplete gkAchievement value =
  sendMessage gkAchievement setPercentCompleteSelector value

-- | Set to NO until percentComplete = 100.
--
-- ObjC selector: @- completed@
completed :: IsGKAchievement gkAchievement => gkAchievement -> IO Bool
completed gkAchievement =
  sendMessage gkAchievement completedSelector

-- | Date the achievement was last reported. Read-only. Created at initialization
--
-- ObjC selector: @- lastReportedDate@
lastReportedDate :: IsGKAchievement gkAchievement => gkAchievement -> IO (Id NSDate)
lastReportedDate gkAchievement =
  sendMessage gkAchievement lastReportedDateSelector

-- | A banner will be momentarily displayed after reporting a completed achievement
--
-- ObjC selector: @- showsCompletionBanner@
showsCompletionBanner :: IsGKAchievement gkAchievement => gkAchievement -> IO Bool
showsCompletionBanner gkAchievement =
  sendMessage gkAchievement showsCompletionBannerSelector

-- | A banner will be momentarily displayed after reporting a completed achievement
--
-- ObjC selector: @- setShowsCompletionBanner:@
setShowsCompletionBanner :: IsGKAchievement gkAchievement => gkAchievement -> Bool -> IO ()
setShowsCompletionBanner gkAchievement value =
  sendMessage gkAchievement setShowsCompletionBannerSelector value

-- | The identifier of the player that earned the achievement.
--
-- ObjC selector: @- player@
player :: IsGKAchievement gkAchievement => gkAchievement -> IO (Id GKPlayer)
player gkAchievement =
  sendMessage gkAchievement playerSelector

-- | * This property is obsolete. **
--
-- ObjC selector: @- playerID@
playerID :: IsGKAchievement gkAchievement => gkAchievement -> IO (Id NSString)
playerID gkAchievement =
  sendMessage gkAchievement playerIDSelector

-- | @- hidden@
hidden :: IsGKAchievement gkAchievement => gkAchievement -> IO Bool
hidden gkAchievement =
  sendMessage gkAchievement hiddenSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetAchievementsWithCompletionHandler:@
resetAchievementsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
resetAchievementsWithCompletionHandlerSelector = mkSelector "resetAchievementsWithCompletionHandler:"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[Id NSString] (Id GKAchievement)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @initWithIdentifier:player:@
initWithIdentifier_playerSelector :: Selector '[Id NSString, Id GKPlayer] (Id GKAchievement)
initWithIdentifier_playerSelector = mkSelector "initWithIdentifier:player:"

-- | @Selector@ for @reportAchievements:withCompletionHandler:@
reportAchievements_withCompletionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
reportAchievements_withCompletionHandlerSelector = mkSelector "reportAchievements:withCompletionHandler:"

-- | @Selector@ for @challengeComposeControllerWithPlayers:message:completionHandler:@
challengeComposeControllerWithPlayers_message_completionHandlerSelector :: Selector '[Id NSArray, Id NSString, Ptr ()] (Id NSViewController)
challengeComposeControllerWithPlayers_message_completionHandlerSelector = mkSelector "challengeComposeControllerWithPlayers:message:completionHandler:"

-- | @Selector@ for @challengeComposeControllerWithMessage:players:completionHandler:@
challengeComposeControllerWithMessage_players_completionHandlerSelector :: Selector '[Id NSString, Id NSArray, Ptr ()] (Id NSViewController)
challengeComposeControllerWithMessage_players_completionHandlerSelector = mkSelector "challengeComposeControllerWithMessage:players:completionHandler:"

-- | @Selector@ for @challengeComposeControllerWithMessage:players:completion:@
challengeComposeControllerWithMessage_players_completionSelector :: Selector '[Id NSString, Id NSArray, Ptr ()] (Id NSViewController)
challengeComposeControllerWithMessage_players_completionSelector = mkSelector "challengeComposeControllerWithMessage:players:completion:"

-- | @Selector@ for @issueChallengeToPlayers:message:@
issueChallengeToPlayers_messageSelector :: Selector '[Id NSArray, Id NSString] ()
issueChallengeToPlayers_messageSelector = mkSelector "issueChallengeToPlayers:message:"

-- | @Selector@ for @reportAchievements:withEligibleChallenges:withCompletionHandler:@
reportAchievements_withEligibleChallenges_withCompletionHandlerSelector :: Selector '[Id NSArray, Id NSArray, Ptr ()] ()
reportAchievements_withEligibleChallenges_withCompletionHandlerSelector = mkSelector "reportAchievements:withEligibleChallenges:withCompletionHandler:"

-- | @Selector@ for @initWithIdentifier:forPlayer:@
initWithIdentifier_forPlayerSelector :: Selector '[Id NSString, Id NSString] (Id GKAchievement)
initWithIdentifier_forPlayerSelector = mkSelector "initWithIdentifier:forPlayer:"

-- | @Selector@ for @reportAchievementWithCompletionHandler:@
reportAchievementWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
reportAchievementWithCompletionHandlerSelector = mkSelector "reportAchievementWithCompletionHandler:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @percentComplete@
percentCompleteSelector :: Selector '[] CDouble
percentCompleteSelector = mkSelector "percentComplete"

-- | @Selector@ for @setPercentComplete:@
setPercentCompleteSelector :: Selector '[CDouble] ()
setPercentCompleteSelector = mkSelector "setPercentComplete:"

-- | @Selector@ for @completed@
completedSelector :: Selector '[] Bool
completedSelector = mkSelector "completed"

-- | @Selector@ for @lastReportedDate@
lastReportedDateSelector :: Selector '[] (Id NSDate)
lastReportedDateSelector = mkSelector "lastReportedDate"

-- | @Selector@ for @showsCompletionBanner@
showsCompletionBannerSelector :: Selector '[] Bool
showsCompletionBannerSelector = mkSelector "showsCompletionBanner"

-- | @Selector@ for @setShowsCompletionBanner:@
setShowsCompletionBannerSelector :: Selector '[Bool] ()
setShowsCompletionBannerSelector = mkSelector "setShowsCompletionBanner:"

-- | @Selector@ for @player@
playerSelector :: Selector '[] (Id GKPlayer)
playerSelector = mkSelector "player"

-- | @Selector@ for @playerID@
playerIDSelector :: Selector '[] (Id NSString)
playerIDSelector = mkSelector "playerID"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

