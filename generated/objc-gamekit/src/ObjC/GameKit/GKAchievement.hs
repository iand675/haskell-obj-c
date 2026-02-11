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
  , hidden
  , resetAchievementsWithCompletionHandlerSelector
  , initWithIdentifierSelector
  , initWithIdentifier_playerSelector
  , reportAchievements_withCompletionHandlerSelector
  , challengeComposeControllerWithPlayers_message_completionHandlerSelector
  , challengeComposeControllerWithMessage_players_completionHandlerSelector
  , challengeComposeControllerWithMessage_players_completionSelector
  , issueChallengeToPlayers_messageSelector
  , reportAchievements_withEligibleChallenges_withCompletionHandlerSelector
  , initWithIdentifier_forPlayerSelector
  , reportAchievementWithCompletionHandlerSelector
  , identifierSelector
  , setIdentifierSelector
  , percentCompleteSelector
  , setPercentCompleteSelector
  , completedSelector
  , lastReportedDateSelector
  , showsCompletionBannerSelector
  , setShowsCompletionBannerSelector
  , playerSelector
  , hiddenSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Reset the achievements progress for the local player. All the entries for the local player are removed from the server. Error will be nil on success.Possible reasons for error: 1. Local player not authenticated 2. Communications failure
--
-- ObjC selector: @+ resetAchievementsWithCompletionHandler:@
resetAchievementsWithCompletionHandler :: Ptr () -> IO ()
resetAchievementsWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "GKAchievement"
    sendClassMsg cls' (mkSelector "resetAchievementsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Designated initializer
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsGKAchievement gkAchievement, IsNSString identifier) => gkAchievement -> identifier -> IO (Id GKAchievement)
initWithIdentifier gkAchievement  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg gkAchievement (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize the achievement for a specific player. Use to submit participant achievements when ending a turn-based match.
--
-- ObjC selector: @- initWithIdentifier:player:@
initWithIdentifier_player :: (IsGKAchievement gkAchievement, IsNSString identifier, IsGKPlayer player) => gkAchievement -> identifier -> player -> IO (Id GKAchievement)
initWithIdentifier_player gkAchievement  identifier player =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr player $ \raw_player ->
      sendMsg gkAchievement (mkSelector "initWithIdentifier:player:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_player :: Ptr ())] >>= ownedObject . castPtr

-- | Report an array of achievements to the server. Percent complete is required. Points, completed state are set based on percentComplete. isHidden is set to NO anytime this method is invoked. Date is optional. Error will be nil on success. Possible reasons for error: 1. Local player not authenticated 2. Communications failure 3. Reported Achievement does not exist
--
-- ObjC selector: @+ reportAchievements:withCompletionHandler:@
reportAchievements_withCompletionHandler :: IsNSArray achievements => achievements -> Ptr () -> IO ()
reportAchievements_withCompletionHandler achievements completionHandler =
  do
    cls' <- getRequiredClass "GKAchievement"
    withObjCPtr achievements $ \raw_achievements ->
      sendClassMsg cls' (mkSelector "reportAchievements:withCompletionHandler:") retVoid [argPtr (castPtr raw_achievements :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | * This method is obsolete. Calling this method does nothing and will return nil **
--
-- ObjC selector: @- challengeComposeControllerWithPlayers:message:completionHandler:@
challengeComposeControllerWithPlayers_message_completionHandler :: (IsGKAchievement gkAchievement, IsNSArray playerIDs, IsNSString message) => gkAchievement -> playerIDs -> message -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithPlayers_message_completionHandler gkAchievement  playerIDs message completionHandler =
withObjCPtr playerIDs $ \raw_playerIDs ->
  withObjCPtr message $ \raw_message ->
      sendMsg gkAchievement (mkSelector "challengeComposeControllerWithPlayers:message:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_playerIDs :: Ptr ()), argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- challengeComposeControllerWithMessage:players:completionHandler:@
challengeComposeControllerWithMessage_players_completionHandler :: (IsGKAchievement gkAchievement, IsNSString message, IsNSArray players) => gkAchievement -> message -> players -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithMessage_players_completionHandler gkAchievement  message players completionHandler =
withObjCPtr message $ \raw_message ->
  withObjCPtr players $ \raw_players ->
      sendMsg gkAchievement (mkSelector "challengeComposeControllerWithMessage:players:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr raw_players :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- challengeComposeControllerWithMessage:players:completion:@
challengeComposeControllerWithMessage_players_completion :: (IsGKAchievement gkAchievement, IsNSString message, IsNSArray players) => gkAchievement -> message -> players -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithMessage_players_completion gkAchievement  message players completionHandler =
withObjCPtr message $ \raw_message ->
  withObjCPtr players $ \raw_players ->
      sendMsg gkAchievement (mkSelector "challengeComposeControllerWithMessage:players:completion:") (retPtr retVoid) [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr raw_players :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- issueChallengeToPlayers:message:@
issueChallengeToPlayers_message :: (IsGKAchievement gkAchievement, IsNSArray playerIDs, IsNSString message) => gkAchievement -> playerIDs -> message -> IO ()
issueChallengeToPlayers_message gkAchievement  playerIDs message =
withObjCPtr playerIDs $ \raw_playerIDs ->
  withObjCPtr message $ \raw_message ->
      sendMsg gkAchievement (mkSelector "issueChallengeToPlayers:message:") retVoid [argPtr (castPtr raw_playerIDs :: Ptr ()), argPtr (castPtr raw_message :: Ptr ())]

-- | Use this alternative to reportAchievements:withCompletionHandler: to allow only certain specific challenges to be completed. Pass nil to avoid completing any challenges.
--
-- ObjC selector: @+ reportAchievements:withEligibleChallenges:withCompletionHandler:@
reportAchievements_withEligibleChallenges_withCompletionHandler :: (IsNSArray achievements, IsNSArray challenges) => achievements -> challenges -> Ptr () -> IO ()
reportAchievements_withEligibleChallenges_withCompletionHandler achievements challenges completionHandler =
  do
    cls' <- getRequiredClass "GKAchievement"
    withObjCPtr achievements $ \raw_achievements ->
      withObjCPtr challenges $ \raw_challenges ->
        sendClassMsg cls' (mkSelector "reportAchievements:withEligibleChallenges:withCompletionHandler:") retVoid [argPtr (castPtr raw_achievements :: Ptr ()), argPtr (castPtr raw_challenges :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | * This method is obsolete. Calling this initializer does nothing and will return nil **
--
-- ObjC selector: @- initWithIdentifier:forPlayer:@
initWithIdentifier_forPlayer :: (IsGKAchievement gkAchievement, IsNSString identifier, IsNSString playerID) => gkAchievement -> identifier -> playerID -> IO (Id GKAchievement)
initWithIdentifier_forPlayer gkAchievement  identifier playerID =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr playerID $ \raw_playerID ->
      sendMsg gkAchievement (mkSelector "initWithIdentifier:forPlayer:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_playerID :: Ptr ())] >>= ownedObject . castPtr

-- | @- reportAchievementWithCompletionHandler:@
reportAchievementWithCompletionHandler :: IsGKAchievement gkAchievement => gkAchievement -> Ptr () -> IO ()
reportAchievementWithCompletionHandler gkAchievement  completionHandler =
  sendMsg gkAchievement (mkSelector "reportAchievementWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Achievement identifier
--
-- ObjC selector: @- identifier@
identifier :: IsGKAchievement gkAchievement => gkAchievement -> IO (Id NSString)
identifier gkAchievement  =
  sendMsg gkAchievement (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Achievement identifier
--
-- ObjC selector: @- setIdentifier:@
setIdentifier :: (IsGKAchievement gkAchievement, IsNSString value) => gkAchievement -> value -> IO ()
setIdentifier gkAchievement  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkAchievement (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Required, Percentage of achievement complete.
--
-- ObjC selector: @- percentComplete@
percentComplete :: IsGKAchievement gkAchievement => gkAchievement -> IO CDouble
percentComplete gkAchievement  =
  sendMsg gkAchievement (mkSelector "percentComplete") retCDouble []

-- | Required, Percentage of achievement complete.
--
-- ObjC selector: @- setPercentComplete:@
setPercentComplete :: IsGKAchievement gkAchievement => gkAchievement -> CDouble -> IO ()
setPercentComplete gkAchievement  value =
  sendMsg gkAchievement (mkSelector "setPercentComplete:") retVoid [argCDouble (fromIntegral value)]

-- | Set to NO until percentComplete = 100.
--
-- ObjC selector: @- completed@
completed :: IsGKAchievement gkAchievement => gkAchievement -> IO Bool
completed gkAchievement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkAchievement (mkSelector "completed") retCULong []

-- | Date the achievement was last reported. Read-only. Created at initialization
--
-- ObjC selector: @- lastReportedDate@
lastReportedDate :: IsGKAchievement gkAchievement => gkAchievement -> IO (Id NSDate)
lastReportedDate gkAchievement  =
  sendMsg gkAchievement (mkSelector "lastReportedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A banner will be momentarily displayed after reporting a completed achievement
--
-- ObjC selector: @- showsCompletionBanner@
showsCompletionBanner :: IsGKAchievement gkAchievement => gkAchievement -> IO Bool
showsCompletionBanner gkAchievement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkAchievement (mkSelector "showsCompletionBanner") retCULong []

-- | A banner will be momentarily displayed after reporting a completed achievement
--
-- ObjC selector: @- setShowsCompletionBanner:@
setShowsCompletionBanner :: IsGKAchievement gkAchievement => gkAchievement -> Bool -> IO ()
setShowsCompletionBanner gkAchievement  value =
  sendMsg gkAchievement (mkSelector "setShowsCompletionBanner:") retVoid [argCULong (if value then 1 else 0)]

-- | The identifier of the player that earned the achievement.
--
-- ObjC selector: @- player@
player :: IsGKAchievement gkAchievement => gkAchievement -> IO (Id GKPlayer)
player gkAchievement  =
  sendMsg gkAchievement (mkSelector "player") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hidden@
hidden :: IsGKAchievement gkAchievement => gkAchievement -> IO Bool
hidden gkAchievement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkAchievement (mkSelector "hidden") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetAchievementsWithCompletionHandler:@
resetAchievementsWithCompletionHandlerSelector :: Selector
resetAchievementsWithCompletionHandlerSelector = mkSelector "resetAchievementsWithCompletionHandler:"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @initWithIdentifier:player:@
initWithIdentifier_playerSelector :: Selector
initWithIdentifier_playerSelector = mkSelector "initWithIdentifier:player:"

-- | @Selector@ for @reportAchievements:withCompletionHandler:@
reportAchievements_withCompletionHandlerSelector :: Selector
reportAchievements_withCompletionHandlerSelector = mkSelector "reportAchievements:withCompletionHandler:"

-- | @Selector@ for @challengeComposeControllerWithPlayers:message:completionHandler:@
challengeComposeControllerWithPlayers_message_completionHandlerSelector :: Selector
challengeComposeControllerWithPlayers_message_completionHandlerSelector = mkSelector "challengeComposeControllerWithPlayers:message:completionHandler:"

-- | @Selector@ for @challengeComposeControllerWithMessage:players:completionHandler:@
challengeComposeControllerWithMessage_players_completionHandlerSelector :: Selector
challengeComposeControllerWithMessage_players_completionHandlerSelector = mkSelector "challengeComposeControllerWithMessage:players:completionHandler:"

-- | @Selector@ for @challengeComposeControllerWithMessage:players:completion:@
challengeComposeControllerWithMessage_players_completionSelector :: Selector
challengeComposeControllerWithMessage_players_completionSelector = mkSelector "challengeComposeControllerWithMessage:players:completion:"

-- | @Selector@ for @issueChallengeToPlayers:message:@
issueChallengeToPlayers_messageSelector :: Selector
issueChallengeToPlayers_messageSelector = mkSelector "issueChallengeToPlayers:message:"

-- | @Selector@ for @reportAchievements:withEligibleChallenges:withCompletionHandler:@
reportAchievements_withEligibleChallenges_withCompletionHandlerSelector :: Selector
reportAchievements_withEligibleChallenges_withCompletionHandlerSelector = mkSelector "reportAchievements:withEligibleChallenges:withCompletionHandler:"

-- | @Selector@ for @initWithIdentifier:forPlayer:@
initWithIdentifier_forPlayerSelector :: Selector
initWithIdentifier_forPlayerSelector = mkSelector "initWithIdentifier:forPlayer:"

-- | @Selector@ for @reportAchievementWithCompletionHandler:@
reportAchievementWithCompletionHandlerSelector :: Selector
reportAchievementWithCompletionHandlerSelector = mkSelector "reportAchievementWithCompletionHandler:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @percentComplete@
percentCompleteSelector :: Selector
percentCompleteSelector = mkSelector "percentComplete"

-- | @Selector@ for @setPercentComplete:@
setPercentCompleteSelector :: Selector
setPercentCompleteSelector = mkSelector "setPercentComplete:"

-- | @Selector@ for @completed@
completedSelector :: Selector
completedSelector = mkSelector "completed"

-- | @Selector@ for @lastReportedDate@
lastReportedDateSelector :: Selector
lastReportedDateSelector = mkSelector "lastReportedDate"

-- | @Selector@ for @showsCompletionBanner@
showsCompletionBannerSelector :: Selector
showsCompletionBannerSelector = mkSelector "showsCompletionBanner"

-- | @Selector@ for @setShowsCompletionBanner:@
setShowsCompletionBannerSelector :: Selector
setShowsCompletionBannerSelector = mkSelector "setShowsCompletionBanner:"

-- | @Selector@ for @player@
playerSelector :: Selector
playerSelector = mkSelector "player"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

