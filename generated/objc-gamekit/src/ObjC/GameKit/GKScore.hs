{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GKScore represents a score in the leaderboards.
--
-- Generated bindings for @GKScore@.
module ObjC.GameKit.GKScore
  ( GKScore
  , IsGKScore(..)
  , initWithLeaderboardIdentifier
  , initWithLeaderboardIdentifier_player
  , reportScores_withCompletionHandler
  , challengeComposeControllerWithPlayers_message_completionHandler
  , challengeComposeControllerWithMessage_players_completionHandler
  , challengeComposeControllerWithMessage_players_completion
  , issueChallengeToPlayers_message
  , reportScores_withEligibleChallenges_withCompletionHandler
  , reportLeaderboardScores_withEligibleChallenges_withCompletionHandler
  , initWithLeaderboardIdentifier_forPlayer
  , reportScoreWithCompletionHandler
  , initWithCategory
  , value
  , setValue
  , formattedValue
  , leaderboardIdentifier
  , setLeaderboardIdentifier
  , context
  , setContext
  , date
  , player
  , rank
  , shouldSetDefaultLeaderboard
  , setShouldSetDefaultLeaderboard
  , initWithLeaderboardIdentifierSelector
  , initWithLeaderboardIdentifier_playerSelector
  , reportScores_withCompletionHandlerSelector
  , challengeComposeControllerWithPlayers_message_completionHandlerSelector
  , challengeComposeControllerWithMessage_players_completionHandlerSelector
  , challengeComposeControllerWithMessage_players_completionSelector
  , issueChallengeToPlayers_messageSelector
  , reportScores_withEligibleChallenges_withCompletionHandlerSelector
  , reportLeaderboardScores_withEligibleChallenges_withCompletionHandlerSelector
  , initWithLeaderboardIdentifier_forPlayerSelector
  , reportScoreWithCompletionHandlerSelector
  , initWithCategorySelector
  , valueSelector
  , setValueSelector
  , formattedValueSelector
  , leaderboardIdentifierSelector
  , setLeaderboardIdentifierSelector
  , contextSelector
  , setContextSelector
  , dateSelector
  , playerSelector
  , rankSelector
  , shouldSetDefaultLeaderboardSelector
  , setShouldSetDefaultLeaderboardSelector


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

-- | Initialize the score with the local player and current date.
--
-- ObjC selector: @- initWithLeaderboardIdentifier:@
initWithLeaderboardIdentifier :: (IsGKScore gkScore, IsNSString identifier) => gkScore -> identifier -> IO (Id GKScore)
initWithLeaderboardIdentifier gkScore  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg gkScore (mkSelector "initWithLeaderboardIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize the achievement for a specific player. Use to submit participant scores when ending a turn-based match.
--
-- ObjC selector: @- initWithLeaderboardIdentifier:player:@
initWithLeaderboardIdentifier_player :: (IsGKScore gkScore, IsNSString identifier, IsGKPlayer player) => gkScore -> identifier -> player -> IO (Id GKScore)
initWithLeaderboardIdentifier_player gkScore  identifier player =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr player $ \raw_player ->
      sendMsg gkScore (mkSelector "initWithLeaderboardIdentifier:player:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_player :: Ptr ())] >>= ownedObject . castPtr

-- | Report scores to the server. The value must be set, and date may be changed. Possible reasons for error: 1. Value not set 2. Local player not authenticated 3. Communications problem
--
-- ObjC selector: @+ reportScores:withCompletionHandler:@
reportScores_withCompletionHandler :: IsNSArray scores => scores -> Ptr () -> IO ()
reportScores_withCompletionHandler scores completionHandler =
  do
    cls' <- getRequiredClass "GKScore"
    withObjCPtr scores $ \raw_scores ->
      sendClassMsg cls' (mkSelector "reportScores:withCompletionHandler:") retVoid [argPtr (castPtr raw_scores :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | * This method is obsolete. Calling this method does nothing and will return nil **
--
-- ObjC selector: @- challengeComposeControllerWithPlayers:message:completionHandler:@
challengeComposeControllerWithPlayers_message_completionHandler :: (IsGKScore gkScore, IsNSArray playerIDs, IsNSString message) => gkScore -> playerIDs -> message -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithPlayers_message_completionHandler gkScore  playerIDs message completionHandler =
withObjCPtr playerIDs $ \raw_playerIDs ->
  withObjCPtr message $ \raw_message ->
      sendMsg gkScore (mkSelector "challengeComposeControllerWithPlayers:message:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_playerIDs :: Ptr ()), argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- challengeComposeControllerWithMessage:players:completionHandler:@
challengeComposeControllerWithMessage_players_completionHandler :: (IsGKScore gkScore, IsNSString message, IsNSArray players) => gkScore -> message -> players -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithMessage_players_completionHandler gkScore  message players completionHandler =
withObjCPtr message $ \raw_message ->
  withObjCPtr players $ \raw_players ->
      sendMsg gkScore (mkSelector "challengeComposeControllerWithMessage:players:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr raw_players :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- challengeComposeControllerWithMessage:players:completion:@
challengeComposeControllerWithMessage_players_completion :: (IsGKScore gkScore, IsNSString message, IsNSArray players) => gkScore -> message -> players -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithMessage_players_completion gkScore  message players completionHandler =
withObjCPtr message $ \raw_message ->
  withObjCPtr players $ \raw_players ->
      sendMsg gkScore (mkSelector "challengeComposeControllerWithMessage:players:completion:") (retPtr retVoid) [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr raw_players :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- issueChallengeToPlayers:message:@
issueChallengeToPlayers_message :: (IsGKScore gkScore, IsNSArray playerIDs, IsNSString message) => gkScore -> playerIDs -> message -> IO ()
issueChallengeToPlayers_message gkScore  playerIDs message =
withObjCPtr playerIDs $ \raw_playerIDs ->
  withObjCPtr message $ \raw_message ->
      sendMsg gkScore (mkSelector "issueChallengeToPlayers:message:") retVoid [argPtr (castPtr raw_playerIDs :: Ptr ()), argPtr (castPtr raw_message :: Ptr ())]

-- | Use this alternative to reportScores:withCompletionHandler: to allow only certain specific challenges to be completed. Pass nil to avoid completing any challenges.
--
-- ObjC selector: @+ reportScores:withEligibleChallenges:withCompletionHandler:@
reportScores_withEligibleChallenges_withCompletionHandler :: (IsNSArray scores, IsNSArray challenges) => scores -> challenges -> Ptr () -> IO ()
reportScores_withEligibleChallenges_withCompletionHandler scores challenges completionHandler =
  do
    cls' <- getRequiredClass "GKScore"
    withObjCPtr scores $ \raw_scores ->
      withObjCPtr challenges $ \raw_challenges ->
        sendClassMsg cls' (mkSelector "reportScores:withEligibleChallenges:withCompletionHandler:") retVoid [argPtr (castPtr raw_scores :: Ptr ()), argPtr (castPtr raw_challenges :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ reportLeaderboardScores:withEligibleChallenges:withCompletionHandler:@
reportLeaderboardScores_withEligibleChallenges_withCompletionHandler :: (IsNSArray scores, IsNSArray challenges) => scores -> challenges -> Ptr () -> IO ()
reportLeaderboardScores_withEligibleChallenges_withCompletionHandler scores challenges completionHandler =
  do
    cls' <- getRequiredClass "GKScore"
    withObjCPtr scores $ \raw_scores ->
      withObjCPtr challenges $ \raw_challenges ->
        sendClassMsg cls' (mkSelector "reportLeaderboardScores:withEligibleChallenges:withCompletionHandler:") retVoid [argPtr (castPtr raw_scores :: Ptr ()), argPtr (castPtr raw_challenges :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | * This method is obsolete. Calling this initialiser does nothing and will return nil **
--
-- ObjC selector: @- initWithLeaderboardIdentifier:forPlayer:@
initWithLeaderboardIdentifier_forPlayer :: (IsGKScore gkScore, IsNSString identifier, IsNSString playerID) => gkScore -> identifier -> playerID -> IO (Id GKScore)
initWithLeaderboardIdentifier_forPlayer gkScore  identifier playerID =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr playerID $ \raw_playerID ->
      sendMsg gkScore (mkSelector "initWithLeaderboardIdentifier:forPlayer:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_playerID :: Ptr ())] >>= ownedObject . castPtr

-- | @- reportScoreWithCompletionHandler:@
reportScoreWithCompletionHandler :: IsGKScore gkScore => gkScore -> Ptr () -> IO ()
reportScoreWithCompletionHandler gkScore  completionHandler =
  sendMsg gkScore (mkSelector "reportScoreWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- initWithCategory:@
initWithCategory :: (IsGKScore gkScore, IsNSString category) => gkScore -> category -> IO (Id GKScore)
initWithCategory gkScore  category =
withObjCPtr category $ \raw_category ->
    sendMsg gkScore (mkSelector "initWithCategory:") (retPtr retVoid) [argPtr (castPtr raw_category :: Ptr ())] >>= ownedObject . castPtr

-- | The score value as a 64bit integer.
--
-- ObjC selector: @- value@
value :: IsGKScore gkScore => gkScore -> IO CLong
value gkScore  =
  sendMsg gkScore (mkSelector "value") retCLong []

-- | The score value as a 64bit integer.
--
-- ObjC selector: @- setValue:@
setValue :: IsGKScore gkScore => gkScore -> CLong -> IO ()
setValue gkScore  value =
  sendMsg gkScore (mkSelector "setValue:") retVoid [argCLong (fromIntegral value)]

-- | The score formatted as a string, localized with a label
--
-- ObjC selector: @- formattedValue@
formattedValue :: IsGKScore gkScore => gkScore -> IO (Id NSString)
formattedValue gkScore  =
  sendMsg gkScore (mkSelector "formattedValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | leaderboard identifier (required)
--
-- ObjC selector: @- leaderboardIdentifier@
leaderboardIdentifier :: IsGKScore gkScore => gkScore -> IO (Id NSString)
leaderboardIdentifier gkScore  =
  sendMsg gkScore (mkSelector "leaderboardIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | leaderboard identifier (required)
--
-- ObjC selector: @- setLeaderboardIdentifier:@
setLeaderboardIdentifier :: (IsGKScore gkScore, IsNSString value) => gkScore -> value -> IO ()
setLeaderboardIdentifier gkScore  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkScore (mkSelector "setLeaderboardIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | optional additional context that allows a game to store and retrieve additional data associated with the store.  Default value of zero is returned if no value is set.
--
-- ObjC selector: @- context@
context :: IsGKScore gkScore => gkScore -> IO CULong
context gkScore  =
  sendMsg gkScore (mkSelector "context") retCULong []

-- | optional additional context that allows a game to store and retrieve additional data associated with the store.  Default value of zero is returned if no value is set.
--
-- ObjC selector: @- setContext:@
setContext :: IsGKScore gkScore => gkScore -> CULong -> IO ()
setContext gkScore  value =
  sendMsg gkScore (mkSelector "setContext:") retVoid [argCULong (fromIntegral value)]

-- | The date this score was recorded. A newly initialized, unsubmitted GKScore records the current date at init time.
--
-- ObjC selector: @- date@
date :: IsGKScore gkScore => gkScore -> IO (Id NSDate)
date gkScore  =
  sendMsg gkScore (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The player that recorded the score.
--
-- ObjC selector: @- player@
player :: IsGKScore gkScore => gkScore -> IO (Id GKPlayer)
player gkScore  =
  sendMsg gkScore (mkSelector "player") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The rank of the player within the leaderboard, only valid when returned from GKLeaderboard
--
-- ObjC selector: @- rank@
rank :: IsGKScore gkScore => gkScore -> IO CLong
rank gkScore  =
  sendMsg gkScore (mkSelector "rank") retCLong []

-- | Convenience property to make the leaderboard associated with this GKScore, the default leaderboard for this player. Default value is false. If true, reporting that score will make the category this score belongs to, the default leaderboard for this user
--
-- ObjC selector: @- shouldSetDefaultLeaderboard@
shouldSetDefaultLeaderboard :: IsGKScore gkScore => gkScore -> IO Bool
shouldSetDefaultLeaderboard gkScore  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkScore (mkSelector "shouldSetDefaultLeaderboard") retCULong []

-- | Convenience property to make the leaderboard associated with this GKScore, the default leaderboard for this player. Default value is false. If true, reporting that score will make the category this score belongs to, the default leaderboard for this user
--
-- ObjC selector: @- setShouldSetDefaultLeaderboard:@
setShouldSetDefaultLeaderboard :: IsGKScore gkScore => gkScore -> Bool -> IO ()
setShouldSetDefaultLeaderboard gkScore  value =
  sendMsg gkScore (mkSelector "setShouldSetDefaultLeaderboard:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLeaderboardIdentifier:@
initWithLeaderboardIdentifierSelector :: Selector
initWithLeaderboardIdentifierSelector = mkSelector "initWithLeaderboardIdentifier:"

-- | @Selector@ for @initWithLeaderboardIdentifier:player:@
initWithLeaderboardIdentifier_playerSelector :: Selector
initWithLeaderboardIdentifier_playerSelector = mkSelector "initWithLeaderboardIdentifier:player:"

-- | @Selector@ for @reportScores:withCompletionHandler:@
reportScores_withCompletionHandlerSelector :: Selector
reportScores_withCompletionHandlerSelector = mkSelector "reportScores:withCompletionHandler:"

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

-- | @Selector@ for @reportScores:withEligibleChallenges:withCompletionHandler:@
reportScores_withEligibleChallenges_withCompletionHandlerSelector :: Selector
reportScores_withEligibleChallenges_withCompletionHandlerSelector = mkSelector "reportScores:withEligibleChallenges:withCompletionHandler:"

-- | @Selector@ for @reportLeaderboardScores:withEligibleChallenges:withCompletionHandler:@
reportLeaderboardScores_withEligibleChallenges_withCompletionHandlerSelector :: Selector
reportLeaderboardScores_withEligibleChallenges_withCompletionHandlerSelector = mkSelector "reportLeaderboardScores:withEligibleChallenges:withCompletionHandler:"

-- | @Selector@ for @initWithLeaderboardIdentifier:forPlayer:@
initWithLeaderboardIdentifier_forPlayerSelector :: Selector
initWithLeaderboardIdentifier_forPlayerSelector = mkSelector "initWithLeaderboardIdentifier:forPlayer:"

-- | @Selector@ for @reportScoreWithCompletionHandler:@
reportScoreWithCompletionHandlerSelector :: Selector
reportScoreWithCompletionHandlerSelector = mkSelector "reportScoreWithCompletionHandler:"

-- | @Selector@ for @initWithCategory:@
initWithCategorySelector :: Selector
initWithCategorySelector = mkSelector "initWithCategory:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @formattedValue@
formattedValueSelector :: Selector
formattedValueSelector = mkSelector "formattedValue"

-- | @Selector@ for @leaderboardIdentifier@
leaderboardIdentifierSelector :: Selector
leaderboardIdentifierSelector = mkSelector "leaderboardIdentifier"

-- | @Selector@ for @setLeaderboardIdentifier:@
setLeaderboardIdentifierSelector :: Selector
setLeaderboardIdentifierSelector = mkSelector "setLeaderboardIdentifier:"

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

-- | @Selector@ for @setContext:@
setContextSelector :: Selector
setContextSelector = mkSelector "setContext:"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @player@
playerSelector :: Selector
playerSelector = mkSelector "player"

-- | @Selector@ for @rank@
rankSelector :: Selector
rankSelector = mkSelector "rank"

-- | @Selector@ for @shouldSetDefaultLeaderboard@
shouldSetDefaultLeaderboardSelector :: Selector
shouldSetDefaultLeaderboardSelector = mkSelector "shouldSetDefaultLeaderboard"

-- | @Selector@ for @setShouldSetDefaultLeaderboard:@
setShouldSetDefaultLeaderboardSelector :: Selector
setShouldSetDefaultLeaderboardSelector = mkSelector "setShouldSetDefaultLeaderboard:"

