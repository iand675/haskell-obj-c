{-# LANGUAGE DataKinds #-}
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
  , playerID
  , category
  , setCategory
  , categorySelector
  , challengeComposeControllerWithMessage_players_completionHandlerSelector
  , challengeComposeControllerWithMessage_players_completionSelector
  , challengeComposeControllerWithPlayers_message_completionHandlerSelector
  , contextSelector
  , dateSelector
  , formattedValueSelector
  , initWithCategorySelector
  , initWithLeaderboardIdentifierSelector
  , initWithLeaderboardIdentifier_forPlayerSelector
  , initWithLeaderboardIdentifier_playerSelector
  , issueChallengeToPlayers_messageSelector
  , leaderboardIdentifierSelector
  , playerIDSelector
  , playerSelector
  , rankSelector
  , reportLeaderboardScores_withEligibleChallenges_withCompletionHandlerSelector
  , reportScoreWithCompletionHandlerSelector
  , reportScores_withCompletionHandlerSelector
  , reportScores_withEligibleChallenges_withCompletionHandlerSelector
  , setCategorySelector
  , setContextSelector
  , setLeaderboardIdentifierSelector
  , setShouldSetDefaultLeaderboardSelector
  , setValueSelector
  , shouldSetDefaultLeaderboardSelector
  , valueSelector


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

-- | Initialize the score with the local player and current date.
--
-- ObjC selector: @- initWithLeaderboardIdentifier:@
initWithLeaderboardIdentifier :: (IsGKScore gkScore, IsNSString identifier) => gkScore -> identifier -> IO (Id GKScore)
initWithLeaderboardIdentifier gkScore identifier =
  sendOwnedMessage gkScore initWithLeaderboardIdentifierSelector (toNSString identifier)

-- | Initialize the achievement for a specific player. Use to submit participant scores when ending a turn-based match.
--
-- ObjC selector: @- initWithLeaderboardIdentifier:player:@
initWithLeaderboardIdentifier_player :: (IsGKScore gkScore, IsNSString identifier, IsGKPlayer player) => gkScore -> identifier -> player -> IO (Id GKScore)
initWithLeaderboardIdentifier_player gkScore identifier player =
  sendOwnedMessage gkScore initWithLeaderboardIdentifier_playerSelector (toNSString identifier) (toGKPlayer player)

-- | Report scores to the server. The value must be set, and date may be changed. Possible reasons for error: 1. Value not set 2. Local player not authenticated 3. Communications problem
--
-- ObjC selector: @+ reportScores:withCompletionHandler:@
reportScores_withCompletionHandler :: IsNSArray scores => scores -> Ptr () -> IO ()
reportScores_withCompletionHandler scores completionHandler =
  do
    cls' <- getRequiredClass "GKScore"
    sendClassMessage cls' reportScores_withCompletionHandlerSelector (toNSArray scores) completionHandler

-- | * This method is obsolete. Calling this method does nothing and will return nil **
--
-- ObjC selector: @- challengeComposeControllerWithPlayers:message:completionHandler:@
challengeComposeControllerWithPlayers_message_completionHandler :: (IsGKScore gkScore, IsNSArray playerIDs, IsNSString message) => gkScore -> playerIDs -> message -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithPlayers_message_completionHandler gkScore playerIDs message completionHandler =
  sendMessage gkScore challengeComposeControllerWithPlayers_message_completionHandlerSelector (toNSArray playerIDs) (toNSString message) completionHandler

-- | @- challengeComposeControllerWithMessage:players:completionHandler:@
challengeComposeControllerWithMessage_players_completionHandler :: (IsGKScore gkScore, IsNSString message, IsNSArray players) => gkScore -> message -> players -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithMessage_players_completionHandler gkScore message players completionHandler =
  sendMessage gkScore challengeComposeControllerWithMessage_players_completionHandlerSelector (toNSString message) (toNSArray players) completionHandler

-- | @- challengeComposeControllerWithMessage:players:completion:@
challengeComposeControllerWithMessage_players_completion :: (IsGKScore gkScore, IsNSString message, IsNSArray players) => gkScore -> message -> players -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithMessage_players_completion gkScore message players completionHandler =
  sendMessage gkScore challengeComposeControllerWithMessage_players_completionSelector (toNSString message) (toNSArray players) completionHandler

-- | * This method is obsolete. It will never be invoked and its implementation does nothing**
--
-- ObjC selector: @- issueChallengeToPlayers:message:@
issueChallengeToPlayers_message :: (IsGKScore gkScore, IsNSArray playerIDs, IsNSString message) => gkScore -> playerIDs -> message -> IO ()
issueChallengeToPlayers_message gkScore playerIDs message =
  sendMessage gkScore issueChallengeToPlayers_messageSelector (toNSArray playerIDs) (toNSString message)

-- | Use this alternative to reportScores:withCompletionHandler: to allow only certain specific challenges to be completed. Pass nil to avoid completing any challenges.
--
-- ObjC selector: @+ reportScores:withEligibleChallenges:withCompletionHandler:@
reportScores_withEligibleChallenges_withCompletionHandler :: (IsNSArray scores, IsNSArray challenges) => scores -> challenges -> Ptr () -> IO ()
reportScores_withEligibleChallenges_withCompletionHandler scores challenges completionHandler =
  do
    cls' <- getRequiredClass "GKScore"
    sendClassMessage cls' reportScores_withEligibleChallenges_withCompletionHandlerSelector (toNSArray scores) (toNSArray challenges) completionHandler

-- | @+ reportLeaderboardScores:withEligibleChallenges:withCompletionHandler:@
reportLeaderboardScores_withEligibleChallenges_withCompletionHandler :: (IsNSArray scores, IsNSArray challenges) => scores -> challenges -> Ptr () -> IO ()
reportLeaderboardScores_withEligibleChallenges_withCompletionHandler scores challenges completionHandler =
  do
    cls' <- getRequiredClass "GKScore"
    sendClassMessage cls' reportLeaderboardScores_withEligibleChallenges_withCompletionHandlerSelector (toNSArray scores) (toNSArray challenges) completionHandler

-- | * This method is obsolete. Calling this initialiser does nothing and will return nil **
--
-- ObjC selector: @- initWithLeaderboardIdentifier:forPlayer:@
initWithLeaderboardIdentifier_forPlayer :: (IsGKScore gkScore, IsNSString identifier, IsNSString playerID) => gkScore -> identifier -> playerID -> IO (Id GKScore)
initWithLeaderboardIdentifier_forPlayer gkScore identifier playerID =
  sendOwnedMessage gkScore initWithLeaderboardIdentifier_forPlayerSelector (toNSString identifier) (toNSString playerID)

-- | @- reportScoreWithCompletionHandler:@
reportScoreWithCompletionHandler :: IsGKScore gkScore => gkScore -> Ptr () -> IO ()
reportScoreWithCompletionHandler gkScore completionHandler =
  sendMessage gkScore reportScoreWithCompletionHandlerSelector completionHandler

-- | @- initWithCategory:@
initWithCategory :: (IsGKScore gkScore, IsNSString category) => gkScore -> category -> IO (Id GKScore)
initWithCategory gkScore category =
  sendOwnedMessage gkScore initWithCategorySelector (toNSString category)

-- | The score value as a 64bit integer.
--
-- ObjC selector: @- value@
value :: IsGKScore gkScore => gkScore -> IO CLong
value gkScore =
  sendMessage gkScore valueSelector

-- | The score value as a 64bit integer.
--
-- ObjC selector: @- setValue:@
setValue :: IsGKScore gkScore => gkScore -> CLong -> IO ()
setValue gkScore value =
  sendMessage gkScore setValueSelector value

-- | The score formatted as a string, localized with a label
--
-- ObjC selector: @- formattedValue@
formattedValue :: IsGKScore gkScore => gkScore -> IO (Id NSString)
formattedValue gkScore =
  sendMessage gkScore formattedValueSelector

-- | leaderboard identifier (required)
--
-- ObjC selector: @- leaderboardIdentifier@
leaderboardIdentifier :: IsGKScore gkScore => gkScore -> IO (Id NSString)
leaderboardIdentifier gkScore =
  sendMessage gkScore leaderboardIdentifierSelector

-- | leaderboard identifier (required)
--
-- ObjC selector: @- setLeaderboardIdentifier:@
setLeaderboardIdentifier :: (IsGKScore gkScore, IsNSString value) => gkScore -> value -> IO ()
setLeaderboardIdentifier gkScore value =
  sendMessage gkScore setLeaderboardIdentifierSelector (toNSString value)

-- | optional additional context that allows a game to store and retrieve additional data associated with the store.  Default value of zero is returned if no value is set.
--
-- ObjC selector: @- context@
context :: IsGKScore gkScore => gkScore -> IO CULong
context gkScore =
  sendMessage gkScore contextSelector

-- | optional additional context that allows a game to store and retrieve additional data associated with the store.  Default value of zero is returned if no value is set.
--
-- ObjC selector: @- setContext:@
setContext :: IsGKScore gkScore => gkScore -> CULong -> IO ()
setContext gkScore value =
  sendMessage gkScore setContextSelector value

-- | The date this score was recorded. A newly initialized, unsubmitted GKScore records the current date at init time.
--
-- ObjC selector: @- date@
date :: IsGKScore gkScore => gkScore -> IO (Id NSDate)
date gkScore =
  sendMessage gkScore dateSelector

-- | The player that recorded the score.
--
-- ObjC selector: @- player@
player :: IsGKScore gkScore => gkScore -> IO (Id GKPlayer)
player gkScore =
  sendMessage gkScore playerSelector

-- | The rank of the player within the leaderboard, only valid when returned from GKLeaderboard
--
-- ObjC selector: @- rank@
rank :: IsGKScore gkScore => gkScore -> IO CLong
rank gkScore =
  sendMessage gkScore rankSelector

-- | Convenience property to make the leaderboard associated with this GKScore, the default leaderboard for this player. Default value is false. If true, reporting that score will make the category this score belongs to, the default leaderboard for this user
--
-- ObjC selector: @- shouldSetDefaultLeaderboard@
shouldSetDefaultLeaderboard :: IsGKScore gkScore => gkScore -> IO Bool
shouldSetDefaultLeaderboard gkScore =
  sendMessage gkScore shouldSetDefaultLeaderboardSelector

-- | Convenience property to make the leaderboard associated with this GKScore, the default leaderboard for this player. Default value is false. If true, reporting that score will make the category this score belongs to, the default leaderboard for this user
--
-- ObjC selector: @- setShouldSetDefaultLeaderboard:@
setShouldSetDefaultLeaderboard :: IsGKScore gkScore => gkScore -> Bool -> IO ()
setShouldSetDefaultLeaderboard gkScore value =
  sendMessage gkScore setShouldSetDefaultLeaderboardSelector value

-- | * This property is obsolete. **
--
-- ObjC selector: @- playerID@
playerID :: IsGKScore gkScore => gkScore -> IO (Id NSString)
playerID gkScore =
  sendMessage gkScore playerIDSelector

-- | @- category@
category :: IsGKScore gkScore => gkScore -> IO (Id NSString)
category gkScore =
  sendMessage gkScore categorySelector

-- | @- setCategory:@
setCategory :: (IsGKScore gkScore, IsNSString value) => gkScore -> value -> IO ()
setCategory gkScore value =
  sendMessage gkScore setCategorySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLeaderboardIdentifier:@
initWithLeaderboardIdentifierSelector :: Selector '[Id NSString] (Id GKScore)
initWithLeaderboardIdentifierSelector = mkSelector "initWithLeaderboardIdentifier:"

-- | @Selector@ for @initWithLeaderboardIdentifier:player:@
initWithLeaderboardIdentifier_playerSelector :: Selector '[Id NSString, Id GKPlayer] (Id GKScore)
initWithLeaderboardIdentifier_playerSelector = mkSelector "initWithLeaderboardIdentifier:player:"

-- | @Selector@ for @reportScores:withCompletionHandler:@
reportScores_withCompletionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
reportScores_withCompletionHandlerSelector = mkSelector "reportScores:withCompletionHandler:"

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

-- | @Selector@ for @reportScores:withEligibleChallenges:withCompletionHandler:@
reportScores_withEligibleChallenges_withCompletionHandlerSelector :: Selector '[Id NSArray, Id NSArray, Ptr ()] ()
reportScores_withEligibleChallenges_withCompletionHandlerSelector = mkSelector "reportScores:withEligibleChallenges:withCompletionHandler:"

-- | @Selector@ for @reportLeaderboardScores:withEligibleChallenges:withCompletionHandler:@
reportLeaderboardScores_withEligibleChallenges_withCompletionHandlerSelector :: Selector '[Id NSArray, Id NSArray, Ptr ()] ()
reportLeaderboardScores_withEligibleChallenges_withCompletionHandlerSelector = mkSelector "reportLeaderboardScores:withEligibleChallenges:withCompletionHandler:"

-- | @Selector@ for @initWithLeaderboardIdentifier:forPlayer:@
initWithLeaderboardIdentifier_forPlayerSelector :: Selector '[Id NSString, Id NSString] (Id GKScore)
initWithLeaderboardIdentifier_forPlayerSelector = mkSelector "initWithLeaderboardIdentifier:forPlayer:"

-- | @Selector@ for @reportScoreWithCompletionHandler:@
reportScoreWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
reportScoreWithCompletionHandlerSelector = mkSelector "reportScoreWithCompletionHandler:"

-- | @Selector@ for @initWithCategory:@
initWithCategorySelector :: Selector '[Id NSString] (Id GKScore)
initWithCategorySelector = mkSelector "initWithCategory:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] CLong
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[CLong] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @formattedValue@
formattedValueSelector :: Selector '[] (Id NSString)
formattedValueSelector = mkSelector "formattedValue"

-- | @Selector@ for @leaderboardIdentifier@
leaderboardIdentifierSelector :: Selector '[] (Id NSString)
leaderboardIdentifierSelector = mkSelector "leaderboardIdentifier"

-- | @Selector@ for @setLeaderboardIdentifier:@
setLeaderboardIdentifierSelector :: Selector '[Id NSString] ()
setLeaderboardIdentifierSelector = mkSelector "setLeaderboardIdentifier:"

-- | @Selector@ for @context@
contextSelector :: Selector '[] CULong
contextSelector = mkSelector "context"

-- | @Selector@ for @setContext:@
setContextSelector :: Selector '[CULong] ()
setContextSelector = mkSelector "setContext:"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @player@
playerSelector :: Selector '[] (Id GKPlayer)
playerSelector = mkSelector "player"

-- | @Selector@ for @rank@
rankSelector :: Selector '[] CLong
rankSelector = mkSelector "rank"

-- | @Selector@ for @shouldSetDefaultLeaderboard@
shouldSetDefaultLeaderboardSelector :: Selector '[] Bool
shouldSetDefaultLeaderboardSelector = mkSelector "shouldSetDefaultLeaderboard"

-- | @Selector@ for @setShouldSetDefaultLeaderboard:@
setShouldSetDefaultLeaderboardSelector :: Selector '[Bool] ()
setShouldSetDefaultLeaderboardSelector = mkSelector "setShouldSetDefaultLeaderboard:"

-- | @Selector@ for @playerID@
playerIDSelector :: Selector '[] (Id NSString)
playerIDSelector = mkSelector "playerID"

-- | @Selector@ for @category@
categorySelector :: Selector '[] (Id NSString)
categorySelector = mkSelector "category"

-- | @Selector@ for @setCategory:@
setCategorySelector :: Selector '[Id NSString] ()
setCategorySelector = mkSelector "setCategory:"

