{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GKLeaderboard represents a single instance of a leaderboard for the current game. Leaderboards can be of the following types:      1. Classic - Traditional, non-expiring leaderboards      2. Recurring - Periodic timed leaderboards that follow a recurrence rule defined in App Store Connect.
--
-- Generated bindings for @GKLeaderboard@.
module ObjC.GameKit.GKLeaderboard
  ( GKLeaderboard
  , IsGKLeaderboard(..)
  , loadPreviousOccurrenceWithCompletionHandler
  , submitScore_context_player_leaderboardIDs_completionHandler
  , submitScore_context_player_completionHandler
  , loadImageWithCompletionHandler
  , initWithPlayerIDs
  , setDefaultLeaderboard_withCompletionHandler
  , init_
  , initWithPlayers
  , title
  , groupIdentifier
  , baseLeaderboardID
  , type_
  , startDate
  , nextStartDate
  , duration
  , leaderboardDescription
  , releaseState
  , activityIdentifier
  , activityProperties
  , isHidden
  , category
  , setCategory
  , timeScope
  , setTimeScope
  , playerScope
  , setPlayerScope
  , identifier
  , setIdentifier
  , range
  , setRange
  , scores
  , maxRange
  , localPlayerScore
  , loading
  , activityIdentifierSelector
  , activityPropertiesSelector
  , baseLeaderboardIDSelector
  , categorySelector
  , durationSelector
  , groupIdentifierSelector
  , identifierSelector
  , initSelector
  , initWithPlayerIDsSelector
  , initWithPlayersSelector
  , isHiddenSelector
  , leaderboardDescriptionSelector
  , loadImageWithCompletionHandlerSelector
  , loadPreviousOccurrenceWithCompletionHandlerSelector
  , loadingSelector
  , localPlayerScoreSelector
  , maxRangeSelector
  , nextStartDateSelector
  , playerScopeSelector
  , rangeSelector
  , releaseStateSelector
  , scoresSelector
  , setCategorySelector
  , setDefaultLeaderboard_withCompletionHandlerSelector
  , setIdentifierSelector
  , setPlayerScopeSelector
  , setRangeSelector
  , setTimeScopeSelector
  , startDateSelector
  , submitScore_context_player_completionHandlerSelector
  , submitScore_context_player_leaderboardIDs_completionHandlerSelector
  , timeScopeSelector
  , titleSelector
  , typeSelector

  -- * Enum types
  , GKLeaderboardPlayerScope(GKLeaderboardPlayerScope)
  , pattern GKLeaderboardPlayerScopeGlobal
  , pattern GKLeaderboardPlayerScopeFriendsOnly
  , GKLeaderboardTimeScope(GKLeaderboardTimeScope)
  , pattern GKLeaderboardTimeScopeToday
  , pattern GKLeaderboardTimeScopeWeek
  , pattern GKLeaderboardTimeScopeAllTime
  , GKLeaderboardType(GKLeaderboardType)
  , pattern GKLeaderboardTypeClassic
  , pattern GKLeaderboardTypeRecurring
  , GKReleaseState(GKReleaseState)
  , pattern GKReleaseStateUnknown
  , pattern GKReleaseStateReleased
  , pattern GKReleaseStatePrereleased

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
import ObjC.Foundation.Internal.Classes

-- | Loads the occurrence preceding this occurrence for a recurring leaderboard in which the local player submitted a score. If no previous occurrence is found that the player submitted a score to, then the most recent previous occurrence is returned.
--
-- ObjC selector: @- loadPreviousOccurrenceWithCompletionHandler:@
loadPreviousOccurrenceWithCompletionHandler :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> Ptr () -> IO ()
loadPreviousOccurrenceWithCompletionHandler gkLeaderboard completionHandler =
  sendMessage gkLeaderboard loadPreviousOccurrenceWithCompletionHandlerSelector completionHandler

-- | Class method to submit a single score to multiple leaderboards   score - earned by the player   context - developer supplied metadata associated with the player's score   player - the player for whom this score is being submitted   leaderboardIDs - one or more leaderboard IDs defined in App Store Connect
--
-- ObjC selector: @+ submitScore:context:player:leaderboardIDs:completionHandler:@
submitScore_context_player_leaderboardIDs_completionHandler :: (IsGKPlayer player, IsNSArray leaderboardIDs) => CLong -> CULong -> player -> leaderboardIDs -> Ptr () -> IO ()
submitScore_context_player_leaderboardIDs_completionHandler score context player leaderboardIDs completionHandler =
  do
    cls' <- getRequiredClass "GKLeaderboard"
    sendClassMessage cls' submitScore_context_player_leaderboardIDs_completionHandlerSelector score context (toGKPlayer player) (toNSArray leaderboardIDs) completionHandler

-- | Instance method to submit a single score to the leaderboard associated with this instance   score - earned by the player   context - developer supplied metadata associated with the player's score   player - the player for whom this score is being submitted
--
-- ObjC selector: @- submitScore:context:player:completionHandler:@
submitScore_context_player_completionHandler :: (IsGKLeaderboard gkLeaderboard, IsGKPlayer player) => gkLeaderboard -> CLong -> CULong -> player -> Ptr () -> IO ()
submitScore_context_player_completionHandler gkLeaderboard score context player completionHandler =
  sendMessage gkLeaderboard submitScore_context_player_completionHandlerSelector score context (toGKPlayer player) completionHandler

-- | Asynchronously load the image. Error will be nil on success.
--
-- ObjC selector: @- loadImageWithCompletionHandler:@
loadImageWithCompletionHandler :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> Ptr () -> IO ()
loadImageWithCompletionHandler gkLeaderboard completionHandler =
  sendMessage gkLeaderboard loadImageWithCompletionHandlerSelector completionHandler

-- | @- initWithPlayerIDs:@
initWithPlayerIDs :: (IsGKLeaderboard gkLeaderboard, IsNSArray playerIDs) => gkLeaderboard -> playerIDs -> IO (Id GKLeaderboard)
initWithPlayerIDs gkLeaderboard playerIDs =
  sendOwnedMessage gkLeaderboard initWithPlayerIDsSelector (toNSArray playerIDs)

-- | @+ setDefaultLeaderboard:withCompletionHandler:@
setDefaultLeaderboard_withCompletionHandler :: IsNSString leaderboardIdentifier => leaderboardIdentifier -> Ptr () -> IO ()
setDefaultLeaderboard_withCompletionHandler leaderboardIdentifier completionHandler =
  do
    cls' <- getRequiredClass "GKLeaderboard"
    sendClassMessage cls' setDefaultLeaderboard_withCompletionHandlerSelector (toNSString leaderboardIdentifier) completionHandler

-- | Default is the range 1-10 with Global/AllTime scopes. If you want to change the scopes or range, set the properites before loading the scores.
--
-- ObjC selector: @- init@
init_ :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id GKLeaderboard)
init_ gkLeaderboard =
  sendOwnedMessage gkLeaderboard initSelector

-- | Specify an array of GKPlayers. For example, the players who are in a match together Defaults to AllTime score, if you want to change the timeScope, set the property before loading the scores. Range and playerScope are not applicable. players may not be nil.
--
-- ObjC selector: @- initWithPlayers:@
initWithPlayers :: (IsGKLeaderboard gkLeaderboard, IsNSArray players) => gkLeaderboard -> players -> IO (Id GKLeaderboard)
initWithPlayers gkLeaderboard players =
  sendOwnedMessage gkLeaderboard initWithPlayersSelector (toNSArray players)

-- | Localized title
--
-- ObjC selector: @- title@
title :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
title gkLeaderboard =
  sendMessage gkLeaderboard titleSelector

-- | set when leaderboards have been designated a game group; set when loadLeaderboardsWithCompletionHandler has been called for leaderboards that support game groups
--
-- ObjC selector: @- groupIdentifier@
groupIdentifier :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
groupIdentifier gkLeaderboard =
  sendMessage gkLeaderboard groupIdentifierSelector

-- | Leaderboard ID defined in App Store Connect that this instance is associated with
--
-- ObjC selector: @- baseLeaderboardID@
baseLeaderboardID :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
baseLeaderboardID gkLeaderboard =
  sendMessage gkLeaderboard baseLeaderboardIDSelector

-- | Type of leaderboard
--
-- ObjC selector: @- type@
type_ :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO GKLeaderboardType
type_ gkLeaderboard =
  sendMessage gkLeaderboard typeSelector

-- | Date and time this instance started accepting score submissions (only applicable to recurring leaderboards)
--
-- ObjC selector: @- startDate@
startDate :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSDate)
startDate gkLeaderboard =
  sendMessage gkLeaderboard startDateSelector

-- | Date and time the next instance will start accepting score submissions (only applicable to recurring leaderboards)
--
-- ObjC selector: @- nextStartDate@
nextStartDate :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSDate)
nextStartDate gkLeaderboard =
  sendMessage gkLeaderboard nextStartDateSelector

-- | Duration from startDate during which this leaderboard instance accepts score submissions (only applicable to recurring leaderboards)
--
-- ObjC selector: @- duration@
duration :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO CDouble
duration gkLeaderboard =
  sendMessage gkLeaderboard durationSelector

-- | The description of this Leaderboard as configured by the developer in App Store Connect.
--
-- ObjC selector: @- leaderboardDescription@
leaderboardDescription :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
leaderboardDescription gkLeaderboard =
  sendMessage gkLeaderboard leaderboardDescriptionSelector

-- | The release state of the leaderboard in App Store Connect.
--
-- ObjC selector: @- releaseState@
releaseState :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO GKReleaseState
releaseState gkLeaderboard =
  sendMessage gkLeaderboard releaseStateSelector

-- | The identifier of the game activity associated with this leaderboard, as configured by the developer in App Store Connect.
--
-- ObjC selector: @- activityIdentifier@
activityIdentifier :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
activityIdentifier gkLeaderboard =
  sendMessage gkLeaderboard activityIdentifierSelector

-- | The properties when associating this leaderboard with a game activity, as configured by the developer in App Store Connect.
--
-- ObjC selector: @- activityProperties@
activityProperties :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSDictionary)
activityProperties gkLeaderboard =
  sendMessage gkLeaderboard activityPropertiesSelector

-- | A Boolean value that indicates whether the current leaderboard isn't visible in Game Center views.
--
-- You can still submit scores to a hidden leaderboard.
--
-- ObjC selector: @- isHidden@
isHidden :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO Bool
isHidden gkLeaderboard =
  sendMessage gkLeaderboard isHiddenSelector

-- | @- category@
category :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
category gkLeaderboard =
  sendMessage gkLeaderboard categorySelector

-- | @- setCategory:@
setCategory :: (IsGKLeaderboard gkLeaderboard, IsNSString value) => gkLeaderboard -> value -> IO ()
setCategory gkLeaderboard value =
  sendMessage gkLeaderboard setCategorySelector (toNSString value)

-- | @- timeScope@
timeScope :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO GKLeaderboardTimeScope
timeScope gkLeaderboard =
  sendMessage gkLeaderboard timeScopeSelector

-- | @- setTimeScope:@
setTimeScope :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> GKLeaderboardTimeScope -> IO ()
setTimeScope gkLeaderboard value =
  sendMessage gkLeaderboard setTimeScopeSelector value

-- | Filter on friends. Does not apply to leaderboard initialized with players.
--
-- ObjC selector: @- playerScope@
playerScope :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO GKLeaderboardPlayerScope
playerScope gkLeaderboard =
  sendMessage gkLeaderboard playerScopeSelector

-- | Filter on friends. Does not apply to leaderboard initialized with players.
--
-- ObjC selector: @- setPlayerScope:@
setPlayerScope :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> GKLeaderboardPlayerScope -> IO ()
setPlayerScope gkLeaderboard value =
  sendMessage gkLeaderboard setPlayerScopeSelector value

-- | leaderboardID. If nil, fetch the aggregate leaderboard.
--
-- ObjC selector: @- identifier@
identifier :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
identifier gkLeaderboard =
  sendMessage gkLeaderboard identifierSelector

-- | leaderboardID. If nil, fetch the aggregate leaderboard.
--
-- ObjC selector: @- setIdentifier:@
setIdentifier :: (IsGKLeaderboard gkLeaderboard, IsNSString value) => gkLeaderboard -> value -> IO ()
setIdentifier gkLeaderboard value =
  sendMessage gkLeaderboard setIdentifierSelector (toNSString value)

-- | Leaderboards start at index 1 and the length should be less than 100. Does not apply to leaderboards initialized with players.  Exception will be thrown if developer tries to set an invalid range.
--
-- ObjC selector: @- range@
range :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO NSRange
range gkLeaderboard =
  sendMessage gkLeaderboard rangeSelector

-- | Leaderboards start at index 1 and the length should be less than 100. Does not apply to leaderboards initialized with players.  Exception will be thrown if developer tries to set an invalid range.
--
-- ObjC selector: @- setRange:@
setRange :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> NSRange -> IO ()
setRange gkLeaderboard value =
  sendMessage gkLeaderboard setRangeSelector value

-- | Scores are not valid until loadScores: has completed.
--
-- ObjC selector: @- scores@
scores :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSArray)
scores gkLeaderboard =
  sendMessage gkLeaderboard scoresSelector

-- | The maxRange which represents the size of the leaderboard is not valid until loadScores: has completed.
--
-- ObjC selector: @- maxRange@
maxRange :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO CULong
maxRange gkLeaderboard =
  sendMessage gkLeaderboard maxRangeSelector

-- | The local player's score
--
-- ObjC selector: @- localPlayerScore@
localPlayerScore :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id GKScore)
localPlayerScore gkLeaderboard =
  sendMessage gkLeaderboard localPlayerScoreSelector

-- | This property is true if the leaderboard is currently loading
--
-- ObjC selector: @- loading@
loading :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO Bool
loading gkLeaderboard =
  sendMessage gkLeaderboard loadingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadPreviousOccurrenceWithCompletionHandler:@
loadPreviousOccurrenceWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadPreviousOccurrenceWithCompletionHandlerSelector = mkSelector "loadPreviousOccurrenceWithCompletionHandler:"

-- | @Selector@ for @submitScore:context:player:leaderboardIDs:completionHandler:@
submitScore_context_player_leaderboardIDs_completionHandlerSelector :: Selector '[CLong, CULong, Id GKPlayer, Id NSArray, Ptr ()] ()
submitScore_context_player_leaderboardIDs_completionHandlerSelector = mkSelector "submitScore:context:player:leaderboardIDs:completionHandler:"

-- | @Selector@ for @submitScore:context:player:completionHandler:@
submitScore_context_player_completionHandlerSelector :: Selector '[CLong, CULong, Id GKPlayer, Ptr ()] ()
submitScore_context_player_completionHandlerSelector = mkSelector "submitScore:context:player:completionHandler:"

-- | @Selector@ for @loadImageWithCompletionHandler:@
loadImageWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadImageWithCompletionHandlerSelector = mkSelector "loadImageWithCompletionHandler:"

-- | @Selector@ for @initWithPlayerIDs:@
initWithPlayerIDsSelector :: Selector '[Id NSArray] (Id GKLeaderboard)
initWithPlayerIDsSelector = mkSelector "initWithPlayerIDs:"

-- | @Selector@ for @setDefaultLeaderboard:withCompletionHandler:@
setDefaultLeaderboard_withCompletionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
setDefaultLeaderboard_withCompletionHandlerSelector = mkSelector "setDefaultLeaderboard:withCompletionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GKLeaderboard)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPlayers:@
initWithPlayersSelector :: Selector '[Id NSArray] (Id GKLeaderboard)
initWithPlayersSelector = mkSelector "initWithPlayers:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @groupIdentifier@
groupIdentifierSelector :: Selector '[] (Id NSString)
groupIdentifierSelector = mkSelector "groupIdentifier"

-- | @Selector@ for @baseLeaderboardID@
baseLeaderboardIDSelector :: Selector '[] (Id NSString)
baseLeaderboardIDSelector = mkSelector "baseLeaderboardID"

-- | @Selector@ for @type@
typeSelector :: Selector '[] GKLeaderboardType
typeSelector = mkSelector "type"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @nextStartDate@
nextStartDateSelector :: Selector '[] (Id NSDate)
nextStartDateSelector = mkSelector "nextStartDate"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @leaderboardDescription@
leaderboardDescriptionSelector :: Selector '[] (Id NSString)
leaderboardDescriptionSelector = mkSelector "leaderboardDescription"

-- | @Selector@ for @releaseState@
releaseStateSelector :: Selector '[] GKReleaseState
releaseStateSelector = mkSelector "releaseState"

-- | @Selector@ for @activityIdentifier@
activityIdentifierSelector :: Selector '[] (Id NSString)
activityIdentifierSelector = mkSelector "activityIdentifier"

-- | @Selector@ for @activityProperties@
activityPropertiesSelector :: Selector '[] (Id NSDictionary)
activityPropertiesSelector = mkSelector "activityProperties"

-- | @Selector@ for @isHidden@
isHiddenSelector :: Selector '[] Bool
isHiddenSelector = mkSelector "isHidden"

-- | @Selector@ for @category@
categorySelector :: Selector '[] (Id NSString)
categorySelector = mkSelector "category"

-- | @Selector@ for @setCategory:@
setCategorySelector :: Selector '[Id NSString] ()
setCategorySelector = mkSelector "setCategory:"

-- | @Selector@ for @timeScope@
timeScopeSelector :: Selector '[] GKLeaderboardTimeScope
timeScopeSelector = mkSelector "timeScope"

-- | @Selector@ for @setTimeScope:@
setTimeScopeSelector :: Selector '[GKLeaderboardTimeScope] ()
setTimeScopeSelector = mkSelector "setTimeScope:"

-- | @Selector@ for @playerScope@
playerScopeSelector :: Selector '[] GKLeaderboardPlayerScope
playerScopeSelector = mkSelector "playerScope"

-- | @Selector@ for @setPlayerScope:@
setPlayerScopeSelector :: Selector '[GKLeaderboardPlayerScope] ()
setPlayerScopeSelector = mkSelector "setPlayerScope:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @range@
rangeSelector :: Selector '[] NSRange
rangeSelector = mkSelector "range"

-- | @Selector@ for @setRange:@
setRangeSelector :: Selector '[NSRange] ()
setRangeSelector = mkSelector "setRange:"

-- | @Selector@ for @scores@
scoresSelector :: Selector '[] (Id NSArray)
scoresSelector = mkSelector "scores"

-- | @Selector@ for @maxRange@
maxRangeSelector :: Selector '[] CULong
maxRangeSelector = mkSelector "maxRange"

-- | @Selector@ for @localPlayerScore@
localPlayerScoreSelector :: Selector '[] (Id GKScore)
localPlayerScoreSelector = mkSelector "localPlayerScore"

-- | @Selector@ for @loading@
loadingSelector :: Selector '[] Bool
loadingSelector = mkSelector "loading"

