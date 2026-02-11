{-# LANGUAGE PatternSynonyms #-}
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
  , loadPreviousOccurrenceWithCompletionHandlerSelector
  , submitScore_context_player_leaderboardIDs_completionHandlerSelector
  , submitScore_context_player_completionHandlerSelector
  , loadImageWithCompletionHandlerSelector
  , initWithPlayerIDsSelector
  , setDefaultLeaderboard_withCompletionHandlerSelector
  , initSelector
  , initWithPlayersSelector
  , titleSelector
  , groupIdentifierSelector
  , baseLeaderboardIDSelector
  , typeSelector
  , startDateSelector
  , nextStartDateSelector
  , durationSelector
  , leaderboardDescriptionSelector
  , releaseStateSelector
  , activityIdentifierSelector
  , activityPropertiesSelector
  , isHiddenSelector
  , categorySelector
  , setCategorySelector
  , timeScopeSelector
  , setTimeScopeSelector
  , playerScopeSelector
  , setPlayerScopeSelector
  , identifierSelector
  , setIdentifierSelector
  , rangeSelector
  , setRangeSelector
  , scoresSelector
  , maxRangeSelector
  , localPlayerScoreSelector
  , loadingSelector

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
import ObjC.Foundation.Internal.Classes

-- | Loads the occurrence preceding this occurrence for a recurring leaderboard in which the local player submitted a score. If no previous occurrence is found that the player submitted a score to, then the most recent previous occurrence is returned.
--
-- ObjC selector: @- loadPreviousOccurrenceWithCompletionHandler:@
loadPreviousOccurrenceWithCompletionHandler :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> Ptr () -> IO ()
loadPreviousOccurrenceWithCompletionHandler gkLeaderboard  completionHandler =
    sendMsg gkLeaderboard (mkSelector "loadPreviousOccurrenceWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Class method to submit a single score to multiple leaderboards   score - earned by the player   context - developer supplied metadata associated with the player's score   player - the player for whom this score is being submitted   leaderboardIDs - one or more leaderboard IDs defined in App Store Connect
--
-- ObjC selector: @+ submitScore:context:player:leaderboardIDs:completionHandler:@
submitScore_context_player_leaderboardIDs_completionHandler :: (IsGKPlayer player, IsNSArray leaderboardIDs) => CLong -> CULong -> player -> leaderboardIDs -> Ptr () -> IO ()
submitScore_context_player_leaderboardIDs_completionHandler score context player leaderboardIDs completionHandler =
  do
    cls' <- getRequiredClass "GKLeaderboard"
    withObjCPtr player $ \raw_player ->
      withObjCPtr leaderboardIDs $ \raw_leaderboardIDs ->
        sendClassMsg cls' (mkSelector "submitScore:context:player:leaderboardIDs:completionHandler:") retVoid [argCLong score, argCULong context, argPtr (castPtr raw_player :: Ptr ()), argPtr (castPtr raw_leaderboardIDs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Instance method to submit a single score to the leaderboard associated with this instance   score - earned by the player   context - developer supplied metadata associated with the player's score   player - the player for whom this score is being submitted
--
-- ObjC selector: @- submitScore:context:player:completionHandler:@
submitScore_context_player_completionHandler :: (IsGKLeaderboard gkLeaderboard, IsGKPlayer player) => gkLeaderboard -> CLong -> CULong -> player -> Ptr () -> IO ()
submitScore_context_player_completionHandler gkLeaderboard  score context player completionHandler =
  withObjCPtr player $ \raw_player ->
      sendMsg gkLeaderboard (mkSelector "submitScore:context:player:completionHandler:") retVoid [argCLong score, argCULong context, argPtr (castPtr raw_player :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Asynchronously load the image. Error will be nil on success.
--
-- ObjC selector: @- loadImageWithCompletionHandler:@
loadImageWithCompletionHandler :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> Ptr () -> IO ()
loadImageWithCompletionHandler gkLeaderboard  completionHandler =
    sendMsg gkLeaderboard (mkSelector "loadImageWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- initWithPlayerIDs:@
initWithPlayerIDs :: (IsGKLeaderboard gkLeaderboard, IsNSArray playerIDs) => gkLeaderboard -> playerIDs -> IO (Id GKLeaderboard)
initWithPlayerIDs gkLeaderboard  playerIDs =
  withObjCPtr playerIDs $ \raw_playerIDs ->
      sendMsg gkLeaderboard (mkSelector "initWithPlayerIDs:") (retPtr retVoid) [argPtr (castPtr raw_playerIDs :: Ptr ())] >>= ownedObject . castPtr

-- | @+ setDefaultLeaderboard:withCompletionHandler:@
setDefaultLeaderboard_withCompletionHandler :: IsNSString leaderboardIdentifier => leaderboardIdentifier -> Ptr () -> IO ()
setDefaultLeaderboard_withCompletionHandler leaderboardIdentifier completionHandler =
  do
    cls' <- getRequiredClass "GKLeaderboard"
    withObjCPtr leaderboardIdentifier $ \raw_leaderboardIdentifier ->
      sendClassMsg cls' (mkSelector "setDefaultLeaderboard:withCompletionHandler:") retVoid [argPtr (castPtr raw_leaderboardIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Default is the range 1-10 with Global/AllTime scopes. If you want to change the scopes or range, set the properites before loading the scores.
--
-- ObjC selector: @- init@
init_ :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id GKLeaderboard)
init_ gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Specify an array of GKPlayers. For example, the players who are in a match together Defaults to AllTime score, if you want to change the timeScope, set the property before loading the scores. Range and playerScope are not applicable. players may not be nil.
--
-- ObjC selector: @- initWithPlayers:@
initWithPlayers :: (IsGKLeaderboard gkLeaderboard, IsNSArray players) => gkLeaderboard -> players -> IO (Id GKLeaderboard)
initWithPlayers gkLeaderboard  players =
  withObjCPtr players $ \raw_players ->
      sendMsg gkLeaderboard (mkSelector "initWithPlayers:") (retPtr retVoid) [argPtr (castPtr raw_players :: Ptr ())] >>= ownedObject . castPtr

-- | Localized title
--
-- ObjC selector: @- title@
title :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
title gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | set when leaderboards have been designated a game group; set when loadLeaderboardsWithCompletionHandler has been called for leaderboards that support game groups
--
-- ObjC selector: @- groupIdentifier@
groupIdentifier :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
groupIdentifier gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "groupIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Leaderboard ID defined in App Store Connect that this instance is associated with
--
-- ObjC selector: @- baseLeaderboardID@
baseLeaderboardID :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
baseLeaderboardID gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "baseLeaderboardID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Type of leaderboard
--
-- ObjC selector: @- type@
type_ :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO GKLeaderboardType
type_ gkLeaderboard  =
    fmap (coerce :: CLong -> GKLeaderboardType) $ sendMsg gkLeaderboard (mkSelector "type") retCLong []

-- | Date and time this instance started accepting score submissions (only applicable to recurring leaderboards)
--
-- ObjC selector: @- startDate@
startDate :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSDate)
startDate gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Date and time the next instance will start accepting score submissions (only applicable to recurring leaderboards)
--
-- ObjC selector: @- nextStartDate@
nextStartDate :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSDate)
nextStartDate gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "nextStartDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Duration from startDate during which this leaderboard instance accepts score submissions (only applicable to recurring leaderboards)
--
-- ObjC selector: @- duration@
duration :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO CDouble
duration gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "duration") retCDouble []

-- | The description of this Leaderboard as configured by the developer in App Store Connect.
--
-- ObjC selector: @- leaderboardDescription@
leaderboardDescription :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
leaderboardDescription gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "leaderboardDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The release state of the leaderboard in App Store Connect.
--
-- ObjC selector: @- releaseState@
releaseState :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO GKReleaseState
releaseState gkLeaderboard  =
    fmap (coerce :: CULong -> GKReleaseState) $ sendMsg gkLeaderboard (mkSelector "releaseState") retCULong []

-- | The identifier of the game activity associated with this leaderboard, as configured by the developer in App Store Connect.
--
-- ObjC selector: @- activityIdentifier@
activityIdentifier :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
activityIdentifier gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "activityIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The properties when associating this leaderboard with a game activity, as configured by the developer in App Store Connect.
--
-- ObjC selector: @- activityProperties@
activityProperties :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSDictionary)
activityProperties gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "activityProperties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value that indicates whether the current leaderboard isn't visible in Game Center views.
--
-- You can still submit scores to a hidden leaderboard.
--
-- ObjC selector: @- isHidden@
isHidden :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO Bool
isHidden gkLeaderboard  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkLeaderboard (mkSelector "isHidden") retCULong []

-- | @- category@
category :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
category gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "category") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCategory:@
setCategory :: (IsGKLeaderboard gkLeaderboard, IsNSString value) => gkLeaderboard -> value -> IO ()
setCategory gkLeaderboard  value =
  withObjCPtr value $ \raw_value ->
      sendMsg gkLeaderboard (mkSelector "setCategory:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeScope@
timeScope :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO GKLeaderboardTimeScope
timeScope gkLeaderboard  =
    fmap (coerce :: CLong -> GKLeaderboardTimeScope) $ sendMsg gkLeaderboard (mkSelector "timeScope") retCLong []

-- | @- setTimeScope:@
setTimeScope :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> GKLeaderboardTimeScope -> IO ()
setTimeScope gkLeaderboard  value =
    sendMsg gkLeaderboard (mkSelector "setTimeScope:") retVoid [argCLong (coerce value)]

-- | Filter on friends. Does not apply to leaderboard initialized with players.
--
-- ObjC selector: @- playerScope@
playerScope :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO GKLeaderboardPlayerScope
playerScope gkLeaderboard  =
    fmap (coerce :: CLong -> GKLeaderboardPlayerScope) $ sendMsg gkLeaderboard (mkSelector "playerScope") retCLong []

-- | Filter on friends. Does not apply to leaderboard initialized with players.
--
-- ObjC selector: @- setPlayerScope:@
setPlayerScope :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> GKLeaderboardPlayerScope -> IO ()
setPlayerScope gkLeaderboard  value =
    sendMsg gkLeaderboard (mkSelector "setPlayerScope:") retVoid [argCLong (coerce value)]

-- | leaderboardID. If nil, fetch the aggregate leaderboard.
--
-- ObjC selector: @- identifier@
identifier :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSString)
identifier gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | leaderboardID. If nil, fetch the aggregate leaderboard.
--
-- ObjC selector: @- setIdentifier:@
setIdentifier :: (IsGKLeaderboard gkLeaderboard, IsNSString value) => gkLeaderboard -> value -> IO ()
setIdentifier gkLeaderboard  value =
  withObjCPtr value $ \raw_value ->
      sendMsg gkLeaderboard (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Leaderboards start at index 1 and the length should be less than 100. Does not apply to leaderboards initialized with players.  Exception will be thrown if developer tries to set an invalid range.
--
-- ObjC selector: @- range@
range :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO NSRange
range gkLeaderboard  =
    sendMsgStret gkLeaderboard (mkSelector "range") retNSRange []

-- | Leaderboards start at index 1 and the length should be less than 100. Does not apply to leaderboards initialized with players.  Exception will be thrown if developer tries to set an invalid range.
--
-- ObjC selector: @- setRange:@
setRange :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> NSRange -> IO ()
setRange gkLeaderboard  value =
    sendMsg gkLeaderboard (mkSelector "setRange:") retVoid [argNSRange value]

-- | Scores are not valid until loadScores: has completed.
--
-- ObjC selector: @- scores@
scores :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id NSArray)
scores gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "scores") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The maxRange which represents the size of the leaderboard is not valid until loadScores: has completed.
--
-- ObjC selector: @- maxRange@
maxRange :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO CULong
maxRange gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "maxRange") retCULong []

-- | The local player's score
--
-- ObjC selector: @- localPlayerScore@
localPlayerScore :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO (Id GKScore)
localPlayerScore gkLeaderboard  =
    sendMsg gkLeaderboard (mkSelector "localPlayerScore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This property is true if the leaderboard is currently loading
--
-- ObjC selector: @- loading@
loading :: IsGKLeaderboard gkLeaderboard => gkLeaderboard -> IO Bool
loading gkLeaderboard  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkLeaderboard (mkSelector "loading") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadPreviousOccurrenceWithCompletionHandler:@
loadPreviousOccurrenceWithCompletionHandlerSelector :: Selector
loadPreviousOccurrenceWithCompletionHandlerSelector = mkSelector "loadPreviousOccurrenceWithCompletionHandler:"

-- | @Selector@ for @submitScore:context:player:leaderboardIDs:completionHandler:@
submitScore_context_player_leaderboardIDs_completionHandlerSelector :: Selector
submitScore_context_player_leaderboardIDs_completionHandlerSelector = mkSelector "submitScore:context:player:leaderboardIDs:completionHandler:"

-- | @Selector@ for @submitScore:context:player:completionHandler:@
submitScore_context_player_completionHandlerSelector :: Selector
submitScore_context_player_completionHandlerSelector = mkSelector "submitScore:context:player:completionHandler:"

-- | @Selector@ for @loadImageWithCompletionHandler:@
loadImageWithCompletionHandlerSelector :: Selector
loadImageWithCompletionHandlerSelector = mkSelector "loadImageWithCompletionHandler:"

-- | @Selector@ for @initWithPlayerIDs:@
initWithPlayerIDsSelector :: Selector
initWithPlayerIDsSelector = mkSelector "initWithPlayerIDs:"

-- | @Selector@ for @setDefaultLeaderboard:withCompletionHandler:@
setDefaultLeaderboard_withCompletionHandlerSelector :: Selector
setDefaultLeaderboard_withCompletionHandlerSelector = mkSelector "setDefaultLeaderboard:withCompletionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPlayers:@
initWithPlayersSelector :: Selector
initWithPlayersSelector = mkSelector "initWithPlayers:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @groupIdentifier@
groupIdentifierSelector :: Selector
groupIdentifierSelector = mkSelector "groupIdentifier"

-- | @Selector@ for @baseLeaderboardID@
baseLeaderboardIDSelector :: Selector
baseLeaderboardIDSelector = mkSelector "baseLeaderboardID"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @nextStartDate@
nextStartDateSelector :: Selector
nextStartDateSelector = mkSelector "nextStartDate"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @leaderboardDescription@
leaderboardDescriptionSelector :: Selector
leaderboardDescriptionSelector = mkSelector "leaderboardDescription"

-- | @Selector@ for @releaseState@
releaseStateSelector :: Selector
releaseStateSelector = mkSelector "releaseState"

-- | @Selector@ for @activityIdentifier@
activityIdentifierSelector :: Selector
activityIdentifierSelector = mkSelector "activityIdentifier"

-- | @Selector@ for @activityProperties@
activityPropertiesSelector :: Selector
activityPropertiesSelector = mkSelector "activityProperties"

-- | @Selector@ for @isHidden@
isHiddenSelector :: Selector
isHiddenSelector = mkSelector "isHidden"

-- | @Selector@ for @category@
categorySelector :: Selector
categorySelector = mkSelector "category"

-- | @Selector@ for @setCategory:@
setCategorySelector :: Selector
setCategorySelector = mkSelector "setCategory:"

-- | @Selector@ for @timeScope@
timeScopeSelector :: Selector
timeScopeSelector = mkSelector "timeScope"

-- | @Selector@ for @setTimeScope:@
setTimeScopeSelector :: Selector
setTimeScopeSelector = mkSelector "setTimeScope:"

-- | @Selector@ for @playerScope@
playerScopeSelector :: Selector
playerScopeSelector = mkSelector "playerScope"

-- | @Selector@ for @setPlayerScope:@
setPlayerScopeSelector :: Selector
setPlayerScopeSelector = mkSelector "setPlayerScope:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @range@
rangeSelector :: Selector
rangeSelector = mkSelector "range"

-- | @Selector@ for @setRange:@
setRangeSelector :: Selector
setRangeSelector = mkSelector "setRange:"

-- | @Selector@ for @scores@
scoresSelector :: Selector
scoresSelector = mkSelector "scores"

-- | @Selector@ for @maxRange@
maxRangeSelector :: Selector
maxRangeSelector = mkSelector "maxRange"

-- | @Selector@ for @localPlayerScore@
localPlayerScoreSelector :: Selector
localPlayerScoreSelector = mkSelector "localPlayerScore"

-- | @Selector@ for @loading@
loadingSelector :: Selector
loadingSelector = mkSelector "loading"

