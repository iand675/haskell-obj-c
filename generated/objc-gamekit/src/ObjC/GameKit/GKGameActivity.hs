{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a single instance of a game activity for the current game.
--
-- Generated bindings for @GKGameActivity@.
module ObjC.GameKit.GKGameActivity
  ( GKGameActivity
  , IsGKGameActivity(..)
  , init_
  , startWithDefinition_partyCode_error
  , startWithDefinition_error
  , isValidPartyCode
  , initWithDefinition
  , start
  , pause
  , resume
  , end
  , setScoreOnLeaderboard_toScore_context
  , setScoreOnLeaderboard_toScore
  , getScoreOnLeaderboard
  , removeScoresFromLeaderboards
  , setProgressOnAchievement_toPercentComplete
  , setAchievementCompleted
  , getProgressOnAchievement
  , removeAchievements
  , checkPendingGameActivityExistenceWithCompletionHandler
  , makeMatchRequest
  , findMatchWithCompletionHandler
  , identifier
  , activityDefinition
  , properties
  , setProperties
  , state
  , partyCode
  , partyURL
  , creationDate
  , startDate
  , lastResumeDate
  , endDate
  , duration
  , achievements
  , leaderboardScores
  , validPartyCodeAlphabet
  , initSelector
  , startWithDefinition_partyCode_errorSelector
  , startWithDefinition_errorSelector
  , isValidPartyCodeSelector
  , initWithDefinitionSelector
  , startSelector
  , pauseSelector
  , resumeSelector
  , endSelector
  , setScoreOnLeaderboard_toScore_contextSelector
  , setScoreOnLeaderboard_toScoreSelector
  , getScoreOnLeaderboardSelector
  , removeScoresFromLeaderboardsSelector
  , setProgressOnAchievement_toPercentCompleteSelector
  , setAchievementCompletedSelector
  , getProgressOnAchievementSelector
  , removeAchievementsSelector
  , checkPendingGameActivityExistenceWithCompletionHandlerSelector
  , makeMatchRequestSelector
  , findMatchWithCompletionHandlerSelector
  , identifierSelector
  , activityDefinitionSelector
  , propertiesSelector
  , setPropertiesSelector
  , stateSelector
  , partyCodeSelector
  , partyURLSelector
  , creationDateSelector
  , startDateSelector
  , lastResumeDateSelector
  , endDateSelector
  , durationSelector
  , achievementsSelector
  , leaderboardScoresSelector
  , validPartyCodeAlphabetSelector

  -- * Enum types
  , GKGameActivityState(GKGameActivityState)
  , pattern GKGameActivityStateInitialized
  , pattern GKGameActivityStateActive
  , pattern GKGameActivityStatePaused
  , pattern GKGameActivityStateEnded

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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO (Id GKGameActivity)
init_ gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates and starts a new game activity with a custom party code.
--
-- The framework converts the party code to uppercase.
--
-- ObjC selector: @+ startWithDefinition:partyCode:error:@
startWithDefinition_partyCode_error :: (IsGKGameActivityDefinition activityDefinition, IsNSString partyCode, IsNSError error_) => activityDefinition -> partyCode -> error_ -> IO (Id GKGameActivity)
startWithDefinition_partyCode_error activityDefinition partyCode error_ =
  do
    cls' <- getRequiredClass "GKGameActivity"
    withObjCPtr activityDefinition $ \raw_activityDefinition ->
      withObjCPtr partyCode $ \raw_partyCode ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "startWithDefinition:partyCode:error:") (retPtr retVoid) [argPtr (castPtr raw_activityDefinition :: Ptr ()), argPtr (castPtr raw_partyCode :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Creates and starts a game activity with a definition.
--
-- ObjC selector: @+ startWithDefinition:error:@
startWithDefinition_error :: (IsGKGameActivityDefinition activityDefinition, IsNSError error_) => activityDefinition -> error_ -> IO (Id GKGameActivity)
startWithDefinition_error activityDefinition error_ =
  do
    cls' <- getRequiredClass "GKGameActivity"
    withObjCPtr activityDefinition $ \raw_activityDefinition ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "startWithDefinition:error:") (retPtr retVoid) [argPtr (castPtr raw_activityDefinition :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Checks whether a party code is in valid format.
--
-- Party code should be two parts of strings with the same length (2-6) connected with a dash, and the code can be either pure digits (0-9), or both parts are uppercased characters from ``GKGameActivity/validPartyCodeAlphabet``.
--
-- ObjC selector: @+ isValidPartyCode:@
isValidPartyCode :: IsNSString partyCode => partyCode -> IO Bool
isValidPartyCode partyCode =
  do
    cls' <- getRequiredClass "GKGameActivity"
    withObjCPtr partyCode $ \raw_partyCode ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isValidPartyCode:") retCULong [argPtr (castPtr raw_partyCode :: Ptr ())]

-- | Creates a game activity with definition.
--
-- ObjC selector: @- initWithDefinition:@
initWithDefinition :: (IsGKGameActivity gkGameActivity, IsGKGameActivityDefinition activityDefinition) => gkGameActivity -> activityDefinition -> IO (Id GKGameActivity)
initWithDefinition gkGameActivity  activityDefinition =
withObjCPtr activityDefinition $ \raw_activityDefinition ->
    sendMsg gkGameActivity (mkSelector "initWithDefinition:") (retPtr retVoid) [argPtr (castPtr raw_activityDefinition :: Ptr ())] >>= ownedObject . castPtr

-- | Starts the game activity if it's not already started.
--
-- ObjC selector: @- start@
start :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO ()
start gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "start") retVoid []

-- | Pauses the game activity if it's not already paused.
--
-- ObjC selector: @- pause@
pause :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO ()
pause gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "pause") retVoid []

-- | Resumes the game activity if it was paused.
--
-- ObjC selector: @- resume@
resume :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO ()
resume gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "resume") retVoid []

-- | Ends the game activity if it's not already ended.
--
-- This reports all associated achievements and submit scores to leaderboards.
--
-- ObjC selector: @- end@
end :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO ()
end gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "end") retVoid []

-- | Set a score of a leaderboard with a context for a player.
--
-- The framewwork submits the score to the leaderboard when the activity ends.
--
-- ObjC selector: @- setScoreOnLeaderboard:toScore:context:@
setScoreOnLeaderboard_toScore_context :: (IsGKGameActivity gkGameActivity, IsGKLeaderboard leaderboard) => gkGameActivity -> leaderboard -> CLong -> CULong -> IO ()
setScoreOnLeaderboard_toScore_context gkGameActivity  leaderboard score context =
withObjCPtr leaderboard $ \raw_leaderboard ->
    sendMsg gkGameActivity (mkSelector "setScoreOnLeaderboard:toScore:context:") retVoid [argPtr (castPtr raw_leaderboard :: Ptr ()), argCLong (fromIntegral score), argCULong (fromIntegral context)]

-- | Set a score of a leaderboard for a player.
--
-- The framewowrk submits the score to the leaderboard when the activity ends.
--
-- ObjC selector: @- setScoreOnLeaderboard:toScore:@
setScoreOnLeaderboard_toScore :: (IsGKGameActivity gkGameActivity, IsGKLeaderboard leaderboard) => gkGameActivity -> leaderboard -> CLong -> IO ()
setScoreOnLeaderboard_toScore gkGameActivity  leaderboard score =
withObjCPtr leaderboard $ \raw_leaderboard ->
    sendMsg gkGameActivity (mkSelector "setScoreOnLeaderboard:toScore:") retVoid [argPtr (castPtr raw_leaderboard :: Ptr ()), argCLong (fromIntegral score)]

-- | Get the leaderboard score from a specific leaderboard of the local player if previously set.
--
-- ObjC selector: @- getScoreOnLeaderboard:@
getScoreOnLeaderboard :: (IsGKGameActivity gkGameActivity, IsGKLeaderboard leaderboard) => gkGameActivity -> leaderboard -> IO (Id GKLeaderboardScore)
getScoreOnLeaderboard gkGameActivity  leaderboard =
withObjCPtr leaderboard $ \raw_leaderboard ->
    sendMsg gkGameActivity (mkSelector "getScoreOnLeaderboard:") (retPtr retVoid) [argPtr (castPtr raw_leaderboard :: Ptr ())] >>= retainedObject . castPtr

-- | Removes all scores from leaderboards for a player if exist.
--
-- ObjC selector: @- removeScoresFromLeaderboards:@
removeScoresFromLeaderboards :: (IsGKGameActivity gkGameActivity, IsNSArray leaderboards) => gkGameActivity -> leaderboards -> IO ()
removeScoresFromLeaderboards gkGameActivity  leaderboards =
withObjCPtr leaderboards $ \raw_leaderboards ->
    sendMsg gkGameActivity (mkSelector "removeScoresFromLeaderboards:") retVoid [argPtr (castPtr raw_leaderboards :: Ptr ())]

-- | Set a progress for an achievement for a player.
--
-- The framework reports achievement progress when the activity ends.
--
-- ObjC selector: @- setProgressOnAchievement:toPercentComplete:@
setProgressOnAchievement_toPercentComplete :: (IsGKGameActivity gkGameActivity, IsGKAchievement achievement) => gkGameActivity -> achievement -> CDouble -> IO ()
setProgressOnAchievement_toPercentComplete gkGameActivity  achievement percentComplete =
withObjCPtr achievement $ \raw_achievement ->
    sendMsg gkGameActivity (mkSelector "setProgressOnAchievement:toPercentComplete:") retVoid [argPtr (castPtr raw_achievement :: Ptr ()), argCDouble (fromIntegral percentComplete)]

-- | Set progress to 100% for an achievement for a player.
--
-- The system reports achievement completion when the activity ends.
--
-- ObjC selector: @- setAchievementCompleted:@
setAchievementCompleted :: (IsGKGameActivity gkGameActivity, IsGKAchievement achievement) => gkGameActivity -> achievement -> IO ()
setAchievementCompleted gkGameActivity  achievement =
withObjCPtr achievement $ \raw_achievement ->
    sendMsg gkGameActivity (mkSelector "setAchievementCompleted:") retVoid [argPtr (castPtr raw_achievement :: Ptr ())]

-- | Get the achievement progress from a specific achievement of the local player if previously set.
--
-- Returns @0@ if the achievement hasn't been set in the current activity.
--
-- ObjC selector: @- getProgressOnAchievement:@
getProgressOnAchievement :: (IsGKGameActivity gkGameActivity, IsGKAchievement achievement) => gkGameActivity -> achievement -> IO CDouble
getProgressOnAchievement gkGameActivity  achievement =
withObjCPtr achievement $ \raw_achievement ->
    sendMsg gkGameActivity (mkSelector "getProgressOnAchievement:") retCDouble [argPtr (castPtr raw_achievement :: Ptr ())]

-- | Removes all achievements if they exist.
--
-- ObjC selector: @- removeAchievements:@
removeAchievements :: (IsGKGameActivity gkGameActivity, IsNSArray achievements) => gkGameActivity -> achievements -> IO ()
removeAchievements gkGameActivity  achievements =
withObjCPtr achievements $ \raw_achievements ->
    sendMsg gkGameActivity (mkSelector "removeAchievements:") retVoid [argPtr (castPtr raw_achievements :: Ptr ())]

-- | Checks whether there is a pending activity to handle for the current game.
--
-- You can call this method before you initialize Game Center to avoid activating the system banner or welcome experience.
--
-- ObjC selector: @+ checkPendingGameActivityExistenceWithCompletionHandler:@
checkPendingGameActivityExistenceWithCompletionHandler :: Ptr () -> IO ()
checkPendingGameActivityExistenceWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "GKGameActivity"
    sendClassMsg cls' (mkSelector "checkPendingGameActivityExistenceWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Makes a match request object with information from the activity, which you can use to find matches for the local player.
--
-- ObjC selector: @- makeMatchRequest@
makeMatchRequest :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO (Id GKMatchRequest)
makeMatchRequest gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "makeMatchRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Use information from the activity to find matches for the local player.
--
-- GameKit creates a classic match making request with the activity's party code and other information, and returns the match object in the completion handler or any error that occurred. An error occurs if this activity doesn't support party code, or has an unsupported range of players, which is used to be configured as match request's @minPlayers@ and @maxPlayers@.
--
-- ObjC selector: @- findMatchWithCompletionHandler:@
findMatchWithCompletionHandler :: IsGKGameActivity gkGameActivity => gkGameActivity -> Ptr () -> IO ()
findMatchWithCompletionHandler gkGameActivity  completionHandler =
  sendMsg gkGameActivity (mkSelector "findMatchWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | The identifier of this activity instance.
--
-- ObjC selector: @- identifier@
identifier :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO (Id NSString)
identifier gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The activity definition that this activity instance is based on.
--
-- ObjC selector: @- activityDefinition@
activityDefinition :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO (Id GKGameActivityDefinition)
activityDefinition gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "activityDefinition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Properties that contain additional information about the activity.
--
-- This takes precedence over ``GKGameActivityDefinition/defaultProperties`` on the activity definition.
--
-- 1. The framework initializes this dictionary with the default properties from the activity definition and deep linked properties, if any. 2. If deep linking contains the same key as the default properties, the deep linked value overrides the default value. 3. You can update the properties at runtime.
--
-- ObjC selector: @- properties@
properties :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO (Id NSDictionary)
properties gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "properties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Properties that contain additional information about the activity.
--
-- This takes precedence over ``GKGameActivityDefinition/defaultProperties`` on the activity definition.
--
-- 1. The framework initializes this dictionary with the default properties from the activity definition and deep linked properties, if any. 2. If deep linking contains the same key as the default properties, the deep linked value overrides the default value. 3. You can update the properties at runtime.
--
-- ObjC selector: @- setProperties:@
setProperties :: (IsGKGameActivity gkGameActivity, IsNSDictionary value) => gkGameActivity -> value -> IO ()
setProperties gkGameActivity  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkGameActivity (mkSelector "setProperties:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The state of the game activity.
--
-- ObjC selector: @- state@
state :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO GKGameActivityState
state gkGameActivity  =
  fmap (coerce :: CULong -> GKGameActivityState) $ sendMsg gkGameActivity (mkSelector "state") retCULong []

-- | If the game supports party code, this is the party code that can be shared among players to join the party.
--
-- If the game doesn't support party code, this value will be @nil@. Use ``GKGameActivity/start(definition:partyCode:)`` to create a game activity with a custom party code.
--
-- ObjC selector: @- partyCode@
partyCode :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO (Id NSString)
partyCode gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "partyCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If the game supports party code, this is the URL that can be shared among players to join the party.
--
-- ObjC selector: @- partyURL@
partyURL :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO (Id NSURL)
partyURL gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "partyURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date when the activity was created.
--
-- ObjC selector: @- creationDate@
creationDate :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO (Id NSDate)
creationDate gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "creationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date when the activity was initially started.
--
-- ObjC selector: @- startDate@
startDate :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO (Id NSDate)
startDate gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date when the activity was last resumed.
--
-- - If the activity was first started, this will be the same as the start date. - If the activity was paused and resumed, this will be the date when the activity was resumed.
--
-- ObjC selector: @- lastResumeDate@
lastResumeDate :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO (Id NSDate)
lastResumeDate gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "lastResumeDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date when the activity was officially ended.
--
-- ObjC selector: @- endDate@
endDate :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO (Id NSDate)
endDate gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The total time elapsed while in active state.
--
-- ObjC selector: @- duration@
duration :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO CDouble
duration gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "duration") retCDouble []

-- | All achievements that have been associated with this activity.
--
-- Progress of each achievement will be reported when the activity ends.
--
-- ObjC selector: @- achievements@
achievements :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO (Id NSSet)
achievements gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "achievements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | All leaderboard scores that have been associated with this activity.
--
-- Scores will be submitted to the leaderboards when the activity ends.
--
-- ObjC selector: @- leaderboardScores@
leaderboardScores :: IsGKGameActivity gkGameActivity => gkGameActivity -> IO (Id NSSet)
leaderboardScores gkGameActivity  =
  sendMsg gkGameActivity (mkSelector "leaderboardScores") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Allowed characters for the party code to be used to share this activity.
--
-- ObjC selector: @+ validPartyCodeAlphabet@
validPartyCodeAlphabet :: IO (Id NSArray)
validPartyCodeAlphabet  =
  do
    cls' <- getRequiredClass "GKGameActivity"
    sendClassMsg cls' (mkSelector "validPartyCodeAlphabet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @startWithDefinition:partyCode:error:@
startWithDefinition_partyCode_errorSelector :: Selector
startWithDefinition_partyCode_errorSelector = mkSelector "startWithDefinition:partyCode:error:"

-- | @Selector@ for @startWithDefinition:error:@
startWithDefinition_errorSelector :: Selector
startWithDefinition_errorSelector = mkSelector "startWithDefinition:error:"

-- | @Selector@ for @isValidPartyCode:@
isValidPartyCodeSelector :: Selector
isValidPartyCodeSelector = mkSelector "isValidPartyCode:"

-- | @Selector@ for @initWithDefinition:@
initWithDefinitionSelector :: Selector
initWithDefinitionSelector = mkSelector "initWithDefinition:"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @pause@
pauseSelector :: Selector
pauseSelector = mkSelector "pause"

-- | @Selector@ for @resume@
resumeSelector :: Selector
resumeSelector = mkSelector "resume"

-- | @Selector@ for @end@
endSelector :: Selector
endSelector = mkSelector "end"

-- | @Selector@ for @setScoreOnLeaderboard:toScore:context:@
setScoreOnLeaderboard_toScore_contextSelector :: Selector
setScoreOnLeaderboard_toScore_contextSelector = mkSelector "setScoreOnLeaderboard:toScore:context:"

-- | @Selector@ for @setScoreOnLeaderboard:toScore:@
setScoreOnLeaderboard_toScoreSelector :: Selector
setScoreOnLeaderboard_toScoreSelector = mkSelector "setScoreOnLeaderboard:toScore:"

-- | @Selector@ for @getScoreOnLeaderboard:@
getScoreOnLeaderboardSelector :: Selector
getScoreOnLeaderboardSelector = mkSelector "getScoreOnLeaderboard:"

-- | @Selector@ for @removeScoresFromLeaderboards:@
removeScoresFromLeaderboardsSelector :: Selector
removeScoresFromLeaderboardsSelector = mkSelector "removeScoresFromLeaderboards:"

-- | @Selector@ for @setProgressOnAchievement:toPercentComplete:@
setProgressOnAchievement_toPercentCompleteSelector :: Selector
setProgressOnAchievement_toPercentCompleteSelector = mkSelector "setProgressOnAchievement:toPercentComplete:"

-- | @Selector@ for @setAchievementCompleted:@
setAchievementCompletedSelector :: Selector
setAchievementCompletedSelector = mkSelector "setAchievementCompleted:"

-- | @Selector@ for @getProgressOnAchievement:@
getProgressOnAchievementSelector :: Selector
getProgressOnAchievementSelector = mkSelector "getProgressOnAchievement:"

-- | @Selector@ for @removeAchievements:@
removeAchievementsSelector :: Selector
removeAchievementsSelector = mkSelector "removeAchievements:"

-- | @Selector@ for @checkPendingGameActivityExistenceWithCompletionHandler:@
checkPendingGameActivityExistenceWithCompletionHandlerSelector :: Selector
checkPendingGameActivityExistenceWithCompletionHandlerSelector = mkSelector "checkPendingGameActivityExistenceWithCompletionHandler:"

-- | @Selector@ for @makeMatchRequest@
makeMatchRequestSelector :: Selector
makeMatchRequestSelector = mkSelector "makeMatchRequest"

-- | @Selector@ for @findMatchWithCompletionHandler:@
findMatchWithCompletionHandlerSelector :: Selector
findMatchWithCompletionHandlerSelector = mkSelector "findMatchWithCompletionHandler:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @activityDefinition@
activityDefinitionSelector :: Selector
activityDefinitionSelector = mkSelector "activityDefinition"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @setProperties:@
setPropertiesSelector :: Selector
setPropertiesSelector = mkSelector "setProperties:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @partyCode@
partyCodeSelector :: Selector
partyCodeSelector = mkSelector "partyCode"

-- | @Selector@ for @partyURL@
partyURLSelector :: Selector
partyURLSelector = mkSelector "partyURL"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @lastResumeDate@
lastResumeDateSelector :: Selector
lastResumeDateSelector = mkSelector "lastResumeDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @achievements@
achievementsSelector :: Selector
achievementsSelector = mkSelector "achievements"

-- | @Selector@ for @leaderboardScores@
leaderboardScoresSelector :: Selector
leaderboardScoresSelector = mkSelector "leaderboardScores"

-- | @Selector@ for @validPartyCodeAlphabet@
validPartyCodeAlphabetSelector :: Selector
validPartyCodeAlphabetSelector = mkSelector "validPartyCodeAlphabet"

