{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKTurnBasedMatch@.
module ObjC.GameKit.GKTurnBasedMatch
  ( GKTurnBasedMatch
  , IsGKTurnBasedMatch(..)
  , setLocalizableMessageWithKey_arguments
  , findMatchForRequest_withCompletionHandler
  , loadMatchWithID_withCompletionHandler
  , rematchWithCompletionHandler
  , acceptInviteWithCompletionHandler
  , declineInviteWithCompletionHandler
  , removeWithCompletionHandler
  , loadMatchDataWithCompletionHandler
  , endTurnWithNextParticipants_turnTimeout_matchData_completionHandler
  , participantQuitInTurnWithOutcome_nextParticipants_turnTimeout_matchData_completionHandler
  , participantQuitOutOfTurnWithOutcome_withCompletionHandler
  , endMatchInTurnWithMatchData_completionHandler
  , endMatchInTurnWithMatchData_scores_achievements_completionHandler
  , endMatchInTurnWithMatchData_leaderboardScores_achievements_completionHandler
  , saveCurrentTurnWithMatchData_completionHandler
  , saveMergedMatchData_withResolvedExchanges_completionHandler
  , sendExchangeToParticipants_data_localizableMessageKey_arguments_timeout_completionHandler
  , sendReminderToParticipants_localizableMessageKey_arguments_completionHandler
  , endTurnWithNextParticipant_matchData_completionHandler
  , participantQuitInTurnWithOutcome_nextParticipant_matchData_completionHandler
  , matchID
  , creationDate
  , participants
  , status
  , currentParticipant
  , matchData
  , message
  , setMessage
  , matchDataMaximumSize
  , exchanges
  , activeExchanges
  , completedExchanges
  , exchangeDataMaximumSize
  , exchangeMaxInitiatedExchangesPerPlayer
  , setLocalizableMessageWithKey_argumentsSelector
  , findMatchForRequest_withCompletionHandlerSelector
  , loadMatchWithID_withCompletionHandlerSelector
  , rematchWithCompletionHandlerSelector
  , acceptInviteWithCompletionHandlerSelector
  , declineInviteWithCompletionHandlerSelector
  , removeWithCompletionHandlerSelector
  , loadMatchDataWithCompletionHandlerSelector
  , endTurnWithNextParticipants_turnTimeout_matchData_completionHandlerSelector
  , participantQuitInTurnWithOutcome_nextParticipants_turnTimeout_matchData_completionHandlerSelector
  , participantQuitOutOfTurnWithOutcome_withCompletionHandlerSelector
  , endMatchInTurnWithMatchData_completionHandlerSelector
  , endMatchInTurnWithMatchData_scores_achievements_completionHandlerSelector
  , endMatchInTurnWithMatchData_leaderboardScores_achievements_completionHandlerSelector
  , saveCurrentTurnWithMatchData_completionHandlerSelector
  , saveMergedMatchData_withResolvedExchanges_completionHandlerSelector
  , sendExchangeToParticipants_data_localizableMessageKey_arguments_timeout_completionHandlerSelector
  , sendReminderToParticipants_localizableMessageKey_arguments_completionHandlerSelector
  , endTurnWithNextParticipant_matchData_completionHandlerSelector
  , participantQuitInTurnWithOutcome_nextParticipant_matchData_completionHandlerSelector
  , matchIDSelector
  , creationDateSelector
  , participantsSelector
  , statusSelector
  , currentParticipantSelector
  , matchDataSelector
  , messageSelector
  , setMessageSelector
  , matchDataMaximumSizeSelector
  , exchangesSelector
  , activeExchangesSelector
  , completedExchangesSelector
  , exchangeDataMaximumSizeSelector
  , exchangeMaxInitiatedExchangesPerPlayerSelector

  -- * Enum types
  , GKTurnBasedMatchOutcome(GKTurnBasedMatchOutcome)
  , pattern GKTurnBasedMatchOutcomeNone
  , pattern GKTurnBasedMatchOutcomeQuit
  , pattern GKTurnBasedMatchOutcomeWon
  , pattern GKTurnBasedMatchOutcomeLost
  , pattern GKTurnBasedMatchOutcomeTied
  , pattern GKTurnBasedMatchOutcomeTimeExpired
  , pattern GKTurnBasedMatchOutcomeFirst
  , pattern GKTurnBasedMatchOutcomeSecond
  , pattern GKTurnBasedMatchOutcomeThird
  , pattern GKTurnBasedMatchOutcomeFourth
  , pattern GKTurnBasedMatchOutcomeCustomRange
  , GKTurnBasedMatchStatus(GKTurnBasedMatchStatus)
  , pattern GKTurnBasedMatchStatusUnknown
  , pattern GKTurnBasedMatchStatusOpen
  , pattern GKTurnBasedMatchStatusEnded
  , pattern GKTurnBasedMatchStatusMatching

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

-- | @- setLocalizableMessageWithKey:arguments:@
setLocalizableMessageWithKey_arguments :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSString key, IsNSArray arguments) => gkTurnBasedMatch -> key -> arguments -> IO ()
setLocalizableMessageWithKey_arguments gkTurnBasedMatch  key arguments =
withObjCPtr key $ \raw_key ->
  withObjCPtr arguments $ \raw_arguments ->
      sendMsg gkTurnBasedMatch (mkSelector "setLocalizableMessageWithKey:arguments:") retVoid [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ())]

-- | @+ findMatchForRequest:withCompletionHandler:@
findMatchForRequest_withCompletionHandler :: IsGKMatchRequest request => request -> Ptr () -> IO ()
findMatchForRequest_withCompletionHandler request completionHandler =
  do
    cls' <- getRequiredClass "GKTurnBasedMatch"
    withObjCPtr request $ \raw_request ->
      sendClassMsg cls' (mkSelector "findMatchForRequest:withCompletionHandler:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ loadMatchWithID:withCompletionHandler:@
loadMatchWithID_withCompletionHandler :: IsNSString matchID => matchID -> Ptr () -> IO ()
loadMatchWithID_withCompletionHandler matchID completionHandler =
  do
    cls' <- getRequiredClass "GKTurnBasedMatch"
    withObjCPtr matchID $ \raw_matchID ->
      sendClassMsg cls' (mkSelector "loadMatchWithID:withCompletionHandler:") retVoid [argPtr (castPtr raw_matchID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- rematchWithCompletionHandler:@
rematchWithCompletionHandler :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> Ptr () -> IO ()
rematchWithCompletionHandler gkTurnBasedMatch  completionHandler =
  sendMsg gkTurnBasedMatch (mkSelector "rematchWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- acceptInviteWithCompletionHandler:@
acceptInviteWithCompletionHandler :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> Ptr () -> IO ()
acceptInviteWithCompletionHandler gkTurnBasedMatch  completionHandler =
  sendMsg gkTurnBasedMatch (mkSelector "acceptInviteWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- declineInviteWithCompletionHandler:@
declineInviteWithCompletionHandler :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> Ptr () -> IO ()
declineInviteWithCompletionHandler gkTurnBasedMatch  completionHandler =
  sendMsg gkTurnBasedMatch (mkSelector "declineInviteWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- removeWithCompletionHandler:@
removeWithCompletionHandler :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> Ptr () -> IO ()
removeWithCompletionHandler gkTurnBasedMatch  completionHandler =
  sendMsg gkTurnBasedMatch (mkSelector "removeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- loadMatchDataWithCompletionHandler:@
loadMatchDataWithCompletionHandler :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> Ptr () -> IO ()
loadMatchDataWithCompletionHandler gkTurnBasedMatch  completionHandler =
  sendMsg gkTurnBasedMatch (mkSelector "loadMatchDataWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- endTurnWithNextParticipants:turnTimeout:matchData:completionHandler:@
endTurnWithNextParticipants_turnTimeout_matchData_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSArray nextParticipants, IsNSData matchData) => gkTurnBasedMatch -> nextParticipants -> CDouble -> matchData -> Ptr () -> IO ()
endTurnWithNextParticipants_turnTimeout_matchData_completionHandler gkTurnBasedMatch  nextParticipants timeout matchData completionHandler =
withObjCPtr nextParticipants $ \raw_nextParticipants ->
  withObjCPtr matchData $ \raw_matchData ->
      sendMsg gkTurnBasedMatch (mkSelector "endTurnWithNextParticipants:turnTimeout:matchData:completionHandler:") retVoid [argPtr (castPtr raw_nextParticipants :: Ptr ()), argCDouble (fromIntegral timeout), argPtr (castPtr raw_matchData :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- participantQuitInTurnWithOutcome:nextParticipants:turnTimeout:matchData:completionHandler:@
participantQuitInTurnWithOutcome_nextParticipants_turnTimeout_matchData_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSArray nextParticipants, IsNSData matchData) => gkTurnBasedMatch -> GKTurnBasedMatchOutcome -> nextParticipants -> CDouble -> matchData -> Ptr () -> IO ()
participantQuitInTurnWithOutcome_nextParticipants_turnTimeout_matchData_completionHandler gkTurnBasedMatch  matchOutcome nextParticipants timeout matchData completionHandler =
withObjCPtr nextParticipants $ \raw_nextParticipants ->
  withObjCPtr matchData $ \raw_matchData ->
      sendMsg gkTurnBasedMatch (mkSelector "participantQuitInTurnWithOutcome:nextParticipants:turnTimeout:matchData:completionHandler:") retVoid [argCLong (coerce matchOutcome), argPtr (castPtr raw_nextParticipants :: Ptr ()), argCDouble (fromIntegral timeout), argPtr (castPtr raw_matchData :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- participantQuitOutOfTurnWithOutcome:withCompletionHandler:@
participantQuitOutOfTurnWithOutcome_withCompletionHandler :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> GKTurnBasedMatchOutcome -> Ptr () -> IO ()
participantQuitOutOfTurnWithOutcome_withCompletionHandler gkTurnBasedMatch  matchOutcome completionHandler =
  sendMsg gkTurnBasedMatch (mkSelector "participantQuitOutOfTurnWithOutcome:withCompletionHandler:") retVoid [argCLong (coerce matchOutcome), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- endMatchInTurnWithMatchData:completionHandler:@
endMatchInTurnWithMatchData_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSData matchData) => gkTurnBasedMatch -> matchData -> Ptr () -> IO ()
endMatchInTurnWithMatchData_completionHandler gkTurnBasedMatch  matchData completionHandler =
withObjCPtr matchData $ \raw_matchData ->
    sendMsg gkTurnBasedMatch (mkSelector "endMatchInTurnWithMatchData:completionHandler:") retVoid [argPtr (castPtr raw_matchData :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- endMatchInTurnWithMatchData:scores:achievements:completionHandler:@
endMatchInTurnWithMatchData_scores_achievements_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSData matchData, IsNSArray scores, IsNSArray achievements) => gkTurnBasedMatch -> matchData -> scores -> achievements -> Ptr () -> IO ()
endMatchInTurnWithMatchData_scores_achievements_completionHandler gkTurnBasedMatch  matchData scores achievements completionHandler =
withObjCPtr matchData $ \raw_matchData ->
  withObjCPtr scores $ \raw_scores ->
    withObjCPtr achievements $ \raw_achievements ->
        sendMsg gkTurnBasedMatch (mkSelector "endMatchInTurnWithMatchData:scores:achievements:completionHandler:") retVoid [argPtr (castPtr raw_matchData :: Ptr ()), argPtr (castPtr raw_scores :: Ptr ()), argPtr (castPtr raw_achievements :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- endMatchInTurnWithMatchData:leaderboardScores:achievements:completionHandler:@
endMatchInTurnWithMatchData_leaderboardScores_achievements_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSData matchData, IsNSArray scores, IsNSArray achievements) => gkTurnBasedMatch -> matchData -> scores -> achievements -> Ptr () -> IO ()
endMatchInTurnWithMatchData_leaderboardScores_achievements_completionHandler gkTurnBasedMatch  matchData scores achievements completionHandler =
withObjCPtr matchData $ \raw_matchData ->
  withObjCPtr scores $ \raw_scores ->
    withObjCPtr achievements $ \raw_achievements ->
        sendMsg gkTurnBasedMatch (mkSelector "endMatchInTurnWithMatchData:leaderboardScores:achievements:completionHandler:") retVoid [argPtr (castPtr raw_matchData :: Ptr ()), argPtr (castPtr raw_scores :: Ptr ()), argPtr (castPtr raw_achievements :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- saveCurrentTurnWithMatchData:completionHandler:@
saveCurrentTurnWithMatchData_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSData matchData) => gkTurnBasedMatch -> matchData -> Ptr () -> IO ()
saveCurrentTurnWithMatchData_completionHandler gkTurnBasedMatch  matchData completionHandler =
withObjCPtr matchData $ \raw_matchData ->
    sendMsg gkTurnBasedMatch (mkSelector "saveCurrentTurnWithMatchData:completionHandler:") retVoid [argPtr (castPtr raw_matchData :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- saveMergedMatchData:withResolvedExchanges:completionHandler:@
saveMergedMatchData_withResolvedExchanges_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSData matchData, IsNSArray exchanges) => gkTurnBasedMatch -> matchData -> exchanges -> Ptr () -> IO ()
saveMergedMatchData_withResolvedExchanges_completionHandler gkTurnBasedMatch  matchData exchanges completionHandler =
withObjCPtr matchData $ \raw_matchData ->
  withObjCPtr exchanges $ \raw_exchanges ->
      sendMsg gkTurnBasedMatch (mkSelector "saveMergedMatchData:withResolvedExchanges:completionHandler:") retVoid [argPtr (castPtr raw_matchData :: Ptr ()), argPtr (castPtr raw_exchanges :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- sendExchangeToParticipants:data:localizableMessageKey:arguments:timeout:completionHandler:@
sendExchangeToParticipants_data_localizableMessageKey_arguments_timeout_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSArray participants, IsNSData data_, IsNSString key, IsNSArray arguments) => gkTurnBasedMatch -> participants -> data_ -> key -> arguments -> CDouble -> Ptr () -> IO ()
sendExchangeToParticipants_data_localizableMessageKey_arguments_timeout_completionHandler gkTurnBasedMatch  participants data_ key arguments timeout completionHandler =
withObjCPtr participants $ \raw_participants ->
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr key $ \raw_key ->
      withObjCPtr arguments $ \raw_arguments ->
          sendMsg gkTurnBasedMatch (mkSelector "sendExchangeToParticipants:data:localizableMessageKey:arguments:timeout:completionHandler:") retVoid [argPtr (castPtr raw_participants :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ()), argCDouble (fromIntegral timeout), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- sendReminderToParticipants:localizableMessageKey:arguments:completionHandler:@
sendReminderToParticipants_localizableMessageKey_arguments_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSArray participants, IsNSString key, IsNSArray arguments) => gkTurnBasedMatch -> participants -> key -> arguments -> Ptr () -> IO ()
sendReminderToParticipants_localizableMessageKey_arguments_completionHandler gkTurnBasedMatch  participants key arguments completionHandler =
withObjCPtr participants $ \raw_participants ->
  withObjCPtr key $ \raw_key ->
    withObjCPtr arguments $ \raw_arguments ->
        sendMsg gkTurnBasedMatch (mkSelector "sendReminderToParticipants:localizableMessageKey:arguments:completionHandler:") retVoid [argPtr (castPtr raw_participants :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- endTurnWithNextParticipant:matchData:completionHandler:@
endTurnWithNextParticipant_matchData_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsGKTurnBasedParticipant nextParticipant, IsNSData matchData) => gkTurnBasedMatch -> nextParticipant -> matchData -> Ptr () -> IO ()
endTurnWithNextParticipant_matchData_completionHandler gkTurnBasedMatch  nextParticipant matchData completionHandler =
withObjCPtr nextParticipant $ \raw_nextParticipant ->
  withObjCPtr matchData $ \raw_matchData ->
      sendMsg gkTurnBasedMatch (mkSelector "endTurnWithNextParticipant:matchData:completionHandler:") retVoid [argPtr (castPtr raw_nextParticipant :: Ptr ()), argPtr (castPtr raw_matchData :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- participantQuitInTurnWithOutcome:nextParticipant:matchData:completionHandler:@
participantQuitInTurnWithOutcome_nextParticipant_matchData_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsGKTurnBasedParticipant nextParticipant, IsNSData matchData) => gkTurnBasedMatch -> GKTurnBasedMatchOutcome -> nextParticipant -> matchData -> Ptr () -> IO ()
participantQuitInTurnWithOutcome_nextParticipant_matchData_completionHandler gkTurnBasedMatch  matchOutcome nextParticipant matchData completionHandler =
withObjCPtr nextParticipant $ \raw_nextParticipant ->
  withObjCPtr matchData $ \raw_matchData ->
      sendMsg gkTurnBasedMatch (mkSelector "participantQuitInTurnWithOutcome:nextParticipant:matchData:completionHandler:") retVoid [argCLong (coerce matchOutcome), argPtr (castPtr raw_nextParticipant :: Ptr ()), argPtr (castPtr raw_matchData :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- matchID@
matchID :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSString)
matchID gkTurnBasedMatch  =
  sendMsg gkTurnBasedMatch (mkSelector "matchID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- creationDate@
creationDate :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSDate)
creationDate gkTurnBasedMatch  =
  sendMsg gkTurnBasedMatch (mkSelector "creationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- participants@
participants :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSArray)
participants gkTurnBasedMatch  =
  sendMsg gkTurnBasedMatch (mkSelector "participants") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- status@
status :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO GKTurnBasedMatchStatus
status gkTurnBasedMatch  =
  fmap (coerce :: CLong -> GKTurnBasedMatchStatus) $ sendMsg gkTurnBasedMatch (mkSelector "status") retCLong []

-- | @- currentParticipant@
currentParticipant :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id GKTurnBasedParticipant)
currentParticipant gkTurnBasedMatch  =
  sendMsg gkTurnBasedMatch (mkSelector "currentParticipant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- matchData@
matchData :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSData)
matchData gkTurnBasedMatch  =
  sendMsg gkTurnBasedMatch (mkSelector "matchData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- message@
message :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSString)
message gkTurnBasedMatch  =
  sendMsg gkTurnBasedMatch (mkSelector "message") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMessage:@
setMessage :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSString value) => gkTurnBasedMatch -> value -> IO ()
setMessage gkTurnBasedMatch  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkTurnBasedMatch (mkSelector "setMessage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- matchDataMaximumSize@
matchDataMaximumSize :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO CULong
matchDataMaximumSize gkTurnBasedMatch  =
  sendMsg gkTurnBasedMatch (mkSelector "matchDataMaximumSize") retCULong []

-- | @- exchanges@
exchanges :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSArray)
exchanges gkTurnBasedMatch  =
  sendMsg gkTurnBasedMatch (mkSelector "exchanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- activeExchanges@
activeExchanges :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSArray)
activeExchanges gkTurnBasedMatch  =
  sendMsg gkTurnBasedMatch (mkSelector "activeExchanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- completedExchanges@
completedExchanges :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSArray)
completedExchanges gkTurnBasedMatch  =
  sendMsg gkTurnBasedMatch (mkSelector "completedExchanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- exchangeDataMaximumSize@
exchangeDataMaximumSize :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO CULong
exchangeDataMaximumSize gkTurnBasedMatch  =
  sendMsg gkTurnBasedMatch (mkSelector "exchangeDataMaximumSize") retCULong []

-- | @- exchangeMaxInitiatedExchangesPerPlayer@
exchangeMaxInitiatedExchangesPerPlayer :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO CULong
exchangeMaxInitiatedExchangesPerPlayer gkTurnBasedMatch  =
  sendMsg gkTurnBasedMatch (mkSelector "exchangeMaxInitiatedExchangesPerPlayer") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setLocalizableMessageWithKey:arguments:@
setLocalizableMessageWithKey_argumentsSelector :: Selector
setLocalizableMessageWithKey_argumentsSelector = mkSelector "setLocalizableMessageWithKey:arguments:"

-- | @Selector@ for @findMatchForRequest:withCompletionHandler:@
findMatchForRequest_withCompletionHandlerSelector :: Selector
findMatchForRequest_withCompletionHandlerSelector = mkSelector "findMatchForRequest:withCompletionHandler:"

-- | @Selector@ for @loadMatchWithID:withCompletionHandler:@
loadMatchWithID_withCompletionHandlerSelector :: Selector
loadMatchWithID_withCompletionHandlerSelector = mkSelector "loadMatchWithID:withCompletionHandler:"

-- | @Selector@ for @rematchWithCompletionHandler:@
rematchWithCompletionHandlerSelector :: Selector
rematchWithCompletionHandlerSelector = mkSelector "rematchWithCompletionHandler:"

-- | @Selector@ for @acceptInviteWithCompletionHandler:@
acceptInviteWithCompletionHandlerSelector :: Selector
acceptInviteWithCompletionHandlerSelector = mkSelector "acceptInviteWithCompletionHandler:"

-- | @Selector@ for @declineInviteWithCompletionHandler:@
declineInviteWithCompletionHandlerSelector :: Selector
declineInviteWithCompletionHandlerSelector = mkSelector "declineInviteWithCompletionHandler:"

-- | @Selector@ for @removeWithCompletionHandler:@
removeWithCompletionHandlerSelector :: Selector
removeWithCompletionHandlerSelector = mkSelector "removeWithCompletionHandler:"

-- | @Selector@ for @loadMatchDataWithCompletionHandler:@
loadMatchDataWithCompletionHandlerSelector :: Selector
loadMatchDataWithCompletionHandlerSelector = mkSelector "loadMatchDataWithCompletionHandler:"

-- | @Selector@ for @endTurnWithNextParticipants:turnTimeout:matchData:completionHandler:@
endTurnWithNextParticipants_turnTimeout_matchData_completionHandlerSelector :: Selector
endTurnWithNextParticipants_turnTimeout_matchData_completionHandlerSelector = mkSelector "endTurnWithNextParticipants:turnTimeout:matchData:completionHandler:"

-- | @Selector@ for @participantQuitInTurnWithOutcome:nextParticipants:turnTimeout:matchData:completionHandler:@
participantQuitInTurnWithOutcome_nextParticipants_turnTimeout_matchData_completionHandlerSelector :: Selector
participantQuitInTurnWithOutcome_nextParticipants_turnTimeout_matchData_completionHandlerSelector = mkSelector "participantQuitInTurnWithOutcome:nextParticipants:turnTimeout:matchData:completionHandler:"

-- | @Selector@ for @participantQuitOutOfTurnWithOutcome:withCompletionHandler:@
participantQuitOutOfTurnWithOutcome_withCompletionHandlerSelector :: Selector
participantQuitOutOfTurnWithOutcome_withCompletionHandlerSelector = mkSelector "participantQuitOutOfTurnWithOutcome:withCompletionHandler:"

-- | @Selector@ for @endMatchInTurnWithMatchData:completionHandler:@
endMatchInTurnWithMatchData_completionHandlerSelector :: Selector
endMatchInTurnWithMatchData_completionHandlerSelector = mkSelector "endMatchInTurnWithMatchData:completionHandler:"

-- | @Selector@ for @endMatchInTurnWithMatchData:scores:achievements:completionHandler:@
endMatchInTurnWithMatchData_scores_achievements_completionHandlerSelector :: Selector
endMatchInTurnWithMatchData_scores_achievements_completionHandlerSelector = mkSelector "endMatchInTurnWithMatchData:scores:achievements:completionHandler:"

-- | @Selector@ for @endMatchInTurnWithMatchData:leaderboardScores:achievements:completionHandler:@
endMatchInTurnWithMatchData_leaderboardScores_achievements_completionHandlerSelector :: Selector
endMatchInTurnWithMatchData_leaderboardScores_achievements_completionHandlerSelector = mkSelector "endMatchInTurnWithMatchData:leaderboardScores:achievements:completionHandler:"

-- | @Selector@ for @saveCurrentTurnWithMatchData:completionHandler:@
saveCurrentTurnWithMatchData_completionHandlerSelector :: Selector
saveCurrentTurnWithMatchData_completionHandlerSelector = mkSelector "saveCurrentTurnWithMatchData:completionHandler:"

-- | @Selector@ for @saveMergedMatchData:withResolvedExchanges:completionHandler:@
saveMergedMatchData_withResolvedExchanges_completionHandlerSelector :: Selector
saveMergedMatchData_withResolvedExchanges_completionHandlerSelector = mkSelector "saveMergedMatchData:withResolvedExchanges:completionHandler:"

-- | @Selector@ for @sendExchangeToParticipants:data:localizableMessageKey:arguments:timeout:completionHandler:@
sendExchangeToParticipants_data_localizableMessageKey_arguments_timeout_completionHandlerSelector :: Selector
sendExchangeToParticipants_data_localizableMessageKey_arguments_timeout_completionHandlerSelector = mkSelector "sendExchangeToParticipants:data:localizableMessageKey:arguments:timeout:completionHandler:"

-- | @Selector@ for @sendReminderToParticipants:localizableMessageKey:arguments:completionHandler:@
sendReminderToParticipants_localizableMessageKey_arguments_completionHandlerSelector :: Selector
sendReminderToParticipants_localizableMessageKey_arguments_completionHandlerSelector = mkSelector "sendReminderToParticipants:localizableMessageKey:arguments:completionHandler:"

-- | @Selector@ for @endTurnWithNextParticipant:matchData:completionHandler:@
endTurnWithNextParticipant_matchData_completionHandlerSelector :: Selector
endTurnWithNextParticipant_matchData_completionHandlerSelector = mkSelector "endTurnWithNextParticipant:matchData:completionHandler:"

-- | @Selector@ for @participantQuitInTurnWithOutcome:nextParticipant:matchData:completionHandler:@
participantQuitInTurnWithOutcome_nextParticipant_matchData_completionHandlerSelector :: Selector
participantQuitInTurnWithOutcome_nextParticipant_matchData_completionHandlerSelector = mkSelector "participantQuitInTurnWithOutcome:nextParticipant:matchData:completionHandler:"

-- | @Selector@ for @matchID@
matchIDSelector :: Selector
matchIDSelector = mkSelector "matchID"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @participants@
participantsSelector :: Selector
participantsSelector = mkSelector "participants"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @currentParticipant@
currentParticipantSelector :: Selector
currentParticipantSelector = mkSelector "currentParticipant"

-- | @Selector@ for @matchData@
matchDataSelector :: Selector
matchDataSelector = mkSelector "matchData"

-- | @Selector@ for @message@
messageSelector :: Selector
messageSelector = mkSelector "message"

-- | @Selector@ for @setMessage:@
setMessageSelector :: Selector
setMessageSelector = mkSelector "setMessage:"

-- | @Selector@ for @matchDataMaximumSize@
matchDataMaximumSizeSelector :: Selector
matchDataMaximumSizeSelector = mkSelector "matchDataMaximumSize"

-- | @Selector@ for @exchanges@
exchangesSelector :: Selector
exchangesSelector = mkSelector "exchanges"

-- | @Selector@ for @activeExchanges@
activeExchangesSelector :: Selector
activeExchangesSelector = mkSelector "activeExchanges"

-- | @Selector@ for @completedExchanges@
completedExchangesSelector :: Selector
completedExchangesSelector = mkSelector "completedExchanges"

-- | @Selector@ for @exchangeDataMaximumSize@
exchangeDataMaximumSizeSelector :: Selector
exchangeDataMaximumSizeSelector = mkSelector "exchangeDataMaximumSize"

-- | @Selector@ for @exchangeMaxInitiatedExchangesPerPlayer@
exchangeMaxInitiatedExchangesPerPlayerSelector :: Selector
exchangeMaxInitiatedExchangesPerPlayerSelector = mkSelector "exchangeMaxInitiatedExchangesPerPlayer"

