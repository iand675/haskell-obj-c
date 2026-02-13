{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , acceptInviteWithCompletionHandlerSelector
  , activeExchangesSelector
  , completedExchangesSelector
  , creationDateSelector
  , currentParticipantSelector
  , declineInviteWithCompletionHandlerSelector
  , endMatchInTurnWithMatchData_completionHandlerSelector
  , endMatchInTurnWithMatchData_leaderboardScores_achievements_completionHandlerSelector
  , endMatchInTurnWithMatchData_scores_achievements_completionHandlerSelector
  , endTurnWithNextParticipant_matchData_completionHandlerSelector
  , endTurnWithNextParticipants_turnTimeout_matchData_completionHandlerSelector
  , exchangeDataMaximumSizeSelector
  , exchangeMaxInitiatedExchangesPerPlayerSelector
  , exchangesSelector
  , findMatchForRequest_withCompletionHandlerSelector
  , loadMatchDataWithCompletionHandlerSelector
  , loadMatchWithID_withCompletionHandlerSelector
  , matchDataMaximumSizeSelector
  , matchDataSelector
  , matchIDSelector
  , messageSelector
  , participantQuitInTurnWithOutcome_nextParticipant_matchData_completionHandlerSelector
  , participantQuitInTurnWithOutcome_nextParticipants_turnTimeout_matchData_completionHandlerSelector
  , participantQuitOutOfTurnWithOutcome_withCompletionHandlerSelector
  , participantsSelector
  , rematchWithCompletionHandlerSelector
  , removeWithCompletionHandlerSelector
  , saveCurrentTurnWithMatchData_completionHandlerSelector
  , saveMergedMatchData_withResolvedExchanges_completionHandlerSelector
  , sendExchangeToParticipants_data_localizableMessageKey_arguments_timeout_completionHandlerSelector
  , sendReminderToParticipants_localizableMessageKey_arguments_completionHandlerSelector
  , setLocalizableMessageWithKey_argumentsSelector
  , setMessageSelector
  , statusSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- setLocalizableMessageWithKey:arguments:@
setLocalizableMessageWithKey_arguments :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSString key, IsNSArray arguments) => gkTurnBasedMatch -> key -> arguments -> IO ()
setLocalizableMessageWithKey_arguments gkTurnBasedMatch key arguments =
  sendMessage gkTurnBasedMatch setLocalizableMessageWithKey_argumentsSelector (toNSString key) (toNSArray arguments)

-- | @+ findMatchForRequest:withCompletionHandler:@
findMatchForRequest_withCompletionHandler :: IsGKMatchRequest request => request -> Ptr () -> IO ()
findMatchForRequest_withCompletionHandler request completionHandler =
  do
    cls' <- getRequiredClass "GKTurnBasedMatch"
    sendClassMessage cls' findMatchForRequest_withCompletionHandlerSelector (toGKMatchRequest request) completionHandler

-- | @+ loadMatchWithID:withCompletionHandler:@
loadMatchWithID_withCompletionHandler :: IsNSString matchID => matchID -> Ptr () -> IO ()
loadMatchWithID_withCompletionHandler matchID completionHandler =
  do
    cls' <- getRequiredClass "GKTurnBasedMatch"
    sendClassMessage cls' loadMatchWithID_withCompletionHandlerSelector (toNSString matchID) completionHandler

-- | @- rematchWithCompletionHandler:@
rematchWithCompletionHandler :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> Ptr () -> IO ()
rematchWithCompletionHandler gkTurnBasedMatch completionHandler =
  sendMessage gkTurnBasedMatch rematchWithCompletionHandlerSelector completionHandler

-- | @- acceptInviteWithCompletionHandler:@
acceptInviteWithCompletionHandler :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> Ptr () -> IO ()
acceptInviteWithCompletionHandler gkTurnBasedMatch completionHandler =
  sendMessage gkTurnBasedMatch acceptInviteWithCompletionHandlerSelector completionHandler

-- | @- declineInviteWithCompletionHandler:@
declineInviteWithCompletionHandler :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> Ptr () -> IO ()
declineInviteWithCompletionHandler gkTurnBasedMatch completionHandler =
  sendMessage gkTurnBasedMatch declineInviteWithCompletionHandlerSelector completionHandler

-- | @- removeWithCompletionHandler:@
removeWithCompletionHandler :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> Ptr () -> IO ()
removeWithCompletionHandler gkTurnBasedMatch completionHandler =
  sendMessage gkTurnBasedMatch removeWithCompletionHandlerSelector completionHandler

-- | @- loadMatchDataWithCompletionHandler:@
loadMatchDataWithCompletionHandler :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> Ptr () -> IO ()
loadMatchDataWithCompletionHandler gkTurnBasedMatch completionHandler =
  sendMessage gkTurnBasedMatch loadMatchDataWithCompletionHandlerSelector completionHandler

-- | @- endTurnWithNextParticipants:turnTimeout:matchData:completionHandler:@
endTurnWithNextParticipants_turnTimeout_matchData_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSArray nextParticipants, IsNSData matchData) => gkTurnBasedMatch -> nextParticipants -> CDouble -> matchData -> Ptr () -> IO ()
endTurnWithNextParticipants_turnTimeout_matchData_completionHandler gkTurnBasedMatch nextParticipants timeout matchData completionHandler =
  sendMessage gkTurnBasedMatch endTurnWithNextParticipants_turnTimeout_matchData_completionHandlerSelector (toNSArray nextParticipants) timeout (toNSData matchData) completionHandler

-- | @- participantQuitInTurnWithOutcome:nextParticipants:turnTimeout:matchData:completionHandler:@
participantQuitInTurnWithOutcome_nextParticipants_turnTimeout_matchData_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSArray nextParticipants, IsNSData matchData) => gkTurnBasedMatch -> GKTurnBasedMatchOutcome -> nextParticipants -> CDouble -> matchData -> Ptr () -> IO ()
participantQuitInTurnWithOutcome_nextParticipants_turnTimeout_matchData_completionHandler gkTurnBasedMatch matchOutcome nextParticipants timeout matchData completionHandler =
  sendMessage gkTurnBasedMatch participantQuitInTurnWithOutcome_nextParticipants_turnTimeout_matchData_completionHandlerSelector matchOutcome (toNSArray nextParticipants) timeout (toNSData matchData) completionHandler

-- | @- participantQuitOutOfTurnWithOutcome:withCompletionHandler:@
participantQuitOutOfTurnWithOutcome_withCompletionHandler :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> GKTurnBasedMatchOutcome -> Ptr () -> IO ()
participantQuitOutOfTurnWithOutcome_withCompletionHandler gkTurnBasedMatch matchOutcome completionHandler =
  sendMessage gkTurnBasedMatch participantQuitOutOfTurnWithOutcome_withCompletionHandlerSelector matchOutcome completionHandler

-- | @- endMatchInTurnWithMatchData:completionHandler:@
endMatchInTurnWithMatchData_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSData matchData) => gkTurnBasedMatch -> matchData -> Ptr () -> IO ()
endMatchInTurnWithMatchData_completionHandler gkTurnBasedMatch matchData completionHandler =
  sendMessage gkTurnBasedMatch endMatchInTurnWithMatchData_completionHandlerSelector (toNSData matchData) completionHandler

-- | @- endMatchInTurnWithMatchData:scores:achievements:completionHandler:@
endMatchInTurnWithMatchData_scores_achievements_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSData matchData, IsNSArray scores, IsNSArray achievements) => gkTurnBasedMatch -> matchData -> scores -> achievements -> Ptr () -> IO ()
endMatchInTurnWithMatchData_scores_achievements_completionHandler gkTurnBasedMatch matchData scores achievements completionHandler =
  sendMessage gkTurnBasedMatch endMatchInTurnWithMatchData_scores_achievements_completionHandlerSelector (toNSData matchData) (toNSArray scores) (toNSArray achievements) completionHandler

-- | @- endMatchInTurnWithMatchData:leaderboardScores:achievements:completionHandler:@
endMatchInTurnWithMatchData_leaderboardScores_achievements_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSData matchData, IsNSArray scores, IsNSArray achievements) => gkTurnBasedMatch -> matchData -> scores -> achievements -> Ptr () -> IO ()
endMatchInTurnWithMatchData_leaderboardScores_achievements_completionHandler gkTurnBasedMatch matchData scores achievements completionHandler =
  sendMessage gkTurnBasedMatch endMatchInTurnWithMatchData_leaderboardScores_achievements_completionHandlerSelector (toNSData matchData) (toNSArray scores) (toNSArray achievements) completionHandler

-- | @- saveCurrentTurnWithMatchData:completionHandler:@
saveCurrentTurnWithMatchData_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSData matchData) => gkTurnBasedMatch -> matchData -> Ptr () -> IO ()
saveCurrentTurnWithMatchData_completionHandler gkTurnBasedMatch matchData completionHandler =
  sendMessage gkTurnBasedMatch saveCurrentTurnWithMatchData_completionHandlerSelector (toNSData matchData) completionHandler

-- | @- saveMergedMatchData:withResolvedExchanges:completionHandler:@
saveMergedMatchData_withResolvedExchanges_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSData matchData, IsNSArray exchanges) => gkTurnBasedMatch -> matchData -> exchanges -> Ptr () -> IO ()
saveMergedMatchData_withResolvedExchanges_completionHandler gkTurnBasedMatch matchData exchanges completionHandler =
  sendMessage gkTurnBasedMatch saveMergedMatchData_withResolvedExchanges_completionHandlerSelector (toNSData matchData) (toNSArray exchanges) completionHandler

-- | @- sendExchangeToParticipants:data:localizableMessageKey:arguments:timeout:completionHandler:@
sendExchangeToParticipants_data_localizableMessageKey_arguments_timeout_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSArray participants, IsNSData data_, IsNSString key, IsNSArray arguments) => gkTurnBasedMatch -> participants -> data_ -> key -> arguments -> CDouble -> Ptr () -> IO ()
sendExchangeToParticipants_data_localizableMessageKey_arguments_timeout_completionHandler gkTurnBasedMatch participants data_ key arguments timeout completionHandler =
  sendMessage gkTurnBasedMatch sendExchangeToParticipants_data_localizableMessageKey_arguments_timeout_completionHandlerSelector (toNSArray participants) (toNSData data_) (toNSString key) (toNSArray arguments) timeout completionHandler

-- | @- sendReminderToParticipants:localizableMessageKey:arguments:completionHandler:@
sendReminderToParticipants_localizableMessageKey_arguments_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSArray participants, IsNSString key, IsNSArray arguments) => gkTurnBasedMatch -> participants -> key -> arguments -> Ptr () -> IO ()
sendReminderToParticipants_localizableMessageKey_arguments_completionHandler gkTurnBasedMatch participants key arguments completionHandler =
  sendMessage gkTurnBasedMatch sendReminderToParticipants_localizableMessageKey_arguments_completionHandlerSelector (toNSArray participants) (toNSString key) (toNSArray arguments) completionHandler

-- | @- endTurnWithNextParticipant:matchData:completionHandler:@
endTurnWithNextParticipant_matchData_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsGKTurnBasedParticipant nextParticipant, IsNSData matchData) => gkTurnBasedMatch -> nextParticipant -> matchData -> Ptr () -> IO ()
endTurnWithNextParticipant_matchData_completionHandler gkTurnBasedMatch nextParticipant matchData completionHandler =
  sendMessage gkTurnBasedMatch endTurnWithNextParticipant_matchData_completionHandlerSelector (toGKTurnBasedParticipant nextParticipant) (toNSData matchData) completionHandler

-- | @- participantQuitInTurnWithOutcome:nextParticipant:matchData:completionHandler:@
participantQuitInTurnWithOutcome_nextParticipant_matchData_completionHandler :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsGKTurnBasedParticipant nextParticipant, IsNSData matchData) => gkTurnBasedMatch -> GKTurnBasedMatchOutcome -> nextParticipant -> matchData -> Ptr () -> IO ()
participantQuitInTurnWithOutcome_nextParticipant_matchData_completionHandler gkTurnBasedMatch matchOutcome nextParticipant matchData completionHandler =
  sendMessage gkTurnBasedMatch participantQuitInTurnWithOutcome_nextParticipant_matchData_completionHandlerSelector matchOutcome (toGKTurnBasedParticipant nextParticipant) (toNSData matchData) completionHandler

-- | @- matchID@
matchID :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSString)
matchID gkTurnBasedMatch =
  sendMessage gkTurnBasedMatch matchIDSelector

-- | @- creationDate@
creationDate :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSDate)
creationDate gkTurnBasedMatch =
  sendMessage gkTurnBasedMatch creationDateSelector

-- | @- participants@
participants :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSArray)
participants gkTurnBasedMatch =
  sendMessage gkTurnBasedMatch participantsSelector

-- | @- status@
status :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO GKTurnBasedMatchStatus
status gkTurnBasedMatch =
  sendMessage gkTurnBasedMatch statusSelector

-- | @- currentParticipant@
currentParticipant :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id GKTurnBasedParticipant)
currentParticipant gkTurnBasedMatch =
  sendMessage gkTurnBasedMatch currentParticipantSelector

-- | @- matchData@
matchData :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSData)
matchData gkTurnBasedMatch =
  sendMessage gkTurnBasedMatch matchDataSelector

-- | @- message@
message :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSString)
message gkTurnBasedMatch =
  sendMessage gkTurnBasedMatch messageSelector

-- | @- setMessage:@
setMessage :: (IsGKTurnBasedMatch gkTurnBasedMatch, IsNSString value) => gkTurnBasedMatch -> value -> IO ()
setMessage gkTurnBasedMatch value =
  sendMessage gkTurnBasedMatch setMessageSelector (toNSString value)

-- | @- matchDataMaximumSize@
matchDataMaximumSize :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO CULong
matchDataMaximumSize gkTurnBasedMatch =
  sendMessage gkTurnBasedMatch matchDataMaximumSizeSelector

-- | @- exchanges@
exchanges :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSArray)
exchanges gkTurnBasedMatch =
  sendMessage gkTurnBasedMatch exchangesSelector

-- | @- activeExchanges@
activeExchanges :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSArray)
activeExchanges gkTurnBasedMatch =
  sendMessage gkTurnBasedMatch activeExchangesSelector

-- | @- completedExchanges@
completedExchanges :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO (Id NSArray)
completedExchanges gkTurnBasedMatch =
  sendMessage gkTurnBasedMatch completedExchangesSelector

-- | @- exchangeDataMaximumSize@
exchangeDataMaximumSize :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO CULong
exchangeDataMaximumSize gkTurnBasedMatch =
  sendMessage gkTurnBasedMatch exchangeDataMaximumSizeSelector

-- | @- exchangeMaxInitiatedExchangesPerPlayer@
exchangeMaxInitiatedExchangesPerPlayer :: IsGKTurnBasedMatch gkTurnBasedMatch => gkTurnBasedMatch -> IO CULong
exchangeMaxInitiatedExchangesPerPlayer gkTurnBasedMatch =
  sendMessage gkTurnBasedMatch exchangeMaxInitiatedExchangesPerPlayerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setLocalizableMessageWithKey:arguments:@
setLocalizableMessageWithKey_argumentsSelector :: Selector '[Id NSString, Id NSArray] ()
setLocalizableMessageWithKey_argumentsSelector = mkSelector "setLocalizableMessageWithKey:arguments:"

-- | @Selector@ for @findMatchForRequest:withCompletionHandler:@
findMatchForRequest_withCompletionHandlerSelector :: Selector '[Id GKMatchRequest, Ptr ()] ()
findMatchForRequest_withCompletionHandlerSelector = mkSelector "findMatchForRequest:withCompletionHandler:"

-- | @Selector@ for @loadMatchWithID:withCompletionHandler:@
loadMatchWithID_withCompletionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
loadMatchWithID_withCompletionHandlerSelector = mkSelector "loadMatchWithID:withCompletionHandler:"

-- | @Selector@ for @rematchWithCompletionHandler:@
rematchWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
rematchWithCompletionHandlerSelector = mkSelector "rematchWithCompletionHandler:"

-- | @Selector@ for @acceptInviteWithCompletionHandler:@
acceptInviteWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
acceptInviteWithCompletionHandlerSelector = mkSelector "acceptInviteWithCompletionHandler:"

-- | @Selector@ for @declineInviteWithCompletionHandler:@
declineInviteWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
declineInviteWithCompletionHandlerSelector = mkSelector "declineInviteWithCompletionHandler:"

-- | @Selector@ for @removeWithCompletionHandler:@
removeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
removeWithCompletionHandlerSelector = mkSelector "removeWithCompletionHandler:"

-- | @Selector@ for @loadMatchDataWithCompletionHandler:@
loadMatchDataWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadMatchDataWithCompletionHandlerSelector = mkSelector "loadMatchDataWithCompletionHandler:"

-- | @Selector@ for @endTurnWithNextParticipants:turnTimeout:matchData:completionHandler:@
endTurnWithNextParticipants_turnTimeout_matchData_completionHandlerSelector :: Selector '[Id NSArray, CDouble, Id NSData, Ptr ()] ()
endTurnWithNextParticipants_turnTimeout_matchData_completionHandlerSelector = mkSelector "endTurnWithNextParticipants:turnTimeout:matchData:completionHandler:"

-- | @Selector@ for @participantQuitInTurnWithOutcome:nextParticipants:turnTimeout:matchData:completionHandler:@
participantQuitInTurnWithOutcome_nextParticipants_turnTimeout_matchData_completionHandlerSelector :: Selector '[GKTurnBasedMatchOutcome, Id NSArray, CDouble, Id NSData, Ptr ()] ()
participantQuitInTurnWithOutcome_nextParticipants_turnTimeout_matchData_completionHandlerSelector = mkSelector "participantQuitInTurnWithOutcome:nextParticipants:turnTimeout:matchData:completionHandler:"

-- | @Selector@ for @participantQuitOutOfTurnWithOutcome:withCompletionHandler:@
participantQuitOutOfTurnWithOutcome_withCompletionHandlerSelector :: Selector '[GKTurnBasedMatchOutcome, Ptr ()] ()
participantQuitOutOfTurnWithOutcome_withCompletionHandlerSelector = mkSelector "participantQuitOutOfTurnWithOutcome:withCompletionHandler:"

-- | @Selector@ for @endMatchInTurnWithMatchData:completionHandler:@
endMatchInTurnWithMatchData_completionHandlerSelector :: Selector '[Id NSData, Ptr ()] ()
endMatchInTurnWithMatchData_completionHandlerSelector = mkSelector "endMatchInTurnWithMatchData:completionHandler:"

-- | @Selector@ for @endMatchInTurnWithMatchData:scores:achievements:completionHandler:@
endMatchInTurnWithMatchData_scores_achievements_completionHandlerSelector :: Selector '[Id NSData, Id NSArray, Id NSArray, Ptr ()] ()
endMatchInTurnWithMatchData_scores_achievements_completionHandlerSelector = mkSelector "endMatchInTurnWithMatchData:scores:achievements:completionHandler:"

-- | @Selector@ for @endMatchInTurnWithMatchData:leaderboardScores:achievements:completionHandler:@
endMatchInTurnWithMatchData_leaderboardScores_achievements_completionHandlerSelector :: Selector '[Id NSData, Id NSArray, Id NSArray, Ptr ()] ()
endMatchInTurnWithMatchData_leaderboardScores_achievements_completionHandlerSelector = mkSelector "endMatchInTurnWithMatchData:leaderboardScores:achievements:completionHandler:"

-- | @Selector@ for @saveCurrentTurnWithMatchData:completionHandler:@
saveCurrentTurnWithMatchData_completionHandlerSelector :: Selector '[Id NSData, Ptr ()] ()
saveCurrentTurnWithMatchData_completionHandlerSelector = mkSelector "saveCurrentTurnWithMatchData:completionHandler:"

-- | @Selector@ for @saveMergedMatchData:withResolvedExchanges:completionHandler:@
saveMergedMatchData_withResolvedExchanges_completionHandlerSelector :: Selector '[Id NSData, Id NSArray, Ptr ()] ()
saveMergedMatchData_withResolvedExchanges_completionHandlerSelector = mkSelector "saveMergedMatchData:withResolvedExchanges:completionHandler:"

-- | @Selector@ for @sendExchangeToParticipants:data:localizableMessageKey:arguments:timeout:completionHandler:@
sendExchangeToParticipants_data_localizableMessageKey_arguments_timeout_completionHandlerSelector :: Selector '[Id NSArray, Id NSData, Id NSString, Id NSArray, CDouble, Ptr ()] ()
sendExchangeToParticipants_data_localizableMessageKey_arguments_timeout_completionHandlerSelector = mkSelector "sendExchangeToParticipants:data:localizableMessageKey:arguments:timeout:completionHandler:"

-- | @Selector@ for @sendReminderToParticipants:localizableMessageKey:arguments:completionHandler:@
sendReminderToParticipants_localizableMessageKey_arguments_completionHandlerSelector :: Selector '[Id NSArray, Id NSString, Id NSArray, Ptr ()] ()
sendReminderToParticipants_localizableMessageKey_arguments_completionHandlerSelector = mkSelector "sendReminderToParticipants:localizableMessageKey:arguments:completionHandler:"

-- | @Selector@ for @endTurnWithNextParticipant:matchData:completionHandler:@
endTurnWithNextParticipant_matchData_completionHandlerSelector :: Selector '[Id GKTurnBasedParticipant, Id NSData, Ptr ()] ()
endTurnWithNextParticipant_matchData_completionHandlerSelector = mkSelector "endTurnWithNextParticipant:matchData:completionHandler:"

-- | @Selector@ for @participantQuitInTurnWithOutcome:nextParticipant:matchData:completionHandler:@
participantQuitInTurnWithOutcome_nextParticipant_matchData_completionHandlerSelector :: Selector '[GKTurnBasedMatchOutcome, Id GKTurnBasedParticipant, Id NSData, Ptr ()] ()
participantQuitInTurnWithOutcome_nextParticipant_matchData_completionHandlerSelector = mkSelector "participantQuitInTurnWithOutcome:nextParticipant:matchData:completionHandler:"

-- | @Selector@ for @matchID@
matchIDSelector :: Selector '[] (Id NSString)
matchIDSelector = mkSelector "matchID"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector '[] (Id NSDate)
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @participants@
participantsSelector :: Selector '[] (Id NSArray)
participantsSelector = mkSelector "participants"

-- | @Selector@ for @status@
statusSelector :: Selector '[] GKTurnBasedMatchStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @currentParticipant@
currentParticipantSelector :: Selector '[] (Id GKTurnBasedParticipant)
currentParticipantSelector = mkSelector "currentParticipant"

-- | @Selector@ for @matchData@
matchDataSelector :: Selector '[] (Id NSData)
matchDataSelector = mkSelector "matchData"

-- | @Selector@ for @message@
messageSelector :: Selector '[] (Id NSString)
messageSelector = mkSelector "message"

-- | @Selector@ for @setMessage:@
setMessageSelector :: Selector '[Id NSString] ()
setMessageSelector = mkSelector "setMessage:"

-- | @Selector@ for @matchDataMaximumSize@
matchDataMaximumSizeSelector :: Selector '[] CULong
matchDataMaximumSizeSelector = mkSelector "matchDataMaximumSize"

-- | @Selector@ for @exchanges@
exchangesSelector :: Selector '[] (Id NSArray)
exchangesSelector = mkSelector "exchanges"

-- | @Selector@ for @activeExchanges@
activeExchangesSelector :: Selector '[] (Id NSArray)
activeExchangesSelector = mkSelector "activeExchanges"

-- | @Selector@ for @completedExchanges@
completedExchangesSelector :: Selector '[] (Id NSArray)
completedExchangesSelector = mkSelector "completedExchanges"

-- | @Selector@ for @exchangeDataMaximumSize@
exchangeDataMaximumSizeSelector :: Selector '[] CULong
exchangeDataMaximumSizeSelector = mkSelector "exchangeDataMaximumSize"

-- | @Selector@ for @exchangeMaxInitiatedExchangesPerPlayer@
exchangeMaxInitiatedExchangesPerPlayerSelector :: Selector '[] CULong
exchangeMaxInitiatedExchangesPerPlayerSelector = mkSelector "exchangeMaxInitiatedExchangesPerPlayer"

