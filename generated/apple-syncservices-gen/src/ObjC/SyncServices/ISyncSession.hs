{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ISyncSession@.
module ObjC.SyncServices.ISyncSession
  ( ISyncSession
  , IsISyncSession(..)
  , beginSessionWithClient_entityNames_beforeDate
  , beginSessionInBackgroundWithClient_entityNames_target_selector
  , cancelPreviousBeginSessionWithClient
  , beginSessionWithClient_entityNames_beforeDate_lastAnchors
  , beginSessionInBackgroundWithClient_entityNames_target_selector_lastAnchors
  , clientDidResetEntityNames
  , clientWantsToPushAllRecordsForEntityNames
  , shouldPushChangesForEntityName
  , shouldPushAllRecordsForEntityName
  , shouldPullChangesForEntityName
  , shouldReplaceAllRecordsOnClientForEntityName
  , pushChange
  , pushChangesFromRecord_withIdentifier
  , deleteRecordWithIdentifier
  , clientLostRecordWithIdentifier_shouldReplaceOnNextSync
  , clientFinishedPushingChangesWithNextAnchors
  , prepareToPullChangesForEntityNames_beforeDate
  , prepareToPullChangesInBackgroundForEntityNames_target_selector
  , changeEnumeratorForEntityNames
  , clientAcceptedChangesForRecordWithIdentifier_formattedRecord_newRecordIdentifier
  , clientRefusedChangesForRecordWithIdentifier
  , clientCommittedAcceptedChanges
  , clientCommittedAcceptedChangesWithNextAnchors
  , clientChangedRecordIdentifiers
  , isCancelled
  , cancelSyncing
  , finishSyncing
  , clientInfoForRecordWithIdentifier
  , setClientInfo_forRecordWithIdentifier
  , snapshotOfRecordsInTruth
  , ping
  , beginSessionInBackgroundWithClient_entityNames_target_selectorSelector
  , beginSessionInBackgroundWithClient_entityNames_target_selector_lastAnchorsSelector
  , beginSessionWithClient_entityNames_beforeDateSelector
  , beginSessionWithClient_entityNames_beforeDate_lastAnchorsSelector
  , cancelPreviousBeginSessionWithClientSelector
  , cancelSyncingSelector
  , changeEnumeratorForEntityNamesSelector
  , clientAcceptedChangesForRecordWithIdentifier_formattedRecord_newRecordIdentifierSelector
  , clientChangedRecordIdentifiersSelector
  , clientCommittedAcceptedChangesSelector
  , clientCommittedAcceptedChangesWithNextAnchorsSelector
  , clientDidResetEntityNamesSelector
  , clientFinishedPushingChangesWithNextAnchorsSelector
  , clientInfoForRecordWithIdentifierSelector
  , clientLostRecordWithIdentifier_shouldReplaceOnNextSyncSelector
  , clientRefusedChangesForRecordWithIdentifierSelector
  , clientWantsToPushAllRecordsForEntityNamesSelector
  , deleteRecordWithIdentifierSelector
  , finishSyncingSelector
  , isCancelledSelector
  , pingSelector
  , prepareToPullChangesForEntityNames_beforeDateSelector
  , prepareToPullChangesInBackgroundForEntityNames_target_selectorSelector
  , pushChangeSelector
  , pushChangesFromRecord_withIdentifierSelector
  , setClientInfo_forRecordWithIdentifierSelector
  , shouldPullChangesForEntityNameSelector
  , shouldPushAllRecordsForEntityNameSelector
  , shouldPushChangesForEntityNameSelector
  , shouldReplaceAllRecordsOnClientForEntityNameSelector
  , snapshotOfRecordsInTruthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SyncServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ beginSessionWithClient:entityNames:beforeDate:@
beginSessionWithClient_entityNames_beforeDate :: (IsISyncClient client, IsNSArray entityNames, IsNSDate date) => client -> entityNames -> date -> IO (Id ISyncSession)
beginSessionWithClient_entityNames_beforeDate client entityNames date =
  do
    cls' <- getRequiredClass "ISyncSession"
    sendClassMessage cls' beginSessionWithClient_entityNames_beforeDateSelector (toISyncClient client) (toNSArray entityNames) (toNSDate date)

-- | @+ beginSessionInBackgroundWithClient:entityNames:target:selector:@
beginSessionInBackgroundWithClient_entityNames_target_selector :: (IsISyncClient client, IsNSArray entityNames) => client -> entityNames -> RawId -> Sel -> IO ()
beginSessionInBackgroundWithClient_entityNames_target_selector client entityNames target selector =
  do
    cls' <- getRequiredClass "ISyncSession"
    sendClassMessage cls' beginSessionInBackgroundWithClient_entityNames_target_selectorSelector (toISyncClient client) (toNSArray entityNames) target selector

-- | @+ cancelPreviousBeginSessionWithClient:@
cancelPreviousBeginSessionWithClient :: IsISyncClient client => client -> IO ()
cancelPreviousBeginSessionWithClient client =
  do
    cls' <- getRequiredClass "ISyncSession"
    sendClassMessage cls' cancelPreviousBeginSessionWithClientSelector (toISyncClient client)

-- | @+ beginSessionWithClient:entityNames:beforeDate:lastAnchors:@
beginSessionWithClient_entityNames_beforeDate_lastAnchors :: (IsISyncClient client, IsNSArray entityNames, IsNSDate date, IsNSDictionary anchors) => client -> entityNames -> date -> anchors -> IO (Id ISyncSession)
beginSessionWithClient_entityNames_beforeDate_lastAnchors client entityNames date anchors =
  do
    cls' <- getRequiredClass "ISyncSession"
    sendClassMessage cls' beginSessionWithClient_entityNames_beforeDate_lastAnchorsSelector (toISyncClient client) (toNSArray entityNames) (toNSDate date) (toNSDictionary anchors)

-- | @+ beginSessionInBackgroundWithClient:entityNames:target:selector:lastAnchors:@
beginSessionInBackgroundWithClient_entityNames_target_selector_lastAnchors :: (IsISyncClient client, IsNSArray entityNames, IsNSDictionary anchors) => client -> entityNames -> RawId -> Sel -> anchors -> IO ()
beginSessionInBackgroundWithClient_entityNames_target_selector_lastAnchors client entityNames target selector anchors =
  do
    cls' <- getRequiredClass "ISyncSession"
    sendClassMessage cls' beginSessionInBackgroundWithClient_entityNames_target_selector_lastAnchorsSelector (toISyncClient client) (toNSArray entityNames) target selector (toNSDictionary anchors)

-- | @- clientDidResetEntityNames:@
clientDidResetEntityNames :: (IsISyncSession iSyncSession, IsNSArray entityNames) => iSyncSession -> entityNames -> IO ()
clientDidResetEntityNames iSyncSession entityNames =
  sendMessage iSyncSession clientDidResetEntityNamesSelector (toNSArray entityNames)

-- | @- clientWantsToPushAllRecordsForEntityNames:@
clientWantsToPushAllRecordsForEntityNames :: (IsISyncSession iSyncSession, IsNSArray entityNames) => iSyncSession -> entityNames -> IO ()
clientWantsToPushAllRecordsForEntityNames iSyncSession entityNames =
  sendMessage iSyncSession clientWantsToPushAllRecordsForEntityNamesSelector (toNSArray entityNames)

-- | @- shouldPushChangesForEntityName:@
shouldPushChangesForEntityName :: (IsISyncSession iSyncSession, IsNSString entityName) => iSyncSession -> entityName -> IO Bool
shouldPushChangesForEntityName iSyncSession entityName =
  sendMessage iSyncSession shouldPushChangesForEntityNameSelector (toNSString entityName)

-- | @- shouldPushAllRecordsForEntityName:@
shouldPushAllRecordsForEntityName :: (IsISyncSession iSyncSession, IsNSString entityName) => iSyncSession -> entityName -> IO Bool
shouldPushAllRecordsForEntityName iSyncSession entityName =
  sendMessage iSyncSession shouldPushAllRecordsForEntityNameSelector (toNSString entityName)

-- | @- shouldPullChangesForEntityName:@
shouldPullChangesForEntityName :: (IsISyncSession iSyncSession, IsNSString entityName) => iSyncSession -> entityName -> IO Bool
shouldPullChangesForEntityName iSyncSession entityName =
  sendMessage iSyncSession shouldPullChangesForEntityNameSelector (toNSString entityName)

-- | @- shouldReplaceAllRecordsOnClientForEntityName:@
shouldReplaceAllRecordsOnClientForEntityName :: (IsISyncSession iSyncSession, IsNSString entityName) => iSyncSession -> entityName -> IO Bool
shouldReplaceAllRecordsOnClientForEntityName iSyncSession entityName =
  sendMessage iSyncSession shouldReplaceAllRecordsOnClientForEntityNameSelector (toNSString entityName)

-- | @- pushChange:@
pushChange :: (IsISyncSession iSyncSession, IsISyncChange change) => iSyncSession -> change -> IO ()
pushChange iSyncSession change =
  sendMessage iSyncSession pushChangeSelector (toISyncChange change)

-- | @- pushChangesFromRecord:withIdentifier:@
pushChangesFromRecord_withIdentifier :: (IsISyncSession iSyncSession, IsNSDictionary record, IsNSString recordId) => iSyncSession -> record -> recordId -> IO ()
pushChangesFromRecord_withIdentifier iSyncSession record recordId =
  sendMessage iSyncSession pushChangesFromRecord_withIdentifierSelector (toNSDictionary record) (toNSString recordId)

-- | @- deleteRecordWithIdentifier:@
deleteRecordWithIdentifier :: (IsISyncSession iSyncSession, IsNSString recordId) => iSyncSession -> recordId -> IO ()
deleteRecordWithIdentifier iSyncSession recordId =
  sendMessage iSyncSession deleteRecordWithIdentifierSelector (toNSString recordId)

-- | @- clientLostRecordWithIdentifier:shouldReplaceOnNextSync:@
clientLostRecordWithIdentifier_shouldReplaceOnNextSync :: (IsISyncSession iSyncSession, IsNSString recordId) => iSyncSession -> recordId -> Bool -> IO ()
clientLostRecordWithIdentifier_shouldReplaceOnNextSync iSyncSession recordId flag =
  sendMessage iSyncSession clientLostRecordWithIdentifier_shouldReplaceOnNextSyncSelector (toNSString recordId) flag

-- | @- clientFinishedPushingChangesWithNextAnchors:@
clientFinishedPushingChangesWithNextAnchors :: (IsISyncSession iSyncSession, IsNSDictionary anchors) => iSyncSession -> anchors -> IO ()
clientFinishedPushingChangesWithNextAnchors iSyncSession anchors =
  sendMessage iSyncSession clientFinishedPushingChangesWithNextAnchorsSelector (toNSDictionary anchors)

-- | @- prepareToPullChangesForEntityNames:beforeDate:@
prepareToPullChangesForEntityNames_beforeDate :: (IsISyncSession iSyncSession, IsNSArray entityNames, IsNSDate date) => iSyncSession -> entityNames -> date -> IO Bool
prepareToPullChangesForEntityNames_beforeDate iSyncSession entityNames date =
  sendMessage iSyncSession prepareToPullChangesForEntityNames_beforeDateSelector (toNSArray entityNames) (toNSDate date)

-- | @- prepareToPullChangesInBackgroundForEntityNames:target:selector:@
prepareToPullChangesInBackgroundForEntityNames_target_selector :: (IsISyncSession iSyncSession, IsNSArray entityNames) => iSyncSession -> entityNames -> RawId -> Sel -> IO ()
prepareToPullChangesInBackgroundForEntityNames_target_selector iSyncSession entityNames target selector =
  sendMessage iSyncSession prepareToPullChangesInBackgroundForEntityNames_target_selectorSelector (toNSArray entityNames) target selector

-- | @- changeEnumeratorForEntityNames:@
changeEnumeratorForEntityNames :: (IsISyncSession iSyncSession, IsNSArray entityNames) => iSyncSession -> entityNames -> IO (Id NSEnumerator)
changeEnumeratorForEntityNames iSyncSession entityNames =
  sendMessage iSyncSession changeEnumeratorForEntityNamesSelector (toNSArray entityNames)

-- | @- clientAcceptedChangesForRecordWithIdentifier:formattedRecord:newRecordIdentifier:@
clientAcceptedChangesForRecordWithIdentifier_formattedRecord_newRecordIdentifier :: (IsISyncSession iSyncSession, IsNSString recordId, IsNSDictionary formattedRecord, IsNSString recordId2) => iSyncSession -> recordId -> formattedRecord -> recordId2 -> IO ()
clientAcceptedChangesForRecordWithIdentifier_formattedRecord_newRecordIdentifier iSyncSession recordId formattedRecord recordId2 =
  sendMessage iSyncSession clientAcceptedChangesForRecordWithIdentifier_formattedRecord_newRecordIdentifierSelector (toNSString recordId) (toNSDictionary formattedRecord) (toNSString recordId2)

-- | @- clientRefusedChangesForRecordWithIdentifier:@
clientRefusedChangesForRecordWithIdentifier :: (IsISyncSession iSyncSession, IsNSString recordId) => iSyncSession -> recordId -> IO ()
clientRefusedChangesForRecordWithIdentifier iSyncSession recordId =
  sendMessage iSyncSession clientRefusedChangesForRecordWithIdentifierSelector (toNSString recordId)

-- | @- clientCommittedAcceptedChanges@
clientCommittedAcceptedChanges :: IsISyncSession iSyncSession => iSyncSession -> IO ()
clientCommittedAcceptedChanges iSyncSession =
  sendMessage iSyncSession clientCommittedAcceptedChangesSelector

-- | @- clientCommittedAcceptedChangesWithNextAnchors:@
clientCommittedAcceptedChangesWithNextAnchors :: (IsISyncSession iSyncSession, IsNSDictionary anchors) => iSyncSession -> anchors -> IO ()
clientCommittedAcceptedChangesWithNextAnchors iSyncSession anchors =
  sendMessage iSyncSession clientCommittedAcceptedChangesWithNextAnchorsSelector (toNSDictionary anchors)

-- | @- clientChangedRecordIdentifiers:@
clientChangedRecordIdentifiers :: (IsISyncSession iSyncSession, IsNSDictionary oldToNew) => iSyncSession -> oldToNew -> IO ()
clientChangedRecordIdentifiers iSyncSession oldToNew =
  sendMessage iSyncSession clientChangedRecordIdentifiersSelector (toNSDictionary oldToNew)

-- | @- isCancelled@
isCancelled :: IsISyncSession iSyncSession => iSyncSession -> IO Bool
isCancelled iSyncSession =
  sendMessage iSyncSession isCancelledSelector

-- | @- cancelSyncing@
cancelSyncing :: IsISyncSession iSyncSession => iSyncSession -> IO ()
cancelSyncing iSyncSession =
  sendMessage iSyncSession cancelSyncingSelector

-- | @- finishSyncing@
finishSyncing :: IsISyncSession iSyncSession => iSyncSession -> IO ()
finishSyncing iSyncSession =
  sendMessage iSyncSession finishSyncingSelector

-- | @- clientInfoForRecordWithIdentifier:@
clientInfoForRecordWithIdentifier :: (IsISyncSession iSyncSession, IsNSString recordId) => iSyncSession -> recordId -> IO RawId
clientInfoForRecordWithIdentifier iSyncSession recordId =
  sendMessage iSyncSession clientInfoForRecordWithIdentifierSelector (toNSString recordId)

-- | @- setClientInfo:forRecordWithIdentifier:@
setClientInfo_forRecordWithIdentifier :: (IsISyncSession iSyncSession, IsNSString recordId) => iSyncSession -> RawId -> recordId -> IO ()
setClientInfo_forRecordWithIdentifier iSyncSession clientInfo recordId =
  sendMessage iSyncSession setClientInfo_forRecordWithIdentifierSelector clientInfo (toNSString recordId)

-- | @- snapshotOfRecordsInTruth@
snapshotOfRecordsInTruth :: IsISyncSession iSyncSession => iSyncSession -> IO (Id ISyncRecordSnapshot)
snapshotOfRecordsInTruth iSyncSession =
  sendMessage iSyncSession snapshotOfRecordsInTruthSelector

-- | @- ping@
ping :: IsISyncSession iSyncSession => iSyncSession -> IO ()
ping iSyncSession =
  sendMessage iSyncSession pingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @beginSessionWithClient:entityNames:beforeDate:@
beginSessionWithClient_entityNames_beforeDateSelector :: Selector '[Id ISyncClient, Id NSArray, Id NSDate] (Id ISyncSession)
beginSessionWithClient_entityNames_beforeDateSelector = mkSelector "beginSessionWithClient:entityNames:beforeDate:"

-- | @Selector@ for @beginSessionInBackgroundWithClient:entityNames:target:selector:@
beginSessionInBackgroundWithClient_entityNames_target_selectorSelector :: Selector '[Id ISyncClient, Id NSArray, RawId, Sel] ()
beginSessionInBackgroundWithClient_entityNames_target_selectorSelector = mkSelector "beginSessionInBackgroundWithClient:entityNames:target:selector:"

-- | @Selector@ for @cancelPreviousBeginSessionWithClient:@
cancelPreviousBeginSessionWithClientSelector :: Selector '[Id ISyncClient] ()
cancelPreviousBeginSessionWithClientSelector = mkSelector "cancelPreviousBeginSessionWithClient:"

-- | @Selector@ for @beginSessionWithClient:entityNames:beforeDate:lastAnchors:@
beginSessionWithClient_entityNames_beforeDate_lastAnchorsSelector :: Selector '[Id ISyncClient, Id NSArray, Id NSDate, Id NSDictionary] (Id ISyncSession)
beginSessionWithClient_entityNames_beforeDate_lastAnchorsSelector = mkSelector "beginSessionWithClient:entityNames:beforeDate:lastAnchors:"

-- | @Selector@ for @beginSessionInBackgroundWithClient:entityNames:target:selector:lastAnchors:@
beginSessionInBackgroundWithClient_entityNames_target_selector_lastAnchorsSelector :: Selector '[Id ISyncClient, Id NSArray, RawId, Sel, Id NSDictionary] ()
beginSessionInBackgroundWithClient_entityNames_target_selector_lastAnchorsSelector = mkSelector "beginSessionInBackgroundWithClient:entityNames:target:selector:lastAnchors:"

-- | @Selector@ for @clientDidResetEntityNames:@
clientDidResetEntityNamesSelector :: Selector '[Id NSArray] ()
clientDidResetEntityNamesSelector = mkSelector "clientDidResetEntityNames:"

-- | @Selector@ for @clientWantsToPushAllRecordsForEntityNames:@
clientWantsToPushAllRecordsForEntityNamesSelector :: Selector '[Id NSArray] ()
clientWantsToPushAllRecordsForEntityNamesSelector = mkSelector "clientWantsToPushAllRecordsForEntityNames:"

-- | @Selector@ for @shouldPushChangesForEntityName:@
shouldPushChangesForEntityNameSelector :: Selector '[Id NSString] Bool
shouldPushChangesForEntityNameSelector = mkSelector "shouldPushChangesForEntityName:"

-- | @Selector@ for @shouldPushAllRecordsForEntityName:@
shouldPushAllRecordsForEntityNameSelector :: Selector '[Id NSString] Bool
shouldPushAllRecordsForEntityNameSelector = mkSelector "shouldPushAllRecordsForEntityName:"

-- | @Selector@ for @shouldPullChangesForEntityName:@
shouldPullChangesForEntityNameSelector :: Selector '[Id NSString] Bool
shouldPullChangesForEntityNameSelector = mkSelector "shouldPullChangesForEntityName:"

-- | @Selector@ for @shouldReplaceAllRecordsOnClientForEntityName:@
shouldReplaceAllRecordsOnClientForEntityNameSelector :: Selector '[Id NSString] Bool
shouldReplaceAllRecordsOnClientForEntityNameSelector = mkSelector "shouldReplaceAllRecordsOnClientForEntityName:"

-- | @Selector@ for @pushChange:@
pushChangeSelector :: Selector '[Id ISyncChange] ()
pushChangeSelector = mkSelector "pushChange:"

-- | @Selector@ for @pushChangesFromRecord:withIdentifier:@
pushChangesFromRecord_withIdentifierSelector :: Selector '[Id NSDictionary, Id NSString] ()
pushChangesFromRecord_withIdentifierSelector = mkSelector "pushChangesFromRecord:withIdentifier:"

-- | @Selector@ for @deleteRecordWithIdentifier:@
deleteRecordWithIdentifierSelector :: Selector '[Id NSString] ()
deleteRecordWithIdentifierSelector = mkSelector "deleteRecordWithIdentifier:"

-- | @Selector@ for @clientLostRecordWithIdentifier:shouldReplaceOnNextSync:@
clientLostRecordWithIdentifier_shouldReplaceOnNextSyncSelector :: Selector '[Id NSString, Bool] ()
clientLostRecordWithIdentifier_shouldReplaceOnNextSyncSelector = mkSelector "clientLostRecordWithIdentifier:shouldReplaceOnNextSync:"

-- | @Selector@ for @clientFinishedPushingChangesWithNextAnchors:@
clientFinishedPushingChangesWithNextAnchorsSelector :: Selector '[Id NSDictionary] ()
clientFinishedPushingChangesWithNextAnchorsSelector = mkSelector "clientFinishedPushingChangesWithNextAnchors:"

-- | @Selector@ for @prepareToPullChangesForEntityNames:beforeDate:@
prepareToPullChangesForEntityNames_beforeDateSelector :: Selector '[Id NSArray, Id NSDate] Bool
prepareToPullChangesForEntityNames_beforeDateSelector = mkSelector "prepareToPullChangesForEntityNames:beforeDate:"

-- | @Selector@ for @prepareToPullChangesInBackgroundForEntityNames:target:selector:@
prepareToPullChangesInBackgroundForEntityNames_target_selectorSelector :: Selector '[Id NSArray, RawId, Sel] ()
prepareToPullChangesInBackgroundForEntityNames_target_selectorSelector = mkSelector "prepareToPullChangesInBackgroundForEntityNames:target:selector:"

-- | @Selector@ for @changeEnumeratorForEntityNames:@
changeEnumeratorForEntityNamesSelector :: Selector '[Id NSArray] (Id NSEnumerator)
changeEnumeratorForEntityNamesSelector = mkSelector "changeEnumeratorForEntityNames:"

-- | @Selector@ for @clientAcceptedChangesForRecordWithIdentifier:formattedRecord:newRecordIdentifier:@
clientAcceptedChangesForRecordWithIdentifier_formattedRecord_newRecordIdentifierSelector :: Selector '[Id NSString, Id NSDictionary, Id NSString] ()
clientAcceptedChangesForRecordWithIdentifier_formattedRecord_newRecordIdentifierSelector = mkSelector "clientAcceptedChangesForRecordWithIdentifier:formattedRecord:newRecordIdentifier:"

-- | @Selector@ for @clientRefusedChangesForRecordWithIdentifier:@
clientRefusedChangesForRecordWithIdentifierSelector :: Selector '[Id NSString] ()
clientRefusedChangesForRecordWithIdentifierSelector = mkSelector "clientRefusedChangesForRecordWithIdentifier:"

-- | @Selector@ for @clientCommittedAcceptedChanges@
clientCommittedAcceptedChangesSelector :: Selector '[] ()
clientCommittedAcceptedChangesSelector = mkSelector "clientCommittedAcceptedChanges"

-- | @Selector@ for @clientCommittedAcceptedChangesWithNextAnchors:@
clientCommittedAcceptedChangesWithNextAnchorsSelector :: Selector '[Id NSDictionary] ()
clientCommittedAcceptedChangesWithNextAnchorsSelector = mkSelector "clientCommittedAcceptedChangesWithNextAnchors:"

-- | @Selector@ for @clientChangedRecordIdentifiers:@
clientChangedRecordIdentifiersSelector :: Selector '[Id NSDictionary] ()
clientChangedRecordIdentifiersSelector = mkSelector "clientChangedRecordIdentifiers:"

-- | @Selector@ for @isCancelled@
isCancelledSelector :: Selector '[] Bool
isCancelledSelector = mkSelector "isCancelled"

-- | @Selector@ for @cancelSyncing@
cancelSyncingSelector :: Selector '[] ()
cancelSyncingSelector = mkSelector "cancelSyncing"

-- | @Selector@ for @finishSyncing@
finishSyncingSelector :: Selector '[] ()
finishSyncingSelector = mkSelector "finishSyncing"

-- | @Selector@ for @clientInfoForRecordWithIdentifier:@
clientInfoForRecordWithIdentifierSelector :: Selector '[Id NSString] RawId
clientInfoForRecordWithIdentifierSelector = mkSelector "clientInfoForRecordWithIdentifier:"

-- | @Selector@ for @setClientInfo:forRecordWithIdentifier:@
setClientInfo_forRecordWithIdentifierSelector :: Selector '[RawId, Id NSString] ()
setClientInfo_forRecordWithIdentifierSelector = mkSelector "setClientInfo:forRecordWithIdentifier:"

-- | @Selector@ for @snapshotOfRecordsInTruth@
snapshotOfRecordsInTruthSelector :: Selector '[] (Id ISyncRecordSnapshot)
snapshotOfRecordsInTruthSelector = mkSelector "snapshotOfRecordsInTruth"

-- | @Selector@ for @ping@
pingSelector :: Selector '[] ()
pingSelector = mkSelector "ping"

