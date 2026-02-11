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
  , beginSessionWithClient_entityNames_beforeDateSelector
  , beginSessionInBackgroundWithClient_entityNames_target_selectorSelector
  , cancelPreviousBeginSessionWithClientSelector
  , beginSessionWithClient_entityNames_beforeDate_lastAnchorsSelector
  , beginSessionInBackgroundWithClient_entityNames_target_selector_lastAnchorsSelector
  , clientDidResetEntityNamesSelector
  , clientWantsToPushAllRecordsForEntityNamesSelector
  , shouldPushChangesForEntityNameSelector
  , shouldPushAllRecordsForEntityNameSelector
  , shouldPullChangesForEntityNameSelector
  , shouldReplaceAllRecordsOnClientForEntityNameSelector
  , pushChangeSelector
  , pushChangesFromRecord_withIdentifierSelector
  , deleteRecordWithIdentifierSelector
  , clientLostRecordWithIdentifier_shouldReplaceOnNextSyncSelector
  , clientFinishedPushingChangesWithNextAnchorsSelector
  , prepareToPullChangesForEntityNames_beforeDateSelector
  , prepareToPullChangesInBackgroundForEntityNames_target_selectorSelector
  , changeEnumeratorForEntityNamesSelector
  , clientAcceptedChangesForRecordWithIdentifier_formattedRecord_newRecordIdentifierSelector
  , clientRefusedChangesForRecordWithIdentifierSelector
  , clientCommittedAcceptedChangesSelector
  , clientCommittedAcceptedChangesWithNextAnchorsSelector
  , clientChangedRecordIdentifiersSelector
  , isCancelledSelector
  , cancelSyncingSelector
  , finishSyncingSelector
  , clientInfoForRecordWithIdentifierSelector
  , setClientInfo_forRecordWithIdentifierSelector
  , snapshotOfRecordsInTruthSelector
  , pingSelector


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

import ObjC.SyncServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ beginSessionWithClient:entityNames:beforeDate:@
beginSessionWithClient_entityNames_beforeDate :: (IsISyncClient client, IsNSArray entityNames, IsNSDate date) => client -> entityNames -> date -> IO (Id ISyncSession)
beginSessionWithClient_entityNames_beforeDate client entityNames date =
  do
    cls' <- getRequiredClass "ISyncSession"
    withObjCPtr client $ \raw_client ->
      withObjCPtr entityNames $ \raw_entityNames ->
        withObjCPtr date $ \raw_date ->
          sendClassMsg cls' (mkSelector "beginSessionWithClient:entityNames:beforeDate:") (retPtr retVoid) [argPtr (castPtr raw_client :: Ptr ()), argPtr (castPtr raw_entityNames :: Ptr ()), argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @+ beginSessionInBackgroundWithClient:entityNames:target:selector:@
beginSessionInBackgroundWithClient_entityNames_target_selector :: (IsISyncClient client, IsNSArray entityNames) => client -> entityNames -> RawId -> Selector -> IO ()
beginSessionInBackgroundWithClient_entityNames_target_selector client entityNames target selector =
  do
    cls' <- getRequiredClass "ISyncSession"
    withObjCPtr client $ \raw_client ->
      withObjCPtr entityNames $ \raw_entityNames ->
        sendClassMsg cls' (mkSelector "beginSessionInBackgroundWithClient:entityNames:target:selector:") retVoid [argPtr (castPtr raw_client :: Ptr ()), argPtr (castPtr raw_entityNames :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector selector)]

-- | @+ cancelPreviousBeginSessionWithClient:@
cancelPreviousBeginSessionWithClient :: IsISyncClient client => client -> IO ()
cancelPreviousBeginSessionWithClient client =
  do
    cls' <- getRequiredClass "ISyncSession"
    withObjCPtr client $ \raw_client ->
      sendClassMsg cls' (mkSelector "cancelPreviousBeginSessionWithClient:") retVoid [argPtr (castPtr raw_client :: Ptr ())]

-- | @+ beginSessionWithClient:entityNames:beforeDate:lastAnchors:@
beginSessionWithClient_entityNames_beforeDate_lastAnchors :: (IsISyncClient client, IsNSArray entityNames, IsNSDate date, IsNSDictionary anchors) => client -> entityNames -> date -> anchors -> IO (Id ISyncSession)
beginSessionWithClient_entityNames_beforeDate_lastAnchors client entityNames date anchors =
  do
    cls' <- getRequiredClass "ISyncSession"
    withObjCPtr client $ \raw_client ->
      withObjCPtr entityNames $ \raw_entityNames ->
        withObjCPtr date $ \raw_date ->
          withObjCPtr anchors $ \raw_anchors ->
            sendClassMsg cls' (mkSelector "beginSessionWithClient:entityNames:beforeDate:lastAnchors:") (retPtr retVoid) [argPtr (castPtr raw_client :: Ptr ()), argPtr (castPtr raw_entityNames :: Ptr ()), argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr raw_anchors :: Ptr ())] >>= retainedObject . castPtr

-- | @+ beginSessionInBackgroundWithClient:entityNames:target:selector:lastAnchors:@
beginSessionInBackgroundWithClient_entityNames_target_selector_lastAnchors :: (IsISyncClient client, IsNSArray entityNames, IsNSDictionary anchors) => client -> entityNames -> RawId -> Selector -> anchors -> IO ()
beginSessionInBackgroundWithClient_entityNames_target_selector_lastAnchors client entityNames target selector anchors =
  do
    cls' <- getRequiredClass "ISyncSession"
    withObjCPtr client $ \raw_client ->
      withObjCPtr entityNames $ \raw_entityNames ->
        withObjCPtr anchors $ \raw_anchors ->
          sendClassMsg cls' (mkSelector "beginSessionInBackgroundWithClient:entityNames:target:selector:lastAnchors:") retVoid [argPtr (castPtr raw_client :: Ptr ()), argPtr (castPtr raw_entityNames :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector selector), argPtr (castPtr raw_anchors :: Ptr ())]

-- | @- clientDidResetEntityNames:@
clientDidResetEntityNames :: (IsISyncSession iSyncSession, IsNSArray entityNames) => iSyncSession -> entityNames -> IO ()
clientDidResetEntityNames iSyncSession  entityNames =
withObjCPtr entityNames $ \raw_entityNames ->
    sendMsg iSyncSession (mkSelector "clientDidResetEntityNames:") retVoid [argPtr (castPtr raw_entityNames :: Ptr ())]

-- | @- clientWantsToPushAllRecordsForEntityNames:@
clientWantsToPushAllRecordsForEntityNames :: (IsISyncSession iSyncSession, IsNSArray entityNames) => iSyncSession -> entityNames -> IO ()
clientWantsToPushAllRecordsForEntityNames iSyncSession  entityNames =
withObjCPtr entityNames $ \raw_entityNames ->
    sendMsg iSyncSession (mkSelector "clientWantsToPushAllRecordsForEntityNames:") retVoid [argPtr (castPtr raw_entityNames :: Ptr ())]

-- | @- shouldPushChangesForEntityName:@
shouldPushChangesForEntityName :: (IsISyncSession iSyncSession, IsNSString entityName) => iSyncSession -> entityName -> IO Bool
shouldPushChangesForEntityName iSyncSession  entityName =
withObjCPtr entityName $ \raw_entityName ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncSession (mkSelector "shouldPushChangesForEntityName:") retCULong [argPtr (castPtr raw_entityName :: Ptr ())]

-- | @- shouldPushAllRecordsForEntityName:@
shouldPushAllRecordsForEntityName :: (IsISyncSession iSyncSession, IsNSString entityName) => iSyncSession -> entityName -> IO Bool
shouldPushAllRecordsForEntityName iSyncSession  entityName =
withObjCPtr entityName $ \raw_entityName ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncSession (mkSelector "shouldPushAllRecordsForEntityName:") retCULong [argPtr (castPtr raw_entityName :: Ptr ())]

-- | @- shouldPullChangesForEntityName:@
shouldPullChangesForEntityName :: (IsISyncSession iSyncSession, IsNSString entityName) => iSyncSession -> entityName -> IO Bool
shouldPullChangesForEntityName iSyncSession  entityName =
withObjCPtr entityName $ \raw_entityName ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncSession (mkSelector "shouldPullChangesForEntityName:") retCULong [argPtr (castPtr raw_entityName :: Ptr ())]

-- | @- shouldReplaceAllRecordsOnClientForEntityName:@
shouldReplaceAllRecordsOnClientForEntityName :: (IsISyncSession iSyncSession, IsNSString entityName) => iSyncSession -> entityName -> IO Bool
shouldReplaceAllRecordsOnClientForEntityName iSyncSession  entityName =
withObjCPtr entityName $ \raw_entityName ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncSession (mkSelector "shouldReplaceAllRecordsOnClientForEntityName:") retCULong [argPtr (castPtr raw_entityName :: Ptr ())]

-- | @- pushChange:@
pushChange :: (IsISyncSession iSyncSession, IsISyncChange change) => iSyncSession -> change -> IO ()
pushChange iSyncSession  change =
withObjCPtr change $ \raw_change ->
    sendMsg iSyncSession (mkSelector "pushChange:") retVoid [argPtr (castPtr raw_change :: Ptr ())]

-- | @- pushChangesFromRecord:withIdentifier:@
pushChangesFromRecord_withIdentifier :: (IsISyncSession iSyncSession, IsNSDictionary record, IsNSString recordId) => iSyncSession -> record -> recordId -> IO ()
pushChangesFromRecord_withIdentifier iSyncSession  record recordId =
withObjCPtr record $ \raw_record ->
  withObjCPtr recordId $ \raw_recordId ->
      sendMsg iSyncSession (mkSelector "pushChangesFromRecord:withIdentifier:") retVoid [argPtr (castPtr raw_record :: Ptr ()), argPtr (castPtr raw_recordId :: Ptr ())]

-- | @- deleteRecordWithIdentifier:@
deleteRecordWithIdentifier :: (IsISyncSession iSyncSession, IsNSString recordId) => iSyncSession -> recordId -> IO ()
deleteRecordWithIdentifier iSyncSession  recordId =
withObjCPtr recordId $ \raw_recordId ->
    sendMsg iSyncSession (mkSelector "deleteRecordWithIdentifier:") retVoid [argPtr (castPtr raw_recordId :: Ptr ())]

-- | @- clientLostRecordWithIdentifier:shouldReplaceOnNextSync:@
clientLostRecordWithIdentifier_shouldReplaceOnNextSync :: (IsISyncSession iSyncSession, IsNSString recordId) => iSyncSession -> recordId -> Bool -> IO ()
clientLostRecordWithIdentifier_shouldReplaceOnNextSync iSyncSession  recordId flag =
withObjCPtr recordId $ \raw_recordId ->
    sendMsg iSyncSession (mkSelector "clientLostRecordWithIdentifier:shouldReplaceOnNextSync:") retVoid [argPtr (castPtr raw_recordId :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- clientFinishedPushingChangesWithNextAnchors:@
clientFinishedPushingChangesWithNextAnchors :: (IsISyncSession iSyncSession, IsNSDictionary anchors) => iSyncSession -> anchors -> IO ()
clientFinishedPushingChangesWithNextAnchors iSyncSession  anchors =
withObjCPtr anchors $ \raw_anchors ->
    sendMsg iSyncSession (mkSelector "clientFinishedPushingChangesWithNextAnchors:") retVoid [argPtr (castPtr raw_anchors :: Ptr ())]

-- | @- prepareToPullChangesForEntityNames:beforeDate:@
prepareToPullChangesForEntityNames_beforeDate :: (IsISyncSession iSyncSession, IsNSArray entityNames, IsNSDate date) => iSyncSession -> entityNames -> date -> IO Bool
prepareToPullChangesForEntityNames_beforeDate iSyncSession  entityNames date =
withObjCPtr entityNames $ \raw_entityNames ->
  withObjCPtr date $ \raw_date ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncSession (mkSelector "prepareToPullChangesForEntityNames:beforeDate:") retCULong [argPtr (castPtr raw_entityNames :: Ptr ()), argPtr (castPtr raw_date :: Ptr ())]

-- | @- prepareToPullChangesInBackgroundForEntityNames:target:selector:@
prepareToPullChangesInBackgroundForEntityNames_target_selector :: (IsISyncSession iSyncSession, IsNSArray entityNames) => iSyncSession -> entityNames -> RawId -> Selector -> IO ()
prepareToPullChangesInBackgroundForEntityNames_target_selector iSyncSession  entityNames target selector =
withObjCPtr entityNames $ \raw_entityNames ->
    sendMsg iSyncSession (mkSelector "prepareToPullChangesInBackgroundForEntityNames:target:selector:") retVoid [argPtr (castPtr raw_entityNames :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector selector)]

-- | @- changeEnumeratorForEntityNames:@
changeEnumeratorForEntityNames :: (IsISyncSession iSyncSession, IsNSArray entityNames) => iSyncSession -> entityNames -> IO (Id NSEnumerator)
changeEnumeratorForEntityNames iSyncSession  entityNames =
withObjCPtr entityNames $ \raw_entityNames ->
    sendMsg iSyncSession (mkSelector "changeEnumeratorForEntityNames:") (retPtr retVoid) [argPtr (castPtr raw_entityNames :: Ptr ())] >>= retainedObject . castPtr

-- | @- clientAcceptedChangesForRecordWithIdentifier:formattedRecord:newRecordIdentifier:@
clientAcceptedChangesForRecordWithIdentifier_formattedRecord_newRecordIdentifier :: (IsISyncSession iSyncSession, IsNSString recordId, IsNSDictionary formattedRecord, IsNSString recordId) => iSyncSession -> recordId -> formattedRecord -> recordId -> IO ()
clientAcceptedChangesForRecordWithIdentifier_formattedRecord_newRecordIdentifier iSyncSession  recordId formattedRecord recordId =
withObjCPtr recordId $ \raw_recordId ->
  withObjCPtr formattedRecord $ \raw_formattedRecord ->
    withObjCPtr recordId $ \raw_recordId ->
        sendMsg iSyncSession (mkSelector "clientAcceptedChangesForRecordWithIdentifier:formattedRecord:newRecordIdentifier:") retVoid [argPtr (castPtr raw_recordId :: Ptr ()), argPtr (castPtr raw_formattedRecord :: Ptr ()), argPtr (castPtr raw_recordId :: Ptr ())]

-- | @- clientRefusedChangesForRecordWithIdentifier:@
clientRefusedChangesForRecordWithIdentifier :: (IsISyncSession iSyncSession, IsNSString recordId) => iSyncSession -> recordId -> IO ()
clientRefusedChangesForRecordWithIdentifier iSyncSession  recordId =
withObjCPtr recordId $ \raw_recordId ->
    sendMsg iSyncSession (mkSelector "clientRefusedChangesForRecordWithIdentifier:") retVoid [argPtr (castPtr raw_recordId :: Ptr ())]

-- | @- clientCommittedAcceptedChanges@
clientCommittedAcceptedChanges :: IsISyncSession iSyncSession => iSyncSession -> IO ()
clientCommittedAcceptedChanges iSyncSession  =
  sendMsg iSyncSession (mkSelector "clientCommittedAcceptedChanges") retVoid []

-- | @- clientCommittedAcceptedChangesWithNextAnchors:@
clientCommittedAcceptedChangesWithNextAnchors :: (IsISyncSession iSyncSession, IsNSDictionary anchors) => iSyncSession -> anchors -> IO ()
clientCommittedAcceptedChangesWithNextAnchors iSyncSession  anchors =
withObjCPtr anchors $ \raw_anchors ->
    sendMsg iSyncSession (mkSelector "clientCommittedAcceptedChangesWithNextAnchors:") retVoid [argPtr (castPtr raw_anchors :: Ptr ())]

-- | @- clientChangedRecordIdentifiers:@
clientChangedRecordIdentifiers :: (IsISyncSession iSyncSession, IsNSDictionary oldToNew) => iSyncSession -> oldToNew -> IO ()
clientChangedRecordIdentifiers iSyncSession  oldToNew =
withObjCPtr oldToNew $ \raw_oldToNew ->
    sendMsg iSyncSession (mkSelector "clientChangedRecordIdentifiers:") retVoid [argPtr (castPtr raw_oldToNew :: Ptr ())]

-- | @- isCancelled@
isCancelled :: IsISyncSession iSyncSession => iSyncSession -> IO Bool
isCancelled iSyncSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncSession (mkSelector "isCancelled") retCULong []

-- | @- cancelSyncing@
cancelSyncing :: IsISyncSession iSyncSession => iSyncSession -> IO ()
cancelSyncing iSyncSession  =
  sendMsg iSyncSession (mkSelector "cancelSyncing") retVoid []

-- | @- finishSyncing@
finishSyncing :: IsISyncSession iSyncSession => iSyncSession -> IO ()
finishSyncing iSyncSession  =
  sendMsg iSyncSession (mkSelector "finishSyncing") retVoid []

-- | @- clientInfoForRecordWithIdentifier:@
clientInfoForRecordWithIdentifier :: (IsISyncSession iSyncSession, IsNSString recordId) => iSyncSession -> recordId -> IO RawId
clientInfoForRecordWithIdentifier iSyncSession  recordId =
withObjCPtr recordId $ \raw_recordId ->
    fmap (RawId . castPtr) $ sendMsg iSyncSession (mkSelector "clientInfoForRecordWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_recordId :: Ptr ())]

-- | @- setClientInfo:forRecordWithIdentifier:@
setClientInfo_forRecordWithIdentifier :: (IsISyncSession iSyncSession, IsNSString recordId) => iSyncSession -> RawId -> recordId -> IO ()
setClientInfo_forRecordWithIdentifier iSyncSession  clientInfo recordId =
withObjCPtr recordId $ \raw_recordId ->
    sendMsg iSyncSession (mkSelector "setClientInfo:forRecordWithIdentifier:") retVoid [argPtr (castPtr (unRawId clientInfo) :: Ptr ()), argPtr (castPtr raw_recordId :: Ptr ())]

-- | @- snapshotOfRecordsInTruth@
snapshotOfRecordsInTruth :: IsISyncSession iSyncSession => iSyncSession -> IO (Id ISyncRecordSnapshot)
snapshotOfRecordsInTruth iSyncSession  =
  sendMsg iSyncSession (mkSelector "snapshotOfRecordsInTruth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- ping@
ping :: IsISyncSession iSyncSession => iSyncSession -> IO ()
ping iSyncSession  =
  sendMsg iSyncSession (mkSelector "ping") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @beginSessionWithClient:entityNames:beforeDate:@
beginSessionWithClient_entityNames_beforeDateSelector :: Selector
beginSessionWithClient_entityNames_beforeDateSelector = mkSelector "beginSessionWithClient:entityNames:beforeDate:"

-- | @Selector@ for @beginSessionInBackgroundWithClient:entityNames:target:selector:@
beginSessionInBackgroundWithClient_entityNames_target_selectorSelector :: Selector
beginSessionInBackgroundWithClient_entityNames_target_selectorSelector = mkSelector "beginSessionInBackgroundWithClient:entityNames:target:selector:"

-- | @Selector@ for @cancelPreviousBeginSessionWithClient:@
cancelPreviousBeginSessionWithClientSelector :: Selector
cancelPreviousBeginSessionWithClientSelector = mkSelector "cancelPreviousBeginSessionWithClient:"

-- | @Selector@ for @beginSessionWithClient:entityNames:beforeDate:lastAnchors:@
beginSessionWithClient_entityNames_beforeDate_lastAnchorsSelector :: Selector
beginSessionWithClient_entityNames_beforeDate_lastAnchorsSelector = mkSelector "beginSessionWithClient:entityNames:beforeDate:lastAnchors:"

-- | @Selector@ for @beginSessionInBackgroundWithClient:entityNames:target:selector:lastAnchors:@
beginSessionInBackgroundWithClient_entityNames_target_selector_lastAnchorsSelector :: Selector
beginSessionInBackgroundWithClient_entityNames_target_selector_lastAnchorsSelector = mkSelector "beginSessionInBackgroundWithClient:entityNames:target:selector:lastAnchors:"

-- | @Selector@ for @clientDidResetEntityNames:@
clientDidResetEntityNamesSelector :: Selector
clientDidResetEntityNamesSelector = mkSelector "clientDidResetEntityNames:"

-- | @Selector@ for @clientWantsToPushAllRecordsForEntityNames:@
clientWantsToPushAllRecordsForEntityNamesSelector :: Selector
clientWantsToPushAllRecordsForEntityNamesSelector = mkSelector "clientWantsToPushAllRecordsForEntityNames:"

-- | @Selector@ for @shouldPushChangesForEntityName:@
shouldPushChangesForEntityNameSelector :: Selector
shouldPushChangesForEntityNameSelector = mkSelector "shouldPushChangesForEntityName:"

-- | @Selector@ for @shouldPushAllRecordsForEntityName:@
shouldPushAllRecordsForEntityNameSelector :: Selector
shouldPushAllRecordsForEntityNameSelector = mkSelector "shouldPushAllRecordsForEntityName:"

-- | @Selector@ for @shouldPullChangesForEntityName:@
shouldPullChangesForEntityNameSelector :: Selector
shouldPullChangesForEntityNameSelector = mkSelector "shouldPullChangesForEntityName:"

-- | @Selector@ for @shouldReplaceAllRecordsOnClientForEntityName:@
shouldReplaceAllRecordsOnClientForEntityNameSelector :: Selector
shouldReplaceAllRecordsOnClientForEntityNameSelector = mkSelector "shouldReplaceAllRecordsOnClientForEntityName:"

-- | @Selector@ for @pushChange:@
pushChangeSelector :: Selector
pushChangeSelector = mkSelector "pushChange:"

-- | @Selector@ for @pushChangesFromRecord:withIdentifier:@
pushChangesFromRecord_withIdentifierSelector :: Selector
pushChangesFromRecord_withIdentifierSelector = mkSelector "pushChangesFromRecord:withIdentifier:"

-- | @Selector@ for @deleteRecordWithIdentifier:@
deleteRecordWithIdentifierSelector :: Selector
deleteRecordWithIdentifierSelector = mkSelector "deleteRecordWithIdentifier:"

-- | @Selector@ for @clientLostRecordWithIdentifier:shouldReplaceOnNextSync:@
clientLostRecordWithIdentifier_shouldReplaceOnNextSyncSelector :: Selector
clientLostRecordWithIdentifier_shouldReplaceOnNextSyncSelector = mkSelector "clientLostRecordWithIdentifier:shouldReplaceOnNextSync:"

-- | @Selector@ for @clientFinishedPushingChangesWithNextAnchors:@
clientFinishedPushingChangesWithNextAnchorsSelector :: Selector
clientFinishedPushingChangesWithNextAnchorsSelector = mkSelector "clientFinishedPushingChangesWithNextAnchors:"

-- | @Selector@ for @prepareToPullChangesForEntityNames:beforeDate:@
prepareToPullChangesForEntityNames_beforeDateSelector :: Selector
prepareToPullChangesForEntityNames_beforeDateSelector = mkSelector "prepareToPullChangesForEntityNames:beforeDate:"

-- | @Selector@ for @prepareToPullChangesInBackgroundForEntityNames:target:selector:@
prepareToPullChangesInBackgroundForEntityNames_target_selectorSelector :: Selector
prepareToPullChangesInBackgroundForEntityNames_target_selectorSelector = mkSelector "prepareToPullChangesInBackgroundForEntityNames:target:selector:"

-- | @Selector@ for @changeEnumeratorForEntityNames:@
changeEnumeratorForEntityNamesSelector :: Selector
changeEnumeratorForEntityNamesSelector = mkSelector "changeEnumeratorForEntityNames:"

-- | @Selector@ for @clientAcceptedChangesForRecordWithIdentifier:formattedRecord:newRecordIdentifier:@
clientAcceptedChangesForRecordWithIdentifier_formattedRecord_newRecordIdentifierSelector :: Selector
clientAcceptedChangesForRecordWithIdentifier_formattedRecord_newRecordIdentifierSelector = mkSelector "clientAcceptedChangesForRecordWithIdentifier:formattedRecord:newRecordIdentifier:"

-- | @Selector@ for @clientRefusedChangesForRecordWithIdentifier:@
clientRefusedChangesForRecordWithIdentifierSelector :: Selector
clientRefusedChangesForRecordWithIdentifierSelector = mkSelector "clientRefusedChangesForRecordWithIdentifier:"

-- | @Selector@ for @clientCommittedAcceptedChanges@
clientCommittedAcceptedChangesSelector :: Selector
clientCommittedAcceptedChangesSelector = mkSelector "clientCommittedAcceptedChanges"

-- | @Selector@ for @clientCommittedAcceptedChangesWithNextAnchors:@
clientCommittedAcceptedChangesWithNextAnchorsSelector :: Selector
clientCommittedAcceptedChangesWithNextAnchorsSelector = mkSelector "clientCommittedAcceptedChangesWithNextAnchors:"

-- | @Selector@ for @clientChangedRecordIdentifiers:@
clientChangedRecordIdentifiersSelector :: Selector
clientChangedRecordIdentifiersSelector = mkSelector "clientChangedRecordIdentifiers:"

-- | @Selector@ for @isCancelled@
isCancelledSelector :: Selector
isCancelledSelector = mkSelector "isCancelled"

-- | @Selector@ for @cancelSyncing@
cancelSyncingSelector :: Selector
cancelSyncingSelector = mkSelector "cancelSyncing"

-- | @Selector@ for @finishSyncing@
finishSyncingSelector :: Selector
finishSyncingSelector = mkSelector "finishSyncing"

-- | @Selector@ for @clientInfoForRecordWithIdentifier:@
clientInfoForRecordWithIdentifierSelector :: Selector
clientInfoForRecordWithIdentifierSelector = mkSelector "clientInfoForRecordWithIdentifier:"

-- | @Selector@ for @setClientInfo:forRecordWithIdentifier:@
setClientInfo_forRecordWithIdentifierSelector :: Selector
setClientInfo_forRecordWithIdentifierSelector = mkSelector "setClientInfo:forRecordWithIdentifier:"

-- | @Selector@ for @snapshotOfRecordsInTruth@
snapshotOfRecordsInTruthSelector :: Selector
snapshotOfRecordsInTruthSelector = mkSelector "snapshotOfRecordsInTruth"

-- | @Selector@ for @ping@
pingSelector :: Selector
pingSelector = mkSelector "ping"

