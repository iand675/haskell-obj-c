{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ISyncManager@.
module ObjC.SyncServices.ISyncManager
  ( ISyncManager
  , IsISyncManager(..)
  , sharedManager
  , isEnabled
  , syncDisabledReason
  , clientWithIdentifier
  , registerClientWithIdentifier_descriptionFilePath
  , unregisterClient
  , registerSchemaWithBundlePath
  , unregisterSchemaWithName
  , clientWithIdentifier_needsSyncing
  , snapshotOfRecordsInTruthWithEntityNames_usingIdentifiersForClient
  , addRequestMode
  , removeRequestMode
  , requestModes
  , addRequestModeSelector
  , clientWithIdentifierSelector
  , clientWithIdentifier_needsSyncingSelector
  , isEnabledSelector
  , registerClientWithIdentifier_descriptionFilePathSelector
  , registerSchemaWithBundlePathSelector
  , removeRequestModeSelector
  , requestModesSelector
  , sharedManagerSelector
  , snapshotOfRecordsInTruthWithEntityNames_usingIdentifiersForClientSelector
  , syncDisabledReasonSelector
  , unregisterClientSelector
  , unregisterSchemaWithNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SyncServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedManager@
sharedManager :: IO (Id ISyncManager)
sharedManager  =
  do
    cls' <- getRequiredClass "ISyncManager"
    sendClassMessage cls' sharedManagerSelector

-- | @- isEnabled@
isEnabled :: IsISyncManager iSyncManager => iSyncManager -> IO Bool
isEnabled iSyncManager =
  sendMessage iSyncManager isEnabledSelector

-- | @- syncDisabledReason@
syncDisabledReason :: IsISyncManager iSyncManager => iSyncManager -> IO (Id NSError)
syncDisabledReason iSyncManager =
  sendMessage iSyncManager syncDisabledReasonSelector

-- | @- clientWithIdentifier:@
clientWithIdentifier :: (IsISyncManager iSyncManager, IsNSString clientId) => iSyncManager -> clientId -> IO (Id ISyncClient)
clientWithIdentifier iSyncManager clientId =
  sendMessage iSyncManager clientWithIdentifierSelector (toNSString clientId)

-- | @- registerClientWithIdentifier:descriptionFilePath:@
registerClientWithIdentifier_descriptionFilePath :: (IsISyncManager iSyncManager, IsNSString clientId, IsNSString descriptionFilePath) => iSyncManager -> clientId -> descriptionFilePath -> IO (Id ISyncClient)
registerClientWithIdentifier_descriptionFilePath iSyncManager clientId descriptionFilePath =
  sendMessage iSyncManager registerClientWithIdentifier_descriptionFilePathSelector (toNSString clientId) (toNSString descriptionFilePath)

-- | @- unregisterClient:@
unregisterClient :: (IsISyncManager iSyncManager, IsISyncClient client) => iSyncManager -> client -> IO ()
unregisterClient iSyncManager client =
  sendMessage iSyncManager unregisterClientSelector (toISyncClient client)

-- | @- registerSchemaWithBundlePath:@
registerSchemaWithBundlePath :: (IsISyncManager iSyncManager, IsNSString bundlePath) => iSyncManager -> bundlePath -> IO Bool
registerSchemaWithBundlePath iSyncManager bundlePath =
  sendMessage iSyncManager registerSchemaWithBundlePathSelector (toNSString bundlePath)

-- | @- unregisterSchemaWithName:@
unregisterSchemaWithName :: (IsISyncManager iSyncManager, IsNSString schemaName) => iSyncManager -> schemaName -> IO ()
unregisterSchemaWithName iSyncManager schemaName =
  sendMessage iSyncManager unregisterSchemaWithNameSelector (toNSString schemaName)

-- | @- clientWithIdentifier:needsSyncing:@
clientWithIdentifier_needsSyncing :: (IsISyncManager iSyncManager, IsNSString clientId) => iSyncManager -> clientId -> Bool -> IO ()
clientWithIdentifier_needsSyncing iSyncManager clientId flag =
  sendMessage iSyncManager clientWithIdentifier_needsSyncingSelector (toNSString clientId) flag

-- | @- snapshotOfRecordsInTruthWithEntityNames:usingIdentifiersForClient:@
snapshotOfRecordsInTruthWithEntityNames_usingIdentifiersForClient :: (IsISyncManager iSyncManager, IsNSArray entityNames, IsISyncClient client) => iSyncManager -> entityNames -> client -> IO (Id ISyncRecordSnapshot)
snapshotOfRecordsInTruthWithEntityNames_usingIdentifiersForClient iSyncManager entityNames client =
  sendMessage iSyncManager snapshotOfRecordsInTruthWithEntityNames_usingIdentifiersForClientSelector (toNSArray entityNames) (toISyncClient client)

-- | @- addRequestMode:@
addRequestMode :: (IsISyncManager iSyncManager, IsNSString mode) => iSyncManager -> mode -> IO ()
addRequestMode iSyncManager mode =
  sendMessage iSyncManager addRequestModeSelector (toNSString mode)

-- | @- removeRequestMode:@
removeRequestMode :: (IsISyncManager iSyncManager, IsNSString mode) => iSyncManager -> mode -> IO ()
removeRequestMode iSyncManager mode =
  sendMessage iSyncManager removeRequestModeSelector (toNSString mode)

-- | @- requestModes@
requestModes :: IsISyncManager iSyncManager => iSyncManager -> IO (Id NSArray)
requestModes iSyncManager =
  sendMessage iSyncManager requestModesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector '[] (Id ISyncManager)
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @isEnabled@
isEnabledSelector :: Selector '[] Bool
isEnabledSelector = mkSelector "isEnabled"

-- | @Selector@ for @syncDisabledReason@
syncDisabledReasonSelector :: Selector '[] (Id NSError)
syncDisabledReasonSelector = mkSelector "syncDisabledReason"

-- | @Selector@ for @clientWithIdentifier:@
clientWithIdentifierSelector :: Selector '[Id NSString] (Id ISyncClient)
clientWithIdentifierSelector = mkSelector "clientWithIdentifier:"

-- | @Selector@ for @registerClientWithIdentifier:descriptionFilePath:@
registerClientWithIdentifier_descriptionFilePathSelector :: Selector '[Id NSString, Id NSString] (Id ISyncClient)
registerClientWithIdentifier_descriptionFilePathSelector = mkSelector "registerClientWithIdentifier:descriptionFilePath:"

-- | @Selector@ for @unregisterClient:@
unregisterClientSelector :: Selector '[Id ISyncClient] ()
unregisterClientSelector = mkSelector "unregisterClient:"

-- | @Selector@ for @registerSchemaWithBundlePath:@
registerSchemaWithBundlePathSelector :: Selector '[Id NSString] Bool
registerSchemaWithBundlePathSelector = mkSelector "registerSchemaWithBundlePath:"

-- | @Selector@ for @unregisterSchemaWithName:@
unregisterSchemaWithNameSelector :: Selector '[Id NSString] ()
unregisterSchemaWithNameSelector = mkSelector "unregisterSchemaWithName:"

-- | @Selector@ for @clientWithIdentifier:needsSyncing:@
clientWithIdentifier_needsSyncingSelector :: Selector '[Id NSString, Bool] ()
clientWithIdentifier_needsSyncingSelector = mkSelector "clientWithIdentifier:needsSyncing:"

-- | @Selector@ for @snapshotOfRecordsInTruthWithEntityNames:usingIdentifiersForClient:@
snapshotOfRecordsInTruthWithEntityNames_usingIdentifiersForClientSelector :: Selector '[Id NSArray, Id ISyncClient] (Id ISyncRecordSnapshot)
snapshotOfRecordsInTruthWithEntityNames_usingIdentifiersForClientSelector = mkSelector "snapshotOfRecordsInTruthWithEntityNames:usingIdentifiersForClient:"

-- | @Selector@ for @addRequestMode:@
addRequestModeSelector :: Selector '[Id NSString] ()
addRequestModeSelector = mkSelector "addRequestMode:"

-- | @Selector@ for @removeRequestMode:@
removeRequestModeSelector :: Selector '[Id NSString] ()
removeRequestModeSelector = mkSelector "removeRequestMode:"

-- | @Selector@ for @requestModes@
requestModesSelector :: Selector '[] (Id NSArray)
requestModesSelector = mkSelector "requestModes"

