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
  , sharedManagerSelector
  , isEnabledSelector
  , syncDisabledReasonSelector
  , clientWithIdentifierSelector
  , registerClientWithIdentifier_descriptionFilePathSelector
  , unregisterClientSelector
  , registerSchemaWithBundlePathSelector
  , unregisterSchemaWithNameSelector
  , clientWithIdentifier_needsSyncingSelector
  , snapshotOfRecordsInTruthWithEntityNames_usingIdentifiersForClientSelector
  , addRequestModeSelector
  , removeRequestModeSelector
  , requestModesSelector


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

-- | @+ sharedManager@
sharedManager :: IO (Id ISyncManager)
sharedManager  =
  do
    cls' <- getRequiredClass "ISyncManager"
    sendClassMsg cls' (mkSelector "sharedManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- isEnabled@
isEnabled :: IsISyncManager iSyncManager => iSyncManager -> IO Bool
isEnabled iSyncManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncManager (mkSelector "isEnabled") retCULong []

-- | @- syncDisabledReason@
syncDisabledReason :: IsISyncManager iSyncManager => iSyncManager -> IO (Id NSError)
syncDisabledReason iSyncManager  =
  sendMsg iSyncManager (mkSelector "syncDisabledReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- clientWithIdentifier:@
clientWithIdentifier :: (IsISyncManager iSyncManager, IsNSString clientId) => iSyncManager -> clientId -> IO (Id ISyncClient)
clientWithIdentifier iSyncManager  clientId =
withObjCPtr clientId $ \raw_clientId ->
    sendMsg iSyncManager (mkSelector "clientWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_clientId :: Ptr ())] >>= retainedObject . castPtr

-- | @- registerClientWithIdentifier:descriptionFilePath:@
registerClientWithIdentifier_descriptionFilePath :: (IsISyncManager iSyncManager, IsNSString clientId, IsNSString descriptionFilePath) => iSyncManager -> clientId -> descriptionFilePath -> IO (Id ISyncClient)
registerClientWithIdentifier_descriptionFilePath iSyncManager  clientId descriptionFilePath =
withObjCPtr clientId $ \raw_clientId ->
  withObjCPtr descriptionFilePath $ \raw_descriptionFilePath ->
      sendMsg iSyncManager (mkSelector "registerClientWithIdentifier:descriptionFilePath:") (retPtr retVoid) [argPtr (castPtr raw_clientId :: Ptr ()), argPtr (castPtr raw_descriptionFilePath :: Ptr ())] >>= retainedObject . castPtr

-- | @- unregisterClient:@
unregisterClient :: (IsISyncManager iSyncManager, IsISyncClient client) => iSyncManager -> client -> IO ()
unregisterClient iSyncManager  client =
withObjCPtr client $ \raw_client ->
    sendMsg iSyncManager (mkSelector "unregisterClient:") retVoid [argPtr (castPtr raw_client :: Ptr ())]

-- | @- registerSchemaWithBundlePath:@
registerSchemaWithBundlePath :: (IsISyncManager iSyncManager, IsNSString bundlePath) => iSyncManager -> bundlePath -> IO Bool
registerSchemaWithBundlePath iSyncManager  bundlePath =
withObjCPtr bundlePath $ \raw_bundlePath ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncManager (mkSelector "registerSchemaWithBundlePath:") retCULong [argPtr (castPtr raw_bundlePath :: Ptr ())]

-- | @- unregisterSchemaWithName:@
unregisterSchemaWithName :: (IsISyncManager iSyncManager, IsNSString schemaName) => iSyncManager -> schemaName -> IO ()
unregisterSchemaWithName iSyncManager  schemaName =
withObjCPtr schemaName $ \raw_schemaName ->
    sendMsg iSyncManager (mkSelector "unregisterSchemaWithName:") retVoid [argPtr (castPtr raw_schemaName :: Ptr ())]

-- | @- clientWithIdentifier:needsSyncing:@
clientWithIdentifier_needsSyncing :: (IsISyncManager iSyncManager, IsNSString clientId) => iSyncManager -> clientId -> Bool -> IO ()
clientWithIdentifier_needsSyncing iSyncManager  clientId flag =
withObjCPtr clientId $ \raw_clientId ->
    sendMsg iSyncManager (mkSelector "clientWithIdentifier:needsSyncing:") retVoid [argPtr (castPtr raw_clientId :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- snapshotOfRecordsInTruthWithEntityNames:usingIdentifiersForClient:@
snapshotOfRecordsInTruthWithEntityNames_usingIdentifiersForClient :: (IsISyncManager iSyncManager, IsNSArray entityNames, IsISyncClient client) => iSyncManager -> entityNames -> client -> IO (Id ISyncRecordSnapshot)
snapshotOfRecordsInTruthWithEntityNames_usingIdentifiersForClient iSyncManager  entityNames client =
withObjCPtr entityNames $ \raw_entityNames ->
  withObjCPtr client $ \raw_client ->
      sendMsg iSyncManager (mkSelector "snapshotOfRecordsInTruthWithEntityNames:usingIdentifiersForClient:") (retPtr retVoid) [argPtr (castPtr raw_entityNames :: Ptr ()), argPtr (castPtr raw_client :: Ptr ())] >>= retainedObject . castPtr

-- | @- addRequestMode:@
addRequestMode :: (IsISyncManager iSyncManager, IsNSString mode) => iSyncManager -> mode -> IO ()
addRequestMode iSyncManager  mode =
withObjCPtr mode $ \raw_mode ->
    sendMsg iSyncManager (mkSelector "addRequestMode:") retVoid [argPtr (castPtr raw_mode :: Ptr ())]

-- | @- removeRequestMode:@
removeRequestMode :: (IsISyncManager iSyncManager, IsNSString mode) => iSyncManager -> mode -> IO ()
removeRequestMode iSyncManager  mode =
withObjCPtr mode $ \raw_mode ->
    sendMsg iSyncManager (mkSelector "removeRequestMode:") retVoid [argPtr (castPtr raw_mode :: Ptr ())]

-- | @- requestModes@
requestModes :: IsISyncManager iSyncManager => iSyncManager -> IO (Id NSArray)
requestModes iSyncManager  =
  sendMsg iSyncManager (mkSelector "requestModes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @isEnabled@
isEnabledSelector :: Selector
isEnabledSelector = mkSelector "isEnabled"

-- | @Selector@ for @syncDisabledReason@
syncDisabledReasonSelector :: Selector
syncDisabledReasonSelector = mkSelector "syncDisabledReason"

-- | @Selector@ for @clientWithIdentifier:@
clientWithIdentifierSelector :: Selector
clientWithIdentifierSelector = mkSelector "clientWithIdentifier:"

-- | @Selector@ for @registerClientWithIdentifier:descriptionFilePath:@
registerClientWithIdentifier_descriptionFilePathSelector :: Selector
registerClientWithIdentifier_descriptionFilePathSelector = mkSelector "registerClientWithIdentifier:descriptionFilePath:"

-- | @Selector@ for @unregisterClient:@
unregisterClientSelector :: Selector
unregisterClientSelector = mkSelector "unregisterClient:"

-- | @Selector@ for @registerSchemaWithBundlePath:@
registerSchemaWithBundlePathSelector :: Selector
registerSchemaWithBundlePathSelector = mkSelector "registerSchemaWithBundlePath:"

-- | @Selector@ for @unregisterSchemaWithName:@
unregisterSchemaWithNameSelector :: Selector
unregisterSchemaWithNameSelector = mkSelector "unregisterSchemaWithName:"

-- | @Selector@ for @clientWithIdentifier:needsSyncing:@
clientWithIdentifier_needsSyncingSelector :: Selector
clientWithIdentifier_needsSyncingSelector = mkSelector "clientWithIdentifier:needsSyncing:"

-- | @Selector@ for @snapshotOfRecordsInTruthWithEntityNames:usingIdentifiersForClient:@
snapshotOfRecordsInTruthWithEntityNames_usingIdentifiersForClientSelector :: Selector
snapshotOfRecordsInTruthWithEntityNames_usingIdentifiersForClientSelector = mkSelector "snapshotOfRecordsInTruthWithEntityNames:usingIdentifiersForClient:"

-- | @Selector@ for @addRequestMode:@
addRequestModeSelector :: Selector
addRequestModeSelector = mkSelector "addRequestMode:"

-- | @Selector@ for @removeRequestMode:@
removeRequestModeSelector :: Selector
removeRequestModeSelector = mkSelector "removeRequestMode:"

-- | @Selector@ for @requestModes@
requestModesSelector :: Selector
requestModesSelector = mkSelector "requestModes"

