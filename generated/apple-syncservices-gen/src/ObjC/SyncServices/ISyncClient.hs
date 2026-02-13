{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ISyncClient@.
module ObjC.SyncServices.ISyncClient
  ( ISyncClient
  , IsISyncClient(..)
  , clientIdentifier
  , clientType
  , displayName
  , setDisplayName
  , imagePath
  , setImagePath
  , supportedEntityNames
  , canPushChangesForEntityName
  , canPullChangesForEntityName
  , lastSyncDateForEntityName
  , lastSyncStatusForEntityName
  , enabledEntityNames
  , isEnabledForEntityName
  , setEnabled_forEntityNames
  , formatsRelationships
  , setFormatsRelationships
  , shouldReplaceClientRecordsForEntityName
  , setShouldReplaceClientRecords_forEntityNames
  , objectForKey
  , setObject_forKey
  , filters
  , setFilters
  , shouldSynchronizeWithClientsOfType
  , setShouldSynchronize_withClientsOfType
  , syncAlertToolPath
  , setSyncAlertToolPath
  , setSyncAlertHandler_selector
  , canPullChangesForEntityNameSelector
  , canPushChangesForEntityNameSelector
  , clientIdentifierSelector
  , clientTypeSelector
  , displayNameSelector
  , enabledEntityNamesSelector
  , filtersSelector
  , formatsRelationshipsSelector
  , imagePathSelector
  , isEnabledForEntityNameSelector
  , lastSyncDateForEntityNameSelector
  , lastSyncStatusForEntityNameSelector
  , objectForKeySelector
  , setDisplayNameSelector
  , setEnabled_forEntityNamesSelector
  , setFiltersSelector
  , setFormatsRelationshipsSelector
  , setImagePathSelector
  , setObject_forKeySelector
  , setShouldReplaceClientRecords_forEntityNamesSelector
  , setShouldSynchronize_withClientsOfTypeSelector
  , setSyncAlertHandler_selectorSelector
  , setSyncAlertToolPathSelector
  , shouldReplaceClientRecordsForEntityNameSelector
  , shouldSynchronizeWithClientsOfTypeSelector
  , supportedEntityNamesSelector
  , syncAlertToolPathSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SyncServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- clientIdentifier@
clientIdentifier :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSString)
clientIdentifier iSyncClient =
  sendMessage iSyncClient clientIdentifierSelector

-- | @- clientType@
clientType :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSString)
clientType iSyncClient =
  sendMessage iSyncClient clientTypeSelector

-- | @- displayName@
displayName :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSString)
displayName iSyncClient =
  sendMessage iSyncClient displayNameSelector

-- | @- setDisplayName:@
setDisplayName :: (IsISyncClient iSyncClient, IsNSString displayName) => iSyncClient -> displayName -> IO ()
setDisplayName iSyncClient displayName =
  sendMessage iSyncClient setDisplayNameSelector (toNSString displayName)

-- | @- imagePath@
imagePath :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSString)
imagePath iSyncClient =
  sendMessage iSyncClient imagePathSelector

-- | @- setImagePath:@
setImagePath :: (IsISyncClient iSyncClient, IsNSString path) => iSyncClient -> path -> IO ()
setImagePath iSyncClient path =
  sendMessage iSyncClient setImagePathSelector (toNSString path)

-- | @- supportedEntityNames@
supportedEntityNames :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSArray)
supportedEntityNames iSyncClient =
  sendMessage iSyncClient supportedEntityNamesSelector

-- | @- canPushChangesForEntityName:@
canPushChangesForEntityName :: (IsISyncClient iSyncClient, IsNSString entityName) => iSyncClient -> entityName -> IO Bool
canPushChangesForEntityName iSyncClient entityName =
  sendMessage iSyncClient canPushChangesForEntityNameSelector (toNSString entityName)

-- | @- canPullChangesForEntityName:@
canPullChangesForEntityName :: (IsISyncClient iSyncClient, IsNSString entityName) => iSyncClient -> entityName -> IO Bool
canPullChangesForEntityName iSyncClient entityName =
  sendMessage iSyncClient canPullChangesForEntityNameSelector (toNSString entityName)

-- | @- lastSyncDateForEntityName:@
lastSyncDateForEntityName :: (IsISyncClient iSyncClient, IsNSString entityName) => iSyncClient -> entityName -> IO (Id NSDate)
lastSyncDateForEntityName iSyncClient entityName =
  sendMessage iSyncClient lastSyncDateForEntityNameSelector (toNSString entityName)

-- | @- lastSyncStatusForEntityName:@
lastSyncStatusForEntityName :: (IsISyncClient iSyncClient, IsNSString entityName) => iSyncClient -> entityName -> IO CInt
lastSyncStatusForEntityName iSyncClient entityName =
  sendMessage iSyncClient lastSyncStatusForEntityNameSelector (toNSString entityName)

-- | @- enabledEntityNames@
enabledEntityNames :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSArray)
enabledEntityNames iSyncClient =
  sendMessage iSyncClient enabledEntityNamesSelector

-- | @- isEnabledForEntityName:@
isEnabledForEntityName :: (IsISyncClient iSyncClient, IsNSString entityName) => iSyncClient -> entityName -> IO Bool
isEnabledForEntityName iSyncClient entityName =
  sendMessage iSyncClient isEnabledForEntityNameSelector (toNSString entityName)

-- | @- setEnabled:forEntityNames:@
setEnabled_forEntityNames :: (IsISyncClient iSyncClient, IsNSArray entityNames) => iSyncClient -> Bool -> entityNames -> IO ()
setEnabled_forEntityNames iSyncClient flag entityNames =
  sendMessage iSyncClient setEnabled_forEntityNamesSelector flag (toNSArray entityNames)

-- | @- formatsRelationships@
formatsRelationships :: IsISyncClient iSyncClient => iSyncClient -> IO Bool
formatsRelationships iSyncClient =
  sendMessage iSyncClient formatsRelationshipsSelector

-- | @- setFormatsRelationships:@
setFormatsRelationships :: IsISyncClient iSyncClient => iSyncClient -> Bool -> IO ()
setFormatsRelationships iSyncClient flag =
  sendMessage iSyncClient setFormatsRelationshipsSelector flag

-- | @- shouldReplaceClientRecordsForEntityName:@
shouldReplaceClientRecordsForEntityName :: (IsISyncClient iSyncClient, IsNSString entityName) => iSyncClient -> entityName -> IO Bool
shouldReplaceClientRecordsForEntityName iSyncClient entityName =
  sendMessage iSyncClient shouldReplaceClientRecordsForEntityNameSelector (toNSString entityName)

-- | @- setShouldReplaceClientRecords:forEntityNames:@
setShouldReplaceClientRecords_forEntityNames :: (IsISyncClient iSyncClient, IsNSArray entityNames) => iSyncClient -> Bool -> entityNames -> IO ()
setShouldReplaceClientRecords_forEntityNames iSyncClient flag entityNames =
  sendMessage iSyncClient setShouldReplaceClientRecords_forEntityNamesSelector flag (toNSArray entityNames)

-- | @- objectForKey:@
objectForKey :: (IsISyncClient iSyncClient, IsNSString key) => iSyncClient -> key -> IO RawId
objectForKey iSyncClient key =
  sendMessage iSyncClient objectForKeySelector (toNSString key)

-- | @- setObject:forKey:@
setObject_forKey :: (IsISyncClient iSyncClient, IsNSString key) => iSyncClient -> RawId -> key -> IO ()
setObject_forKey iSyncClient value key =
  sendMessage iSyncClient setObject_forKeySelector value (toNSString key)

-- | @- filters@
filters :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSArray)
filters iSyncClient =
  sendMessage iSyncClient filtersSelector

-- | @- setFilters:@
setFilters :: (IsISyncClient iSyncClient, IsNSArray filters) => iSyncClient -> filters -> IO ()
setFilters iSyncClient filters =
  sendMessage iSyncClient setFiltersSelector (toNSArray filters)

-- | @- shouldSynchronizeWithClientsOfType:@
shouldSynchronizeWithClientsOfType :: (IsISyncClient iSyncClient, IsNSString clientType) => iSyncClient -> clientType -> IO Bool
shouldSynchronizeWithClientsOfType iSyncClient clientType =
  sendMessage iSyncClient shouldSynchronizeWithClientsOfTypeSelector (toNSString clientType)

-- | @- setShouldSynchronize:withClientsOfType:@
setShouldSynchronize_withClientsOfType :: (IsISyncClient iSyncClient, IsNSString clientType) => iSyncClient -> Bool -> clientType -> IO ()
setShouldSynchronize_withClientsOfType iSyncClient flag clientType =
  sendMessage iSyncClient setShouldSynchronize_withClientsOfTypeSelector flag (toNSString clientType)

-- | @- syncAlertToolPath@
syncAlertToolPath :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSString)
syncAlertToolPath iSyncClient =
  sendMessage iSyncClient syncAlertToolPathSelector

-- | @- setSyncAlertToolPath:@
setSyncAlertToolPath :: (IsISyncClient iSyncClient, IsNSString path) => iSyncClient -> path -> IO ()
setSyncAlertToolPath iSyncClient path =
  sendMessage iSyncClient setSyncAlertToolPathSelector (toNSString path)

-- | @- setSyncAlertHandler:selector:@
setSyncAlertHandler_selector :: IsISyncClient iSyncClient => iSyncClient -> RawId -> Sel -> IO ()
setSyncAlertHandler_selector iSyncClient handler selector =
  sendMessage iSyncClient setSyncAlertHandler_selectorSelector handler selector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clientIdentifier@
clientIdentifierSelector :: Selector '[] (Id NSString)
clientIdentifierSelector = mkSelector "clientIdentifier"

-- | @Selector@ for @clientType@
clientTypeSelector :: Selector '[] (Id NSString)
clientTypeSelector = mkSelector "clientType"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @setDisplayName:@
setDisplayNameSelector :: Selector '[Id NSString] ()
setDisplayNameSelector = mkSelector "setDisplayName:"

-- | @Selector@ for @imagePath@
imagePathSelector :: Selector '[] (Id NSString)
imagePathSelector = mkSelector "imagePath"

-- | @Selector@ for @setImagePath:@
setImagePathSelector :: Selector '[Id NSString] ()
setImagePathSelector = mkSelector "setImagePath:"

-- | @Selector@ for @supportedEntityNames@
supportedEntityNamesSelector :: Selector '[] (Id NSArray)
supportedEntityNamesSelector = mkSelector "supportedEntityNames"

-- | @Selector@ for @canPushChangesForEntityName:@
canPushChangesForEntityNameSelector :: Selector '[Id NSString] Bool
canPushChangesForEntityNameSelector = mkSelector "canPushChangesForEntityName:"

-- | @Selector@ for @canPullChangesForEntityName:@
canPullChangesForEntityNameSelector :: Selector '[Id NSString] Bool
canPullChangesForEntityNameSelector = mkSelector "canPullChangesForEntityName:"

-- | @Selector@ for @lastSyncDateForEntityName:@
lastSyncDateForEntityNameSelector :: Selector '[Id NSString] (Id NSDate)
lastSyncDateForEntityNameSelector = mkSelector "lastSyncDateForEntityName:"

-- | @Selector@ for @lastSyncStatusForEntityName:@
lastSyncStatusForEntityNameSelector :: Selector '[Id NSString] CInt
lastSyncStatusForEntityNameSelector = mkSelector "lastSyncStatusForEntityName:"

-- | @Selector@ for @enabledEntityNames@
enabledEntityNamesSelector :: Selector '[] (Id NSArray)
enabledEntityNamesSelector = mkSelector "enabledEntityNames"

-- | @Selector@ for @isEnabledForEntityName:@
isEnabledForEntityNameSelector :: Selector '[Id NSString] Bool
isEnabledForEntityNameSelector = mkSelector "isEnabledForEntityName:"

-- | @Selector@ for @setEnabled:forEntityNames:@
setEnabled_forEntityNamesSelector :: Selector '[Bool, Id NSArray] ()
setEnabled_forEntityNamesSelector = mkSelector "setEnabled:forEntityNames:"

-- | @Selector@ for @formatsRelationships@
formatsRelationshipsSelector :: Selector '[] Bool
formatsRelationshipsSelector = mkSelector "formatsRelationships"

-- | @Selector@ for @setFormatsRelationships:@
setFormatsRelationshipsSelector :: Selector '[Bool] ()
setFormatsRelationshipsSelector = mkSelector "setFormatsRelationships:"

-- | @Selector@ for @shouldReplaceClientRecordsForEntityName:@
shouldReplaceClientRecordsForEntityNameSelector :: Selector '[Id NSString] Bool
shouldReplaceClientRecordsForEntityNameSelector = mkSelector "shouldReplaceClientRecordsForEntityName:"

-- | @Selector@ for @setShouldReplaceClientRecords:forEntityNames:@
setShouldReplaceClientRecords_forEntityNamesSelector :: Selector '[Bool, Id NSArray] ()
setShouldReplaceClientRecords_forEntityNamesSelector = mkSelector "setShouldReplaceClientRecords:forEntityNames:"

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector '[Id NSString] RawId
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @setObject:forKey:@
setObject_forKeySelector :: Selector '[RawId, Id NSString] ()
setObject_forKeySelector = mkSelector "setObject:forKey:"

-- | @Selector@ for @filters@
filtersSelector :: Selector '[] (Id NSArray)
filtersSelector = mkSelector "filters"

-- | @Selector@ for @setFilters:@
setFiltersSelector :: Selector '[Id NSArray] ()
setFiltersSelector = mkSelector "setFilters:"

-- | @Selector@ for @shouldSynchronizeWithClientsOfType:@
shouldSynchronizeWithClientsOfTypeSelector :: Selector '[Id NSString] Bool
shouldSynchronizeWithClientsOfTypeSelector = mkSelector "shouldSynchronizeWithClientsOfType:"

-- | @Selector@ for @setShouldSynchronize:withClientsOfType:@
setShouldSynchronize_withClientsOfTypeSelector :: Selector '[Bool, Id NSString] ()
setShouldSynchronize_withClientsOfTypeSelector = mkSelector "setShouldSynchronize:withClientsOfType:"

-- | @Selector@ for @syncAlertToolPath@
syncAlertToolPathSelector :: Selector '[] (Id NSString)
syncAlertToolPathSelector = mkSelector "syncAlertToolPath"

-- | @Selector@ for @setSyncAlertToolPath:@
setSyncAlertToolPathSelector :: Selector '[Id NSString] ()
setSyncAlertToolPathSelector = mkSelector "setSyncAlertToolPath:"

-- | @Selector@ for @setSyncAlertHandler:selector:@
setSyncAlertHandler_selectorSelector :: Selector '[RawId, Sel] ()
setSyncAlertHandler_selectorSelector = mkSelector "setSyncAlertHandler:selector:"

