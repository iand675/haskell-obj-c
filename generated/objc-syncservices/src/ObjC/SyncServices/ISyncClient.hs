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
  , clientIdentifierSelector
  , clientTypeSelector
  , displayNameSelector
  , setDisplayNameSelector
  , imagePathSelector
  , setImagePathSelector
  , supportedEntityNamesSelector
  , canPushChangesForEntityNameSelector
  , canPullChangesForEntityNameSelector
  , lastSyncDateForEntityNameSelector
  , lastSyncStatusForEntityNameSelector
  , enabledEntityNamesSelector
  , isEnabledForEntityNameSelector
  , setEnabled_forEntityNamesSelector
  , formatsRelationshipsSelector
  , setFormatsRelationshipsSelector
  , shouldReplaceClientRecordsForEntityNameSelector
  , setShouldReplaceClientRecords_forEntityNamesSelector
  , objectForKeySelector
  , setObject_forKeySelector
  , filtersSelector
  , setFiltersSelector
  , shouldSynchronizeWithClientsOfTypeSelector
  , setShouldSynchronize_withClientsOfTypeSelector
  , syncAlertToolPathSelector
  , setSyncAlertToolPathSelector
  , setSyncAlertHandler_selectorSelector


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

-- | @- clientIdentifier@
clientIdentifier :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSString)
clientIdentifier iSyncClient  =
  sendMsg iSyncClient (mkSelector "clientIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- clientType@
clientType :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSString)
clientType iSyncClient  =
  sendMsg iSyncClient (mkSelector "clientType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- displayName@
displayName :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSString)
displayName iSyncClient  =
  sendMsg iSyncClient (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDisplayName:@
setDisplayName :: (IsISyncClient iSyncClient, IsNSString displayName) => iSyncClient -> displayName -> IO ()
setDisplayName iSyncClient  displayName =
withObjCPtr displayName $ \raw_displayName ->
    sendMsg iSyncClient (mkSelector "setDisplayName:") retVoid [argPtr (castPtr raw_displayName :: Ptr ())]

-- | @- imagePath@
imagePath :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSString)
imagePath iSyncClient  =
  sendMsg iSyncClient (mkSelector "imagePath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImagePath:@
setImagePath :: (IsISyncClient iSyncClient, IsNSString path) => iSyncClient -> path -> IO ()
setImagePath iSyncClient  path =
withObjCPtr path $ \raw_path ->
    sendMsg iSyncClient (mkSelector "setImagePath:") retVoid [argPtr (castPtr raw_path :: Ptr ())]

-- | @- supportedEntityNames@
supportedEntityNames :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSArray)
supportedEntityNames iSyncClient  =
  sendMsg iSyncClient (mkSelector "supportedEntityNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- canPushChangesForEntityName:@
canPushChangesForEntityName :: (IsISyncClient iSyncClient, IsNSString entityName) => iSyncClient -> entityName -> IO Bool
canPushChangesForEntityName iSyncClient  entityName =
withObjCPtr entityName $ \raw_entityName ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncClient (mkSelector "canPushChangesForEntityName:") retCULong [argPtr (castPtr raw_entityName :: Ptr ())]

-- | @- canPullChangesForEntityName:@
canPullChangesForEntityName :: (IsISyncClient iSyncClient, IsNSString entityName) => iSyncClient -> entityName -> IO Bool
canPullChangesForEntityName iSyncClient  entityName =
withObjCPtr entityName $ \raw_entityName ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncClient (mkSelector "canPullChangesForEntityName:") retCULong [argPtr (castPtr raw_entityName :: Ptr ())]

-- | @- lastSyncDateForEntityName:@
lastSyncDateForEntityName :: (IsISyncClient iSyncClient, IsNSString entityName) => iSyncClient -> entityName -> IO (Id NSDate)
lastSyncDateForEntityName iSyncClient  entityName =
withObjCPtr entityName $ \raw_entityName ->
    sendMsg iSyncClient (mkSelector "lastSyncDateForEntityName:") (retPtr retVoid) [argPtr (castPtr raw_entityName :: Ptr ())] >>= retainedObject . castPtr

-- | @- lastSyncStatusForEntityName:@
lastSyncStatusForEntityName :: (IsISyncClient iSyncClient, IsNSString entityName) => iSyncClient -> entityName -> IO CInt
lastSyncStatusForEntityName iSyncClient  entityName =
withObjCPtr entityName $ \raw_entityName ->
    sendMsg iSyncClient (mkSelector "lastSyncStatusForEntityName:") retCInt [argPtr (castPtr raw_entityName :: Ptr ())]

-- | @- enabledEntityNames@
enabledEntityNames :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSArray)
enabledEntityNames iSyncClient  =
  sendMsg iSyncClient (mkSelector "enabledEntityNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- isEnabledForEntityName:@
isEnabledForEntityName :: (IsISyncClient iSyncClient, IsNSString entityName) => iSyncClient -> entityName -> IO Bool
isEnabledForEntityName iSyncClient  entityName =
withObjCPtr entityName $ \raw_entityName ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncClient (mkSelector "isEnabledForEntityName:") retCULong [argPtr (castPtr raw_entityName :: Ptr ())]

-- | @- setEnabled:forEntityNames:@
setEnabled_forEntityNames :: (IsISyncClient iSyncClient, IsNSArray entityNames) => iSyncClient -> Bool -> entityNames -> IO ()
setEnabled_forEntityNames iSyncClient  flag entityNames =
withObjCPtr entityNames $ \raw_entityNames ->
    sendMsg iSyncClient (mkSelector "setEnabled:forEntityNames:") retVoid [argCULong (if flag then 1 else 0), argPtr (castPtr raw_entityNames :: Ptr ())]

-- | @- formatsRelationships@
formatsRelationships :: IsISyncClient iSyncClient => iSyncClient -> IO Bool
formatsRelationships iSyncClient  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncClient (mkSelector "formatsRelationships") retCULong []

-- | @- setFormatsRelationships:@
setFormatsRelationships :: IsISyncClient iSyncClient => iSyncClient -> Bool -> IO ()
setFormatsRelationships iSyncClient  flag =
  sendMsg iSyncClient (mkSelector "setFormatsRelationships:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- shouldReplaceClientRecordsForEntityName:@
shouldReplaceClientRecordsForEntityName :: (IsISyncClient iSyncClient, IsNSString entityName) => iSyncClient -> entityName -> IO Bool
shouldReplaceClientRecordsForEntityName iSyncClient  entityName =
withObjCPtr entityName $ \raw_entityName ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncClient (mkSelector "shouldReplaceClientRecordsForEntityName:") retCULong [argPtr (castPtr raw_entityName :: Ptr ())]

-- | @- setShouldReplaceClientRecords:forEntityNames:@
setShouldReplaceClientRecords_forEntityNames :: (IsISyncClient iSyncClient, IsNSArray entityNames) => iSyncClient -> Bool -> entityNames -> IO ()
setShouldReplaceClientRecords_forEntityNames iSyncClient  flag entityNames =
withObjCPtr entityNames $ \raw_entityNames ->
    sendMsg iSyncClient (mkSelector "setShouldReplaceClientRecords:forEntityNames:") retVoid [argCULong (if flag then 1 else 0), argPtr (castPtr raw_entityNames :: Ptr ())]

-- | @- objectForKey:@
objectForKey :: (IsISyncClient iSyncClient, IsNSString key) => iSyncClient -> key -> IO RawId
objectForKey iSyncClient  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg iSyncClient (mkSelector "objectForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setObject:forKey:@
setObject_forKey :: (IsISyncClient iSyncClient, IsNSString key) => iSyncClient -> RawId -> key -> IO ()
setObject_forKey iSyncClient  value key =
withObjCPtr key $ \raw_key ->
    sendMsg iSyncClient (mkSelector "setObject:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- filters@
filters :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSArray)
filters iSyncClient  =
  sendMsg iSyncClient (mkSelector "filters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFilters:@
setFilters :: (IsISyncClient iSyncClient, IsNSArray filters) => iSyncClient -> filters -> IO ()
setFilters iSyncClient  filters =
withObjCPtr filters $ \raw_filters ->
    sendMsg iSyncClient (mkSelector "setFilters:") retVoid [argPtr (castPtr raw_filters :: Ptr ())]

-- | @- shouldSynchronizeWithClientsOfType:@
shouldSynchronizeWithClientsOfType :: (IsISyncClient iSyncClient, IsNSString clientType) => iSyncClient -> clientType -> IO Bool
shouldSynchronizeWithClientsOfType iSyncClient  clientType =
withObjCPtr clientType $ \raw_clientType ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg iSyncClient (mkSelector "shouldSynchronizeWithClientsOfType:") retCULong [argPtr (castPtr raw_clientType :: Ptr ())]

-- | @- setShouldSynchronize:withClientsOfType:@
setShouldSynchronize_withClientsOfType :: (IsISyncClient iSyncClient, IsNSString clientType) => iSyncClient -> Bool -> clientType -> IO ()
setShouldSynchronize_withClientsOfType iSyncClient  flag clientType =
withObjCPtr clientType $ \raw_clientType ->
    sendMsg iSyncClient (mkSelector "setShouldSynchronize:withClientsOfType:") retVoid [argCULong (if flag then 1 else 0), argPtr (castPtr raw_clientType :: Ptr ())]

-- | @- syncAlertToolPath@
syncAlertToolPath :: IsISyncClient iSyncClient => iSyncClient -> IO (Id NSString)
syncAlertToolPath iSyncClient  =
  sendMsg iSyncClient (mkSelector "syncAlertToolPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSyncAlertToolPath:@
setSyncAlertToolPath :: (IsISyncClient iSyncClient, IsNSString path) => iSyncClient -> path -> IO ()
setSyncAlertToolPath iSyncClient  path =
withObjCPtr path $ \raw_path ->
    sendMsg iSyncClient (mkSelector "setSyncAlertToolPath:") retVoid [argPtr (castPtr raw_path :: Ptr ())]

-- | @- setSyncAlertHandler:selector:@
setSyncAlertHandler_selector :: IsISyncClient iSyncClient => iSyncClient -> RawId -> Selector -> IO ()
setSyncAlertHandler_selector iSyncClient  handler selector =
  sendMsg iSyncClient (mkSelector "setSyncAlertHandler:selector:") retVoid [argPtr (castPtr (unRawId handler) :: Ptr ()), argPtr (unSelector selector)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clientIdentifier@
clientIdentifierSelector :: Selector
clientIdentifierSelector = mkSelector "clientIdentifier"

-- | @Selector@ for @clientType@
clientTypeSelector :: Selector
clientTypeSelector = mkSelector "clientType"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @setDisplayName:@
setDisplayNameSelector :: Selector
setDisplayNameSelector = mkSelector "setDisplayName:"

-- | @Selector@ for @imagePath@
imagePathSelector :: Selector
imagePathSelector = mkSelector "imagePath"

-- | @Selector@ for @setImagePath:@
setImagePathSelector :: Selector
setImagePathSelector = mkSelector "setImagePath:"

-- | @Selector@ for @supportedEntityNames@
supportedEntityNamesSelector :: Selector
supportedEntityNamesSelector = mkSelector "supportedEntityNames"

-- | @Selector@ for @canPushChangesForEntityName:@
canPushChangesForEntityNameSelector :: Selector
canPushChangesForEntityNameSelector = mkSelector "canPushChangesForEntityName:"

-- | @Selector@ for @canPullChangesForEntityName:@
canPullChangesForEntityNameSelector :: Selector
canPullChangesForEntityNameSelector = mkSelector "canPullChangesForEntityName:"

-- | @Selector@ for @lastSyncDateForEntityName:@
lastSyncDateForEntityNameSelector :: Selector
lastSyncDateForEntityNameSelector = mkSelector "lastSyncDateForEntityName:"

-- | @Selector@ for @lastSyncStatusForEntityName:@
lastSyncStatusForEntityNameSelector :: Selector
lastSyncStatusForEntityNameSelector = mkSelector "lastSyncStatusForEntityName:"

-- | @Selector@ for @enabledEntityNames@
enabledEntityNamesSelector :: Selector
enabledEntityNamesSelector = mkSelector "enabledEntityNames"

-- | @Selector@ for @isEnabledForEntityName:@
isEnabledForEntityNameSelector :: Selector
isEnabledForEntityNameSelector = mkSelector "isEnabledForEntityName:"

-- | @Selector@ for @setEnabled:forEntityNames:@
setEnabled_forEntityNamesSelector :: Selector
setEnabled_forEntityNamesSelector = mkSelector "setEnabled:forEntityNames:"

-- | @Selector@ for @formatsRelationships@
formatsRelationshipsSelector :: Selector
formatsRelationshipsSelector = mkSelector "formatsRelationships"

-- | @Selector@ for @setFormatsRelationships:@
setFormatsRelationshipsSelector :: Selector
setFormatsRelationshipsSelector = mkSelector "setFormatsRelationships:"

-- | @Selector@ for @shouldReplaceClientRecordsForEntityName:@
shouldReplaceClientRecordsForEntityNameSelector :: Selector
shouldReplaceClientRecordsForEntityNameSelector = mkSelector "shouldReplaceClientRecordsForEntityName:"

-- | @Selector@ for @setShouldReplaceClientRecords:forEntityNames:@
setShouldReplaceClientRecords_forEntityNamesSelector :: Selector
setShouldReplaceClientRecords_forEntityNamesSelector = mkSelector "setShouldReplaceClientRecords:forEntityNames:"

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @setObject:forKey:@
setObject_forKeySelector :: Selector
setObject_forKeySelector = mkSelector "setObject:forKey:"

-- | @Selector@ for @filters@
filtersSelector :: Selector
filtersSelector = mkSelector "filters"

-- | @Selector@ for @setFilters:@
setFiltersSelector :: Selector
setFiltersSelector = mkSelector "setFilters:"

-- | @Selector@ for @shouldSynchronizeWithClientsOfType:@
shouldSynchronizeWithClientsOfTypeSelector :: Selector
shouldSynchronizeWithClientsOfTypeSelector = mkSelector "shouldSynchronizeWithClientsOfType:"

-- | @Selector@ for @setShouldSynchronize:withClientsOfType:@
setShouldSynchronize_withClientsOfTypeSelector :: Selector
setShouldSynchronize_withClientsOfTypeSelector = mkSelector "setShouldSynchronize:withClientsOfType:"

-- | @Selector@ for @syncAlertToolPath@
syncAlertToolPathSelector :: Selector
syncAlertToolPathSelector = mkSelector "syncAlertToolPath"

-- | @Selector@ for @setSyncAlertToolPath:@
setSyncAlertToolPathSelector :: Selector
setSyncAlertToolPathSelector = mkSelector "setSyncAlertToolPath:"

-- | @Selector@ for @setSyncAlertHandler:selector:@
setSyncAlertHandler_selectorSelector :: Selector
setSyncAlertHandler_selectorSelector = mkSelector "setSyncAlertHandler:selector:"

