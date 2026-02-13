{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentCloudKitContainer@.
module ObjC.CoreData.NSPersistentCloudKitContainer
  ( NSPersistentCloudKitContainer
  , IsNSPersistentCloudKitContainer(..)
  , initializeCloudKitSchemaWithOptions_error
  , recordForManagedObjectID
  , recordsForManagedObjectIDs
  , recordIDForManagedObjectID
  , recordIDsForManagedObjectIDs
  , canUpdateRecordForManagedObjectWithID
  , canDeleteRecordForManagedObjectWithID
  , canModifyManagedObjectsInStore
  , purgeObjectsAndRecordsInZoneWithID_inPersistentStore_completion
  , persistUpdatedShare_inPersistentStore_completion
  , fetchSharesMatchingObjectIDs_error
  , fetchSharesInPersistentStore_error
  , canDeleteRecordForManagedObjectWithIDSelector
  , canModifyManagedObjectsInStoreSelector
  , canUpdateRecordForManagedObjectWithIDSelector
  , fetchSharesInPersistentStore_errorSelector
  , fetchSharesMatchingObjectIDs_errorSelector
  , initializeCloudKitSchemaWithOptions_errorSelector
  , persistUpdatedShare_inPersistentStore_completionSelector
  , purgeObjectsAndRecordsInZoneWithID_inPersistentStore_completionSelector
  , recordForManagedObjectIDSelector
  , recordIDForManagedObjectIDSelector
  , recordIDsForManagedObjectIDsSelector
  , recordsForManagedObjectIDsSelector

  -- * Enum types
  , NSPersistentCloudKitContainerSchemaInitializationOptions(NSPersistentCloudKitContainerSchemaInitializationOptions)
  , pattern NSPersistentCloudKitContainerSchemaInitializationOptionsNone
  , pattern NSPersistentCloudKitContainerSchemaInitializationOptionsDryRun
  , pattern NSPersistentCloudKitContainerSchemaInitializationOptionsPrintSchema

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initializeCloudKitSchemaWithOptions:error:@
initializeCloudKitSchemaWithOptions_error :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSError error_) => nsPersistentCloudKitContainer -> NSPersistentCloudKitContainerSchemaInitializationOptions -> error_ -> IO Bool
initializeCloudKitSchemaWithOptions_error nsPersistentCloudKitContainer options error_ =
  sendOwnedMessage nsPersistentCloudKitContainer initializeCloudKitSchemaWithOptions_errorSelector options (toNSError error_)

-- | @- recordForManagedObjectID:@
recordForManagedObjectID :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSManagedObjectID managedObjectID) => nsPersistentCloudKitContainer -> managedObjectID -> IO (Id CKRecord)
recordForManagedObjectID nsPersistentCloudKitContainer managedObjectID =
  sendMessage nsPersistentCloudKitContainer recordForManagedObjectIDSelector (toNSManagedObjectID managedObjectID)

-- | @- recordsForManagedObjectIDs:@
recordsForManagedObjectIDs :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSArray managedObjectIDs) => nsPersistentCloudKitContainer -> managedObjectIDs -> IO (Id NSDictionary)
recordsForManagedObjectIDs nsPersistentCloudKitContainer managedObjectIDs =
  sendMessage nsPersistentCloudKitContainer recordsForManagedObjectIDsSelector (toNSArray managedObjectIDs)

-- | @- recordIDForManagedObjectID:@
recordIDForManagedObjectID :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSManagedObjectID managedObjectID) => nsPersistentCloudKitContainer -> managedObjectID -> IO (Id CKRecordID)
recordIDForManagedObjectID nsPersistentCloudKitContainer managedObjectID =
  sendMessage nsPersistentCloudKitContainer recordIDForManagedObjectIDSelector (toNSManagedObjectID managedObjectID)

-- | @- recordIDsForManagedObjectIDs:@
recordIDsForManagedObjectIDs :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSArray managedObjectIDs) => nsPersistentCloudKitContainer -> managedObjectIDs -> IO (Id NSDictionary)
recordIDsForManagedObjectIDs nsPersistentCloudKitContainer managedObjectIDs =
  sendMessage nsPersistentCloudKitContainer recordIDsForManagedObjectIDsSelector (toNSArray managedObjectIDs)

-- | @- canUpdateRecordForManagedObjectWithID:@
canUpdateRecordForManagedObjectWithID :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSManagedObjectID objectID) => nsPersistentCloudKitContainer -> objectID -> IO Bool
canUpdateRecordForManagedObjectWithID nsPersistentCloudKitContainer objectID =
  sendMessage nsPersistentCloudKitContainer canUpdateRecordForManagedObjectWithIDSelector (toNSManagedObjectID objectID)

-- | @- canDeleteRecordForManagedObjectWithID:@
canDeleteRecordForManagedObjectWithID :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSManagedObjectID objectID) => nsPersistentCloudKitContainer -> objectID -> IO Bool
canDeleteRecordForManagedObjectWithID nsPersistentCloudKitContainer objectID =
  sendMessage nsPersistentCloudKitContainer canDeleteRecordForManagedObjectWithIDSelector (toNSManagedObjectID objectID)

-- | @- canModifyManagedObjectsInStore:@
canModifyManagedObjectsInStore :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSPersistentStore store) => nsPersistentCloudKitContainer -> store -> IO Bool
canModifyManagedObjectsInStore nsPersistentCloudKitContainer store =
  sendMessage nsPersistentCloudKitContainer canModifyManagedObjectsInStoreSelector (toNSPersistentStore store)

-- | @- purgeObjectsAndRecordsInZoneWithID:inPersistentStore:completion:@
purgeObjectsAndRecordsInZoneWithID_inPersistentStore_completion :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsCKRecordZoneID zoneID, IsNSPersistentStore persistentStore) => nsPersistentCloudKitContainer -> zoneID -> persistentStore -> Ptr () -> IO ()
purgeObjectsAndRecordsInZoneWithID_inPersistentStore_completion nsPersistentCloudKitContainer zoneID persistentStore completion =
  sendMessage nsPersistentCloudKitContainer purgeObjectsAndRecordsInZoneWithID_inPersistentStore_completionSelector (toCKRecordZoneID zoneID) (toNSPersistentStore persistentStore) completion

-- | @- persistUpdatedShare:inPersistentStore:completion:@
persistUpdatedShare_inPersistentStore_completion :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsCKShare share, IsNSPersistentStore persistentStore) => nsPersistentCloudKitContainer -> share -> persistentStore -> Ptr () -> IO ()
persistUpdatedShare_inPersistentStore_completion nsPersistentCloudKitContainer share persistentStore completion =
  sendMessage nsPersistentCloudKitContainer persistUpdatedShare_inPersistentStore_completionSelector (toCKShare share) (toNSPersistentStore persistentStore) completion

-- | @- fetchSharesMatchingObjectIDs:error:@
fetchSharesMatchingObjectIDs_error :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSArray objectIDs, IsNSError error_) => nsPersistentCloudKitContainer -> objectIDs -> error_ -> IO (Id NSDictionary)
fetchSharesMatchingObjectIDs_error nsPersistentCloudKitContainer objectIDs error_ =
  sendMessage nsPersistentCloudKitContainer fetchSharesMatchingObjectIDs_errorSelector (toNSArray objectIDs) (toNSError error_)

-- | @- fetchSharesInPersistentStore:error:@
fetchSharesInPersistentStore_error :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSPersistentStore persistentStore, IsNSError error_) => nsPersistentCloudKitContainer -> persistentStore -> error_ -> IO (Id NSArray)
fetchSharesInPersistentStore_error nsPersistentCloudKitContainer persistentStore error_ =
  sendMessage nsPersistentCloudKitContainer fetchSharesInPersistentStore_errorSelector (toNSPersistentStore persistentStore) (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initializeCloudKitSchemaWithOptions:error:@
initializeCloudKitSchemaWithOptions_errorSelector :: Selector '[NSPersistentCloudKitContainerSchemaInitializationOptions, Id NSError] Bool
initializeCloudKitSchemaWithOptions_errorSelector = mkSelector "initializeCloudKitSchemaWithOptions:error:"

-- | @Selector@ for @recordForManagedObjectID:@
recordForManagedObjectIDSelector :: Selector '[Id NSManagedObjectID] (Id CKRecord)
recordForManagedObjectIDSelector = mkSelector "recordForManagedObjectID:"

-- | @Selector@ for @recordsForManagedObjectIDs:@
recordsForManagedObjectIDsSelector :: Selector '[Id NSArray] (Id NSDictionary)
recordsForManagedObjectIDsSelector = mkSelector "recordsForManagedObjectIDs:"

-- | @Selector@ for @recordIDForManagedObjectID:@
recordIDForManagedObjectIDSelector :: Selector '[Id NSManagedObjectID] (Id CKRecordID)
recordIDForManagedObjectIDSelector = mkSelector "recordIDForManagedObjectID:"

-- | @Selector@ for @recordIDsForManagedObjectIDs:@
recordIDsForManagedObjectIDsSelector :: Selector '[Id NSArray] (Id NSDictionary)
recordIDsForManagedObjectIDsSelector = mkSelector "recordIDsForManagedObjectIDs:"

-- | @Selector@ for @canUpdateRecordForManagedObjectWithID:@
canUpdateRecordForManagedObjectWithIDSelector :: Selector '[Id NSManagedObjectID] Bool
canUpdateRecordForManagedObjectWithIDSelector = mkSelector "canUpdateRecordForManagedObjectWithID:"

-- | @Selector@ for @canDeleteRecordForManagedObjectWithID:@
canDeleteRecordForManagedObjectWithIDSelector :: Selector '[Id NSManagedObjectID] Bool
canDeleteRecordForManagedObjectWithIDSelector = mkSelector "canDeleteRecordForManagedObjectWithID:"

-- | @Selector@ for @canModifyManagedObjectsInStore:@
canModifyManagedObjectsInStoreSelector :: Selector '[Id NSPersistentStore] Bool
canModifyManagedObjectsInStoreSelector = mkSelector "canModifyManagedObjectsInStore:"

-- | @Selector@ for @purgeObjectsAndRecordsInZoneWithID:inPersistentStore:completion:@
purgeObjectsAndRecordsInZoneWithID_inPersistentStore_completionSelector :: Selector '[Id CKRecordZoneID, Id NSPersistentStore, Ptr ()] ()
purgeObjectsAndRecordsInZoneWithID_inPersistentStore_completionSelector = mkSelector "purgeObjectsAndRecordsInZoneWithID:inPersistentStore:completion:"

-- | @Selector@ for @persistUpdatedShare:inPersistentStore:completion:@
persistUpdatedShare_inPersistentStore_completionSelector :: Selector '[Id CKShare, Id NSPersistentStore, Ptr ()] ()
persistUpdatedShare_inPersistentStore_completionSelector = mkSelector "persistUpdatedShare:inPersistentStore:completion:"

-- | @Selector@ for @fetchSharesMatchingObjectIDs:error:@
fetchSharesMatchingObjectIDs_errorSelector :: Selector '[Id NSArray, Id NSError] (Id NSDictionary)
fetchSharesMatchingObjectIDs_errorSelector = mkSelector "fetchSharesMatchingObjectIDs:error:"

-- | @Selector@ for @fetchSharesInPersistentStore:error:@
fetchSharesInPersistentStore_errorSelector :: Selector '[Id NSPersistentStore, Id NSError] (Id NSArray)
fetchSharesInPersistentStore_errorSelector = mkSelector "fetchSharesInPersistentStore:error:"

