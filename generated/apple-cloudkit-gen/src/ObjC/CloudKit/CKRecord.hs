{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKRecord@.
module ObjC.CloudKit.CKRecord
  ( CKRecord
  , IsCKRecord(..)
  , init_
  , new
  , initWithRecordType
  , initWithRecordType_recordID
  , initWithRecordType_zoneID
  , objectForKey
  , setObject_forKey
  , allKeys
  , allTokens
  , objectForKeyedSubscript
  , setObject_forKeyedSubscript
  , changedKeys
  , encodeSystemFieldsWithCoder
  , setParentReferenceFromRecord
  , setParentReferenceFromRecordID
  , recordType
  , recordID
  , recordChangeTag
  , creatorUserRecordID
  , creationDate
  , lastModifiedUserRecordID
  , modificationDate
  , share
  , parent
  , setParent
  , encryptedValues
  , allKeysSelector
  , allTokensSelector
  , changedKeysSelector
  , creationDateSelector
  , creatorUserRecordIDSelector
  , encodeSystemFieldsWithCoderSelector
  , encryptedValuesSelector
  , initSelector
  , initWithRecordTypeSelector
  , initWithRecordType_recordIDSelector
  , initWithRecordType_zoneIDSelector
  , lastModifiedUserRecordIDSelector
  , modificationDateSelector
  , newSelector
  , objectForKeySelector
  , objectForKeyedSubscriptSelector
  , parentSelector
  , recordChangeTagSelector
  , recordIDSelector
  , recordTypeSelector
  , setObject_forKeySelector
  , setObject_forKeyedSubscriptSelector
  , setParentReferenceFromRecordIDSelector
  , setParentReferenceFromRecordSelector
  , setParentSelector
  , shareSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKRecord ckRecord => ckRecord -> IO (Id CKRecord)
init_ ckRecord =
  sendOwnedMessage ckRecord initSelector

-- | @+ new@
new :: IO (Id CKRecord)
new  =
  do
    cls' <- getRequiredClass "CKRecord"
    sendOwnedClassMessage cls' newSelector

-- | This creates the record in the default zone.
--
-- ObjC selector: @- initWithRecordType:@
initWithRecordType :: (IsCKRecord ckRecord, IsNSString recordType) => ckRecord -> recordType -> IO (Id CKRecord)
initWithRecordType ckRecord recordType =
  sendOwnedMessage ckRecord initWithRecordTypeSelector (toNSString recordType)

-- | @- initWithRecordType:recordID:@
initWithRecordType_recordID :: (IsCKRecord ckRecord, IsNSString recordType, IsCKRecordID recordID) => ckRecord -> recordType -> recordID -> IO (Id CKRecord)
initWithRecordType_recordID ckRecord recordType recordID =
  sendOwnedMessage ckRecord initWithRecordType_recordIDSelector (toNSString recordType) (toCKRecordID recordID)

-- | @- initWithRecordType:zoneID:@
initWithRecordType_zoneID :: (IsCKRecord ckRecord, IsNSString recordType, IsCKRecordZoneID zoneID) => ckRecord -> recordType -> zoneID -> IO (Id CKRecord)
initWithRecordType_zoneID ckRecord recordType zoneID =
  sendOwnedMessage ckRecord initWithRecordType_zoneIDSelector (toNSString recordType) (toCKRecordZoneID zoneID)

-- | In addition to @objectForKey:@ and @setObject:forKey:,@ dictionary-style subscripting (@record[key]@ and
--
-- record[key] = value
--
-- ) can be used to get and set values.  Acceptable value object classes are:  - CKReference  - CKAsset  - CLLocation  - NSData  - NSDate  - NSNumber  - NSString  - NSArray containing objects of any of the types above
--
-- Any other classes will result in an exception with name @NSInvalidArgumentException.@
--
-- Whenever possible, value objects will be copied when set on a record.
--
-- Field keys starting with '_' are reserved. Attempting to set a key prefixed with a '_' will result in an error.
--
-- Key names roughly match C variable name restrictions. They must begin with an ASCII letter and can contain ASCII letters and numbers and the underscore character.  The maximum key length is 255 characters.
--
-- ObjC selector: @- objectForKey:@
objectForKey :: (IsCKRecord ckRecord, IsNSString key) => ckRecord -> key -> IO RawId
objectForKey ckRecord key =
  sendMessage ckRecord objectForKeySelector (toNSString key)

-- | @- setObject:forKey:@
setObject_forKey :: (IsCKRecord ckRecord, IsNSString key) => ckRecord -> RawId -> key -> IO ()
setObject_forKey ckRecord object key =
  sendMessage ckRecord setObject_forKeySelector object (toNSString key)

-- | @- allKeys@
allKeys :: IsCKRecord ckRecord => ckRecord -> IO (Id NSArray)
allKeys ckRecord =
  sendMessage ckRecord allKeysSelector

-- | A special property that returns an array of token generated from all the string field values in the record.
--
-- These tokens have been normalized for the current locale, so they are suitable for performing full-text searches.
--
-- ObjC selector: @- allTokens@
allTokens :: IsCKRecord ckRecord => ckRecord -> IO (Id NSArray)
allTokens ckRecord =
  sendMessage ckRecord allTokensSelector

-- | @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsCKRecord ckRecord, IsNSString key) => ckRecord -> key -> IO RawId
objectForKeyedSubscript ckRecord key =
  sendMessage ckRecord objectForKeyedSubscriptSelector (toNSString key)

-- | @- setObject:forKeyedSubscript:@
setObject_forKeyedSubscript :: (IsCKRecord ckRecord, IsNSString key) => ckRecord -> RawId -> key -> IO ()
setObject_forKeyedSubscript ckRecord object key =
  sendMessage ckRecord setObject_forKeyedSubscriptSelector object (toNSString key)

-- | A list of keys that have been modified on the local CKRecord instance
--
-- ObjC selector: @- changedKeys@
changedKeys :: IsCKRecord ckRecord => ckRecord -> IO (Id NSArray)
changedKeys ckRecord =
  sendMessage ckRecord changedKeysSelector

-- | @CKRecord@ supports @NSSecureCoding.@  When you invoke @encodeWithCoder:@ on a @CKRecord,@ it encodes all its values.  Including the record values you've set.  If you want to store a @CKRecord@ instance locally, AND you're already storing the record values locally, that's overkill.  In that case, you can use @encodeSystemFieldsWithCoder:.@  This will encode all parts of a @CKRecord@ except the record keys / values you have access to via the @changedKeys@ and @objectForKey:@ methods.  If you use @initWithCoder:@ to reconstitute a @CKRecord@ you encoded via @encodeSystemFieldsWithCoder:,@ then be aware that  - any record values you had set on the original instance, but had not saved, will be lost  - the reconstituted CKRecord's @changedKeys@ will be empty
--
-- ObjC selector: @- encodeSystemFieldsWithCoder:@
encodeSystemFieldsWithCoder :: (IsCKRecord ckRecord, IsNSCoder coder) => ckRecord -> coder -> IO ()
encodeSystemFieldsWithCoder ckRecord coder =
  sendMessage ckRecord encodeSystemFieldsWithCoderSelector (toNSCoder coder)

-- | Convenience wrappers around creating a @CKReference@ to a parent record. The resulting @CKReference@ will have
--
-- referenceAction = CKReferenceActionNone
--
-- ObjC selector: @- setParentReferenceFromRecord:@
setParentReferenceFromRecord :: (IsCKRecord ckRecord, IsCKRecord parentRecord) => ckRecord -> parentRecord -> IO ()
setParentReferenceFromRecord ckRecord parentRecord =
  sendMessage ckRecord setParentReferenceFromRecordSelector (toCKRecord parentRecord)

-- | @- setParentReferenceFromRecordID:@
setParentReferenceFromRecordID :: (IsCKRecord ckRecord, IsCKRecordID parentRecordID) => ckRecord -> parentRecordID -> IO ()
setParentReferenceFromRecordID ckRecord parentRecordID =
  sendMessage ckRecord setParentReferenceFromRecordIDSelector (toCKRecordID parentRecordID)

-- | @- recordType@
recordType :: IsCKRecord ckRecord => ckRecord -> IO (Id NSString)
recordType ckRecord =
  sendMessage ckRecord recordTypeSelector

-- | @- recordID@
recordID :: IsCKRecord ckRecord => ckRecord -> IO (Id CKRecordID)
recordID ckRecord =
  sendMessage ckRecord recordIDSelector

-- | Change tags are updated by the server to a unique value every time a record is modified.  A different change tag necessarily means that the contents of the record are different.
--
-- ObjC selector: @- recordChangeTag@
recordChangeTag :: IsCKRecord ckRecord => ckRecord -> IO (Id NSString)
recordChangeTag ckRecord =
  sendMessage ckRecord recordChangeTagSelector

-- | This is a User Record recordID, identifying the user that created this record.
--
-- ObjC selector: @- creatorUserRecordID@
creatorUserRecordID :: IsCKRecord ckRecord => ckRecord -> IO (Id CKRecordID)
creatorUserRecordID ckRecord =
  sendMessage ckRecord creatorUserRecordIDSelector

-- | @- creationDate@
creationDate :: IsCKRecord ckRecord => ckRecord -> IO (Id NSDate)
creationDate ckRecord =
  sendMessage ckRecord creationDateSelector

-- | This is a User Record recordID, identifying the user that last modified this record.
--
-- ObjC selector: @- lastModifiedUserRecordID@
lastModifiedUserRecordID :: IsCKRecord ckRecord => ckRecord -> IO (Id CKRecordID)
lastModifiedUserRecordID ckRecord =
  sendMessage ckRecord lastModifiedUserRecordIDSelector

-- | @- modificationDate@
modificationDate :: IsCKRecord ckRecord => ckRecord -> IO (Id NSDate)
modificationDate ckRecord =
  sendMessage ckRecord modificationDateSelector

-- | The share property on a record can be set by creating a share using
--
-- -[CKShare initWithRootRecord:]
--
-- .
--
-- The share property on a record will be removed when the corresponding CKShare is deleted from the server. Send this record in the same batch as the share delete and this record's share property will be updated.
--
-- Sharing is only supported in zones with the @CKRecordZoneCapabilitySharing@ capability. The default zone does not support sharing.
--
-- If any records have a parent reference to this record, they are implicitly shared alongside this record.
--
-- Note that records in a parent chain must only exist within one share. If a child record already has a share reference set then you will get a @CKErrorAlreadyShared@ error if you try to share any of that record's parents.
--
-- Child records can be shared independently, even if they have a common parent.  For example:  Record A has two child records, Record B and Record C.      A     / \\    B   C
--
-- These configurations are supported:  - Record A part of Share 1, or  - Record B part of Share 1, or  - Record C part of Share 1, or  - Record B part of Share 1, Record C part of Share 2
--
-- These configurations are not supported:  Record A part of Share 1, Record B part of Share 2, or    -- This is not allowed because Record B would then be in two shares; Share 1 by being Record A's child, and Share 2  Record A part of Share 1, Record C part of Share 2, or    -- This is not allowed because Record C would then be in two shares; Share 1 by being Record A's child, and Share 2  Record A part of Share 1, Record B part of Share 2, Record C part of Share 3    -- This is not allowed because both Record B and Record C would then each be in two shares.
--
-- Whenever possible, it is suggested that you construct your parent hierarchies such that you will only need to share the topmost record of that hierarchy.
--
-- ObjC selector: @- share@
share :: IsCKRecord ckRecord => ckRecord -> IO (Id CKReference)
share ckRecord =
  sendMessage ckRecord shareSelector

-- | Use a parent reference to teach CloudKit about the hierarchy of your records.
--
-- When a record is shared, all children of that record are also shared.
--
-- A parent record reference must have @CKReferenceActionNone@ set. You can create a separate reference with @CKReferenceActionDeleteSelf@ if you would like your hierarchy cleaned up when the parent record is deleted.
--
-- The target of a parent reference must exist at save time - either already on the server, or part of the same @CKModifyRecordsOperation@ batch.
--
-- You are encouraged to set up the @parent@ relationships as part of normal record saves, even if you're not planning on sharing records at this time.  This allows you to share and unshare a hierarchy of records at a later date by only modifying the "top level" record, setting or clearing its @share@ reference.
--
-- ObjC selector: @- parent@
parent :: IsCKRecord ckRecord => ckRecord -> IO (Id CKReference)
parent ckRecord =
  sendMessage ckRecord parentSelector

-- | Use a parent reference to teach CloudKit about the hierarchy of your records.
--
-- When a record is shared, all children of that record are also shared.
--
-- A parent record reference must have @CKReferenceActionNone@ set. You can create a separate reference with @CKReferenceActionDeleteSelf@ if you would like your hierarchy cleaned up when the parent record is deleted.
--
-- The target of a parent reference must exist at save time - either already on the server, or part of the same @CKModifyRecordsOperation@ batch.
--
-- You are encouraged to set up the @parent@ relationships as part of normal record saves, even if you're not planning on sharing records at this time.  This allows you to share and unshare a hierarchy of records at a later date by only modifying the "top level" record, setting or clearing its @share@ reference.
--
-- ObjC selector: @- setParent:@
setParent :: (IsCKRecord ckRecord, IsCKReference value) => ckRecord -> value -> IO ()
setParent ckRecord value =
  sendMessage ckRecord setParentSelector (toCKReference value)

-- | Any values set here will be locally encrypted before being saved to the server and locally decrypted when fetched from the server. Encryption and decryption is handled by the CloudKit framework. Key material necessary for decryption are available to the owner of the record, as well as any users that can access this record via a CKShare. All CKRecordValue types can be set here except CKAsset and CKReference.
--
-- ObjC selector: @- encryptedValues@
encryptedValues :: IsCKRecord ckRecord => ckRecord -> IO RawId
encryptedValues ckRecord =
  sendMessage ckRecord encryptedValuesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKRecord)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKRecord)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithRecordType:@
initWithRecordTypeSelector :: Selector '[Id NSString] (Id CKRecord)
initWithRecordTypeSelector = mkSelector "initWithRecordType:"

-- | @Selector@ for @initWithRecordType:recordID:@
initWithRecordType_recordIDSelector :: Selector '[Id NSString, Id CKRecordID] (Id CKRecord)
initWithRecordType_recordIDSelector = mkSelector "initWithRecordType:recordID:"

-- | @Selector@ for @initWithRecordType:zoneID:@
initWithRecordType_zoneIDSelector :: Selector '[Id NSString, Id CKRecordZoneID] (Id CKRecord)
initWithRecordType_zoneIDSelector = mkSelector "initWithRecordType:zoneID:"

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector '[Id NSString] RawId
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @setObject:forKey:@
setObject_forKeySelector :: Selector '[RawId, Id NSString] ()
setObject_forKeySelector = mkSelector "setObject:forKey:"

-- | @Selector@ for @allKeys@
allKeysSelector :: Selector '[] (Id NSArray)
allKeysSelector = mkSelector "allKeys"

-- | @Selector@ for @allTokens@
allTokensSelector :: Selector '[] (Id NSArray)
allTokensSelector = mkSelector "allTokens"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector '[Id NSString] RawId
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @setObject:forKeyedSubscript:@
setObject_forKeyedSubscriptSelector :: Selector '[RawId, Id NSString] ()
setObject_forKeyedSubscriptSelector = mkSelector "setObject:forKeyedSubscript:"

-- | @Selector@ for @changedKeys@
changedKeysSelector :: Selector '[] (Id NSArray)
changedKeysSelector = mkSelector "changedKeys"

-- | @Selector@ for @encodeSystemFieldsWithCoder:@
encodeSystemFieldsWithCoderSelector :: Selector '[Id NSCoder] ()
encodeSystemFieldsWithCoderSelector = mkSelector "encodeSystemFieldsWithCoder:"

-- | @Selector@ for @setParentReferenceFromRecord:@
setParentReferenceFromRecordSelector :: Selector '[Id CKRecord] ()
setParentReferenceFromRecordSelector = mkSelector "setParentReferenceFromRecord:"

-- | @Selector@ for @setParentReferenceFromRecordID:@
setParentReferenceFromRecordIDSelector :: Selector '[Id CKRecordID] ()
setParentReferenceFromRecordIDSelector = mkSelector "setParentReferenceFromRecordID:"

-- | @Selector@ for @recordType@
recordTypeSelector :: Selector '[] (Id NSString)
recordTypeSelector = mkSelector "recordType"

-- | @Selector@ for @recordID@
recordIDSelector :: Selector '[] (Id CKRecordID)
recordIDSelector = mkSelector "recordID"

-- | @Selector@ for @recordChangeTag@
recordChangeTagSelector :: Selector '[] (Id NSString)
recordChangeTagSelector = mkSelector "recordChangeTag"

-- | @Selector@ for @creatorUserRecordID@
creatorUserRecordIDSelector :: Selector '[] (Id CKRecordID)
creatorUserRecordIDSelector = mkSelector "creatorUserRecordID"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector '[] (Id NSDate)
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @lastModifiedUserRecordID@
lastModifiedUserRecordIDSelector :: Selector '[] (Id CKRecordID)
lastModifiedUserRecordIDSelector = mkSelector "lastModifiedUserRecordID"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector '[] (Id NSDate)
modificationDateSelector = mkSelector "modificationDate"

-- | @Selector@ for @share@
shareSelector :: Selector '[] (Id CKReference)
shareSelector = mkSelector "share"

-- | @Selector@ for @parent@
parentSelector :: Selector '[] (Id CKReference)
parentSelector = mkSelector "parent"

-- | @Selector@ for @setParent:@
setParentSelector :: Selector '[Id CKReference] ()
setParentSelector = mkSelector "setParent:"

-- | @Selector@ for @encryptedValues@
encryptedValuesSelector :: Selector '[] RawId
encryptedValuesSelector = mkSelector "encryptedValues"

