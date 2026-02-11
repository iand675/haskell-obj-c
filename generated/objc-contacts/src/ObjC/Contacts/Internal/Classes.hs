{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Contacts.Internal.Classes (
    module ObjC.Contacts.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- CNChangeHistoryEvent ----------

-- | Phantom type for @CNChangeHistoryEvent@.
data CNChangeHistoryEvent

instance IsObjCObject (Id CNChangeHistoryEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNChangeHistoryEvent"

class IsNSObject a => IsCNChangeHistoryEvent a where
  toCNChangeHistoryEvent :: a -> Id CNChangeHistoryEvent

instance IsCNChangeHistoryEvent (Id CNChangeHistoryEvent) where
  toCNChangeHistoryEvent = unsafeCastId

instance IsNSObject (Id CNChangeHistoryEvent) where
  toNSObject = unsafeCastId

-- ---------- CNContact ----------

-- | An immutable value object representing a contact.
--
-- CNContact is thread safe.
--
-- If using a CNContact instance where you are not certain of the keys that were fetched, use isKeyAvailable: or areKeysAvailable:. If these return NO you need to refetch the contact by the contact identifier with the keys you want to fetch. Accessing a property that was not fetched will throw CNContactPropertyNotFetchedExceptionName.
-- 
-- Phantom type for @CNContact@.
data CNContact

instance IsObjCObject (Id CNContact) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNContact"

class IsNSObject a => IsCNContact a where
  toCNContact :: a -> Id CNContact

instance IsCNContact (Id CNContact) where
  toCNContact = unsafeCastId

instance IsNSObject (Id CNContact) where
  toNSObject = unsafeCastId

-- ---------- CNContactProperty ----------

-- | Contains related information for a specific contact property.
--
-- CNContactProperty is used by the CNContactPicker to return the user's selected property.
-- 
-- Phantom type for @CNContactProperty@.
data CNContactProperty

instance IsObjCObject (Id CNContactProperty) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNContactProperty"

class IsNSObject a => IsCNContactProperty a where
  toCNContactProperty :: a -> Id CNContactProperty

instance IsCNContactProperty (Id CNContactProperty) where
  toCNContactProperty = unsafeCastId

instance IsNSObject (Id CNContactProperty) where
  toNSObject = unsafeCastId

-- ---------- CNContactRelation ----------

-- | An immutable value object representing a related contact.
--
-- CNContactRelation is thread safe.
-- 
-- Phantom type for @CNContactRelation@.
data CNContactRelation

instance IsObjCObject (Id CNContactRelation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNContactRelation"

class IsNSObject a => IsCNContactRelation a where
  toCNContactRelation :: a -> Id CNContactRelation

instance IsCNContactRelation (Id CNContactRelation) where
  toCNContactRelation = unsafeCastId

instance IsNSObject (Id CNContactRelation) where
  toNSObject = unsafeCastId

-- ---------- CNContactStore ----------

-- | Provides methods to fetch and save contacts.
--
-- The CNContactStore is a thread safe class that can fetch and save contacts, fetch and save groups, and fetch containers.
--
-- Note: Some good practices are: 1) Only fetch contact properties that will be used. 2) When fetching all contacts and caching the results, first fetch all contact identifiers only. Then fetch batches of detailed contacts by identifiers as you need them. 3) To aggregate several contact fetches collect a set of unique contact identifiers from the fetches. Then fetch batches of detailed contacts by identifiers. 4) When CNContactStoreDidChangeNotification is posted, if you cache any fetched contacts/groups/containers then they must be refetched and the old cached objects released.
-- 
-- Phantom type for @CNContactStore@.
data CNContactStore

instance IsObjCObject (Id CNContactStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNContactStore"

class IsNSObject a => IsCNContactStore a where
  toCNContactStore :: a -> Id CNContactStore

instance IsCNContactStore (Id CNContactStore) where
  toCNContactStore = unsafeCastId

instance IsNSObject (Id CNContactStore) where
  toNSObject = unsafeCastId

-- ---------- CNContactVCardSerialization ----------

-- | Contact vCard support.
--
-- This converts between a contact and its vCard representation.
-- 
-- Phantom type for @CNContactVCardSerialization@.
data CNContactVCardSerialization

instance IsObjCObject (Id CNContactVCardSerialization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNContactVCardSerialization"

class IsNSObject a => IsCNContactVCardSerialization a where
  toCNContactVCardSerialization :: a -> Id CNContactVCardSerialization

instance IsCNContactVCardSerialization (Id CNContactVCardSerialization) where
  toCNContactVCardSerialization = unsafeCastId

instance IsNSObject (Id CNContactVCardSerialization) where
  toNSObject = unsafeCastId

-- ---------- CNContactsUserDefaults ----------

-- | The user defaults for contacts.
--
-- Note: This class is not thread safe.
-- 
-- Phantom type for @CNContactsUserDefaults@.
data CNContactsUserDefaults

instance IsObjCObject (Id CNContactsUserDefaults) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNContactsUserDefaults"

class IsNSObject a => IsCNContactsUserDefaults a where
  toCNContactsUserDefaults :: a -> Id CNContactsUserDefaults

instance IsCNContactsUserDefaults (Id CNContactsUserDefaults) where
  toCNContactsUserDefaults = unsafeCastId

instance IsNSObject (Id CNContactsUserDefaults) where
  toNSObject = unsafeCastId

-- ---------- CNContainer ----------

-- | An immutable value object representing a container.
--
-- CNContainer is thread safe.
-- 
-- Phantom type for @CNContainer@.
data CNContainer

instance IsObjCObject (Id CNContainer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNContainer"

class IsNSObject a => IsCNContainer a where
  toCNContainer :: a -> Id CNContainer

instance IsCNContainer (Id CNContainer) where
  toCNContainer = unsafeCastId

instance IsNSObject (Id CNContainer) where
  toNSObject = unsafeCastId

-- ---------- CNFetchRequest ----------

-- | Phantom type for @CNFetchRequest@.
data CNFetchRequest

instance IsObjCObject (Id CNFetchRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNFetchRequest"

class IsNSObject a => IsCNFetchRequest a where
  toCNFetchRequest :: a -> Id CNFetchRequest

instance IsCNFetchRequest (Id CNFetchRequest) where
  toCNFetchRequest = unsafeCastId

instance IsNSObject (Id CNFetchRequest) where
  toNSObject = unsafeCastId

-- ---------- CNFetchResult ----------

-- | Phantom type for @CNFetchResult@.
data CNFetchResult

instance IsObjCObject (Id CNFetchResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNFetchResult"

class IsNSObject a => IsCNFetchResult a where
  toCNFetchResult :: a -> Id CNFetchResult

instance IsCNFetchResult (Id CNFetchResult) where
  toCNFetchResult = unsafeCastId

instance IsNSObject (Id CNFetchResult) where
  toNSObject = unsafeCastId

-- ---------- CNGroup ----------

-- | An immutable value object representing a group.
--
-- CNGroup is thread safe.
-- 
-- Phantom type for @CNGroup@.
data CNGroup

instance IsObjCObject (Id CNGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNGroup"

class IsNSObject a => IsCNGroup a where
  toCNGroup :: a -> Id CNGroup

instance IsCNGroup (Id CNGroup) where
  toCNGroup = unsafeCastId

instance IsNSObject (Id CNGroup) where
  toNSObject = unsafeCastId

-- ---------- CNInstantMessageAddress ----------

-- | An immutable value object representing an instant message address.
--
-- CNInstantMessageAddress is thread safe.
-- 
-- Phantom type for @CNInstantMessageAddress@.
data CNInstantMessageAddress

instance IsObjCObject (Id CNInstantMessageAddress) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNInstantMessageAddress"

class IsNSObject a => IsCNInstantMessageAddress a where
  toCNInstantMessageAddress :: a -> Id CNInstantMessageAddress

instance IsCNInstantMessageAddress (Id CNInstantMessageAddress) where
  toCNInstantMessageAddress = unsafeCastId

instance IsNSObject (Id CNInstantMessageAddress) where
  toNSObject = unsafeCastId

-- ---------- CNLabeledValue ----------

-- | A contact property that has a value and label.
-- 
-- Phantom type for @CNLabeledValue@.
data CNLabeledValue

instance IsObjCObject (Id CNLabeledValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNLabeledValue"

class IsNSObject a => IsCNLabeledValue a where
  toCNLabeledValue :: a -> Id CNLabeledValue

instance IsCNLabeledValue (Id CNLabeledValue) where
  toCNLabeledValue = unsafeCastId

instance IsNSObject (Id CNLabeledValue) where
  toNSObject = unsafeCastId

-- ---------- CNPhoneNumber ----------

-- | An immutable value object representing a phone number.
--
-- CNPhoneNumber is thread safe.
-- 
-- Phantom type for @CNPhoneNumber@.
data CNPhoneNumber

instance IsObjCObject (Id CNPhoneNumber) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNPhoneNumber"

class IsNSObject a => IsCNPhoneNumber a where
  toCNPhoneNumber :: a -> Id CNPhoneNumber

instance IsCNPhoneNumber (Id CNPhoneNumber) where
  toCNPhoneNumber = unsafeCastId

instance IsNSObject (Id CNPhoneNumber) where
  toNSObject = unsafeCastId

-- ---------- CNPostalAddress ----------

-- | An immutable value object representing a postal address.
--
-- CNPostalAddress is thread safe.
-- 
-- Phantom type for @CNPostalAddress@.
data CNPostalAddress

instance IsObjCObject (Id CNPostalAddress) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNPostalAddress"

class IsNSObject a => IsCNPostalAddress a where
  toCNPostalAddress :: a -> Id CNPostalAddress

instance IsCNPostalAddress (Id CNPostalAddress) where
  toCNPostalAddress = unsafeCastId

instance IsNSObject (Id CNPostalAddress) where
  toNSObject = unsafeCastId

-- ---------- CNSaveRequest ----------

-- | Specifies the changes to save.
--
-- Create a new save request for each save execution on the contact store. Can have many changes batched into one save request. Do not access objects in the save request when it is executing.  A save request only applies the changes to the objects. If there are overlapping changes with multiple, concurrent CNSaveRequests then the last saved change wins.
--
-- If adding an object (contact, group, container) and it is already in the contact store then the executing save request will fail to add that object and will return the error CNErrorCodeInsertedRecordAlreadyExists with CNErrorUserInfoAffectedRecordsKey value as an array containing that object.
--
-- If updating/deleting an object (contact, group, container) and it is not in the contact store then the executing save request will fail to update/delete that object and will return the error CNErrorCodeRecordDoesNotExist with CNErrorUserInfoAffectedRecordsKey value as an array containing that object.
-- 
-- Phantom type for @CNSaveRequest@.
data CNSaveRequest

instance IsObjCObject (Id CNSaveRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNSaveRequest"

class IsNSObject a => IsCNSaveRequest a where
  toCNSaveRequest :: a -> Id CNSaveRequest

instance IsCNSaveRequest (Id CNSaveRequest) where
  toCNSaveRequest = unsafeCastId

instance IsNSObject (Id CNSaveRequest) where
  toNSObject = unsafeCastId

-- ---------- CNSocialProfile ----------

-- | An immutable value object representing a social profile.
--
-- CNSocialProfile is thread safe.
-- 
-- Phantom type for @CNSocialProfile@.
data CNSocialProfile

instance IsObjCObject (Id CNSocialProfile) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNSocialProfile"

class IsNSObject a => IsCNSocialProfile a where
  toCNSocialProfile :: a -> Id CNSocialProfile

instance IsCNSocialProfile (Id CNSocialProfile) where
  toCNSocialProfile = unsafeCastId

instance IsNSObject (Id CNSocialProfile) where
  toNSObject = unsafeCastId

-- ---------- CNChangeHistoryAddContactEvent ----------

-- | A contact was added
-- 
-- Phantom type for @CNChangeHistoryAddContactEvent@.
data CNChangeHistoryAddContactEvent

instance IsObjCObject (Id CNChangeHistoryAddContactEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNChangeHistoryAddContactEvent"

class IsCNChangeHistoryEvent a => IsCNChangeHistoryAddContactEvent a where
  toCNChangeHistoryAddContactEvent :: a -> Id CNChangeHistoryAddContactEvent

instance IsCNChangeHistoryAddContactEvent (Id CNChangeHistoryAddContactEvent) where
  toCNChangeHistoryAddContactEvent = unsafeCastId

instance IsCNChangeHistoryEvent (Id CNChangeHistoryAddContactEvent) where
  toCNChangeHistoryEvent = unsafeCastId

instance IsNSObject (Id CNChangeHistoryAddContactEvent) where
  toNSObject = unsafeCastId

-- ---------- CNChangeHistoryAddGroupEvent ----------

-- | A group was added
-- 
-- Phantom type for @CNChangeHistoryAddGroupEvent@.
data CNChangeHistoryAddGroupEvent

instance IsObjCObject (Id CNChangeHistoryAddGroupEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNChangeHistoryAddGroupEvent"

class IsCNChangeHistoryEvent a => IsCNChangeHistoryAddGroupEvent a where
  toCNChangeHistoryAddGroupEvent :: a -> Id CNChangeHistoryAddGroupEvent

instance IsCNChangeHistoryAddGroupEvent (Id CNChangeHistoryAddGroupEvent) where
  toCNChangeHistoryAddGroupEvent = unsafeCastId

instance IsCNChangeHistoryEvent (Id CNChangeHistoryAddGroupEvent) where
  toCNChangeHistoryEvent = unsafeCastId

instance IsNSObject (Id CNChangeHistoryAddGroupEvent) where
  toNSObject = unsafeCastId

-- ---------- CNChangeHistoryAddMemberToGroupEvent ----------

-- | A contact was added to a group
-- 
-- Phantom type for @CNChangeHistoryAddMemberToGroupEvent@.
data CNChangeHistoryAddMemberToGroupEvent

instance IsObjCObject (Id CNChangeHistoryAddMemberToGroupEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNChangeHistoryAddMemberToGroupEvent"

class IsCNChangeHistoryEvent a => IsCNChangeHistoryAddMemberToGroupEvent a where
  toCNChangeHistoryAddMemberToGroupEvent :: a -> Id CNChangeHistoryAddMemberToGroupEvent

instance IsCNChangeHistoryAddMemberToGroupEvent (Id CNChangeHistoryAddMemberToGroupEvent) where
  toCNChangeHistoryAddMemberToGroupEvent = unsafeCastId

instance IsCNChangeHistoryEvent (Id CNChangeHistoryAddMemberToGroupEvent) where
  toCNChangeHistoryEvent = unsafeCastId

instance IsNSObject (Id CNChangeHistoryAddMemberToGroupEvent) where
  toNSObject = unsafeCastId

-- ---------- CNChangeHistoryAddSubgroupToGroupEvent ----------

-- | A subgroup was added to a group
-- 
-- Phantom type for @CNChangeHistoryAddSubgroupToGroupEvent@.
data CNChangeHistoryAddSubgroupToGroupEvent

instance IsObjCObject (Id CNChangeHistoryAddSubgroupToGroupEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNChangeHistoryAddSubgroupToGroupEvent"

class IsCNChangeHistoryEvent a => IsCNChangeHistoryAddSubgroupToGroupEvent a where
  toCNChangeHistoryAddSubgroupToGroupEvent :: a -> Id CNChangeHistoryAddSubgroupToGroupEvent

instance IsCNChangeHistoryAddSubgroupToGroupEvent (Id CNChangeHistoryAddSubgroupToGroupEvent) where
  toCNChangeHistoryAddSubgroupToGroupEvent = unsafeCastId

instance IsCNChangeHistoryEvent (Id CNChangeHistoryAddSubgroupToGroupEvent) where
  toCNChangeHistoryEvent = unsafeCastId

instance IsNSObject (Id CNChangeHistoryAddSubgroupToGroupEvent) where
  toNSObject = unsafeCastId

-- ---------- CNChangeHistoryDeleteContactEvent ----------

-- | A contact was removed
-- 
-- Phantom type for @CNChangeHistoryDeleteContactEvent@.
data CNChangeHistoryDeleteContactEvent

instance IsObjCObject (Id CNChangeHistoryDeleteContactEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNChangeHistoryDeleteContactEvent"

class IsCNChangeHistoryEvent a => IsCNChangeHistoryDeleteContactEvent a where
  toCNChangeHistoryDeleteContactEvent :: a -> Id CNChangeHistoryDeleteContactEvent

instance IsCNChangeHistoryDeleteContactEvent (Id CNChangeHistoryDeleteContactEvent) where
  toCNChangeHistoryDeleteContactEvent = unsafeCastId

instance IsCNChangeHistoryEvent (Id CNChangeHistoryDeleteContactEvent) where
  toCNChangeHistoryEvent = unsafeCastId

instance IsNSObject (Id CNChangeHistoryDeleteContactEvent) where
  toNSObject = unsafeCastId

-- ---------- CNChangeHistoryDeleteGroupEvent ----------

-- | A group was deleted
-- 
-- Phantom type for @CNChangeHistoryDeleteGroupEvent@.
data CNChangeHistoryDeleteGroupEvent

instance IsObjCObject (Id CNChangeHistoryDeleteGroupEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNChangeHistoryDeleteGroupEvent"

class IsCNChangeHistoryEvent a => IsCNChangeHistoryDeleteGroupEvent a where
  toCNChangeHistoryDeleteGroupEvent :: a -> Id CNChangeHistoryDeleteGroupEvent

instance IsCNChangeHistoryDeleteGroupEvent (Id CNChangeHistoryDeleteGroupEvent) where
  toCNChangeHistoryDeleteGroupEvent = unsafeCastId

instance IsCNChangeHistoryEvent (Id CNChangeHistoryDeleteGroupEvent) where
  toCNChangeHistoryEvent = unsafeCastId

instance IsNSObject (Id CNChangeHistoryDeleteGroupEvent) where
  toNSObject = unsafeCastId

-- ---------- CNChangeHistoryDropEverythingEvent ----------

-- | Drop all cached information your app has persisted
-- 
-- Phantom type for @CNChangeHistoryDropEverythingEvent@.
data CNChangeHistoryDropEverythingEvent

instance IsObjCObject (Id CNChangeHistoryDropEverythingEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNChangeHistoryDropEverythingEvent"

class IsCNChangeHistoryEvent a => IsCNChangeHistoryDropEverythingEvent a where
  toCNChangeHistoryDropEverythingEvent :: a -> Id CNChangeHistoryDropEverythingEvent

instance IsCNChangeHistoryDropEverythingEvent (Id CNChangeHistoryDropEverythingEvent) where
  toCNChangeHistoryDropEverythingEvent = unsafeCastId

instance IsCNChangeHistoryEvent (Id CNChangeHistoryDropEverythingEvent) where
  toCNChangeHistoryEvent = unsafeCastId

instance IsNSObject (Id CNChangeHistoryDropEverythingEvent) where
  toNSObject = unsafeCastId

-- ---------- CNChangeHistoryRemoveMemberFromGroupEvent ----------

-- | A contact was removed from a group
-- 
-- Phantom type for @CNChangeHistoryRemoveMemberFromGroupEvent@.
data CNChangeHistoryRemoveMemberFromGroupEvent

instance IsObjCObject (Id CNChangeHistoryRemoveMemberFromGroupEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNChangeHistoryRemoveMemberFromGroupEvent"

class IsCNChangeHistoryEvent a => IsCNChangeHistoryRemoveMemberFromGroupEvent a where
  toCNChangeHistoryRemoveMemberFromGroupEvent :: a -> Id CNChangeHistoryRemoveMemberFromGroupEvent

instance IsCNChangeHistoryRemoveMemberFromGroupEvent (Id CNChangeHistoryRemoveMemberFromGroupEvent) where
  toCNChangeHistoryRemoveMemberFromGroupEvent = unsafeCastId

instance IsCNChangeHistoryEvent (Id CNChangeHistoryRemoveMemberFromGroupEvent) where
  toCNChangeHistoryEvent = unsafeCastId

instance IsNSObject (Id CNChangeHistoryRemoveMemberFromGroupEvent) where
  toNSObject = unsafeCastId

-- ---------- CNChangeHistoryRemoveSubgroupFromGroupEvent ----------

-- | A subgroup was removed from a group
-- 
-- Phantom type for @CNChangeHistoryRemoveSubgroupFromGroupEvent@.
data CNChangeHistoryRemoveSubgroupFromGroupEvent

instance IsObjCObject (Id CNChangeHistoryRemoveSubgroupFromGroupEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNChangeHistoryRemoveSubgroupFromGroupEvent"

class IsCNChangeHistoryEvent a => IsCNChangeHistoryRemoveSubgroupFromGroupEvent a where
  toCNChangeHistoryRemoveSubgroupFromGroupEvent :: a -> Id CNChangeHistoryRemoveSubgroupFromGroupEvent

instance IsCNChangeHistoryRemoveSubgroupFromGroupEvent (Id CNChangeHistoryRemoveSubgroupFromGroupEvent) where
  toCNChangeHistoryRemoveSubgroupFromGroupEvent = unsafeCastId

instance IsCNChangeHistoryEvent (Id CNChangeHistoryRemoveSubgroupFromGroupEvent) where
  toCNChangeHistoryEvent = unsafeCastId

instance IsNSObject (Id CNChangeHistoryRemoveSubgroupFromGroupEvent) where
  toNSObject = unsafeCastId

-- ---------- CNChangeHistoryUpdateContactEvent ----------

-- | A contact was updated
-- 
-- Phantom type for @CNChangeHistoryUpdateContactEvent@.
data CNChangeHistoryUpdateContactEvent

instance IsObjCObject (Id CNChangeHistoryUpdateContactEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNChangeHistoryUpdateContactEvent"

class IsCNChangeHistoryEvent a => IsCNChangeHistoryUpdateContactEvent a where
  toCNChangeHistoryUpdateContactEvent :: a -> Id CNChangeHistoryUpdateContactEvent

instance IsCNChangeHistoryUpdateContactEvent (Id CNChangeHistoryUpdateContactEvent) where
  toCNChangeHistoryUpdateContactEvent = unsafeCastId

instance IsCNChangeHistoryEvent (Id CNChangeHistoryUpdateContactEvent) where
  toCNChangeHistoryEvent = unsafeCastId

instance IsNSObject (Id CNChangeHistoryUpdateContactEvent) where
  toNSObject = unsafeCastId

-- ---------- CNChangeHistoryUpdateGroupEvent ----------

-- | A group was updated
-- 
-- Phantom type for @CNChangeHistoryUpdateGroupEvent@.
data CNChangeHistoryUpdateGroupEvent

instance IsObjCObject (Id CNChangeHistoryUpdateGroupEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNChangeHistoryUpdateGroupEvent"

class IsCNChangeHistoryEvent a => IsCNChangeHistoryUpdateGroupEvent a where
  toCNChangeHistoryUpdateGroupEvent :: a -> Id CNChangeHistoryUpdateGroupEvent

instance IsCNChangeHistoryUpdateGroupEvent (Id CNChangeHistoryUpdateGroupEvent) where
  toCNChangeHistoryUpdateGroupEvent = unsafeCastId

instance IsCNChangeHistoryEvent (Id CNChangeHistoryUpdateGroupEvent) where
  toCNChangeHistoryEvent = unsafeCastId

instance IsNSObject (Id CNChangeHistoryUpdateGroupEvent) where
  toNSObject = unsafeCastId

-- ---------- CNMutableContact ----------

-- | A mutable value object representing a contact.
--
-- CNMutableContact is not thread safe. If this is a mutable copy of CNContact then it will throw CNContactPropertyNotFetchedExceptionName when accessing a property that was not fetched for the CNContact.
--
-- Note: To remove properties when saving a mutable contact, set string properties and array properties to empty values. Set other properties to nil.
-- 
-- Phantom type for @CNMutableContact@.
data CNMutableContact

instance IsObjCObject (Id CNMutableContact) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNMutableContact"

class IsCNContact a => IsCNMutableContact a where
  toCNMutableContact :: a -> Id CNMutableContact

instance IsCNMutableContact (Id CNMutableContact) where
  toCNMutableContact = unsafeCastId

instance IsCNContact (Id CNMutableContact) where
  toCNContact = unsafeCastId

instance IsNSObject (Id CNMutableContact) where
  toNSObject = unsafeCastId

-- ---------- CNChangeHistoryFetchRequest ----------

-- | Specifies the criteria to fetch change history.
--
-- Changes to contacts are always returned.              All changes are coalesced to remove redundant adds, updates and deletes.              This request is used with [CNContactStore enumeratorForChangeHistoryFetchRequest:error:].
-- 
-- Phantom type for @CNChangeHistoryFetchRequest@.
data CNChangeHistoryFetchRequest

instance IsObjCObject (Id CNChangeHistoryFetchRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNChangeHistoryFetchRequest"

class IsCNFetchRequest a => IsCNChangeHistoryFetchRequest a where
  toCNChangeHistoryFetchRequest :: a -> Id CNChangeHistoryFetchRequest

instance IsCNChangeHistoryFetchRequest (Id CNChangeHistoryFetchRequest) where
  toCNChangeHistoryFetchRequest = unsafeCastId

instance IsCNFetchRequest (Id CNChangeHistoryFetchRequest) where
  toCNFetchRequest = unsafeCastId

instance IsNSObject (Id CNChangeHistoryFetchRequest) where
  toNSObject = unsafeCastId

-- ---------- CNContactFetchRequest ----------

-- | Specifies the search criteria to fetch contacts.
--
-- Used with [CNContactStore enumerateContactsWithFetchRequest:error:usingBlock:]. Can combine any of these options to create a contact fetch request.
-- 
-- Phantom type for @CNContactFetchRequest@.
data CNContactFetchRequest

instance IsObjCObject (Id CNContactFetchRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNContactFetchRequest"

class IsCNFetchRequest a => IsCNContactFetchRequest a where
  toCNContactFetchRequest :: a -> Id CNContactFetchRequest

instance IsCNContactFetchRequest (Id CNContactFetchRequest) where
  toCNContactFetchRequest = unsafeCastId

instance IsCNFetchRequest (Id CNContactFetchRequest) where
  toCNFetchRequest = unsafeCastId

instance IsNSObject (Id CNContactFetchRequest) where
  toNSObject = unsafeCastId

-- ---------- CNMutableGroup ----------

-- | A mutable value object representing a group.
--
-- CNMutableGroup is not thread safe.
-- 
-- Phantom type for @CNMutableGroup@.
data CNMutableGroup

instance IsObjCObject (Id CNMutableGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNMutableGroup"

class IsCNGroup a => IsCNMutableGroup a where
  toCNMutableGroup :: a -> Id CNMutableGroup

instance IsCNMutableGroup (Id CNMutableGroup) where
  toCNMutableGroup = unsafeCastId

instance IsCNGroup (Id CNMutableGroup) where
  toCNGroup = unsafeCastId

instance IsNSObject (Id CNMutableGroup) where
  toNSObject = unsafeCastId

-- ---------- CNMutablePostalAddress ----------

-- | A mutable value object representing a postal address.
--
-- CNMutablePostalAddress is not thread safe.
--
-- Note: To remove properties when saving a mutable postal address, set string properties to empty values.
-- 
-- Phantom type for @CNMutablePostalAddress@.
data CNMutablePostalAddress

instance IsObjCObject (Id CNMutablePostalAddress) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNMutablePostalAddress"

class IsCNPostalAddress a => IsCNMutablePostalAddress a where
  toCNMutablePostalAddress :: a -> Id CNMutablePostalAddress

instance IsCNMutablePostalAddress (Id CNMutablePostalAddress) where
  toCNMutablePostalAddress = unsafeCastId

instance IsCNPostalAddress (Id CNMutablePostalAddress) where
  toCNPostalAddress = unsafeCastId

instance IsNSObject (Id CNMutablePostalAddress) where
  toNSObject = unsafeCastId

-- ---------- CNContactFormatter ----------

-- | Formats a contact name.
--
-- This formatter handles international ordering and delimiting of the contact name components. This includes applying the user defaults when appropriate.
-- 
-- Phantom type for @CNContactFormatter@.
data CNContactFormatter

instance IsObjCObject (Id CNContactFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNContactFormatter"

class IsNSFormatter a => IsCNContactFormatter a where
  toCNContactFormatter :: a -> Id CNContactFormatter

instance IsCNContactFormatter (Id CNContactFormatter) where
  toCNContactFormatter = unsafeCastId

instance IsNSFormatter (Id CNContactFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id CNContactFormatter) where
  toNSObject = unsafeCastId

-- ---------- CNPostalAddressFormatter ----------

-- | Formats a postal address.
--
-- This formatter handles international formatting of a postal address.
-- 
-- Phantom type for @CNPostalAddressFormatter@.
data CNPostalAddressFormatter

instance IsObjCObject (Id CNPostalAddressFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNPostalAddressFormatter"

class IsNSFormatter a => IsCNPostalAddressFormatter a where
  toCNPostalAddressFormatter :: a -> Id CNPostalAddressFormatter

instance IsCNPostalAddressFormatter (Id CNPostalAddressFormatter) where
  toCNPostalAddressFormatter = unsafeCastId

instance IsNSFormatter (Id CNPostalAddressFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id CNPostalAddressFormatter) where
  toNSObject = unsafeCastId
