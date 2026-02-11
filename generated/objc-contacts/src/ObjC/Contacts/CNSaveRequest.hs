{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Specifies the changes to save.
--
-- Create a new save request for each save execution on the contact store. Can have many changes batched into one save request. Do not access objects in the save request when it is executing.  A save request only applies the changes to the objects. If there are overlapping changes with multiple, concurrent CNSaveRequests then the last saved change wins.
--
-- If adding an object (contact, group, container) and it is already in the contact store then the executing save request will fail to add that object and will return the error CNErrorCodeInsertedRecordAlreadyExists with CNErrorUserInfoAffectedRecordsKey value as an array containing that object.
--
-- If updating/deleting an object (contact, group, container) and it is not in the contact store then the executing save request will fail to update/delete that object and will return the error CNErrorCodeRecordDoesNotExist with CNErrorUserInfoAffectedRecordsKey value as an array containing that object.
--
-- Generated bindings for @CNSaveRequest@.
module ObjC.Contacts.CNSaveRequest
  ( CNSaveRequest
  , IsCNSaveRequest(..)
  , addContact_toContainerWithIdentifier
  , updateContact
  , deleteContact
  , addGroup_toContainerWithIdentifier
  , updateGroup
  , deleteGroup
  , addSubgroup_toGroup
  , removeSubgroup_fromGroup
  , addMember_toGroup
  , removeMember_fromGroup
  , transactionAuthor
  , setTransactionAuthor
  , shouldRefetchContacts
  , setShouldRefetchContacts
  , addContact_toContainerWithIdentifierSelector
  , updateContactSelector
  , deleteContactSelector
  , addGroup_toContainerWithIdentifierSelector
  , updateGroupSelector
  , deleteGroupSelector
  , addSubgroup_toGroupSelector
  , removeSubgroup_fromGroupSelector
  , addMember_toGroupSelector
  , removeMember_fromGroupSelector
  , transactionAuthorSelector
  , setTransactionAuthorSelector
  , shouldRefetchContactsSelector
  , setShouldRefetchContactsSelector


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

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Add a new contact to the contact store.
--
-- The contact may be modified by the executing save request. If the contact was previously specified to be deleted in the save request that will no longer occur.
--
-- @contact@ — The new contact to add.
--
-- @identifier@ — The container identifier to add the new contact to. Set to nil for the default container.
--
-- ObjC selector: @- addContact:toContainerWithIdentifier:@
addContact_toContainerWithIdentifier :: (IsCNSaveRequest cnSaveRequest, IsCNMutableContact contact, IsNSString identifier) => cnSaveRequest -> contact -> identifier -> IO ()
addContact_toContainerWithIdentifier cnSaveRequest  contact identifier =
withObjCPtr contact $ \raw_contact ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg cnSaveRequest (mkSelector "addContact:toContainerWithIdentifier:") retVoid [argPtr (castPtr raw_contact :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())]

-- | Update an existing contact in the contact store.
--
-- The contact must already exist in the contact store. The contact may be modified by the executing save request.
--
-- ObjC selector: @- updateContact:@
updateContact :: (IsCNSaveRequest cnSaveRequest, IsCNMutableContact contact) => cnSaveRequest -> contact -> IO ()
updateContact cnSaveRequest  contact =
withObjCPtr contact $ \raw_contact ->
    sendMsg cnSaveRequest (mkSelector "updateContact:") retVoid [argPtr (castPtr raw_contact :: Ptr ())]

-- | Delete a contact from the contact store.
--
-- If the contact was previously specified to be added in the save request that will no longer occur.
--
-- ObjC selector: @- deleteContact:@
deleteContact :: (IsCNSaveRequest cnSaveRequest, IsCNMutableContact contact) => cnSaveRequest -> contact -> IO ()
deleteContact cnSaveRequest  contact =
withObjCPtr contact $ \raw_contact ->
    sendMsg cnSaveRequest (mkSelector "deleteContact:") retVoid [argPtr (castPtr raw_contact :: Ptr ())]

-- | Add a new group to the contact store.
--
-- If the group was previously specified to be deleted in the save request that will no longer occur.
--
-- @group@ — The new group to add.
--
-- @identifier@ — The container identifier to add the new group to. Set to nil for the default container.
--
-- ObjC selector: @- addGroup:toContainerWithIdentifier:@
addGroup_toContainerWithIdentifier :: (IsCNSaveRequest cnSaveRequest, IsCNMutableGroup group, IsNSString identifier) => cnSaveRequest -> group -> identifier -> IO ()
addGroup_toContainerWithIdentifier cnSaveRequest  group identifier =
withObjCPtr group $ \raw_group ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg cnSaveRequest (mkSelector "addGroup:toContainerWithIdentifier:") retVoid [argPtr (castPtr raw_group :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())]

-- | Update an existing group in the contact store.
--
-- The group must already exist in the contact store.
--
-- ObjC selector: @- updateGroup:@
updateGroup :: (IsCNSaveRequest cnSaveRequest, IsCNMutableGroup group) => cnSaveRequest -> group -> IO ()
updateGroup cnSaveRequest  group =
withObjCPtr group $ \raw_group ->
    sendMsg cnSaveRequest (mkSelector "updateGroup:") retVoid [argPtr (castPtr raw_group :: Ptr ())]

-- | Delete a group from the contact store.
--
-- The contacts in the group are not deleted. If the group was previously specified to be added in the save request that will no longer occur.
--
-- ObjC selector: @- deleteGroup:@
deleteGroup :: (IsCNSaveRequest cnSaveRequest, IsCNMutableGroup group) => cnSaveRequest -> group -> IO ()
deleteGroup cnSaveRequest  group =
withObjCPtr group $ \raw_group ->
    sendMsg cnSaveRequest (mkSelector "deleteGroup:") retVoid [argPtr (castPtr raw_group :: Ptr ())]

-- | Add a new subgroup to a group.
--
-- If the subgroup was previously specified to be deleted in the save request that will no longer occur.
--
-- @subgroup@ — The new group to add.
--
-- @group@ — The group to add the subgroup to.
--
-- ObjC selector: @- addSubgroup:toGroup:@
addSubgroup_toGroup :: (IsCNSaveRequest cnSaveRequest, IsCNGroup subgroup, IsCNGroup group) => cnSaveRequest -> subgroup -> group -> IO ()
addSubgroup_toGroup cnSaveRequest  subgroup group =
withObjCPtr subgroup $ \raw_subgroup ->
  withObjCPtr group $ \raw_group ->
      sendMsg cnSaveRequest (mkSelector "addSubgroup:toGroup:") retVoid [argPtr (castPtr raw_subgroup :: Ptr ()), argPtr (castPtr raw_group :: Ptr ())]

-- | Remove a subgroup from a group.
--
-- The contacts in the subgroup's membership are not affected. If the subgroup was previously specified to be added in the save request that will no longer occur.
--
-- @subgroup@ — The new group to add.
--
-- @group@ — The group to add the subgroup to.
--
-- ObjC selector: @- removeSubgroup:fromGroup:@
removeSubgroup_fromGroup :: (IsCNSaveRequest cnSaveRequest, IsCNGroup subgroup, IsCNGroup group) => cnSaveRequest -> subgroup -> group -> IO ()
removeSubgroup_fromGroup cnSaveRequest  subgroup group =
withObjCPtr subgroup $ \raw_subgroup ->
  withObjCPtr group $ \raw_group ->
      sendMsg cnSaveRequest (mkSelector "removeSubgroup:fromGroup:") retVoid [argPtr (castPtr raw_subgroup :: Ptr ()), argPtr (castPtr raw_group :: Ptr ())]

-- | Add a new member to a group.
--
-- If the membership was previously specified to be deleted in the save request that will no longer occur.
--
-- @contact@ — The new member to add to the group.
--
-- @group@ — The group to add the member to.
--
-- ObjC selector: @- addMember:toGroup:@
addMember_toGroup :: (IsCNSaveRequest cnSaveRequest, IsCNContact contact, IsCNGroup group) => cnSaveRequest -> contact -> group -> IO ()
addMember_toGroup cnSaveRequest  contact group =
withObjCPtr contact $ \raw_contact ->
  withObjCPtr group $ \raw_group ->
      sendMsg cnSaveRequest (mkSelector "addMember:toGroup:") retVoid [argPtr (castPtr raw_contact :: Ptr ()), argPtr (castPtr raw_group :: Ptr ())]

-- | Remove a member from a group.
--
-- The contact is not deleted. It is only removed as a member of the group. If the membership was previously specified to be added in the save request that will no longer occur.
--
-- @contact@ — The member to remove from the group.
--
-- @group@ — The group to remove the member from.
--
-- ObjC selector: @- removeMember:fromGroup:@
removeMember_fromGroup :: (IsCNSaveRequest cnSaveRequest, IsCNContact contact, IsCNGroup group) => cnSaveRequest -> contact -> group -> IO ()
removeMember_fromGroup cnSaveRequest  contact group =
withObjCPtr contact $ \raw_contact ->
  withObjCPtr group $ \raw_group ->
      sendMsg cnSaveRequest (mkSelector "removeMember:fromGroup:") retVoid [argPtr (castPtr raw_contact :: Ptr ()), argPtr (castPtr raw_group :: Ptr ())]

-- | The author of this transaction.
--
-- Use this, in conjunction with @CNChangeHistoryFetchRequest.excludedTransactionAuthors,@              to suppress fetching of changes the author already knows about.
--
-- ObjC selector: @- transactionAuthor@
transactionAuthor :: IsCNSaveRequest cnSaveRequest => cnSaveRequest -> IO (Id NSString)
transactionAuthor cnSaveRequest  =
  sendMsg cnSaveRequest (mkSelector "transactionAuthor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The author of this transaction.
--
-- Use this, in conjunction with @CNChangeHistoryFetchRequest.excludedTransactionAuthors,@              to suppress fetching of changes the author already knows about.
--
-- ObjC selector: @- setTransactionAuthor:@
setTransactionAuthor :: (IsCNSaveRequest cnSaveRequest, IsNSString value) => cnSaveRequest -> value -> IO ()
setTransactionAuthor cnSaveRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnSaveRequest (mkSelector "setTransactionAuthor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Should the contacts be refetched as part of executing the save request.
--
-- Default is @YES@ where added and updated contacts are refetched by the executing save request.              Set to @NO@ to suppress this refetch behavior and reduce the execution time of the save request.
--
-- Note: If set to @NO@ do not use the contacts after the executed save request as they may not be in a current state.
--
-- ObjC selector: @- shouldRefetchContacts@
shouldRefetchContacts :: IsCNSaveRequest cnSaveRequest => cnSaveRequest -> IO Bool
shouldRefetchContacts cnSaveRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnSaveRequest (mkSelector "shouldRefetchContacts") retCULong []

-- | Should the contacts be refetched as part of executing the save request.
--
-- Default is @YES@ where added and updated contacts are refetched by the executing save request.              Set to @NO@ to suppress this refetch behavior and reduce the execution time of the save request.
--
-- Note: If set to @NO@ do not use the contacts after the executed save request as they may not be in a current state.
--
-- ObjC selector: @- setShouldRefetchContacts:@
setShouldRefetchContacts :: IsCNSaveRequest cnSaveRequest => cnSaveRequest -> Bool -> IO ()
setShouldRefetchContacts cnSaveRequest  value =
  sendMsg cnSaveRequest (mkSelector "setShouldRefetchContacts:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addContact:toContainerWithIdentifier:@
addContact_toContainerWithIdentifierSelector :: Selector
addContact_toContainerWithIdentifierSelector = mkSelector "addContact:toContainerWithIdentifier:"

-- | @Selector@ for @updateContact:@
updateContactSelector :: Selector
updateContactSelector = mkSelector "updateContact:"

-- | @Selector@ for @deleteContact:@
deleteContactSelector :: Selector
deleteContactSelector = mkSelector "deleteContact:"

-- | @Selector@ for @addGroup:toContainerWithIdentifier:@
addGroup_toContainerWithIdentifierSelector :: Selector
addGroup_toContainerWithIdentifierSelector = mkSelector "addGroup:toContainerWithIdentifier:"

-- | @Selector@ for @updateGroup:@
updateGroupSelector :: Selector
updateGroupSelector = mkSelector "updateGroup:"

-- | @Selector@ for @deleteGroup:@
deleteGroupSelector :: Selector
deleteGroupSelector = mkSelector "deleteGroup:"

-- | @Selector@ for @addSubgroup:toGroup:@
addSubgroup_toGroupSelector :: Selector
addSubgroup_toGroupSelector = mkSelector "addSubgroup:toGroup:"

-- | @Selector@ for @removeSubgroup:fromGroup:@
removeSubgroup_fromGroupSelector :: Selector
removeSubgroup_fromGroupSelector = mkSelector "removeSubgroup:fromGroup:"

-- | @Selector@ for @addMember:toGroup:@
addMember_toGroupSelector :: Selector
addMember_toGroupSelector = mkSelector "addMember:toGroup:"

-- | @Selector@ for @removeMember:fromGroup:@
removeMember_fromGroupSelector :: Selector
removeMember_fromGroupSelector = mkSelector "removeMember:fromGroup:"

-- | @Selector@ for @transactionAuthor@
transactionAuthorSelector :: Selector
transactionAuthorSelector = mkSelector "transactionAuthor"

-- | @Selector@ for @setTransactionAuthor:@
setTransactionAuthorSelector :: Selector
setTransactionAuthorSelector = mkSelector "setTransactionAuthor:"

-- | @Selector@ for @shouldRefetchContacts@
shouldRefetchContactsSelector :: Selector
shouldRefetchContactsSelector = mkSelector "shouldRefetchContacts"

-- | @Selector@ for @setShouldRefetchContacts:@
setShouldRefetchContactsSelector :: Selector
setShouldRefetchContactsSelector = mkSelector "setShouldRefetchContacts:"

