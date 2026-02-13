{-# LANGUAGE DataKinds #-}
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
  , addGroup_toContainerWithIdentifierSelector
  , addMember_toGroupSelector
  , addSubgroup_toGroupSelector
  , deleteContactSelector
  , deleteGroupSelector
  , removeMember_fromGroupSelector
  , removeSubgroup_fromGroupSelector
  , setShouldRefetchContactsSelector
  , setTransactionAuthorSelector
  , shouldRefetchContactsSelector
  , transactionAuthorSelector
  , updateContactSelector
  , updateGroupSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
addContact_toContainerWithIdentifier cnSaveRequest contact identifier =
  sendMessage cnSaveRequest addContact_toContainerWithIdentifierSelector (toCNMutableContact contact) (toNSString identifier)

-- | Update an existing contact in the contact store.
--
-- The contact must already exist in the contact store. The contact may be modified by the executing save request.
--
-- ObjC selector: @- updateContact:@
updateContact :: (IsCNSaveRequest cnSaveRequest, IsCNMutableContact contact) => cnSaveRequest -> contact -> IO ()
updateContact cnSaveRequest contact =
  sendMessage cnSaveRequest updateContactSelector (toCNMutableContact contact)

-- | Delete a contact from the contact store.
--
-- If the contact was previously specified to be added in the save request that will no longer occur.
--
-- ObjC selector: @- deleteContact:@
deleteContact :: (IsCNSaveRequest cnSaveRequest, IsCNMutableContact contact) => cnSaveRequest -> contact -> IO ()
deleteContact cnSaveRequest contact =
  sendMessage cnSaveRequest deleteContactSelector (toCNMutableContact contact)

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
addGroup_toContainerWithIdentifier cnSaveRequest group identifier =
  sendMessage cnSaveRequest addGroup_toContainerWithIdentifierSelector (toCNMutableGroup group) (toNSString identifier)

-- | Update an existing group in the contact store.
--
-- The group must already exist in the contact store.
--
-- ObjC selector: @- updateGroup:@
updateGroup :: (IsCNSaveRequest cnSaveRequest, IsCNMutableGroup group) => cnSaveRequest -> group -> IO ()
updateGroup cnSaveRequest group =
  sendMessage cnSaveRequest updateGroupSelector (toCNMutableGroup group)

-- | Delete a group from the contact store.
--
-- The contacts in the group are not deleted. If the group was previously specified to be added in the save request that will no longer occur.
--
-- ObjC selector: @- deleteGroup:@
deleteGroup :: (IsCNSaveRequest cnSaveRequest, IsCNMutableGroup group) => cnSaveRequest -> group -> IO ()
deleteGroup cnSaveRequest group =
  sendMessage cnSaveRequest deleteGroupSelector (toCNMutableGroup group)

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
addSubgroup_toGroup cnSaveRequest subgroup group =
  sendMessage cnSaveRequest addSubgroup_toGroupSelector (toCNGroup subgroup) (toCNGroup group)

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
removeSubgroup_fromGroup cnSaveRequest subgroup group =
  sendMessage cnSaveRequest removeSubgroup_fromGroupSelector (toCNGroup subgroup) (toCNGroup group)

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
addMember_toGroup cnSaveRequest contact group =
  sendMessage cnSaveRequest addMember_toGroupSelector (toCNContact contact) (toCNGroup group)

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
removeMember_fromGroup cnSaveRequest contact group =
  sendMessage cnSaveRequest removeMember_fromGroupSelector (toCNContact contact) (toCNGroup group)

-- | The author of this transaction.
--
-- Use this, in conjunction with @CNChangeHistoryFetchRequest.excludedTransactionAuthors,@              to suppress fetching of changes the author already knows about.
--
-- ObjC selector: @- transactionAuthor@
transactionAuthor :: IsCNSaveRequest cnSaveRequest => cnSaveRequest -> IO (Id NSString)
transactionAuthor cnSaveRequest =
  sendMessage cnSaveRequest transactionAuthorSelector

-- | The author of this transaction.
--
-- Use this, in conjunction with @CNChangeHistoryFetchRequest.excludedTransactionAuthors,@              to suppress fetching of changes the author already knows about.
--
-- ObjC selector: @- setTransactionAuthor:@
setTransactionAuthor :: (IsCNSaveRequest cnSaveRequest, IsNSString value) => cnSaveRequest -> value -> IO ()
setTransactionAuthor cnSaveRequest value =
  sendMessage cnSaveRequest setTransactionAuthorSelector (toNSString value)

-- | Should the contacts be refetched as part of executing the save request.
--
-- Default is @YES@ where added and updated contacts are refetched by the executing save request.              Set to @NO@ to suppress this refetch behavior and reduce the execution time of the save request.
--
-- Note: If set to @NO@ do not use the contacts after the executed save request as they may not be in a current state.
--
-- ObjC selector: @- shouldRefetchContacts@
shouldRefetchContacts :: IsCNSaveRequest cnSaveRequest => cnSaveRequest -> IO Bool
shouldRefetchContacts cnSaveRequest =
  sendMessage cnSaveRequest shouldRefetchContactsSelector

-- | Should the contacts be refetched as part of executing the save request.
--
-- Default is @YES@ where added and updated contacts are refetched by the executing save request.              Set to @NO@ to suppress this refetch behavior and reduce the execution time of the save request.
--
-- Note: If set to @NO@ do not use the contacts after the executed save request as they may not be in a current state.
--
-- ObjC selector: @- setShouldRefetchContacts:@
setShouldRefetchContacts :: IsCNSaveRequest cnSaveRequest => cnSaveRequest -> Bool -> IO ()
setShouldRefetchContacts cnSaveRequest value =
  sendMessage cnSaveRequest setShouldRefetchContactsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addContact:toContainerWithIdentifier:@
addContact_toContainerWithIdentifierSelector :: Selector '[Id CNMutableContact, Id NSString] ()
addContact_toContainerWithIdentifierSelector = mkSelector "addContact:toContainerWithIdentifier:"

-- | @Selector@ for @updateContact:@
updateContactSelector :: Selector '[Id CNMutableContact] ()
updateContactSelector = mkSelector "updateContact:"

-- | @Selector@ for @deleteContact:@
deleteContactSelector :: Selector '[Id CNMutableContact] ()
deleteContactSelector = mkSelector "deleteContact:"

-- | @Selector@ for @addGroup:toContainerWithIdentifier:@
addGroup_toContainerWithIdentifierSelector :: Selector '[Id CNMutableGroup, Id NSString] ()
addGroup_toContainerWithIdentifierSelector = mkSelector "addGroup:toContainerWithIdentifier:"

-- | @Selector@ for @updateGroup:@
updateGroupSelector :: Selector '[Id CNMutableGroup] ()
updateGroupSelector = mkSelector "updateGroup:"

-- | @Selector@ for @deleteGroup:@
deleteGroupSelector :: Selector '[Id CNMutableGroup] ()
deleteGroupSelector = mkSelector "deleteGroup:"

-- | @Selector@ for @addSubgroup:toGroup:@
addSubgroup_toGroupSelector :: Selector '[Id CNGroup, Id CNGroup] ()
addSubgroup_toGroupSelector = mkSelector "addSubgroup:toGroup:"

-- | @Selector@ for @removeSubgroup:fromGroup:@
removeSubgroup_fromGroupSelector :: Selector '[Id CNGroup, Id CNGroup] ()
removeSubgroup_fromGroupSelector = mkSelector "removeSubgroup:fromGroup:"

-- | @Selector@ for @addMember:toGroup:@
addMember_toGroupSelector :: Selector '[Id CNContact, Id CNGroup] ()
addMember_toGroupSelector = mkSelector "addMember:toGroup:"

-- | @Selector@ for @removeMember:fromGroup:@
removeMember_fromGroupSelector :: Selector '[Id CNContact, Id CNGroup] ()
removeMember_fromGroupSelector = mkSelector "removeMember:fromGroup:"

-- | @Selector@ for @transactionAuthor@
transactionAuthorSelector :: Selector '[] (Id NSString)
transactionAuthorSelector = mkSelector "transactionAuthor"

-- | @Selector@ for @setTransactionAuthor:@
setTransactionAuthorSelector :: Selector '[Id NSString] ()
setTransactionAuthorSelector = mkSelector "setTransactionAuthor:"

-- | @Selector@ for @shouldRefetchContacts@
shouldRefetchContactsSelector :: Selector '[] Bool
shouldRefetchContactsSelector = mkSelector "shouldRefetchContacts"

-- | @Selector@ for @setShouldRefetchContacts:@
setShouldRefetchContactsSelector :: Selector '[Bool] ()
setShouldRefetchContactsSelector = mkSelector "setShouldRefetchContacts:"

