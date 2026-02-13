{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EKParticipant
--
-- Abstract class representing a participant attached to an event.
--
-- Generated bindings for @EKParticipant@.
module ObjC.EventKit.EKParticipant
  ( EKParticipant
  , IsEKParticipant(..)
  , abPersonInAddressBook
  , url
  , name
  , participantStatus
  , participantRole
  , participantType
  , currentUser
  , contactPredicate
  , abPersonInAddressBookSelector
  , contactPredicateSelector
  , currentUserSelector
  , nameSelector
  , participantRoleSelector
  , participantStatusSelector
  , participantTypeSelector
  , urlSelector

  -- * Enum types
  , EKParticipantRole(EKParticipantRole)
  , pattern EKParticipantRoleUnknown
  , pattern EKParticipantRoleRequired
  , pattern EKParticipantRoleOptional
  , pattern EKParticipantRoleChair
  , pattern EKParticipantRoleNonParticipant
  , EKParticipantStatus(EKParticipantStatus)
  , pattern EKParticipantStatusUnknown
  , pattern EKParticipantStatusPending
  , pattern EKParticipantStatusAccepted
  , pattern EKParticipantStatusDeclined
  , pattern EKParticipantStatusTentative
  , pattern EKParticipantStatusDelegated
  , pattern EKParticipantStatusCompleted
  , pattern EKParticipantStatusInProcess
  , EKParticipantType(EKParticipantType)
  , pattern EKParticipantTypeUnknown
  , pattern EKParticipantTypePerson
  , pattern EKParticipantTypeRoom
  , pattern EKParticipantTypeResource
  , pattern EKParticipantTypeGroup

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.EventKit.Internal.Classes
import ObjC.EventKit.Internal.Enums
import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | ABPersonInAddressBook
--
-- Returns the ABPerson that represents this participant.
--
-- This method returns the ABPerson that represents this participant, if a match can be found based on email address in the address book passed. If we cannot find the participant, nil is returned.
--
-- ObjC selector: @- ABPersonInAddressBook:@
abPersonInAddressBook :: (IsEKParticipant ekParticipant, IsABAddressBook addressBook) => ekParticipant -> addressBook -> IO (Id ABPerson)
abPersonInAddressBook ekParticipant addressBook =
  sendMessage ekParticipant abPersonInAddressBookSelector (toABAddressBook addressBook)

-- | url
--
-- URL representing this participant.
--
-- ObjC selector: @- URL@
url :: IsEKParticipant ekParticipant => ekParticipant -> IO (Id NSURL)
url ekParticipant =
  sendMessage ekParticipant urlSelector

-- | name
--
-- Name of this participant.
--
-- ObjC selector: @- name@
name :: IsEKParticipant ekParticipant => ekParticipant -> IO (Id NSString)
name ekParticipant =
  sendMessage ekParticipant nameSelector

-- | participantStatus
--
-- The status of the attendee.
--
-- Returns the status of the attendee as a EKParticipantStatus value.
--
-- ObjC selector: @- participantStatus@
participantStatus :: IsEKParticipant ekParticipant => ekParticipant -> IO EKParticipantStatus
participantStatus ekParticipant =
  sendMessage ekParticipant participantStatusSelector

-- | participantRole
--
-- The role of the attendee.
--
-- Returns the role of the attendee as a EKParticipantRole value.
--
-- ObjC selector: @- participantRole@
participantRole :: IsEKParticipant ekParticipant => ekParticipant -> IO EKParticipantRole
participantRole ekParticipant =
  sendMessage ekParticipant participantRoleSelector

-- | participantType
--
-- The type of the attendee.
--
-- Returns the type of the attendee as a EKParticipantType value.
--
-- ObjC selector: @- participantType@
participantType :: IsEKParticipant ekParticipant => ekParticipant -> IO EKParticipantType
participantType ekParticipant =
  sendMessage ekParticipant participantTypeSelector

-- | currentUser
--
-- A boolean indicating whether this participant represents the                owner of this account.
--
-- ObjC selector: @- currentUser@
currentUser :: IsEKParticipant ekParticipant => ekParticipant -> IO Bool
currentUser ekParticipant =
  sendMessage ekParticipant currentUserSelector

-- | contactPredicate
--
-- Returns a predicate to use with Contacts.framework to retrieve the corresponding                CNContact instance.
--
-- This method returns a predicate that can be used with a CNContactStore to fetch                a CNContact instance for this participant, if one exists.
--
-- ObjC selector: @- contactPredicate@
contactPredicate :: IsEKParticipant ekParticipant => ekParticipant -> IO RawId
contactPredicate ekParticipant =
  sendMessage ekParticipant contactPredicateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ABPersonInAddressBook:@
abPersonInAddressBookSelector :: Selector '[Id ABAddressBook] (Id ABPerson)
abPersonInAddressBookSelector = mkSelector "ABPersonInAddressBook:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @participantStatus@
participantStatusSelector :: Selector '[] EKParticipantStatus
participantStatusSelector = mkSelector "participantStatus"

-- | @Selector@ for @participantRole@
participantRoleSelector :: Selector '[] EKParticipantRole
participantRoleSelector = mkSelector "participantRole"

-- | @Selector@ for @participantType@
participantTypeSelector :: Selector '[] EKParticipantType
participantTypeSelector = mkSelector "participantType"

-- | @Selector@ for @currentUser@
currentUserSelector :: Selector '[] Bool
currentUserSelector = mkSelector "currentUser"

-- | @Selector@ for @contactPredicate@
contactPredicateSelector :: Selector '[] RawId
contactPredicateSelector = mkSelector "contactPredicate"

