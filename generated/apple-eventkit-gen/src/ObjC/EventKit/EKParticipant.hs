{-# LANGUAGE PatternSynonyms #-}
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
  , urlSelector
  , nameSelector
  , participantStatusSelector
  , participantRoleSelector
  , participantTypeSelector
  , currentUserSelector
  , contactPredicateSelector

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
abPersonInAddressBook ekParticipant  addressBook =
  withObjCPtr addressBook $ \raw_addressBook ->
      sendMsg ekParticipant (mkSelector "ABPersonInAddressBook:") (retPtr retVoid) [argPtr (castPtr raw_addressBook :: Ptr ())] >>= retainedObject . castPtr

-- | url
--
-- URL representing this participant.
--
-- ObjC selector: @- URL@
url :: IsEKParticipant ekParticipant => ekParticipant -> IO (Id NSURL)
url ekParticipant  =
    sendMsg ekParticipant (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Name of this participant.
--
-- ObjC selector: @- name@
name :: IsEKParticipant ekParticipant => ekParticipant -> IO (Id NSString)
name ekParticipant  =
    sendMsg ekParticipant (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | participantStatus
--
-- The status of the attendee.
--
-- Returns the status of the attendee as a EKParticipantStatus value.
--
-- ObjC selector: @- participantStatus@
participantStatus :: IsEKParticipant ekParticipant => ekParticipant -> IO EKParticipantStatus
participantStatus ekParticipant  =
    fmap (coerce :: CLong -> EKParticipantStatus) $ sendMsg ekParticipant (mkSelector "participantStatus") retCLong []

-- | participantRole
--
-- The role of the attendee.
--
-- Returns the role of the attendee as a EKParticipantRole value.
--
-- ObjC selector: @- participantRole@
participantRole :: IsEKParticipant ekParticipant => ekParticipant -> IO EKParticipantRole
participantRole ekParticipant  =
    fmap (coerce :: CLong -> EKParticipantRole) $ sendMsg ekParticipant (mkSelector "participantRole") retCLong []

-- | participantType
--
-- The type of the attendee.
--
-- Returns the type of the attendee as a EKParticipantType value.
--
-- ObjC selector: @- participantType@
participantType :: IsEKParticipant ekParticipant => ekParticipant -> IO EKParticipantType
participantType ekParticipant  =
    fmap (coerce :: CLong -> EKParticipantType) $ sendMsg ekParticipant (mkSelector "participantType") retCLong []

-- | currentUser
--
-- A boolean indicating whether this participant represents the                owner of this account.
--
-- ObjC selector: @- currentUser@
currentUser :: IsEKParticipant ekParticipant => ekParticipant -> IO Bool
currentUser ekParticipant  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekParticipant (mkSelector "currentUser") retCULong []

-- | contactPredicate
--
-- Returns a predicate to use with Contacts.framework to retrieve the corresponding                CNContact instance.
--
-- This method returns a predicate that can be used with a CNContactStore to fetch                a CNContact instance for this participant, if one exists.
--
-- ObjC selector: @- contactPredicate@
contactPredicate :: IsEKParticipant ekParticipant => ekParticipant -> IO RawId
contactPredicate ekParticipant  =
    fmap (RawId . castPtr) $ sendMsg ekParticipant (mkSelector "contactPredicate") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ABPersonInAddressBook:@
abPersonInAddressBookSelector :: Selector
abPersonInAddressBookSelector = mkSelector "ABPersonInAddressBook:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @participantStatus@
participantStatusSelector :: Selector
participantStatusSelector = mkSelector "participantStatus"

-- | @Selector@ for @participantRole@
participantRoleSelector :: Selector
participantRoleSelector = mkSelector "participantRole"

-- | @Selector@ for @participantType@
participantTypeSelector :: Selector
participantTypeSelector = mkSelector "participantType"

-- | @Selector@ for @currentUser@
currentUserSelector :: Selector
currentUserSelector = mkSelector "currentUser"

-- | @Selector@ for @contactPredicate@
contactPredicateSelector :: Selector
contactPredicateSelector = mkSelector "contactPredicate"

