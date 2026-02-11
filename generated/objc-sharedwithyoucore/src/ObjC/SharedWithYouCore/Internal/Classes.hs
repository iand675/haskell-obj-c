{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.SharedWithYouCore.Internal.Classes (
    module ObjC.SharedWithYouCore.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- SWAction ----------

-- | Phantom type for @SWAction@.
data SWAction

instance IsObjCObject (Id SWAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWAction"

class IsNSObject a => IsSWAction a where
  toSWAction :: a -> Id SWAction

instance IsSWAction (Id SWAction) where
  toSWAction = unsafeCastId

instance IsNSObject (Id SWAction) where
  toNSObject = unsafeCastId

-- ---------- SWCollaborationCoordinator ----------

-- | Phantom type for @SWCollaborationCoordinator@.
data SWCollaborationCoordinator

instance IsObjCObject (Id SWCollaborationCoordinator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWCollaborationCoordinator"

class IsNSObject a => IsSWCollaborationCoordinator a where
  toSWCollaborationCoordinator :: a -> Id SWCollaborationCoordinator

instance IsSWCollaborationCoordinator (Id SWCollaborationCoordinator) where
  toSWCollaborationCoordinator = unsafeCastId

instance IsNSObject (Id SWCollaborationCoordinator) where
  toNSObject = unsafeCastId

-- ---------- SWCollaborationMetadata ----------

-- | Phantom type for @SWCollaborationMetadata@.
data SWCollaborationMetadata

instance IsObjCObject (Id SWCollaborationMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWCollaborationMetadata"

class IsNSObject a => IsSWCollaborationMetadata a where
  toSWCollaborationMetadata :: a -> Id SWCollaborationMetadata

instance IsSWCollaborationMetadata (Id SWCollaborationMetadata) where
  toSWCollaborationMetadata = unsafeCastId

instance IsNSObject (Id SWCollaborationMetadata) where
  toNSObject = unsafeCastId

-- ---------- SWCollaborationOption ----------

-- | SWCollaborationOption
--
-- A user-facing option for configuring a shareable collaborative item
--
-- SWCollaborationOptions represent the available settings (such as permissions) a user can configure on a collaborative item
-- 
-- Phantom type for @SWCollaborationOption@.
data SWCollaborationOption

instance IsObjCObject (Id SWCollaborationOption) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWCollaborationOption"

class IsNSObject a => IsSWCollaborationOption a where
  toSWCollaborationOption :: a -> Id SWCollaborationOption

instance IsSWCollaborationOption (Id SWCollaborationOption) where
  toSWCollaborationOption = unsafeCastId

instance IsNSObject (Id SWCollaborationOption) where
  toNSObject = unsafeCastId

-- ---------- SWCollaborationOptionsGroup ----------

-- | SWCollaborationOptionsGroup
--
-- A group of SWCollaborationOptions that should be displayed and configured together
--
-- Use SWCollaborationOptionsGroup to represent a group of options used to configure a collaborative item. An SWCollaborationOptionsGroup with one option indicates a switch.
-- 
-- Phantom type for @SWCollaborationOptionsGroup@.
data SWCollaborationOptionsGroup

instance IsObjCObject (Id SWCollaborationOptionsGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWCollaborationOptionsGroup"

class IsNSObject a => IsSWCollaborationOptionsGroup a where
  toSWCollaborationOptionsGroup :: a -> Id SWCollaborationOptionsGroup

instance IsSWCollaborationOptionsGroup (Id SWCollaborationOptionsGroup) where
  toSWCollaborationOptionsGroup = unsafeCastId

instance IsNSObject (Id SWCollaborationOptionsGroup) where
  toNSObject = unsafeCastId

-- ---------- SWCollaborationShareOptions ----------

-- | SWCollaborationShareOptions
--
-- represents the state of the collaboration options for the document.
--
-- SWCollaborationShareOptions contains the SWCollaborationOptionsGorups that are available for the collaboration as well as a string, provided by the client, that summarizes the state of the selected options.
-- 
-- Phantom type for @SWCollaborationShareOptions@.
data SWCollaborationShareOptions

instance IsObjCObject (Id SWCollaborationShareOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWCollaborationShareOptions"

class IsNSObject a => IsSWCollaborationShareOptions a where
  toSWCollaborationShareOptions :: a -> Id SWCollaborationShareOptions

instance IsSWCollaborationShareOptions (Id SWCollaborationShareOptions) where
  toSWCollaborationShareOptions = unsafeCastId

instance IsNSObject (Id SWCollaborationShareOptions) where
  toNSObject = unsafeCastId

-- ---------- SWPerson ----------

-- | Phantom type for @SWPerson@.
data SWPerson

instance IsObjCObject (Id SWPerson) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWPerson"

class IsNSObject a => IsSWPerson a where
  toSWPerson :: a -> Id SWPerson

instance IsSWPerson (Id SWPerson) where
  toSWPerson = unsafeCastId

instance IsNSObject (Id SWPerson) where
  toNSObject = unsafeCastId

-- ---------- SWPersonIdentity ----------

-- | SWPersonIdentity
--
-- Represents an opaque Merkle tree where the root hash of the tree can uniquely identify the individual by all of their devices. The individual's devices can prove themselves to be part of this identity, and can then be used for cryptographic signatures for that individual.
-- 
-- Phantom type for @SWPersonIdentity@.
data SWPersonIdentity

instance IsObjCObject (Id SWPersonIdentity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWPersonIdentity"

class IsNSObject a => IsSWPersonIdentity a where
  toSWPersonIdentity :: a -> Id SWPersonIdentity

instance IsSWPersonIdentity (Id SWPersonIdentity) where
  toSWPersonIdentity = unsafeCastId

instance IsNSObject (Id SWPersonIdentity) where
  toNSObject = unsafeCastId

-- ---------- SWPersonIdentityProof ----------

-- | SWPersonIdentityProof
--
-- Represents an opaque Merkle tree proof of inclusion. Inclusion hashes are provided to verify that the individual device has access to the document.
-- 
-- Phantom type for @SWPersonIdentityProof@.
data SWPersonIdentityProof

instance IsObjCObject (Id SWPersonIdentityProof) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWPersonIdentityProof"

class IsNSObject a => IsSWPersonIdentityProof a where
  toSWPersonIdentityProof :: a -> Id SWPersonIdentityProof

instance IsSWPersonIdentityProof (Id SWPersonIdentityProof) where
  toSWPersonIdentityProof = unsafeCastId

instance IsNSObject (Id SWPersonIdentityProof) where
  toNSObject = unsafeCastId

-- ---------- SWStartCollaborationAction ----------

-- | Phantom type for @SWStartCollaborationAction@.
data SWStartCollaborationAction

instance IsObjCObject (Id SWStartCollaborationAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWStartCollaborationAction"

class IsSWAction a => IsSWStartCollaborationAction a where
  toSWStartCollaborationAction :: a -> Id SWStartCollaborationAction

instance IsSWStartCollaborationAction (Id SWStartCollaborationAction) where
  toSWStartCollaborationAction = unsafeCastId

instance IsNSObject (Id SWStartCollaborationAction) where
  toNSObject = unsafeCastId

instance IsSWAction (Id SWStartCollaborationAction) where
  toSWAction = unsafeCastId

-- ---------- SWUpdateCollaborationParticipantsAction ----------

-- | Phantom type for @SWUpdateCollaborationParticipantsAction@.
data SWUpdateCollaborationParticipantsAction

instance IsObjCObject (Id SWUpdateCollaborationParticipantsAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWUpdateCollaborationParticipantsAction"

class IsSWAction a => IsSWUpdateCollaborationParticipantsAction a where
  toSWUpdateCollaborationParticipantsAction :: a -> Id SWUpdateCollaborationParticipantsAction

instance IsSWUpdateCollaborationParticipantsAction (Id SWUpdateCollaborationParticipantsAction) where
  toSWUpdateCollaborationParticipantsAction = unsafeCastId

instance IsNSObject (Id SWUpdateCollaborationParticipantsAction) where
  toNSObject = unsafeCastId

instance IsSWAction (Id SWUpdateCollaborationParticipantsAction) where
  toSWAction = unsafeCastId

-- ---------- SWCollaborationOptionsPickerGroup ----------

-- | SWCollaborationOptionsPickerGroup
--
-- Represents a group of SWCollaborationOptions that should be grouped together in a picker list, with mutually exclusive options.
--
-- SWCollaborationOptionsPickerGroup is displayed as a picker view. Only one option in the group can be selected by the user.
-- 
-- Phantom type for @SWCollaborationOptionsPickerGroup@.
data SWCollaborationOptionsPickerGroup

instance IsObjCObject (Id SWCollaborationOptionsPickerGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWCollaborationOptionsPickerGroup"

class IsSWCollaborationOptionsGroup a => IsSWCollaborationOptionsPickerGroup a where
  toSWCollaborationOptionsPickerGroup :: a -> Id SWCollaborationOptionsPickerGroup

instance IsSWCollaborationOptionsPickerGroup (Id SWCollaborationOptionsPickerGroup) where
  toSWCollaborationOptionsPickerGroup = unsafeCastId

instance IsNSObject (Id SWCollaborationOptionsPickerGroup) where
  toNSObject = unsafeCastId

instance IsSWCollaborationOptionsGroup (Id SWCollaborationOptionsPickerGroup) where
  toSWCollaborationOptionsGroup = unsafeCastId

-- ---------- SWSignedPersonIdentityProof ----------

-- | Phantom type for @SWSignedPersonIdentityProof@.
data SWSignedPersonIdentityProof

instance IsObjCObject (Id SWSignedPersonIdentityProof) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWSignedPersonIdentityProof"

class IsSWPersonIdentityProof a => IsSWSignedPersonIdentityProof a where
  toSWSignedPersonIdentityProof :: a -> Id SWSignedPersonIdentityProof

instance IsSWSignedPersonIdentityProof (Id SWSignedPersonIdentityProof) where
  toSWSignedPersonIdentityProof = unsafeCastId

instance IsNSObject (Id SWSignedPersonIdentityProof) where
  toNSObject = unsafeCastId

instance IsSWPersonIdentityProof (Id SWSignedPersonIdentityProof) where
  toSWPersonIdentityProof = unsafeCastId
