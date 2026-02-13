{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKShareMetadata@.
module ObjC.CloudKit.CKShareMetadata
  ( CKShareMetadata
  , IsCKShareMetadata(..)
  , init_
  , new
  , containerIdentifier
  , share
  , hierarchicalRootRecordID
  , participantRole
  , participantStatus
  , participantPermission
  , ownerIdentity
  , rootRecord
  , participantType
  , rootRecordID
  , containerIdentifierSelector
  , hierarchicalRootRecordIDSelector
  , initSelector
  , newSelector
  , ownerIdentitySelector
  , participantPermissionSelector
  , participantRoleSelector
  , participantStatusSelector
  , participantTypeSelector
  , rootRecordIDSelector
  , rootRecordSelector
  , shareSelector

  -- * Enum types
  , CKShareParticipantAcceptanceStatus(CKShareParticipantAcceptanceStatus)
  , pattern CKShareParticipantAcceptanceStatusUnknown
  , pattern CKShareParticipantAcceptanceStatusPending
  , pattern CKShareParticipantAcceptanceStatusAccepted
  , pattern CKShareParticipantAcceptanceStatusRemoved
  , CKShareParticipantPermission(CKShareParticipantPermission)
  , pattern CKShareParticipantPermissionUnknown
  , pattern CKShareParticipantPermissionNone
  , pattern CKShareParticipantPermissionReadOnly
  , pattern CKShareParticipantPermissionReadWrite
  , CKShareParticipantRole(CKShareParticipantRole)
  , pattern CKShareParticipantRoleUnknown
  , pattern CKShareParticipantRoleOwner
  , pattern CKShareParticipantRolePrivateUser
  , pattern CKShareParticipantRolePublicUser
  , pattern CKShareParticipantRoleAdministrator
  , CKShareParticipantType(CKShareParticipantType)
  , pattern CKShareParticipantTypeUnknown
  , pattern CKShareParticipantTypeOwner
  , pattern CKShareParticipantTypePrivateUser
  , pattern CKShareParticipantTypePublicUser

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id CKShareMetadata)
init_ ckShareMetadata =
  sendOwnedMessage ckShareMetadata initSelector

-- | @+ new@
new :: IO (Id CKShareMetadata)
new  =
  do
    cls' <- getRequiredClass "CKShareMetadata"
    sendOwnedClassMessage cls' newSelector

-- | @- containerIdentifier@
containerIdentifier :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id NSString)
containerIdentifier ckShareMetadata =
  sendMessage ckShareMetadata containerIdentifierSelector

-- | @- share@
share :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id CKShare)
share ckShareMetadata =
  sendMessage ckShareMetadata shareSelector

-- | @- hierarchicalRootRecordID@
hierarchicalRootRecordID :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id CKRecordID)
hierarchicalRootRecordID ckShareMetadata =
  sendMessage ckShareMetadata hierarchicalRootRecordIDSelector

-- | These properties reflect the participant properties of the user invoking CKFetchShareMetadataOperation
--
-- ObjC selector: @- participantRole@
participantRole :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO CKShareParticipantRole
participantRole ckShareMetadata =
  sendMessage ckShareMetadata participantRoleSelector

-- | @- participantStatus@
participantStatus :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO CKShareParticipantAcceptanceStatus
participantStatus ckShareMetadata =
  sendMessage ckShareMetadata participantStatusSelector

-- | @- participantPermission@
participantPermission :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO CKShareParticipantPermission
participantPermission ckShareMetadata =
  sendMessage ckShareMetadata participantPermissionSelector

-- | @- ownerIdentity@
ownerIdentity :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id CKUserIdentity)
ownerIdentity ckShareMetadata =
  sendMessage ckShareMetadata ownerIdentitySelector

-- | This is only present if the share metadata was returned from a CKFetchShareMetadataOperation with shouldFetchRootRecord set to YES
--
-- ObjC selector: @- rootRecord@
rootRecord :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id CKRecord)
rootRecord ckShareMetadata =
  sendMessage ckShareMetadata rootRecordSelector

-- | @- participantType@
participantType :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO CKShareParticipantType
participantType ckShareMetadata =
  sendMessage ckShareMetadata participantTypeSelector

-- | @- rootRecordID@
rootRecordID :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id CKRecordID)
rootRecordID ckShareMetadata =
  sendMessage ckShareMetadata rootRecordIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKShareMetadata)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKShareMetadata)
newSelector = mkSelector "new"

-- | @Selector@ for @containerIdentifier@
containerIdentifierSelector :: Selector '[] (Id NSString)
containerIdentifierSelector = mkSelector "containerIdentifier"

-- | @Selector@ for @share@
shareSelector :: Selector '[] (Id CKShare)
shareSelector = mkSelector "share"

-- | @Selector@ for @hierarchicalRootRecordID@
hierarchicalRootRecordIDSelector :: Selector '[] (Id CKRecordID)
hierarchicalRootRecordIDSelector = mkSelector "hierarchicalRootRecordID"

-- | @Selector@ for @participantRole@
participantRoleSelector :: Selector '[] CKShareParticipantRole
participantRoleSelector = mkSelector "participantRole"

-- | @Selector@ for @participantStatus@
participantStatusSelector :: Selector '[] CKShareParticipantAcceptanceStatus
participantStatusSelector = mkSelector "participantStatus"

-- | @Selector@ for @participantPermission@
participantPermissionSelector :: Selector '[] CKShareParticipantPermission
participantPermissionSelector = mkSelector "participantPermission"

-- | @Selector@ for @ownerIdentity@
ownerIdentitySelector :: Selector '[] (Id CKUserIdentity)
ownerIdentitySelector = mkSelector "ownerIdentity"

-- | @Selector@ for @rootRecord@
rootRecordSelector :: Selector '[] (Id CKRecord)
rootRecordSelector = mkSelector "rootRecord"

-- | @Selector@ for @participantType@
participantTypeSelector :: Selector '[] CKShareParticipantType
participantTypeSelector = mkSelector "participantType"

-- | @Selector@ for @rootRecordID@
rootRecordIDSelector :: Selector '[] (Id CKRecordID)
rootRecordIDSelector = mkSelector "rootRecordID"

