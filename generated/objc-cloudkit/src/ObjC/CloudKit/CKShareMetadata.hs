{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , containerIdentifierSelector
  , shareSelector
  , hierarchicalRootRecordIDSelector
  , participantRoleSelector
  , participantStatusSelector
  , participantPermissionSelector
  , ownerIdentitySelector
  , rootRecordSelector
  , participantTypeSelector
  , rootRecordIDSelector

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

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id CKShareMetadata)
init_ ckShareMetadata  =
  sendMsg ckShareMetadata (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKShareMetadata)
new  =
  do
    cls' <- getRequiredClass "CKShareMetadata"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- containerIdentifier@
containerIdentifier :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id NSString)
containerIdentifier ckShareMetadata  =
  sendMsg ckShareMetadata (mkSelector "containerIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- share@
share :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id CKShare)
share ckShareMetadata  =
  sendMsg ckShareMetadata (mkSelector "share") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hierarchicalRootRecordID@
hierarchicalRootRecordID :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id CKRecordID)
hierarchicalRootRecordID ckShareMetadata  =
  sendMsg ckShareMetadata (mkSelector "hierarchicalRootRecordID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | These properties reflect the participant properties of the user invoking CKFetchShareMetadataOperation
--
-- ObjC selector: @- participantRole@
participantRole :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO CKShareParticipantRole
participantRole ckShareMetadata  =
  fmap (coerce :: CLong -> CKShareParticipantRole) $ sendMsg ckShareMetadata (mkSelector "participantRole") retCLong []

-- | @- participantStatus@
participantStatus :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO CKShareParticipantAcceptanceStatus
participantStatus ckShareMetadata  =
  fmap (coerce :: CLong -> CKShareParticipantAcceptanceStatus) $ sendMsg ckShareMetadata (mkSelector "participantStatus") retCLong []

-- | @- participantPermission@
participantPermission :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO CKShareParticipantPermission
participantPermission ckShareMetadata  =
  fmap (coerce :: CLong -> CKShareParticipantPermission) $ sendMsg ckShareMetadata (mkSelector "participantPermission") retCLong []

-- | @- ownerIdentity@
ownerIdentity :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id CKUserIdentity)
ownerIdentity ckShareMetadata  =
  sendMsg ckShareMetadata (mkSelector "ownerIdentity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This is only present if the share metadata was returned from a CKFetchShareMetadataOperation with shouldFetchRootRecord set to YES
--
-- ObjC selector: @- rootRecord@
rootRecord :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id CKRecord)
rootRecord ckShareMetadata  =
  sendMsg ckShareMetadata (mkSelector "rootRecord") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- participantType@
participantType :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO CKShareParticipantType
participantType ckShareMetadata  =
  fmap (coerce :: CLong -> CKShareParticipantType) $ sendMsg ckShareMetadata (mkSelector "participantType") retCLong []

-- | @- rootRecordID@
rootRecordID :: IsCKShareMetadata ckShareMetadata => ckShareMetadata -> IO (Id CKRecordID)
rootRecordID ckShareMetadata  =
  sendMsg ckShareMetadata (mkSelector "rootRecordID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @containerIdentifier@
containerIdentifierSelector :: Selector
containerIdentifierSelector = mkSelector "containerIdentifier"

-- | @Selector@ for @share@
shareSelector :: Selector
shareSelector = mkSelector "share"

-- | @Selector@ for @hierarchicalRootRecordID@
hierarchicalRootRecordIDSelector :: Selector
hierarchicalRootRecordIDSelector = mkSelector "hierarchicalRootRecordID"

-- | @Selector@ for @participantRole@
participantRoleSelector :: Selector
participantRoleSelector = mkSelector "participantRole"

-- | @Selector@ for @participantStatus@
participantStatusSelector :: Selector
participantStatusSelector = mkSelector "participantStatus"

-- | @Selector@ for @participantPermission@
participantPermissionSelector :: Selector
participantPermissionSelector = mkSelector "participantPermission"

-- | @Selector@ for @ownerIdentity@
ownerIdentitySelector :: Selector
ownerIdentitySelector = mkSelector "ownerIdentity"

-- | @Selector@ for @rootRecord@
rootRecordSelector :: Selector
rootRecordSelector = mkSelector "rootRecord"

-- | @Selector@ for @participantType@
participantTypeSelector :: Selector
participantTypeSelector = mkSelector "participantType"

-- | @Selector@ for @rootRecordID@
rootRecordIDSelector :: Selector
rootRecordIDSelector = mkSelector "rootRecordID"

