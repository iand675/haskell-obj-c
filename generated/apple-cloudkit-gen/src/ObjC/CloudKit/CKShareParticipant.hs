{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKShareParticipant@.
module ObjC.CloudKit.CKShareParticipant
  ( CKShareParticipant
  , IsCKShareParticipant(..)
  , init_
  , new
  , oneTimeURLParticipant
  , userIdentity
  , role_
  , setRole
  , type_
  , setType
  , acceptanceStatus
  , permission
  , setPermission
  , participantID
  , isApprovedRequester
  , dateAddedToShare
  , acceptanceStatusSelector
  , dateAddedToShareSelector
  , initSelector
  , isApprovedRequesterSelector
  , newSelector
  , oneTimeURLParticipantSelector
  , participantIDSelector
  , permissionSelector
  , roleSelector
  , setPermissionSelector
  , setRoleSelector
  , setTypeSelector
  , typeSelector
  , userIdentitySelector

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

-- | Use @CKFetchShareParticipantsOperation@ to create a @CKShareParticipant@ object
--
-- ObjC selector: @- init@
init_ :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO (Id CKShareParticipant)
init_ ckShareParticipant =
  sendOwnedMessage ckShareParticipant initSelector

-- | @+ new@
new :: IO (Id CKShareParticipant)
new  =
  do
    cls' <- getRequiredClass "CKShareParticipant"
    sendOwnedClassMessage cls' newSelector

-- | Generate a unique URL for inviting a participant without knowing their handle
--
-- When a participant's email address / phone number / userRecordID isn't known up-front, a ``CKShareParticipant/oneTimeURLParticipant`` can be added to the share. Once the share is saved, a custom invitation link or one-time URL is available for the added participant via ``CKShare/oneTimeURLForParticipantID:``. This custom link can be used by any recipient user to fetch share metadata and accept the share.
--
-- Note that a one-time URL participant in the ``ParticipantAcceptanceStatus/pending`` state has empty ``CKUserIdentity/nameComponents`` and a nil ``CKUserIdentity/lookupInfo``.
--
-- ObjC selector: @+ oneTimeURLParticipant@
oneTimeURLParticipant :: IO (Id CKShareParticipant)
oneTimeURLParticipant  =
  do
    cls' <- getRequiredClass "CKShareParticipant"
    sendClassMessage cls' oneTimeURLParticipantSelector

-- | @- userIdentity@
userIdentity :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO (Id CKUserIdentity)
userIdentity ckShareParticipant =
  sendMessage ckShareParticipant userIdentitySelector

-- | The default participant role is @CKShareParticipantRolePrivateUser.@
--
-- ObjC selector: @- role@
role_ :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO CKShareParticipantRole
role_ ckShareParticipant =
  sendMessage ckShareParticipant roleSelector

-- | The default participant role is @CKShareParticipantRolePrivateUser.@
--
-- ObjC selector: @- setRole:@
setRole :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> CKShareParticipantRole -> IO ()
setRole ckShareParticipant value =
  sendMessage ckShareParticipant setRoleSelector value

-- | The default participant type is ``CloudKit/CKShareParticipantType/CKShareParticipantTypePrivateUser``.
--
-- ObjC selector: @- type@
type_ :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO CKShareParticipantType
type_ ckShareParticipant =
  sendMessage ckShareParticipant typeSelector

-- | The default participant type is ``CloudKit/CKShareParticipantType/CKShareParticipantTypePrivateUser``.
--
-- ObjC selector: @- setType:@
setType :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> CKShareParticipantType -> IO ()
setType ckShareParticipant value =
  sendMessage ckShareParticipant setTypeSelector value

-- | @- acceptanceStatus@
acceptanceStatus :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO CKShareParticipantAcceptanceStatus
acceptanceStatus ckShareParticipant =
  sendMessage ckShareParticipant acceptanceStatusSelector

-- | The default permission for a new participant is @CKShareParticipantPermissionReadOnly.@
--
-- ObjC selector: @- permission@
permission :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO CKShareParticipantPermission
permission ckShareParticipant =
  sendMessage ckShareParticipant permissionSelector

-- | The default permission for a new participant is @CKShareParticipantPermissionReadOnly.@
--
-- ObjC selector: @- setPermission:@
setPermission :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> CKShareParticipantPermission -> IO ()
setPermission ckShareParticipant value =
  sendMessage ckShareParticipant setPermissionSelector value

-- | A unique identifier for this participant.
--
-- ObjC selector: @- participantID@
participantID :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO (Id NSString)
participantID ckShareParticipant =
  sendMessage ckShareParticipant participantIDSelector

-- | Indicates whether the participant was originally a requester who was approved to join the share.
--
-- ObjC selector: @- isApprovedRequester@
isApprovedRequester :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO Bool
isApprovedRequester ckShareParticipant =
  sendMessage ckShareParticipant isApprovedRequesterSelector

-- | The date and time when the participant was added to the share.
--
-- This timestamp is set when the share is successfully saved to the server.
--
-- ObjC selector: @- dateAddedToShare@
dateAddedToShare :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO (Id NSDate)
dateAddedToShare ckShareParticipant =
  sendMessage ckShareParticipant dateAddedToShareSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKShareParticipant)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKShareParticipant)
newSelector = mkSelector "new"

-- | @Selector@ for @oneTimeURLParticipant@
oneTimeURLParticipantSelector :: Selector '[] (Id CKShareParticipant)
oneTimeURLParticipantSelector = mkSelector "oneTimeURLParticipant"

-- | @Selector@ for @userIdentity@
userIdentitySelector :: Selector '[] (Id CKUserIdentity)
userIdentitySelector = mkSelector "userIdentity"

-- | @Selector@ for @role@
roleSelector :: Selector '[] CKShareParticipantRole
roleSelector = mkSelector "role"

-- | @Selector@ for @setRole:@
setRoleSelector :: Selector '[CKShareParticipantRole] ()
setRoleSelector = mkSelector "setRole:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] CKShareParticipantType
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[CKShareParticipantType] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @acceptanceStatus@
acceptanceStatusSelector :: Selector '[] CKShareParticipantAcceptanceStatus
acceptanceStatusSelector = mkSelector "acceptanceStatus"

-- | @Selector@ for @permission@
permissionSelector :: Selector '[] CKShareParticipantPermission
permissionSelector = mkSelector "permission"

-- | @Selector@ for @setPermission:@
setPermissionSelector :: Selector '[CKShareParticipantPermission] ()
setPermissionSelector = mkSelector "setPermission:"

-- | @Selector@ for @participantID@
participantIDSelector :: Selector '[] (Id NSString)
participantIDSelector = mkSelector "participantID"

-- | @Selector@ for @isApprovedRequester@
isApprovedRequesterSelector :: Selector '[] Bool
isApprovedRequesterSelector = mkSelector "isApprovedRequester"

-- | @Selector@ for @dateAddedToShare@
dateAddedToShareSelector :: Selector '[] (Id NSDate)
dateAddedToShareSelector = mkSelector "dateAddedToShare"

