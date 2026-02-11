{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , oneTimeURLParticipantSelector
  , userIdentitySelector
  , roleSelector
  , setRoleSelector
  , typeSelector
  , setTypeSelector
  , acceptanceStatusSelector
  , permissionSelector
  , setPermissionSelector
  , participantIDSelector
  , isApprovedRequesterSelector
  , dateAddedToShareSelector

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

-- | Use @CKFetchShareParticipantsOperation@ to create a @CKShareParticipant@ object
--
-- ObjC selector: @- init@
init_ :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO (Id CKShareParticipant)
init_ ckShareParticipant  =
    sendMsg ckShareParticipant (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKShareParticipant)
new  =
  do
    cls' <- getRequiredClass "CKShareParticipant"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "oneTimeURLParticipant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- userIdentity@
userIdentity :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO (Id CKUserIdentity)
userIdentity ckShareParticipant  =
    sendMsg ckShareParticipant (mkSelector "userIdentity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The default participant role is @CKShareParticipantRolePrivateUser.@
--
-- ObjC selector: @- role@
role_ :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO CKShareParticipantRole
role_ ckShareParticipant  =
    fmap (coerce :: CLong -> CKShareParticipantRole) $ sendMsg ckShareParticipant (mkSelector "role") retCLong []

-- | The default participant role is @CKShareParticipantRolePrivateUser.@
--
-- ObjC selector: @- setRole:@
setRole :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> CKShareParticipantRole -> IO ()
setRole ckShareParticipant  value =
    sendMsg ckShareParticipant (mkSelector "setRole:") retVoid [argCLong (coerce value)]

-- | The default participant type is ``CloudKit/CKShareParticipantType/CKShareParticipantTypePrivateUser``.
--
-- ObjC selector: @- type@
type_ :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO CKShareParticipantType
type_ ckShareParticipant  =
    fmap (coerce :: CLong -> CKShareParticipantType) $ sendMsg ckShareParticipant (mkSelector "type") retCLong []

-- | The default participant type is ``CloudKit/CKShareParticipantType/CKShareParticipantTypePrivateUser``.
--
-- ObjC selector: @- setType:@
setType :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> CKShareParticipantType -> IO ()
setType ckShareParticipant  value =
    sendMsg ckShareParticipant (mkSelector "setType:") retVoid [argCLong (coerce value)]

-- | @- acceptanceStatus@
acceptanceStatus :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO CKShareParticipantAcceptanceStatus
acceptanceStatus ckShareParticipant  =
    fmap (coerce :: CLong -> CKShareParticipantAcceptanceStatus) $ sendMsg ckShareParticipant (mkSelector "acceptanceStatus") retCLong []

-- | The default permission for a new participant is @CKShareParticipantPermissionReadOnly.@
--
-- ObjC selector: @- permission@
permission :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO CKShareParticipantPermission
permission ckShareParticipant  =
    fmap (coerce :: CLong -> CKShareParticipantPermission) $ sendMsg ckShareParticipant (mkSelector "permission") retCLong []

-- | The default permission for a new participant is @CKShareParticipantPermissionReadOnly.@
--
-- ObjC selector: @- setPermission:@
setPermission :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> CKShareParticipantPermission -> IO ()
setPermission ckShareParticipant  value =
    sendMsg ckShareParticipant (mkSelector "setPermission:") retVoid [argCLong (coerce value)]

-- | A unique identifier for this participant.
--
-- ObjC selector: @- participantID@
participantID :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO (Id NSString)
participantID ckShareParticipant  =
    sendMsg ckShareParticipant (mkSelector "participantID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates whether the participant was originally a requester who was approved to join the share.
--
-- ObjC selector: @- isApprovedRequester@
isApprovedRequester :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO Bool
isApprovedRequester ckShareParticipant  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckShareParticipant (mkSelector "isApprovedRequester") retCULong []

-- | The date and time when the participant was added to the share.
--
-- This timestamp is set when the share is successfully saved to the server.
--
-- ObjC selector: @- dateAddedToShare@
dateAddedToShare :: IsCKShareParticipant ckShareParticipant => ckShareParticipant -> IO (Id NSDate)
dateAddedToShare ckShareParticipant  =
    sendMsg ckShareParticipant (mkSelector "dateAddedToShare") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @oneTimeURLParticipant@
oneTimeURLParticipantSelector :: Selector
oneTimeURLParticipantSelector = mkSelector "oneTimeURLParticipant"

-- | @Selector@ for @userIdentity@
userIdentitySelector :: Selector
userIdentitySelector = mkSelector "userIdentity"

-- | @Selector@ for @role@
roleSelector :: Selector
roleSelector = mkSelector "role"

-- | @Selector@ for @setRole:@
setRoleSelector :: Selector
setRoleSelector = mkSelector "setRole:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @acceptanceStatus@
acceptanceStatusSelector :: Selector
acceptanceStatusSelector = mkSelector "acceptanceStatus"

-- | @Selector@ for @permission@
permissionSelector :: Selector
permissionSelector = mkSelector "permission"

-- | @Selector@ for @setPermission:@
setPermissionSelector :: Selector
setPermissionSelector = mkSelector "setPermission:"

-- | @Selector@ for @participantID@
participantIDSelector :: Selector
participantIDSelector = mkSelector "participantID"

-- | @Selector@ for @isApprovedRequester@
isApprovedRequesterSelector :: Selector
isApprovedRequesterSelector = mkSelector "isApprovedRequester"

-- | @Selector@ for @dateAddedToShare@
dateAddedToShareSelector :: Selector
dateAddedToShareSelector = mkSelector "dateAddedToShare"

