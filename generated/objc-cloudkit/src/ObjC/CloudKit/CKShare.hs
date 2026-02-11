{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKShare
--
-- Like CKRecords, CKShares can store arbitrary key-value pairs.  They are modified and fetched in the same manner.  A share, its root record, and its root record's children records will only appear in a participant's CKFetchRecordChangesOperation's results after the share has been accepted by that participant.  Clients have access to the share (and optionally the root record) before accepting a share, via the CKShareMetadata object.  Note that in order to access a root record before accepting a share, you must run a CKFetchShareMetadataOperation requesting the root record.  A CKShare will appear in a CKFetchRecordChangesOperation's results set whenever the participant list is updated.  For that reason, you shouldn't place heavy key-value pairs in it.
--
-- Generated bindings for @CKShare@.
module ObjC.CloudKit.CKShare
  ( CKShare
  , IsCKShare(..)
  , initWithRootRecord
  , initWithRootRecord_shareID
  , initWithRecordZoneID
  , initWithCoder
  , addParticipant
  , removeParticipant
  , oneTimeURLForParticipantID
  , init_
  , new
  , initWithRecordType
  , initWithRecordType_recordID
  , initWithRecordType_zoneID
  , denyRequesters
  , blockRequesters
  , unblockIdentities
  , publicPermission
  , setPublicPermission
  , url
  , participants
  , owner
  , currentUserParticipant
  , requesters
  , blockedIdentities
  , allowsAccessRequests
  , setAllowsAccessRequests
  , initWithRootRecordSelector
  , initWithRootRecord_shareIDSelector
  , initWithRecordZoneIDSelector
  , initWithCoderSelector
  , addParticipantSelector
  , removeParticipantSelector
  , oneTimeURLForParticipantIDSelector
  , initSelector
  , newSelector
  , initWithRecordTypeSelector
  , initWithRecordType_recordIDSelector
  , initWithRecordType_zoneIDSelector
  , denyRequestersSelector
  , blockRequestersSelector
  , unblockIdentitiesSelector
  , publicPermissionSelector
  , setPublicPermissionSelector
  , urlSelector
  , participantsSelector
  , ownerSelector
  , currentUserParticipantSelector
  , requestersSelector
  , blockedIdentitiesSelector
  , allowsAccessRequestsSelector
  , setAllowsAccessRequestsSelector

  -- * Enum types
  , CKShareParticipantPermission(CKShareParticipantPermission)
  , pattern CKShareParticipantPermissionUnknown
  , pattern CKShareParticipantPermissionNone
  , pattern CKShareParticipantPermissionReadOnly
  , pattern CKShareParticipantPermissionReadWrite

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

-- | When saving a newly created CKShare, you must save the share and its rootRecord in the same CKModifyRecordsOperation batch.
--
-- ObjC selector: @- initWithRootRecord:@
initWithRootRecord :: (IsCKShare ckShare, IsCKRecord rootRecord) => ckShare -> rootRecord -> IO (Id CKShare)
initWithRootRecord ckShare  rootRecord =
withObjCPtr rootRecord $ \raw_rootRecord ->
    sendMsg ckShare (mkSelector "initWithRootRecord:") (retPtr retVoid) [argPtr (castPtr raw_rootRecord :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithRootRecord:shareID:@
initWithRootRecord_shareID :: (IsCKShare ckShare, IsCKRecord rootRecord, IsCKRecordID shareID) => ckShare -> rootRecord -> shareID -> IO (Id CKShare)
initWithRootRecord_shareID ckShare  rootRecord shareID =
withObjCPtr rootRecord $ \raw_rootRecord ->
  withObjCPtr shareID $ \raw_shareID ->
      sendMsg ckShare (mkSelector "initWithRootRecord:shareID:") (retPtr retVoid) [argPtr (castPtr raw_rootRecord :: Ptr ()), argPtr (castPtr raw_shareID :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a zone-wide @CKShare.@  A zone-wide @CKShare@ can only exist in a zone with sharing capability @CKRecordZoneCapabilityZoneWideSharing.@ Only one such share can exist in a zone at a time.
--
-- All records in this zone will appear in a participant's @CKFetchRecordZoneChangesOperation@ results in the shared database after the share has been accepted by the participant.
--
-- Since these shares do not have an associated root record, @shouldFetchRootRecord@ and @rootRecordDesiredKeys@ are always ignored when running a @CKFetchShareMetadataOperation@ on a zone-wide share URL. Additionally, @rootRecordID@ on the resulting @CKShareMetadata@ is always absent.
--
-- ObjC selector: @- initWithRecordZoneID:@
initWithRecordZoneID :: (IsCKShare ckShare, IsCKRecordZoneID recordZoneID) => ckShare -> recordZoneID -> IO (Id CKShare)
initWithRecordZoneID ckShare  recordZoneID =
withObjCPtr recordZoneID $ \raw_recordZoneID ->
    sendMsg ckShare (mkSelector "initWithRecordZoneID:") (retPtr retVoid) [argPtr (castPtr raw_recordZoneID :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsCKShare ckShare, IsNSCoder aDecoder) => ckShare -> aDecoder -> IO (Id CKShare)
initWithCoder ckShare  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg ckShare (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | If a participant with a matching userIdentity already exists, then that existing participant's properties will be updated; no new participant will be added.  A ``CKShareParticipant`` instance that has already been added to one ``CKShare`` cannot be added to another, unless it is removed from the first ``CKShare`` through @removeParticipant@.  In order to modify the list of participants, a share must have publicPermission set to @CKShareParticipantPermissionNone.@  That is, you cannot mix-and-match private users and public users in the same share.
--
-- See: CKShareParticipantRole
--
-- ObjC selector: @- addParticipant:@
addParticipant :: (IsCKShare ckShare, IsCKShareParticipant participant) => ckShare -> participant -> IO ()
addParticipant ckShare  participant =
withObjCPtr participant $ \raw_participant ->
    sendMsg ckShare (mkSelector "addParticipant:") retVoid [argPtr (castPtr raw_participant :: Ptr ())]

-- | It's not allowed to call @removeParticipant@ on a ``CKShare`` with a ``CKShareParticipant`` that has never been added to that share through @addParticipant@.
--
-- ObjC selector: @- removeParticipant:@
removeParticipant :: (IsCKShare ckShare, IsCKShareParticipant participant) => ckShare -> participant -> IO ()
removeParticipant ckShare  participant =
withObjCPtr participant $ \raw_participant ->
    sendMsg ckShare (mkSelector "removeParticipant:") retVoid [argPtr (castPtr raw_participant :: Ptr ())]

-- | Invitation URLs that can be used by any receiver to claim the associated participantID and join the share.
--
-- Only available after a share record has been saved to the server for participants created via ``CKShareParticipant/oneTimeURLParticipant``. One-time URLs are stable, and tied to the associated participantIDs as long as the participant is part of the share. Typically, a recipient user invited via their handle is provided a ``URL`` directly by the share's owner. However, any user can also use a one-time URL in the same manner to fetch share metadata and accept the share. After share acceptance, the one-time URL becomes functionally equivalent to the regular ``URL``.
--
-- - Parameters:   - participantID: The ``CKShareParticipant/participantID`` corresponding to the ``CKShareParticipant/oneTimeURLParticipant`` added to the share.
--
-- ObjC selector: @- oneTimeURLForParticipantID:@
oneTimeURLForParticipantID :: (IsCKShare ckShare, IsNSString participantID) => ckShare -> participantID -> IO (Id NSURL)
oneTimeURLForParticipantID ckShare  participantID =
withObjCPtr participantID $ \raw_participantID ->
    sendMsg ckShare (mkSelector "oneTimeURLForParticipantID:") (retPtr retVoid) [argPtr (castPtr raw_participantID :: Ptr ())] >>= retainedObject . castPtr

-- | These superclass-provided initializers are not allowed for CKShare
--
-- ObjC selector: @- init@
init_ :: IsCKShare ckShare => ckShare -> IO (Id CKShare)
init_ ckShare  =
  sendMsg ckShare (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKShare)
new  =
  do
    cls' <- getRequiredClass "CKShare"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithRecordType:@
initWithRecordType :: (IsCKShare ckShare, IsNSString recordType) => ckShare -> recordType -> IO (Id CKShare)
initWithRecordType ckShare  recordType =
withObjCPtr recordType $ \raw_recordType ->
    sendMsg ckShare (mkSelector "initWithRecordType:") (retPtr retVoid) [argPtr (castPtr raw_recordType :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithRecordType:recordID:@
initWithRecordType_recordID :: (IsCKShare ckShare, IsNSString recordType, IsCKRecordID recordID) => ckShare -> recordType -> recordID -> IO (Id CKShare)
initWithRecordType_recordID ckShare  recordType recordID =
withObjCPtr recordType $ \raw_recordType ->
  withObjCPtr recordID $ \raw_recordID ->
      sendMsg ckShare (mkSelector "initWithRecordType:recordID:") (retPtr retVoid) [argPtr (castPtr raw_recordType :: Ptr ()), argPtr (castPtr raw_recordID :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithRecordType:zoneID:@
initWithRecordType_zoneID :: (IsCKShare ckShare, IsNSString recordType, IsCKRecordZoneID zoneID) => ckShare -> recordType -> zoneID -> IO (Id CKShare)
initWithRecordType_zoneID ckShare  recordType zoneID =
withObjCPtr recordType $ \raw_recordType ->
  withObjCPtr zoneID $ \raw_zoneID ->
      sendMsg ckShare (mkSelector "initWithRecordType:zoneID:") (retPtr retVoid) [argPtr (castPtr raw_recordType :: Ptr ()), argPtr (castPtr raw_zoneID :: Ptr ())] >>= ownedObject . castPtr

-- | Denies access requests from specified users.
--
-- Use this method to deny pending access requests from uninvited users. Denied requesters are removed from the ``CloudKit/CKShare/requesters`` array. To persist the changes, save the share to the server after calling this method.
--
-- After denial, requesters can still submit new access requests unless explicitly blocked using ``CloudKit/CKShare/blockRequesters:``.
--
-- Only the share owner or an administrator can invoke this method. Attempts by other participants result in an exception.
--
-- - Parameter requesters: An array of ``CKShareAccessRequester`` objects to deny.
--
-- ObjC selector: @- denyRequesters:@
denyRequesters :: (IsCKShare ckShare, IsNSArray requesters) => ckShare -> requesters -> IO ()
denyRequesters ckShare  requesters =
withObjCPtr requesters $ \raw_requesters ->
    sendMsg ckShare (mkSelector "denyRequesters:") retVoid [argPtr (castPtr raw_requesters :: Ptr ())]

-- | Blocks specified users from requesting access to this share.
--
-- Blocking prevents users from submitting future access requests and removes existing participants from the share. Blocked requesters appear in the ``CloudKit/CKShare/blockedIdentities`` array.
--
-- To persist this change, save the share to the server after calling this method.
--
-- Only the share owner or an administrator can invoke this method. Attempts by other participants result in an exception.
--
-- - Parameter requesters: An array of ``CKShareAccessRequester`` objects to block.
--
-- ObjC selector: @- blockRequesters:@
blockRequesters :: (IsCKShare ckShare, IsNSArray requesters) => ckShare -> requesters -> IO ()
blockRequesters ckShare  requesters =
withObjCPtr requesters $ \raw_requesters ->
    sendMsg ckShare (mkSelector "blockRequesters:") retVoid [argPtr (castPtr raw_requesters :: Ptr ())]

-- | Unblocks previously blocked users, allowing them to request access again.
--
-- Use this method to remove specified identities from the ``CloudKit/CKShare/blockedIdentities`` array. Unblocked identities can request access again if access requests are enabled.
--
-- To persist this change, save the share to the server after calling this method.
--
-- Only the share owner or an administrator can invoke this method. Attempts by other participants result in an exception.
--
-- - Parameter blockedIdentities: An array of ``CKShareBlockedIdentity`` objects to unblock.
--
-- ObjC selector: @- unblockIdentities:@
unblockIdentities :: (IsCKShare ckShare, IsNSArray blockedIdentities) => ckShare -> blockedIdentities -> IO ()
unblockIdentities ckShare  blockedIdentities =
withObjCPtr blockedIdentities $ \raw_blockedIdentities ->
    sendMsg ckShare (mkSelector "unblockIdentities:") retVoid [argPtr (castPtr raw_blockedIdentities :: Ptr ())]

-- | Defines what permission a user has when not explicitly added to the share.
--
-- Shares with @publicPermission@ more permissive than @CKShareParticipantPermissionNone@ can be joined by any user with access to the share's shareURL.  By default, public permission is @CKShareParticipantPermissionNone.@  Changing the public permission to @CKShareParticipantPermissionReadOnly@ or @CKShareParticipantPermissionReadWrite@ will result in all pending participants being removed.  Already-accepted participants will remain on the share.  Changing the public permission to @CKShareParticipantPermissionNone@ will result in all participants being removed from the share.  You may subsequently choose to call @addParticipant:@ before saving the share, those participants will be added to the share.
--
-- ObjC selector: @- publicPermission@
publicPermission :: IsCKShare ckShare => ckShare -> IO CKShareParticipantPermission
publicPermission ckShare  =
  fmap (coerce :: CLong -> CKShareParticipantPermission) $ sendMsg ckShare (mkSelector "publicPermission") retCLong []

-- | Defines what permission a user has when not explicitly added to the share.
--
-- Shares with @publicPermission@ more permissive than @CKShareParticipantPermissionNone@ can be joined by any user with access to the share's shareURL.  By default, public permission is @CKShareParticipantPermissionNone.@  Changing the public permission to @CKShareParticipantPermissionReadOnly@ or @CKShareParticipantPermissionReadWrite@ will result in all pending participants being removed.  Already-accepted participants will remain on the share.  Changing the public permission to @CKShareParticipantPermissionNone@ will result in all participants being removed from the share.  You may subsequently choose to call @addParticipant:@ before saving the share, those participants will be added to the share.
--
-- ObjC selector: @- setPublicPermission:@
setPublicPermission :: IsCKShare ckShare => ckShare -> CKShareParticipantPermission -> IO ()
setPublicPermission ckShare  value =
  sendMsg ckShare (mkSelector "setPublicPermission:") retVoid [argCLong (coerce value)]

-- | A URL that can be used to invite participants to this share.
--
-- Only available after share record has been saved to the server.  This url is stable, and is tied to the rootRecord.  That is, if you share a rootRecord, delete the share, and re-share the same rootRecord via a newly created share, that newly created share's url will be identical to the prior share's url
--
-- ObjC selector: @- URL@
url :: IsCKShare ckShare => ckShare -> IO (Id NSURL)
url ckShare  =
  sendMsg ckShare (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | All participants on the share that the current user has permissions to see.
--
-- At the minimum that will include the owner and the current user.
--
-- ObjC selector: @- participants@
participants :: IsCKShare ckShare => ckShare -> IO (Id NSArray)
participants ckShare  =
  sendMsg ckShare (mkSelector "participants") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience methods for fetching special users from the participant array
--
-- ObjC selector: @- owner@
owner :: IsCKShare ckShare => ckShare -> IO (Id CKShareParticipant)
owner ckShare  =
  sendMsg ckShare (mkSelector "owner") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentUserParticipant@
currentUserParticipant :: IsCKShare ckShare => ckShare -> IO (Id CKShareParticipant)
currentUserParticipant ckShare  =
  sendMsg ckShare (mkSelector "currentUserParticipant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A list of all uninvited users who have requested access to this share.
--
-- When share access requests are allowed, uninvited users can request to join the share. All pending access requests appear in this array. Each requester is returned with name components and either an email or phone number.
--
-- Either share owners or administrators can respond to these access requests.
--
-- ### Responding to Access Requests:
--
-- - **Approve Requesters:**     - Fetch the participant information by running ``CKFetchShareParticipantsOperation`` with       the requester's ``CKShareAccessRequester/participantLookupInfo``.     - Add the resulting participant to the share.
--
-- - **Deny Requesters:**     - Use ``CloudKit/CKShare/denyRequesters:`` to remove the requester from the requesters list.
--
-- - **Block Requesters:**     - Use ``CloudKit/CKShare/blockRequesters:`` to block requesters.     - Blocking a requester prevents them from sending future access requests to the share.
--
-- ObjC selector: @- requesters@
requesters :: IsCKShare ckShare => ckShare -> IO (Id NSArray)
requesters ckShare  =
  sendMsg ckShare (mkSelector "requesters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A list of users blocked from requesting access to this share.
--
-- Identities remain in this list until an owner or administrator calls ``CloudKit/CKShare/unblockIdentities:``.
--
-- ObjC selector: @- blockedIdentities@
blockedIdentities :: IsCKShare ckShare => ckShare -> IO (Id NSArray)
blockedIdentities ckShare  =
  sendMsg ckShare (mkSelector "blockedIdentities") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates whether uninvited users can request access to this share.
--
-- By default, this property is set to @NO@. When set to @YES@, uninvited users can request access to the share if they discover the share URL. When set to @NO@, the server prevents uninvited users from requesting access and does not indicate whether the share exists.
--
-- Only the share owner or an administrator can modify this property. Attempts by other participants to modify this property result in an exception.
--
-- ObjC selector: @- allowsAccessRequests@
allowsAccessRequests :: IsCKShare ckShare => ckShare -> IO Bool
allowsAccessRequests ckShare  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckShare (mkSelector "allowsAccessRequests") retCULong []

-- | Indicates whether uninvited users can request access to this share.
--
-- By default, this property is set to @NO@. When set to @YES@, uninvited users can request access to the share if they discover the share URL. When set to @NO@, the server prevents uninvited users from requesting access and does not indicate whether the share exists.
--
-- Only the share owner or an administrator can modify this property. Attempts by other participants to modify this property result in an exception.
--
-- ObjC selector: @- setAllowsAccessRequests:@
setAllowsAccessRequests :: IsCKShare ckShare => ckShare -> Bool -> IO ()
setAllowsAccessRequests ckShare  value =
  sendMsg ckShare (mkSelector "setAllowsAccessRequests:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRootRecord:@
initWithRootRecordSelector :: Selector
initWithRootRecordSelector = mkSelector "initWithRootRecord:"

-- | @Selector@ for @initWithRootRecord:shareID:@
initWithRootRecord_shareIDSelector :: Selector
initWithRootRecord_shareIDSelector = mkSelector "initWithRootRecord:shareID:"

-- | @Selector@ for @initWithRecordZoneID:@
initWithRecordZoneIDSelector :: Selector
initWithRecordZoneIDSelector = mkSelector "initWithRecordZoneID:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @addParticipant:@
addParticipantSelector :: Selector
addParticipantSelector = mkSelector "addParticipant:"

-- | @Selector@ for @removeParticipant:@
removeParticipantSelector :: Selector
removeParticipantSelector = mkSelector "removeParticipant:"

-- | @Selector@ for @oneTimeURLForParticipantID:@
oneTimeURLForParticipantIDSelector :: Selector
oneTimeURLForParticipantIDSelector = mkSelector "oneTimeURLForParticipantID:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithRecordType:@
initWithRecordTypeSelector :: Selector
initWithRecordTypeSelector = mkSelector "initWithRecordType:"

-- | @Selector@ for @initWithRecordType:recordID:@
initWithRecordType_recordIDSelector :: Selector
initWithRecordType_recordIDSelector = mkSelector "initWithRecordType:recordID:"

-- | @Selector@ for @initWithRecordType:zoneID:@
initWithRecordType_zoneIDSelector :: Selector
initWithRecordType_zoneIDSelector = mkSelector "initWithRecordType:zoneID:"

-- | @Selector@ for @denyRequesters:@
denyRequestersSelector :: Selector
denyRequestersSelector = mkSelector "denyRequesters:"

-- | @Selector@ for @blockRequesters:@
blockRequestersSelector :: Selector
blockRequestersSelector = mkSelector "blockRequesters:"

-- | @Selector@ for @unblockIdentities:@
unblockIdentitiesSelector :: Selector
unblockIdentitiesSelector = mkSelector "unblockIdentities:"

-- | @Selector@ for @publicPermission@
publicPermissionSelector :: Selector
publicPermissionSelector = mkSelector "publicPermission"

-- | @Selector@ for @setPublicPermission:@
setPublicPermissionSelector :: Selector
setPublicPermissionSelector = mkSelector "setPublicPermission:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @participants@
participantsSelector :: Selector
participantsSelector = mkSelector "participants"

-- | @Selector@ for @owner@
ownerSelector :: Selector
ownerSelector = mkSelector "owner"

-- | @Selector@ for @currentUserParticipant@
currentUserParticipantSelector :: Selector
currentUserParticipantSelector = mkSelector "currentUserParticipant"

-- | @Selector@ for @requesters@
requestersSelector :: Selector
requestersSelector = mkSelector "requesters"

-- | @Selector@ for @blockedIdentities@
blockedIdentitiesSelector :: Selector
blockedIdentitiesSelector = mkSelector "blockedIdentities"

-- | @Selector@ for @allowsAccessRequests@
allowsAccessRequestsSelector :: Selector
allowsAccessRequestsSelector = mkSelector "allowsAccessRequests"

-- | @Selector@ for @setAllowsAccessRequests:@
setAllowsAccessRequestsSelector :: Selector
setAllowsAccessRequestsSelector = mkSelector "setAllowsAccessRequests:"

