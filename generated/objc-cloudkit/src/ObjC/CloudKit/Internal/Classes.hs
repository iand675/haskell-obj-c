{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CloudKit.Internal.Classes (
    module ObjC.CloudKit.Internal.Classes,
    module ObjC.CoreData.Internal.Classes,
    module ObjC.CoreLocation.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.CoreData.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- CKAllowedSharingOptions ----------

-- | Phantom type for @CKAllowedSharingOptions@.
data CKAllowedSharingOptions

instance IsObjCObject (Id CKAllowedSharingOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKAllowedSharingOptions"

class IsNSObject a => IsCKAllowedSharingOptions a where
  toCKAllowedSharingOptions :: a -> Id CKAllowedSharingOptions

instance IsCKAllowedSharingOptions (Id CKAllowedSharingOptions) where
  toCKAllowedSharingOptions = unsafeCastId

instance IsNSObject (Id CKAllowedSharingOptions) where
  toNSObject = unsafeCastId

-- ---------- CKAsset ----------

-- | Phantom type for @CKAsset@.
data CKAsset

instance IsObjCObject (Id CKAsset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKAsset"

class IsNSObject a => IsCKAsset a where
  toCKAsset :: a -> Id CKAsset

instance IsCKAsset (Id CKAsset) where
  toCKAsset = unsafeCastId

instance IsNSObject (Id CKAsset) where
  toNSObject = unsafeCastId

-- ---------- CKContainer ----------

-- | CKContainer
--
-- A CKContainer, and its CKDatabases, are the main entry points into the CloudKit framework.
--
-- Several methods in CloudKit accept completion handlers to indicate when they're completed.  All CKOperation subclasses include progress and completion blocks to report significant events in their lifecycles.  Each of these handlers and blocks is invoked on a non-main serial queue.  The receiver is responsible for handling the message on a different queue or thread if it is required.
-- 
-- Phantom type for @CKContainer@.
data CKContainer

instance IsObjCObject (Id CKContainer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKContainer"

class IsNSObject a => IsCKContainer a where
  toCKContainer :: a -> Id CKContainer

instance IsCKContainer (Id CKContainer) where
  toCKContainer = unsafeCastId

instance IsNSObject (Id CKContainer) where
  toNSObject = unsafeCastId

-- ---------- CKFetchRecordZoneChangesConfiguration ----------

-- | Phantom type for @CKFetchRecordZoneChangesConfiguration@.
data CKFetchRecordZoneChangesConfiguration

instance IsObjCObject (Id CKFetchRecordZoneChangesConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKFetchRecordZoneChangesConfiguration"

class IsNSObject a => IsCKFetchRecordZoneChangesConfiguration a where
  toCKFetchRecordZoneChangesConfiguration :: a -> Id CKFetchRecordZoneChangesConfiguration

instance IsCKFetchRecordZoneChangesConfiguration (Id CKFetchRecordZoneChangesConfiguration) where
  toCKFetchRecordZoneChangesConfiguration = unsafeCastId

instance IsNSObject (Id CKFetchRecordZoneChangesConfiguration) where
  toNSObject = unsafeCastId

-- ---------- CKFetchRecordZoneChangesOptions ----------

-- | Phantom type for @CKFetchRecordZoneChangesOptions@.
data CKFetchRecordZoneChangesOptions

instance IsObjCObject (Id CKFetchRecordZoneChangesOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKFetchRecordZoneChangesOptions"

class IsNSObject a => IsCKFetchRecordZoneChangesOptions a where
  toCKFetchRecordZoneChangesOptions :: a -> Id CKFetchRecordZoneChangesOptions

instance IsCKFetchRecordZoneChangesOptions (Id CKFetchRecordZoneChangesOptions) where
  toCKFetchRecordZoneChangesOptions = unsafeCastId

instance IsNSObject (Id CKFetchRecordZoneChangesOptions) where
  toNSObject = unsafeCastId

-- ---------- CKNotification ----------

-- | Pushes from CloudKit servers contain both CloudKit-specific and APS-specific information. APS-specific information includes elements like alerts, badges, sounds, categories, etc. When receiving a push from CloudKit servers, the push may be delivered via multiple API flows. The flow(s) chosen will depend on the type of push requested (e.g. via the ``CKSubscription`` that triggered it and its configured @notificationInfo@).
--
-- Pushes with UI elements (alerts, badges, sounds): These pushes are delivered via the @UserNotifications@ framework, in the form of a @UNNotification@ Applications should use the @UserNotifications@ framework to interact with the UI elements of this push. Applications may create a ``CKNotification`` from a @UNNotification@ in their @UNUserNotificationCenterDelegate@:
--
-- func userNotificationCenter(_ center: UNUserNotificationCenter, willPresent notification: UNNotification) async -> UNNotificationPresentationOptions {         let ckNotification = CKNotification(fromRemoteNotificationDictionary: notification.request.content.userInfo)     }
--
-- Pushes with @content-available@: These pushes are delivered via an application delegate, in the form of a remote notification. For example: @UIApplicationDelegate.application(_:didReceiveRemoteNotification:) async@ Applications do not need to interact with any UI element in the push payload argument, that's intended to be handled via the @UserNotifications@ flow (a push with both UI elements and @content-available@ will be delivered via both API flows) Applications may create a ``CKNotification`` from the remote notification in their @UIApplicationDelegate@:
--
-- func application(_ application: UIApplication, didReceiveRemoteNotification userInfo: [AnyHashable : Any]) async -> UIBackgroundFetchResult {         let ckNotification = CKNotification(fromRemoteNotificationDictionary: userInfo)     }
-- 
-- Phantom type for @CKNotification@.
data CKNotification

instance IsObjCObject (Id CKNotification) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKNotification"

class IsNSObject a => IsCKNotification a where
  toCKNotification :: a -> Id CKNotification

instance IsCKNotification (Id CKNotification) where
  toCKNotification = unsafeCastId

instance IsNSObject (Id CKNotification) where
  toNSObject = unsafeCastId

-- ---------- CKNotificationID ----------

-- | Phantom type for @CKNotificationID@.
data CKNotificationID

instance IsObjCObject (Id CKNotificationID) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKNotificationID"

class IsNSObject a => IsCKNotificationID a where
  toCKNotificationID :: a -> Id CKNotificationID

instance IsCKNotificationID (Id CKNotificationID) where
  toCKNotificationID = unsafeCastId

instance IsNSObject (Id CKNotificationID) where
  toNSObject = unsafeCastId

-- ---------- CKNotificationInfo ----------

-- | CKNotificationInfo
--
-- The payload of a push notification delivered in the UIApplication @application:didReceiveRemoteNotification:@ delegate method contains information about the firing subscription.
--
-- Use
--
-- +[CKNotification notificationFromRemoteNotificationDictionary:]
--
-- to parse that payload.  On tvOS, alerts, badges, sounds, and categories are not handled in push notifications. However, CKSubscriptions remain available to help you avoid polling the server.
-- 
-- Phantom type for @CKNotificationInfo@.
data CKNotificationInfo

instance IsObjCObject (Id CKNotificationInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKNotificationInfo"

class IsNSObject a => IsCKNotificationInfo a where
  toCKNotificationInfo :: a -> Id CKNotificationInfo

instance IsCKNotificationInfo (Id CKNotificationInfo) where
  toCKNotificationInfo = unsafeCastId

instance IsNSObject (Id CKNotificationInfo) where
  toNSObject = unsafeCastId

-- ---------- CKOperationConfiguration ----------

-- | CKOperationConfiguration
--
-- An operation configuration is a set of properties that describes how your operation should behave.  All properties have a default value.  When determining what properties to apply to an operation, we consult the operation's configuration property, as well as the operation->group->defaultConfiguration property.  We combine them following these rules:
--
-- Group Default Configuration Value | Operation Configuration Value |        Value Applied To Operation
-- -----------------------------------+-------------------------------+-----------------------------------------
-- default value           |         default value         |                  default value
-- default value           |         explicit value        |       operation.configuration explicit value
-- explicit value          |         default value         | operation.group.defaultConfiguration explicit value
-- explicit value          |         explicit value        |       operation.configuration explicit value
--
-- For example:  CKOperationGroup -> defaultConfiguration -> allowsCellularAccess explicitly set to NO  + CKOperation -> configuration -> allowsCellularAccess has default value of YES  = disallow cellular access
--
-- CKOperationGroup -> defaultConfiguration -> allowsCellularAccess explicitly set to NO  + CKOperation -> configuration -> allowsCellularAccess explicitly set to YES  = allow cellular access
-- 
-- Phantom type for @CKOperationConfiguration@.
data CKOperationConfiguration

instance IsObjCObject (Id CKOperationConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKOperationConfiguration"

class IsNSObject a => IsCKOperationConfiguration a where
  toCKOperationConfiguration :: a -> Id CKOperationConfiguration

instance IsCKOperationConfiguration (Id CKOperationConfiguration) where
  toCKOperationConfiguration = unsafeCastId

instance IsNSObject (Id CKOperationConfiguration) where
  toNSObject = unsafeCastId

-- ---------- CKOperationGroup ----------

-- | CKOperationGroup
--
-- A mechanism for your app to group several operations at the granularity of a user action.
--
-- For example, when building a Calendar application, these things might warrant being their own operation groups:  - an initial fetch of data from the server, consisting of many queries, fetchChanges, and fetch operations  - doing an incremental fetch of data in response to a push notification  - saving several records due to a user saving a calendar event
--
-- You associate @CKOperationGroup@ s with@CKOperation@ s by setting the @CKOperation.group@ property.  Create a new @CKOperationGroup@ instance for each distinct user action.
-- 
-- Phantom type for @CKOperationGroup@.
data CKOperationGroup

instance IsObjCObject (Id CKOperationGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKOperationGroup"

class IsNSObject a => IsCKOperationGroup a where
  toCKOperationGroup :: a -> Id CKOperationGroup

instance IsCKOperationGroup (Id CKOperationGroup) where
  toCKOperationGroup = unsafeCastId

instance IsNSObject (Id CKOperationGroup) where
  toNSObject = unsafeCastId

-- ---------- CKQuery ----------

-- | CKQuery
--
-- Only AND compound predicates are allowed.
--
-- Key names must begin with either an upper or lower case character ([a-zA-Z]) and may be followed by characters, numbers, or underscores ([0-9a-zA-Z_]). Keypaths may only resolve to the currently evaluated object, so the '.' character is not allowed in key names.
--
-- A limited subset of classes are allowed as predicate arguments:  - NSString  - NSDate  - NSData  - NSNumber  - NSArray  - CKReference  - CKRecord  - CLLocation
--
-- Any other class as an argument will result in an error when executing the query.
-- 
-- Phantom type for @CKQuery@.
data CKQuery

instance IsObjCObject (Id CKQuery) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKQuery"

class IsNSObject a => IsCKQuery a where
  toCKQuery :: a -> Id CKQuery

instance IsCKQuery (Id CKQuery) where
  toCKQuery = unsafeCastId

instance IsNSObject (Id CKQuery) where
  toNSObject = unsafeCastId

-- ---------- CKQueryCursor ----------

-- | Phantom type for @CKQueryCursor@.
data CKQueryCursor

instance IsObjCObject (Id CKQueryCursor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKQueryCursor"

class IsNSObject a => IsCKQueryCursor a where
  toCKQueryCursor :: a -> Id CKQueryCursor

instance IsCKQueryCursor (Id CKQueryCursor) where
  toCKQueryCursor = unsafeCastId

instance IsNSObject (Id CKQueryCursor) where
  toNSObject = unsafeCastId

-- ---------- CKRecord ----------

-- | Phantom type for @CKRecord@.
data CKRecord

instance IsObjCObject (Id CKRecord) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKRecord"

class IsNSObject a => IsCKRecord a where
  toCKRecord :: a -> Id CKRecord

instance IsCKRecord (Id CKRecord) where
  toCKRecord = unsafeCastId

instance IsNSObject (Id CKRecord) where
  toNSObject = unsafeCastId

-- ---------- CKRecordID ----------

-- | Phantom type for @CKRecordID@.
data CKRecordID

instance IsObjCObject (Id CKRecordID) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKRecordID"

class IsNSObject a => IsCKRecordID a where
  toCKRecordID :: a -> Id CKRecordID

instance IsCKRecordID (Id CKRecordID) where
  toCKRecordID = unsafeCastId

instance IsNSObject (Id CKRecordID) where
  toNSObject = unsafeCastId

-- ---------- CKRecordZone ----------

-- | Phantom type for @CKRecordZone@.
data CKRecordZone

instance IsObjCObject (Id CKRecordZone) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKRecordZone"

class IsNSObject a => IsCKRecordZone a where
  toCKRecordZone :: a -> Id CKRecordZone

instance IsCKRecordZone (Id CKRecordZone) where
  toCKRecordZone = unsafeCastId

instance IsNSObject (Id CKRecordZone) where
  toNSObject = unsafeCastId

-- ---------- CKRecordZoneID ----------

-- | Phantom type for @CKRecordZoneID@.
data CKRecordZoneID

instance IsObjCObject (Id CKRecordZoneID) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKRecordZoneID"

class IsNSObject a => IsCKRecordZoneID a where
  toCKRecordZoneID :: a -> Id CKRecordZoneID

instance IsCKRecordZoneID (Id CKRecordZoneID) where
  toCKRecordZoneID = unsafeCastId

instance IsNSObject (Id CKRecordZoneID) where
  toNSObject = unsafeCastId

-- ---------- CKReference ----------

-- | Phantom type for @CKReference@.
data CKReference

instance IsObjCObject (Id CKReference) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKReference"

class IsNSObject a => IsCKReference a where
  toCKReference :: a -> Id CKReference

instance IsCKReference (Id CKReference) where
  toCKReference = unsafeCastId

instance IsNSObject (Id CKReference) where
  toNSObject = unsafeCastId

-- ---------- CKServerChangeToken ----------

-- | Phantom type for @CKServerChangeToken@.
data CKServerChangeToken

instance IsObjCObject (Id CKServerChangeToken) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKServerChangeToken"

class IsNSObject a => IsCKServerChangeToken a where
  toCKServerChangeToken :: a -> Id CKServerChangeToken

instance IsCKServerChangeToken (Id CKServerChangeToken) where
  toCKServerChangeToken = unsafeCastId

instance IsNSObject (Id CKServerChangeToken) where
  toNSObject = unsafeCastId

-- ---------- CKShareAccessRequester ----------

-- | Phantom type for @CKShareAccessRequester@.
data CKShareAccessRequester

instance IsObjCObject (Id CKShareAccessRequester) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKShareAccessRequester"

class IsNSObject a => IsCKShareAccessRequester a where
  toCKShareAccessRequester :: a -> Id CKShareAccessRequester

instance IsCKShareAccessRequester (Id CKShareAccessRequester) where
  toCKShareAccessRequester = unsafeCastId

instance IsNSObject (Id CKShareAccessRequester) where
  toNSObject = unsafeCastId

-- ---------- CKShareBlockedIdentity ----------

-- | Phantom type for @CKShareBlockedIdentity@.
data CKShareBlockedIdentity

instance IsObjCObject (Id CKShareBlockedIdentity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKShareBlockedIdentity"

class IsNSObject a => IsCKShareBlockedIdentity a where
  toCKShareBlockedIdentity :: a -> Id CKShareBlockedIdentity

instance IsCKShareBlockedIdentity (Id CKShareBlockedIdentity) where
  toCKShareBlockedIdentity = unsafeCastId

instance IsNSObject (Id CKShareBlockedIdentity) where
  toNSObject = unsafeCastId

-- ---------- CKShareMetadata ----------

-- | Phantom type for @CKShareMetadata@.
data CKShareMetadata

instance IsObjCObject (Id CKShareMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKShareMetadata"

class IsNSObject a => IsCKShareMetadata a where
  toCKShareMetadata :: a -> Id CKShareMetadata

instance IsCKShareMetadata (Id CKShareMetadata) where
  toCKShareMetadata = unsafeCastId

instance IsNSObject (Id CKShareMetadata) where
  toNSObject = unsafeCastId

-- ---------- CKShareParticipant ----------

-- | Phantom type for @CKShareParticipant@.
data CKShareParticipant

instance IsObjCObject (Id CKShareParticipant) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKShareParticipant"

class IsNSObject a => IsCKShareParticipant a where
  toCKShareParticipant :: a -> Id CKShareParticipant

instance IsCKShareParticipant (Id CKShareParticipant) where
  toCKShareParticipant = unsafeCastId

instance IsNSObject (Id CKShareParticipant) where
  toNSObject = unsafeCastId

-- ---------- CKSubscription ----------

-- | Phantom type for @CKSubscription@.
data CKSubscription

instance IsObjCObject (Id CKSubscription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSubscription"

class IsNSObject a => IsCKSubscription a where
  toCKSubscription :: a -> Id CKSubscription

instance IsCKSubscription (Id CKSubscription) where
  toCKSubscription = unsafeCastId

instance IsNSObject (Id CKSubscription) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngine ----------

-- | @CKSyncEngine@ encapsulates the logic of syncing data with a CloudKit database.
--
-- Syncing with CloudKit involves many moving pieces. Apps need to schedule syncs, create and batch operations, subscribe to database changes, listen for push notifications, store sync state, handle a multitude of errors, and more. @CKSyncEngine@ is designed to encapsulate this logic in a higher-level API.
--
-- # Start Your Sync Engine
--
-- Generally, you should initialize your @CKSyncEngine@ soon after your process launches. The sync engine will perform work in the background on your behalf, and it needs to be initialized so that it can properly listen for push notifications and handle scheduled sync tasks.
--
-- When initializing your sync engine, you need to provide an object conforming to the @CKSyncEngineDelegate@ protocol. This protocol is the main method of communication between the sync engine and your app. You also need to provide your last known version of the ``CKSyncEngine/State/Serialization``. See ``CKSyncEngine/State`` and ``CKSyncEngine/Event/StateUpdate`` for more details on the sync engine state.
--
-- Note that before using @CKSyncEngine@ in your app, you need to add the CloudKit and remote notification capabilities.
--
-- # Sending Changes to the Server
--
-- In order to send changes to the server, you first need to tell the sync engine you have pending changes to send. You can do this by adding pending changes to the sync engine's ``CKSyncEngine/state`` property.
--
-- When you add pending changes to the state, the sync engine will schedule a task to sync. When the sync task runs, the sync engine will start sending changes to the server. The sync engine will automatically send database changes from ``CKSyncEngine/State/pendingDatabaseChanges``, but you need to provide the record zone changes yourself. In order to send record zone changes, you need to return them from @-[CKSyncEngineDelegate syncEngine:nextRecordZoneChangeBatchForContext:]@.
--
-- When the sync engine finishes sending a batch of changes to the server, your @CKSyncEngineDelegate@ will receive ``CKSyncEngine/Event/sentDatabaseChanges(_:)`` and ``CKSyncEngine/Event/sentRecordZoneChanges(_:)`` events. These events will notify you of the success or failure of the changes you tried to send.
--
-- At a high level, sending changes to the server happens with the following order of operations:
--
-- 1. You add pending changes to ``CKSyncEngine/state``. 2. You receive ``CKSyncEngine/Event/willSendChanges(_:)`` in @-[CKSyncEngineDelegate syncEngine:handleEvent:]@ 3. If there are pending database changes, the sync engine sends the next batch. 4. If any database changes were sent, your delegate receives``CKSyncEngine/Event/sentDatabaseChanges(_:)``. 5. Repeat from step 3 until all pending database changes are sent, then move on to record zone changes in step 6. 6. The sync engine asks for the next batch of record zone changes by calling @-[CKSyncEngineDelegate syncEngine:nextRecordZoneChangeBatchForContext:]@. 7. The sync engine sends the next record zone change batch to the server. 8. If any record zone changes were sent, your delegate receives ``CKSyncEngine/Event/sentRecordZoneChanges(_:)``. 9. If you added any pending database changes during steps 6-8, the sync engine repeats from step 3. Otherwise, it repeats from step 6. 10. When all pending changes are sent, your delegate receives ``CKSyncEngine/Event/didSendChanges(_:)``.
--
-- # Fetching Changes from the Server
--
-- The sync engine will automatically listen for remote notifications, and it will fetch changes from the server when necessary. Generally, you'll receive events in this order:
--
-- 1. Your delegate receives ``CKSyncEngine/Event/willFetchChanges(_:)``. 2. If there are new database changes to fetch, you receive batches of them in ``CKSyncEngine/Event/fetchedDatabaseChanges(_:)`` events. 3. If there are new record zone changes to fetch, you will receive ``CKSyncEngine/Event/willFetchRecordZoneChanges(_:)`` for each zone that has new changes. 4. The sync engine fetches record zone changes and gives you batches of them in ``CKSyncEngine/Event/fetchedRecordZoneChanges(_:)`` events. 5. Your delegate receives ``CKSyncEngine/Event/didFetchRecordZoneChanges(_:)`` for each zone that had changes to fetch. 6. Your delegate receives ``CKSyncEngine/Event/didFetchChanges(_:)``, indicating that sync engine has finished fetching changes.
--
-- # Sync Scheduling
--
-- ## Automatic sync
--
-- By default, the sync engine will automatically schedule sync tasks on your behalf. If the user is signed in, the device has a network connection, and the system is generally in a good state, these scheduled syncs will happen relatively quickly. However, if the device has no network, is low on power, or is otherwise under a heavy load, these automatic syncs might be delayed. Similarly, if the user isn't signed in to an account, the sync engine won't perform any sync tasks at all.
--
-- ## Manual sync
--
-- Generally, you should rely on this automatic sync behavior, but there may be some cases where you want to manually trigger a sync. For example, if you have a pull-to-refresh UI, you can call ``CKSyncEngine/fetchChanges(_:)`` to tell the sync engine to fetch immediately. Or if you want to provide some sort of "backup now" button, you can call ``CKSyncEngine/sendChanges(_:)`` to send to the server immediately.
--
-- ### Testing
--
-- These manual sync functions might also be useful during automated testing. When writing automated tests, you can turn off automatic sync via ``CKSyncEngine/Configuration/automaticallySync``. Then, you'll have complete control over the ordering of sync events. This allows you to interject behavior in the sync flow and simulate specific sequences of events.
--
-- # Error Handling
--
-- There are some transient errors that the sync engine will handle automatically behind the scenes. The sync engine will retry the operations for these transient errors automatically when it makes sense to do so. Specifically, the sync engine will handle the following errors on your behalf:
--
-- * ``CKError/notAuthenticated`` * ``CKError/accountTemporarilyUnavailable`` * ``CKError/networkFailure`` * ``CKError/networkUnavailable`` * ``CKError/requestRateLimited`` * ``CKError/serviceUnavailable`` * ``CKError/zoneBusy``
--
-- When the sync engine encounters one of these errors, it will wait for the system to be in a good state and try again. For example, if the server sends back a @.requestRateLimited@ error, the sync engine will respect this throttle and try again after the retry-after time.
--
-- @CKSyncEngine@ will _not_ handle errors that require application-specific logic. For example, if you try to save a record and get a ``CKError/serverRecordChanged``, you need to handle that error yourself. There are plenty of errors that the sync engine cannot handle on your behalf, see ``CKError`` for a list of all the possible errors.
--
-- # Accounts
--
-- @CKSyncEngine@ monitors for account status, and it will only sync if there's an account signed in. Because of this, you can initialize your @CKSyncEngine@ at any time, regardless of account status. If there is no account, or if the user disabled sync in settings, the sync engine will stay dormant in the background. Once an account is available, the sync engine will start syncing automatically.
--
-- It will also listen for when the user signs in or out of their account. When it notices an account change, it will send an ``CKSyncEngine/Event/accountChange(_:)`` to your delegate. It's your responsibility to react appropriately to this change and update your local persistence.
-- 
-- Phantom type for @CKSyncEngine@.
data CKSyncEngine

instance IsObjCObject (Id CKSyncEngine) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngine"

class IsNSObject a => IsCKSyncEngine a where
  toCKSyncEngine :: a -> Id CKSyncEngine

instance IsCKSyncEngine (Id CKSyncEngine) where
  toCKSyncEngine = unsafeCastId

instance IsNSObject (Id CKSyncEngine) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineConfiguration ----------

-- | Phantom type for @CKSyncEngineConfiguration@.
data CKSyncEngineConfiguration

instance IsObjCObject (Id CKSyncEngineConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineConfiguration"

class IsNSObject a => IsCKSyncEngineConfiguration a where
  toCKSyncEngineConfiguration :: a -> Id CKSyncEngineConfiguration

instance IsCKSyncEngineConfiguration (Id CKSyncEngineConfiguration) where
  toCKSyncEngineConfiguration = unsafeCastId

instance IsNSObject (Id CKSyncEngineConfiguration) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineEvent ----------

-- | An event that occurs during the operation of a @CKSyncEngine@.
--
-- While syncing, @CKSyncEngine@ posts several different types of events. Each event has an associated struct value with details describing the nature of the event.
--
-- At a high level, the sync engine events can be grouped into a few different categories:
--
-- ## Local state changes
--
-- - ``CKSyncEngineStateUpdateEvent`` - ``CKSyncEngineAccountChangeEvent``
--
-- ## Fetched changes
--
-- - ``CKSyncEngineFetchedDatabaseChangesEvent`` - ``CKSyncEngineFetchedRecordZoneChangesEvent``
--
-- ## Sent changes
--
-- - ``CKSyncEngineSentDatabaseChangesEvent`` - ``CKSyncEngineSentRecordZoneChangesEvent``
--
-- ## Fetch changes lifecycle
--
-- - ``CKSyncEngineWillFetchChangesEvent`` - ``CKSyncEngineWillFetchRecordZoneChangesEvent`` - ``CKSyncEngineDidFetchRecordZoneChangesEvent`` - ``CKSyncEngineDidFetchChangesEvent``
--
-- ## Send changes lifecycle
--
-- - ``CKSyncEngineWillSendChangesEvent`` - ``CKSyncEngineDidSendChangesEvent``
--
-- See the documentation for each event struct for more details about when and why an event might be posted.
-- 
-- Phantom type for @CKSyncEngineEvent@.
data CKSyncEngineEvent

instance IsObjCObject (Id CKSyncEngineEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineEvent"

class IsNSObject a => IsCKSyncEngineEvent a where
  toCKSyncEngineEvent :: a -> Id CKSyncEngineEvent

instance IsCKSyncEngineEvent (Id CKSyncEngineEvent) where
  toCKSyncEngineEvent = unsafeCastId

instance IsNSObject (Id CKSyncEngineEvent) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineFailedRecordSave ----------

-- | Phantom type for @CKSyncEngineFailedRecordSave@.
data CKSyncEngineFailedRecordSave

instance IsObjCObject (Id CKSyncEngineFailedRecordSave) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineFailedRecordSave"

class IsNSObject a => IsCKSyncEngineFailedRecordSave a where
  toCKSyncEngineFailedRecordSave :: a -> Id CKSyncEngineFailedRecordSave

instance IsCKSyncEngineFailedRecordSave (Id CKSyncEngineFailedRecordSave) where
  toCKSyncEngineFailedRecordSave = unsafeCastId

instance IsNSObject (Id CKSyncEngineFailedRecordSave) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineFailedZoneSave ----------

-- | Phantom type for @CKSyncEngineFailedZoneSave@.
data CKSyncEngineFailedZoneSave

instance IsObjCObject (Id CKSyncEngineFailedZoneSave) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineFailedZoneSave"

class IsNSObject a => IsCKSyncEngineFailedZoneSave a where
  toCKSyncEngineFailedZoneSave :: a -> Id CKSyncEngineFailedZoneSave

instance IsCKSyncEngineFailedZoneSave (Id CKSyncEngineFailedZoneSave) where
  toCKSyncEngineFailedZoneSave = unsafeCastId

instance IsNSObject (Id CKSyncEngineFailedZoneSave) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineFetchChangesContext ----------

-- | The context of an attempt to fetch changes from the server.
--
-- The sync engine might attempt to fetch changes to the server for many reasons. For example, if you call ``CKSyncEngine/fetchChanges(_:)``, it'll try to fetch changes immediately. Or if it receives a push notification, it'll schedule a sync and fetch changes when the scheduler task runs. This object represents one of those attempts to fetch changes.
-- 
-- Phantom type for @CKSyncEngineFetchChangesContext@.
data CKSyncEngineFetchChangesContext

instance IsObjCObject (Id CKSyncEngineFetchChangesContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineFetchChangesContext"

class IsNSObject a => IsCKSyncEngineFetchChangesContext a where
  toCKSyncEngineFetchChangesContext :: a -> Id CKSyncEngineFetchChangesContext

instance IsCKSyncEngineFetchChangesContext (Id CKSyncEngineFetchChangesContext) where
  toCKSyncEngineFetchChangesContext = unsafeCastId

instance IsNSObject (Id CKSyncEngineFetchChangesContext) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineFetchChangesOptions ----------

-- | A set of options to use when fetching changes from the server.
-- 
-- Phantom type for @CKSyncEngineFetchChangesOptions@.
data CKSyncEngineFetchChangesOptions

instance IsObjCObject (Id CKSyncEngineFetchChangesOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineFetchChangesOptions"

class IsNSObject a => IsCKSyncEngineFetchChangesOptions a where
  toCKSyncEngineFetchChangesOptions :: a -> Id CKSyncEngineFetchChangesOptions

instance IsCKSyncEngineFetchChangesOptions (Id CKSyncEngineFetchChangesOptions) where
  toCKSyncEngineFetchChangesOptions = unsafeCastId

instance IsNSObject (Id CKSyncEngineFetchChangesOptions) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineFetchChangesScope ----------

-- | A scope in which the sync engine will fetch changes from the server.
-- 
-- Phantom type for @CKSyncEngineFetchChangesScope@.
data CKSyncEngineFetchChangesScope

instance IsObjCObject (Id CKSyncEngineFetchChangesScope) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineFetchChangesScope"

class IsNSObject a => IsCKSyncEngineFetchChangesScope a where
  toCKSyncEngineFetchChangesScope :: a -> Id CKSyncEngineFetchChangesScope

instance IsCKSyncEngineFetchChangesScope (Id CKSyncEngineFetchChangesScope) where
  toCKSyncEngineFetchChangesScope = unsafeCastId

instance IsNSObject (Id CKSyncEngineFetchChangesScope) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineFetchedRecordDeletion ----------

-- | Phantom type for @CKSyncEngineFetchedRecordDeletion@.
data CKSyncEngineFetchedRecordDeletion

instance IsObjCObject (Id CKSyncEngineFetchedRecordDeletion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineFetchedRecordDeletion"

class IsNSObject a => IsCKSyncEngineFetchedRecordDeletion a where
  toCKSyncEngineFetchedRecordDeletion :: a -> Id CKSyncEngineFetchedRecordDeletion

instance IsCKSyncEngineFetchedRecordDeletion (Id CKSyncEngineFetchedRecordDeletion) where
  toCKSyncEngineFetchedRecordDeletion = unsafeCastId

instance IsNSObject (Id CKSyncEngineFetchedRecordDeletion) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineFetchedZoneDeletion ----------

-- | Phantom type for @CKSyncEngineFetchedZoneDeletion@.
data CKSyncEngineFetchedZoneDeletion

instance IsObjCObject (Id CKSyncEngineFetchedZoneDeletion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineFetchedZoneDeletion"

class IsNSObject a => IsCKSyncEngineFetchedZoneDeletion a where
  toCKSyncEngineFetchedZoneDeletion :: a -> Id CKSyncEngineFetchedZoneDeletion

instance IsCKSyncEngineFetchedZoneDeletion (Id CKSyncEngineFetchedZoneDeletion) where
  toCKSyncEngineFetchedZoneDeletion = unsafeCastId

instance IsNSObject (Id CKSyncEngineFetchedZoneDeletion) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEnginePendingDatabaseChange ----------

-- | A change in a database that needs to be sent to the server.
-- 
-- Phantom type for @CKSyncEnginePendingDatabaseChange@.
data CKSyncEnginePendingDatabaseChange

instance IsObjCObject (Id CKSyncEnginePendingDatabaseChange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEnginePendingDatabaseChange"

class IsNSObject a => IsCKSyncEnginePendingDatabaseChange a where
  toCKSyncEnginePendingDatabaseChange :: a -> Id CKSyncEnginePendingDatabaseChange

instance IsCKSyncEnginePendingDatabaseChange (Id CKSyncEnginePendingDatabaseChange) where
  toCKSyncEnginePendingDatabaseChange = unsafeCastId

instance IsNSObject (Id CKSyncEnginePendingDatabaseChange) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEnginePendingRecordZoneChange ----------

-- | A change in a record zone that needs to be sent to the server.
-- 
-- Phantom type for @CKSyncEnginePendingRecordZoneChange@.
data CKSyncEnginePendingRecordZoneChange

instance IsObjCObject (Id CKSyncEnginePendingRecordZoneChange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEnginePendingRecordZoneChange"

class IsNSObject a => IsCKSyncEnginePendingRecordZoneChange a where
  toCKSyncEnginePendingRecordZoneChange :: a -> Id CKSyncEnginePendingRecordZoneChange

instance IsCKSyncEnginePendingRecordZoneChange (Id CKSyncEnginePendingRecordZoneChange) where
  toCKSyncEnginePendingRecordZoneChange = unsafeCastId

instance IsNSObject (Id CKSyncEnginePendingRecordZoneChange) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineRecordZoneChangeBatch ----------

-- | A batch of record zone changes that @CKSyncEngine@ will send to the server in a single request.
-- 
-- Phantom type for @CKSyncEngineRecordZoneChangeBatch@.
data CKSyncEngineRecordZoneChangeBatch

instance IsObjCObject (Id CKSyncEngineRecordZoneChangeBatch) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineRecordZoneChangeBatch"

class IsNSObject a => IsCKSyncEngineRecordZoneChangeBatch a where
  toCKSyncEngineRecordZoneChangeBatch :: a -> Id CKSyncEngineRecordZoneChangeBatch

instance IsCKSyncEngineRecordZoneChangeBatch (Id CKSyncEngineRecordZoneChangeBatch) where
  toCKSyncEngineRecordZoneChangeBatch = unsafeCastId

instance IsNSObject (Id CKSyncEngineRecordZoneChangeBatch) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineSendChangesContext ----------

-- | The context of an attempt to send changes to the server.
--
-- The sync engine might attempt to send changes to the server for many reasons. For example, if you call ``CKSyncEngine/sendChanges(_:)``, it'll try to send changes immediately. Or if you add pending changes to the state, it'll schedule a sync and send changes when the scheduler task runs. This object represents one of those attempts to send changes.
-- 
-- Phantom type for @CKSyncEngineSendChangesContext@.
data CKSyncEngineSendChangesContext

instance IsObjCObject (Id CKSyncEngineSendChangesContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineSendChangesContext"

class IsNSObject a => IsCKSyncEngineSendChangesContext a where
  toCKSyncEngineSendChangesContext :: a -> Id CKSyncEngineSendChangesContext

instance IsCKSyncEngineSendChangesContext (Id CKSyncEngineSendChangesContext) where
  toCKSyncEngineSendChangesContext = unsafeCastId

instance IsNSObject (Id CKSyncEngineSendChangesContext) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineSendChangesOptions ----------

-- | A set of options to use when sending changes to the server.
-- 
-- Phantom type for @CKSyncEngineSendChangesOptions@.
data CKSyncEngineSendChangesOptions

instance IsObjCObject (Id CKSyncEngineSendChangesOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineSendChangesOptions"

class IsNSObject a => IsCKSyncEngineSendChangesOptions a where
  toCKSyncEngineSendChangesOptions :: a -> Id CKSyncEngineSendChangesOptions

instance IsCKSyncEngineSendChangesOptions (Id CKSyncEngineSendChangesOptions) where
  toCKSyncEngineSendChangesOptions = unsafeCastId

instance IsNSObject (Id CKSyncEngineSendChangesOptions) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineSendChangesScope ----------

-- | A scope in which the sync engine will send changes to  the server.
-- 
-- Phantom type for @CKSyncEngineSendChangesScope@.
data CKSyncEngineSendChangesScope

instance IsObjCObject (Id CKSyncEngineSendChangesScope) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineSendChangesScope"

class IsNSObject a => IsCKSyncEngineSendChangesScope a where
  toCKSyncEngineSendChangesScope :: a -> Id CKSyncEngineSendChangesScope

instance IsCKSyncEngineSendChangesScope (Id CKSyncEngineSendChangesScope) where
  toCKSyncEngineSendChangesScope = unsafeCastId

instance IsNSObject (Id CKSyncEngineSendChangesScope) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineState ----------

-- | An object that tracks some state required for proper and efficient operation of ``CKSyncEngine-5sie5``.
--
-- ``CKSyncEngine-5sie5`` needs to track several things in order to properly sync. For example, it needs to remember the last server change tokens for your database and zones. It also needs to keep track of things like the last known user record ID and other various pieces of state.
--
-- A lot of this state is hidden internally, but some of it you can control.
--
-- ## Pending changes
--
-- One of the main things you can control is the list of pending changes to send to the server. You can control these by calling functions like ``addPendingDatabaseChanges:`` and ``addPendingRecordZoneChanges:``. When you add new pending changes, the sync engine will automatically schedule a task to sync with the server.
--
-- ## State serialization
--
-- ``CKSyncEngine-5sie5`` will occasionally update its state in the background. When it updates its state, your delegate will receive a ``CKSyncEngineStateUpdateEvent``.
--
-- This event will contain a ``CKSyncEngineStateSerialization``, which you should persist locally. The next time your process launches, you initialize your sync engine with the last state serialization you received.
-- 
-- Phantom type for @CKSyncEngineState@.
data CKSyncEngineState

instance IsObjCObject (Id CKSyncEngineState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineState"

class IsNSObject a => IsCKSyncEngineState a where
  toCKSyncEngineState :: a -> Id CKSyncEngineState

instance IsCKSyncEngineState (Id CKSyncEngineState) where
  toCKSyncEngineState = unsafeCastId

instance IsNSObject (Id CKSyncEngineState) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineStateSerialization ----------

-- | A serialized representation of a ``CKSyncEngineState``.
--
-- This will be passed to your delegate via ``CKSyncEngine/Event/StateUpdate``. You should use @NSSecureCoding@ to persist this locally alongside your other data and use it the next time you initialize your sync engine.
-- 
-- Phantom type for @CKSyncEngineStateSerialization@.
data CKSyncEngineStateSerialization

instance IsObjCObject (Id CKSyncEngineStateSerialization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineStateSerialization"

class IsNSObject a => IsCKSyncEngineStateSerialization a where
  toCKSyncEngineStateSerialization :: a -> Id CKSyncEngineStateSerialization

instance IsCKSyncEngineStateSerialization (Id CKSyncEngineStateSerialization) where
  toCKSyncEngineStateSerialization = unsafeCastId

instance IsNSObject (Id CKSyncEngineStateSerialization) where
  toNSObject = unsafeCastId

-- ---------- CKSystemSharingUIObserver ----------

-- | Phantom type for @CKSystemSharingUIObserver@.
data CKSystemSharingUIObserver

instance IsObjCObject (Id CKSystemSharingUIObserver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSystemSharingUIObserver"

class IsNSObject a => IsCKSystemSharingUIObserver a where
  toCKSystemSharingUIObserver :: a -> Id CKSystemSharingUIObserver

instance IsCKSystemSharingUIObserver (Id CKSystemSharingUIObserver) where
  toCKSystemSharingUIObserver = unsafeCastId

instance IsNSObject (Id CKSystemSharingUIObserver) where
  toNSObject = unsafeCastId

-- ---------- CKUserIdentity ----------

-- | Phantom type for @CKUserIdentity@.
data CKUserIdentity

instance IsObjCObject (Id CKUserIdentity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKUserIdentity"

class IsNSObject a => IsCKUserIdentity a where
  toCKUserIdentity :: a -> Id CKUserIdentity

instance IsCKUserIdentity (Id CKUserIdentity) where
  toCKUserIdentity = unsafeCastId

instance IsNSObject (Id CKUserIdentity) where
  toNSObject = unsafeCastId

-- ---------- CKUserIdentityLookupInfo ----------

-- | Phantom type for @CKUserIdentityLookupInfo@.
data CKUserIdentityLookupInfo

instance IsObjCObject (Id CKUserIdentityLookupInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKUserIdentityLookupInfo"

class IsNSObject a => IsCKUserIdentityLookupInfo a where
  toCKUserIdentityLookupInfo :: a -> Id CKUserIdentityLookupInfo

instance IsCKUserIdentityLookupInfo (Id CKUserIdentityLookupInfo) where
  toCKUserIdentityLookupInfo = unsafeCastId

instance IsNSObject (Id CKUserIdentityLookupInfo) where
  toNSObject = unsafeCastId

-- ---------- CKDatabaseNotification ----------

-- | A notification generated by a ``CKDatabaseSubscription``
--
-- @notificationType@ == @.database@ When properties must be dropped (see @isPruned),@ here's the order of importance.  The most important properties are first, they'll be the last ones to be dropped. - notificationID - badge - alertLocalizationKey - alertLocalizationArgs - alertBody - alertActionLocalizationKey - alertLaunchImage - soundName - content-available - containerIdentifier - subscriptionOwnerUserRecordID - titleLocalizationKey - titleLocalizationArgs - title - subtitleLocalizationKey - subtitleLocalizationArgs - subtitle
-- 
-- Phantom type for @CKDatabaseNotification@.
data CKDatabaseNotification

instance IsObjCObject (Id CKDatabaseNotification) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKDatabaseNotification"

class IsCKNotification a => IsCKDatabaseNotification a where
  toCKDatabaseNotification :: a -> Id CKDatabaseNotification

instance IsCKDatabaseNotification (Id CKDatabaseNotification) where
  toCKDatabaseNotification = unsafeCastId

instance IsCKNotification (Id CKDatabaseNotification) where
  toCKNotification = unsafeCastId

instance IsNSObject (Id CKDatabaseNotification) where
  toNSObject = unsafeCastId

-- ---------- CKQueryNotification ----------

-- | A notification generated by a ``CKQuerySubscription``
--
-- @notificationType@ == @.query@ When properties must be dropped (see @isPruned),@ here's the order of importance.  The most important properties are first, they'll be the last ones to be dropped. - notificationID - badge - alertLocalizationKey - alertLocalizationArgs - alertBody - alertActionLocalizationKey - alertLaunchImage - soundName - content-available - desiredKeys - queryNotificationReason - recordID - containerIdentifier - subscriptionOwnerUserRecordID - titleLocalizationKey - titleLocalizationArgs - title - subtitleLocalizationKey - subtitleLocalizationArgs - subtitle
-- 
-- Phantom type for @CKQueryNotification@.
data CKQueryNotification

instance IsObjCObject (Id CKQueryNotification) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKQueryNotification"

class IsCKNotification a => IsCKQueryNotification a where
  toCKQueryNotification :: a -> Id CKQueryNotification

instance IsCKQueryNotification (Id CKQueryNotification) where
  toCKQueryNotification = unsafeCastId

instance IsCKNotification (Id CKQueryNotification) where
  toCKNotification = unsafeCastId

instance IsNSObject (Id CKQueryNotification) where
  toNSObject = unsafeCastId

-- ---------- CKRecordZoneNotification ----------

-- | A notification generated by a ``CKRecordZoneSubscription``
--
-- @notificationType@ == @.recordZone@ When properties must be dropped (see @isPruned),@ here's the order of importance.  The most important properties are first, they'll be the last ones to be dropped. - notificationID - badge - alertLocalizationKey - alertLocalizationArgs - alertBody - alertActionLocalizationKey - alertLaunchImage - soundName - content-available - recordZoneID - containerIdentifier - subscriptionOwnerUserRecordID - titleLocalizationKey - titleLocalizationArgs - title - subtitleLocalizationKey - subtitleLocalizationArgs - subtitle
-- 
-- Phantom type for @CKRecordZoneNotification@.
data CKRecordZoneNotification

instance IsObjCObject (Id CKRecordZoneNotification) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKRecordZoneNotification"

class IsCKNotification a => IsCKRecordZoneNotification a where
  toCKRecordZoneNotification :: a -> Id CKRecordZoneNotification

instance IsCKRecordZoneNotification (Id CKRecordZoneNotification) where
  toCKRecordZoneNotification = unsafeCastId

instance IsCKNotification (Id CKRecordZoneNotification) where
  toCKNotification = unsafeCastId

instance IsNSObject (Id CKRecordZoneNotification) where
  toNSObject = unsafeCastId

-- ---------- CKShare ----------

-- | CKShare
--
-- Like CKRecords, CKShares can store arbitrary key-value pairs.  They are modified and fetched in the same manner.  A share, its root record, and its root record's children records will only appear in a participant's CKFetchRecordChangesOperation's results after the share has been accepted by that participant.  Clients have access to the share (and optionally the root record) before accepting a share, via the CKShareMetadata object.  Note that in order to access a root record before accepting a share, you must run a CKFetchShareMetadataOperation requesting the root record.  A CKShare will appear in a CKFetchRecordChangesOperation's results set whenever the participant list is updated.  For that reason, you shouldn't place heavy key-value pairs in it.
-- 
-- Phantom type for @CKShare@.
data CKShare

instance IsObjCObject (Id CKShare) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKShare"

class IsCKRecord a => IsCKShare a where
  toCKShare :: a -> Id CKShare

instance IsCKShare (Id CKShare) where
  toCKShare = unsafeCastId

instance IsCKRecord (Id CKShare) where
  toCKRecord = unsafeCastId

instance IsNSObject (Id CKShare) where
  toNSObject = unsafeCastId

-- ---------- CKDatabaseSubscription ----------

-- | CKDatabaseSubscription
--
-- A subscription fires whenever any change happens in the database that this subscription was saved in.
--
-- @CKDatabaseSubscription@ is only supported in the Private and Shared databases.
-- 
-- Phantom type for @CKDatabaseSubscription@.
data CKDatabaseSubscription

instance IsObjCObject (Id CKDatabaseSubscription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKDatabaseSubscription"

class IsCKSubscription a => IsCKDatabaseSubscription a where
  toCKDatabaseSubscription :: a -> Id CKDatabaseSubscription

instance IsCKDatabaseSubscription (Id CKDatabaseSubscription) where
  toCKDatabaseSubscription = unsafeCastId

instance IsCKSubscription (Id CKDatabaseSubscription) where
  toCKSubscription = unsafeCastId

instance IsNSObject (Id CKDatabaseSubscription) where
  toNSObject = unsafeCastId

-- ---------- CKQuerySubscription ----------

-- | CKQuerySubscription
--
-- A subscription that fires whenever a change matching the predicate occurs.
--
-- @CKQuerySubscriptions@ are not supported in a @sharedCloudDatabase@
-- 
-- Phantom type for @CKQuerySubscription@.
data CKQuerySubscription

instance IsObjCObject (Id CKQuerySubscription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKQuerySubscription"

class IsCKSubscription a => IsCKQuerySubscription a where
  toCKQuerySubscription :: a -> Id CKQuerySubscription

instance IsCKQuerySubscription (Id CKQuerySubscription) where
  toCKQuerySubscription = unsafeCastId

instance IsCKSubscription (Id CKQuerySubscription) where
  toCKSubscription = unsafeCastId

instance IsNSObject (Id CKQuerySubscription) where
  toNSObject = unsafeCastId

-- ---------- CKRecordZoneSubscription ----------

-- | CKRecordZoneSubscription
--
-- A subscription that fires whenever any change happens in the indicated Record Zone.
--
-- The RecordZone must have the capability @CKRecordZoneCapabilityFetchChanges@  @CKRecordZoneSubscriptions@ are not supported in a @sharedCloudDatabase@
-- 
-- Phantom type for @CKRecordZoneSubscription@.
data CKRecordZoneSubscription

instance IsObjCObject (Id CKRecordZoneSubscription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKRecordZoneSubscription"

class IsCKSubscription a => IsCKRecordZoneSubscription a where
  toCKRecordZoneSubscription :: a -> Id CKRecordZoneSubscription

instance IsCKRecordZoneSubscription (Id CKRecordZoneSubscription) where
  toCKRecordZoneSubscription = unsafeCastId

instance IsCKSubscription (Id CKRecordZoneSubscription) where
  toCKSubscription = unsafeCastId

instance IsNSObject (Id CKRecordZoneSubscription) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineAccountChangeEvent ----------

-- | The user signed in or out of their account.
--
-- The sync engine automatically listens for account changes, and it will send this event when the user signs in or out. It's your responsibility to react appropriately to this change and update your local persistence.
--
-- When the logged-in account changes, the sync engine will reset its internal state under the hood. This means that it will clear any pending database or record zone changes that you may have added.
--
-- Note that it's possible the account changes multiple times while your app is quit. If this happens, you will only receive one account change event representing the transition between the last known state and the current state.
-- 
-- Phantom type for @CKSyncEngineAccountChangeEvent@.
data CKSyncEngineAccountChangeEvent

instance IsObjCObject (Id CKSyncEngineAccountChangeEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineAccountChangeEvent"

class IsCKSyncEngineEvent a => IsCKSyncEngineAccountChangeEvent a where
  toCKSyncEngineAccountChangeEvent :: a -> Id CKSyncEngineAccountChangeEvent

instance IsCKSyncEngineAccountChangeEvent (Id CKSyncEngineAccountChangeEvent) where
  toCKSyncEngineAccountChangeEvent = unsafeCastId

instance IsCKSyncEngineEvent (Id CKSyncEngineAccountChangeEvent) where
  toCKSyncEngineEvent = unsafeCastId

instance IsNSObject (Id CKSyncEngineAccountChangeEvent) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineDidFetchChangesEvent ----------

-- | The sync engine finished fetching changes from the server.
--
-- This might be a good signal to perform any post-processing tasks required after persisting fetched changes to disk.
--
-- You should receive one @CKSyncEngineDidFetchChangesEvent@ for each @CKSyncEngineWillFetchChangesEvent@.
-- 
-- Phantom type for @CKSyncEngineDidFetchChangesEvent@.
data CKSyncEngineDidFetchChangesEvent

instance IsObjCObject (Id CKSyncEngineDidFetchChangesEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineDidFetchChangesEvent"

class IsCKSyncEngineEvent a => IsCKSyncEngineDidFetchChangesEvent a where
  toCKSyncEngineDidFetchChangesEvent :: a -> Id CKSyncEngineDidFetchChangesEvent

instance IsCKSyncEngineDidFetchChangesEvent (Id CKSyncEngineDidFetchChangesEvent) where
  toCKSyncEngineDidFetchChangesEvent = unsafeCastId

instance IsCKSyncEngineEvent (Id CKSyncEngineDidFetchChangesEvent) where
  toCKSyncEngineEvent = unsafeCastId

instance IsNSObject (Id CKSyncEngineDidFetchChangesEvent) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineDidFetchRecordZoneChangesEvent ----------

-- | The sync engine finished fetching record zone changes from the server for a specific zone.
--
-- This might be a good signal to perform any post-processing tasks on a per-zone basis if necessary.
--
-- You should receive one @CKSyncEngineDidFetchRecordZoneChangesEvent@ for each @CKSyncEngineWillFetchRecordZoneChangesEvent@.
-- 
-- Phantom type for @CKSyncEngineDidFetchRecordZoneChangesEvent@.
data CKSyncEngineDidFetchRecordZoneChangesEvent

instance IsObjCObject (Id CKSyncEngineDidFetchRecordZoneChangesEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineDidFetchRecordZoneChangesEvent"

class IsCKSyncEngineEvent a => IsCKSyncEngineDidFetchRecordZoneChangesEvent a where
  toCKSyncEngineDidFetchRecordZoneChangesEvent :: a -> Id CKSyncEngineDidFetchRecordZoneChangesEvent

instance IsCKSyncEngineDidFetchRecordZoneChangesEvent (Id CKSyncEngineDidFetchRecordZoneChangesEvent) where
  toCKSyncEngineDidFetchRecordZoneChangesEvent = unsafeCastId

instance IsCKSyncEngineEvent (Id CKSyncEngineDidFetchRecordZoneChangesEvent) where
  toCKSyncEngineEvent = unsafeCastId

instance IsNSObject (Id CKSyncEngineDidFetchRecordZoneChangesEvent) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineDidSendChangesEvent ----------

-- | The sync engine finished sending changes to the server.
--
-- You should receive one @CKSyncEngineDidSendChangesEvent@ for every @CKSyncEngineWillSendChangesEvent@.
-- 
-- Phantom type for @CKSyncEngineDidSendChangesEvent@.
data CKSyncEngineDidSendChangesEvent

instance IsObjCObject (Id CKSyncEngineDidSendChangesEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineDidSendChangesEvent"

class IsCKSyncEngineEvent a => IsCKSyncEngineDidSendChangesEvent a where
  toCKSyncEngineDidSendChangesEvent :: a -> Id CKSyncEngineDidSendChangesEvent

instance IsCKSyncEngineDidSendChangesEvent (Id CKSyncEngineDidSendChangesEvent) where
  toCKSyncEngineDidSendChangesEvent = unsafeCastId

instance IsCKSyncEngineEvent (Id CKSyncEngineDidSendChangesEvent) where
  toCKSyncEngineEvent = unsafeCastId

instance IsNSObject (Id CKSyncEngineDidSendChangesEvent) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineFetchedDatabaseChangesEvent ----------

-- | A batch of database changes was fetched from the server.
--
-- If there are a lot of new changes on the server, then you might receive many of these events in a row.
--
-- The ordering of fetched changes is not guaranteed, but changes will typically be fetched from oldest to newest.
-- 
-- Phantom type for @CKSyncEngineFetchedDatabaseChangesEvent@.
data CKSyncEngineFetchedDatabaseChangesEvent

instance IsObjCObject (Id CKSyncEngineFetchedDatabaseChangesEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineFetchedDatabaseChangesEvent"

class IsCKSyncEngineEvent a => IsCKSyncEngineFetchedDatabaseChangesEvent a where
  toCKSyncEngineFetchedDatabaseChangesEvent :: a -> Id CKSyncEngineFetchedDatabaseChangesEvent

instance IsCKSyncEngineFetchedDatabaseChangesEvent (Id CKSyncEngineFetchedDatabaseChangesEvent) where
  toCKSyncEngineFetchedDatabaseChangesEvent = unsafeCastId

instance IsCKSyncEngineEvent (Id CKSyncEngineFetchedDatabaseChangesEvent) where
  toCKSyncEngineEvent = unsafeCastId

instance IsNSObject (Id CKSyncEngineFetchedDatabaseChangesEvent) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineFetchedRecordZoneChangesEvent ----------

-- | A batch of record zone changes was fetched from the server.
--
-- If there are a lot of new changes on the server, then you might receive many of these events in a row.
--
-- The ordering of fetched changes is not guaranteed, but changes will typically be fetched from oldest to newest.
-- 
-- Phantom type for @CKSyncEngineFetchedRecordZoneChangesEvent@.
data CKSyncEngineFetchedRecordZoneChangesEvent

instance IsObjCObject (Id CKSyncEngineFetchedRecordZoneChangesEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineFetchedRecordZoneChangesEvent"

class IsCKSyncEngineEvent a => IsCKSyncEngineFetchedRecordZoneChangesEvent a where
  toCKSyncEngineFetchedRecordZoneChangesEvent :: a -> Id CKSyncEngineFetchedRecordZoneChangesEvent

instance IsCKSyncEngineFetchedRecordZoneChangesEvent (Id CKSyncEngineFetchedRecordZoneChangesEvent) where
  toCKSyncEngineFetchedRecordZoneChangesEvent = unsafeCastId

instance IsCKSyncEngineEvent (Id CKSyncEngineFetchedRecordZoneChangesEvent) where
  toCKSyncEngineEvent = unsafeCastId

instance IsNSObject (Id CKSyncEngineFetchedRecordZoneChangesEvent) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineSentDatabaseChangesEvent ----------

-- | The sync engine finished sending a batch of database changes to the server.
--
-- If a change failed, try to resolve the issue causing the error and make the change again if necessary.
-- 
-- Phantom type for @CKSyncEngineSentDatabaseChangesEvent@.
data CKSyncEngineSentDatabaseChangesEvent

instance IsObjCObject (Id CKSyncEngineSentDatabaseChangesEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineSentDatabaseChangesEvent"

class IsCKSyncEngineEvent a => IsCKSyncEngineSentDatabaseChangesEvent a where
  toCKSyncEngineSentDatabaseChangesEvent :: a -> Id CKSyncEngineSentDatabaseChangesEvent

instance IsCKSyncEngineSentDatabaseChangesEvent (Id CKSyncEngineSentDatabaseChangesEvent) where
  toCKSyncEngineSentDatabaseChangesEvent = unsafeCastId

instance IsCKSyncEngineEvent (Id CKSyncEngineSentDatabaseChangesEvent) where
  toCKSyncEngineEvent = unsafeCastId

instance IsNSObject (Id CKSyncEngineSentDatabaseChangesEvent) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineSentRecordZoneChangesEvent ----------

-- | The sync engine finished sending a batch of record zone changes to the server.
--
-- If a record save succeeded, you should encode the system fields of this record to use the next time you save. See @encodeSystemFields@ on ``CKRecord``.
--
-- If a record deletion succeeded, you should remove any local system fields for that record.
--
-- If the record change failed, try to resolve the issue causing the error and save the record again if necessary.
-- 
-- Phantom type for @CKSyncEngineSentRecordZoneChangesEvent@.
data CKSyncEngineSentRecordZoneChangesEvent

instance IsObjCObject (Id CKSyncEngineSentRecordZoneChangesEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineSentRecordZoneChangesEvent"

class IsCKSyncEngineEvent a => IsCKSyncEngineSentRecordZoneChangesEvent a where
  toCKSyncEngineSentRecordZoneChangesEvent :: a -> Id CKSyncEngineSentRecordZoneChangesEvent

instance IsCKSyncEngineSentRecordZoneChangesEvent (Id CKSyncEngineSentRecordZoneChangesEvent) where
  toCKSyncEngineSentRecordZoneChangesEvent = unsafeCastId

instance IsCKSyncEngineEvent (Id CKSyncEngineSentRecordZoneChangesEvent) where
  toCKSyncEngineEvent = unsafeCastId

instance IsNSObject (Id CKSyncEngineSentRecordZoneChangesEvent) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineStateUpdateEvent ----------

-- | The sync engine state was updated, and you should persist it locally.
--
-- In order to function properly and efficiently, @CKSyncEngine@ tracks some state internally. When the sync engine state changes, it will give you the latest serialized version in a ``CKSyncEngine/Event/StateUpdate``. This event will happen occasionally when the sync engine modifies the state internally during normal sync operation. This event will also happen when you change the state yourself.
--
-- The sync engine does not persist this state to disk, so you need to persist it in alongside your own local data. The next time your process launches, use this latest state serialization in ``CKSyncEngineConfiguration/stateSerialization`` to initialize your sync engine.
--
-- This state is directly tied to the changes you fetch and send with the sync engine. You should ensure that any changes fetched prior to receiving this state are also persisted alongside this state.
-- 
-- Phantom type for @CKSyncEngineStateUpdateEvent@.
data CKSyncEngineStateUpdateEvent

instance IsObjCObject (Id CKSyncEngineStateUpdateEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineStateUpdateEvent"

class IsCKSyncEngineEvent a => IsCKSyncEngineStateUpdateEvent a where
  toCKSyncEngineStateUpdateEvent :: a -> Id CKSyncEngineStateUpdateEvent

instance IsCKSyncEngineStateUpdateEvent (Id CKSyncEngineStateUpdateEvent) where
  toCKSyncEngineStateUpdateEvent = unsafeCastId

instance IsCKSyncEngineEvent (Id CKSyncEngineStateUpdateEvent) where
  toCKSyncEngineEvent = unsafeCastId

instance IsNSObject (Id CKSyncEngineStateUpdateEvent) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineWillFetchChangesEvent ----------

-- | The sync engine is about to fetch changes from the server.
--
-- This might be a good signal to prepare your local data store for incoming changes if necessary. The changes themselves will be delivered via @CKSyncEngineFetchedDatabaseChanges@ and @CKSyncEngineFetchedRecordZoneChangesEvent@.
--
-- Note that this event might not always occur every time you call @fetchChanges@. For example, if you call @fetchChanges@ concurrently while the engine is already fetching changes, this event might not be sent. Similarly, if there's no logged-in account, the engine might short-circuit the call to @fetchChanges@, and this event won't be sent.
-- 
-- Phantom type for @CKSyncEngineWillFetchChangesEvent@.
data CKSyncEngineWillFetchChangesEvent

instance IsObjCObject (Id CKSyncEngineWillFetchChangesEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineWillFetchChangesEvent"

class IsCKSyncEngineEvent a => IsCKSyncEngineWillFetchChangesEvent a where
  toCKSyncEngineWillFetchChangesEvent :: a -> Id CKSyncEngineWillFetchChangesEvent

instance IsCKSyncEngineWillFetchChangesEvent (Id CKSyncEngineWillFetchChangesEvent) where
  toCKSyncEngineWillFetchChangesEvent = unsafeCastId

instance IsCKSyncEngineEvent (Id CKSyncEngineWillFetchChangesEvent) where
  toCKSyncEngineEvent = unsafeCastId

instance IsNSObject (Id CKSyncEngineWillFetchChangesEvent) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineWillFetchRecordZoneChangesEvent ----------

-- | The sync engine is about to fetch record zone changes from the server for a specific zone.
--
-- This might be a good signal to prepare your local data store for incoming changes if necessary.
-- 
-- Phantom type for @CKSyncEngineWillFetchRecordZoneChangesEvent@.
data CKSyncEngineWillFetchRecordZoneChangesEvent

instance IsObjCObject (Id CKSyncEngineWillFetchRecordZoneChangesEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineWillFetchRecordZoneChangesEvent"

class IsCKSyncEngineEvent a => IsCKSyncEngineWillFetchRecordZoneChangesEvent a where
  toCKSyncEngineWillFetchRecordZoneChangesEvent :: a -> Id CKSyncEngineWillFetchRecordZoneChangesEvent

instance IsCKSyncEngineWillFetchRecordZoneChangesEvent (Id CKSyncEngineWillFetchRecordZoneChangesEvent) where
  toCKSyncEngineWillFetchRecordZoneChangesEvent = unsafeCastId

instance IsCKSyncEngineEvent (Id CKSyncEngineWillFetchRecordZoneChangesEvent) where
  toCKSyncEngineEvent = unsafeCastId

instance IsNSObject (Id CKSyncEngineWillFetchRecordZoneChangesEvent) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEngineWillSendChangesEvent ----------

-- | The sync engine is about to send changes to the server.
-- 
-- Phantom type for @CKSyncEngineWillSendChangesEvent@.
data CKSyncEngineWillSendChangesEvent

instance IsObjCObject (Id CKSyncEngineWillSendChangesEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEngineWillSendChangesEvent"

class IsCKSyncEngineEvent a => IsCKSyncEngineWillSendChangesEvent a where
  toCKSyncEngineWillSendChangesEvent :: a -> Id CKSyncEngineWillSendChangesEvent

instance IsCKSyncEngineWillSendChangesEvent (Id CKSyncEngineWillSendChangesEvent) where
  toCKSyncEngineWillSendChangesEvent = unsafeCastId

instance IsCKSyncEngineEvent (Id CKSyncEngineWillSendChangesEvent) where
  toCKSyncEngineEvent = unsafeCastId

instance IsNSObject (Id CKSyncEngineWillSendChangesEvent) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEnginePendingZoneDelete ----------

-- | A zone delete that needs to be sent to the server.
-- 
-- Phantom type for @CKSyncEnginePendingZoneDelete@.
data CKSyncEnginePendingZoneDelete

instance IsObjCObject (Id CKSyncEnginePendingZoneDelete) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEnginePendingZoneDelete"

class IsCKSyncEnginePendingDatabaseChange a => IsCKSyncEnginePendingZoneDelete a where
  toCKSyncEnginePendingZoneDelete :: a -> Id CKSyncEnginePendingZoneDelete

instance IsCKSyncEnginePendingZoneDelete (Id CKSyncEnginePendingZoneDelete) where
  toCKSyncEnginePendingZoneDelete = unsafeCastId

instance IsCKSyncEnginePendingDatabaseChange (Id CKSyncEnginePendingZoneDelete) where
  toCKSyncEnginePendingDatabaseChange = unsafeCastId

instance IsNSObject (Id CKSyncEnginePendingZoneDelete) where
  toNSObject = unsafeCastId

-- ---------- CKSyncEnginePendingZoneSave ----------

-- | A zone save that needs to be sent to the server.
-- 
-- Phantom type for @CKSyncEnginePendingZoneSave@.
data CKSyncEnginePendingZoneSave

instance IsObjCObject (Id CKSyncEnginePendingZoneSave) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKSyncEnginePendingZoneSave"

class IsCKSyncEnginePendingDatabaseChange a => IsCKSyncEnginePendingZoneSave a where
  toCKSyncEnginePendingZoneSave :: a -> Id CKSyncEnginePendingZoneSave

instance IsCKSyncEnginePendingZoneSave (Id CKSyncEnginePendingZoneSave) where
  toCKSyncEnginePendingZoneSave = unsafeCastId

instance IsCKSyncEnginePendingDatabaseChange (Id CKSyncEnginePendingZoneSave) where
  toCKSyncEnginePendingDatabaseChange = unsafeCastId

instance IsNSObject (Id CKSyncEnginePendingZoneSave) where
  toNSObject = unsafeCastId

-- ---------- CKOperation ----------

-- | Phantom type for @CKOperation@.
data CKOperation

instance IsObjCObject (Id CKOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKOperation"

class IsNSOperation a => IsCKOperation a where
  toCKOperation :: a -> Id CKOperation

instance IsCKOperation (Id CKOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKLocationSortDescriptor ----------

-- | Phantom type for @CKLocationSortDescriptor@.
data CKLocationSortDescriptor

instance IsObjCObject (Id CKLocationSortDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKLocationSortDescriptor"

class IsNSSortDescriptor a => IsCKLocationSortDescriptor a where
  toCKLocationSortDescriptor :: a -> Id CKLocationSortDescriptor

instance IsCKLocationSortDescriptor (Id CKLocationSortDescriptor) where
  toCKLocationSortDescriptor = unsafeCastId

instance IsNSObject (Id CKLocationSortDescriptor) where
  toNSObject = unsafeCastId

instance IsNSSortDescriptor (Id CKLocationSortDescriptor) where
  toNSSortDescriptor = unsafeCastId

-- ---------- CKAcceptSharesOperation ----------

-- | Phantom type for @CKAcceptSharesOperation@.
data CKAcceptSharesOperation

instance IsObjCObject (Id CKAcceptSharesOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKAcceptSharesOperation"

class IsCKOperation a => IsCKAcceptSharesOperation a where
  toCKAcceptSharesOperation :: a -> Id CKAcceptSharesOperation

instance IsCKAcceptSharesOperation (Id CKAcceptSharesOperation) where
  toCKAcceptSharesOperation = unsafeCastId

instance IsCKOperation (Id CKAcceptSharesOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKAcceptSharesOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKAcceptSharesOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKDatabaseOperation ----------

-- | Phantom type for @CKDatabaseOperation@.
data CKDatabaseOperation

instance IsObjCObject (Id CKDatabaseOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKDatabaseOperation"

class IsCKOperation a => IsCKDatabaseOperation a where
  toCKDatabaseOperation :: a -> Id CKDatabaseOperation

instance IsCKDatabaseOperation (Id CKDatabaseOperation) where
  toCKDatabaseOperation = unsafeCastId

instance IsCKOperation (Id CKDatabaseOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKDatabaseOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKDatabaseOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKDiscoverAllUserIdentitiesOperation ----------

-- | CKDiscoverAllUserIdentitiesOperation
--
-- Finds all discoverable users in the device's contacts database. No Contacts access dialog will be displayed.
--
-- This operation scales linearly with the number of email addresses and phone numbers in the device's address book.  It may take some time to complete.
-- 
-- Phantom type for @CKDiscoverAllUserIdentitiesOperation@.
data CKDiscoverAllUserIdentitiesOperation

instance IsObjCObject (Id CKDiscoverAllUserIdentitiesOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKDiscoverAllUserIdentitiesOperation"

class IsCKOperation a => IsCKDiscoverAllUserIdentitiesOperation a where
  toCKDiscoverAllUserIdentitiesOperation :: a -> Id CKDiscoverAllUserIdentitiesOperation

instance IsCKDiscoverAllUserIdentitiesOperation (Id CKDiscoverAllUserIdentitiesOperation) where
  toCKDiscoverAllUserIdentitiesOperation = unsafeCastId

instance IsCKOperation (Id CKDiscoverAllUserIdentitiesOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKDiscoverAllUserIdentitiesOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKDiscoverAllUserIdentitiesOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKDiscoverUserIdentitiesOperation ----------

-- | Phantom type for @CKDiscoverUserIdentitiesOperation@.
data CKDiscoverUserIdentitiesOperation

instance IsObjCObject (Id CKDiscoverUserIdentitiesOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKDiscoverUserIdentitiesOperation"

class IsCKOperation a => IsCKDiscoverUserIdentitiesOperation a where
  toCKDiscoverUserIdentitiesOperation :: a -> Id CKDiscoverUserIdentitiesOperation

instance IsCKDiscoverUserIdentitiesOperation (Id CKDiscoverUserIdentitiesOperation) where
  toCKDiscoverUserIdentitiesOperation = unsafeCastId

instance IsCKOperation (Id CKDiscoverUserIdentitiesOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKDiscoverUserIdentitiesOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKDiscoverUserIdentitiesOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKFetchNotificationChangesOperation ----------

-- | Phantom type for @CKFetchNotificationChangesOperation@.
data CKFetchNotificationChangesOperation

instance IsObjCObject (Id CKFetchNotificationChangesOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKFetchNotificationChangesOperation"

class IsCKOperation a => IsCKFetchNotificationChangesOperation a where
  toCKFetchNotificationChangesOperation :: a -> Id CKFetchNotificationChangesOperation

instance IsCKFetchNotificationChangesOperation (Id CKFetchNotificationChangesOperation) where
  toCKFetchNotificationChangesOperation = unsafeCastId

instance IsCKOperation (Id CKFetchNotificationChangesOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKFetchNotificationChangesOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKFetchNotificationChangesOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKFetchShareMetadataOperation ----------

-- | CKFetchShareMetadataOperation
--
-- Fetch the @CKShareMetadata@ for a share URL.
--
-- Since you can't know what container this share is in before you fetch its metadata, you may run this operation in any container you have access to
-- 
-- Phantom type for @CKFetchShareMetadataOperation@.
data CKFetchShareMetadataOperation

instance IsObjCObject (Id CKFetchShareMetadataOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKFetchShareMetadataOperation"

class IsCKOperation a => IsCKFetchShareMetadataOperation a where
  toCKFetchShareMetadataOperation :: a -> Id CKFetchShareMetadataOperation

instance IsCKFetchShareMetadataOperation (Id CKFetchShareMetadataOperation) where
  toCKFetchShareMetadataOperation = unsafeCastId

instance IsCKOperation (Id CKFetchShareMetadataOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKFetchShareMetadataOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKFetchShareMetadataOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKFetchShareParticipantsOperation ----------

-- | Phantom type for @CKFetchShareParticipantsOperation@.
data CKFetchShareParticipantsOperation

instance IsObjCObject (Id CKFetchShareParticipantsOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKFetchShareParticipantsOperation"

class IsCKOperation a => IsCKFetchShareParticipantsOperation a where
  toCKFetchShareParticipantsOperation :: a -> Id CKFetchShareParticipantsOperation

instance IsCKFetchShareParticipantsOperation (Id CKFetchShareParticipantsOperation) where
  toCKFetchShareParticipantsOperation = unsafeCastId

instance IsCKOperation (Id CKFetchShareParticipantsOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKFetchShareParticipantsOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKFetchShareParticipantsOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKMarkNotificationsReadOperation ----------

-- | Phantom type for @CKMarkNotificationsReadOperation@.
data CKMarkNotificationsReadOperation

instance IsObjCObject (Id CKMarkNotificationsReadOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKMarkNotificationsReadOperation"

class IsCKOperation a => IsCKMarkNotificationsReadOperation a where
  toCKMarkNotificationsReadOperation :: a -> Id CKMarkNotificationsReadOperation

instance IsCKMarkNotificationsReadOperation (Id CKMarkNotificationsReadOperation) where
  toCKMarkNotificationsReadOperation = unsafeCastId

instance IsCKOperation (Id CKMarkNotificationsReadOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKMarkNotificationsReadOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKMarkNotificationsReadOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKModifyBadgeOperation ----------

-- | Phantom type for @CKModifyBadgeOperation@.
data CKModifyBadgeOperation

instance IsObjCObject (Id CKModifyBadgeOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKModifyBadgeOperation"

class IsCKOperation a => IsCKModifyBadgeOperation a where
  toCKModifyBadgeOperation :: a -> Id CKModifyBadgeOperation

instance IsCKModifyBadgeOperation (Id CKModifyBadgeOperation) where
  toCKModifyBadgeOperation = unsafeCastId

instance IsCKOperation (Id CKModifyBadgeOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKModifyBadgeOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKModifyBadgeOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKShareRequestAccessOperation ----------

-- | Phantom type for @CKShareRequestAccessOperation@.
data CKShareRequestAccessOperation

instance IsObjCObject (Id CKShareRequestAccessOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKShareRequestAccessOperation"

class IsCKOperation a => IsCKShareRequestAccessOperation a where
  toCKShareRequestAccessOperation :: a -> Id CKShareRequestAccessOperation

instance IsCKShareRequestAccessOperation (Id CKShareRequestAccessOperation) where
  toCKShareRequestAccessOperation = unsafeCastId

instance IsCKOperation (Id CKShareRequestAccessOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKShareRequestAccessOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKShareRequestAccessOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKFetchDatabaseChangesOperation ----------

-- | CKFetchDatabaseChangesOperation
--
-- This operation will fetch changes to record zones within a database
--
-- If a change anchor from a previous @CKFetchDatabaseChangesOperation@ is passed in, only the zones that have changed since that anchor will be returned.  This per-database @serverChangeToken@ is not to be confused with the per-recordZone @serverChangeToken@ from @CKFetchRecordZoneChangesOperation.@  If this is your first fetch or if you wish to re-fetch all zones, pass nil for the change token.  Change token are opaque tokens and clients should not infer any behavior based on their content.  @CKFetchDatabaseChangesOperation@ is supported in a @privateCloudDatabase@ and @sharedCloudDatabase@
-- 
-- Phantom type for @CKFetchDatabaseChangesOperation@.
data CKFetchDatabaseChangesOperation

instance IsObjCObject (Id CKFetchDatabaseChangesOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKFetchDatabaseChangesOperation"

class IsCKDatabaseOperation a => IsCKFetchDatabaseChangesOperation a where
  toCKFetchDatabaseChangesOperation :: a -> Id CKFetchDatabaseChangesOperation

instance IsCKFetchDatabaseChangesOperation (Id CKFetchDatabaseChangesOperation) where
  toCKFetchDatabaseChangesOperation = unsafeCastId

instance IsCKDatabaseOperation (Id CKFetchDatabaseChangesOperation) where
  toCKDatabaseOperation = unsafeCastId

instance IsCKOperation (Id CKFetchDatabaseChangesOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKFetchDatabaseChangesOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKFetchDatabaseChangesOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKFetchRecordChangesOperation ----------

-- | CKFetchRecordChangesOperation
--
-- Use CKFetchRecordZoneChangesOperation instead of this class.
--
-- Any serverChangeTokens saved from a CKFetchRecordChangesOperation are usable as a serverRecordZoneChangeToken in CKFetchRecordZoneChangesOperation
--
-- This operation will fetch records changes in the given record zone.
--
-- If a change token from a previous @CKFetchRecordChangesOperation@ is passed in, only the records that have changed since that token will be fetched.  If this is your first fetch or if you wish to re-fetch all records, pass nil for the change token.  Change tokens are opaque tokens and clients should not infer any behavior based on their content
-- 
-- Phantom type for @CKFetchRecordChangesOperation@.
data CKFetchRecordChangesOperation

instance IsObjCObject (Id CKFetchRecordChangesOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKFetchRecordChangesOperation"

class IsCKDatabaseOperation a => IsCKFetchRecordChangesOperation a where
  toCKFetchRecordChangesOperation :: a -> Id CKFetchRecordChangesOperation

instance IsCKFetchRecordChangesOperation (Id CKFetchRecordChangesOperation) where
  toCKFetchRecordChangesOperation = unsafeCastId

instance IsCKDatabaseOperation (Id CKFetchRecordChangesOperation) where
  toCKDatabaseOperation = unsafeCastId

instance IsCKOperation (Id CKFetchRecordChangesOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKFetchRecordChangesOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKFetchRecordChangesOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKFetchRecordZoneChangesOperation ----------

-- | This operation will fetch records changes across the given record zones
--
-- For each @previousServerChangeToken@ passed in with a @CKFetchRecordZoneChangesConfiguration,@ only records that have changed since that anchor will be fetched.  If this is your first fetch of a zone or if you wish to re-fetch all records within a zone, do not include a @previousServerChangeToken.@  Change tokens are opaque tokens and clients should not infer any behavior based on their content.
-- 
-- Phantom type for @CKFetchRecordZoneChangesOperation@.
data CKFetchRecordZoneChangesOperation

instance IsObjCObject (Id CKFetchRecordZoneChangesOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKFetchRecordZoneChangesOperation"

class IsCKDatabaseOperation a => IsCKFetchRecordZoneChangesOperation a where
  toCKFetchRecordZoneChangesOperation :: a -> Id CKFetchRecordZoneChangesOperation

instance IsCKFetchRecordZoneChangesOperation (Id CKFetchRecordZoneChangesOperation) where
  toCKFetchRecordZoneChangesOperation = unsafeCastId

instance IsCKDatabaseOperation (Id CKFetchRecordZoneChangesOperation) where
  toCKDatabaseOperation = unsafeCastId

instance IsCKOperation (Id CKFetchRecordZoneChangesOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKFetchRecordZoneChangesOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKFetchRecordZoneChangesOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKFetchRecordZonesOperation ----------

-- | Phantom type for @CKFetchRecordZonesOperation@.
data CKFetchRecordZonesOperation

instance IsObjCObject (Id CKFetchRecordZonesOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKFetchRecordZonesOperation"

class IsCKDatabaseOperation a => IsCKFetchRecordZonesOperation a where
  toCKFetchRecordZonesOperation :: a -> Id CKFetchRecordZonesOperation

instance IsCKFetchRecordZonesOperation (Id CKFetchRecordZonesOperation) where
  toCKFetchRecordZonesOperation = unsafeCastId

instance IsCKDatabaseOperation (Id CKFetchRecordZonesOperation) where
  toCKDatabaseOperation = unsafeCastId

instance IsCKOperation (Id CKFetchRecordZonesOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKFetchRecordZonesOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKFetchRecordZonesOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKFetchRecordsOperation ----------

-- | Phantom type for @CKFetchRecordsOperation@.
data CKFetchRecordsOperation

instance IsObjCObject (Id CKFetchRecordsOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKFetchRecordsOperation"

class IsCKDatabaseOperation a => IsCKFetchRecordsOperation a where
  toCKFetchRecordsOperation :: a -> Id CKFetchRecordsOperation

instance IsCKFetchRecordsOperation (Id CKFetchRecordsOperation) where
  toCKFetchRecordsOperation = unsafeCastId

instance IsCKDatabaseOperation (Id CKFetchRecordsOperation) where
  toCKDatabaseOperation = unsafeCastId

instance IsCKOperation (Id CKFetchRecordsOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKFetchRecordsOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKFetchRecordsOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKFetchSubscriptionsOperation ----------

-- | Phantom type for @CKFetchSubscriptionsOperation@.
data CKFetchSubscriptionsOperation

instance IsObjCObject (Id CKFetchSubscriptionsOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKFetchSubscriptionsOperation"

class IsCKDatabaseOperation a => IsCKFetchSubscriptionsOperation a where
  toCKFetchSubscriptionsOperation :: a -> Id CKFetchSubscriptionsOperation

instance IsCKFetchSubscriptionsOperation (Id CKFetchSubscriptionsOperation) where
  toCKFetchSubscriptionsOperation = unsafeCastId

instance IsCKDatabaseOperation (Id CKFetchSubscriptionsOperation) where
  toCKDatabaseOperation = unsafeCastId

instance IsCKOperation (Id CKFetchSubscriptionsOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKFetchSubscriptionsOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKFetchSubscriptionsOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKFetchWebAuthTokenOperation ----------

-- | CKFetchWebAuthTokenOperation
--
-- This operation will fetch a web auth token given an API token obtained from the CloudKit Dashboard for your container
-- 
-- Phantom type for @CKFetchWebAuthTokenOperation@.
data CKFetchWebAuthTokenOperation

instance IsObjCObject (Id CKFetchWebAuthTokenOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKFetchWebAuthTokenOperation"

class IsCKDatabaseOperation a => IsCKFetchWebAuthTokenOperation a where
  toCKFetchWebAuthTokenOperation :: a -> Id CKFetchWebAuthTokenOperation

instance IsCKFetchWebAuthTokenOperation (Id CKFetchWebAuthTokenOperation) where
  toCKFetchWebAuthTokenOperation = unsafeCastId

instance IsCKDatabaseOperation (Id CKFetchWebAuthTokenOperation) where
  toCKDatabaseOperation = unsafeCastId

instance IsCKOperation (Id CKFetchWebAuthTokenOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKFetchWebAuthTokenOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKFetchWebAuthTokenOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKModifyRecordZonesOperation ----------

-- | Phantom type for @CKModifyRecordZonesOperation@.
data CKModifyRecordZonesOperation

instance IsObjCObject (Id CKModifyRecordZonesOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKModifyRecordZonesOperation"

class IsCKDatabaseOperation a => IsCKModifyRecordZonesOperation a where
  toCKModifyRecordZonesOperation :: a -> Id CKModifyRecordZonesOperation

instance IsCKModifyRecordZonesOperation (Id CKModifyRecordZonesOperation) where
  toCKModifyRecordZonesOperation = unsafeCastId

instance IsCKDatabaseOperation (Id CKModifyRecordZonesOperation) where
  toCKDatabaseOperation = unsafeCastId

instance IsCKOperation (Id CKModifyRecordZonesOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKModifyRecordZonesOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKModifyRecordZonesOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKModifyRecordsOperation ----------

-- | Phantom type for @CKModifyRecordsOperation@.
data CKModifyRecordsOperation

instance IsObjCObject (Id CKModifyRecordsOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKModifyRecordsOperation"

class IsCKDatabaseOperation a => IsCKModifyRecordsOperation a where
  toCKModifyRecordsOperation :: a -> Id CKModifyRecordsOperation

instance IsCKModifyRecordsOperation (Id CKModifyRecordsOperation) where
  toCKModifyRecordsOperation = unsafeCastId

instance IsCKDatabaseOperation (Id CKModifyRecordsOperation) where
  toCKDatabaseOperation = unsafeCastId

instance IsCKOperation (Id CKModifyRecordsOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKModifyRecordsOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKModifyRecordsOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKModifySubscriptionsOperation ----------

-- | Phantom type for @CKModifySubscriptionsOperation@.
data CKModifySubscriptionsOperation

instance IsObjCObject (Id CKModifySubscriptionsOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKModifySubscriptionsOperation"

class IsCKDatabaseOperation a => IsCKModifySubscriptionsOperation a where
  toCKModifySubscriptionsOperation :: a -> Id CKModifySubscriptionsOperation

instance IsCKModifySubscriptionsOperation (Id CKModifySubscriptionsOperation) where
  toCKModifySubscriptionsOperation = unsafeCastId

instance IsCKDatabaseOperation (Id CKModifySubscriptionsOperation) where
  toCKDatabaseOperation = unsafeCastId

instance IsCKOperation (Id CKModifySubscriptionsOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKModifySubscriptionsOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKModifySubscriptionsOperation) where
  toNSOperation = unsafeCastId

-- ---------- CKQueryOperation ----------

-- | Phantom type for @CKQueryOperation@.
data CKQueryOperation

instance IsObjCObject (Id CKQueryOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKQueryOperation"

class IsCKDatabaseOperation a => IsCKQueryOperation a where
  toCKQueryOperation :: a -> Id CKQueryOperation

instance IsCKQueryOperation (Id CKQueryOperation) where
  toCKQueryOperation = unsafeCastId

instance IsCKDatabaseOperation (Id CKQueryOperation) where
  toCKDatabaseOperation = unsafeCastId

instance IsCKOperation (Id CKQueryOperation) where
  toCKOperation = unsafeCastId

instance IsNSObject (Id CKQueryOperation) where
  toNSObject = unsafeCastId

instance IsNSOperation (Id CKQueryOperation) where
  toNSOperation = unsafeCastId
