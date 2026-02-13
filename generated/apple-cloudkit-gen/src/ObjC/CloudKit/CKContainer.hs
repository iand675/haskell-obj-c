{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKContainer
--
-- A CKContainer, and its CKDatabases, are the main entry points into the CloudKit framework.
--
-- Several methods in CloudKit accept completion handlers to indicate when they're completed.  All CKOperation subclasses include progress and completion blocks to report significant events in their lifecycles.  Each of these handlers and blocks is invoked on a non-main serial queue.  The receiver is responsible for handling the message on a different queue or thread if it is required.
--
-- Generated bindings for @CKContainer@.
module ObjC.CloudKit.CKContainer
  ( CKContainer
  , IsCKContainer(..)
  , init_
  , new
  , defaultContainer
  , containerWithIdentifier
  , addOperation
  , fetchLongLivedOperationWithID_completionHandler
  , fetchShareParticipantWithEmailAddress_completionHandler
  , fetchShareParticipantWithPhoneNumber_completionHandler
  , fetchShareParticipantWithUserRecordID_completionHandler
  , fetchShareMetadataWithURL_completionHandler
  , acceptShareMetadata_completionHandler
  , fetchUserRecordIDWithCompletionHandler
  , discoverUserIdentityWithEmailAddress_completionHandler
  , discoverUserIdentityWithPhoneNumber_completionHandler
  , discoverUserIdentityWithUserRecordID_completionHandler
  , statusForApplicationPermission_completionHandler
  , requestApplicationPermission_completionHandler
  , accountStatusWithCompletionHandler
  , databaseWithDatabaseScope
  , containerIdentifier
  , privateCloudDatabase
  , publicCloudDatabase
  , sharedCloudDatabase
  , acceptShareMetadata_completionHandlerSelector
  , accountStatusWithCompletionHandlerSelector
  , addOperationSelector
  , containerIdentifierSelector
  , containerWithIdentifierSelector
  , databaseWithDatabaseScopeSelector
  , defaultContainerSelector
  , discoverUserIdentityWithEmailAddress_completionHandlerSelector
  , discoverUserIdentityWithPhoneNumber_completionHandlerSelector
  , discoverUserIdentityWithUserRecordID_completionHandlerSelector
  , fetchLongLivedOperationWithID_completionHandlerSelector
  , fetchShareMetadataWithURL_completionHandlerSelector
  , fetchShareParticipantWithEmailAddress_completionHandlerSelector
  , fetchShareParticipantWithPhoneNumber_completionHandlerSelector
  , fetchShareParticipantWithUserRecordID_completionHandlerSelector
  , fetchUserRecordIDWithCompletionHandlerSelector
  , initSelector
  , newSelector
  , privateCloudDatabaseSelector
  , publicCloudDatabaseSelector
  , requestApplicationPermission_completionHandlerSelector
  , sharedCloudDatabaseSelector
  , statusForApplicationPermission_completionHandlerSelector

  -- * Enum types
  , CKApplicationPermissions(CKApplicationPermissions)
  , pattern CKApplicationPermissionUserDiscoverability
  , CKDatabaseScope(CKDatabaseScope)
  , pattern CKDatabaseScopePublic
  , pattern CKDatabaseScopePrivate
  , pattern CKDatabaseScopeShared

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
init_ :: IsCKContainer ckContainer => ckContainer -> IO (Id CKContainer)
init_ ckContainer =
  sendOwnedMessage ckContainer initSelector

-- | @+ new@
new :: IO (Id CKContainer)
new  =
  do
    cls' <- getRequiredClass "CKContainer"
    sendOwnedClassMessage cls' newSelector

-- | Convenience method that uses the calling process' "iCloud.\\(application-identifier)" as the container identifier
--
-- application-identifier is the calling process' @application-identifier@ entitlement on iOS / tvOS / watchOS.  application-identifier is the calling process' @com.apple.application-identifier@ entitlement on macOS.  On all OSes, if an @com.apple.developer.associated-application-identifier@ entitlement is present, its value will be preferred over the @application-identifier@ variants.
--
-- ObjC selector: @+ defaultContainer@
defaultContainer :: IO (Id CKContainer)
defaultContainer  =
  do
    cls' <- getRequiredClass "CKContainer"
    sendClassMessage cls' defaultContainerSelector

-- | Obtain a CKContainer for the given containerIdentifier
--
-- If the application is in production mode (aka, @com.apple.developer.icloud-container-environment@ is set to Production in your entitlements plist, and you have no override in @com.apple.developer.icloud-container-development-container-identifiers),@ then the production environment is used.
--
-- ObjC selector: @+ containerWithIdentifier:@
containerWithIdentifier :: IsNSString containerIdentifier => containerIdentifier -> IO (Id CKContainer)
containerWithIdentifier containerIdentifier =
  do
    cls' <- getRequiredClass "CKContainer"
    sendClassMessage cls' containerWithIdentifierSelector (toNSString containerIdentifier)

-- | @- addOperation:@
addOperation :: (IsCKContainer ckContainer, IsCKOperation operation) => ckContainer -> operation -> IO ()
addOperation ckContainer operation =
  sendMessage ckContainer addOperationSelector (toCKOperation operation)

-- | @- fetchLongLivedOperationWithID:completionHandler:@
fetchLongLivedOperationWithID_completionHandler :: (IsCKContainer ckContainer, IsNSString operationID) => ckContainer -> operationID -> Ptr () -> IO ()
fetchLongLivedOperationWithID_completionHandler ckContainer operationID completionHandler =
  sendMessage ckContainer fetchLongLivedOperationWithID_completionHandlerSelector (toNSString operationID) completionHandler

-- | Fetches share participants matching the provided info.
--
-- @CKFetchShareParticipantsOperation@ is the more configurable, @CKOperation@ -based alternative to these methods.
--
-- ObjC selector: @- fetchShareParticipantWithEmailAddress:completionHandler:@
fetchShareParticipantWithEmailAddress_completionHandler :: (IsCKContainer ckContainer, IsNSString emailAddress) => ckContainer -> emailAddress -> Ptr () -> IO ()
fetchShareParticipantWithEmailAddress_completionHandler ckContainer emailAddress completionHandler =
  sendMessage ckContainer fetchShareParticipantWithEmailAddress_completionHandlerSelector (toNSString emailAddress) completionHandler

-- | @- fetchShareParticipantWithPhoneNumber:completionHandler:@
fetchShareParticipantWithPhoneNumber_completionHandler :: (IsCKContainer ckContainer, IsNSString phoneNumber) => ckContainer -> phoneNumber -> Ptr () -> IO ()
fetchShareParticipantWithPhoneNumber_completionHandler ckContainer phoneNumber completionHandler =
  sendMessage ckContainer fetchShareParticipantWithPhoneNumber_completionHandlerSelector (toNSString phoneNumber) completionHandler

-- | @- fetchShareParticipantWithUserRecordID:completionHandler:@
fetchShareParticipantWithUserRecordID_completionHandler :: (IsCKContainer ckContainer, IsCKRecordID userRecordID) => ckContainer -> userRecordID -> Ptr () -> IO ()
fetchShareParticipantWithUserRecordID_completionHandler ckContainer userRecordID completionHandler =
  sendMessage ckContainer fetchShareParticipantWithUserRecordID_completionHandlerSelector (toCKRecordID userRecordID) completionHandler

-- | @- fetchShareMetadataWithURL:completionHandler:@
fetchShareMetadataWithURL_completionHandler :: (IsCKContainer ckContainer, IsNSURL url) => ckContainer -> url -> Ptr () -> IO ()
fetchShareMetadataWithURL_completionHandler ckContainer url completionHandler =
  sendMessage ckContainer fetchShareMetadataWithURL_completionHandlerSelector (toNSURL url) completionHandler

-- | @- acceptShareMetadata:completionHandler:@
acceptShareMetadata_completionHandler :: (IsCKContainer ckContainer, IsCKShareMetadata metadata) => ckContainer -> metadata -> Ptr () -> IO ()
acceptShareMetadata_completionHandler ckContainer metadata completionHandler =
  sendMessage ckContainer acceptShareMetadata_completionHandlerSelector (toCKShareMetadata metadata) completionHandler

-- | If there is no iCloud account configured, or if access is restricted, a @CKErrorNotAuthenticated@ error will be returned.
--
-- This work is treated as having @NSQualityOfServiceUserInitiated@ quality of service.
--
-- ObjC selector: @- fetchUserRecordIDWithCompletionHandler:@
fetchUserRecordIDWithCompletionHandler :: IsCKContainer ckContainer => ckContainer -> Ptr () -> IO ()
fetchUserRecordIDWithCompletionHandler ckContainer completionHandler =
  sendMessage ckContainer fetchUserRecordIDWithCompletionHandlerSelector completionHandler

-- | Fetches the user identity that corresponds to the given email address.
--
-- Only users who have opted-in to user discoverability will have their identities returned by this method.  If a user with the inputted email exists in iCloud, but has not opted-in to user discoverability, this method completes with a nil @userInfo.@  @CKDiscoverUserIdentitiesOperation@ is the more configurable, @CKOperation@ -based alternative to this method
--
-- ObjC selector: @- discoverUserIdentityWithEmailAddress:completionHandler:@
discoverUserIdentityWithEmailAddress_completionHandler :: (IsCKContainer ckContainer, IsNSString email) => ckContainer -> email -> Ptr () -> IO ()
discoverUserIdentityWithEmailAddress_completionHandler ckContainer email completionHandler =
  sendMessage ckContainer discoverUserIdentityWithEmailAddress_completionHandlerSelector (toNSString email) completionHandler

-- | Fetches the user identity that corresponds to the given phone number.
--
-- Only users who have opted-in to user discoverability will have their identities returned by this method.  If a user with the inputted phone number exists in iCloud, but has not opted-in to user discoverability, this method completes with a nil @userInfo.@  @CKDiscoverUserIdentitiesOperation@ is the more configurable, @CKOperation@ -based alternative to this method
--
-- ObjC selector: @- discoverUserIdentityWithPhoneNumber:completionHandler:@
discoverUserIdentityWithPhoneNumber_completionHandler :: (IsCKContainer ckContainer, IsNSString phoneNumber) => ckContainer -> phoneNumber -> Ptr () -> IO ()
discoverUserIdentityWithPhoneNumber_completionHandler ckContainer phoneNumber completionHandler =
  sendMessage ckContainer discoverUserIdentityWithPhoneNumber_completionHandlerSelector (toNSString phoneNumber) completionHandler

-- | Fetches the user identity that corresponds to the given user record id.
--
-- Only users who have opted-in to user discoverability will have their identities returned by this method.  If a user has not opted-in to user discoverability, this method completes with a nil @userInfo.@  @CKDiscoverUserIdentitiesOperation@ is the more configurable, @CKOperation@ -based alternative to this method
--
-- ObjC selector: @- discoverUserIdentityWithUserRecordID:completionHandler:@
discoverUserIdentityWithUserRecordID_completionHandler :: (IsCKContainer ckContainer, IsCKRecordID userRecordID) => ckContainer -> userRecordID -> Ptr () -> IO ()
discoverUserIdentityWithUserRecordID_completionHandler ckContainer userRecordID completionHandler =
  sendMessage ckContainer discoverUserIdentityWithUserRecordID_completionHandlerSelector (toCKRecordID userRecordID) completionHandler

-- | @- statusForApplicationPermission:completionHandler:@
statusForApplicationPermission_completionHandler :: IsCKContainer ckContainer => ckContainer -> CKApplicationPermissions -> Ptr () -> IO ()
statusForApplicationPermission_completionHandler ckContainer applicationPermission completionHandler =
  sendMessage ckContainer statusForApplicationPermission_completionHandlerSelector applicationPermission completionHandler

-- | @- requestApplicationPermission:completionHandler:@
requestApplicationPermission_completionHandler :: IsCKContainer ckContainer => ckContainer -> CKApplicationPermissions -> Ptr () -> IO ()
requestApplicationPermission_completionHandler ckContainer applicationPermission completionHandler =
  sendMessage ckContainer requestApplicationPermission_completionHandlerSelector applicationPermission completionHandler

-- | @- accountStatusWithCompletionHandler:@
accountStatusWithCompletionHandler :: IsCKContainer ckContainer => ckContainer -> Ptr () -> IO ()
accountStatusWithCompletionHandler ckContainer completionHandler =
  sendMessage ckContainer accountStatusWithCompletionHandlerSelector completionHandler

-- | Convenience methods
--
-- Returns: a database that's pointer-equal to one of the above properties
--
-- ObjC selector: @- databaseWithDatabaseScope:@
databaseWithDatabaseScope :: IsCKContainer ckContainer => ckContainer -> CKDatabaseScope -> IO (Id CKDatabase)
databaseWithDatabaseScope ckContainer databaseScope =
  sendMessage ckContainer databaseWithDatabaseScopeSelector databaseScope

-- | @- containerIdentifier@
containerIdentifier :: IsCKContainer ckContainer => ckContainer -> IO (Id NSString)
containerIdentifier ckContainer =
  sendMessage ckContainer containerIdentifierSelector

-- | @- privateCloudDatabase@
privateCloudDatabase :: IsCKContainer ckContainer => ckContainer -> IO (Id CKDatabase)
privateCloudDatabase ckContainer =
  sendMessage ckContainer privateCloudDatabaseSelector

-- | @- publicCloudDatabase@
publicCloudDatabase :: IsCKContainer ckContainer => ckContainer -> IO (Id CKDatabase)
publicCloudDatabase ckContainer =
  sendMessage ckContainer publicCloudDatabaseSelector

-- | @- sharedCloudDatabase@
sharedCloudDatabase :: IsCKContainer ckContainer => ckContainer -> IO (Id CKDatabase)
sharedCloudDatabase ckContainer =
  sendMessage ckContainer sharedCloudDatabaseSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKContainer)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKContainer)
newSelector = mkSelector "new"

-- | @Selector@ for @defaultContainer@
defaultContainerSelector :: Selector '[] (Id CKContainer)
defaultContainerSelector = mkSelector "defaultContainer"

-- | @Selector@ for @containerWithIdentifier:@
containerWithIdentifierSelector :: Selector '[Id NSString] (Id CKContainer)
containerWithIdentifierSelector = mkSelector "containerWithIdentifier:"

-- | @Selector@ for @addOperation:@
addOperationSelector :: Selector '[Id CKOperation] ()
addOperationSelector = mkSelector "addOperation:"

-- | @Selector@ for @fetchLongLivedOperationWithID:completionHandler:@
fetchLongLivedOperationWithID_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
fetchLongLivedOperationWithID_completionHandlerSelector = mkSelector "fetchLongLivedOperationWithID:completionHandler:"

-- | @Selector@ for @fetchShareParticipantWithEmailAddress:completionHandler:@
fetchShareParticipantWithEmailAddress_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
fetchShareParticipantWithEmailAddress_completionHandlerSelector = mkSelector "fetchShareParticipantWithEmailAddress:completionHandler:"

-- | @Selector@ for @fetchShareParticipantWithPhoneNumber:completionHandler:@
fetchShareParticipantWithPhoneNumber_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
fetchShareParticipantWithPhoneNumber_completionHandlerSelector = mkSelector "fetchShareParticipantWithPhoneNumber:completionHandler:"

-- | @Selector@ for @fetchShareParticipantWithUserRecordID:completionHandler:@
fetchShareParticipantWithUserRecordID_completionHandlerSelector :: Selector '[Id CKRecordID, Ptr ()] ()
fetchShareParticipantWithUserRecordID_completionHandlerSelector = mkSelector "fetchShareParticipantWithUserRecordID:completionHandler:"

-- | @Selector@ for @fetchShareMetadataWithURL:completionHandler:@
fetchShareMetadataWithURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
fetchShareMetadataWithURL_completionHandlerSelector = mkSelector "fetchShareMetadataWithURL:completionHandler:"

-- | @Selector@ for @acceptShareMetadata:completionHandler:@
acceptShareMetadata_completionHandlerSelector :: Selector '[Id CKShareMetadata, Ptr ()] ()
acceptShareMetadata_completionHandlerSelector = mkSelector "acceptShareMetadata:completionHandler:"

-- | @Selector@ for @fetchUserRecordIDWithCompletionHandler:@
fetchUserRecordIDWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
fetchUserRecordIDWithCompletionHandlerSelector = mkSelector "fetchUserRecordIDWithCompletionHandler:"

-- | @Selector@ for @discoverUserIdentityWithEmailAddress:completionHandler:@
discoverUserIdentityWithEmailAddress_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
discoverUserIdentityWithEmailAddress_completionHandlerSelector = mkSelector "discoverUserIdentityWithEmailAddress:completionHandler:"

-- | @Selector@ for @discoverUserIdentityWithPhoneNumber:completionHandler:@
discoverUserIdentityWithPhoneNumber_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
discoverUserIdentityWithPhoneNumber_completionHandlerSelector = mkSelector "discoverUserIdentityWithPhoneNumber:completionHandler:"

-- | @Selector@ for @discoverUserIdentityWithUserRecordID:completionHandler:@
discoverUserIdentityWithUserRecordID_completionHandlerSelector :: Selector '[Id CKRecordID, Ptr ()] ()
discoverUserIdentityWithUserRecordID_completionHandlerSelector = mkSelector "discoverUserIdentityWithUserRecordID:completionHandler:"

-- | @Selector@ for @statusForApplicationPermission:completionHandler:@
statusForApplicationPermission_completionHandlerSelector :: Selector '[CKApplicationPermissions, Ptr ()] ()
statusForApplicationPermission_completionHandlerSelector = mkSelector "statusForApplicationPermission:completionHandler:"

-- | @Selector@ for @requestApplicationPermission:completionHandler:@
requestApplicationPermission_completionHandlerSelector :: Selector '[CKApplicationPermissions, Ptr ()] ()
requestApplicationPermission_completionHandlerSelector = mkSelector "requestApplicationPermission:completionHandler:"

-- | @Selector@ for @accountStatusWithCompletionHandler:@
accountStatusWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
accountStatusWithCompletionHandlerSelector = mkSelector "accountStatusWithCompletionHandler:"

-- | @Selector@ for @databaseWithDatabaseScope:@
databaseWithDatabaseScopeSelector :: Selector '[CKDatabaseScope] (Id CKDatabase)
databaseWithDatabaseScopeSelector = mkSelector "databaseWithDatabaseScope:"

-- | @Selector@ for @containerIdentifier@
containerIdentifierSelector :: Selector '[] (Id NSString)
containerIdentifierSelector = mkSelector "containerIdentifier"

-- | @Selector@ for @privateCloudDatabase@
privateCloudDatabaseSelector :: Selector '[] (Id CKDatabase)
privateCloudDatabaseSelector = mkSelector "privateCloudDatabase"

-- | @Selector@ for @publicCloudDatabase@
publicCloudDatabaseSelector :: Selector '[] (Id CKDatabase)
publicCloudDatabaseSelector = mkSelector "publicCloudDatabase"

-- | @Selector@ for @sharedCloudDatabase@
sharedCloudDatabaseSelector :: Selector '[] (Id CKDatabase)
sharedCloudDatabaseSelector = mkSelector "sharedCloudDatabase"

