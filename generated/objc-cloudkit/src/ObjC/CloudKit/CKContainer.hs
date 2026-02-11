{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , defaultContainerSelector
  , containerWithIdentifierSelector
  , addOperationSelector
  , fetchLongLivedOperationWithID_completionHandlerSelector
  , fetchShareParticipantWithEmailAddress_completionHandlerSelector
  , fetchShareParticipantWithPhoneNumber_completionHandlerSelector
  , fetchShareParticipantWithUserRecordID_completionHandlerSelector
  , fetchShareMetadataWithURL_completionHandlerSelector
  , acceptShareMetadata_completionHandlerSelector
  , fetchUserRecordIDWithCompletionHandlerSelector
  , discoverUserIdentityWithEmailAddress_completionHandlerSelector
  , discoverUserIdentityWithPhoneNumber_completionHandlerSelector
  , discoverUserIdentityWithUserRecordID_completionHandlerSelector
  , statusForApplicationPermission_completionHandlerSelector
  , requestApplicationPermission_completionHandlerSelector
  , accountStatusWithCompletionHandlerSelector
  , databaseWithDatabaseScopeSelector
  , containerIdentifierSelector
  , privateCloudDatabaseSelector
  , publicCloudDatabaseSelector
  , sharedCloudDatabaseSelector

  -- * Enum types
  , CKApplicationPermissions(CKApplicationPermissions)
  , pattern CKApplicationPermissionUserDiscoverability
  , CKDatabaseScope(CKDatabaseScope)
  , pattern CKDatabaseScopePublic
  , pattern CKDatabaseScopePrivate
  , pattern CKDatabaseScopeShared

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
import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKContainer ckContainer => ckContainer -> IO (Id CKContainer)
init_ ckContainer  =
  sendMsg ckContainer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKContainer)
new  =
  do
    cls' <- getRequiredClass "CKContainer"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Convenience method that uses the calling process' "iCloud.\\(application-identifier)" as the container identifier
--
-- application-identifier is the calling process' @application-identifier@ entitlement on iOS / tvOS / watchOS.  application-identifier is the calling process' @com.apple.application-identifier@ entitlement on macOS.  On all OSes, if an @com.apple.developer.associated-application-identifier@ entitlement is present, its value will be preferred over the @application-identifier@ variants.
--
-- ObjC selector: @+ defaultContainer@
defaultContainer :: IO (Id CKContainer)
defaultContainer  =
  do
    cls' <- getRequiredClass "CKContainer"
    sendClassMsg cls' (mkSelector "defaultContainer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Obtain a CKContainer for the given containerIdentifier
--
-- If the application is in production mode (aka, @com.apple.developer.icloud-container-environment@ is set to Production in your entitlements plist, and you have no override in @com.apple.developer.icloud-container-development-container-identifiers),@ then the production environment is used.
--
-- ObjC selector: @+ containerWithIdentifier:@
containerWithIdentifier :: IsNSString containerIdentifier => containerIdentifier -> IO (Id CKContainer)
containerWithIdentifier containerIdentifier =
  do
    cls' <- getRequiredClass "CKContainer"
    withObjCPtr containerIdentifier $ \raw_containerIdentifier ->
      sendClassMsg cls' (mkSelector "containerWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_containerIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- addOperation:@
addOperation :: (IsCKContainer ckContainer, IsCKOperation operation) => ckContainer -> operation -> IO ()
addOperation ckContainer  operation =
withObjCPtr operation $ \raw_operation ->
    sendMsg ckContainer (mkSelector "addOperation:") retVoid [argPtr (castPtr raw_operation :: Ptr ())]

-- | @- fetchLongLivedOperationWithID:completionHandler:@
fetchLongLivedOperationWithID_completionHandler :: (IsCKContainer ckContainer, IsNSString operationID) => ckContainer -> operationID -> Ptr () -> IO ()
fetchLongLivedOperationWithID_completionHandler ckContainer  operationID completionHandler =
withObjCPtr operationID $ \raw_operationID ->
    sendMsg ckContainer (mkSelector "fetchLongLivedOperationWithID:completionHandler:") retVoid [argPtr (castPtr raw_operationID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Fetches share participants matching the provided info.
--
-- @CKFetchShareParticipantsOperation@ is the more configurable, @CKOperation@ -based alternative to these methods.
--
-- ObjC selector: @- fetchShareParticipantWithEmailAddress:completionHandler:@
fetchShareParticipantWithEmailAddress_completionHandler :: (IsCKContainer ckContainer, IsNSString emailAddress) => ckContainer -> emailAddress -> Ptr () -> IO ()
fetchShareParticipantWithEmailAddress_completionHandler ckContainer  emailAddress completionHandler =
withObjCPtr emailAddress $ \raw_emailAddress ->
    sendMsg ckContainer (mkSelector "fetchShareParticipantWithEmailAddress:completionHandler:") retVoid [argPtr (castPtr raw_emailAddress :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- fetchShareParticipantWithPhoneNumber:completionHandler:@
fetchShareParticipantWithPhoneNumber_completionHandler :: (IsCKContainer ckContainer, IsNSString phoneNumber) => ckContainer -> phoneNumber -> Ptr () -> IO ()
fetchShareParticipantWithPhoneNumber_completionHandler ckContainer  phoneNumber completionHandler =
withObjCPtr phoneNumber $ \raw_phoneNumber ->
    sendMsg ckContainer (mkSelector "fetchShareParticipantWithPhoneNumber:completionHandler:") retVoid [argPtr (castPtr raw_phoneNumber :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- fetchShareParticipantWithUserRecordID:completionHandler:@
fetchShareParticipantWithUserRecordID_completionHandler :: (IsCKContainer ckContainer, IsCKRecordID userRecordID) => ckContainer -> userRecordID -> Ptr () -> IO ()
fetchShareParticipantWithUserRecordID_completionHandler ckContainer  userRecordID completionHandler =
withObjCPtr userRecordID $ \raw_userRecordID ->
    sendMsg ckContainer (mkSelector "fetchShareParticipantWithUserRecordID:completionHandler:") retVoid [argPtr (castPtr raw_userRecordID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- fetchShareMetadataWithURL:completionHandler:@
fetchShareMetadataWithURL_completionHandler :: (IsCKContainer ckContainer, IsNSURL url) => ckContainer -> url -> Ptr () -> IO ()
fetchShareMetadataWithURL_completionHandler ckContainer  url completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg ckContainer (mkSelector "fetchShareMetadataWithURL:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- acceptShareMetadata:completionHandler:@
acceptShareMetadata_completionHandler :: (IsCKContainer ckContainer, IsCKShareMetadata metadata) => ckContainer -> metadata -> Ptr () -> IO ()
acceptShareMetadata_completionHandler ckContainer  metadata completionHandler =
withObjCPtr metadata $ \raw_metadata ->
    sendMsg ckContainer (mkSelector "acceptShareMetadata:completionHandler:") retVoid [argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | If there is no iCloud account configured, or if access is restricted, a @CKErrorNotAuthenticated@ error will be returned.
--
-- This work is treated as having @NSQualityOfServiceUserInitiated@ quality of service.
--
-- ObjC selector: @- fetchUserRecordIDWithCompletionHandler:@
fetchUserRecordIDWithCompletionHandler :: IsCKContainer ckContainer => ckContainer -> Ptr () -> IO ()
fetchUserRecordIDWithCompletionHandler ckContainer  completionHandler =
  sendMsg ckContainer (mkSelector "fetchUserRecordIDWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Fetches the user identity that corresponds to the given email address.
--
-- Only users who have opted-in to user discoverability will have their identities returned by this method.  If a user with the inputted email exists in iCloud, but has not opted-in to user discoverability, this method completes with a nil @userInfo.@  @CKDiscoverUserIdentitiesOperation@ is the more configurable, @CKOperation@ -based alternative to this method
--
-- ObjC selector: @- discoverUserIdentityWithEmailAddress:completionHandler:@
discoverUserIdentityWithEmailAddress_completionHandler :: (IsCKContainer ckContainer, IsNSString email) => ckContainer -> email -> Ptr () -> IO ()
discoverUserIdentityWithEmailAddress_completionHandler ckContainer  email completionHandler =
withObjCPtr email $ \raw_email ->
    sendMsg ckContainer (mkSelector "discoverUserIdentityWithEmailAddress:completionHandler:") retVoid [argPtr (castPtr raw_email :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Fetches the user identity that corresponds to the given phone number.
--
-- Only users who have opted-in to user discoverability will have their identities returned by this method.  If a user with the inputted phone number exists in iCloud, but has not opted-in to user discoverability, this method completes with a nil @userInfo.@  @CKDiscoverUserIdentitiesOperation@ is the more configurable, @CKOperation@ -based alternative to this method
--
-- ObjC selector: @- discoverUserIdentityWithPhoneNumber:completionHandler:@
discoverUserIdentityWithPhoneNumber_completionHandler :: (IsCKContainer ckContainer, IsNSString phoneNumber) => ckContainer -> phoneNumber -> Ptr () -> IO ()
discoverUserIdentityWithPhoneNumber_completionHandler ckContainer  phoneNumber completionHandler =
withObjCPtr phoneNumber $ \raw_phoneNumber ->
    sendMsg ckContainer (mkSelector "discoverUserIdentityWithPhoneNumber:completionHandler:") retVoid [argPtr (castPtr raw_phoneNumber :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Fetches the user identity that corresponds to the given user record id.
--
-- Only users who have opted-in to user discoverability will have their identities returned by this method.  If a user has not opted-in to user discoverability, this method completes with a nil @userInfo.@  @CKDiscoverUserIdentitiesOperation@ is the more configurable, @CKOperation@ -based alternative to this method
--
-- ObjC selector: @- discoverUserIdentityWithUserRecordID:completionHandler:@
discoverUserIdentityWithUserRecordID_completionHandler :: (IsCKContainer ckContainer, IsCKRecordID userRecordID) => ckContainer -> userRecordID -> Ptr () -> IO ()
discoverUserIdentityWithUserRecordID_completionHandler ckContainer  userRecordID completionHandler =
withObjCPtr userRecordID $ \raw_userRecordID ->
    sendMsg ckContainer (mkSelector "discoverUserIdentityWithUserRecordID:completionHandler:") retVoid [argPtr (castPtr raw_userRecordID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- statusForApplicationPermission:completionHandler:@
statusForApplicationPermission_completionHandler :: IsCKContainer ckContainer => ckContainer -> CKApplicationPermissions -> Ptr () -> IO ()
statusForApplicationPermission_completionHandler ckContainer  applicationPermission completionHandler =
  sendMsg ckContainer (mkSelector "statusForApplicationPermission:completionHandler:") retVoid [argCULong (coerce applicationPermission), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- requestApplicationPermission:completionHandler:@
requestApplicationPermission_completionHandler :: IsCKContainer ckContainer => ckContainer -> CKApplicationPermissions -> Ptr () -> IO ()
requestApplicationPermission_completionHandler ckContainer  applicationPermission completionHandler =
  sendMsg ckContainer (mkSelector "requestApplicationPermission:completionHandler:") retVoid [argCULong (coerce applicationPermission), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- accountStatusWithCompletionHandler:@
accountStatusWithCompletionHandler :: IsCKContainer ckContainer => ckContainer -> Ptr () -> IO ()
accountStatusWithCompletionHandler ckContainer  completionHandler =
  sendMsg ckContainer (mkSelector "accountStatusWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Convenience methods
--
-- Returns: a database that's pointer-equal to one of the above properties
--
-- ObjC selector: @- databaseWithDatabaseScope:@
databaseWithDatabaseScope :: IsCKContainer ckContainer => ckContainer -> CKDatabaseScope -> IO (Id CKDatabase)
databaseWithDatabaseScope ckContainer  databaseScope =
  sendMsg ckContainer (mkSelector "databaseWithDatabaseScope:") (retPtr retVoid) [argCLong (coerce databaseScope)] >>= retainedObject . castPtr

-- | @- containerIdentifier@
containerIdentifier :: IsCKContainer ckContainer => ckContainer -> IO (Id NSString)
containerIdentifier ckContainer  =
  sendMsg ckContainer (mkSelector "containerIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- privateCloudDatabase@
privateCloudDatabase :: IsCKContainer ckContainer => ckContainer -> IO (Id CKDatabase)
privateCloudDatabase ckContainer  =
  sendMsg ckContainer (mkSelector "privateCloudDatabase") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- publicCloudDatabase@
publicCloudDatabase :: IsCKContainer ckContainer => ckContainer -> IO (Id CKDatabase)
publicCloudDatabase ckContainer  =
  sendMsg ckContainer (mkSelector "publicCloudDatabase") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sharedCloudDatabase@
sharedCloudDatabase :: IsCKContainer ckContainer => ckContainer -> IO (Id CKDatabase)
sharedCloudDatabase ckContainer  =
  sendMsg ckContainer (mkSelector "sharedCloudDatabase") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @defaultContainer@
defaultContainerSelector :: Selector
defaultContainerSelector = mkSelector "defaultContainer"

-- | @Selector@ for @containerWithIdentifier:@
containerWithIdentifierSelector :: Selector
containerWithIdentifierSelector = mkSelector "containerWithIdentifier:"

-- | @Selector@ for @addOperation:@
addOperationSelector :: Selector
addOperationSelector = mkSelector "addOperation:"

-- | @Selector@ for @fetchLongLivedOperationWithID:completionHandler:@
fetchLongLivedOperationWithID_completionHandlerSelector :: Selector
fetchLongLivedOperationWithID_completionHandlerSelector = mkSelector "fetchLongLivedOperationWithID:completionHandler:"

-- | @Selector@ for @fetchShareParticipantWithEmailAddress:completionHandler:@
fetchShareParticipantWithEmailAddress_completionHandlerSelector :: Selector
fetchShareParticipantWithEmailAddress_completionHandlerSelector = mkSelector "fetchShareParticipantWithEmailAddress:completionHandler:"

-- | @Selector@ for @fetchShareParticipantWithPhoneNumber:completionHandler:@
fetchShareParticipantWithPhoneNumber_completionHandlerSelector :: Selector
fetchShareParticipantWithPhoneNumber_completionHandlerSelector = mkSelector "fetchShareParticipantWithPhoneNumber:completionHandler:"

-- | @Selector@ for @fetchShareParticipantWithUserRecordID:completionHandler:@
fetchShareParticipantWithUserRecordID_completionHandlerSelector :: Selector
fetchShareParticipantWithUserRecordID_completionHandlerSelector = mkSelector "fetchShareParticipantWithUserRecordID:completionHandler:"

-- | @Selector@ for @fetchShareMetadataWithURL:completionHandler:@
fetchShareMetadataWithURL_completionHandlerSelector :: Selector
fetchShareMetadataWithURL_completionHandlerSelector = mkSelector "fetchShareMetadataWithURL:completionHandler:"

-- | @Selector@ for @acceptShareMetadata:completionHandler:@
acceptShareMetadata_completionHandlerSelector :: Selector
acceptShareMetadata_completionHandlerSelector = mkSelector "acceptShareMetadata:completionHandler:"

-- | @Selector@ for @fetchUserRecordIDWithCompletionHandler:@
fetchUserRecordIDWithCompletionHandlerSelector :: Selector
fetchUserRecordIDWithCompletionHandlerSelector = mkSelector "fetchUserRecordIDWithCompletionHandler:"

-- | @Selector@ for @discoverUserIdentityWithEmailAddress:completionHandler:@
discoverUserIdentityWithEmailAddress_completionHandlerSelector :: Selector
discoverUserIdentityWithEmailAddress_completionHandlerSelector = mkSelector "discoverUserIdentityWithEmailAddress:completionHandler:"

-- | @Selector@ for @discoverUserIdentityWithPhoneNumber:completionHandler:@
discoverUserIdentityWithPhoneNumber_completionHandlerSelector :: Selector
discoverUserIdentityWithPhoneNumber_completionHandlerSelector = mkSelector "discoverUserIdentityWithPhoneNumber:completionHandler:"

-- | @Selector@ for @discoverUserIdentityWithUserRecordID:completionHandler:@
discoverUserIdentityWithUserRecordID_completionHandlerSelector :: Selector
discoverUserIdentityWithUserRecordID_completionHandlerSelector = mkSelector "discoverUserIdentityWithUserRecordID:completionHandler:"

-- | @Selector@ for @statusForApplicationPermission:completionHandler:@
statusForApplicationPermission_completionHandlerSelector :: Selector
statusForApplicationPermission_completionHandlerSelector = mkSelector "statusForApplicationPermission:completionHandler:"

-- | @Selector@ for @requestApplicationPermission:completionHandler:@
requestApplicationPermission_completionHandlerSelector :: Selector
requestApplicationPermission_completionHandlerSelector = mkSelector "requestApplicationPermission:completionHandler:"

-- | @Selector@ for @accountStatusWithCompletionHandler:@
accountStatusWithCompletionHandlerSelector :: Selector
accountStatusWithCompletionHandlerSelector = mkSelector "accountStatusWithCompletionHandler:"

-- | @Selector@ for @databaseWithDatabaseScope:@
databaseWithDatabaseScopeSelector :: Selector
databaseWithDatabaseScopeSelector = mkSelector "databaseWithDatabaseScope:"

-- | @Selector@ for @containerIdentifier@
containerIdentifierSelector :: Selector
containerIdentifierSelector = mkSelector "containerIdentifier"

-- | @Selector@ for @privateCloudDatabase@
privateCloudDatabaseSelector :: Selector
privateCloudDatabaseSelector = mkSelector "privateCloudDatabase"

-- | @Selector@ for @publicCloudDatabase@
publicCloudDatabaseSelector :: Selector
publicCloudDatabaseSelector = mkSelector "publicCloudDatabase"

-- | @Selector@ for @sharedCloudDatabase@
sharedCloudDatabaseSelector :: Selector
sharedCloudDatabaseSelector = mkSelector "sharedCloudDatabase"

