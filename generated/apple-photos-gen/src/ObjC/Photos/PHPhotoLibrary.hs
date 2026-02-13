{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHPhotoLibrary@.
module ObjC.Photos.PHPhotoLibrary
  ( PHPhotoLibrary
  , IsPHPhotoLibrary(..)
  , sharedPhotoLibrary
  , authorizationStatusForAccessLevel
  , requestAuthorizationForAccessLevel_handler
  , authorizationStatus
  , requestAuthorization
  , registerAvailabilityObserver
  , unregisterAvailabilityObserver
  , performChanges_completionHandler
  , performChangesAndWait_error
  , registerChangeObserver
  , unregisterChangeObserver
  , fetchPersistentChangesSinceToken_error
  , localIdentifierMappingsForCloudIdentifiers
  , cloudIdentifierMappingsForLocalIdentifiers
  , localIdentifiersForCloudIdentifiers
  , cloudIdentifiersForLocalIdentifiers
  , unavailabilityReason
  , currentChangeToken
  , authorizationStatusForAccessLevelSelector
  , authorizationStatusSelector
  , cloudIdentifierMappingsForLocalIdentifiersSelector
  , cloudIdentifiersForLocalIdentifiersSelector
  , currentChangeTokenSelector
  , fetchPersistentChangesSinceToken_errorSelector
  , localIdentifierMappingsForCloudIdentifiersSelector
  , localIdentifiersForCloudIdentifiersSelector
  , performChangesAndWait_errorSelector
  , performChanges_completionHandlerSelector
  , registerAvailabilityObserverSelector
  , registerChangeObserverSelector
  , requestAuthorizationForAccessLevel_handlerSelector
  , requestAuthorizationSelector
  , sharedPhotoLibrarySelector
  , unavailabilityReasonSelector
  , unregisterAvailabilityObserverSelector
  , unregisterChangeObserverSelector

  -- * Enum types
  , PHAccessLevel(PHAccessLevel)
  , pattern PHAccessLevelAddOnly
  , pattern PHAccessLevelReadWrite
  , PHAuthorizationStatus(PHAuthorizationStatus)
  , pattern PHAuthorizationStatusNotDetermined
  , pattern PHAuthorizationStatusRestricted
  , pattern PHAuthorizationStatusDenied
  , pattern PHAuthorizationStatusAuthorized
  , pattern PHAuthorizationStatusLimited

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ sharedPhotoLibrary@
sharedPhotoLibrary :: IO (Id PHPhotoLibrary)
sharedPhotoLibrary  =
  do
    cls' <- getRequiredClass "PHPhotoLibrary"
    sendClassMessage cls' sharedPhotoLibrarySelector

-- | Replaces @+authorizationStatus@ to support add-only/read-write access level status
--
-- ObjC selector: @+ authorizationStatusForAccessLevel:@
authorizationStatusForAccessLevel :: PHAccessLevel -> IO PHAuthorizationStatus
authorizationStatusForAccessLevel accessLevel =
  do
    cls' <- getRequiredClass "PHPhotoLibrary"
    sendClassMessage cls' authorizationStatusForAccessLevelSelector accessLevel

-- | @+ requestAuthorizationForAccessLevel:handler:@
requestAuthorizationForAccessLevel_handler :: PHAccessLevel -> Ptr () -> IO ()
requestAuthorizationForAccessLevel_handler accessLevel handler =
  do
    cls' <- getRequiredClass "PHPhotoLibrary"
    sendClassMessage cls' requestAuthorizationForAccessLevel_handlerSelector accessLevel handler

-- | Deprecated and replaced by authorizationStatusForAccessLevel:, will return @PHAuthorizationStatusAuthorized@ if the user has chosen limited photo library access
--
-- ObjC selector: @+ authorizationStatus@
authorizationStatus :: IO PHAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "PHPhotoLibrary"
    sendClassMessage cls' authorizationStatusSelector

-- | @+ requestAuthorization:@
requestAuthorization :: Ptr () -> IO ()
requestAuthorization handler =
  do
    cls' <- getRequiredClass "PHPhotoLibrary"
    sendClassMessage cls' requestAuthorizationSelector handler

-- | @- registerAvailabilityObserver:@
registerAvailabilityObserver :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> RawId -> IO ()
registerAvailabilityObserver phPhotoLibrary observer =
  sendMessage phPhotoLibrary registerAvailabilityObserverSelector observer

-- | @- unregisterAvailabilityObserver:@
unregisterAvailabilityObserver :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> RawId -> IO ()
unregisterAvailabilityObserver phPhotoLibrary observer =
  sendMessage phPhotoLibrary unregisterAvailabilityObserverSelector observer

-- | @- performChanges:completionHandler:@
performChanges_completionHandler :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> Ptr () -> Ptr () -> IO ()
performChanges_completionHandler phPhotoLibrary changeBlock completionHandler =
  sendMessage phPhotoLibrary performChanges_completionHandlerSelector changeBlock completionHandler

-- | @- performChangesAndWait:error:@
performChangesAndWait_error :: (IsPHPhotoLibrary phPhotoLibrary, IsNSError error_) => phPhotoLibrary -> Ptr () -> error_ -> IO Bool
performChangesAndWait_error phPhotoLibrary changeBlock error_ =
  sendMessage phPhotoLibrary performChangesAndWait_errorSelector changeBlock (toNSError error_)

-- | @- registerChangeObserver:@
registerChangeObserver :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> RawId -> IO ()
registerChangeObserver phPhotoLibrary observer =
  sendMessage phPhotoLibrary registerChangeObserverSelector observer

-- | @- unregisterChangeObserver:@
unregisterChangeObserver :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> RawId -> IO ()
unregisterChangeObserver phPhotoLibrary observer =
  sendMessage phPhotoLibrary unregisterChangeObserverSelector observer

-- | @- fetchPersistentChangesSinceToken:error:@
fetchPersistentChangesSinceToken_error :: (IsPHPhotoLibrary phPhotoLibrary, IsPHPersistentChangeToken token, IsNSError error_) => phPhotoLibrary -> token -> error_ -> IO (Id PHPersistentChangeFetchResult)
fetchPersistentChangesSinceToken_error phPhotoLibrary token error_ =
  sendMessage phPhotoLibrary fetchPersistentChangesSinceToken_errorSelector (toPHPersistentChangeToken token) (toNSError error_)

-- | Returns a dictionary that maps each cloud identifier from the provided array to a PLLocalIdentifierMapping result containing the local identifier found for that cloud identifier.
--
-- This method can be very expensive so they should be used sparingly for batch lookup of all needed identifiers. Clients should work in terms of local identifiers and call these methods only once after loading from and before saving to persistent storage.  If the attempt to lookup a local identifier for a given cloud identifier fails, the error parameter will indicate the reason.
--
-- @cloudIdentifiers@ — The array of @PHCloudIdentifier@ instances whose local identifiers are to being requested.
--
-- ObjC selector: @- localIdentifierMappingsForCloudIdentifiers:@
localIdentifierMappingsForCloudIdentifiers :: (IsPHPhotoLibrary phPhotoLibrary, IsNSArray cloudIdentifiers) => phPhotoLibrary -> cloudIdentifiers -> IO (Id NSDictionary)
localIdentifierMappingsForCloudIdentifiers phPhotoLibrary cloudIdentifiers =
  sendMessage phPhotoLibrary localIdentifierMappingsForCloudIdentifiersSelector (toNSArray cloudIdentifiers)

-- | Returns a dictionary that maps each local identifier from the provided array to a PLCloudIdentifierMapping result containing the cloud identifier found for that local identifier
--
-- This method can be very expensive so they should be used sparingly for batch lookup of all needed identifiers. Clients should work in terms of local identifiers and call these methods only once after loading from and before saving to persistent storage.  If the attempt to lookup a cloud identifier for a given local identifier fails, the error parameter will indicate the reason.
--
-- @localIdentifiers@ — The array of @NSString@ instances whose cloud identifiers are to being requested.
--
-- ObjC selector: @- cloudIdentifierMappingsForLocalIdentifiers:@
cloudIdentifierMappingsForLocalIdentifiers :: (IsPHPhotoLibrary phPhotoLibrary, IsNSArray localIdentifiers) => phPhotoLibrary -> localIdentifiers -> IO (Id NSDictionary)
cloudIdentifierMappingsForLocalIdentifiers phPhotoLibrary localIdentifiers =
  sendMessage phPhotoLibrary cloudIdentifierMappingsForLocalIdentifiersSelector (toNSArray localIdentifiers)

-- | DEPRECATED: These two methods can be very expensive so they should be used sparingly for batch lookup of all needed identifiers. Clients should work in terms of local identifiers and call these methods only once after loading from and before saving to persistent storage.
--
-- ObjC selector: @- localIdentifiersForCloudIdentifiers:@
localIdentifiersForCloudIdentifiers :: (IsPHPhotoLibrary phPhotoLibrary, IsNSArray cloudIdentifiers) => phPhotoLibrary -> cloudIdentifiers -> IO (Id NSArray)
localIdentifiersForCloudIdentifiers phPhotoLibrary cloudIdentifiers =
  sendMessage phPhotoLibrary localIdentifiersForCloudIdentifiersSelector (toNSArray cloudIdentifiers)

-- | @- cloudIdentifiersForLocalIdentifiers:@
cloudIdentifiersForLocalIdentifiers :: (IsPHPhotoLibrary phPhotoLibrary, IsNSArray localIdentifiers) => phPhotoLibrary -> localIdentifiers -> IO (Id NSArray)
cloudIdentifiersForLocalIdentifiers phPhotoLibrary localIdentifiers =
  sendMessage phPhotoLibrary cloudIdentifiersForLocalIdentifiersSelector (toNSArray localIdentifiers)

-- | @- unavailabilityReason@
unavailabilityReason :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> IO (Id NSError)
unavailabilityReason phPhotoLibrary =
  sendMessage phPhotoLibrary unavailabilityReasonSelector

-- | @- currentChangeToken@
currentChangeToken :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> IO (Id PHPersistentChangeToken)
currentChangeToken phPhotoLibrary =
  sendMessage phPhotoLibrary currentChangeTokenSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedPhotoLibrary@
sharedPhotoLibrarySelector :: Selector '[] (Id PHPhotoLibrary)
sharedPhotoLibrarySelector = mkSelector "sharedPhotoLibrary"

-- | @Selector@ for @authorizationStatusForAccessLevel:@
authorizationStatusForAccessLevelSelector :: Selector '[PHAccessLevel] PHAuthorizationStatus
authorizationStatusForAccessLevelSelector = mkSelector "authorizationStatusForAccessLevel:"

-- | @Selector@ for @requestAuthorizationForAccessLevel:handler:@
requestAuthorizationForAccessLevel_handlerSelector :: Selector '[PHAccessLevel, Ptr ()] ()
requestAuthorizationForAccessLevel_handlerSelector = mkSelector "requestAuthorizationForAccessLevel:handler:"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] PHAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @requestAuthorization:@
requestAuthorizationSelector :: Selector '[Ptr ()] ()
requestAuthorizationSelector = mkSelector "requestAuthorization:"

-- | @Selector@ for @registerAvailabilityObserver:@
registerAvailabilityObserverSelector :: Selector '[RawId] ()
registerAvailabilityObserverSelector = mkSelector "registerAvailabilityObserver:"

-- | @Selector@ for @unregisterAvailabilityObserver:@
unregisterAvailabilityObserverSelector :: Selector '[RawId] ()
unregisterAvailabilityObserverSelector = mkSelector "unregisterAvailabilityObserver:"

-- | @Selector@ for @performChanges:completionHandler:@
performChanges_completionHandlerSelector :: Selector '[Ptr (), Ptr ()] ()
performChanges_completionHandlerSelector = mkSelector "performChanges:completionHandler:"

-- | @Selector@ for @performChangesAndWait:error:@
performChangesAndWait_errorSelector :: Selector '[Ptr (), Id NSError] Bool
performChangesAndWait_errorSelector = mkSelector "performChangesAndWait:error:"

-- | @Selector@ for @registerChangeObserver:@
registerChangeObserverSelector :: Selector '[RawId] ()
registerChangeObserverSelector = mkSelector "registerChangeObserver:"

-- | @Selector@ for @unregisterChangeObserver:@
unregisterChangeObserverSelector :: Selector '[RawId] ()
unregisterChangeObserverSelector = mkSelector "unregisterChangeObserver:"

-- | @Selector@ for @fetchPersistentChangesSinceToken:error:@
fetchPersistentChangesSinceToken_errorSelector :: Selector '[Id PHPersistentChangeToken, Id NSError] (Id PHPersistentChangeFetchResult)
fetchPersistentChangesSinceToken_errorSelector = mkSelector "fetchPersistentChangesSinceToken:error:"

-- | @Selector@ for @localIdentifierMappingsForCloudIdentifiers:@
localIdentifierMappingsForCloudIdentifiersSelector :: Selector '[Id NSArray] (Id NSDictionary)
localIdentifierMappingsForCloudIdentifiersSelector = mkSelector "localIdentifierMappingsForCloudIdentifiers:"

-- | @Selector@ for @cloudIdentifierMappingsForLocalIdentifiers:@
cloudIdentifierMappingsForLocalIdentifiersSelector :: Selector '[Id NSArray] (Id NSDictionary)
cloudIdentifierMappingsForLocalIdentifiersSelector = mkSelector "cloudIdentifierMappingsForLocalIdentifiers:"

-- | @Selector@ for @localIdentifiersForCloudIdentifiers:@
localIdentifiersForCloudIdentifiersSelector :: Selector '[Id NSArray] (Id NSArray)
localIdentifiersForCloudIdentifiersSelector = mkSelector "localIdentifiersForCloudIdentifiers:"

-- | @Selector@ for @cloudIdentifiersForLocalIdentifiers:@
cloudIdentifiersForLocalIdentifiersSelector :: Selector '[Id NSArray] (Id NSArray)
cloudIdentifiersForLocalIdentifiersSelector = mkSelector "cloudIdentifiersForLocalIdentifiers:"

-- | @Selector@ for @unavailabilityReason@
unavailabilityReasonSelector :: Selector '[] (Id NSError)
unavailabilityReasonSelector = mkSelector "unavailabilityReason"

-- | @Selector@ for @currentChangeToken@
currentChangeTokenSelector :: Selector '[] (Id PHPersistentChangeToken)
currentChangeTokenSelector = mkSelector "currentChangeToken"

