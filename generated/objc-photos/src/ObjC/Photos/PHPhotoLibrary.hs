{-# LANGUAGE PatternSynonyms #-}
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
  , sharedPhotoLibrarySelector
  , authorizationStatusForAccessLevelSelector
  , requestAuthorizationForAccessLevel_handlerSelector
  , authorizationStatusSelector
  , requestAuthorizationSelector
  , registerAvailabilityObserverSelector
  , unregisterAvailabilityObserverSelector
  , performChanges_completionHandlerSelector
  , performChangesAndWait_errorSelector
  , registerChangeObserverSelector
  , unregisterChangeObserverSelector
  , fetchPersistentChangesSinceToken_errorSelector
  , localIdentifierMappingsForCloudIdentifiersSelector
  , cloudIdentifierMappingsForLocalIdentifiersSelector
  , localIdentifiersForCloudIdentifiersSelector
  , cloudIdentifiersForLocalIdentifiersSelector
  , unavailabilityReasonSelector
  , currentChangeTokenSelector

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

import ObjC.Photos.Internal.Classes
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ sharedPhotoLibrary@
sharedPhotoLibrary :: IO (Id PHPhotoLibrary)
sharedPhotoLibrary  =
  do
    cls' <- getRequiredClass "PHPhotoLibrary"
    sendClassMsg cls' (mkSelector "sharedPhotoLibrary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Replaces @+authorizationStatus@ to support add-only/read-write access level status
--
-- ObjC selector: @+ authorizationStatusForAccessLevel:@
authorizationStatusForAccessLevel :: PHAccessLevel -> IO PHAuthorizationStatus
authorizationStatusForAccessLevel accessLevel =
  do
    cls' <- getRequiredClass "PHPhotoLibrary"
    fmap (coerce :: CLong -> PHAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatusForAccessLevel:") retCLong [argCLong (coerce accessLevel)]

-- | @+ requestAuthorizationForAccessLevel:handler:@
requestAuthorizationForAccessLevel_handler :: PHAccessLevel -> Ptr () -> IO ()
requestAuthorizationForAccessLevel_handler accessLevel handler =
  do
    cls' <- getRequiredClass "PHPhotoLibrary"
    sendClassMsg cls' (mkSelector "requestAuthorizationForAccessLevel:handler:") retVoid [argCLong (coerce accessLevel), argPtr (castPtr handler :: Ptr ())]

-- | Deprecated and replaced by authorizationStatusForAccessLevel:, will return @PHAuthorizationStatusAuthorized@ if the user has chosen limited photo library access
--
-- ObjC selector: @+ authorizationStatus@
authorizationStatus :: IO PHAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "PHPhotoLibrary"
    fmap (coerce :: CLong -> PHAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- | @+ requestAuthorization:@
requestAuthorization :: Ptr () -> IO ()
requestAuthorization handler =
  do
    cls' <- getRequiredClass "PHPhotoLibrary"
    sendClassMsg cls' (mkSelector "requestAuthorization:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @- registerAvailabilityObserver:@
registerAvailabilityObserver :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> RawId -> IO ()
registerAvailabilityObserver phPhotoLibrary  observer =
  sendMsg phPhotoLibrary (mkSelector "registerAvailabilityObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | @- unregisterAvailabilityObserver:@
unregisterAvailabilityObserver :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> RawId -> IO ()
unregisterAvailabilityObserver phPhotoLibrary  observer =
  sendMsg phPhotoLibrary (mkSelector "unregisterAvailabilityObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | @- performChanges:completionHandler:@
performChanges_completionHandler :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> Ptr () -> Ptr () -> IO ()
performChanges_completionHandler phPhotoLibrary  changeBlock completionHandler =
  sendMsg phPhotoLibrary (mkSelector "performChanges:completionHandler:") retVoid [argPtr (castPtr changeBlock :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- performChangesAndWait:error:@
performChangesAndWait_error :: (IsPHPhotoLibrary phPhotoLibrary, IsNSError error_) => phPhotoLibrary -> Ptr () -> error_ -> IO Bool
performChangesAndWait_error phPhotoLibrary  changeBlock error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg phPhotoLibrary (mkSelector "performChangesAndWait:error:") retCULong [argPtr (castPtr changeBlock :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- registerChangeObserver:@
registerChangeObserver :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> RawId -> IO ()
registerChangeObserver phPhotoLibrary  observer =
  sendMsg phPhotoLibrary (mkSelector "registerChangeObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | @- unregisterChangeObserver:@
unregisterChangeObserver :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> RawId -> IO ()
unregisterChangeObserver phPhotoLibrary  observer =
  sendMsg phPhotoLibrary (mkSelector "unregisterChangeObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | @- fetchPersistentChangesSinceToken:error:@
fetchPersistentChangesSinceToken_error :: (IsPHPhotoLibrary phPhotoLibrary, IsPHPersistentChangeToken token, IsNSError error_) => phPhotoLibrary -> token -> error_ -> IO (Id PHPersistentChangeFetchResult)
fetchPersistentChangesSinceToken_error phPhotoLibrary  token error_ =
withObjCPtr token $ \raw_token ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg phPhotoLibrary (mkSelector "fetchPersistentChangesSinceToken:error:") (retPtr retVoid) [argPtr (castPtr raw_token :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a dictionary that maps each cloud identifier from the provided array to a PLLocalIdentifierMapping result containing the local identifier found for that cloud identifier.
--
-- This method can be very expensive so they should be used sparingly for batch lookup of all needed identifiers. Clients should work in terms of local identifiers and call these methods only once after loading from and before saving to persistent storage.  If the attempt to lookup a local identifier for a given cloud identifier fails, the error parameter will indicate the reason.
--
-- @cloudIdentifiers@ — The array of @PHCloudIdentifier@ instances whose local identifiers are to being requested.
--
-- ObjC selector: @- localIdentifierMappingsForCloudIdentifiers:@
localIdentifierMappingsForCloudIdentifiers :: (IsPHPhotoLibrary phPhotoLibrary, IsNSArray cloudIdentifiers) => phPhotoLibrary -> cloudIdentifiers -> IO (Id NSDictionary)
localIdentifierMappingsForCloudIdentifiers phPhotoLibrary  cloudIdentifiers =
withObjCPtr cloudIdentifiers $ \raw_cloudIdentifiers ->
    sendMsg phPhotoLibrary (mkSelector "localIdentifierMappingsForCloudIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_cloudIdentifiers :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a dictionary that maps each local identifier from the provided array to a PLCloudIdentifierMapping result containing the cloud identifier found for that local identifier
--
-- This method can be very expensive so they should be used sparingly for batch lookup of all needed identifiers. Clients should work in terms of local identifiers and call these methods only once after loading from and before saving to persistent storage.  If the attempt to lookup a cloud identifier for a given local identifier fails, the error parameter will indicate the reason.
--
-- @localIdentifiers@ — The array of @NSString@ instances whose cloud identifiers are to being requested.
--
-- ObjC selector: @- cloudIdentifierMappingsForLocalIdentifiers:@
cloudIdentifierMappingsForLocalIdentifiers :: (IsPHPhotoLibrary phPhotoLibrary, IsNSArray localIdentifiers) => phPhotoLibrary -> localIdentifiers -> IO (Id NSDictionary)
cloudIdentifierMappingsForLocalIdentifiers phPhotoLibrary  localIdentifiers =
withObjCPtr localIdentifiers $ \raw_localIdentifiers ->
    sendMsg phPhotoLibrary (mkSelector "cloudIdentifierMappingsForLocalIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_localIdentifiers :: Ptr ())] >>= retainedObject . castPtr

-- | DEPRECATED: These two methods can be very expensive so they should be used sparingly for batch lookup of all needed identifiers. Clients should work in terms of local identifiers and call these methods only once after loading from and before saving to persistent storage.
--
-- ObjC selector: @- localIdentifiersForCloudIdentifiers:@
localIdentifiersForCloudIdentifiers :: (IsPHPhotoLibrary phPhotoLibrary, IsNSArray cloudIdentifiers) => phPhotoLibrary -> cloudIdentifiers -> IO (Id NSArray)
localIdentifiersForCloudIdentifiers phPhotoLibrary  cloudIdentifiers =
withObjCPtr cloudIdentifiers $ \raw_cloudIdentifiers ->
    sendMsg phPhotoLibrary (mkSelector "localIdentifiersForCloudIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_cloudIdentifiers :: Ptr ())] >>= retainedObject . castPtr

-- | @- cloudIdentifiersForLocalIdentifiers:@
cloudIdentifiersForLocalIdentifiers :: (IsPHPhotoLibrary phPhotoLibrary, IsNSArray localIdentifiers) => phPhotoLibrary -> localIdentifiers -> IO (Id NSArray)
cloudIdentifiersForLocalIdentifiers phPhotoLibrary  localIdentifiers =
withObjCPtr localIdentifiers $ \raw_localIdentifiers ->
    sendMsg phPhotoLibrary (mkSelector "cloudIdentifiersForLocalIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_localIdentifiers :: Ptr ())] >>= retainedObject . castPtr

-- | @- unavailabilityReason@
unavailabilityReason :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> IO (Id NSError)
unavailabilityReason phPhotoLibrary  =
  sendMsg phPhotoLibrary (mkSelector "unavailabilityReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentChangeToken@
currentChangeToken :: IsPHPhotoLibrary phPhotoLibrary => phPhotoLibrary -> IO (Id PHPersistentChangeToken)
currentChangeToken phPhotoLibrary  =
  sendMsg phPhotoLibrary (mkSelector "currentChangeToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedPhotoLibrary@
sharedPhotoLibrarySelector :: Selector
sharedPhotoLibrarySelector = mkSelector "sharedPhotoLibrary"

-- | @Selector@ for @authorizationStatusForAccessLevel:@
authorizationStatusForAccessLevelSelector :: Selector
authorizationStatusForAccessLevelSelector = mkSelector "authorizationStatusForAccessLevel:"

-- | @Selector@ for @requestAuthorizationForAccessLevel:handler:@
requestAuthorizationForAccessLevel_handlerSelector :: Selector
requestAuthorizationForAccessLevel_handlerSelector = mkSelector "requestAuthorizationForAccessLevel:handler:"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @requestAuthorization:@
requestAuthorizationSelector :: Selector
requestAuthorizationSelector = mkSelector "requestAuthorization:"

-- | @Selector@ for @registerAvailabilityObserver:@
registerAvailabilityObserverSelector :: Selector
registerAvailabilityObserverSelector = mkSelector "registerAvailabilityObserver:"

-- | @Selector@ for @unregisterAvailabilityObserver:@
unregisterAvailabilityObserverSelector :: Selector
unregisterAvailabilityObserverSelector = mkSelector "unregisterAvailabilityObserver:"

-- | @Selector@ for @performChanges:completionHandler:@
performChanges_completionHandlerSelector :: Selector
performChanges_completionHandlerSelector = mkSelector "performChanges:completionHandler:"

-- | @Selector@ for @performChangesAndWait:error:@
performChangesAndWait_errorSelector :: Selector
performChangesAndWait_errorSelector = mkSelector "performChangesAndWait:error:"

-- | @Selector@ for @registerChangeObserver:@
registerChangeObserverSelector :: Selector
registerChangeObserverSelector = mkSelector "registerChangeObserver:"

-- | @Selector@ for @unregisterChangeObserver:@
unregisterChangeObserverSelector :: Selector
unregisterChangeObserverSelector = mkSelector "unregisterChangeObserver:"

-- | @Selector@ for @fetchPersistentChangesSinceToken:error:@
fetchPersistentChangesSinceToken_errorSelector :: Selector
fetchPersistentChangesSinceToken_errorSelector = mkSelector "fetchPersistentChangesSinceToken:error:"

-- | @Selector@ for @localIdentifierMappingsForCloudIdentifiers:@
localIdentifierMappingsForCloudIdentifiersSelector :: Selector
localIdentifierMappingsForCloudIdentifiersSelector = mkSelector "localIdentifierMappingsForCloudIdentifiers:"

-- | @Selector@ for @cloudIdentifierMappingsForLocalIdentifiers:@
cloudIdentifierMappingsForLocalIdentifiersSelector :: Selector
cloudIdentifierMappingsForLocalIdentifiersSelector = mkSelector "cloudIdentifierMappingsForLocalIdentifiers:"

-- | @Selector@ for @localIdentifiersForCloudIdentifiers:@
localIdentifiersForCloudIdentifiersSelector :: Selector
localIdentifiersForCloudIdentifiersSelector = mkSelector "localIdentifiersForCloudIdentifiers:"

-- | @Selector@ for @cloudIdentifiersForLocalIdentifiers:@
cloudIdentifiersForLocalIdentifiersSelector :: Selector
cloudIdentifiersForLocalIdentifiersSelector = mkSelector "cloudIdentifiersForLocalIdentifiers:"

-- | @Selector@ for @unavailabilityReason@
unavailabilityReasonSelector :: Selector
unavailabilityReasonSelector = mkSelector "unavailabilityReason"

-- | @Selector@ for @currentChangeToken@
currentChangeTokenSelector :: Selector
currentChangeTokenSelector = mkSelector "currentChangeToken"

