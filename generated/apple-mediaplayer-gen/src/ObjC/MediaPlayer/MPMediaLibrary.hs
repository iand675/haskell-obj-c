{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMediaLibrary@.
module ObjC.MediaPlayer.MPMediaLibrary
  ( MPMediaLibrary
  , IsMPMediaLibrary(..)
  , defaultMediaLibrary
  , beginGeneratingLibraryChangeNotifications
  , endGeneratingLibraryChangeNotifications
  , authorizationStatus
  , requestAuthorization
  , getPlaylistWithUUID_creationMetadata_completionHandler
  , lastModifiedDate
  , authorizationStatusSelector
  , beginGeneratingLibraryChangeNotificationsSelector
  , defaultMediaLibrarySelector
  , endGeneratingLibraryChangeNotificationsSelector
  , getPlaylistWithUUID_creationMetadata_completionHandlerSelector
  , lastModifiedDateSelector
  , requestAuthorizationSelector

  -- * Enum types
  , MPMediaLibraryAuthorizationStatus(MPMediaLibraryAuthorizationStatus)
  , pattern MPMediaLibraryAuthorizationStatusNotDetermined
  , pattern MPMediaLibraryAuthorizationStatusDenied
  , pattern MPMediaLibraryAuthorizationStatusRestricted
  , pattern MPMediaLibraryAuthorizationStatusAuthorized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ defaultMediaLibrary@
defaultMediaLibrary :: IO (Id MPMediaLibrary)
defaultMediaLibrary  =
  do
    cls' <- getRequiredClass "MPMediaLibrary"
    sendClassMessage cls' defaultMediaLibrarySelector

-- | @- beginGeneratingLibraryChangeNotifications@
beginGeneratingLibraryChangeNotifications :: IsMPMediaLibrary mpMediaLibrary => mpMediaLibrary -> IO ()
beginGeneratingLibraryChangeNotifications mpMediaLibrary =
  sendMessage mpMediaLibrary beginGeneratingLibraryChangeNotificationsSelector

-- | @- endGeneratingLibraryChangeNotifications@
endGeneratingLibraryChangeNotifications :: IsMPMediaLibrary mpMediaLibrary => mpMediaLibrary -> IO ()
endGeneratingLibraryChangeNotifications mpMediaLibrary =
  sendMessage mpMediaLibrary endGeneratingLibraryChangeNotificationsSelector

-- | @+ authorizationStatus@
authorizationStatus :: IO MPMediaLibraryAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "MPMediaLibrary"
    sendClassMessage cls' authorizationStatusSelector

-- | @+ requestAuthorization:@
requestAuthorization :: Ptr () -> IO ()
requestAuthorization completionHandler =
  do
    cls' <- getRequiredClass "MPMediaLibrary"
    sendClassMessage cls' requestAuthorizationSelector completionHandler

-- | Finds the playlist associated with the UUID. If the playlist exists, the creation metadata is ignored. If no such playlist exists and creation metadata is valid, a playlist associated the UUID will be created.
--
-- The UUID should typically be pre-generated to avoid creating a new playlist with every call.
--
-- ObjC selector: @- getPlaylistWithUUID:creationMetadata:completionHandler:@
getPlaylistWithUUID_creationMetadata_completionHandler :: (IsMPMediaLibrary mpMediaLibrary, IsNSUUID uuid, IsMPMediaPlaylistCreationMetadata creationMetadata) => mpMediaLibrary -> uuid -> creationMetadata -> Ptr () -> IO ()
getPlaylistWithUUID_creationMetadata_completionHandler mpMediaLibrary uuid creationMetadata completionHandler =
  sendMessage mpMediaLibrary getPlaylistWithUUID_creationMetadata_completionHandlerSelector (toNSUUID uuid) (toMPMediaPlaylistCreationMetadata creationMetadata) completionHandler

-- | @- lastModifiedDate@
lastModifiedDate :: IsMPMediaLibrary mpMediaLibrary => mpMediaLibrary -> IO (Id NSDate)
lastModifiedDate mpMediaLibrary =
  sendMessage mpMediaLibrary lastModifiedDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultMediaLibrary@
defaultMediaLibrarySelector :: Selector '[] (Id MPMediaLibrary)
defaultMediaLibrarySelector = mkSelector "defaultMediaLibrary"

-- | @Selector@ for @beginGeneratingLibraryChangeNotifications@
beginGeneratingLibraryChangeNotificationsSelector :: Selector '[] ()
beginGeneratingLibraryChangeNotificationsSelector = mkSelector "beginGeneratingLibraryChangeNotifications"

-- | @Selector@ for @endGeneratingLibraryChangeNotifications@
endGeneratingLibraryChangeNotificationsSelector :: Selector '[] ()
endGeneratingLibraryChangeNotificationsSelector = mkSelector "endGeneratingLibraryChangeNotifications"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] MPMediaLibraryAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @requestAuthorization:@
requestAuthorizationSelector :: Selector '[Ptr ()] ()
requestAuthorizationSelector = mkSelector "requestAuthorization:"

-- | @Selector@ for @getPlaylistWithUUID:creationMetadata:completionHandler:@
getPlaylistWithUUID_creationMetadata_completionHandlerSelector :: Selector '[Id NSUUID, Id MPMediaPlaylistCreationMetadata, Ptr ()] ()
getPlaylistWithUUID_creationMetadata_completionHandlerSelector = mkSelector "getPlaylistWithUUID:creationMetadata:completionHandler:"

-- | @Selector@ for @lastModifiedDate@
lastModifiedDateSelector :: Selector '[] (Id NSDate)
lastModifiedDateSelector = mkSelector "lastModifiedDate"

