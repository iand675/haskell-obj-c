{-# LANGUAGE PatternSynonyms #-}
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
  , defaultMediaLibrarySelector
  , beginGeneratingLibraryChangeNotificationsSelector
  , endGeneratingLibraryChangeNotificationsSelector
  , authorizationStatusSelector
  , requestAuthorizationSelector
  , getPlaylistWithUUID_creationMetadata_completionHandlerSelector
  , lastModifiedDateSelector

  -- * Enum types
  , MPMediaLibraryAuthorizationStatus(MPMediaLibraryAuthorizationStatus)
  , pattern MPMediaLibraryAuthorizationStatusNotDetermined
  , pattern MPMediaLibraryAuthorizationStatusDenied
  , pattern MPMediaLibraryAuthorizationStatusRestricted
  , pattern MPMediaLibraryAuthorizationStatusAuthorized

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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ defaultMediaLibrary@
defaultMediaLibrary :: IO (Id MPMediaLibrary)
defaultMediaLibrary  =
  do
    cls' <- getRequiredClass "MPMediaLibrary"
    sendClassMsg cls' (mkSelector "defaultMediaLibrary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- beginGeneratingLibraryChangeNotifications@
beginGeneratingLibraryChangeNotifications :: IsMPMediaLibrary mpMediaLibrary => mpMediaLibrary -> IO ()
beginGeneratingLibraryChangeNotifications mpMediaLibrary  =
  sendMsg mpMediaLibrary (mkSelector "beginGeneratingLibraryChangeNotifications") retVoid []

-- | @- endGeneratingLibraryChangeNotifications@
endGeneratingLibraryChangeNotifications :: IsMPMediaLibrary mpMediaLibrary => mpMediaLibrary -> IO ()
endGeneratingLibraryChangeNotifications mpMediaLibrary  =
  sendMsg mpMediaLibrary (mkSelector "endGeneratingLibraryChangeNotifications") retVoid []

-- | @+ authorizationStatus@
authorizationStatus :: IO MPMediaLibraryAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "MPMediaLibrary"
    fmap (coerce :: CLong -> MPMediaLibraryAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- | @+ requestAuthorization:@
requestAuthorization :: Ptr () -> IO ()
requestAuthorization completionHandler =
  do
    cls' <- getRequiredClass "MPMediaLibrary"
    sendClassMsg cls' (mkSelector "requestAuthorization:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Finds the playlist associated with the UUID. If the playlist exists, the creation metadata is ignored. If no such playlist exists and creation metadata is valid, a playlist associated the UUID will be created.
--
-- The UUID should typically be pre-generated to avoid creating a new playlist with every call.
--
-- ObjC selector: @- getPlaylistWithUUID:creationMetadata:completionHandler:@
getPlaylistWithUUID_creationMetadata_completionHandler :: (IsMPMediaLibrary mpMediaLibrary, IsNSUUID uuid, IsMPMediaPlaylistCreationMetadata creationMetadata) => mpMediaLibrary -> uuid -> creationMetadata -> Ptr () -> IO ()
getPlaylistWithUUID_creationMetadata_completionHandler mpMediaLibrary  uuid creationMetadata completionHandler =
withObjCPtr uuid $ \raw_uuid ->
  withObjCPtr creationMetadata $ \raw_creationMetadata ->
      sendMsg mpMediaLibrary (mkSelector "getPlaylistWithUUID:creationMetadata:completionHandler:") retVoid [argPtr (castPtr raw_uuid :: Ptr ()), argPtr (castPtr raw_creationMetadata :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- lastModifiedDate@
lastModifiedDate :: IsMPMediaLibrary mpMediaLibrary => mpMediaLibrary -> IO (Id NSDate)
lastModifiedDate mpMediaLibrary  =
  sendMsg mpMediaLibrary (mkSelector "lastModifiedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultMediaLibrary@
defaultMediaLibrarySelector :: Selector
defaultMediaLibrarySelector = mkSelector "defaultMediaLibrary"

-- | @Selector@ for @beginGeneratingLibraryChangeNotifications@
beginGeneratingLibraryChangeNotificationsSelector :: Selector
beginGeneratingLibraryChangeNotificationsSelector = mkSelector "beginGeneratingLibraryChangeNotifications"

-- | @Selector@ for @endGeneratingLibraryChangeNotifications@
endGeneratingLibraryChangeNotificationsSelector :: Selector
endGeneratingLibraryChangeNotificationsSelector = mkSelector "endGeneratingLibraryChangeNotifications"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @requestAuthorization:@
requestAuthorizationSelector :: Selector
requestAuthorizationSelector = mkSelector "requestAuthorization:"

-- | @Selector@ for @getPlaylistWithUUID:creationMetadata:completionHandler:@
getPlaylistWithUUID_creationMetadata_completionHandlerSelector :: Selector
getPlaylistWithUUID_creationMetadata_completionHandlerSelector = mkSelector "getPlaylistWithUUID:creationMetadata:completionHandler:"

-- | @Selector@ for @lastModifiedDate@
lastModifiedDateSelector :: Selector
lastModifiedDateSelector = mkSelector "lastModifiedDate"

