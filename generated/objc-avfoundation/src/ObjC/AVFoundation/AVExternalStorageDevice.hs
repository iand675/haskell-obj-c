{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVExternalStorageDevice
--
-- An AVExternalStorageDevice represents a physical external storage device connected to the device that can be used to store captured media assets.
--
-- Each instance of AVExternalStorageDevice corresponds to a physical external storage device where captured media assets can be stored. Instances of AVExternalStorageDevice cannot be created directly. An array of all currently available external storage devices can be obtained using AVExternalStorageDeviceDiscoverySession.
--
-- Instances of AVExternalStorageDevice can be used with AVCaptureFileOutput subclasses for writing media files.
--
-- Generated bindings for @AVExternalStorageDevice@.
module ObjC.AVFoundation.AVExternalStorageDevice
  ( AVExternalStorageDevice
  , IsAVExternalStorageDevice(..)
  , init_
  , new
  , nextAvailableURLsWithPathExtensions_error
  , requestAccessWithCompletionHandler
  , displayName
  , freeSize
  , totalSize
  , connected
  , uuid
  , notRecommendedForCaptureUse
  , authorizationStatus
  , initSelector
  , newSelector
  , nextAvailableURLsWithPathExtensions_errorSelector
  , requestAccessWithCompletionHandlerSelector
  , displayNameSelector
  , freeSizeSelector
  , totalSizeSelector
  , connectedSelector
  , uuidSelector
  , notRecommendedForCaptureUseSelector
  , authorizationStatusSelector

  -- * Enum types
  , AVAuthorizationStatus(AVAuthorizationStatus)
  , pattern AVAuthorizationStatusNotDetermined
  , pattern AVAuthorizationStatusRestricted
  , pattern AVAuthorizationStatusDenied
  , pattern AVAuthorizationStatusAuthorized

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

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVExternalStorageDevice avExternalStorageDevice => avExternalStorageDevice -> IO (Id AVExternalStorageDevice)
init_ avExternalStorageDevice  =
  sendMsg avExternalStorageDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVExternalStorageDevice)
new  =
  do
    cls' <- getRequiredClass "AVExternalStorageDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | nextAvailableURLsWithPathExtensions:error:
--
-- Next available security-scoped, DCF compliant URL array with different path extensions.
--
-- @extensionArray@ — An array of path extensions for the next available URL requested.
--
-- @outError@ — An out parameter with error information indicating why the URL could not be provided. If this method is successful, error will be nil.
--
-- Returns: An array of DCF compliant security-scoped URL with all the path extensions requested.
--
-- Configures the folder structure (create a DCIM folder if there isn't one already) on the external storage device to provide the next available unique DCF compliant security-scoped URL array with different path extensions.
--
-- Security-scoped URL requires the use of startAccessingSecurityScopedResource, and stopAccessingSecurityScopedResource for access.	[nextAvailableURL startAccessingSecurityScopedResource];	. . .	// your code to capture image / video	. . .	[nextAvailableURL stopAccessingSecurityScopedResource];
--
-- Use the +requestAccessWithCompletionHandler: method to request access to external storage device before getting the next available URL array else an error will be thrown.
--
-- ObjC selector: @- nextAvailableURLsWithPathExtensions:error:@
nextAvailableURLsWithPathExtensions_error :: (IsAVExternalStorageDevice avExternalStorageDevice, IsNSArray extensionArray, IsNSError outError) => avExternalStorageDevice -> extensionArray -> outError -> IO (Id NSArray)
nextAvailableURLsWithPathExtensions_error avExternalStorageDevice  extensionArray outError =
withObjCPtr extensionArray $ \raw_extensionArray ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg avExternalStorageDevice (mkSelector "nextAvailableURLsWithPathExtensions:error:") (retPtr retVoid) [argPtr (castPtr raw_extensionArray :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | requestAccessWithCompletionHandler:
--
-- Requests access to capture onto an external storage device connected to this device, showing a dialog to the user if necessary.
--
-- @handler@ — A completion handler block called with the result of requesting access to capture onto an external storage device.
--
-- Use this method to request access to capture onto an external storage device connected to this device.
--
-- This call will not block while the user is being asked for access, allowing the client to continue running. Until access has been granted, trying to capture into detected external storage devices will result in an error. The user is only asked for permission the first time the client requests access, later calls use the authorization status selected by the user.
--
-- The completion handler is called on an arbitrary dispatch queue. It is the client's responsibility to ensure that any UIKit-related updates are called on the main queue or main thread as a result.
--
-- ObjC selector: @+ requestAccessWithCompletionHandler:@
requestAccessWithCompletionHandler :: Ptr () -> IO ()
requestAccessWithCompletionHandler handler =
  do
    cls' <- getRequiredClass "AVExternalStorageDevice"
    sendClassMsg cls' (mkSelector "requestAccessWithCompletionHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | displayName
--
-- Display name of the external storage device.
--
-- This property can be used for displaying the name of an external storage device in a user interface. Will return nil if we fail to extract information from external storage device.
--
-- ObjC selector: @- displayName@
displayName :: IsAVExternalStorageDevice avExternalStorageDevice => avExternalStorageDevice -> IO (Id NSString)
displayName avExternalStorageDevice  =
  sendMsg avExternalStorageDevice (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | freeSize
--
-- Current free size in bytes.
--
-- This property represents the free size available on the external storage device. Will return -1 if we fail to extract information from external storage device.
--
-- ObjC selector: @- freeSize@
freeSize :: IsAVExternalStorageDevice avExternalStorageDevice => avExternalStorageDevice -> IO CLong
freeSize avExternalStorageDevice  =
  sendMsg avExternalStorageDevice (mkSelector "freeSize") retCLong []

-- | totalSize
--
-- Total storage size in bytes.
--
-- This property represents the total storage size available on the external storage device. Will return -1 if we fail to extract information from external storage device.
--
-- ObjC selector: @- totalSize@
totalSize :: IsAVExternalStorageDevice avExternalStorageDevice => avExternalStorageDevice -> IO CLong
totalSize avExternalStorageDevice  =
  sendMsg avExternalStorageDevice (mkSelector "totalSize") retCLong []

-- | connected
--
-- Indicates whether the external storage device is connected and available to the system.
--
-- The property gives the current connection status of the external storage device.
--
-- ObjC selector: @- connected@
connected :: IsAVExternalStorageDevice avExternalStorageDevice => avExternalStorageDevice -> IO Bool
connected avExternalStorageDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avExternalStorageDevice (mkSelector "connected") retCULong []

-- | uuid
--
-- A unique identifier for external storage device.
--
-- This property can be used to select a specific external storage device with ImageCapture framework APIs to read media assets. Will return nil if we fail to extract information from external storage device.	For example the string value of this property will match the value from [ICDevice UUIDString].
--
-- ObjC selector: @- uuid@
uuid :: IsAVExternalStorageDevice avExternalStorageDevice => avExternalStorageDevice -> IO (Id NSUUID)
uuid avExternalStorageDevice  =
  sendMsg avExternalStorageDevice (mkSelector "uuid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | notRecommendedForCaptureUse
--
-- Indicates whether the external storage device is not recommended for capture use.
--
-- This property is used to let the client know if the external storage device is not suitable for camera capture.
--
-- ObjC selector: @- notRecommendedForCaptureUse@
notRecommendedForCaptureUse :: IsAVExternalStorageDevice avExternalStorageDevice => avExternalStorageDevice -> IO Bool
notRecommendedForCaptureUse avExternalStorageDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avExternalStorageDevice (mkSelector "notRecommendedForCaptureUse") retCULong []

-- | authorizationStatus
--
-- Returns the client's authorization status for capturing onto an external storage device connected to this device.
--
-- This method returns the AVAuthorizationStatus of the client for capturing onto an external storage device connected to this device. If the status is AVAuthorizationStatusNotDetermined, you may use the +requestAccessWithCompletionHandler: method to request access by prompting the user.
--
-- ObjC selector: @+ authorizationStatus@
authorizationStatus :: IO AVAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "AVExternalStorageDevice"
    fmap (coerce :: CLong -> AVAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @nextAvailableURLsWithPathExtensions:error:@
nextAvailableURLsWithPathExtensions_errorSelector :: Selector
nextAvailableURLsWithPathExtensions_errorSelector = mkSelector "nextAvailableURLsWithPathExtensions:error:"

-- | @Selector@ for @requestAccessWithCompletionHandler:@
requestAccessWithCompletionHandlerSelector :: Selector
requestAccessWithCompletionHandlerSelector = mkSelector "requestAccessWithCompletionHandler:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @freeSize@
freeSizeSelector :: Selector
freeSizeSelector = mkSelector "freeSize"

-- | @Selector@ for @totalSize@
totalSizeSelector :: Selector
totalSizeSelector = mkSelector "totalSize"

-- | @Selector@ for @connected@
connectedSelector :: Selector
connectedSelector = mkSelector "connected"

-- | @Selector@ for @uuid@
uuidSelector :: Selector
uuidSelector = mkSelector "uuid"

-- | @Selector@ for @notRecommendedForCaptureUse@
notRecommendedForCaptureUseSelector :: Selector
notRecommendedForCaptureUseSelector = mkSelector "notRecommendedForCaptureUse"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

