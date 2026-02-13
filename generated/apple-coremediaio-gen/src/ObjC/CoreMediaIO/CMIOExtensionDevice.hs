{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMIOExtensionDevice
--
-- A CMIOExtensionDevice describes a device.
--
-- Generated bindings for @CMIOExtensionDevice@.
module ObjC.CoreMediaIO.CMIOExtensionDevice
  ( CMIOExtensionDevice
  , IsCMIOExtensionDevice(..)
  , init_
  , new
  , deviceWithLocalizedName_deviceID_legacyDeviceID_source
  , initWithLocalizedName_deviceID_legacyDeviceID_source
  , deviceWithLocalizedName_deviceID_source
  , initWithLocalizedName_deviceID_source
  , addStream_error
  , removeStream_error
  , notifyPropertiesChanged
  , localizedName
  , deviceID
  , legacyDeviceID
  , source
  , streams
  , addStream_errorSelector
  , deviceIDSelector
  , deviceWithLocalizedName_deviceID_legacyDeviceID_sourceSelector
  , deviceWithLocalizedName_deviceID_sourceSelector
  , initSelector
  , initWithLocalizedName_deviceID_legacyDeviceID_sourceSelector
  , initWithLocalizedName_deviceID_sourceSelector
  , legacyDeviceIDSelector
  , localizedNameSelector
  , newSelector
  , notifyPropertiesChangedSelector
  , removeStream_errorSelector
  , sourceSelector
  , streamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionDevice cmioExtensionDevice => cmioExtensionDevice -> IO (Id CMIOExtensionDevice)
init_ cmioExtensionDevice =
  sendOwnedMessage cmioExtensionDevice initSelector

-- | @+ new@
new :: IO (Id CMIOExtensionDevice)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionDevice"
    sendOwnedClassMessage cls' newSelector

-- | deviceWithLocalizedName:deviceID:legacyDeviceID:source:
--
-- Returns a device instance.
--
-- @localizedName@ — The localized name of the device.
--
-- @deviceID@ — The device id (as a UUID).
--
-- @legacyDeviceID@ — The device identifier as a string (for backward compatibility with existing CMIO DAL clients, it may differ from deviceID.UUIDString). May be nil if your device has no compatibility requirements.
--
-- @source@ — The device source, a client instantiated object for the device that conforms to the CMIOExtensionDeviceSource protocol.
--
-- Returns: A CMIOExtensionDevice instance.
--
-- ObjC selector: @+ deviceWithLocalizedName:deviceID:legacyDeviceID:source:@
deviceWithLocalizedName_deviceID_legacyDeviceID_source :: (IsNSString localizedName, IsNSUUID deviceID, IsNSString legacyDeviceID) => localizedName -> deviceID -> legacyDeviceID -> RawId -> IO (Id CMIOExtensionDevice)
deviceWithLocalizedName_deviceID_legacyDeviceID_source localizedName deviceID legacyDeviceID source =
  do
    cls' <- getRequiredClass "CMIOExtensionDevice"
    sendClassMessage cls' deviceWithLocalizedName_deviceID_legacyDeviceID_sourceSelector (toNSString localizedName) (toNSUUID deviceID) (toNSString legacyDeviceID) source

-- | initWithLocalizedName:deviceID:legacyDeviceID:source:
--
-- Initialize a device instance.
--
-- @localizedName@ — The localized name of the device.
--
-- @deviceID@ — The device id (as a UUID).
--
-- @legacyDeviceID@ — The device identifier as a string (for backward compatibility with existing CMIO DAL clients, it may differ from deviceID.UUIDString). May be nil if your device has no compatibility requirements.
--
-- @source@ — The device source, a client instantiated object for the device that conforms to the CMIOExtensionDeviceSource protocol.
--
-- Returns: A CMIOExtensionDevice instance.
--
-- ObjC selector: @- initWithLocalizedName:deviceID:legacyDeviceID:source:@
initWithLocalizedName_deviceID_legacyDeviceID_source :: (IsCMIOExtensionDevice cmioExtensionDevice, IsNSString localizedName, IsNSUUID deviceID, IsNSString legacyDeviceID) => cmioExtensionDevice -> localizedName -> deviceID -> legacyDeviceID -> RawId -> IO (Id CMIOExtensionDevice)
initWithLocalizedName_deviceID_legacyDeviceID_source cmioExtensionDevice localizedName deviceID legacyDeviceID source =
  sendOwnedMessage cmioExtensionDevice initWithLocalizedName_deviceID_legacyDeviceID_sourceSelector (toNSString localizedName) (toNSUUID deviceID) (toNSString legacyDeviceID) source

-- | deviceWithLocalizedName:deviceID:source:
--
-- Returns a device instance.
--
-- @localizedName@ — The localized name of the device.
--
-- @deviceID@ — The device unique identifier.
--
-- @source@ — The device source, a client instantiated object for the device that conforms to the CMIOExtensionDeviceSource protocol.
--
-- Returns: A CMIOExtensionDevice instance.
--
-- ObjC selector: @+ deviceWithLocalizedName:deviceID:source:@
deviceWithLocalizedName_deviceID_source :: (IsNSString localizedName, IsNSUUID deviceID) => localizedName -> deviceID -> RawId -> IO (Id CMIOExtensionDevice)
deviceWithLocalizedName_deviceID_source localizedName deviceID source =
  do
    cls' <- getRequiredClass "CMIOExtensionDevice"
    sendClassMessage cls' deviceWithLocalizedName_deviceID_sourceSelector (toNSString localizedName) (toNSUUID deviceID) source

-- | initWithLocalizedName:deviceID:source:
--
-- Initialize a device instance.
--
-- @localizedName@ — The localized name of the device.
--
-- @deviceID@ — The device unique identifier.
--
-- @source@ — The device source, a client instantiated object for the device that conforms to the CMIOExtensionDeviceSource protocol.
--
-- Returns: A CMIOExtensionDevice instance.
--
-- ObjC selector: @- initWithLocalizedName:deviceID:source:@
initWithLocalizedName_deviceID_source :: (IsCMIOExtensionDevice cmioExtensionDevice, IsNSString localizedName, IsNSUUID deviceID) => cmioExtensionDevice -> localizedName -> deviceID -> RawId -> IO (Id CMIOExtensionDevice)
initWithLocalizedName_deviceID_source cmioExtensionDevice localizedName deviceID source =
  sendOwnedMessage cmioExtensionDevice initWithLocalizedName_deviceID_sourceSelector (toNSString localizedName) (toNSUUID deviceID) source

-- | addStream:error:
--
-- Add a stream to the device streams array.
--
-- @stream@ — The stream to be added to the device streams array.
--
-- @outError@ — An error return on failure.
--
-- Returns: Return YES on success, NO otherwise.
--
-- ObjC selector: @- addStream:error:@
addStream_error :: (IsCMIOExtensionDevice cmioExtensionDevice, IsCMIOExtensionStream stream, IsNSError outError) => cmioExtensionDevice -> stream -> outError -> IO Bool
addStream_error cmioExtensionDevice stream outError =
  sendMessage cmioExtensionDevice addStream_errorSelector (toCMIOExtensionStream stream) (toNSError outError)

-- | removeStream:error:
--
-- Remove a stream from the device streams array.
--
-- @stream@ — The stream to be removed from the device streams array.
--
-- @outError@ — An error return on failure.
--
-- Returns: Return YES on success, NO otherwise.
--
-- ObjC selector: @- removeStream:error:@
removeStream_error :: (IsCMIOExtensionDevice cmioExtensionDevice, IsCMIOExtensionStream stream, IsNSError outError) => cmioExtensionDevice -> stream -> outError -> IO Bool
removeStream_error cmioExtensionDevice stream outError =
  sendMessage cmioExtensionDevice removeStream_errorSelector (toCMIOExtensionStream stream) (toNSError outError)

-- | notifyPropertiesChanged:
--
-- Notify client(s) of device properties changes.
--
-- @propertyStates@ — The dictionary of properties having changed.
--
-- ObjC selector: @- notifyPropertiesChanged:@
notifyPropertiesChanged :: (IsCMIOExtensionDevice cmioExtensionDevice, IsNSDictionary propertyStates) => cmioExtensionDevice -> propertyStates -> IO ()
notifyPropertiesChanged cmioExtensionDevice propertyStates =
  sendMessage cmioExtensionDevice notifyPropertiesChangedSelector (toNSDictionary propertyStates)

-- | localizedName
--
-- The localized name of the device.
--
-- ObjC selector: @- localizedName@
localizedName :: IsCMIOExtensionDevice cmioExtensionDevice => cmioExtensionDevice -> IO (Id NSString)
localizedName cmioExtensionDevice =
  sendMessage cmioExtensionDevice localizedNameSelector

-- | deviceID
--
-- The device identifier as UUID.
--
-- ObjC selector: @- deviceID@
deviceID :: IsCMIOExtensionDevice cmioExtensionDevice => cmioExtensionDevice -> IO (Id NSUUID)
deviceID cmioExtensionDevice =
  sendMessage cmioExtensionDevice deviceIDSelector

-- | legacyDeviceID
--
-- The device identifier as a string (for backward compatibility with AVCaptureDevice.uniqueIdentifier)
--
-- ObjC selector: @- legacyDeviceID@
legacyDeviceID :: IsCMIOExtensionDevice cmioExtensionDevice => cmioExtensionDevice -> IO (Id NSString)
legacyDeviceID cmioExtensionDevice =
  sendMessage cmioExtensionDevice legacyDeviceIDSelector

-- | source
--
-- The device source.
--
-- ObjC selector: @- source@
source :: IsCMIOExtensionDevice cmioExtensionDevice => cmioExtensionDevice -> IO RawId
source cmioExtensionDevice =
  sendMessage cmioExtensionDevice sourceSelector

-- | streams
--
-- The streams array of the device.
--
-- This property is not key-value observable.
--
-- ObjC selector: @- streams@
streams :: IsCMIOExtensionDevice cmioExtensionDevice => cmioExtensionDevice -> IO (Id NSArray)
streams cmioExtensionDevice =
  sendMessage cmioExtensionDevice streamsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CMIOExtensionDevice)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CMIOExtensionDevice)
newSelector = mkSelector "new"

-- | @Selector@ for @deviceWithLocalizedName:deviceID:legacyDeviceID:source:@
deviceWithLocalizedName_deviceID_legacyDeviceID_sourceSelector :: Selector '[Id NSString, Id NSUUID, Id NSString, RawId] (Id CMIOExtensionDevice)
deviceWithLocalizedName_deviceID_legacyDeviceID_sourceSelector = mkSelector "deviceWithLocalizedName:deviceID:legacyDeviceID:source:"

-- | @Selector@ for @initWithLocalizedName:deviceID:legacyDeviceID:source:@
initWithLocalizedName_deviceID_legacyDeviceID_sourceSelector :: Selector '[Id NSString, Id NSUUID, Id NSString, RawId] (Id CMIOExtensionDevice)
initWithLocalizedName_deviceID_legacyDeviceID_sourceSelector = mkSelector "initWithLocalizedName:deviceID:legacyDeviceID:source:"

-- | @Selector@ for @deviceWithLocalizedName:deviceID:source:@
deviceWithLocalizedName_deviceID_sourceSelector :: Selector '[Id NSString, Id NSUUID, RawId] (Id CMIOExtensionDevice)
deviceWithLocalizedName_deviceID_sourceSelector = mkSelector "deviceWithLocalizedName:deviceID:source:"

-- | @Selector@ for @initWithLocalizedName:deviceID:source:@
initWithLocalizedName_deviceID_sourceSelector :: Selector '[Id NSString, Id NSUUID, RawId] (Id CMIOExtensionDevice)
initWithLocalizedName_deviceID_sourceSelector = mkSelector "initWithLocalizedName:deviceID:source:"

-- | @Selector@ for @addStream:error:@
addStream_errorSelector :: Selector '[Id CMIOExtensionStream, Id NSError] Bool
addStream_errorSelector = mkSelector "addStream:error:"

-- | @Selector@ for @removeStream:error:@
removeStream_errorSelector :: Selector '[Id CMIOExtensionStream, Id NSError] Bool
removeStream_errorSelector = mkSelector "removeStream:error:"

-- | @Selector@ for @notifyPropertiesChanged:@
notifyPropertiesChangedSelector :: Selector '[Id NSDictionary] ()
notifyPropertiesChangedSelector = mkSelector "notifyPropertiesChanged:"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @deviceID@
deviceIDSelector :: Selector '[] (Id NSUUID)
deviceIDSelector = mkSelector "deviceID"

-- | @Selector@ for @legacyDeviceID@
legacyDeviceIDSelector :: Selector '[] (Id NSString)
legacyDeviceIDSelector = mkSelector "legacyDeviceID"

-- | @Selector@ for @source@
sourceSelector :: Selector '[] RawId
sourceSelector = mkSelector "source"

-- | @Selector@ for @streams@
streamsSelector :: Selector '[] (Id NSArray)
streamsSelector = mkSelector "streams"

