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
  , streams
  , initSelector
  , newSelector
  , deviceWithLocalizedName_deviceID_legacyDeviceID_sourceSelector
  , initWithLocalizedName_deviceID_legacyDeviceID_sourceSelector
  , deviceWithLocalizedName_deviceID_sourceSelector
  , initWithLocalizedName_deviceID_sourceSelector
  , addStream_errorSelector
  , removeStream_errorSelector
  , notifyPropertiesChangedSelector
  , localizedNameSelector
  , deviceIDSelector
  , legacyDeviceIDSelector
  , streamsSelector


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

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionDevice cmioExtensionDevice => cmioExtensionDevice -> IO (Id CMIOExtensionDevice)
init_ cmioExtensionDevice  =
  sendMsg cmioExtensionDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CMIOExtensionDevice)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr localizedName $ \raw_localizedName ->
      withObjCPtr deviceID $ \raw_deviceID ->
        withObjCPtr legacyDeviceID $ \raw_legacyDeviceID ->
          sendClassMsg cls' (mkSelector "deviceWithLocalizedName:deviceID:legacyDeviceID:source:") (retPtr retVoid) [argPtr (castPtr raw_localizedName :: Ptr ()), argPtr (castPtr raw_deviceID :: Ptr ()), argPtr (castPtr raw_legacyDeviceID :: Ptr ()), argPtr (castPtr (unRawId source) :: Ptr ())] >>= retainedObject . castPtr

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
initWithLocalizedName_deviceID_legacyDeviceID_source cmioExtensionDevice  localizedName deviceID legacyDeviceID source =
withObjCPtr localizedName $ \raw_localizedName ->
  withObjCPtr deviceID $ \raw_deviceID ->
    withObjCPtr legacyDeviceID $ \raw_legacyDeviceID ->
        sendMsg cmioExtensionDevice (mkSelector "initWithLocalizedName:deviceID:legacyDeviceID:source:") (retPtr retVoid) [argPtr (castPtr raw_localizedName :: Ptr ()), argPtr (castPtr raw_deviceID :: Ptr ()), argPtr (castPtr raw_legacyDeviceID :: Ptr ()), argPtr (castPtr (unRawId source) :: Ptr ())] >>= ownedObject . castPtr

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
    withObjCPtr localizedName $ \raw_localizedName ->
      withObjCPtr deviceID $ \raw_deviceID ->
        sendClassMsg cls' (mkSelector "deviceWithLocalizedName:deviceID:source:") (retPtr retVoid) [argPtr (castPtr raw_localizedName :: Ptr ()), argPtr (castPtr raw_deviceID :: Ptr ()), argPtr (castPtr (unRawId source) :: Ptr ())] >>= retainedObject . castPtr

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
initWithLocalizedName_deviceID_source cmioExtensionDevice  localizedName deviceID source =
withObjCPtr localizedName $ \raw_localizedName ->
  withObjCPtr deviceID $ \raw_deviceID ->
      sendMsg cmioExtensionDevice (mkSelector "initWithLocalizedName:deviceID:source:") (retPtr retVoid) [argPtr (castPtr raw_localizedName :: Ptr ()), argPtr (castPtr raw_deviceID :: Ptr ()), argPtr (castPtr (unRawId source) :: Ptr ())] >>= ownedObject . castPtr

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
addStream_error cmioExtensionDevice  stream outError =
withObjCPtr stream $ \raw_stream ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmioExtensionDevice (mkSelector "addStream:error:") retCULong [argPtr (castPtr raw_stream :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

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
removeStream_error cmioExtensionDevice  stream outError =
withObjCPtr stream $ \raw_stream ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmioExtensionDevice (mkSelector "removeStream:error:") retCULong [argPtr (castPtr raw_stream :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | notifyPropertiesChanged:
--
-- Notify client(s) of device properties changes.
--
-- @propertyStates@ — The dictionary of properties having changed.
--
-- ObjC selector: @- notifyPropertiesChanged:@
notifyPropertiesChanged :: (IsCMIOExtensionDevice cmioExtensionDevice, IsNSDictionary propertyStates) => cmioExtensionDevice -> propertyStates -> IO ()
notifyPropertiesChanged cmioExtensionDevice  propertyStates =
withObjCPtr propertyStates $ \raw_propertyStates ->
    sendMsg cmioExtensionDevice (mkSelector "notifyPropertiesChanged:") retVoid [argPtr (castPtr raw_propertyStates :: Ptr ())]

-- | localizedName
--
-- The localized name of the device.
--
-- ObjC selector: @- localizedName@
localizedName :: IsCMIOExtensionDevice cmioExtensionDevice => cmioExtensionDevice -> IO (Id NSString)
localizedName cmioExtensionDevice  =
  sendMsg cmioExtensionDevice (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | deviceID
--
-- The device identifier as UUID.
--
-- ObjC selector: @- deviceID@
deviceID :: IsCMIOExtensionDevice cmioExtensionDevice => cmioExtensionDevice -> IO (Id NSUUID)
deviceID cmioExtensionDevice  =
  sendMsg cmioExtensionDevice (mkSelector "deviceID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | legacyDeviceID
--
-- The device identifier as a string (for backward compatibility with AVCaptureDevice.uniqueIdentifier)
--
-- ObjC selector: @- legacyDeviceID@
legacyDeviceID :: IsCMIOExtensionDevice cmioExtensionDevice => cmioExtensionDevice -> IO (Id NSString)
legacyDeviceID cmioExtensionDevice  =
  sendMsg cmioExtensionDevice (mkSelector "legacyDeviceID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | streams
--
-- The streams array of the device.
--
-- This property is not key-value observable.
--
-- ObjC selector: @- streams@
streams :: IsCMIOExtensionDevice cmioExtensionDevice => cmioExtensionDevice -> IO (Id NSArray)
streams cmioExtensionDevice  =
  sendMsg cmioExtensionDevice (mkSelector "streams") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @deviceWithLocalizedName:deviceID:legacyDeviceID:source:@
deviceWithLocalizedName_deviceID_legacyDeviceID_sourceSelector :: Selector
deviceWithLocalizedName_deviceID_legacyDeviceID_sourceSelector = mkSelector "deviceWithLocalizedName:deviceID:legacyDeviceID:source:"

-- | @Selector@ for @initWithLocalizedName:deviceID:legacyDeviceID:source:@
initWithLocalizedName_deviceID_legacyDeviceID_sourceSelector :: Selector
initWithLocalizedName_deviceID_legacyDeviceID_sourceSelector = mkSelector "initWithLocalizedName:deviceID:legacyDeviceID:source:"

-- | @Selector@ for @deviceWithLocalizedName:deviceID:source:@
deviceWithLocalizedName_deviceID_sourceSelector :: Selector
deviceWithLocalizedName_deviceID_sourceSelector = mkSelector "deviceWithLocalizedName:deviceID:source:"

-- | @Selector@ for @initWithLocalizedName:deviceID:source:@
initWithLocalizedName_deviceID_sourceSelector :: Selector
initWithLocalizedName_deviceID_sourceSelector = mkSelector "initWithLocalizedName:deviceID:source:"

-- | @Selector@ for @addStream:error:@
addStream_errorSelector :: Selector
addStream_errorSelector = mkSelector "addStream:error:"

-- | @Selector@ for @removeStream:error:@
removeStream_errorSelector :: Selector
removeStream_errorSelector = mkSelector "removeStream:error:"

-- | @Selector@ for @notifyPropertiesChanged:@
notifyPropertiesChangedSelector :: Selector
notifyPropertiesChangedSelector = mkSelector "notifyPropertiesChanged:"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @deviceID@
deviceIDSelector :: Selector
deviceIDSelector = mkSelector "deviceID"

-- | @Selector@ for @legacyDeviceID@
legacyDeviceIDSelector :: Selector
legacyDeviceIDSelector = mkSelector "legacyDeviceID"

-- | @Selector@ for @streams@
streamsSelector :: Selector
streamsSelector = mkSelector "streams"

