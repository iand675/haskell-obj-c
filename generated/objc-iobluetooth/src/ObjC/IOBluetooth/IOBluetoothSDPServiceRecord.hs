{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothSDPServiceRecord
--
-- An instance of this class represents a single SDP service record.
--
-- As a service record, an instance of this class has an NSDictionary of service attributes.                It also has a link to the IOBluetoothDevice that the service belongs to.  The service                dictionary is keyed off of the attribute ID of each attribute represented as an NSNumber.
--
-- Generated bindings for @IOBluetoothSDPServiceRecord@.
module ObjC.IOBluetooth.IOBluetoothSDPServiceRecord
  ( IOBluetoothSDPServiceRecord
  , IsIOBluetoothSDPServiceRecord(..)
  , publishedServiceRecordWithDictionary
  , removeServiceRecord
  , withServiceDictionary_device
  , initWithServiceDictionary_device
  , withSDPServiceRecordRef
  , getSDPServiceRecordRef
  , getDevice
  , getAttributes
  , getAttributeDataElement
  , getServiceName
  , matchesUUID16
  , matchesUUIDArray
  , matchesSearchArray
  , hasServiceFromArray
  , handsFreeSupportedFeatures
  , device
  , attributes
  , sortedAttributes
  , publishedServiceRecordWithDictionarySelector
  , removeServiceRecordSelector
  , withServiceDictionary_deviceSelector
  , initWithServiceDictionary_deviceSelector
  , withSDPServiceRecordRefSelector
  , getSDPServiceRecordRefSelector
  , getDeviceSelector
  , getAttributesSelector
  , getAttributeDataElementSelector
  , getServiceNameSelector
  , matchesUUID16Selector
  , matchesUUIDArraySelector
  , matchesSearchArraySelector
  , hasServiceFromArraySelector
  , handsFreeSupportedFeaturesSelector
  , deviceSelector
  , attributesSelector
  , sortedAttributesSelector


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

import ObjC.IOBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | publishedServiceRecordWithDictionary:
--
-- Adds a service to the local SDP server.
--
-- Returns: Returns an IOBluetoothSDPServiceRecord * with the attributes specified in the provided dictionary.
--
-- Each entry in the dictionary representing the service contains the individual attributes.  Each attribute in the dict is keyed by a string that must begin with a hex number representing the attribute ID.  The key string may contain additional characters if desired as long as they follow a space after the ID hex string.  The attribute value must follow the dictionary format described by IOBluetoothSDPDataElement.  This dictionary format allows a service dict to be created as a plist file and then loaded into the system rather than built up in code.  See the example code for an example of how can be done.
--
-- If the service record handle, L2CAP PSM or RFCOMM channel ID specified in the dictionary are in use, an alternate one will be assigned.
--
-- In addition to attributes that represent the service itself, additional attributes may be specified that control the local behavior of the service.  To specify these local attributes, an additional property titled "LocalAttributes" may be added to the root of the service dict.  The value of this property must be a dictionary that contains the individual local attributes.
--
-- Currently, only two local attributes are supported: "Persistent" and "TargetApplication".
--
-- The "Persistent" local attribute must be either a boolean or number representing whether the service should be persistent.  A persistent service will be saved off and restored any time the Bluetooth hardware is present.  It will persist through reboots and can only be removed by calling IOBluetoothRemoveServiceWithRecordHandle(void).  This attribute is optional.  By default, if no "Persistent" local property is present,	the service will only exist temporarily.  It will be removed either when IOBluetoothRemoveServiceWithRecordHandle(void) is called or when the client application exits.
--
-- The "TargetApplication" local attribute is used to specify an application to be launched when a remote device attempts to connect to the service (by opening either an L2CAP or RFCOMM channel of the type specified in the service).  This value must be a string representing the absolute path to the target executable (not just the .app wrapper - i.e. /System/Library/CoreServices/OBEXAgent.app/Contents/MacOS/OBEXAgent).  This attribute is optional. If no "TargetApplication" local attribute is specified, no special action will take place when an incoming connection to the service is created.  It is up to the client to be monitoring for the connection and to do the right thing when one appears.
--
-- The "LocalAttributes" property is optional.  If it is not specified, by default the created service is transient and will be removed when the client exits.
--
-- Additional local attributes to further control incoming services will be added in the future.
--
-- @serviceDict@ — A dictionary containing the attributes for the new service
--
-- ObjC selector: @+ publishedServiceRecordWithDictionary:@
publishedServiceRecordWithDictionary :: IsNSDictionary serviceDict => serviceDict -> IO (Id IOBluetoothSDPServiceRecord)
publishedServiceRecordWithDictionary serviceDict =
  do
    cls' <- getRequiredClass "IOBluetoothSDPServiceRecord"
    withObjCPtr serviceDict $ \raw_serviceDict ->
      sendClassMsg cls' (mkSelector "publishedServiceRecordWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_serviceDict :: Ptr ())] >>= retainedObject . castPtr

-- | removeServiceRecord
--
-- Removes the service from the local SDP server.
--
-- Returns: Returns kIOReturnSuccess if successful.
--
-- ObjC selector: @- removeServiceRecord@
removeServiceRecord :: IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord => ioBluetoothSDPServiceRecord -> IO CInt
removeServiceRecord ioBluetoothSDPServiceRecord  =
  sendMsg ioBluetoothSDPServiceRecord (mkSelector "removeServiceRecord") retCInt []

-- | withServiceDictionary:device:
--
-- Returns an IOBluetoothSDPServiceRecord * with the attributes specified in the provided service dictionary. Provide				a pointer to an IOBlueotothDevice if you wish to associate the record to a specific IOBluetoothDevice.
--
-- Returns: Returns an IOBluetoothSDPServiceRecord * with the attributes specified in the provided dictionary.
--
-- ObjC selector: @+ withServiceDictionary:device:@
withServiceDictionary_device :: (IsNSDictionary serviceDict, IsIOBluetoothDevice device) => serviceDict -> device -> IO (Id IOBluetoothSDPServiceRecord)
withServiceDictionary_device serviceDict device =
  do
    cls' <- getRequiredClass "IOBluetoothSDPServiceRecord"
    withObjCPtr serviceDict $ \raw_serviceDict ->
      withObjCPtr device $ \raw_device ->
        sendClassMsg cls' (mkSelector "withServiceDictionary:device:") (retPtr retVoid) [argPtr (castPtr raw_serviceDict :: Ptr ()), argPtr (castPtr raw_device :: Ptr ())] >>= retainedObject . castPtr

-- | initWithServiceDictionary
--
-- Returns an initialized IOBluetoothSDPServiceRecord * with the attributes specified in the provided service dictionary. Provide				a pointer to an IOBlueotothDevice if you wish to associate the record to a specific IOBluetoothDevice.
--
-- Returns: Returns an initialized IOBluetoothSDPServiceRecord * with the attributes specified in the provided dictionary.
--
-- ObjC selector: @- initWithServiceDictionary:device:@
initWithServiceDictionary_device :: (IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord, IsNSDictionary serviceDict, IsIOBluetoothDevice device) => ioBluetoothSDPServiceRecord -> serviceDict -> device -> IO (Id IOBluetoothSDPServiceRecord)
initWithServiceDictionary_device ioBluetoothSDPServiceRecord  serviceDict device =
withObjCPtr serviceDict $ \raw_serviceDict ->
  withObjCPtr device $ \raw_device ->
      sendMsg ioBluetoothSDPServiceRecord (mkSelector "initWithServiceDictionary:device:") (retPtr retVoid) [argPtr (castPtr raw_serviceDict :: Ptr ()), argPtr (castPtr raw_device :: Ptr ())] >>= ownedObject . castPtr

-- | withSDPServiceRecordRef:
--
-- Method call to convert an IOBluetoothSDPServiceRecordRef into an IOBluetoothSDPServiceRecord *.
--
-- @sdpServiceRecordRef@ — IOBluetoothSDPServiceRecordRef for which an IOBluetoothSDPServiceRecord * is desired.
--
-- Returns: Returns the IOBluetoothSDPServiceRecord * for the given IOBluetoothSDPServiceRecordRef.
--
-- ObjC selector: @+ withSDPServiceRecordRef:@
withSDPServiceRecordRef :: Ptr () -> IO (Id IOBluetoothSDPServiceRecord)
withSDPServiceRecordRef sdpServiceRecordRef =
  do
    cls' <- getRequiredClass "IOBluetoothSDPServiceRecord"
    sendClassMsg cls' (mkSelector "withSDPServiceRecordRef:") (retPtr retVoid) [argPtr sdpServiceRecordRef] >>= retainedObject . castPtr

-- | getSDPServiceRecordRef
--
-- Returns an IOBluetoothSDPServiceRecordRef representation of the target IOBluetoothSDPServiceRecord object.
--
-- Returns: Returns an IOBluetoothSDPServiceRecordRef representation of the target IOBluetoothSDPServiceRecord object.
--
-- ObjC selector: @- getSDPServiceRecordRef@
getSDPServiceRecordRef :: IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord => ioBluetoothSDPServiceRecord -> IO (Ptr ())
getSDPServiceRecordRef ioBluetoothSDPServiceRecord  =
  fmap castPtr $ sendMsg ioBluetoothSDPServiceRecord (mkSelector "getSDPServiceRecordRef") (retPtr retVoid) []

-- | @- getDevice@
getDevice :: IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord => ioBluetoothSDPServiceRecord -> IO (Id IOBluetoothDevice)
getDevice ioBluetoothSDPServiceRecord  =
  sendMsg ioBluetoothSDPServiceRecord (mkSelector "getDevice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- getAttributes@
getAttributes :: IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord => ioBluetoothSDPServiceRecord -> IO (Id NSDictionary)
getAttributes ioBluetoothSDPServiceRecord  =
  sendMsg ioBluetoothSDPServiceRecord (mkSelector "getAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | getAttributeDataElement:
--
-- Returns the data element for the given attribute ID in the target service.
--
-- @attributeID@ — The attribute ID of the desired attribute.
--
-- Returns: Returns the data element for the given attribute ID in the target service.  If the service does not				contain an attribute with the given ID, then nil is returned.
--
-- ObjC selector: @- getAttributeDataElement:@
getAttributeDataElement :: IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord => ioBluetoothSDPServiceRecord -> CUShort -> IO (Id IOBluetoothSDPDataElement)
getAttributeDataElement ioBluetoothSDPServiceRecord  attributeID =
  sendMsg ioBluetoothSDPServiceRecord (mkSelector "getAttributeDataElement:") (retPtr retVoid) [argCUInt (fromIntegral attributeID)] >>= retainedObject . castPtr

-- | getServiceName
--
-- Returns the name of the service.
--
-- This is currently implemented to simply return the attribute with an id of 0x0100.  In                the future, it will be extended to allow name localization based on the user's chosen                language or other languages.
--
-- Returns: Returns the name of the target service.
--
-- ObjC selector: @- getServiceName@
getServiceName :: IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord => ioBluetoothSDPServiceRecord -> IO (Id NSString)
getServiceName ioBluetoothSDPServiceRecord  =
  sendMsg ioBluetoothSDPServiceRecord (mkSelector "getServiceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | matchesUUID16:
--
-- Returns TRUE the UUID16 is found in the target service.
--
-- NOTE: This method is only available in Mac OS X 10.7 or later.
--
-- @uuid16@ — A BluetoothSDPUUID16 to search for in the target service.
--
-- Returns: Returns TRUE if the UUID16 is present in the service.
--
-- ObjC selector: @- matchesUUID16:@
matchesUUID16 :: IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord => ioBluetoothSDPServiceRecord -> CUShort -> IO Bool
matchesUUID16 ioBluetoothSDPServiceRecord  uuid16 =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioBluetoothSDPServiceRecord (mkSelector "matchesUUID16:") retCULong [argCUInt (fromIntegral uuid16)]

-- | matchesUUIDArray:
--
-- Returns TRUE if ALL of the UUIDs in the given array is found in the target service.
--
-- The given array should contain IOBluetoothSDPUUID objects.  It only returns TRUE if all of                the UUIDs are found.  This method is like hasServiceFromArray: except that it requires that                all UUIDs match instead of any of them matching.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- @array@ — An NSArray of IOBluetoothSDPUUID objects to search for in the target service.
--
-- Returns: Returns TRUE if all of the given UUIDs are present in the service.
--
-- ObjC selector: @- matchesUUIDArray:@
matchesUUIDArray :: (IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord, IsNSArray uuidArray) => ioBluetoothSDPServiceRecord -> uuidArray -> IO Bool
matchesUUIDArray ioBluetoothSDPServiceRecord  uuidArray =
withObjCPtr uuidArray $ \raw_uuidArray ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioBluetoothSDPServiceRecord (mkSelector "matchesUUIDArray:") retCULong [argPtr (castPtr raw_uuidArray :: Ptr ())]

-- | matchesSearchArray:
--
-- Returns TRUE any of the UUID arrays in the search array match the target service.
--
-- The given array should contain NSArray objects.  Each sub-NSArray should contain                IOBluetoothSDPUUID objects.  In turn, each sub-NSArray gets passed to -matchesUUIDArray:                If any of those returns TRUE, then the search stops and TRUE is returned.                Essentially the master NSArray contains the OR operations and each sub-array contains                the AND operations.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- @array@ — An NSArray of NSArrays of IOBluetoothSDPUUID objects.
--
-- Returns: Returns TRUE if any of the UUID arrays match.
--
-- ObjC selector: @- matchesSearchArray:@
matchesSearchArray :: (IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord, IsNSArray searchArray) => ioBluetoothSDPServiceRecord -> searchArray -> IO Bool
matchesSearchArray ioBluetoothSDPServiceRecord  searchArray =
withObjCPtr searchArray $ \raw_searchArray ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioBluetoothSDPServiceRecord (mkSelector "matchesSearchArray:") retCULong [argPtr (castPtr raw_searchArray :: Ptr ())]

-- | hasServiceFromArray:
--
-- Returns TRUE if any one of the UUIDs in the given array is found in the target service.
--
-- The given array should contain IOBluetoothSDPUUID objects.  It is currently implemented                such that it returns TRUE if any of the UUIDs are found.  However in the future, it is likely                that this will change to more closely match the functionality in the SDP spec so that it only                returns TRUE if all of the given UUIDs are present.  That way, both AND and OR comparisons                can be implemented.  Please make a note of this potential change.
--
-- @array@ — An NSArray of IOBluetoothSDPUUID objects to search for in the target service.
--
-- Returns: Returns TRUE if any of the given UUIDs are present in the service.
--
-- ObjC selector: @- hasServiceFromArray:@
hasServiceFromArray :: (IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord, IsNSArray array) => ioBluetoothSDPServiceRecord -> array -> IO Bool
hasServiceFromArray ioBluetoothSDPServiceRecord  array =
withObjCPtr array $ \raw_array ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioBluetoothSDPServiceRecord (mkSelector "hasServiceFromArray:") retCULong [argPtr (castPtr raw_array :: Ptr ())]

-- | handsFreeSupportedFeatures
--
-- Return the hands free supported features
--
-- Returns the hands free supported features bitmap stored in the SDP record. See “IOBluetoothHandsFreeDeviceFeatures and IOBluetoothHandsFreeAudioGatewayFeatures.”
--
-- Returns: The supported features bitmap.
--
-- ObjC selector: @- handsFreeSupportedFeatures@
handsFreeSupportedFeatures :: IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord => ioBluetoothSDPServiceRecord -> IO CUShort
handsFreeSupportedFeatures ioBluetoothSDPServiceRecord  =
  fmap fromIntegral $ sendMsg ioBluetoothSDPServiceRecord (mkSelector "handsFreeSupportedFeatures") retCUInt []

-- | device
--
-- Returns the IOBluetoothDevice that the target service belongs to.
--
-- If the service is a local service (i.e. one the current host is vending out), then nil is returned.
--
-- Returns: Returns the IOBluetoothDevice that the target service belongs to.  If the service is one the local host				is vending, then nil is returned.
--
-- ObjC selector: @- device@
device :: IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord => ioBluetoothSDPServiceRecord -> IO (Id IOBluetoothDevice)
device ioBluetoothSDPServiceRecord  =
  sendMsg ioBluetoothSDPServiceRecord (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | attributes
--
-- Returns an NSDictionary containing the attributes for the service.
--
-- The attribute dictionary is keyed off of the attribute id represented as an NSNumber.  The values                in the NSDictionary are IOBluetoothSDPDataElement objects representing the data element for the                given attribute.
--
-- Returns: Returns an NSDictionary containing the attributes for the target service.
--
-- ObjC selector: @- attributes@
attributes :: IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord => ioBluetoothSDPServiceRecord -> IO (Id NSDictionary)
attributes ioBluetoothSDPServiceRecord  =
  sendMsg ioBluetoothSDPServiceRecord (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sortedAttributes:
--
-- Returns a sorted array of SDP attributes
--
-- This method will walk all the elements of the service record and return an array of 				IOBluetoothSDPServiceAttribute objects sorted by attributeID
--
-- Returns: Returns a sorted array of SDP attributes
--
-- ObjC selector: @- sortedAttributes@
sortedAttributes :: IsIOBluetoothSDPServiceRecord ioBluetoothSDPServiceRecord => ioBluetoothSDPServiceRecord -> IO (Id NSArray)
sortedAttributes ioBluetoothSDPServiceRecord  =
  sendMsg ioBluetoothSDPServiceRecord (mkSelector "sortedAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @publishedServiceRecordWithDictionary:@
publishedServiceRecordWithDictionarySelector :: Selector
publishedServiceRecordWithDictionarySelector = mkSelector "publishedServiceRecordWithDictionary:"

-- | @Selector@ for @removeServiceRecord@
removeServiceRecordSelector :: Selector
removeServiceRecordSelector = mkSelector "removeServiceRecord"

-- | @Selector@ for @withServiceDictionary:device:@
withServiceDictionary_deviceSelector :: Selector
withServiceDictionary_deviceSelector = mkSelector "withServiceDictionary:device:"

-- | @Selector@ for @initWithServiceDictionary:device:@
initWithServiceDictionary_deviceSelector :: Selector
initWithServiceDictionary_deviceSelector = mkSelector "initWithServiceDictionary:device:"

-- | @Selector@ for @withSDPServiceRecordRef:@
withSDPServiceRecordRefSelector :: Selector
withSDPServiceRecordRefSelector = mkSelector "withSDPServiceRecordRef:"

-- | @Selector@ for @getSDPServiceRecordRef@
getSDPServiceRecordRefSelector :: Selector
getSDPServiceRecordRefSelector = mkSelector "getSDPServiceRecordRef"

-- | @Selector@ for @getDevice@
getDeviceSelector :: Selector
getDeviceSelector = mkSelector "getDevice"

-- | @Selector@ for @getAttributes@
getAttributesSelector :: Selector
getAttributesSelector = mkSelector "getAttributes"

-- | @Selector@ for @getAttributeDataElement:@
getAttributeDataElementSelector :: Selector
getAttributeDataElementSelector = mkSelector "getAttributeDataElement:"

-- | @Selector@ for @getServiceName@
getServiceNameSelector :: Selector
getServiceNameSelector = mkSelector "getServiceName"

-- | @Selector@ for @matchesUUID16:@
matchesUUID16Selector :: Selector
matchesUUID16Selector = mkSelector "matchesUUID16:"

-- | @Selector@ for @matchesUUIDArray:@
matchesUUIDArraySelector :: Selector
matchesUUIDArraySelector = mkSelector "matchesUUIDArray:"

-- | @Selector@ for @matchesSearchArray:@
matchesSearchArraySelector :: Selector
matchesSearchArraySelector = mkSelector "matchesSearchArray:"

-- | @Selector@ for @hasServiceFromArray:@
hasServiceFromArraySelector :: Selector
hasServiceFromArraySelector = mkSelector "hasServiceFromArray:"

-- | @Selector@ for @handsFreeSupportedFeatures@
handsFreeSupportedFeaturesSelector :: Selector
handsFreeSupportedFeaturesSelector = mkSelector "handsFreeSupportedFeatures"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @sortedAttributes@
sortedAttributesSelector :: Selector
sortedAttributesSelector = mkSelector "sortedAttributes"

