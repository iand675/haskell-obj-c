{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothDeviceInquiry
--
-- Object representing a device inquiry that finds Bluetooth devices in-range of the computer,					and (optionally) retrieves name information for them.
--
-- You should only use this object if your application needs to know about in-range devices and cannot					use the GUI provided by the IOBluetoothUI framework. It will not let you perform unlimited back-to-back					inquiries, but will instead throttle the number of attempted inquiries if too many are attempted within					a small window of time.					Important Note: DO NOT perform remote name requests on devices from delegate methods or while this					object is in use. If you wish to do your own remote name requests on devices, do them after you have					stopped this object. If you do not heed this warning, you could potentially deadlock your process.
--
-- Generated bindings for @IOBluetoothDeviceInquiry@.
module ObjC.IOBluetooth.IOBluetoothDeviceInquiry
  ( IOBluetoothDeviceInquiry
  , IsIOBluetoothDeviceInquiry(..)
  , inquiryWithDelegate
  , initWithDelegate
  , start
  , stop
  , foundDevices
  , clearFoundDevices
  , setSearchCriteria_majorDeviceClass_minorDeviceClass
  , delegate
  , setDelegate
  , inquiryLength
  , setInquiryLength
  , searchType
  , setSearchType
  , updateNewDeviceNames
  , setUpdateNewDeviceNames
  , inquiryWithDelegateSelector
  , initWithDelegateSelector
  , startSelector
  , stopSelector
  , foundDevicesSelector
  , clearFoundDevicesSelector
  , setSearchCriteria_majorDeviceClass_minorDeviceClassSelector
  , delegateSelector
  , setDelegateSelector
  , inquiryLengthSelector
  , setInquiryLengthSelector
  , searchTypeSelector
  , setSearchTypeSelector
  , updateNewDeviceNamesSelector
  , setUpdateNewDeviceNamesSelector


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

-- | inquiryWithDelegate
--
-- Class method to create an inquiry object.
--
-- @delegate@ — A delegate object that wishes to receive messages from the inquiry object. Delegate methods are listed below, under IOBluetoothDeviceInquiryDelegate.
--
-- Returns: A pointer to the created IOBluetoothDeviceInquiry object.
--
-- The inquiry is NOT automatically started. You musts call -start on it to start the search for in-range devices.
--
-- ObjC selector: @+ inquiryWithDelegate:@
inquiryWithDelegate :: RawId -> IO (Id IOBluetoothDeviceInquiry)
inquiryWithDelegate delegate =
  do
    cls' <- getRequiredClass "IOBluetoothDeviceInquiry"
    sendClassMsg cls' (mkSelector "inquiryWithDelegate:") (retPtr retVoid) [argPtr (castPtr (unRawId delegate) :: Ptr ())] >>= retainedObject . castPtr

-- | initWithDelegate
--
-- Initializes an alloc'd inquiry object, and sets the delegate object, as if -setDelegate: were called on it.
--
-- @delegate@ — A delegate object that wishes to receive messages from the inquiry object. Delegate methods are listed below, under IOBluetoothDeviceInquiryDelegate.
--
-- Returns: A pointer to the initialized IOBluetoothDeviceInquiry object.
--
-- ObjC selector: @- initWithDelegate:@
initWithDelegate :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> RawId -> IO (Id IOBluetoothDeviceInquiry)
initWithDelegate ioBluetoothDeviceInquiry  delegate =
  sendMsg ioBluetoothDeviceInquiry (mkSelector "initWithDelegate:") (retPtr retVoid) [argPtr (castPtr (unRawId delegate) :: Ptr ())] >>= ownedObject . castPtr

-- | start
--
-- Tells inquiry object to begin the inquiry and name updating process, if specified.
--
-- Returns: Returns kIOReturnSuccess if start was successful. Returns kIOReturnBusy if the object is already in process. May return other IOReturn values, as appropriate.
--
-- Calling start multiple times in rapid succession or back-to-back will probably not produce the intended				results. Inquiries are throttled if they are called too quickly in succession.
--
-- ObjC selector: @- start@
start :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> IO CInt
start ioBluetoothDeviceInquiry  =
  sendMsg ioBluetoothDeviceInquiry (mkSelector "start") retCInt []

-- | stop
--
-- Halts the inquiry object. Could either stop the search for new devices, or the updating of found device names.
--
-- Returns: Returns kIOReturnSuccess if the inquiry is successfully stopped. Returns kIOReturnNotPermitted if the inquiry object is already stopped. May return other IOReturn values, as appropriate.
--
-- ObjC selector: @- stop@
stop :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> IO CInt
stop ioBluetoothDeviceInquiry  =
  sendMsg ioBluetoothDeviceInquiry (mkSelector "stop") retCInt []

-- | foundDevices
--
-- Returns found IOBluetoothDevice objects as an array.
--
-- Returns: Returns an NSArray of IOBluetoothDevice objects.
--
-- Will not return nil. If there are no devices found, returns an array with length of 0.
--
-- ObjC selector: @- foundDevices@
foundDevices :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> IO (Id NSArray)
foundDevices ioBluetoothDeviceInquiry  =
  sendMsg ioBluetoothDeviceInquiry (mkSelector "foundDevices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | clearFoundDevices
--
-- Removes all found devices from the inquiry object.
--
-- ObjC selector: @- clearFoundDevices@
clearFoundDevices :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> IO ()
clearFoundDevices ioBluetoothDeviceInquiry  =
  sendMsg ioBluetoothDeviceInquiry (mkSelector "clearFoundDevices") retVoid []

-- | setSearchCriteria
--
-- Use this method to set the criteria for the device search.
--
-- @inServiceClassMajor@ — Set the major service class for found devices. Set to kBluetoothServiceClassMajorAny for all devices. See BluetoothAssignedNumbers.h for possible values.
--
-- @inMajorDeviceClass@ — Set the major device class for found devices. Set to kBluetoothDeviceClassMajorAny for all devices. See BluetoothAssignedNumbers.h for possible values.
--
-- @inMinorDeviceClass@ — Set the minor device class for found devices. Set to kBluetoothDeviceClassMinorAny for all devices. See BluetoothAssignedNumbers.h for possible values.
--
-- The default inquiry object will search for all types of devices. If you wish to find only keyboards, for example, you might use this method like this:
--
-- [myInquiryObject		setSearchCriteria:kBluetoothServiceClassMajorAny										majorDeviceClass:kBluetoothDeviceClassMajorPeripheral										minorDeviceClass:kBluetoothDeviceClassMinorPeripheral1Keyboard];
--
-- However, we recommend only using this if you are certain of the device class you are looking for, as some				devices may report a different/unexpected device class, and the search may miss the device you are interested in.
--
-- ObjC selector: @- setSearchCriteria:majorDeviceClass:minorDeviceClass:@
setSearchCriteria_majorDeviceClass_minorDeviceClass :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> CUInt -> CUInt -> CUInt -> IO ()
setSearchCriteria_majorDeviceClass_minorDeviceClass ioBluetoothDeviceInquiry  inServiceClassMajor inMajorDeviceClass inMinorDeviceClass =
  sendMsg ioBluetoothDeviceInquiry (mkSelector "setSearchCriteria:majorDeviceClass:minorDeviceClass:") retVoid [argCUInt (fromIntegral inServiceClassMajor), argCUInt (fromIntegral inMajorDeviceClass), argCUInt (fromIntegral inMinorDeviceClass)]

-- | @- delegate@
delegate :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> IO RawId
delegate ioBluetoothDeviceInquiry  =
  fmap (RawId . castPtr) $ sendMsg ioBluetoothDeviceInquiry (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> RawId -> IO ()
setDelegate ioBluetoothDeviceInquiry  value =
  sendMsg ioBluetoothDeviceInquiry (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | setInquiryLength
--
-- Set the length of the inquiry that is performed each time -start is used on an inquiry object.
--
-- @seconds@ — Number of seconds the inquiry will search for in-range devices before refreshing device names, if specified.
--
-- Returns: Number of seconds the search will be performed.
--
-- A default of 10 seconds is used, unless a different value is specified using this method.  Note that if you				have called -start again too quickly, your inquiry may actually take much longer than what length you				specify, as inquiries are throttled in the system. Also note that if you have the inquiry object updating				device names for you, the whole inquiry process could be much longer than the specified length, depending				on the number of devices found and how responsive to name requests they are. If you -must- have a strict				inquiry length, disable name updates. In other words, this "length" only refers to the actual device discovery				portion of the whole inquiry process.
--
-- ObjC selector: @- inquiryLength@
inquiryLength :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> IO CUChar
inquiryLength ioBluetoothDeviceInquiry  =
  sendMsg ioBluetoothDeviceInquiry (mkSelector "inquiryLength") retCUChar []

-- | setInquiryLength
--
-- Set the length of the inquiry that is performed each time -start is used on an inquiry object.
--
-- @seconds@ — Number of seconds the inquiry will search for in-range devices before refreshing device names, if specified.
--
-- Returns: Number of seconds the search will be performed.
--
-- A default of 10 seconds is used, unless a different value is specified using this method.  Note that if you				have called -start again too quickly, your inquiry may actually take much longer than what length you				specify, as inquiries are throttled in the system. Also note that if you have the inquiry object updating				device names for you, the whole inquiry process could be much longer than the specified length, depending				on the number of devices found and how responsive to name requests they are. If you -must- have a strict				inquiry length, disable name updates. In other words, this "length" only refers to the actual device discovery				portion of the whole inquiry process.
--
-- ObjC selector: @- setInquiryLength:@
setInquiryLength :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> CUChar -> IO ()
setInquiryLength ioBluetoothDeviceInquiry  value =
  sendMsg ioBluetoothDeviceInquiry (mkSelector "setInquiryLength:") retVoid [argCUChar (fromIntegral value)]

-- | setSearchType
--
-- Set the devices that are found.
--
-- @searchType@ — Bluetooth versions the search will discover.
--
-- A default of kIOBluetoothDeviceSearchClassic is used, unless a different value is specified using this method.
--
-- ObjC selector: @- searchType@
searchType :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> IO CUInt
searchType ioBluetoothDeviceInquiry  =
  sendMsg ioBluetoothDeviceInquiry (mkSelector "searchType") retCUInt []

-- | setSearchType
--
-- Set the devices that are found.
--
-- @searchType@ — Bluetooth versions the search will discover.
--
-- A default of kIOBluetoothDeviceSearchClassic is used, unless a different value is specified using this method.
--
-- ObjC selector: @- setSearchType:@
setSearchType :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> CUInt -> IO ()
setSearchType ioBluetoothDeviceInquiry  value =
  sendMsg ioBluetoothDeviceInquiry (mkSelector "setSearchType:") retVoid [argCUInt (fromIntegral value)]

-- | setUpdateNewDeviceNames
--
-- Sets whether or not the inquiry object will retrieve the names of devices found during the search.
--
-- @inValue@ — Pass TRUE if names are to be updated, otherwise pass FALSE.
--
-- The default value for the inquiry object is TRUE, unless this method is used to change it.
--
-- ObjC selector: @- updateNewDeviceNames@
updateNewDeviceNames :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> IO Bool
updateNewDeviceNames ioBluetoothDeviceInquiry  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioBluetoothDeviceInquiry (mkSelector "updateNewDeviceNames") retCULong []

-- | setUpdateNewDeviceNames
--
-- Sets whether or not the inquiry object will retrieve the names of devices found during the search.
--
-- @inValue@ — Pass TRUE if names are to be updated, otherwise pass FALSE.
--
-- The default value for the inquiry object is TRUE, unless this method is used to change it.
--
-- ObjC selector: @- setUpdateNewDeviceNames:@
setUpdateNewDeviceNames :: IsIOBluetoothDeviceInquiry ioBluetoothDeviceInquiry => ioBluetoothDeviceInquiry -> Bool -> IO ()
setUpdateNewDeviceNames ioBluetoothDeviceInquiry  value =
  sendMsg ioBluetoothDeviceInquiry (mkSelector "setUpdateNewDeviceNames:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @inquiryWithDelegate:@
inquiryWithDelegateSelector :: Selector
inquiryWithDelegateSelector = mkSelector "inquiryWithDelegate:"

-- | @Selector@ for @initWithDelegate:@
initWithDelegateSelector :: Selector
initWithDelegateSelector = mkSelector "initWithDelegate:"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @foundDevices@
foundDevicesSelector :: Selector
foundDevicesSelector = mkSelector "foundDevices"

-- | @Selector@ for @clearFoundDevices@
clearFoundDevicesSelector :: Selector
clearFoundDevicesSelector = mkSelector "clearFoundDevices"

-- | @Selector@ for @setSearchCriteria:majorDeviceClass:minorDeviceClass:@
setSearchCriteria_majorDeviceClass_minorDeviceClassSelector :: Selector
setSearchCriteria_majorDeviceClass_minorDeviceClassSelector = mkSelector "setSearchCriteria:majorDeviceClass:minorDeviceClass:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @inquiryLength@
inquiryLengthSelector :: Selector
inquiryLengthSelector = mkSelector "inquiryLength"

-- | @Selector@ for @setInquiryLength:@
setInquiryLengthSelector :: Selector
setInquiryLengthSelector = mkSelector "setInquiryLength:"

-- | @Selector@ for @searchType@
searchTypeSelector :: Selector
searchTypeSelector = mkSelector "searchType"

-- | @Selector@ for @setSearchType:@
setSearchTypeSelector :: Selector
setSearchTypeSelector = mkSelector "setSearchType:"

-- | @Selector@ for @updateNewDeviceNames@
updateNewDeviceNamesSelector :: Selector
updateNewDeviceNamesSelector = mkSelector "updateNewDeviceNames"

-- | @Selector@ for @setUpdateNewDeviceNames:@
setUpdateNewDeviceNamesSelector :: Selector
setUpdateNewDeviceNamesSelector = mkSelector "setUpdateNewDeviceNames:"

