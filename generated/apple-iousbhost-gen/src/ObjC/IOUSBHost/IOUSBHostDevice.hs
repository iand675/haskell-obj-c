{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOUSBHostDevice
--
-- The IOUSBHostObject representing a USB device
--
-- This class provides functionality to send control requests to the default control endpoint
--
-- Generated bindings for @IOUSBHostDevice@.
module ObjC.IOUSBHost.IOUSBHostDevice
  ( IOUSBHostDevice
  , IsIOUSBHostDevice(..)
  , createMatchingDictionaryWithVendorID_productID_bcdDevice_deviceClass_deviceSubclass_deviceProtocol_speed_productIDArray
  , configureWithValue_matchInterfaces_error
  , configureWithValue_error
  , resetWithError
  , configurationDescriptor
  , configurationDescriptorSelector
  , configureWithValue_errorSelector
  , configureWithValue_matchInterfaces_errorSelector
  , createMatchingDictionaryWithVendorID_productID_bcdDevice_deviceClass_deviceSubclass_deviceProtocol_speed_productIDArraySelector
  , resetWithErrorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOUSBHost.Internal.Classes
import ObjC.IOKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | Creates a matching dictionary for an IOUSBHostDevice to be passed into              IOServiceGetMatchingService
--
-- @vendorID@ — NSNumber representation of device vendorID
--
-- @productID@ — NSNumber representation of device productID
--
-- @bcdDevice@ — NSNumber representation of device release number
--
-- @deviceClass@ — NSNumber representation of device class
--
-- @deviceSubclass@ — NSNumber representation of device subclass
--
-- @deviceProtocol@ — NSNumber representation of device protocol
--
-- @speed@ — NSNumber representation of device speed
--
-- @productIDArray@ — NSArray of NSNumbers representing all productIDs interested in.              If used do not specify the NSNumber productID field
--
-- Returns: CFMutableDictionaryRef to be used with IOService matching methods. To be released by              caller.
--
-- ObjC selector: @+ createMatchingDictionaryWithVendorID:productID:bcdDevice:deviceClass:deviceSubclass:deviceProtocol:speed:productIDArray:@
createMatchingDictionaryWithVendorID_productID_bcdDevice_deviceClass_deviceSubclass_deviceProtocol_speed_productIDArray :: (IsNSNumber vendorID, IsNSNumber productID, IsNSNumber bcdDevice, IsNSNumber deviceClass, IsNSNumber deviceSubclass, IsNSNumber deviceProtocol, IsNSNumber speed, IsNSArray productIDArray) => vendorID -> productID -> bcdDevice -> deviceClass -> deviceSubclass -> deviceProtocol -> speed -> productIDArray -> IO (Ptr ())
createMatchingDictionaryWithVendorID_productID_bcdDevice_deviceClass_deviceSubclass_deviceProtocol_speed_productIDArray vendorID productID bcdDevice deviceClass deviceSubclass deviceProtocol speed productIDArray =
  do
    cls' <- getRequiredClass "IOUSBHostDevice"
    sendClassMessage cls' createMatchingDictionaryWithVendorID_productID_bcdDevice_deviceClass_deviceSubclass_deviceProtocol_speed_productIDArraySelector (toNSNumber vendorID) (toNSNumber productID) (toNSNumber bcdDevice) (toNSNumber deviceClass) (toNSNumber deviceSubclass) (toNSNumber deviceProtocol) (toNSNumber speed) (toNSArray productIDArray)

-- | Select a new configuration for the device
--
-- This method will select a new configuration for a device. If the device was              previously configured all child interfaces will be terminated prior to setting              the new configuration.  This method will send the SET_CONFIGURATION control request              (USB 2.0 9.4.7) to the device. The interfaces will be registered for matching by              default. After the completion of this call, the interfaces are not guaranteed              to be immediately available.
--
-- @value@ — Configuration value to select
--
-- @matchInterfaces@ — If YES, any interfaces within the new configuration will be              registered for matching. By default this is set to YES.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure.
--
-- ObjC selector: @- configureWithValue:matchInterfaces:error:@
configureWithValue_matchInterfaces_error :: (IsIOUSBHostDevice iousbHostDevice, IsNSError error_) => iousbHostDevice -> CULong -> Bool -> error_ -> IO Bool
configureWithValue_matchInterfaces_error iousbHostDevice value matchInterfaces error_ =
  sendMessage iousbHostDevice configureWithValue_matchInterfaces_errorSelector value matchInterfaces (toNSError error_)

-- | Select a new configuration for the device
--
-- This method will select a new configuration for a device.  If the device was              previously configured all child interfaces will be terminated prior to setting              the new configuration.  This method will send the SET_CONFIGURATION control request              (USB 2.0 9.4.7) to the device. The interfaces will be registered for matching by              default. After the completion of this call, the interfaces are not guaranteed              to be immediately available.
--
-- @value@ — Configuration value to select
--
-- Returns: YES on success, an IOReturn error code will be reported on failure.
--
-- ObjC selector: @- configureWithValue:error:@
configureWithValue_error :: (IsIOUSBHostDevice iousbHostDevice, IsNSError error_) => iousbHostDevice -> CULong -> error_ -> IO Bool
configureWithValue_error iousbHostDevice value error_ =
  sendMessage iousbHostDevice configureWithValue_errorSelector value (toNSError error_)

-- | Terminate the device and attempt to reenumerate it
--
-- This function will reset and attempt to reenumerate the USB device.              The current IOUSBHostDevice object and all of its children will be terminated.              A new IOUSBHostDevice IOService object will be created and registered if the reset              is successful and the previous object has finished terminating. The framework IOUSBHostDevice              will no longer have a valid connection with IOService userclient after the call returns              successfully. A new framework client will need to be created to use the re-enumerated device.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure.
--
-- ObjC selector: @- resetWithError:@
resetWithError :: (IsIOUSBHostDevice iousbHostDevice, IsNSError error_) => iousbHostDevice -> error_ -> IO Bool
resetWithError iousbHostDevice error_ =
  sendMessage iousbHostDevice resetWithErrorSelector (toNSError error_)

-- | Return the currently selected configuration descriptor
--
-- This method uses descriptorWithType to return the configuration descriptor currently              selected after a successful setConfiguration call
--
-- Returns: Pointer to the configuration descriptor if found, or nil if the device is not              configured
--
-- ObjC selector: @- configurationDescriptor@
configurationDescriptor :: IsIOUSBHostDevice iousbHostDevice => iousbHostDevice -> IO (Const (Ptr IOUSBConfigurationDescriptor))
configurationDescriptor iousbHostDevice =
  sendMessage iousbHostDevice configurationDescriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createMatchingDictionaryWithVendorID:productID:bcdDevice:deviceClass:deviceSubclass:deviceProtocol:speed:productIDArray:@
createMatchingDictionaryWithVendorID_productID_bcdDevice_deviceClass_deviceSubclass_deviceProtocol_speed_productIDArraySelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, Id NSArray] (Ptr ())
createMatchingDictionaryWithVendorID_productID_bcdDevice_deviceClass_deviceSubclass_deviceProtocol_speed_productIDArraySelector = mkSelector "createMatchingDictionaryWithVendorID:productID:bcdDevice:deviceClass:deviceSubclass:deviceProtocol:speed:productIDArray:"

-- | @Selector@ for @configureWithValue:matchInterfaces:error:@
configureWithValue_matchInterfaces_errorSelector :: Selector '[CULong, Bool, Id NSError] Bool
configureWithValue_matchInterfaces_errorSelector = mkSelector "configureWithValue:matchInterfaces:error:"

-- | @Selector@ for @configureWithValue:error:@
configureWithValue_errorSelector :: Selector '[CULong, Id NSError] Bool
configureWithValue_errorSelector = mkSelector "configureWithValue:error:"

-- | @Selector@ for @resetWithError:@
resetWithErrorSelector :: Selector '[Id NSError] Bool
resetWithErrorSelector = mkSelector "resetWithError:"

-- | @Selector@ for @configurationDescriptor@
configurationDescriptorSelector :: Selector '[] (Const (Ptr IOUSBConfigurationDescriptor))
configurationDescriptorSelector = mkSelector "configurationDescriptor"

