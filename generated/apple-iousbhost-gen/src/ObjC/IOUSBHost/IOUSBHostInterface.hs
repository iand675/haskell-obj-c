{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOUSBHostInterface
--
-- The IOUSBHostObject representing a USB interface
--
-- This class provides functionality to send control requests to the default control endpoint, as well as              create IOUSBHostPipe objects to transfer data.
--
-- Generated bindings for @IOUSBHostInterface@.
module ObjC.IOUSBHost.IOUSBHostInterface
  ( IOUSBHostInterface
  , IsIOUSBHostInterface(..)
  , createMatchingDictionaryWithVendorID_productID_bcdDevice_interfaceNumber_configurationValue_interfaceClass_interfaceSubclass_interfaceProtocol_speed_productIDArray
  , initWithIOService_options_queue_error_interestHandler
  , setIdleTimeout_error
  , selectAlternateSetting_error
  , copyPipeWithAddress_error
  , idleTimeout
  , configurationDescriptor
  , interfaceDescriptor
  , configurationDescriptorSelector
  , copyPipeWithAddress_errorSelector
  , createMatchingDictionaryWithVendorID_productID_bcdDevice_interfaceNumber_configurationValue_interfaceClass_interfaceSubclass_interfaceProtocol_speed_productIDArraySelector
  , idleTimeoutSelector
  , initWithIOService_options_queue_error_interestHandlerSelector
  , interfaceDescriptorSelector
  , selectAlternateSetting_errorSelector
  , setIdleTimeout_errorSelector

  -- * Enum types
  , IOUSBHostObjectInitOptions(IOUSBHostObjectInitOptions)
  , pattern IOUSBHostObjectInitOptionsNone
  , pattern IOUSBHostObjectInitOptionsDeviceCapture
  , pattern IOUSBHostObjectInitOptionsDeviceSeize

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOUSBHost.Internal.Classes
import ObjC.IOKit.Internal.Structs
import ObjC.IOUSBHost.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a matching dictionary for an IOUSBHostInterface to be passed into              IOServiceGetMatchingService
--
-- @vendorID@ — NSNumber representation of device vendorID
--
-- @productID@ — NSNumber representation of device productID
--
-- @bcdDevice@ — NSNumber representation of device release number
--
-- @interfaceNumber@ — NSNumber representation of interface number
--
-- @configurationValue@ — NSNumber representation of the device's configurationValue
--
-- @interfaceClass@ — NSNumber representation of interface class
--
-- @interfaceSubclass@ — NSNumber representation of interface subclass
--
-- @interfaceProtocol@ — NSNumber representation of interface protocol
--
-- @speed@ — NSNumber representation of device speed
--
-- @productIDArray@ — NSArray of NSNumbers representing all productIDs interested in.              If used do not specify the NSNumber productID field
--
-- Returns: CFMutableDictionaryRef to be used with IOService matching methods. To be released              by caller.
--
-- ObjC selector: @+ createMatchingDictionaryWithVendorID:productID:bcdDevice:interfaceNumber:configurationValue:interfaceClass:interfaceSubclass:interfaceProtocol:speed:productIDArray:@
createMatchingDictionaryWithVendorID_productID_bcdDevice_interfaceNumber_configurationValue_interfaceClass_interfaceSubclass_interfaceProtocol_speed_productIDArray :: (IsNSNumber vendorID, IsNSNumber productID, IsNSNumber bcdDevice, IsNSNumber interfaceNumber, IsNSNumber configurationValue, IsNSNumber interfaceClass, IsNSNumber interfaceSubclass, IsNSNumber interfaceProtocol, IsNSNumber speed, IsNSArray productIDArray) => vendorID -> productID -> bcdDevice -> interfaceNumber -> configurationValue -> interfaceClass -> interfaceSubclass -> interfaceProtocol -> speed -> productIDArray -> IO (Ptr ())
createMatchingDictionaryWithVendorID_productID_bcdDevice_interfaceNumber_configurationValue_interfaceClass_interfaceSubclass_interfaceProtocol_speed_productIDArray vendorID productID bcdDevice interfaceNumber configurationValue interfaceClass interfaceSubclass interfaceProtocol speed productIDArray =
  do
    cls' <- getRequiredClass "IOUSBHostInterface"
    sendClassMessage cls' createMatchingDictionaryWithVendorID_productID_bcdDevice_interfaceNumber_configurationValue_interfaceClass_interfaceSubclass_interfaceProtocol_speed_productIDArraySelector (toNSNumber vendorID) (toNSNumber productID) (toNSNumber bcdDevice) (toNSNumber interfaceNumber) (toNSNumber configurationValue) (toNSNumber interfaceClass) (toNSNumber interfaceSubclass) (toNSNumber interfaceProtocol) (toNSNumber speed) (toNSArray productIDArray)

-- | Initializes IOUSBHostInterface object along with user client
--
-- See IOUSBHostObject for documentation.
--
-- ObjC selector: @- initWithIOService:options:queue:error:interestHandler:@
initWithIOService_options_queue_error_interestHandler :: (IsIOUSBHostInterface iousbHostInterface, IsNSObject queue, IsNSError error_) => iousbHostInterface -> CUInt -> IOUSBHostObjectInitOptions -> queue -> error_ -> Ptr () -> IO (Id IOUSBHostInterface)
initWithIOService_options_queue_error_interestHandler iousbHostInterface ioService options queue error_ interestHandler =
  sendOwnedMessage iousbHostInterface initWithIOService_options_queue_error_interestHandlerSelector ioService options (toNSObject queue) (toNSError error_) interestHandler

-- | Sets the desired idle suspend timeout for the interface
--
-- Once the interface is considered idle, it will defer electrical suspend of the              device for the specified duration.
--
-- @idleTimeout@ — The amount of time after all pipes are idle to              wait before suspending the device.
--
-- Returns: YES on success. An IOReturn error code will be reported on failure.
--
-- ObjC selector: @- setIdleTimeout:error:@
setIdleTimeout_error :: (IsIOUSBHostInterface iousbHostInterface, IsNSError error_) => iousbHostInterface -> CDouble -> error_ -> IO Bool
setIdleTimeout_error iousbHostInterface idleTimeout error_ =
  sendMessage iousbHostInterface setIdleTimeout_errorSelector idleTimeout (toNSError error_)

-- | Select an alternate setting for this interface
--
-- This method is used to select an alternate setting for the interface. All pending IO              on the interface's pipes will be aborted, and the open pipes will be closed. The              IOUSBHostPipe objects already created will no longer be valid. The new alternate              setting will be selected via SET_INTERFACE control request (USB 2.0 9.4.10).
--
-- @alternateSetting@ — Alternate interface number to activate
--
-- Returns: YES on success, an IOReturn error code will be reported on failure.
--
-- ObjC selector: @- selectAlternateSetting:error:@
selectAlternateSetting_error :: (IsIOUSBHostInterface iousbHostInterface, IsNSError error_) => iousbHostInterface -> CULong -> error_ -> IO Bool
selectAlternateSetting_error iousbHostInterface alternateSetting error_ =
  sendMessage iousbHostInterface selectAlternateSetting_errorSelector alternateSetting (toNSError error_)

-- | Return the pipe whose bEndpointAddress matches address
--
-- This method will return the pipe whose bEndpointAddress              matches address. If the pipe is returned successfully, it will maintain              a reference to the IOUSBHostInterface.
--
-- @address@ — Endpoint address of the pipe
--
-- Returns: Pointer to an IOUSBHostPipe object or nil. The IOUSBHostPipe is to be released by the caller.              An IOReturn error code will be reported on failure.
--
-- ObjC selector: @- copyPipeWithAddress:error:@
copyPipeWithAddress_error :: (IsIOUSBHostInterface iousbHostInterface, IsNSError error_) => iousbHostInterface -> CULong -> error_ -> IO (Id IOUSBHostPipe)
copyPipeWithAddress_error iousbHostInterface address error_ =
  sendOwnedMessage iousbHostInterface copyPipeWithAddress_errorSelector address (toNSError error_)

-- | Retrieve the current idle suspend timeout.              See
--
-- setIdleTimeout
--
-- Returns: The amount of time after all pipes are idle to wait before              suspending the device,
--
-- ObjC selector: @- idleTimeout@
idleTimeout :: IsIOUSBHostInterface iousbHostInterface => iousbHostInterface -> IO CDouble
idleTimeout iousbHostInterface =
  sendMessage iousbHostInterface idleTimeoutSelector

-- | Retrieve the configuration descriptor associated with this interface
--
-- Returns: IOUSBConfigurationDescriptor pointer
--
-- ObjC selector: @- configurationDescriptor@
configurationDescriptor :: IsIOUSBHostInterface iousbHostInterface => iousbHostInterface -> IO (Const (Ptr IOUSBConfigurationDescriptor))
configurationDescriptor iousbHostInterface =
  sendMessage iousbHostInterface configurationDescriptorSelector

-- | Retrieve the interface descriptor associated with this interface.
--
-- Returns: IOUSBInterfaceDescriptor pointer
--
-- ObjC selector: @- interfaceDescriptor@
interfaceDescriptor :: IsIOUSBHostInterface iousbHostInterface => iousbHostInterface -> IO (Const (Ptr IOUSBInterfaceDescriptor))
interfaceDescriptor iousbHostInterface =
  sendMessage iousbHostInterface interfaceDescriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createMatchingDictionaryWithVendorID:productID:bcdDevice:interfaceNumber:configurationValue:interfaceClass:interfaceSubclass:interfaceProtocol:speed:productIDArray:@
createMatchingDictionaryWithVendorID_productID_bcdDevice_interfaceNumber_configurationValue_interfaceClass_interfaceSubclass_interfaceProtocol_speed_productIDArraySelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, Id NSArray] (Ptr ())
createMatchingDictionaryWithVendorID_productID_bcdDevice_interfaceNumber_configurationValue_interfaceClass_interfaceSubclass_interfaceProtocol_speed_productIDArraySelector = mkSelector "createMatchingDictionaryWithVendorID:productID:bcdDevice:interfaceNumber:configurationValue:interfaceClass:interfaceSubclass:interfaceProtocol:speed:productIDArray:"

-- | @Selector@ for @initWithIOService:options:queue:error:interestHandler:@
initWithIOService_options_queue_error_interestHandlerSelector :: Selector '[CUInt, IOUSBHostObjectInitOptions, Id NSObject, Id NSError, Ptr ()] (Id IOUSBHostInterface)
initWithIOService_options_queue_error_interestHandlerSelector = mkSelector "initWithIOService:options:queue:error:interestHandler:"

-- | @Selector@ for @setIdleTimeout:error:@
setIdleTimeout_errorSelector :: Selector '[CDouble, Id NSError] Bool
setIdleTimeout_errorSelector = mkSelector "setIdleTimeout:error:"

-- | @Selector@ for @selectAlternateSetting:error:@
selectAlternateSetting_errorSelector :: Selector '[CULong, Id NSError] Bool
selectAlternateSetting_errorSelector = mkSelector "selectAlternateSetting:error:"

-- | @Selector@ for @copyPipeWithAddress:error:@
copyPipeWithAddress_errorSelector :: Selector '[CULong, Id NSError] (Id IOUSBHostPipe)
copyPipeWithAddress_errorSelector = mkSelector "copyPipeWithAddress:error:"

-- | @Selector@ for @idleTimeout@
idleTimeoutSelector :: Selector '[] CDouble
idleTimeoutSelector = mkSelector "idleTimeout"

-- | @Selector@ for @configurationDescriptor@
configurationDescriptorSelector :: Selector '[] (Const (Ptr IOUSBConfigurationDescriptor))
configurationDescriptorSelector = mkSelector "configurationDescriptor"

-- | @Selector@ for @interfaceDescriptor@
interfaceDescriptorSelector :: Selector '[] (Const (Ptr IOUSBInterfaceDescriptor))
interfaceDescriptorSelector = mkSelector "interfaceDescriptor"

