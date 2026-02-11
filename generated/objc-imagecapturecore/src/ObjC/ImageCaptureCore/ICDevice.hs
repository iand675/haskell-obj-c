{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICDevice
--
-- ICDevice is an abstract class that represents a device supported by Image Capture facility. ImageCaptureCore defines two concrete subclasses of ICDevice, ICCameraDevice and ICScannerDevice. ICDeviceBrowser creates instances of these two subclasses to represent cameras and scanners it finds.
--
-- Generated bindings for @ICDevice@.
module ObjC.ImageCaptureCore.ICDevice
  ( ICDevice
  , IsICDevice(..)
  , requestOpenSession
  , requestCloseSession
  , requestEject
  , requestOpenSessionWithOptions_completion
  , requestCloseSessionWithOptions_completion
  , requestEjectWithCompletion
  , requestSendMessage_outData_maxReturnedDataSize_sendMessageDelegate_didSendMessageSelector_contextInfo
  , requestEjectOrDisconnect
  , requestYield
  , type_
  , capabilities
  , name
  , productKind
  , icon
  , systemSymbolName
  , transportType
  , uuidString
  , hasOpenSession
  , userData
  , usbLocationID
  , usbProductID
  , usbVendorID
  , remote
  , moduleExecutableArchitecture
  , requestOpenSessionSelector
  , requestCloseSessionSelector
  , requestEjectSelector
  , requestOpenSessionWithOptions_completionSelector
  , requestCloseSessionWithOptions_completionSelector
  , requestEjectWithCompletionSelector
  , requestSendMessage_outData_maxReturnedDataSize_sendMessageDelegate_didSendMessageSelector_contextInfoSelector
  , requestEjectOrDisconnectSelector
  , requestYieldSelector
  , typeSelector
  , capabilitiesSelector
  , nameSelector
  , productKindSelector
  , iconSelector
  , systemSymbolNameSelector
  , transportTypeSelector
  , uuidStringSelector
  , hasOpenSessionSelector
  , userDataSelector
  , usbLocationIDSelector
  , usbProductIDSelector
  , usbVendorIDSelector
  , remoteSelector
  , moduleExecutableArchitectureSelector

  -- * Enum types
  , ICDeviceType(ICDeviceType)
  , pattern ICDeviceTypeCamera
  , pattern ICDeviceTypeScanner

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

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.ImageCaptureCore.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | requestOpenSession
--
-- This message requests to open a session on the device.
--
-- Make sure the receiver's delegate is set prior to sending this message; otherwise this message will be ignored. This request is completed when the delegate receives a "device:didOpenSessionWithError:" message.
--
-- Note: Execution of the delegate callback will occur on the main thread.
--
-- ObjC selector: @- requestOpenSession@
requestOpenSession :: IsICDevice icDevice => icDevice -> IO ()
requestOpenSession icDevice  =
  sendMsg icDevice (mkSelector "requestOpenSession") retVoid []

-- | requestCloseSession
--
-- This message requests to close a previously opened session on this device.
--
-- This request is completed when the delegate receives a "device:didCloseSessionWithError:" message.
--
-- Note: Execution of the delegate callback will occur on the main thread.
--
-- ObjC selector: @- requestCloseSession@
requestCloseSession :: IsICDevice icDevice => icDevice -> IO ()
requestCloseSession icDevice  =
  sendMsg icDevice (mkSelector "requestCloseSession") retVoid []

-- | requestEject
--
-- Eject the media if permitted by the device, or disconnect from a remote device.
--
-- ObjC selector: @- requestEject@
requestEject :: IsICDevice icDevice => icDevice -> IO ()
requestEject icDevice  =
  sendMsg icDevice (mkSelector "requestEject") retVoid []

-- | requestOpenSessionWithOptions:completion
--
-- This message requests to open a session on the device.
--
-- This request will execute the completion handler provided upon return.
--
-- Note: The completion block will execute on an any available queue, often this will not be the main queue.
--
-- ObjC selector: @- requestOpenSessionWithOptions:completion:@
requestOpenSessionWithOptions_completion :: (IsICDevice icDevice, IsNSDictionary options) => icDevice -> options -> Ptr () -> IO ()
requestOpenSessionWithOptions_completion icDevice  options completion =
withObjCPtr options $ \raw_options ->
    sendMsg icDevice (mkSelector "requestOpenSessionWithOptions:completion:") retVoid [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | requestCloseSessionWithOptions:completion
--
-- This message requests to close a previously opened session on this device.
--
-- This request will execute the completion handler provided upon return.
--
-- Note: The completion block will execute on an any available queue, often this will not be the main queue.
--
-- ObjC selector: @- requestCloseSessionWithOptions:completion:@
requestCloseSessionWithOptions_completion :: (IsICDevice icDevice, IsNSDictionary options) => icDevice -> options -> Ptr () -> IO ()
requestCloseSessionWithOptions_completion icDevice  options completion =
withObjCPtr options $ \raw_options ->
    sendMsg icDevice (mkSelector "requestCloseSessionWithOptions:completion:") retVoid [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | requestEjectWithCompletion:
--
-- Eject the media, or disconnect the device - if permitted by the device.
--
-- This request will execute the completion handler provided upon return.
--
-- Note: The completion block will execute on an any available queue, often this will not be the main queue.
--
-- ObjC selector: @- requestEjectWithCompletion:@
requestEjectWithCompletion :: IsICDevice icDevice => icDevice -> Ptr () -> IO ()
requestEjectWithCompletion icDevice  completion =
  sendMsg icDevice (mkSelector "requestEjectWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | requestSendMessage:outData:maxReturnDataSize:sendMessageDelegate:didSendMessageSelector:contextInfo:
--
-- This method asynchronously sends an arbitrary message with optional data to a device.
--
-- This method allows developers to send a private message from a client application to a device module.
--
-- The response to this command will be delivered using didSendMessageSelector of sendMessageDelegate. The didSendMessageSelector should have the same signature as: - (void)didSendMessage:(UInt32)messageCode inData:(NSData*)data error:(NSError*)error contextInfo:(void*)contextInfo.
--
-- The content of error returned should be examined to determine if the request completed successfully.
--
-- Note: This method should not be used to send PTP pass-through commands to a PTP camera. Please refer to 'requestSendPTPCommand:outData:sendCommandDelegate:sendCommandDelegate:contextInfo:' defined in ICCameraDevice.h for sending PTP pass-through commands.
--
-- Note: Execution of the delegate callback will occur on the main thread.
--
-- ObjC selector: @- requestSendMessage:outData:maxReturnedDataSize:sendMessageDelegate:didSendMessageSelector:contextInfo:@
requestSendMessage_outData_maxReturnedDataSize_sendMessageDelegate_didSendMessageSelector_contextInfo :: (IsICDevice icDevice, IsNSData data_) => icDevice -> CUInt -> data_ -> CUInt -> RawId -> Selector -> Ptr () -> IO ()
requestSendMessage_outData_maxReturnedDataSize_sendMessageDelegate_didSendMessageSelector_contextInfo icDevice  messageCode data_ maxReturnedDataSize sendMessageDelegate selector contextInfo =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg icDevice (mkSelector "requestSendMessage:outData:maxReturnedDataSize:sendMessageDelegate:didSendMessageSelector:contextInfo:") retVoid [argCUInt (fromIntegral messageCode), argPtr (castPtr raw_data_ :: Ptr ()), argCUInt (fromIntegral maxReturnedDataSize), argPtr (castPtr (unRawId sendMessageDelegate) :: Ptr ()), argPtr (unSelector selector), argPtr contextInfo]

-- | requestEjectOrDisconnect
--
-- Eject the media if permitted by the device, or disconnect from a remote device.
--
-- ObjC selector: @- requestEjectOrDisconnect@
requestEjectOrDisconnect :: IsICDevice icDevice => icDevice -> IO ()
requestEjectOrDisconnect icDevice  =
  sendMsg icDevice (mkSelector "requestEjectOrDisconnect") retVoid []

-- | requestYield
--
-- This message requests the device module in control of this device to yield control.
--
-- This message should be used only if the client is planning on communicating with the device directly. The device module may not yield control of the device if it has an open session.
--
-- ObjC selector: @- requestYield@
requestYield :: IsICDevice icDevice => icDevice -> IO ()
requestYield icDevice  =
  sendMsg icDevice (mkSelector "requestYield") retVoid []

-- | type
--
-- ￼The type of the device as defined by ICDeviceType OR'd with its ICDeviceLocationType.
--
-- Note: The type of this device can be obtained by AND'ing the value retuned by this property with an appropriate ICDeviceTypeMask.
--
-- Note: The location type of this device can be obtained by AND'ing the value retuned by this property with an appropriate ICDeviceLocationTypeMask.
--
-- ObjC selector: @- type@
type_ :: IsICDevice icDevice => icDevice -> IO ICDeviceType
type_ icDevice  =
  fmap (coerce :: CULong -> ICDeviceType) $ sendMsg icDevice (mkSelector "type") retCULong []

-- | capabilities
--
-- ￼The capabilities of the device as reported by the device module.
--
-- ObjC selector: @- capabilities@
capabilities :: IsICDevice icDevice => icDevice -> IO (Id NSArray)
capabilities icDevice  =
  sendMsg icDevice (mkSelector "capabilities") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- ￼Name of the device as reported by the device module or by the device transport when a device module is not in control of this device.
--
-- Note: This name may change if the device module overrides the default name of the device reported by the device's transport, or if the name of the filesystem volume mounted by the device is changed by the user.
--
-- ObjC selector: @- name@
name :: IsICDevice icDevice => icDevice -> IO (Id NSString)
name icDevice  =
  sendMsg icDevice (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | productKind
--
-- ￼Type of the device. Possible values are: "iPhone", \@"iPod", \@"iPad", \@"Camera", \@"Scanner"
--
-- ObjC selector: @- productKind@
productKind :: IsICDevice icDevice => icDevice -> IO (Id NSString)
productKind icDevice  =
  sendMsg icDevice (mkSelector "productKind") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | icon
--
-- ￼Icon image for the device class.  If there is no custom icon present from a device manufacturer, this will be a rendered version of the system symbol for the device class.  Using a rendered system symbol instead of the systemSymbolName is discouraged.
--
-- ObjC selector: @- icon@
icon :: IsICDevice icDevice => icDevice -> IO (Ptr ())
icon icDevice  =
  fmap castPtr $ sendMsg icDevice (mkSelector "icon") (retPtr retVoid) []

-- | systemSymbolName
--
-- ￼Standard system symbol used to represent the device class.  Using the symbol to render an appropriate device icon will ensure proper scaling for high resolution devices.
--
-- ObjC selector: @- systemSymbolName@
systemSymbolName :: IsICDevice icDevice => icDevice -> IO (Id NSString)
systemSymbolName icDevice  =
  sendMsg icDevice (mkSelector "systemSymbolName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | transportType
--
-- ￼The transport type used by the device. The possible values are: ICTransportTypeUSB or ICTransportTypeMassStorage.
--
-- ObjC selector: @- transportType@
transportType :: IsICDevice icDevice => icDevice -> IO (Id NSString)
transportType icDevice  =
  sendMsg icDevice (mkSelector "transportType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | UUIDString
--
-- ￼A string representation of the Universally Unique ID of the device.
--
-- ObjC selector: @- UUIDString@
uuidString :: IsICDevice icDevice => icDevice -> IO (Id NSString)
uuidString icDevice  =
  sendMsg icDevice (mkSelector "UUIDString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | hasOpenSession
--
-- ￼Indicates whether the device has an open session.
--
-- ObjC selector: @- hasOpenSession@
hasOpenSession :: IsICDevice icDevice => icDevice -> IO Bool
hasOpenSession icDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg icDevice (mkSelector "hasOpenSession") retCULong []

-- | userData
--
-- ￼Client convenience bookkeeping object retained by the framework.
--
-- ObjC selector: @- userData@
userData :: IsICDevice icDevice => icDevice -> IO (Id NSMutableDictionary)
userData icDevice  =
  sendMsg icDevice (mkSelector "userData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | usbLocationID
--
-- ￼The USB location of which the device is occupying.
--
-- ObjC selector: @- usbLocationID@
usbLocationID :: IsICDevice icDevice => icDevice -> IO CInt
usbLocationID icDevice  =
  sendMsg icDevice (mkSelector "usbLocationID") retCInt []

-- | usbProductID
--
-- ￼The USB PID associated with the device attached.
--
-- ObjC selector: @- usbProductID@
usbProductID :: IsICDevice icDevice => icDevice -> IO CInt
usbProductID icDevice  =
  sendMsg icDevice (mkSelector "usbProductID") retCInt []

-- | usbVendorID
--
-- ￼The USB VID associated with the device attached.
--
-- ObjC selector: @- usbVendorID@
usbVendorID :: IsICDevice icDevice => icDevice -> IO CInt
usbVendorID icDevice  =
  sendMsg icDevice (mkSelector "usbVendorID") retCInt []

-- | remote
--
-- ￼Indicates whether the device is a remote device published by Image Capture device sharing facility.
--
-- name
--
-- ￼Name of the device as reported by the device module or by the device transport when a device module is not in control of this device.
--
-- Note: This name may change if the device module overrides the default name of the device reported by the device's transport, or if the name of the filesystem volume mounted by the device is changed by the user.
--
-- ObjC selector: @- remote@
remote :: IsICDevice icDevice => icDevice -> IO Bool
remote icDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg icDevice (mkSelector "remote") retCULong []

-- | moduleExecutableArchitecture
--
-- Reports the device module servicing the requests executable architecture.
--
-- ObjC selector: @- moduleExecutableArchitecture@
moduleExecutableArchitecture :: IsICDevice icDevice => icDevice -> IO CInt
moduleExecutableArchitecture icDevice  =
  sendMsg icDevice (mkSelector "moduleExecutableArchitecture") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestOpenSession@
requestOpenSessionSelector :: Selector
requestOpenSessionSelector = mkSelector "requestOpenSession"

-- | @Selector@ for @requestCloseSession@
requestCloseSessionSelector :: Selector
requestCloseSessionSelector = mkSelector "requestCloseSession"

-- | @Selector@ for @requestEject@
requestEjectSelector :: Selector
requestEjectSelector = mkSelector "requestEject"

-- | @Selector@ for @requestOpenSessionWithOptions:completion:@
requestOpenSessionWithOptions_completionSelector :: Selector
requestOpenSessionWithOptions_completionSelector = mkSelector "requestOpenSessionWithOptions:completion:"

-- | @Selector@ for @requestCloseSessionWithOptions:completion:@
requestCloseSessionWithOptions_completionSelector :: Selector
requestCloseSessionWithOptions_completionSelector = mkSelector "requestCloseSessionWithOptions:completion:"

-- | @Selector@ for @requestEjectWithCompletion:@
requestEjectWithCompletionSelector :: Selector
requestEjectWithCompletionSelector = mkSelector "requestEjectWithCompletion:"

-- | @Selector@ for @requestSendMessage:outData:maxReturnedDataSize:sendMessageDelegate:didSendMessageSelector:contextInfo:@
requestSendMessage_outData_maxReturnedDataSize_sendMessageDelegate_didSendMessageSelector_contextInfoSelector :: Selector
requestSendMessage_outData_maxReturnedDataSize_sendMessageDelegate_didSendMessageSelector_contextInfoSelector = mkSelector "requestSendMessage:outData:maxReturnedDataSize:sendMessageDelegate:didSendMessageSelector:contextInfo:"

-- | @Selector@ for @requestEjectOrDisconnect@
requestEjectOrDisconnectSelector :: Selector
requestEjectOrDisconnectSelector = mkSelector "requestEjectOrDisconnect"

-- | @Selector@ for @requestYield@
requestYieldSelector :: Selector
requestYieldSelector = mkSelector "requestYield"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @capabilities@
capabilitiesSelector :: Selector
capabilitiesSelector = mkSelector "capabilities"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @productKind@
productKindSelector :: Selector
productKindSelector = mkSelector "productKind"

-- | @Selector@ for @icon@
iconSelector :: Selector
iconSelector = mkSelector "icon"

-- | @Selector@ for @systemSymbolName@
systemSymbolNameSelector :: Selector
systemSymbolNameSelector = mkSelector "systemSymbolName"

-- | @Selector@ for @transportType@
transportTypeSelector :: Selector
transportTypeSelector = mkSelector "transportType"

-- | @Selector@ for @UUIDString@
uuidStringSelector :: Selector
uuidStringSelector = mkSelector "UUIDString"

-- | @Selector@ for @hasOpenSession@
hasOpenSessionSelector :: Selector
hasOpenSessionSelector = mkSelector "hasOpenSession"

-- | @Selector@ for @userData@
userDataSelector :: Selector
userDataSelector = mkSelector "userData"

-- | @Selector@ for @usbLocationID@
usbLocationIDSelector :: Selector
usbLocationIDSelector = mkSelector "usbLocationID"

-- | @Selector@ for @usbProductID@
usbProductIDSelector :: Selector
usbProductIDSelector = mkSelector "usbProductID"

-- | @Selector@ for @usbVendorID@
usbVendorIDSelector :: Selector
usbVendorIDSelector = mkSelector "usbVendorID"

-- | @Selector@ for @remote@
remoteSelector :: Selector
remoteSelector = mkSelector "remote"

-- | @Selector@ for @moduleExecutableArchitecture@
moduleExecutableArchitectureSelector :: Selector
moduleExecutableArchitectureSelector = mkSelector "moduleExecutableArchitecture"

