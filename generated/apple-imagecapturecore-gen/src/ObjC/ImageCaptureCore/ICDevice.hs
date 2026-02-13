{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , type_
  , capabilities
  , name
  , productKind
  , icon
  , systemSymbolName
  , transportType
  , uuidString
  , locationDescription
  , hasOpenSession
  , userData
  , modulePath
  , moduleVersion
  , serialNumberString
  , usbLocationID
  , usbProductID
  , usbVendorID
  , autolaunchApplicationPath
  , setAutolaunchApplicationPath
  , remote
  , persistentIDString
  , moduleExecutableArchitecture
  , autolaunchApplicationPathSelector
  , capabilitiesSelector
  , delegateSelector
  , hasOpenSessionSelector
  , iconSelector
  , locationDescriptionSelector
  , moduleExecutableArchitectureSelector
  , modulePathSelector
  , moduleVersionSelector
  , nameSelector
  , persistentIDStringSelector
  , productKindSelector
  , remoteSelector
  , requestCloseSessionSelector
  , requestCloseSessionWithOptions_completionSelector
  , requestEjectOrDisconnectSelector
  , requestEjectSelector
  , requestEjectWithCompletionSelector
  , requestOpenSessionSelector
  , requestOpenSessionWithOptions_completionSelector
  , requestSendMessage_outData_maxReturnedDataSize_sendMessageDelegate_didSendMessageSelector_contextInfoSelector
  , requestYieldSelector
  , serialNumberStringSelector
  , setAutolaunchApplicationPathSelector
  , setDelegateSelector
  , systemSymbolNameSelector
  , transportTypeSelector
  , typeSelector
  , usbLocationIDSelector
  , usbProductIDSelector
  , usbVendorIDSelector
  , userDataSelector
  , uuidStringSelector

  -- * Enum types
  , ICDeviceType(ICDeviceType)
  , pattern ICDeviceTypeCamera
  , pattern ICDeviceTypeScanner

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
requestOpenSession icDevice =
  sendMessage icDevice requestOpenSessionSelector

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
requestCloseSession icDevice =
  sendMessage icDevice requestCloseSessionSelector

-- | requestEject
--
-- Eject the media if permitted by the device, or disconnect from a remote device.
--
-- ObjC selector: @- requestEject@
requestEject :: IsICDevice icDevice => icDevice -> IO ()
requestEject icDevice =
  sendMessage icDevice requestEjectSelector

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
requestOpenSessionWithOptions_completion icDevice options completion =
  sendMessage icDevice requestOpenSessionWithOptions_completionSelector (toNSDictionary options) completion

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
requestCloseSessionWithOptions_completion icDevice options completion =
  sendMessage icDevice requestCloseSessionWithOptions_completionSelector (toNSDictionary options) completion

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
requestEjectWithCompletion icDevice completion =
  sendMessage icDevice requestEjectWithCompletionSelector completion

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
requestSendMessage_outData_maxReturnedDataSize_sendMessageDelegate_didSendMessageSelector_contextInfo :: (IsICDevice icDevice, IsNSData data_) => icDevice -> CUInt -> data_ -> CUInt -> RawId -> Sel -> Ptr () -> IO ()
requestSendMessage_outData_maxReturnedDataSize_sendMessageDelegate_didSendMessageSelector_contextInfo icDevice messageCode data_ maxReturnedDataSize sendMessageDelegate selector contextInfo =
  sendMessage icDevice requestSendMessage_outData_maxReturnedDataSize_sendMessageDelegate_didSendMessageSelector_contextInfoSelector messageCode (toNSData data_) maxReturnedDataSize sendMessageDelegate selector contextInfo

-- | requestEjectOrDisconnect
--
-- Eject the media if permitted by the device, or disconnect from a remote device.
--
-- ObjC selector: @- requestEjectOrDisconnect@
requestEjectOrDisconnect :: IsICDevice icDevice => icDevice -> IO ()
requestEjectOrDisconnect icDevice =
  sendMessage icDevice requestEjectOrDisconnectSelector

-- | requestYield
--
-- This message requests the device module in control of this device to yield control.
--
-- This message should be used only if the client is planning on communicating with the device directly. The device module may not yield control of the device if it has an open session.
--
-- ObjC selector: @- requestYield@
requestYield :: IsICDevice icDevice => icDevice -> IO ()
requestYield icDevice =
  sendMessage icDevice requestYieldSelector

-- | delegate
--
-- The delegate to receive messages once a session is opened on the device.
--
-- The delegate must conform ICDeviceDelegate protocol. In addition it should respond to selectors defined in ICCameraDeviceDelegate protocol in order to effectively interact with the device object. The messages this delegate can expect to receive are described by these protocols.
--
-- ObjC selector: @- delegate@
delegate :: IsICDevice icDevice => icDevice -> IO RawId
delegate icDevice =
  sendMessage icDevice delegateSelector

-- | delegate
--
-- The delegate to receive messages once a session is opened on the device.
--
-- The delegate must conform ICDeviceDelegate protocol. In addition it should respond to selectors defined in ICCameraDeviceDelegate protocol in order to effectively interact with the device object. The messages this delegate can expect to receive are described by these protocols.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsICDevice icDevice => icDevice -> RawId -> IO ()
setDelegate icDevice value =
  sendMessage icDevice setDelegateSelector value

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
type_ icDevice =
  sendMessage icDevice typeSelector

-- | capabilities
--
-- ￼The capabilities of the device as reported by the device module.
--
-- ObjC selector: @- capabilities@
capabilities :: IsICDevice icDevice => icDevice -> IO (Id NSArray)
capabilities icDevice =
  sendMessage icDevice capabilitiesSelector

-- | name
--
-- ￼Name of the device as reported by the device module or by the device transport when a device module is not in control of this device.
--
-- Note: This name may change if the device module overrides the default name of the device reported by the device's transport, or if the name of the filesystem volume mounted by the device is changed by the user.
--
-- ObjC selector: @- name@
name :: IsICDevice icDevice => icDevice -> IO (Id NSString)
name icDevice =
  sendMessage icDevice nameSelector

-- | productKind
--
-- ￼Type of the device. Possible values are: "iPhone", \@"iPod", \@"iPad", \@"Camera", \@"Scanner"
--
-- ObjC selector: @- productKind@
productKind :: IsICDevice icDevice => icDevice -> IO (Id NSString)
productKind icDevice =
  sendMessage icDevice productKindSelector

-- | icon
--
-- ￼Icon image for the device class.  If there is no custom icon present from a device manufacturer, this will be a rendered version of the system symbol for the device class.  Using a rendered system symbol instead of the systemSymbolName is discouraged.
--
-- ObjC selector: @- icon@
icon :: IsICDevice icDevice => icDevice -> IO (Ptr ())
icon icDevice =
  sendMessage icDevice iconSelector

-- | systemSymbolName
--
-- ￼Standard system symbol used to represent the device class.  Using the symbol to render an appropriate device icon will ensure proper scaling for high resolution devices.
--
-- ObjC selector: @- systemSymbolName@
systemSymbolName :: IsICDevice icDevice => icDevice -> IO (Id NSString)
systemSymbolName icDevice =
  sendMessage icDevice systemSymbolNameSelector

-- | transportType
--
-- ￼The transport type used by the device. The possible values are: ICTransportTypeUSB or ICTransportTypeMassStorage.
--
-- ObjC selector: @- transportType@
transportType :: IsICDevice icDevice => icDevice -> IO (Id NSString)
transportType icDevice =
  sendMessage icDevice transportTypeSelector

-- | UUIDString
--
-- ￼A string representation of the Universally Unique ID of the device.
--
-- ObjC selector: @- UUIDString@
uuidString :: IsICDevice icDevice => icDevice -> IO (Id NSString)
uuidString icDevice =
  sendMessage icDevice uuidStringSelector

-- | locationDescription
--
-- ￼A non-localized location description string for the device.
--
-- The value returned in one of the location description strings defined above, or location obtained from the Bonjour TXT record of a network device.
--
-- ObjC selector: @- locationDescription@
locationDescription :: IsICDevice icDevice => icDevice -> IO RawId
locationDescription icDevice =
  sendMessage icDevice locationDescriptionSelector

-- | hasOpenSession
--
-- ￼Indicates whether the device has an open session.
--
-- ObjC selector: @- hasOpenSession@
hasOpenSession :: IsICDevice icDevice => icDevice -> IO Bool
hasOpenSession icDevice =
  sendMessage icDevice hasOpenSessionSelector

-- | userData
--
-- ￼Client convenience bookkeeping object retained by the framework.
--
-- ObjC selector: @- userData@
userData :: IsICDevice icDevice => icDevice -> IO (Id NSMutableDictionary)
userData icDevice =
  sendMessage icDevice userDataSelector

-- | modulePath
--
-- ￼Filesystem path of the device module that is associated with this device. Camera-specific capabilities are defined in ICCameraDevice.h and scanner-specific capabilities are defined in ICScannerDevice.h.
--
-- ObjC selector: @- modulePath@
modulePath :: IsICDevice icDevice => icDevice -> IO RawId
modulePath icDevice =
  sendMessage icDevice modulePathSelector

-- | moduleVersion
--
-- ￼The bundle version of the device module associated with this device.
--
-- Note: This may change if an existing device module associated with this device is updated or a new device module for this device is installed.
--
-- ObjC selector: @- moduleVersion@
moduleVersion :: IsICDevice icDevice => icDevice -> IO RawId
moduleVersion icDevice =
  sendMessage icDevice moduleVersionSelector

-- | serialNumberString
--
-- ￼The serial number of the device. This will be NULL if the device does not provide a serial number.
--
-- ObjC selector: @- serialNumberString@
serialNumberString :: IsICDevice icDevice => icDevice -> IO RawId
serialNumberString icDevice =
  sendMessage icDevice serialNumberStringSelector

-- | usbLocationID
--
-- ￼The USB location of which the device is occupying.
--
-- ObjC selector: @- usbLocationID@
usbLocationID :: IsICDevice icDevice => icDevice -> IO CInt
usbLocationID icDevice =
  sendMessage icDevice usbLocationIDSelector

-- | usbProductID
--
-- ￼The USB PID associated with the device attached.
--
-- ObjC selector: @- usbProductID@
usbProductID :: IsICDevice icDevice => icDevice -> IO CInt
usbProductID icDevice =
  sendMessage icDevice usbProductIDSelector

-- | usbVendorID
--
-- ￼The USB VID associated with the device attached.
--
-- ObjC selector: @- usbVendorID@
usbVendorID :: IsICDevice icDevice => icDevice -> IO CInt
usbVendorID icDevice =
  sendMessage icDevice usbVendorIDSelector

-- | autolaunchApplicationPath
--
-- ￼Filesystem path of an application that is to be automatically launched when this device is added.
--
-- This property is unavailable for devices of ICTransportTypeProximity.
--
-- ObjC selector: @- autolaunchApplicationPath@
autolaunchApplicationPath :: IsICDevice icDevice => icDevice -> IO RawId
autolaunchApplicationPath icDevice =
  sendMessage icDevice autolaunchApplicationPathSelector

-- | autolaunchApplicationPath
--
-- ￼Filesystem path of an application that is to be automatically launched when this device is added.
--
-- This property is unavailable for devices of ICTransportTypeProximity.
--
-- ObjC selector: @- setAutolaunchApplicationPath:@
setAutolaunchApplicationPath :: IsICDevice icDevice => icDevice -> RawId -> IO ()
setAutolaunchApplicationPath icDevice value =
  sendMessage icDevice setAutolaunchApplicationPathSelector value

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
remote icDevice =
  sendMessage icDevice remoteSelector

-- | persistentIDString
--
-- ￼A string representation of the persistent ID of the device.
--
-- ObjC selector: @- persistentIDString@
persistentIDString :: IsICDevice icDevice => icDevice -> IO RawId
persistentIDString icDevice =
  sendMessage icDevice persistentIDStringSelector

-- | moduleExecutableArchitecture
--
-- Reports the device module servicing the requests executable architecture.
--
-- ObjC selector: @- moduleExecutableArchitecture@
moduleExecutableArchitecture :: IsICDevice icDevice => icDevice -> IO CInt
moduleExecutableArchitecture icDevice =
  sendMessage icDevice moduleExecutableArchitectureSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestOpenSession@
requestOpenSessionSelector :: Selector '[] ()
requestOpenSessionSelector = mkSelector "requestOpenSession"

-- | @Selector@ for @requestCloseSession@
requestCloseSessionSelector :: Selector '[] ()
requestCloseSessionSelector = mkSelector "requestCloseSession"

-- | @Selector@ for @requestEject@
requestEjectSelector :: Selector '[] ()
requestEjectSelector = mkSelector "requestEject"

-- | @Selector@ for @requestOpenSessionWithOptions:completion:@
requestOpenSessionWithOptions_completionSelector :: Selector '[Id NSDictionary, Ptr ()] ()
requestOpenSessionWithOptions_completionSelector = mkSelector "requestOpenSessionWithOptions:completion:"

-- | @Selector@ for @requestCloseSessionWithOptions:completion:@
requestCloseSessionWithOptions_completionSelector :: Selector '[Id NSDictionary, Ptr ()] ()
requestCloseSessionWithOptions_completionSelector = mkSelector "requestCloseSessionWithOptions:completion:"

-- | @Selector@ for @requestEjectWithCompletion:@
requestEjectWithCompletionSelector :: Selector '[Ptr ()] ()
requestEjectWithCompletionSelector = mkSelector "requestEjectWithCompletion:"

-- | @Selector@ for @requestSendMessage:outData:maxReturnedDataSize:sendMessageDelegate:didSendMessageSelector:contextInfo:@
requestSendMessage_outData_maxReturnedDataSize_sendMessageDelegate_didSendMessageSelector_contextInfoSelector :: Selector '[CUInt, Id NSData, CUInt, RawId, Sel, Ptr ()] ()
requestSendMessage_outData_maxReturnedDataSize_sendMessageDelegate_didSendMessageSelector_contextInfoSelector = mkSelector "requestSendMessage:outData:maxReturnedDataSize:sendMessageDelegate:didSendMessageSelector:contextInfo:"

-- | @Selector@ for @requestEjectOrDisconnect@
requestEjectOrDisconnectSelector :: Selector '[] ()
requestEjectOrDisconnectSelector = mkSelector "requestEjectOrDisconnect"

-- | @Selector@ for @requestYield@
requestYieldSelector :: Selector '[] ()
requestYieldSelector = mkSelector "requestYield"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] ICDeviceType
typeSelector = mkSelector "type"

-- | @Selector@ for @capabilities@
capabilitiesSelector :: Selector '[] (Id NSArray)
capabilitiesSelector = mkSelector "capabilities"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @productKind@
productKindSelector :: Selector '[] (Id NSString)
productKindSelector = mkSelector "productKind"

-- | @Selector@ for @icon@
iconSelector :: Selector '[] (Ptr ())
iconSelector = mkSelector "icon"

-- | @Selector@ for @systemSymbolName@
systemSymbolNameSelector :: Selector '[] (Id NSString)
systemSymbolNameSelector = mkSelector "systemSymbolName"

-- | @Selector@ for @transportType@
transportTypeSelector :: Selector '[] (Id NSString)
transportTypeSelector = mkSelector "transportType"

-- | @Selector@ for @UUIDString@
uuidStringSelector :: Selector '[] (Id NSString)
uuidStringSelector = mkSelector "UUIDString"

-- | @Selector@ for @locationDescription@
locationDescriptionSelector :: Selector '[] RawId
locationDescriptionSelector = mkSelector "locationDescription"

-- | @Selector@ for @hasOpenSession@
hasOpenSessionSelector :: Selector '[] Bool
hasOpenSessionSelector = mkSelector "hasOpenSession"

-- | @Selector@ for @userData@
userDataSelector :: Selector '[] (Id NSMutableDictionary)
userDataSelector = mkSelector "userData"

-- | @Selector@ for @modulePath@
modulePathSelector :: Selector '[] RawId
modulePathSelector = mkSelector "modulePath"

-- | @Selector@ for @moduleVersion@
moduleVersionSelector :: Selector '[] RawId
moduleVersionSelector = mkSelector "moduleVersion"

-- | @Selector@ for @serialNumberString@
serialNumberStringSelector :: Selector '[] RawId
serialNumberStringSelector = mkSelector "serialNumberString"

-- | @Selector@ for @usbLocationID@
usbLocationIDSelector :: Selector '[] CInt
usbLocationIDSelector = mkSelector "usbLocationID"

-- | @Selector@ for @usbProductID@
usbProductIDSelector :: Selector '[] CInt
usbProductIDSelector = mkSelector "usbProductID"

-- | @Selector@ for @usbVendorID@
usbVendorIDSelector :: Selector '[] CInt
usbVendorIDSelector = mkSelector "usbVendorID"

-- | @Selector@ for @autolaunchApplicationPath@
autolaunchApplicationPathSelector :: Selector '[] RawId
autolaunchApplicationPathSelector = mkSelector "autolaunchApplicationPath"

-- | @Selector@ for @setAutolaunchApplicationPath:@
setAutolaunchApplicationPathSelector :: Selector '[RawId] ()
setAutolaunchApplicationPathSelector = mkSelector "setAutolaunchApplicationPath:"

-- | @Selector@ for @remote@
remoteSelector :: Selector '[] Bool
remoteSelector = mkSelector "remote"

-- | @Selector@ for @persistentIDString@
persistentIDStringSelector :: Selector '[] RawId
persistentIDStringSelector = mkSelector "persistentIDString"

-- | @Selector@ for @moduleExecutableArchitecture@
moduleExecutableArchitectureSelector :: Selector '[] CInt
moduleExecutableArchitectureSelector = mkSelector "moduleExecutableArchitecture"

