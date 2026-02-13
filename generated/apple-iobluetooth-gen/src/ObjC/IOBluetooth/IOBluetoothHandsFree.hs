{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothHandsFree
--
-- Hands free profile class.
--
-- Superclass of IOBluetoothHandsFreeDevice and IOBluetoothHandsFreeAudioGateway classes. Contains the common code used to support the bluetoooth hands free profile.
--
-- IOBluetoothHandsFreeDevice
--
-- IOBluetoothHandsFreeAudioGateway
--
-- Generated bindings for @IOBluetoothHandsFree@.
module ObjC.IOBluetooth.IOBluetoothHandsFree
  ( IOBluetoothHandsFree
  , IsIOBluetoothHandsFree(..)
  , indicator
  , setIndicator_value
  , initWithDevice_delegate
  , connect
  , disconnect
  , connectSCO
  , disconnectSCO
  , isSCOConnected
  , supportedFeatures
  , setSupportedFeatures
  , inputVolume
  , setInputVolume
  , inputMuted
  , setInputMuted
  , outputVolume
  , setOutputVolume
  , outputMuted
  , setOutputMuted
  , device
  , deviceSupportedFeatures
  , deviceSupportedSMSServices
  , deviceCallHoldModes
  , smsMode
  , smsEnabled
  , delegate
  , setDelegate
  , connected
  , connectSCOSelector
  , connectSelector
  , connectedSelector
  , delegateSelector
  , deviceCallHoldModesSelector
  , deviceSelector
  , deviceSupportedFeaturesSelector
  , deviceSupportedSMSServicesSelector
  , disconnectSCOSelector
  , disconnectSelector
  , indicatorSelector
  , initWithDevice_delegateSelector
  , inputMutedSelector
  , inputVolumeSelector
  , isSCOConnectedSelector
  , outputMutedSelector
  , outputVolumeSelector
  , setDelegateSelector
  , setIndicator_valueSelector
  , setInputMutedSelector
  , setInputVolumeSelector
  , setOutputMutedSelector
  , setOutputVolumeSelector
  , setSupportedFeaturesSelector
  , smsEnabledSelector
  , smsModeSelector
  , supportedFeaturesSelector

  -- * Enum types
  , IOBluetoothSMSMode(IOBluetoothSMSMode)
  , pattern IOBluetoothSMSModePDU
  , pattern IOBluetoothSMSModeText

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOBluetooth.Internal.Classes
import ObjC.IOBluetooth.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | indicator:indicatorName
--
-- Return an indicator's value
--
-- Returns an indicator's value.
--
-- @indicatorName@ — See  “Hands free indicator constants," for standard indicator names.
--
-- ObjC selector: @- indicator:@
indicator :: (IsIOBluetoothHandsFree ioBluetoothHandsFree, IsNSString indicatorName) => ioBluetoothHandsFree -> indicatorName -> IO CInt
indicator ioBluetoothHandsFree indicatorName =
  sendMessage ioBluetoothHandsFree indicatorSelector (toNSString indicatorName)

-- | setIndicator:indicatorName:indicatorValue
--
-- Set an indicator's value
--
-- Sets an indicator's value.
--
-- @indicatorName@ — See  “Hands free indicator constants," for standard indicator names.
--
-- @indicatorValue@ — Will set the indicator value as long as it is within the min and max values allowed.
--
-- ObjC selector: @- setIndicator:value:@
setIndicator_value :: (IsIOBluetoothHandsFree ioBluetoothHandsFree, IsNSString indicatorName) => ioBluetoothHandsFree -> indicatorName -> CInt -> IO ()
setIndicator_value ioBluetoothHandsFree indicatorName indicatorValue =
  sendMessage ioBluetoothHandsFree setIndicator_valueSelector (toNSString indicatorName) indicatorValue

-- | initWithDevice:delegate:
--
-- Create a new IOBluetoothHandsFree object
--
-- This method should be called on a subclass (IOBluetoothHandsFreeDevice or IOBluetoothHandsFreeAudioGateway) to get full functionality.
--
-- @device@ — An IOBluetoothDevice
--
-- @inDelegate@ — An object to act as delegate that implements the IOBluetoothHandsFreeDelegate protocol.
--
-- Returns: A newly created IOBluetoothHandsFreeAudioGateway object on success, nil on failure
--
-- ObjC selector: @- initWithDevice:delegate:@
initWithDevice_delegate :: (IsIOBluetoothHandsFree ioBluetoothHandsFree, IsIOBluetoothDevice device) => ioBluetoothHandsFree -> device -> RawId -> IO (Id IOBluetoothHandsFree)
initWithDevice_delegate ioBluetoothHandsFree device inDelegate =
  sendOwnedMessage ioBluetoothHandsFree initWithDevice_delegateSelector (toIOBluetoothDevice device) inDelegate

-- | connect
--
-- Connect to the device
--
-- Connects to the device and sets up a service level connection (RFCOMM channel). Delegate methods will be called once the connection is complete or a failure occurs.
--
-- ObjC selector: @- connect@
connect :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO ()
connect ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree connectSelector

-- | disconnect
--
-- Disconnect from the device
--
-- Disconnects from the device, closes the SCO and service level connection if they are connected. Delegate methods will be called once the disconnection is complete.
--
-- ObjC selector: @- disconnect@
disconnect :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO ()
disconnect ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree disconnectSelector

-- | connectSCO
--
-- Open a SCO connection with the device
--
-- Opens a SCO connection with the device. The device must already have a service level connection or this will return immediately. Delegate methods will be called once the connection is complete of a failure occurs.
--
-- ObjC selector: @- connectSCO@
connectSCO :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO ()
connectSCO ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree connectSCOSelector

-- | disconnectSCO
--
-- Disconnect the SCO connection with the device
--
-- Disconnects the SCO connection with the device (if one exists). Delegate methods will be called once the disconnection is complete.
--
-- ObjC selector: @- disconnectSCO@
disconnectSCO :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO ()
disconnectSCO ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree disconnectSCOSelector

-- | isSCOConnected
--
-- Determine if there is a SCO connection to the device
--
-- Determines if there is a SCO connection to the device.
--
-- Returns: YES if there is a SCO connection to the device; otherwise, NO.
--
-- ObjC selector: @- isSCOConnected@
isSCOConnected :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO Bool
isSCOConnected ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree isSCOConnectedSelector

-- | supportedFeatures
--
-- Return supported features
--
-- Returns the supported features bitmap. The values are described in “IOBluetoothHandsFreeDeviceFeatures and IOBluetoothHandsFreeAudioGatewayFeatures.”
--
-- Returns: The supported features bitmap
--
-- setSupportedFeatures:featuresBitmap
--
-- Set the supported features
--
-- Sets the supported features bitmap. The values are described in “IOBluetoothHandsFreeDeviceFeatures and IOBluetoothHandsFreeAudioGatewayFeatures.”
--
-- @featuresBitmap@ — The features bitmap
--
-- ObjC selector: @- supportedFeatures@
supportedFeatures :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO CUInt
supportedFeatures ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree supportedFeaturesSelector

-- | supportedFeatures
--
-- Return supported features
--
-- Returns the supported features bitmap. The values are described in “IOBluetoothHandsFreeDeviceFeatures and IOBluetoothHandsFreeAudioGatewayFeatures.”
--
-- Returns: The supported features bitmap
--
-- setSupportedFeatures:featuresBitmap
--
-- Set the supported features
--
-- Sets the supported features bitmap. The values are described in “IOBluetoothHandsFreeDeviceFeatures and IOBluetoothHandsFreeAudioGatewayFeatures.”
--
-- @featuresBitmap@ — The features bitmap
--
-- ObjC selector: @- setSupportedFeatures:@
setSupportedFeatures :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> CUInt -> IO ()
setSupportedFeatures ioBluetoothHandsFree value =
  sendMessage ioBluetoothHandsFree setSupportedFeaturesSelector value

-- | inputVolume
--
-- Return the input volume
--
-- Returns the input volume between 0 and 1. 0 is the same as mute.
--
-- Returns: The input volume
--
-- setInputVolume:newVolume
--
-- Set the input volume
--
-- Sets the input volume between 0 and 1. 0 is the same as mute.
--
-- @newVolume@ — The new input volume
--
-- ObjC selector: @- inputVolume@
inputVolume :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO CFloat
inputVolume ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree inputVolumeSelector

-- | inputVolume
--
-- Return the input volume
--
-- Returns the input volume between 0 and 1. 0 is the same as mute.
--
-- Returns: The input volume
--
-- setInputVolume:newVolume
--
-- Set the input volume
--
-- Sets the input volume between 0 and 1. 0 is the same as mute.
--
-- @newVolume@ — The new input volume
--
-- ObjC selector: @- setInputVolume:@
setInputVolume :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> CFloat -> IO ()
setInputVolume ioBluetoothHandsFree value =
  sendMessage ioBluetoothHandsFree setInputVolumeSelector value

-- | isInputMuted
--
-- Return the input mute state.
--
-- Returns the inputs mute state.
--
-- Returns: YES if muted; otherwise NO.
--
-- setInputMuted:muted
--
-- Set the input mute state.
--
-- Sets the inputs mute state.
--
-- @muted@ — YES if muted; otherwise NO.
--
-- ObjC selector: @- inputMuted@
inputMuted :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO Bool
inputMuted ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree inputMutedSelector

-- | isInputMuted
--
-- Return the input mute state.
--
-- Returns the inputs mute state.
--
-- Returns: YES if muted; otherwise NO.
--
-- setInputMuted:muted
--
-- Set the input mute state.
--
-- Sets the inputs mute state.
--
-- @muted@ — YES if muted; otherwise NO.
--
-- ObjC selector: @- setInputMuted:@
setInputMuted :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> Bool -> IO ()
setInputMuted ioBluetoothHandsFree value =
  sendMessage ioBluetoothHandsFree setInputMutedSelector value

-- | outputVolume
--
-- Return the output volume
--
-- Returns the output volume between 0 and 1. 0 is the same as mute.
--
-- Returns: The output volume
--
-- setOutputVolume:newVolume
--
-- Set the output volume
--
-- Sets the output volume between 0 and 1. 0 is the same as mute.
--
-- @newVolume@ — The new output volume
--
-- ObjC selector: @- outputVolume@
outputVolume :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO CFloat
outputVolume ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree outputVolumeSelector

-- | outputVolume
--
-- Return the output volume
--
-- Returns the output volume between 0 and 1. 0 is the same as mute.
--
-- Returns: The output volume
--
-- setOutputVolume:newVolume
--
-- Set the output volume
--
-- Sets the output volume between 0 and 1. 0 is the same as mute.
--
-- @newVolume@ — The new output volume
--
-- ObjC selector: @- setOutputVolume:@
setOutputVolume :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> CFloat -> IO ()
setOutputVolume ioBluetoothHandsFree value =
  sendMessage ioBluetoothHandsFree setOutputVolumeSelector value

-- | isOutputMuted
--
-- Return the output mute state.
--
-- Returns the outputs mute state.
--
-- Returns: YES if muted; otherwise NO.
--
-- setOutputMuted:muted
--
-- Set the output mute state.
--
-- Sets the outputs mute state.
--
-- @muted@ — YES if muted; otherwise NO.
--
-- ObjC selector: @- outputMuted@
outputMuted :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO Bool
outputMuted ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree outputMutedSelector

-- | isOutputMuted
--
-- Return the output mute state.
--
-- Returns the outputs mute state.
--
-- Returns: YES if muted; otherwise NO.
--
-- setOutputMuted:muted
--
-- Set the output mute state.
--
-- Sets the outputs mute state.
--
-- @muted@ — YES if muted; otherwise NO.
--
-- ObjC selector: @- setOutputMuted:@
setOutputMuted :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> Bool -> IO ()
setOutputMuted ioBluetoothHandsFree value =
  sendMessage ioBluetoothHandsFree setOutputMutedSelector value

-- | device
--
-- Return the IOBluetoothDevice.
--
-- Returns the IOBluetoothDevice to connect with.
--
-- Returns: The IOBluetoothDevice object
--
-- ObjC selector: @- device@
device :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO (Id IOBluetoothDevice)
device ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree deviceSelector

-- | deviceSupportedFeatures
--
-- Return the device's supported features.
--
-- Returns the device's supported features bitmap. The values are described in “IOBluetoothHandsFreeDeviceFeatures and IOBluetoothHandsFreeAudioGatewayFeatures.”
--
-- Returns: The device features bitmap
--
-- ObjC selector: @- deviceSupportedFeatures@
deviceSupportedFeatures :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO CUInt
deviceSupportedFeatures ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree deviceSupportedFeaturesSelector

-- | deviceSupportedSMSServices
--
-- Return the device's supported SMS services.
--
-- Returns the device's supported SMS services bitmap. The values are described in “IOBluetoothHandsFreeSMSSupport.”
--
-- Returns: The SMS services supported
--
-- ObjC selector: @- deviceSupportedSMSServices@
deviceSupportedSMSServices :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO CUInt
deviceSupportedSMSServices ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree deviceSupportedSMSServicesSelector

-- | deviceCallHoldModes
--
-- Return the device's supported call hold modes.
--
-- Returns the device's supported call hold modes bitmap. The values are described in “IOBluetoothHandsFreeCallHoldModes.”
--
-- Returns: The SMS services supported
--
-- ObjC selector: @- deviceCallHoldModes@
deviceCallHoldModes :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO CUInt
deviceCallHoldModes ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree deviceCallHoldModesSelector

-- | SMSMode
--
-- Return the device's SMS mode.
--
-- Returns the device's SMS mode. The values are described in “IOBluetoothSMSMode.”
--
-- Returns: The SMS mode
--
-- ObjC selector: @- SMSMode@
smsMode :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO IOBluetoothSMSMode
smsMode ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree smsModeSelector

-- | isSMSEnabled
--
-- Return YES if the device has SMS enabled.
--
-- Returns YES if the device has SMS enabled (by responding to a CMGF command). NO if the device has not set an SMS mode or doesn't support SMS.
--
-- Returns: YES if the device has SMSEnabled; otherwise, NO.
--
-- ObjC selector: @- SMSEnabled@
smsEnabled :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO Bool
smsEnabled ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree smsEnabledSelector

-- | delegate
--
-- Return the delegate
--
-- Returns the hands free object's delegate.
--
-- Returns: The delegate for the hands free object or nil if it doesn't have a delegate.
--
-- setDelegate:newDelegate
--
-- Sets the hands free object’s delegate to a given object or removes an existing delegate.
--
-- A IOBluetoothHandsFree delegate can optionally respond to any of the delegate methods in IOBluetoothHandsFreeDelegate and any subclasses delegates.
--
-- @newDelegate@ — The delegate for the hands free object. Pass nil to remove an existing delegate.
--
-- ObjC selector: @- delegate@
delegate :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO RawId
delegate ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree delegateSelector

-- | delegate
--
-- Return the delegate
--
-- Returns the hands free object's delegate.
--
-- Returns: The delegate for the hands free object or nil if it doesn't have a delegate.
--
-- setDelegate:newDelegate
--
-- Sets the hands free object’s delegate to a given object or removes an existing delegate.
--
-- A IOBluetoothHandsFree delegate can optionally respond to any of the delegate methods in IOBluetoothHandsFreeDelegate and any subclasses delegates.
--
-- @newDelegate@ — The delegate for the hands free object. Pass nil to remove an existing delegate.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> RawId -> IO ()
setDelegate ioBluetoothHandsFree value =
  sendMessage ioBluetoothHandsFree setDelegateSelector value

-- | isConnected
--
-- Determine if there is a serivice level connection to the device
--
-- Determines if there is a serivice level connection to the device.
--
-- Returns: YES if there is a serivice level connection to the device; otherwise, NO.
--
-- ObjC selector: @- connected@
connected :: IsIOBluetoothHandsFree ioBluetoothHandsFree => ioBluetoothHandsFree -> IO Bool
connected ioBluetoothHandsFree =
  sendMessage ioBluetoothHandsFree connectedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @indicator:@
indicatorSelector :: Selector '[Id NSString] CInt
indicatorSelector = mkSelector "indicator:"

-- | @Selector@ for @setIndicator:value:@
setIndicator_valueSelector :: Selector '[Id NSString, CInt] ()
setIndicator_valueSelector = mkSelector "setIndicator:value:"

-- | @Selector@ for @initWithDevice:delegate:@
initWithDevice_delegateSelector :: Selector '[Id IOBluetoothDevice, RawId] (Id IOBluetoothHandsFree)
initWithDevice_delegateSelector = mkSelector "initWithDevice:delegate:"

-- | @Selector@ for @connect@
connectSelector :: Selector '[] ()
connectSelector = mkSelector "connect"

-- | @Selector@ for @disconnect@
disconnectSelector :: Selector '[] ()
disconnectSelector = mkSelector "disconnect"

-- | @Selector@ for @connectSCO@
connectSCOSelector :: Selector '[] ()
connectSCOSelector = mkSelector "connectSCO"

-- | @Selector@ for @disconnectSCO@
disconnectSCOSelector :: Selector '[] ()
disconnectSCOSelector = mkSelector "disconnectSCO"

-- | @Selector@ for @isSCOConnected@
isSCOConnectedSelector :: Selector '[] Bool
isSCOConnectedSelector = mkSelector "isSCOConnected"

-- | @Selector@ for @supportedFeatures@
supportedFeaturesSelector :: Selector '[] CUInt
supportedFeaturesSelector = mkSelector "supportedFeatures"

-- | @Selector@ for @setSupportedFeatures:@
setSupportedFeaturesSelector :: Selector '[CUInt] ()
setSupportedFeaturesSelector = mkSelector "setSupportedFeatures:"

-- | @Selector@ for @inputVolume@
inputVolumeSelector :: Selector '[] CFloat
inputVolumeSelector = mkSelector "inputVolume"

-- | @Selector@ for @setInputVolume:@
setInputVolumeSelector :: Selector '[CFloat] ()
setInputVolumeSelector = mkSelector "setInputVolume:"

-- | @Selector@ for @inputMuted@
inputMutedSelector :: Selector '[] Bool
inputMutedSelector = mkSelector "inputMuted"

-- | @Selector@ for @setInputMuted:@
setInputMutedSelector :: Selector '[Bool] ()
setInputMutedSelector = mkSelector "setInputMuted:"

-- | @Selector@ for @outputVolume@
outputVolumeSelector :: Selector '[] CFloat
outputVolumeSelector = mkSelector "outputVolume"

-- | @Selector@ for @setOutputVolume:@
setOutputVolumeSelector :: Selector '[CFloat] ()
setOutputVolumeSelector = mkSelector "setOutputVolume:"

-- | @Selector@ for @outputMuted@
outputMutedSelector :: Selector '[] Bool
outputMutedSelector = mkSelector "outputMuted"

-- | @Selector@ for @setOutputMuted:@
setOutputMutedSelector :: Selector '[Bool] ()
setOutputMutedSelector = mkSelector "setOutputMuted:"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] (Id IOBluetoothDevice)
deviceSelector = mkSelector "device"

-- | @Selector@ for @deviceSupportedFeatures@
deviceSupportedFeaturesSelector :: Selector '[] CUInt
deviceSupportedFeaturesSelector = mkSelector "deviceSupportedFeatures"

-- | @Selector@ for @deviceSupportedSMSServices@
deviceSupportedSMSServicesSelector :: Selector '[] CUInt
deviceSupportedSMSServicesSelector = mkSelector "deviceSupportedSMSServices"

-- | @Selector@ for @deviceCallHoldModes@
deviceCallHoldModesSelector :: Selector '[] CUInt
deviceCallHoldModesSelector = mkSelector "deviceCallHoldModes"

-- | @Selector@ for @SMSMode@
smsModeSelector :: Selector '[] IOBluetoothSMSMode
smsModeSelector = mkSelector "SMSMode"

-- | @Selector@ for @SMSEnabled@
smsEnabledSelector :: Selector '[] Bool
smsEnabledSelector = mkSelector "SMSEnabled"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @connected@
connectedSelector :: Selector '[] Bool
connectedSelector = mkSelector "connected"

