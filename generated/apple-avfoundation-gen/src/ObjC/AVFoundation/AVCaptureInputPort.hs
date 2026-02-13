{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureInputPort
--
-- An AVCaptureInputPort describes a single stream of media data provided by an AVCaptureInput and provides an interface for connecting that stream to AVCaptureOutput instances via AVCaptureConnection.
--
-- Instances of AVCaptureInputPort cannot be created directly. An AVCaptureInput exposes its input ports via its ports property. Input ports provide information about the format of their media data via the mediaType and formatDescription properties, and allow clients to control the flow of data via the enabled property. Input ports are used by an AVCaptureConnection to define the mapping between inputs and outputs in an AVCaptureSession.
--
-- Generated bindings for @AVCaptureInputPort@.
module ObjC.AVFoundation.AVCaptureInputPort
  ( AVCaptureInputPort
  , IsAVCaptureInputPort(..)
  , init_
  , new
  , input
  , mediaType
  , formatDescription
  , enabled
  , setEnabled
  , clock
  , sourceDeviceType
  , sourceDevicePosition
  , clockSelector
  , enabledSelector
  , formatDescriptionSelector
  , initSelector
  , inputSelector
  , mediaTypeSelector
  , newSelector
  , setEnabledSelector
  , sourceDevicePositionSelector
  , sourceDeviceTypeSelector

  -- * Enum types
  , AVCaptureDevicePosition(AVCaptureDevicePosition)
  , pattern AVCaptureDevicePositionUnspecified
  , pattern AVCaptureDevicePositionBack
  , pattern AVCaptureDevicePositionFront

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO (Id AVCaptureInputPort)
init_ avCaptureInputPort =
  sendOwnedMessage avCaptureInputPort initSelector

-- | @+ new@
new :: IO (Id AVCaptureInputPort)
new  =
  do
    cls' <- getRequiredClass "AVCaptureInputPort"
    sendOwnedClassMessage cls' newSelector

-- | input
--
-- The input that owns the receiver.
--
-- The value of this property is an AVCaptureInput instance that owns the receiver.
--
-- ObjC selector: @- input@
input :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO (Id AVCaptureInput)
input avCaptureInputPort =
  sendMessage avCaptureInputPort inputSelector

-- | mediaType
--
-- The media type of the data provided by the receiver.
--
-- The value of this property is a constant describing the type of media, such as AVMediaTypeVideo or AVMediaTypeAudio, provided by the receiver. Media type constants are defined in AVMediaFormat.h.
--
-- ObjC selector: @- mediaType@
mediaType :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO (Id NSString)
mediaType avCaptureInputPort =
  sendMessage avCaptureInputPort mediaTypeSelector

-- | formatDescription
--
-- The format of the data provided by the receiver.
--
-- The value of this property is a CMFormatDescription that describes the format of the media data currently provided by the receiver. Clients can be notified of changes to the format by observing the AVCaptureInputPortFormatDescriptionDidChangeNotification.
--
-- ObjC selector: @- formatDescription@
formatDescription :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO RawId
formatDescription avCaptureInputPort =
  sendMessage avCaptureInputPort formatDescriptionSelector

-- | enabled
--
-- Whether the receiver should provide data.
--
-- The value of this property is a BOOL that determines whether the receiver should provide data to outputs when a session is running. Clients can set this property to fine tune which media streams from a given input will be used during capture. The default value is YES.
--
-- ObjC selector: @- enabled@
enabled :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO Bool
enabled avCaptureInputPort =
  sendMessage avCaptureInputPort enabledSelector

-- | enabled
--
-- Whether the receiver should provide data.
--
-- The value of this property is a BOOL that determines whether the receiver should provide data to outputs when a session is running. Clients can set this property to fine tune which media streams from a given input will be used during capture. The default value is YES.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> Bool -> IO ()
setEnabled avCaptureInputPort value =
  sendMessage avCaptureInputPort setEnabledSelector value

-- | clock
--
-- Provides access to the "native" clock used by the input port.
--
-- The clock is read-only.
--
-- ObjC selector: @- clock@
clock :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO (Ptr ())
clock avCaptureInputPort =
  sendMessage avCaptureInputPort clockSelector

-- | sourceDeviceType
--
-- The AVCaptureDeviceType of the source device providing input through this port.
--
-- All AVCaptureInputPorts contained in an AVCaptureDeviceInput's ports array have the same sourceDeviceType, which is equal to deviceInput.device.deviceType. All of these ports are legal for use in an AVCaptureSession. When working with virtual devices such as the DualCamera in an AVCaptureMultiCamSession, it is possible to stream media from the virtual device's constituent device streams by discovering and connecting hidden ports. In the case of the DualCamera, its constituent devices are the WideAngle camera and the Telephoto camera. By calling -[AVCaptureDeviceInput portsWithMediaType:sourceDeviceType:sourceDevicePosition:], you may discover ports originating from one or more of the virtual device's constituent devices and then make connections using those ports. Constituent device ports are never present in their owning virtual device input's ports array. As an example, to find the video port originating from the DualCamera's Telephoto camera constituent device, you call [dualCameraDeviceInput portsWithMediaType:AVMediaTypeVideo sourceDeviceType:AVCaptureDeviceTypeBuiltInTelephotoCamera sourceDevicePosition:dualCamera.position] and use the first port in the resulting array.
--
-- ObjC selector: @- sourceDeviceType@
sourceDeviceType :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO (Id NSString)
sourceDeviceType avCaptureInputPort =
  sendMessage avCaptureInputPort sourceDeviceTypeSelector

-- | sourceDevicePosition
--
-- The AVCaptureDevicePosition of the source device providing input through this port.
--
-- All AVCaptureInputPorts contained in an AVCaptureDeviceInput's ports array have the same sourceDevicePosition, which is deviceInput.device.position. When working with microphone input in an AVCaptureMultiCamSession, it is possible to record multiple microphone directions simultaneously, for instance, to record front-facing microphone input to pair with video from the front facing camera, and back-facing microphone input to pair with the video from the back-facing camera. By calling -[AVCaptureDeviceInput portsWithMediaType:sourceDeviceType:sourceDevicePosition:], you may discover additional hidden ports originating from the source audio device. These ports represent individual microphones positioned to pick up audio from one particular direction. Examples follow.
--
-- To discover the audio port that captures omnidirectional audio, use [microphoneDeviceInput portsWithMediaType:AVMediaTypeAudio sourceDeviceType:AVCaptureDeviceTypeMicrophone sourceDevicePosition:AVCaptureDevicePositionUnspecified].firstObject.        To discover the audio port that captures front-facing audio, use [microphoneDeviceInput portsWithMediaType:AVMediaTypeAudio sourceDeviceType:AVCaptureDeviceTypeMicrophone sourceDevicePosition:AVCaptureDevicePositionFront].firstObject.        To discover the audio port that captures back-facing audio, use [microphoneDeviceInput portsWithMediaType:AVMediaTypeAudio sourceDeviceType:AVCaptureDeviceTypeMicrophone sourceDevicePosition:AVCaptureDevicePositionBack].firstObject.
--
-- ObjC selector: @- sourceDevicePosition@
sourceDevicePosition :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO AVCaptureDevicePosition
sourceDevicePosition avCaptureInputPort =
  sendMessage avCaptureInputPort sourceDevicePositionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureInputPort)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureInputPort)
newSelector = mkSelector "new"

-- | @Selector@ for @input@
inputSelector :: Selector '[] (Id AVCaptureInput)
inputSelector = mkSelector "input"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] (Id NSString)
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @formatDescription@
formatDescriptionSelector :: Selector '[] RawId
formatDescriptionSelector = mkSelector "formatDescription"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @clock@
clockSelector :: Selector '[] (Ptr ())
clockSelector = mkSelector "clock"

-- | @Selector@ for @sourceDeviceType@
sourceDeviceTypeSelector :: Selector '[] (Id NSString)
sourceDeviceTypeSelector = mkSelector "sourceDeviceType"

-- | @Selector@ for @sourceDevicePosition@
sourceDevicePositionSelector :: Selector '[] AVCaptureDevicePosition
sourceDevicePositionSelector = mkSelector "sourceDevicePosition"

