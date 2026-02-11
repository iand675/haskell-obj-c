{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , inputSelector
  , mediaTypeSelector
  , formatDescriptionSelector
  , enabledSelector
  , setEnabledSelector
  , clockSelector
  , sourceDeviceTypeSelector
  , sourceDevicePositionSelector

  -- * Enum types
  , AVCaptureDevicePosition(AVCaptureDevicePosition)
  , pattern AVCaptureDevicePositionUnspecified
  , pattern AVCaptureDevicePositionBack
  , pattern AVCaptureDevicePositionFront

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

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO (Id AVCaptureInputPort)
init_ avCaptureInputPort  =
  sendMsg avCaptureInputPort (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureInputPort)
new  =
  do
    cls' <- getRequiredClass "AVCaptureInputPort"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | input
--
-- The input that owns the receiver.
--
-- The value of this property is an AVCaptureInput instance that owns the receiver.
--
-- ObjC selector: @- input@
input :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO (Id AVCaptureInput)
input avCaptureInputPort  =
  sendMsg avCaptureInputPort (mkSelector "input") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mediaType
--
-- The media type of the data provided by the receiver.
--
-- The value of this property is a constant describing the type of media, such as AVMediaTypeVideo or AVMediaTypeAudio, provided by the receiver. Media type constants are defined in AVMediaFormat.h.
--
-- ObjC selector: @- mediaType@
mediaType :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO (Id NSString)
mediaType avCaptureInputPort  =
  sendMsg avCaptureInputPort (mkSelector "mediaType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | formatDescription
--
-- The format of the data provided by the receiver.
--
-- The value of this property is a CMFormatDescription that describes the format of the media data currently provided by the receiver. Clients can be notified of changes to the format by observing the AVCaptureInputPortFormatDescriptionDidChangeNotification.
--
-- ObjC selector: @- formatDescription@
formatDescription :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO RawId
formatDescription avCaptureInputPort  =
  fmap (RawId . castPtr) $ sendMsg avCaptureInputPort (mkSelector "formatDescription") (retPtr retVoid) []

-- | enabled
--
-- Whether the receiver should provide data.
--
-- The value of this property is a BOOL that determines whether the receiver should provide data to outputs when a session is running. Clients can set this property to fine tune which media streams from a given input will be used during capture. The default value is YES.
--
-- ObjC selector: @- enabled@
enabled :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO Bool
enabled avCaptureInputPort  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureInputPort (mkSelector "enabled") retCULong []

-- | enabled
--
-- Whether the receiver should provide data.
--
-- The value of this property is a BOOL that determines whether the receiver should provide data to outputs when a session is running. Clients can set this property to fine tune which media streams from a given input will be used during capture. The default value is YES.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> Bool -> IO ()
setEnabled avCaptureInputPort  value =
  sendMsg avCaptureInputPort (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | clock
--
-- Provides access to the "native" clock used by the input port.
--
-- The clock is read-only.
--
-- ObjC selector: @- clock@
clock :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO (Ptr ())
clock avCaptureInputPort  =
  fmap castPtr $ sendMsg avCaptureInputPort (mkSelector "clock") (retPtr retVoid) []

-- | sourceDeviceType
--
-- The AVCaptureDeviceType of the source device providing input through this port.
--
-- All AVCaptureInputPorts contained in an AVCaptureDeviceInput's ports array have the same sourceDeviceType, which is equal to deviceInput.device.deviceType. All of these ports are legal for use in an AVCaptureSession. When working with virtual devices such as the DualCamera in an AVCaptureMultiCamSession, it is possible to stream media from the virtual device's constituent device streams by discovering and connecting hidden ports. In the case of the DualCamera, its constituent devices are the WideAngle camera and the Telephoto camera. By calling -[AVCaptureDeviceInput portsWithMediaType:sourceDeviceType:sourceDevicePosition:], you may discover ports originating from one or more of the virtual device's constituent devices and then make connections using those ports. Constituent device ports are never present in their owning virtual device input's ports array. As an example, to find the video port originating from the DualCamera's Telephoto camera constituent device, you call [dualCameraDeviceInput portsWithMediaType:AVMediaTypeVideo sourceDeviceType:AVCaptureDeviceTypeBuiltInTelephotoCamera sourceDevicePosition:dualCamera.position] and use the first port in the resulting array.
--
-- ObjC selector: @- sourceDeviceType@
sourceDeviceType :: IsAVCaptureInputPort avCaptureInputPort => avCaptureInputPort -> IO (Id NSString)
sourceDeviceType avCaptureInputPort  =
  sendMsg avCaptureInputPort (mkSelector "sourceDeviceType") (retPtr retVoid) [] >>= retainedObject . castPtr

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
sourceDevicePosition avCaptureInputPort  =
  fmap (coerce :: CLong -> AVCaptureDevicePosition) $ sendMsg avCaptureInputPort (mkSelector "sourceDevicePosition") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @input@
inputSelector :: Selector
inputSelector = mkSelector "input"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @formatDescription@
formatDescriptionSelector :: Selector
formatDescriptionSelector = mkSelector "formatDescription"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @clock@
clockSelector :: Selector
clockSelector = mkSelector "clock"

-- | @Selector@ for @sourceDeviceType@
sourceDeviceTypeSelector :: Selector
sourceDeviceTypeSelector = mkSelector "sourceDeviceType"

-- | @Selector@ for @sourceDevicePosition@
sourceDevicePositionSelector :: Selector
sourceDevicePositionSelector = mkSelector "sourceDevicePosition"

