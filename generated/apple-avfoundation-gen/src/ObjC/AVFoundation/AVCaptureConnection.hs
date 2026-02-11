{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureConnection
--
-- AVCaptureConnection represents a connection between an AVCaptureInputPort or ports, and an AVCaptureOutput or AVCaptureVideoPreviewLayer present in an AVCaptureSession.
--
-- AVCaptureInputs have one or more AVCaptureInputPorts. AVCaptureOutputs can accept data from one or more sources (example - an AVCaptureMovieFileOutput accepts both video and audio data). AVCaptureVideoPreviewLayers can accept data from one AVCaptureInputPort whose mediaType is AVMediaTypeVideo. When an input or output is added to a session, or a video preview layer is associated with a session, the session greedily forms connections between all the compatible AVCaptureInputs' ports and AVCaptureOutputs or AVCaptureVideoPreviewLayers. Iterating through an output's connections or a video preview layer's sole connection, a client may enable or disable the flow of data from a given input to a given output or preview layer.
--
-- Connections involving audio expose an array of AVCaptureAudioChannel objects, which can be used for monitoring levels.
--
-- Connections involving video expose video specific properties, such as videoMirrored and videoRotationAngle.
--
-- Generated bindings for @AVCaptureConnection@.
module ObjC.AVFoundation.AVCaptureConnection
  ( AVCaptureConnection
  , IsAVCaptureConnection(..)
  , init_
  , new
  , connectionWithInputPorts_output
  , connectionWithInputPort_videoPreviewLayer
  , initWithInputPorts_output
  , initWithInputPort_videoPreviewLayer
  , isVideoRotationAngleSupported
  , inputPorts
  , output
  , videoPreviewLayer
  , enabled
  , setEnabled
  , active
  , audioChannels
  , supportsVideoMirroring
  , videoMirrored
  , setVideoMirrored
  , automaticallyAdjustsVideoMirroring
  , setAutomaticallyAdjustsVideoMirroring
  , videoRotationAngle
  , setVideoRotationAngle
  , supportsVideoOrientation
  , videoOrientation
  , setVideoOrientation
  , supportsVideoFieldMode
  , videoFieldMode
  , setVideoFieldMode
  , supportsVideoMinFrameDuration
  , supportsVideoMaxFrameDuration
  , videoMaxScaleAndCropFactor
  , videoScaleAndCropFactor
  , setVideoScaleAndCropFactor
  , preferredVideoStabilizationMode
  , setPreferredVideoStabilizationMode
  , activeVideoStabilizationMode
  , supportsVideoStabilization
  , videoStabilizationEnabled
  , enablesVideoStabilizationWhenAvailable
  , setEnablesVideoStabilizationWhenAvailable
  , cameraIntrinsicMatrixDeliverySupported
  , cameraIntrinsicMatrixDeliveryEnabled
  , setCameraIntrinsicMatrixDeliveryEnabled
  , initSelector
  , newSelector
  , connectionWithInputPorts_outputSelector
  , connectionWithInputPort_videoPreviewLayerSelector
  , initWithInputPorts_outputSelector
  , initWithInputPort_videoPreviewLayerSelector
  , isVideoRotationAngleSupportedSelector
  , inputPortsSelector
  , outputSelector
  , videoPreviewLayerSelector
  , enabledSelector
  , setEnabledSelector
  , activeSelector
  , audioChannelsSelector
  , supportsVideoMirroringSelector
  , videoMirroredSelector
  , setVideoMirroredSelector
  , automaticallyAdjustsVideoMirroringSelector
  , setAutomaticallyAdjustsVideoMirroringSelector
  , videoRotationAngleSelector
  , setVideoRotationAngleSelector
  , supportsVideoOrientationSelector
  , videoOrientationSelector
  , setVideoOrientationSelector
  , supportsVideoFieldModeSelector
  , videoFieldModeSelector
  , setVideoFieldModeSelector
  , supportsVideoMinFrameDurationSelector
  , supportsVideoMaxFrameDurationSelector
  , videoMaxScaleAndCropFactorSelector
  , videoScaleAndCropFactorSelector
  , setVideoScaleAndCropFactorSelector
  , preferredVideoStabilizationModeSelector
  , setPreferredVideoStabilizationModeSelector
  , activeVideoStabilizationModeSelector
  , supportsVideoStabilizationSelector
  , videoStabilizationEnabledSelector
  , enablesVideoStabilizationWhenAvailableSelector
  , setEnablesVideoStabilizationWhenAvailableSelector
  , cameraIntrinsicMatrixDeliverySupportedSelector
  , cameraIntrinsicMatrixDeliveryEnabledSelector
  , setCameraIntrinsicMatrixDeliveryEnabledSelector

  -- * Enum types
  , AVCaptureVideoOrientation(AVCaptureVideoOrientation)
  , pattern AVCaptureVideoOrientationPortrait
  , pattern AVCaptureVideoOrientationPortraitUpsideDown
  , pattern AVCaptureVideoOrientationLandscapeRight
  , pattern AVCaptureVideoOrientationLandscapeLeft
  , AVCaptureVideoStabilizationMode(AVCaptureVideoStabilizationMode)
  , pattern AVCaptureVideoStabilizationModeOff
  , pattern AVCaptureVideoStabilizationModeStandard
  , pattern AVCaptureVideoStabilizationModeCinematic
  , pattern AVCaptureVideoStabilizationModeCinematicExtended
  , pattern AVCaptureVideoStabilizationModePreviewOptimized
  , pattern AVCaptureVideoStabilizationModeCinematicExtendedEnhanced
  , pattern AVCaptureVideoStabilizationModeLowLatency
  , pattern AVCaptureVideoStabilizationModeAuto
  , AVVideoFieldMode(AVVideoFieldMode)
  , pattern AVVideoFieldModeBoth
  , pattern AVVideoFieldModeTopOnly
  , pattern AVVideoFieldModeBottomOnly
  , pattern AVVideoFieldModeDeinterlace

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
init_ :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO (Id AVCaptureConnection)
init_ avCaptureConnection  =
    sendMsg avCaptureConnection (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureConnection)
new  =
  do
    cls' <- getRequiredClass "AVCaptureConnection"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | connectionWithInputPorts:output:
--
-- Returns an AVCaptureConnection instance describing a connection between the specified inputPorts and the specified output.
--
-- @ports@ — An array of AVCaptureInputPort objects associated with AVCaptureInput objects.
--
-- @output@ — An AVCaptureOutput object.
--
-- Returns: An AVCaptureConnection instance joining the specified inputPorts to the specified output port.
--
-- This method returns an instance of AVCaptureConnection that may be subsequently added to an AVCaptureSession instance using AVCaptureSession's -addConnection: method. When using -addInput: or -addOutput:, connections are formed between all compatible inputs and outputs automatically. You do not need to manually create and add connections to the session unless you use the primitive -addInputWithNoConnections: or -addOutputWithNoConnections: methods.
--
-- ObjC selector: @+ connectionWithInputPorts:output:@
connectionWithInputPorts_output :: (IsNSArray ports, IsAVCaptureOutput output) => ports -> output -> IO (Id AVCaptureConnection)
connectionWithInputPorts_output ports output =
  do
    cls' <- getRequiredClass "AVCaptureConnection"
    withObjCPtr ports $ \raw_ports ->
      withObjCPtr output $ \raw_output ->
        sendClassMsg cls' (mkSelector "connectionWithInputPorts:output:") (retPtr retVoid) [argPtr (castPtr raw_ports :: Ptr ()), argPtr (castPtr raw_output :: Ptr ())] >>= retainedObject . castPtr

-- | connectionWithInputPort:videoPreviewLayer:
--
-- Returns an AVCaptureConnection instance describing a connection between the specified inputPort and the specified AVCaptureVideoPreviewLayer instance.
--
-- @port@ — An AVCaptureInputPort object associated with an AVCaptureInput object.
--
-- @layer@ — An AVCaptureVideoPreviewLayer object.
--
-- Returns: An AVCaptureConnection instance joining the specified inputPort to the specified video preview layer.
--
-- This method returns an instance of AVCaptureConnection that may be subsequently added to an AVCaptureSession instance using AVCaptureSession's -addConnection: method. When using AVCaptureVideoPreviewLayer's -initWithSession: or -setSession:, a connection is formed between the first compatible input port and the video preview layer automatically. You do not need to manually create and add connections to the session unless you use AVCaptureVideoPreviewLayer's primitive -initWithSessionWithNoConnection: or -setSessionWithNoConnection: methods.
--
-- ObjC selector: @+ connectionWithInputPort:videoPreviewLayer:@
connectionWithInputPort_videoPreviewLayer :: (IsAVCaptureInputPort port, IsAVCaptureVideoPreviewLayer layer) => port -> layer -> IO (Id AVCaptureConnection)
connectionWithInputPort_videoPreviewLayer port layer =
  do
    cls' <- getRequiredClass "AVCaptureConnection"
    withObjCPtr port $ \raw_port ->
      withObjCPtr layer $ \raw_layer ->
        sendClassMsg cls' (mkSelector "connectionWithInputPort:videoPreviewLayer:") (retPtr retVoid) [argPtr (castPtr raw_port :: Ptr ()), argPtr (castPtr raw_layer :: Ptr ())] >>= retainedObject . castPtr

-- | initWithInputPorts:output:
--
-- Returns an AVCaptureConnection instance describing a connection between the specified inputPorts and the specified output.
--
-- @ports@ — An array of AVCaptureInputPort objects associated with AVCaptureInput objects.
--
-- @output@ — An AVCaptureOutput object.
--
-- Returns: An AVCaptureConnection instance joining the specified inputPorts to the specified output port.
--
-- This method returns an instance of AVCaptureConnection that may be subsequently added to an AVCaptureSession instance using AVCaptureSession's -addConnection: method. When using -addInput: or -addOutput:, connections are formed between all compatible inputs and outputs automatically. You do not need to manually create and add connections to the session unless you use the primitive -addInputWithNoConnections: or -addOutputWithNoConnections: methods.
--
-- ObjC selector: @- initWithInputPorts:output:@
initWithInputPorts_output :: (IsAVCaptureConnection avCaptureConnection, IsNSArray ports, IsAVCaptureOutput output) => avCaptureConnection -> ports -> output -> IO (Id AVCaptureConnection)
initWithInputPorts_output avCaptureConnection  ports output =
  withObjCPtr ports $ \raw_ports ->
    withObjCPtr output $ \raw_output ->
        sendMsg avCaptureConnection (mkSelector "initWithInputPorts:output:") (retPtr retVoid) [argPtr (castPtr raw_ports :: Ptr ()), argPtr (castPtr raw_output :: Ptr ())] >>= ownedObject . castPtr

-- | initWithInputPort:videoPreviewLayer:
--
-- Returns an AVCaptureConnection instance describing a connection between the specified inputPort     and the specified AVCaptureVideoPreviewLayer instance.
--
-- @port@ — An AVCaptureInputPort object associated with an AVCaptureInput object.
--
-- @layer@ — An AVCaptureVideoPreviewLayer object.
--
-- Returns: An AVCaptureConnection instance joining the specified inputPort to the specified video preview layer.
--
-- This method returns an instance of AVCaptureConnection that may be subsequently added to an AVCaptureSession instance using AVCaptureSession's -addConnection: method. When using AVCaptureVideoPreviewLayer's -initWithSession: or -setSession:, a connection is formed between the first compatible input port and the video preview layer automatically. You do not need to manually create and add connections to the session unless you use AVCaptureVideoPreviewLayer's primitive -initWithSessionWithNoConnection: or -setSessionWithNoConnection: methods.
--
-- ObjC selector: @- initWithInputPort:videoPreviewLayer:@
initWithInputPort_videoPreviewLayer :: (IsAVCaptureConnection avCaptureConnection, IsAVCaptureInputPort port, IsAVCaptureVideoPreviewLayer layer) => avCaptureConnection -> port -> layer -> IO (Id AVCaptureConnection)
initWithInputPort_videoPreviewLayer avCaptureConnection  port layer =
  withObjCPtr port $ \raw_port ->
    withObjCPtr layer $ \raw_layer ->
        sendMsg avCaptureConnection (mkSelector "initWithInputPort:videoPreviewLayer:") (retPtr retVoid) [argPtr (castPtr raw_port :: Ptr ()), argPtr (castPtr raw_layer :: Ptr ())] >>= ownedObject . castPtr

-- | isVideoRotationAngleSupported:
--
-- Returns whether the connection supports the given rotation angle in degrees.
--
-- @videoRotationAngle@ — A video rotation angle to be checked.
--
-- Returns: YES if the connection supports the given video rotation angle, NO otherwise.
--
-- The connection's videoRotationAngle property can only be set to a certain angle if this method returns YES for that angle. Only rotation angles of 0, 90, 180 and 270 are supported.
--
-- ObjC selector: @- isVideoRotationAngleSupported:@
isVideoRotationAngleSupported :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> CDouble -> IO Bool
isVideoRotationAngleSupported avCaptureConnection  videoRotationAngle =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "isVideoRotationAngleSupported:") retCULong [argCDouble videoRotationAngle]

-- | inputPorts
--
-- An array of AVCaptureInputPort instances providing data through this connection.
--
-- An AVCaptureConnection may involve one or more AVCaptureInputPorts producing data to the connection's AVCaptureOutput. This property is read-only. An AVCaptureConnection's inputPorts remain static for the life of the object.
--
-- ObjC selector: @- inputPorts@
inputPorts :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO (Id NSArray)
inputPorts avCaptureConnection  =
    sendMsg avCaptureConnection (mkSelector "inputPorts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | output
--
-- The AVCaptureOutput instance consuming data from this connection's inputPorts.
--
-- An AVCaptureConnection may involve one or more AVCaptureInputPorts producing data to the connection's AVCaptureOutput. This property is read-only. An AVCaptureConnection's output remains static for the life of the object. Note that a connection can either be to an output or a video preview layer, but never to both.
--
-- ObjC selector: @- output@
output :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO (Id AVCaptureOutput)
output avCaptureConnection  =
    sendMsg avCaptureConnection (mkSelector "output") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoPreviewLayer
--
-- The AVCaptureVideoPreviewLayer instance consuming data from this connection's inputPort.
--
-- An AVCaptureConnection may involve one AVCaptureInputPort producing data to an AVCaptureVideoPreviewLayer object. This property is read-only. An AVCaptureConnection's videoPreviewLayer remains static for the life of the object. Note that a connection can either be to an output or a video preview layer, but never to both.
--
-- ObjC selector: @- videoPreviewLayer@
videoPreviewLayer :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO (Id AVCaptureVideoPreviewLayer)
videoPreviewLayer avCaptureConnection  =
    sendMsg avCaptureConnection (mkSelector "videoPreviewLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | enabled
--
-- Indicates whether the connection's output should consume data.
--
-- The value of this property is a BOOL that determines whether the receiver's output should consume data from its connected inputPorts when a session is running. Clients can set this property to stop the flow of data to a given output during capture. The default value is YES.
--
-- ObjC selector: @- enabled@
enabled :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
enabled avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "enabled") retCULong []

-- | enabled
--
-- Indicates whether the connection's output should consume data.
--
-- The value of this property is a BOOL that determines whether the receiver's output should consume data from its connected inputPorts when a session is running. Clients can set this property to stop the flow of data to a given output during capture. The default value is YES.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> Bool -> IO ()
setEnabled avCaptureConnection  value =
    sendMsg avCaptureConnection (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | active
--
-- Indicates whether the receiver's output is currently capable of consuming data through this connection.
--
-- The value of this property is a BOOL that determines whether the receiver's output can consume data provided through this connection. This property is read-only. Clients may key-value observe this property to know when a session's configuration forces a connection to become inactive. The default value is YES.
--
-- Prior to iOS 11, the audio connection feeding an AVCaptureAudioDataOutput is made inactive when using AVCaptureSessionPresetPhoto or the equivalent photo format using -[AVCaptureDevice activeFormat]. On iOS 11 and later, the audio connection feeding AVCaptureAudioDataOutput is active for all presets and device formats.
--
-- ObjC selector: @- active@
active :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
active avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "active") retCULong []

-- | audioChannels
--
-- An array of AVCaptureAudioChannel objects representing individual channels of audio data flowing through the connection.
--
-- This property is only applicable to AVCaptureConnection instances involving audio. In such connections, the audioChannels array contains one AVCaptureAudioChannel object for each channel of audio data flowing through this connection.
--
-- ObjC selector: @- audioChannels@
audioChannels :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO (Id NSArray)
audioChannels avCaptureConnection  =
    sendMsg avCaptureConnection (mkSelector "audioChannels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | supportsVideoMirroring
--
-- Indicates whether the connection supports setting the videoMirrored property.
--
-- This property is only applicable to AVCaptureConnection instances involving video. In such connections, the videoMirrored property may only be set if    -isVideoMirroringSupported returns YES.
--
-- ObjC selector: @- supportsVideoMirroring@
supportsVideoMirroring :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
supportsVideoMirroring avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "supportsVideoMirroring") retCULong []

-- | videoMirrored
--
-- Indicates whether the video flowing through the connection should be mirrored about its vertical axis.
--
-- This property is only applicable to AVCaptureConnection instances involving video. if -isVideoMirroringSupported returns YES, videoMirrored may be set to flip the video about its vertical axis and produce a mirror-image effect. This property may not be set unless -isVideoMirroringSupported returns YES, otherwise a NSInvalidArgumentException is thrown. This property may not be set if -automaticallyAdjustsVideoMirroring returns YES, otherwise an NSInvalidArgumentException is thrown.
--
-- ObjC selector: @- videoMirrored@
videoMirrored :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
videoMirrored avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "videoMirrored") retCULong []

-- | videoMirrored
--
-- Indicates whether the video flowing through the connection should be mirrored about its vertical axis.
--
-- This property is only applicable to AVCaptureConnection instances involving video. if -isVideoMirroringSupported returns YES, videoMirrored may be set to flip the video about its vertical axis and produce a mirror-image effect. This property may not be set unless -isVideoMirroringSupported returns YES, otherwise a NSInvalidArgumentException is thrown. This property may not be set if -automaticallyAdjustsVideoMirroring returns YES, otherwise an NSInvalidArgumentException is thrown.
--
-- ObjC selector: @- setVideoMirrored:@
setVideoMirrored :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> Bool -> IO ()
setVideoMirrored avCaptureConnection  value =
    sendMsg avCaptureConnection (mkSelector "setVideoMirrored:") retVoid [argCULong (if value then 1 else 0)]

-- | automaticallyAdjustsVideoMirroring
--
-- Specifies whether or not the value of "videoMirrored" can change based on configuration of the session.
--
-- For some session configurations, video data flowing through the connection will be mirrored by default. When the value of this property is YES, the value of "videoMirrored" may change depending on the configuration of the session, for example after switching to a different AVCaptureDeviceInput. The default value is YES.
--
-- ObjC selector: @- automaticallyAdjustsVideoMirroring@
automaticallyAdjustsVideoMirroring :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
automaticallyAdjustsVideoMirroring avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "automaticallyAdjustsVideoMirroring") retCULong []

-- | automaticallyAdjustsVideoMirroring
--
-- Specifies whether or not the value of "videoMirrored" can change based on configuration of the session.
--
-- For some session configurations, video data flowing through the connection will be mirrored by default. When the value of this property is YES, the value of "videoMirrored" may change depending on the configuration of the session, for example after switching to a different AVCaptureDeviceInput. The default value is YES.
--
-- ObjC selector: @- setAutomaticallyAdjustsVideoMirroring:@
setAutomaticallyAdjustsVideoMirroring :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> Bool -> IO ()
setAutomaticallyAdjustsVideoMirroring avCaptureConnection  value =
    sendMsg avCaptureConnection (mkSelector "setAutomaticallyAdjustsVideoMirroring:") retVoid [argCULong (if value then 1 else 0)]

-- | videoRotationAngle
--
-- Indicates whether the video flowing through the connection should be rotated with a given angle in degrees.
--
-- This property is only applicable to AVCaptureConnection instances involving video or depth. -setVideoRotationAngle: throws an NSInvalidArgumentException if set to an unsupported value (see -isVideoRotationAngleSupported:). Note that setting videoRotationAngle does not necessarily result in physical rotation of video buffers. For instance, a video connection to an AVCaptureMovieFileOutput handles orientation using a Quicktime track matrix. In the AVCapturePhotoOutput, orientation is handled using Exif tags. And the AVCaptureVideoPreviewLayer applies transforms to its contents to perform rotations. However, the AVCaptureVideoDataOutput and AVCaptureDepthDataOutput do output physically rotated video buffers. Setting a video rotation angle for an output that does physically rotate buffers requires a lengthy configuration of the capture render pipeline and should be done before calling -[AVCaptureSession startRunning].
--
-- Starting with the Spring 2024 iPad line, the default value of videoRotationAngle is 180 degrees for video data on Front Camera as compared to 0 degrees on previous devices. So clients using AVCaptureVideoDataOutput and AVCaptureDepthDataOutput should set videoRotationAngle to 0 to avoid the physical buffer rotation described above. And clients rotating video data by themselves must account for the default value of videoRotationAngle when applying angles (videoRotationAngleForHorizonLevelPreview, videoRotationAngleForHorizonLevelCapture) from AVCaptureDeviceRotationCoordinator. Note that this change in default value is currently limited to these iPads, however it is recommended that clients rotating video data themselves incorporate the default rotation value into their workflows for all devices.
--
-- Clients using AVCaptureVideoDataOutput with ProRes Raw should set videoRotationAngle to 0 as rotation is not supported for RAW buffers. If clients want to rotate these buffers themselves they need to apply rotation angles (videoRotationAngleForHorizonLevelPreview, videoRotationAngleForHorizonLevelCapture) provided by AVCaptureDeviceRotationCoordinator.
--
-- ObjC selector: @- videoRotationAngle@
videoRotationAngle :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO CDouble
videoRotationAngle avCaptureConnection  =
    sendMsg avCaptureConnection (mkSelector "videoRotationAngle") retCDouble []

-- | videoRotationAngle
--
-- Indicates whether the video flowing through the connection should be rotated with a given angle in degrees.
--
-- This property is only applicable to AVCaptureConnection instances involving video or depth. -setVideoRotationAngle: throws an NSInvalidArgumentException if set to an unsupported value (see -isVideoRotationAngleSupported:). Note that setting videoRotationAngle does not necessarily result in physical rotation of video buffers. For instance, a video connection to an AVCaptureMovieFileOutput handles orientation using a Quicktime track matrix. In the AVCapturePhotoOutput, orientation is handled using Exif tags. And the AVCaptureVideoPreviewLayer applies transforms to its contents to perform rotations. However, the AVCaptureVideoDataOutput and AVCaptureDepthDataOutput do output physically rotated video buffers. Setting a video rotation angle for an output that does physically rotate buffers requires a lengthy configuration of the capture render pipeline and should be done before calling -[AVCaptureSession startRunning].
--
-- Starting with the Spring 2024 iPad line, the default value of videoRotationAngle is 180 degrees for video data on Front Camera as compared to 0 degrees on previous devices. So clients using AVCaptureVideoDataOutput and AVCaptureDepthDataOutput should set videoRotationAngle to 0 to avoid the physical buffer rotation described above. And clients rotating video data by themselves must account for the default value of videoRotationAngle when applying angles (videoRotationAngleForHorizonLevelPreview, videoRotationAngleForHorizonLevelCapture) from AVCaptureDeviceRotationCoordinator. Note that this change in default value is currently limited to these iPads, however it is recommended that clients rotating video data themselves incorporate the default rotation value into their workflows for all devices.
--
-- Clients using AVCaptureVideoDataOutput with ProRes Raw should set videoRotationAngle to 0 as rotation is not supported for RAW buffers. If clients want to rotate these buffers themselves they need to apply rotation angles (videoRotationAngleForHorizonLevelPreview, videoRotationAngleForHorizonLevelCapture) provided by AVCaptureDeviceRotationCoordinator.
--
-- ObjC selector: @- setVideoRotationAngle:@
setVideoRotationAngle :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> CDouble -> IO ()
setVideoRotationAngle avCaptureConnection  value =
    sendMsg avCaptureConnection (mkSelector "setVideoRotationAngle:") retVoid [argCDouble value]

-- | supportsVideoOrientation
--
-- Indicates whether the connection supports setting the videoOrientation property.
--
-- This property is deprecated. Use -isVideoRotationAngleSupported: instead.
--
-- ObjC selector: @- supportsVideoOrientation@
supportsVideoOrientation :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
supportsVideoOrientation avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "supportsVideoOrientation") retCULong []

-- | videoOrientation
--
-- Indicates whether the video flowing through the connection should be rotated to a given orientation.
--
-- This property is deprecated. Use -videoRotationAngle instead. This property may only be set if -isVideoOrientationSupported returns YES, otherwise an NSInvalidArgumentException is thrown.
--
-- ObjC selector: @- videoOrientation@
videoOrientation :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO AVCaptureVideoOrientation
videoOrientation avCaptureConnection  =
    fmap (coerce :: CLong -> AVCaptureVideoOrientation) $ sendMsg avCaptureConnection (mkSelector "videoOrientation") retCLong []

-- | videoOrientation
--
-- Indicates whether the video flowing through the connection should be rotated to a given orientation.
--
-- This property is deprecated. Use -videoRotationAngle instead. This property may only be set if -isVideoOrientationSupported returns YES, otherwise an NSInvalidArgumentException is thrown.
--
-- ObjC selector: @- setVideoOrientation:@
setVideoOrientation :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> AVCaptureVideoOrientation -> IO ()
setVideoOrientation avCaptureConnection  value =
    sendMsg avCaptureConnection (mkSelector "setVideoOrientation:") retVoid [argCLong (coerce value)]

-- | supportsVideoFieldMode
--
-- Indicates whether the connection supports setting the videoFieldMode property.
--
-- This property is only applicable to AVCaptureConnection instances involving video. In such connections, the videoFieldMode property may only be set if -isVideoFieldModeSupported returns YES.
--
-- ObjC selector: @- supportsVideoFieldMode@
supportsVideoFieldMode :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
supportsVideoFieldMode avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "supportsVideoFieldMode") retCULong []

-- | videoFieldMode
--
-- Indicates how interlaced video flowing through the connection should be treated.
--
-- This property is only applicable to AVCaptureConnection instances involving video. If -isVideoFieldModeSupported returns YES, videoFieldMode may be set to affect interlaced video content flowing through the connection.
--
-- ObjC selector: @- videoFieldMode@
videoFieldMode :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO AVVideoFieldMode
videoFieldMode avCaptureConnection  =
    fmap (coerce :: CLong -> AVVideoFieldMode) $ sendMsg avCaptureConnection (mkSelector "videoFieldMode") retCLong []

-- | videoFieldMode
--
-- Indicates how interlaced video flowing through the connection should be treated.
--
-- This property is only applicable to AVCaptureConnection instances involving video. If -isVideoFieldModeSupported returns YES, videoFieldMode may be set to affect interlaced video content flowing through the connection.
--
-- ObjC selector: @- setVideoFieldMode:@
setVideoFieldMode :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> AVVideoFieldMode -> IO ()
setVideoFieldMode avCaptureConnection  value =
    sendMsg avCaptureConnection (mkSelector "setVideoFieldMode:") retVoid [argCLong (coerce value)]

-- | supportsVideoMinFrameDuration
--
-- Indicates whether the connection supports setting the videoMinFrameDuration property.
--
-- This property is only applicable to AVCaptureConnection instances involving video. In such connections, the videoMinFrameDuration property may only be set if -isVideoMinFrameDurationSupported returns YES.
--
-- This property is deprecated on iOS, where min and max frame rate adjustments are applied exclusively at the AVCaptureDevice using the activeVideoMinFrameDuration and activeVideoMaxFrameDuration properties. On macOS, frame rate adjustments are supported both at the AVCaptureDevice and at AVCaptureConnection, enabling connections to output different frame rates.
--
-- ObjC selector: @- supportsVideoMinFrameDuration@
supportsVideoMinFrameDuration :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
supportsVideoMinFrameDuration avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "supportsVideoMinFrameDuration") retCULong []

-- | supportsVideoMaxFrameDuration
--
-- Indicates whether the connection supports setting the videoMaxFrameDuration property.
--
-- This property is only applicable to AVCaptureConnection instances involving video. In such connections, the videoMaxFrameDuration property may only be set if -isVideoMaxFrameDurationSupported returns YES.
--
-- This property is deprecated on iOS, where min and max frame rate adjustments are applied exclusively at the AVCaptureDevice using the activeVideoMinFrameDuration and activeVideoMaxFrameDuration properties. On macOS, frame rate adjustments are supported both at the AVCaptureDevice and at AVCaptureConnection, enabling connections to output different frame rates.
--
-- ObjC selector: @- supportsVideoMaxFrameDuration@
supportsVideoMaxFrameDuration :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
supportsVideoMaxFrameDuration avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "supportsVideoMaxFrameDuration") retCULong []

-- | videoMaxScaleAndCropFactor
--
-- Indicates the maximum video scale and crop factor supported by the receiver.
--
-- This property is only applicable to AVCaptureConnection instances involving video. In such connections, the videoMaxScaleAndCropFactor property specifies the maximum CGFloat value that may be used when setting the videoScaleAndCropFactor property.
--
-- ObjC selector: @- videoMaxScaleAndCropFactor@
videoMaxScaleAndCropFactor :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO CDouble
videoMaxScaleAndCropFactor avCaptureConnection  =
    sendMsg avCaptureConnection (mkSelector "videoMaxScaleAndCropFactor") retCDouble []

-- | videoScaleAndCropFactor
--
-- Indicates the current video scale and crop factor in use by the receiver.
--
-- This property only applies to AVCaptureStillImageOutput connections. In such connections, the videoScaleAndCropFactor property may be set to a value in the range of 1.0 to videoMaxScaleAndCropFactor. At a factor of 1.0, the image is its original size. At a factor greater than 1.0, the image is scaled by the factor and center-cropped to its original dimensions. This factor is applied in addition to any magnification from AVCaptureDevice's videoZoomFactor property.
--
-- See: -[AVCaptureDevice videoZoomFactor]
--
-- ObjC selector: @- videoScaleAndCropFactor@
videoScaleAndCropFactor :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO CDouble
videoScaleAndCropFactor avCaptureConnection  =
    sendMsg avCaptureConnection (mkSelector "videoScaleAndCropFactor") retCDouble []

-- | videoScaleAndCropFactor
--
-- Indicates the current video scale and crop factor in use by the receiver.
--
-- This property only applies to AVCaptureStillImageOutput connections. In such connections, the videoScaleAndCropFactor property may be set to a value in the range of 1.0 to videoMaxScaleAndCropFactor. At a factor of 1.0, the image is its original size. At a factor greater than 1.0, the image is scaled by the factor and center-cropped to its original dimensions. This factor is applied in addition to any magnification from AVCaptureDevice's videoZoomFactor property.
--
-- See: -[AVCaptureDevice videoZoomFactor]
--
-- ObjC selector: @- setVideoScaleAndCropFactor:@
setVideoScaleAndCropFactor :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> CDouble -> IO ()
setVideoScaleAndCropFactor avCaptureConnection  value =
    sendMsg avCaptureConnection (mkSelector "setVideoScaleAndCropFactor:") retVoid [argCDouble value]

-- | preferredVideoStabilizationMode
--
-- Indicates the stabilization mode to apply to video flowing through the receiver when it is supported.
--
-- This property is only applicable to AVCaptureConnection instances involving video. On devices where the video stabilization feature is supported, only a subset of available source formats may be available for stabilization. By setting the preferredVideoStabilizationMode property to a value other than AVCaptureVideoStabilizationModeOff, video flowing through the receiver is stabilized when the mode is available. In the case of ProRes RAW, this property enables stabilization metadata to be generated, which an application supporting ProRes RAW can optionally apply at playback time using the ProRes RAW SDK. To learn more about the ProRes RAW SDK, refer to the Apple ProRes and ProRes RAW Authorized Products article on support.apple.com. See https://support.apple.com/en-us/118584.
--
-- Enabling video stabilization introduces additional latency into the video capture pipeline and may consume more system memory depending on the stabilization mode and format. If the preferred stabilization mode isn't available, the activeVideoStabilizationMode will be set to AVCaptureVideoStabilizationModeOff. Clients may key-value observe the activeVideoStabilizationMode property to know which stabilization mode is in use or when it is off. The default value is AVCaptureVideoStabilizationModeOff. When setting this property to AVCaptureVideoStabilizationModeAuto, an appropriate stabilization mode will be chosen based on the format and frame rate. For apps linked before iOS 6.0, the default value is AVCaptureVideoStabilizationModeStandard for a video connection attached to an AVCaptureMovieFileOutput instance. For apps linked on or after iOS 6.0, the default value is always AVCaptureVideoStabilizationModeOff. Setting a video stabilization mode using this property may change the value of enablesVideoStabilizationWhenAvailable.
--
-- ObjC selector: @- preferredVideoStabilizationMode@
preferredVideoStabilizationMode :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO AVCaptureVideoStabilizationMode
preferredVideoStabilizationMode avCaptureConnection  =
    fmap (coerce :: CLong -> AVCaptureVideoStabilizationMode) $ sendMsg avCaptureConnection (mkSelector "preferredVideoStabilizationMode") retCLong []

-- | preferredVideoStabilizationMode
--
-- Indicates the stabilization mode to apply to video flowing through the receiver when it is supported.
--
-- This property is only applicable to AVCaptureConnection instances involving video. On devices where the video stabilization feature is supported, only a subset of available source formats may be available for stabilization. By setting the preferredVideoStabilizationMode property to a value other than AVCaptureVideoStabilizationModeOff, video flowing through the receiver is stabilized when the mode is available. In the case of ProRes RAW, this property enables stabilization metadata to be generated, which an application supporting ProRes RAW can optionally apply at playback time using the ProRes RAW SDK. To learn more about the ProRes RAW SDK, refer to the Apple ProRes and ProRes RAW Authorized Products article on support.apple.com. See https://support.apple.com/en-us/118584.
--
-- Enabling video stabilization introduces additional latency into the video capture pipeline and may consume more system memory depending on the stabilization mode and format. If the preferred stabilization mode isn't available, the activeVideoStabilizationMode will be set to AVCaptureVideoStabilizationModeOff. Clients may key-value observe the activeVideoStabilizationMode property to know which stabilization mode is in use or when it is off. The default value is AVCaptureVideoStabilizationModeOff. When setting this property to AVCaptureVideoStabilizationModeAuto, an appropriate stabilization mode will be chosen based on the format and frame rate. For apps linked before iOS 6.0, the default value is AVCaptureVideoStabilizationModeStandard for a video connection attached to an AVCaptureMovieFileOutput instance. For apps linked on or after iOS 6.0, the default value is always AVCaptureVideoStabilizationModeOff. Setting a video stabilization mode using this property may change the value of enablesVideoStabilizationWhenAvailable.
--
-- ObjC selector: @- setPreferredVideoStabilizationMode:@
setPreferredVideoStabilizationMode :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> AVCaptureVideoStabilizationMode -> IO ()
setPreferredVideoStabilizationMode avCaptureConnection  value =
    sendMsg avCaptureConnection (mkSelector "setPreferredVideoStabilizationMode:") retVoid [argCLong (coerce value)]

-- | activeVideoStabilizationMode
--
-- Indicates the stabilization mode currently being applied to video flowing through the receiver.
--
-- This property is only applicable to AVCaptureConnection instances involving video. On devices where the video stabilization feature is supported, only a subset of available source formats may be stabilized. The activeVideoStabilizationMode property returns a value other than AVCaptureVideoStabilizationModeOff if video stabilization is currently in use (or in the case of ProRes RAW, if stabilization metadata is being attached). This property never returns AVCaptureVideoStabilizationModeAuto. This property is key-value observable.
--
-- ObjC selector: @- activeVideoStabilizationMode@
activeVideoStabilizationMode :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO AVCaptureVideoStabilizationMode
activeVideoStabilizationMode avCaptureConnection  =
    fmap (coerce :: CLong -> AVCaptureVideoStabilizationMode) $ sendMsg avCaptureConnection (mkSelector "activeVideoStabilizationMode") retCLong []

-- | supportsVideoStabilization
--
-- Indicates whether the connection supports video stabilization.
--
-- This property is only applicable to AVCaptureConnection instances involving video. In such connections, the -enablesVideoStabilizationWhenAvailable property may only be set if -supportsVideoStabilization returns YES. This property returns YES if the connection's input device has one or more formats that support video stabilization and the connection's output supports video stabilization (or in the case of ProRes RAW, support stabilization metadata attachments).  See [AVCaptureDeviceFormat isVideoStabilizationModeSupported:] to check which video stabilization modes are supported by the active device format.
--
-- ObjC selector: @- supportsVideoStabilization@
supportsVideoStabilization :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
supportsVideoStabilization avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "supportsVideoStabilization") retCULong []

-- | videoStabilizationEnabled
--
-- Indicates whether stabilization is currently being applied to video flowing through the receiver.
--
-- This property is only applicable to AVCaptureConnection instances involving video. On devices where the video stabilization feature is supported, only a subset of available source formats and resolutions may be available for stabilization. The videoStabilizationEnabled property returns YES if video stabilization is currently in use. This property is key-value observable. This property is deprecated. Use activeVideoStabilizationMode instead.
--
-- ObjC selector: @- videoStabilizationEnabled@
videoStabilizationEnabled :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
videoStabilizationEnabled avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "videoStabilizationEnabled") retCULong []

-- | enablesVideoStabilizationWhenAvailable
--
-- Indicates whether stabilization should be applied to video flowing through the receiver when the feature is available.
--
-- This property is only applicable to AVCaptureConnection instances involving video. On devices where the video stabilization feature is supported, only a subset of available source formats and resolutions may be available for stabilization. By setting the enablesVideoStabilizationWhenAvailable property to YES, video flowing through the receiver is stabilized when available. Enabling video stabilization may introduce additional latency into the video capture pipeline. Clients may key-value observe the videoStabilizationEnabled property to know when stabilization is in use or not. The default value is NO. For apps linked before iOS 6.0, the default value is YES for a video connection attached to an AVCaptureMovieFileOutput instance. For apps linked on or after iOS 6.0, the default value is always NO. This property is deprecated. Use preferredVideoStabilizationMode instead.
--
-- ObjC selector: @- enablesVideoStabilizationWhenAvailable@
enablesVideoStabilizationWhenAvailable :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
enablesVideoStabilizationWhenAvailable avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "enablesVideoStabilizationWhenAvailable") retCULong []

-- | enablesVideoStabilizationWhenAvailable
--
-- Indicates whether stabilization should be applied to video flowing through the receiver when the feature is available.
--
-- This property is only applicable to AVCaptureConnection instances involving video. On devices where the video stabilization feature is supported, only a subset of available source formats and resolutions may be available for stabilization. By setting the enablesVideoStabilizationWhenAvailable property to YES, video flowing through the receiver is stabilized when available. Enabling video stabilization may introduce additional latency into the video capture pipeline. Clients may key-value observe the videoStabilizationEnabled property to know when stabilization is in use or not. The default value is NO. For apps linked before iOS 6.0, the default value is YES for a video connection attached to an AVCaptureMovieFileOutput instance. For apps linked on or after iOS 6.0, the default value is always NO. This property is deprecated. Use preferredVideoStabilizationMode instead.
--
-- ObjC selector: @- setEnablesVideoStabilizationWhenAvailable:@
setEnablesVideoStabilizationWhenAvailable :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> Bool -> IO ()
setEnablesVideoStabilizationWhenAvailable avCaptureConnection  value =
    sendMsg avCaptureConnection (mkSelector "setEnablesVideoStabilizationWhenAvailable:") retVoid [argCULong (if value then 1 else 0)]

-- | cameraIntrinsicMatrixDeliverySupported
--
-- Indicates whether the connection supports camera intrinsic matrix delivery.
--
-- This property is only applicable to AVCaptureConnection instances involving video. For such connections, the cameraIntrinsicMatrixDeliveryEnabled property may only be set to YES if -isCameraIntrinsicMatrixDeliverySupported returns YES. This property returns YES if both the connection's input device format and the connection's output support camera intrinsic matrix delivery. Only the AVCaptureVideoDataOutput's connection supports this property. Note that if video stabilization is enabled (preferredVideoStabilizationMode is set to something other than AVCaptureVideoStabilizationModeOff), camera intrinsic matrix delivery is not supported. Starting in iOS 14.3, camera intrinsics are delivered with video buffers on which geometric distortion correction is applied.
--
-- ObjC selector: @- cameraIntrinsicMatrixDeliverySupported@
cameraIntrinsicMatrixDeliverySupported :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
cameraIntrinsicMatrixDeliverySupported avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "cameraIntrinsicMatrixDeliverySupported") retCULong []

-- | cameraIntrinsicMatrixDeliveryEnabled
--
-- Indicates whether camera intrinsic matrix delivery should be enabled.
--
-- This property is only applicable to AVCaptureConnection instances involving video. Refer to property cameraIntrinsicMatrixDeliverySupported before setting this property. When this property is set to YES, the receiver's output will add the kCMSampleBufferAttachmentKey_CameraIntrinsicMatrix sample buffer attachment to all vended sample buffers. This property must be set before the session starts running.
--
-- ObjC selector: @- cameraIntrinsicMatrixDeliveryEnabled@
cameraIntrinsicMatrixDeliveryEnabled :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> IO Bool
cameraIntrinsicMatrixDeliveryEnabled avCaptureConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureConnection (mkSelector "cameraIntrinsicMatrixDeliveryEnabled") retCULong []

-- | cameraIntrinsicMatrixDeliveryEnabled
--
-- Indicates whether camera intrinsic matrix delivery should be enabled.
--
-- This property is only applicable to AVCaptureConnection instances involving video. Refer to property cameraIntrinsicMatrixDeliverySupported before setting this property. When this property is set to YES, the receiver's output will add the kCMSampleBufferAttachmentKey_CameraIntrinsicMatrix sample buffer attachment to all vended sample buffers. This property must be set before the session starts running.
--
-- ObjC selector: @- setCameraIntrinsicMatrixDeliveryEnabled:@
setCameraIntrinsicMatrixDeliveryEnabled :: IsAVCaptureConnection avCaptureConnection => avCaptureConnection -> Bool -> IO ()
setCameraIntrinsicMatrixDeliveryEnabled avCaptureConnection  value =
    sendMsg avCaptureConnection (mkSelector "setCameraIntrinsicMatrixDeliveryEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @connectionWithInputPorts:output:@
connectionWithInputPorts_outputSelector :: Selector
connectionWithInputPorts_outputSelector = mkSelector "connectionWithInputPorts:output:"

-- | @Selector@ for @connectionWithInputPort:videoPreviewLayer:@
connectionWithInputPort_videoPreviewLayerSelector :: Selector
connectionWithInputPort_videoPreviewLayerSelector = mkSelector "connectionWithInputPort:videoPreviewLayer:"

-- | @Selector@ for @initWithInputPorts:output:@
initWithInputPorts_outputSelector :: Selector
initWithInputPorts_outputSelector = mkSelector "initWithInputPorts:output:"

-- | @Selector@ for @initWithInputPort:videoPreviewLayer:@
initWithInputPort_videoPreviewLayerSelector :: Selector
initWithInputPort_videoPreviewLayerSelector = mkSelector "initWithInputPort:videoPreviewLayer:"

-- | @Selector@ for @isVideoRotationAngleSupported:@
isVideoRotationAngleSupportedSelector :: Selector
isVideoRotationAngleSupportedSelector = mkSelector "isVideoRotationAngleSupported:"

-- | @Selector@ for @inputPorts@
inputPortsSelector :: Selector
inputPortsSelector = mkSelector "inputPorts"

-- | @Selector@ for @output@
outputSelector :: Selector
outputSelector = mkSelector "output"

-- | @Selector@ for @videoPreviewLayer@
videoPreviewLayerSelector :: Selector
videoPreviewLayerSelector = mkSelector "videoPreviewLayer"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @audioChannels@
audioChannelsSelector :: Selector
audioChannelsSelector = mkSelector "audioChannels"

-- | @Selector@ for @supportsVideoMirroring@
supportsVideoMirroringSelector :: Selector
supportsVideoMirroringSelector = mkSelector "supportsVideoMirroring"

-- | @Selector@ for @videoMirrored@
videoMirroredSelector :: Selector
videoMirroredSelector = mkSelector "videoMirrored"

-- | @Selector@ for @setVideoMirrored:@
setVideoMirroredSelector :: Selector
setVideoMirroredSelector = mkSelector "setVideoMirrored:"

-- | @Selector@ for @automaticallyAdjustsVideoMirroring@
automaticallyAdjustsVideoMirroringSelector :: Selector
automaticallyAdjustsVideoMirroringSelector = mkSelector "automaticallyAdjustsVideoMirroring"

-- | @Selector@ for @setAutomaticallyAdjustsVideoMirroring:@
setAutomaticallyAdjustsVideoMirroringSelector :: Selector
setAutomaticallyAdjustsVideoMirroringSelector = mkSelector "setAutomaticallyAdjustsVideoMirroring:"

-- | @Selector@ for @videoRotationAngle@
videoRotationAngleSelector :: Selector
videoRotationAngleSelector = mkSelector "videoRotationAngle"

-- | @Selector@ for @setVideoRotationAngle:@
setVideoRotationAngleSelector :: Selector
setVideoRotationAngleSelector = mkSelector "setVideoRotationAngle:"

-- | @Selector@ for @supportsVideoOrientation@
supportsVideoOrientationSelector :: Selector
supportsVideoOrientationSelector = mkSelector "supportsVideoOrientation"

-- | @Selector@ for @videoOrientation@
videoOrientationSelector :: Selector
videoOrientationSelector = mkSelector "videoOrientation"

-- | @Selector@ for @setVideoOrientation:@
setVideoOrientationSelector :: Selector
setVideoOrientationSelector = mkSelector "setVideoOrientation:"

-- | @Selector@ for @supportsVideoFieldMode@
supportsVideoFieldModeSelector :: Selector
supportsVideoFieldModeSelector = mkSelector "supportsVideoFieldMode"

-- | @Selector@ for @videoFieldMode@
videoFieldModeSelector :: Selector
videoFieldModeSelector = mkSelector "videoFieldMode"

-- | @Selector@ for @setVideoFieldMode:@
setVideoFieldModeSelector :: Selector
setVideoFieldModeSelector = mkSelector "setVideoFieldMode:"

-- | @Selector@ for @supportsVideoMinFrameDuration@
supportsVideoMinFrameDurationSelector :: Selector
supportsVideoMinFrameDurationSelector = mkSelector "supportsVideoMinFrameDuration"

-- | @Selector@ for @supportsVideoMaxFrameDuration@
supportsVideoMaxFrameDurationSelector :: Selector
supportsVideoMaxFrameDurationSelector = mkSelector "supportsVideoMaxFrameDuration"

-- | @Selector@ for @videoMaxScaleAndCropFactor@
videoMaxScaleAndCropFactorSelector :: Selector
videoMaxScaleAndCropFactorSelector = mkSelector "videoMaxScaleAndCropFactor"

-- | @Selector@ for @videoScaleAndCropFactor@
videoScaleAndCropFactorSelector :: Selector
videoScaleAndCropFactorSelector = mkSelector "videoScaleAndCropFactor"

-- | @Selector@ for @setVideoScaleAndCropFactor:@
setVideoScaleAndCropFactorSelector :: Selector
setVideoScaleAndCropFactorSelector = mkSelector "setVideoScaleAndCropFactor:"

-- | @Selector@ for @preferredVideoStabilizationMode@
preferredVideoStabilizationModeSelector :: Selector
preferredVideoStabilizationModeSelector = mkSelector "preferredVideoStabilizationMode"

-- | @Selector@ for @setPreferredVideoStabilizationMode:@
setPreferredVideoStabilizationModeSelector :: Selector
setPreferredVideoStabilizationModeSelector = mkSelector "setPreferredVideoStabilizationMode:"

-- | @Selector@ for @activeVideoStabilizationMode@
activeVideoStabilizationModeSelector :: Selector
activeVideoStabilizationModeSelector = mkSelector "activeVideoStabilizationMode"

-- | @Selector@ for @supportsVideoStabilization@
supportsVideoStabilizationSelector :: Selector
supportsVideoStabilizationSelector = mkSelector "supportsVideoStabilization"

-- | @Selector@ for @videoStabilizationEnabled@
videoStabilizationEnabledSelector :: Selector
videoStabilizationEnabledSelector = mkSelector "videoStabilizationEnabled"

-- | @Selector@ for @enablesVideoStabilizationWhenAvailable@
enablesVideoStabilizationWhenAvailableSelector :: Selector
enablesVideoStabilizationWhenAvailableSelector = mkSelector "enablesVideoStabilizationWhenAvailable"

-- | @Selector@ for @setEnablesVideoStabilizationWhenAvailable:@
setEnablesVideoStabilizationWhenAvailableSelector :: Selector
setEnablesVideoStabilizationWhenAvailableSelector = mkSelector "setEnablesVideoStabilizationWhenAvailable:"

-- | @Selector@ for @cameraIntrinsicMatrixDeliverySupported@
cameraIntrinsicMatrixDeliverySupportedSelector :: Selector
cameraIntrinsicMatrixDeliverySupportedSelector = mkSelector "cameraIntrinsicMatrixDeliverySupported"

-- | @Selector@ for @cameraIntrinsicMatrixDeliveryEnabled@
cameraIntrinsicMatrixDeliveryEnabledSelector :: Selector
cameraIntrinsicMatrixDeliveryEnabledSelector = mkSelector "cameraIntrinsicMatrixDeliveryEnabled"

-- | @Selector@ for @setCameraIntrinsicMatrixDeliveryEnabled:@
setCameraIntrinsicMatrixDeliveryEnabledSelector :: Selector
setCameraIntrinsicMatrixDeliveryEnabledSelector = mkSelector "setCameraIntrinsicMatrixDeliveryEnabled:"

