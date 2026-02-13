{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureMovieFileOutput
--
-- AVCaptureMovieFileOutput is a concrete subclass of AVCaptureFileOutput that writes captured media to QuickTime movie files.
--
-- AVCaptureMovieFileOutput implements the complete file recording interface declared by AVCaptureFileOutput for writing media data to QuickTime movie files. In addition, instances of AVCaptureMovieFileOutput allow clients to configure options specific to the QuickTime file format, including allowing them to write metadata collections to each file, specify media encoding options for each track (macOS), and specify an interval at which movie fragments should be written.
--
-- Generated bindings for @AVCaptureMovieFileOutput@.
module ObjC.AVFoundation.AVCaptureMovieFileOutput
  ( AVCaptureMovieFileOutput
  , IsAVCaptureMovieFileOutput(..)
  , init_
  , new
  , supportedOutputSettingsKeysForConnection
  , outputSettingsForConnection
  , setOutputSettings_forConnection
  , recordsVideoOrientationAndMirroringChangesAsMetadataTrackForConnection
  , setRecordsVideoOrientationAndMirroringChanges_asMetadataTrackForConnection
  , setPrimaryConstituentDeviceSwitchingBehaviorForRecording_restrictedSwitchingBehaviorConditions
  , metadata
  , setMetadata
  , availableVideoCodecTypes
  , primaryConstituentDeviceSwitchingBehaviorForRecordingEnabled
  , setPrimaryConstituentDeviceSwitchingBehaviorForRecordingEnabled
  , primaryConstituentDeviceSwitchingBehaviorForRecording
  , primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsForRecording
  , spatialVideoCaptureSupported
  , spatialVideoCaptureEnabled
  , setSpatialVideoCaptureEnabled
  , availableVideoCodecTypesSelector
  , initSelector
  , metadataSelector
  , newSelector
  , outputSettingsForConnectionSelector
  , primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsForRecordingSelector
  , primaryConstituentDeviceSwitchingBehaviorForRecordingEnabledSelector
  , primaryConstituentDeviceSwitchingBehaviorForRecordingSelector
  , recordsVideoOrientationAndMirroringChangesAsMetadataTrackForConnectionSelector
  , setMetadataSelector
  , setOutputSettings_forConnectionSelector
  , setPrimaryConstituentDeviceSwitchingBehaviorForRecordingEnabledSelector
  , setPrimaryConstituentDeviceSwitchingBehaviorForRecording_restrictedSwitchingBehaviorConditionsSelector
  , setRecordsVideoOrientationAndMirroringChanges_asMetadataTrackForConnectionSelector
  , setSpatialVideoCaptureEnabledSelector
  , spatialVideoCaptureEnabledSelector
  , spatialVideoCaptureSupportedSelector
  , supportedOutputSettingsKeysForConnectionSelector

  -- * Enum types
  , AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions(AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions)
  , pattern AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionNone
  , pattern AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionVideoZoomChanged
  , pattern AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionFocusModeChanged
  , pattern AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionExposureModeChanged
  , AVCapturePrimaryConstituentDeviceSwitchingBehavior(AVCapturePrimaryConstituentDeviceSwitchingBehavior)
  , pattern AVCapturePrimaryConstituentDeviceSwitchingBehaviorUnsupported
  , pattern AVCapturePrimaryConstituentDeviceSwitchingBehaviorAuto
  , pattern AVCapturePrimaryConstituentDeviceSwitchingBehaviorRestricted
  , pattern AVCapturePrimaryConstituentDeviceSwitchingBehaviorLocked

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
init_ :: IsAVCaptureMovieFileOutput avCaptureMovieFileOutput => avCaptureMovieFileOutput -> IO (Id AVCaptureMovieFileOutput)
init_ avCaptureMovieFileOutput =
  sendOwnedMessage avCaptureMovieFileOutput initSelector

-- | @+ new@
new :: IO (Id AVCaptureMovieFileOutput)
new  =
  do
    cls' <- getRequiredClass "AVCaptureMovieFileOutput"
    sendOwnedClassMessage cls' newSelector

-- | supportedOutputSettingsKeysForConnection:
--
-- Indicates the supported keys that can be specified in setOutputSettings:forConnection:.
--
-- @connection@ — The connection delivering the media to be encoded.
--
-- Returns an NSArray of NSStrings listing the allowable keys in the receiver's setOutputSettings:forConnection: dictionary.
--
-- ObjC selector: @- supportedOutputSettingsKeysForConnection:@
supportedOutputSettingsKeysForConnection :: (IsAVCaptureMovieFileOutput avCaptureMovieFileOutput, IsAVCaptureConnection connection) => avCaptureMovieFileOutput -> connection -> IO (Id NSArray)
supportedOutputSettingsKeysForConnection avCaptureMovieFileOutput connection =
  sendMessage avCaptureMovieFileOutput supportedOutputSettingsKeysForConnectionSelector (toAVCaptureConnection connection)

-- | outputSettingsForConnection:
--
-- Returns the options the receiver uses to encode media from the given connection as it is being recorded.
--
-- @connection@ — The connection delivering the media to be encoded.
--
-- Returns: An NSDictionary of output settings.
--
-- See AVAudioSettings.h for audio connections or AVVideoSettings.h for video connections for more information on the structure of an output settings dictionary. If the returned value is an empty dictionary (i.e. [NSDictionary dictionary], the format of the media from the connection will not be changed before being written to the file. If -setOutputSettings:forConnection: was called with a nil dictionary, this method returns a non-nil dictionary reflecting the settings used by the AVCaptureSession's current sessionPreset.
--
-- ObjC selector: @- outputSettingsForConnection:@
outputSettingsForConnection :: (IsAVCaptureMovieFileOutput avCaptureMovieFileOutput, IsAVCaptureConnection connection) => avCaptureMovieFileOutput -> connection -> IO (Id NSDictionary)
outputSettingsForConnection avCaptureMovieFileOutput connection =
  sendMessage avCaptureMovieFileOutput outputSettingsForConnectionSelector (toAVCaptureConnection connection)

-- | setOutputSettings:forConnection:
--
-- Sets the options the receiver uses to encode media from the given connection as it is being recorded.
--
-- @outputSettings@ — An NSDictionary of output settings.
--
-- @connection@ — The connection delivering the media to be encoded.
--
-- See AVAudioSettings.h for audio connections or AVVideoSettings.h for video connections for more information on how to construct an output settings dictionary. A value of an empty dictionary (i.e. +[NSDictionary dictionary]), means that the format of the media from the connection should not be changed before being written to the file. A value of nil means that the output format will be determined by the session preset. In this case, -outputSettingsForConnection: will return a non-nil dictionary reflecting the settings used by the AVCaptureSession's current sessionPreset.
--
-- On iOS, your outputSettings dictionary may only contain keys listed in - supportedOutputSettingsKeysForConnection:. If you specify any other key, an NSInvalidArgumentException will be thrown. Further restrictions may be imposed on the AVVideoCodecTypeKey. Its value should be present in the -availableVideoCodecTypes array. If AVVideoCompressionPropertiesKey is specified, you must also specify a valid value for AVVideoCodecKey. On iOS versions prior to 12.0, the only settable key for video connections is AVVideoCodecTypeKey. On iOS 12.0 and later, video connections gain support for AVVideoCompressionPropertiesKey.
--
-- On iOS, -outputSettingsForConnection: always provides a fully populated dictionary. If you call -outputSettingsForConnection: with the intent of overriding a few of the values, you must take care to exclude keys that are not supported before calling -setOutputSettings:forConnection:. When providing an AVVideoCompressionPropertiesKey sub dictionary, you may specify a sparse dictionary. AVCaptureMovieFileOutput will always fill in missing keys with default values for the current AVCaptureSession configuration.
--
-- ObjC selector: @- setOutputSettings:forConnection:@
setOutputSettings_forConnection :: (IsAVCaptureMovieFileOutput avCaptureMovieFileOutput, IsNSDictionary outputSettings, IsAVCaptureConnection connection) => avCaptureMovieFileOutput -> outputSettings -> connection -> IO ()
setOutputSettings_forConnection avCaptureMovieFileOutput outputSettings connection =
  sendMessage avCaptureMovieFileOutput setOutputSettings_forConnectionSelector (toNSDictionary outputSettings) (toAVCaptureConnection connection)

-- | recordsVideoOrientationAndMirroringChangesAsMetadataTrackForConnection:
--
-- Returns YES if the movie file output will create a timed metadata track that records samples which reflect changes made to the given connection's videoOrientation and videoMirrored properties during recording.
--
-- @connection@ — A connection delivering video media to the movie file output. This method throws an NSInvalidArgumentException if the connection does not have a mediaType of AVMediaTypeVideo or if the connection does not terminate at the movie file output.
--
-- See setRecordsVideoOrientationAndMirroringChanges:asMetadataTrackForConnection: for details on the behavior controlled by this value. The default value returned is NO.
--
-- ObjC selector: @- recordsVideoOrientationAndMirroringChangesAsMetadataTrackForConnection:@
recordsVideoOrientationAndMirroringChangesAsMetadataTrackForConnection :: (IsAVCaptureMovieFileOutput avCaptureMovieFileOutput, IsAVCaptureConnection connection) => avCaptureMovieFileOutput -> connection -> IO Bool
recordsVideoOrientationAndMirroringChangesAsMetadataTrackForConnection avCaptureMovieFileOutput connection =
  sendMessage avCaptureMovieFileOutput recordsVideoOrientationAndMirroringChangesAsMetadataTrackForConnectionSelector (toAVCaptureConnection connection)

-- | setRecordsVideoOrientationAndMirroringChanges:asMetadataTrackForConnection:
--
-- Controls whether or not the movie file output will create a timed metadata track that records samples which reflect changes made to the given connection's videoOrientation and videoMirrored properties during recording.
--
-- @doRecordChanges@ — If YES, the movie file output will create a timed metadata track that records samples which reflect changes made to the given connection's videoOrientation and videoMirrored properties during recording.
--
-- @connection@ — A connection delivering video media to the movie file output. This method throws an NSInvalidArgumentException if the connection does not have a mediaType of AVMediaTypeVideo or if the connection does not terminate at the movie file output.
--
-- When a recording is started the current state of a video capture connection's videoOrientation and videoMirrored properties are used to build the display matrix for the created video track. The movie file format allows only one display matrix per track, which means that any changes made during a recording to the videoOrientation and videoMirrored properties are not captured. For example, a user starts a recording with their device in the portrait orientation, and then partway through the recording changes the device to a landscape orientation. The landscape orientation requires a different display matrix, but only the initial display matrix (the portrait display matrix) is recorded for the video track.
--
-- By invoking this method the client application directs the movie file output to create an additional track in the captured movie. This track is a timed metadata track that is associated with the video track, and contains one or more samples that contain a Video Orientation value (as defined by EXIF and TIFF specifications, which is enumerated by CGImagePropertyOrientation in <ImageIO/CGImageProperties.h>). The value represents the display matrix corresponding to the AVCaptureConnection's videoOrientation and videoMirrored properties when applied to the input source. The initial sample written to the timed metadata track represents video track's display matrix. During recording additional samples will be written to the timed metadata track whenever the client application changes the video connection's videoOrienation or videoMirrored properties. Using the above example, when the client application detects the user changing the device from portrait to landscape orientation, it updates the video connection's videoOrientation property, thus causing the movie file output to add a new sample to the timed metadata track.
--
-- After capture, playback and editing applications can use the timed metadata track to enhance their user's experience. For example, when playing back the captured movie, a playback engine can use the samples to adjust the display of the video samples to keep the video properly oriented. Another example is an editing application that uses the sample the sample times to suggest cut points for breaking the captured movie into separate clips, where each clip is properly oriented.
--
-- The default behavior is to not create the timed metadata track.
--
-- The doRecordChanges value is only observed at the start of recording. Changes to the value will not have any effect until the next recording is started.
--
-- ObjC selector: @- setRecordsVideoOrientationAndMirroringChanges:asMetadataTrackForConnection:@
setRecordsVideoOrientationAndMirroringChanges_asMetadataTrackForConnection :: (IsAVCaptureMovieFileOutput avCaptureMovieFileOutput, IsAVCaptureConnection connection) => avCaptureMovieFileOutput -> Bool -> connection -> IO ()
setRecordsVideoOrientationAndMirroringChanges_asMetadataTrackForConnection avCaptureMovieFileOutput doRecordChanges connection =
  sendMessage avCaptureMovieFileOutput setRecordsVideoOrientationAndMirroringChanges_asMetadataTrackForConnectionSelector doRecordChanges (toAVCaptureConnection connection)

-- | setPrimaryConstituentDeviceSwitchingBehaviorForRecording:restrictedSwitchingBehaviorConditions:
--
-- When primaryConstituentDeviceSwitchingBehaviorForRecordingEnabled is set to YES, this method controls the switching behavior and conditions, while a movie file is being recorded.
--
-- This controls the camera selection behavior used while recording a movie, when enabled through primaryConstituentDeviceSwitchingBehaviorForRecordingEnabled. Setting the switching behavior to anything other than AVCapturePrimaryConstituentDeviceSwitchingBehaviorUnsupported when connected to an AVCaptureDevice that does not support constituent device selection throws an NSInvalidArgumentException. Setting restrictedSwitchingBehaviorConditions to something other than AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditionNone while setting switchingBehavior to something other than AVCapturePrimaryConstituentDeviceSwitchingBehaviorRestricted throws an NSInvalidArgumentException exception.
--
-- ObjC selector: @- setPrimaryConstituentDeviceSwitchingBehaviorForRecording:restrictedSwitchingBehaviorConditions:@
setPrimaryConstituentDeviceSwitchingBehaviorForRecording_restrictedSwitchingBehaviorConditions :: IsAVCaptureMovieFileOutput avCaptureMovieFileOutput => avCaptureMovieFileOutput -> AVCapturePrimaryConstituentDeviceSwitchingBehavior -> AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions -> IO ()
setPrimaryConstituentDeviceSwitchingBehaviorForRecording_restrictedSwitchingBehaviorConditions avCaptureMovieFileOutput switchingBehavior restrictedSwitchingBehaviorConditions =
  sendMessage avCaptureMovieFileOutput setPrimaryConstituentDeviceSwitchingBehaviorForRecording_restrictedSwitchingBehaviorConditionsSelector switchingBehavior restrictedSwitchingBehaviorConditions

-- | metadata
--
-- A collection of metadata to be written to the receiver's output files.
--
-- The value of this property is an array of AVMetadataItem objects representing the collection of top-level metadata to be written in each output file.
--
-- ObjC selector: @- metadata@
metadata :: IsAVCaptureMovieFileOutput avCaptureMovieFileOutput => avCaptureMovieFileOutput -> IO (Id NSArray)
metadata avCaptureMovieFileOutput =
  sendMessage avCaptureMovieFileOutput metadataSelector

-- | metadata
--
-- A collection of metadata to be written to the receiver's output files.
--
-- The value of this property is an array of AVMetadataItem objects representing the collection of top-level metadata to be written in each output file.
--
-- ObjC selector: @- setMetadata:@
setMetadata :: (IsAVCaptureMovieFileOutput avCaptureMovieFileOutput, IsNSArray value) => avCaptureMovieFileOutput -> value -> IO ()
setMetadata avCaptureMovieFileOutput value =
  sendMessage avCaptureMovieFileOutput setMetadataSelector (toNSArray value)

-- | availableVideoCodecTypes
--
-- Indicates the supported video codec formats that can be specified in setOutputSettingsForConnection:.
--
-- The value of this property is an NSArray of AVVideoCodecTypes that can be used as values for the AVVideoCodecKey in the receiver's setOutputSettingsForConnection: dictionary. The array of available video codecs may change depending on the current session preset. The first codec in the array is used by default when recording a file.
--
-- ObjC selector: @- availableVideoCodecTypes@
availableVideoCodecTypes :: IsAVCaptureMovieFileOutput avCaptureMovieFileOutput => avCaptureMovieFileOutput -> IO (Id NSArray)
availableVideoCodecTypes avCaptureMovieFileOutput =
  sendMessage avCaptureMovieFileOutput availableVideoCodecTypesSelector

-- | primaryConstituentDeviceSwitchingBehaviorForRecordingEnabled
--
-- Enable or disable a constituent device selection behavior when recording.
--
-- This property enables a camera selection behavior to be applied when recording a movie. Once recording starts, the specified behavior and conditions take effect. Once recording stops the camera selection will change back to the primaryConstituentDeviceSwitchingBehavior specified by the AVCaptureDevice. By default, this property is set to YES when connected to an AVCaptureDevice that supports constituent device switching.
--
-- ObjC selector: @- primaryConstituentDeviceSwitchingBehaviorForRecordingEnabled@
primaryConstituentDeviceSwitchingBehaviorForRecordingEnabled :: IsAVCaptureMovieFileOutput avCaptureMovieFileOutput => avCaptureMovieFileOutput -> IO Bool
primaryConstituentDeviceSwitchingBehaviorForRecordingEnabled avCaptureMovieFileOutput =
  sendMessage avCaptureMovieFileOutput primaryConstituentDeviceSwitchingBehaviorForRecordingEnabledSelector

-- | primaryConstituentDeviceSwitchingBehaviorForRecordingEnabled
--
-- Enable or disable a constituent device selection behavior when recording.
--
-- This property enables a camera selection behavior to be applied when recording a movie. Once recording starts, the specified behavior and conditions take effect. Once recording stops the camera selection will change back to the primaryConstituentDeviceSwitchingBehavior specified by the AVCaptureDevice. By default, this property is set to YES when connected to an AVCaptureDevice that supports constituent device switching.
--
-- ObjC selector: @- setPrimaryConstituentDeviceSwitchingBehaviorForRecordingEnabled:@
setPrimaryConstituentDeviceSwitchingBehaviorForRecordingEnabled :: IsAVCaptureMovieFileOutput avCaptureMovieFileOutput => avCaptureMovieFileOutput -> Bool -> IO ()
setPrimaryConstituentDeviceSwitchingBehaviorForRecordingEnabled avCaptureMovieFileOutput value =
  sendMessage avCaptureMovieFileOutput setPrimaryConstituentDeviceSwitchingBehaviorForRecordingEnabledSelector value

-- | primaryConstituentDeviceSwitchingBehaviorForRecording
--
-- The primaryConstituentDeviceSwitchingBehavior as set by -[AVCaptureMovieFileOutput setPrimaryConstituentDeviceSwitchingBehaviorForRecording:restrictedSwitchingBehaviorConditions:].
--
-- By default, this property is set to AVCapturePrimaryConstituentDeviceSwitchingBehaviorRestricted. This property is key-value observable.
--
-- ObjC selector: @- primaryConstituentDeviceSwitchingBehaviorForRecording@
primaryConstituentDeviceSwitchingBehaviorForRecording :: IsAVCaptureMovieFileOutput avCaptureMovieFileOutput => avCaptureMovieFileOutput -> IO AVCapturePrimaryConstituentDeviceSwitchingBehavior
primaryConstituentDeviceSwitchingBehaviorForRecording avCaptureMovieFileOutput =
  sendMessage avCaptureMovieFileOutput primaryConstituentDeviceSwitchingBehaviorForRecordingSelector

-- | primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsForRecording
--
-- The primaryConstituentDeviceRestrictedSwitchingBehaviorConditions as set by -[AVCaptureMovieFileOutput setPrimaryConstituentDeviceSwitchingBehaviorForRecording:restrictedSwitchingBehaviorConditions:].
--
-- By default, this property is set to AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorCondition{VideoZoomChanged | FocusModeChanged | ExposureModeChanged}. This property is key-value observable.
--
-- ObjC selector: @- primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsForRecording@
primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsForRecording :: IsAVCaptureMovieFileOutput avCaptureMovieFileOutput => avCaptureMovieFileOutput -> IO AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions
primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsForRecording avCaptureMovieFileOutput =
  sendMessage avCaptureMovieFileOutput primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsForRecordingSelector

-- | spatialVideoCaptureSupported
--
-- Returns whether or not capturing spatial video to a file is supported. Note that in order to be supported, two conditions must be met. (1) The source AVCaptureDevice's activeFormat.spatialVideoCaptureSupported property must return YES. (2) The video AVCaptureConnection's activeVideoStabilizationMode property must return AVCaptureVideoStabilizationModeCinematic, AVCaptureVideoStabilizationModeCinematicExtended, or AVCaptureVideoStabilizationModeCinematicExtendedEnhanced.
--
-- ObjC selector: @- spatialVideoCaptureSupported@
spatialVideoCaptureSupported :: IsAVCaptureMovieFileOutput avCaptureMovieFileOutput => avCaptureMovieFileOutput -> IO Bool
spatialVideoCaptureSupported avCaptureMovieFileOutput =
  sendMessage avCaptureMovieFileOutput spatialVideoCaptureSupportedSelector

-- | spatialVideoCaptureEnabled
--
-- Enable or disable capturing spatial video to a file.
--
-- This property enables capturing spatial video to a file. By default, this property is set to NO. Check spatialVideoCaptureSupported before setting this property, as setting to YES will throw an exception if the feature is not supported.
--
-- On iOS, enabling spatial video will overwrite the connected AVCaptureDevice's @videoZoomFactor@, @minAvailableVideoZoomFactor@, and @maxAvailableVideoZoomFactor@ to the field of view of the narrower camera in the pair.
--
-- When spatialVideoCaptureEnabled is true, setting -[AVCaptureDeviceInput activeVideoMinFrameDuration] or -[AVCaptureDeviceInput activeVideoMaxFrameDuration] throws an NSInvalidArgumentException.
--
-- Enabling this property throws an NSInvalidArgumentException if -[AVCaptureDevice isVideoFrameDurationLocked] or -[AVCaptureDevice isFollowingExternalSyncDevice] is true.
--
-- ObjC selector: @- spatialVideoCaptureEnabled@
spatialVideoCaptureEnabled :: IsAVCaptureMovieFileOutput avCaptureMovieFileOutput => avCaptureMovieFileOutput -> IO Bool
spatialVideoCaptureEnabled avCaptureMovieFileOutput =
  sendMessage avCaptureMovieFileOutput spatialVideoCaptureEnabledSelector

-- | spatialVideoCaptureEnabled
--
-- Enable or disable capturing spatial video to a file.
--
-- This property enables capturing spatial video to a file. By default, this property is set to NO. Check spatialVideoCaptureSupported before setting this property, as setting to YES will throw an exception if the feature is not supported.
--
-- On iOS, enabling spatial video will overwrite the connected AVCaptureDevice's @videoZoomFactor@, @minAvailableVideoZoomFactor@, and @maxAvailableVideoZoomFactor@ to the field of view of the narrower camera in the pair.
--
-- When spatialVideoCaptureEnabled is true, setting -[AVCaptureDeviceInput activeVideoMinFrameDuration] or -[AVCaptureDeviceInput activeVideoMaxFrameDuration] throws an NSInvalidArgumentException.
--
-- Enabling this property throws an NSInvalidArgumentException if -[AVCaptureDevice isVideoFrameDurationLocked] or -[AVCaptureDevice isFollowingExternalSyncDevice] is true.
--
-- ObjC selector: @- setSpatialVideoCaptureEnabled:@
setSpatialVideoCaptureEnabled :: IsAVCaptureMovieFileOutput avCaptureMovieFileOutput => avCaptureMovieFileOutput -> Bool -> IO ()
setSpatialVideoCaptureEnabled avCaptureMovieFileOutput value =
  sendMessage avCaptureMovieFileOutput setSpatialVideoCaptureEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureMovieFileOutput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureMovieFileOutput)
newSelector = mkSelector "new"

-- | @Selector@ for @supportedOutputSettingsKeysForConnection:@
supportedOutputSettingsKeysForConnectionSelector :: Selector '[Id AVCaptureConnection] (Id NSArray)
supportedOutputSettingsKeysForConnectionSelector = mkSelector "supportedOutputSettingsKeysForConnection:"

-- | @Selector@ for @outputSettingsForConnection:@
outputSettingsForConnectionSelector :: Selector '[Id AVCaptureConnection] (Id NSDictionary)
outputSettingsForConnectionSelector = mkSelector "outputSettingsForConnection:"

-- | @Selector@ for @setOutputSettings:forConnection:@
setOutputSettings_forConnectionSelector :: Selector '[Id NSDictionary, Id AVCaptureConnection] ()
setOutputSettings_forConnectionSelector = mkSelector "setOutputSettings:forConnection:"

-- | @Selector@ for @recordsVideoOrientationAndMirroringChangesAsMetadataTrackForConnection:@
recordsVideoOrientationAndMirroringChangesAsMetadataTrackForConnectionSelector :: Selector '[Id AVCaptureConnection] Bool
recordsVideoOrientationAndMirroringChangesAsMetadataTrackForConnectionSelector = mkSelector "recordsVideoOrientationAndMirroringChangesAsMetadataTrackForConnection:"

-- | @Selector@ for @setRecordsVideoOrientationAndMirroringChanges:asMetadataTrackForConnection:@
setRecordsVideoOrientationAndMirroringChanges_asMetadataTrackForConnectionSelector :: Selector '[Bool, Id AVCaptureConnection] ()
setRecordsVideoOrientationAndMirroringChanges_asMetadataTrackForConnectionSelector = mkSelector "setRecordsVideoOrientationAndMirroringChanges:asMetadataTrackForConnection:"

-- | @Selector@ for @setPrimaryConstituentDeviceSwitchingBehaviorForRecording:restrictedSwitchingBehaviorConditions:@
setPrimaryConstituentDeviceSwitchingBehaviorForRecording_restrictedSwitchingBehaviorConditionsSelector :: Selector '[AVCapturePrimaryConstituentDeviceSwitchingBehavior, AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions] ()
setPrimaryConstituentDeviceSwitchingBehaviorForRecording_restrictedSwitchingBehaviorConditionsSelector = mkSelector "setPrimaryConstituentDeviceSwitchingBehaviorForRecording:restrictedSwitchingBehaviorConditions:"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSArray)
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector '[Id NSArray] ()
setMetadataSelector = mkSelector "setMetadata:"

-- | @Selector@ for @availableVideoCodecTypes@
availableVideoCodecTypesSelector :: Selector '[] (Id NSArray)
availableVideoCodecTypesSelector = mkSelector "availableVideoCodecTypes"

-- | @Selector@ for @primaryConstituentDeviceSwitchingBehaviorForRecordingEnabled@
primaryConstituentDeviceSwitchingBehaviorForRecordingEnabledSelector :: Selector '[] Bool
primaryConstituentDeviceSwitchingBehaviorForRecordingEnabledSelector = mkSelector "primaryConstituentDeviceSwitchingBehaviorForRecordingEnabled"

-- | @Selector@ for @setPrimaryConstituentDeviceSwitchingBehaviorForRecordingEnabled:@
setPrimaryConstituentDeviceSwitchingBehaviorForRecordingEnabledSelector :: Selector '[Bool] ()
setPrimaryConstituentDeviceSwitchingBehaviorForRecordingEnabledSelector = mkSelector "setPrimaryConstituentDeviceSwitchingBehaviorForRecordingEnabled:"

-- | @Selector@ for @primaryConstituentDeviceSwitchingBehaviorForRecording@
primaryConstituentDeviceSwitchingBehaviorForRecordingSelector :: Selector '[] AVCapturePrimaryConstituentDeviceSwitchingBehavior
primaryConstituentDeviceSwitchingBehaviorForRecordingSelector = mkSelector "primaryConstituentDeviceSwitchingBehaviorForRecording"

-- | @Selector@ for @primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsForRecording@
primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsForRecordingSelector :: Selector '[] AVCapturePrimaryConstituentDeviceRestrictedSwitchingBehaviorConditions
primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsForRecordingSelector = mkSelector "primaryConstituentDeviceRestrictedSwitchingBehaviorConditionsForRecording"

-- | @Selector@ for @spatialVideoCaptureSupported@
spatialVideoCaptureSupportedSelector :: Selector '[] Bool
spatialVideoCaptureSupportedSelector = mkSelector "spatialVideoCaptureSupported"

-- | @Selector@ for @spatialVideoCaptureEnabled@
spatialVideoCaptureEnabledSelector :: Selector '[] Bool
spatialVideoCaptureEnabledSelector = mkSelector "spatialVideoCaptureEnabled"

-- | @Selector@ for @setSpatialVideoCaptureEnabled:@
setSpatialVideoCaptureEnabledSelector :: Selector '[Bool] ()
setSpatialVideoCaptureEnabledSelector = mkSelector "setSpatialVideoCaptureEnabled:"

