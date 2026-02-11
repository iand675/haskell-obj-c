{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureVideoDataOutput
--
-- AVCaptureVideoDataOutput is a concrete subclass of AVCaptureOutput that can be used to process uncompressed or compressed frames from the video being captured.
--
-- Instances of AVCaptureVideoDataOutput produce video frames suitable for processing using other media APIs. Applications can access the frames with the captureOutput:didOutputSampleBuffer:fromConnection: delegate method.
--
-- Generated bindings for @AVCaptureVideoDataOutput@.
module ObjC.AVFoundation.AVCaptureVideoDataOutput
  ( AVCaptureVideoDataOutput
  , IsAVCaptureVideoDataOutput(..)
  , init_
  , new
  , setSampleBufferDelegate_queue
  , recommendedVideoSettingsForAssetWriterWithOutputFileType
  , availableVideoCodecTypesForAssetWriterWithOutputFileType
  , recommendedVideoSettingsForVideoCodecType_assetWriterOutputFileType
  , recommendedVideoSettingsForVideoCodecType_assetWriterOutputFileType_outputFileURL
  , recommendedMovieMetadataForVideoCodecType_assetWriterOutputFileType
  , sampleBufferDelegate
  , sampleBufferCallbackQueue
  , videoSettings
  , setVideoSettings
  , recommendedMediaTimeScaleForAssetWriter
  , availableVideoCVPixelFormatTypes
  , availableVideoCodecTypes
  , alwaysDiscardsLateVideoFrames
  , setAlwaysDiscardsLateVideoFrames
  , automaticallyConfiguresOutputBufferDimensions
  , setAutomaticallyConfiguresOutputBufferDimensions
  , deliversPreviewSizedOutputBuffers
  , setDeliversPreviewSizedOutputBuffers
  , preparesCellularRadioForNetworkConnection
  , setPreparesCellularRadioForNetworkConnection
  , preservesDynamicHDRMetadata
  , setPreservesDynamicHDRMetadata
  , initSelector
  , newSelector
  , setSampleBufferDelegate_queueSelector
  , recommendedVideoSettingsForAssetWriterWithOutputFileTypeSelector
  , availableVideoCodecTypesForAssetWriterWithOutputFileTypeSelector
  , recommendedVideoSettingsForVideoCodecType_assetWriterOutputFileTypeSelector
  , recommendedVideoSettingsForVideoCodecType_assetWriterOutputFileType_outputFileURLSelector
  , recommendedMovieMetadataForVideoCodecType_assetWriterOutputFileTypeSelector
  , sampleBufferDelegateSelector
  , sampleBufferCallbackQueueSelector
  , videoSettingsSelector
  , setVideoSettingsSelector
  , recommendedMediaTimeScaleForAssetWriterSelector
  , availableVideoCVPixelFormatTypesSelector
  , availableVideoCodecTypesSelector
  , alwaysDiscardsLateVideoFramesSelector
  , setAlwaysDiscardsLateVideoFramesSelector
  , automaticallyConfiguresOutputBufferDimensionsSelector
  , setAutomaticallyConfiguresOutputBufferDimensionsSelector
  , deliversPreviewSizedOutputBuffersSelector
  , setDeliversPreviewSizedOutputBuffersSelector
  , preparesCellularRadioForNetworkConnectionSelector
  , setPreparesCellularRadioForNetworkConnectionSelector
  , preservesDynamicHDRMetadataSelector
  , setPreservesDynamicHDRMetadataSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> IO (Id AVCaptureVideoDataOutput)
init_ avCaptureVideoDataOutput  =
    sendMsg avCaptureVideoDataOutput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureVideoDataOutput)
new  =
  do
    cls' <- getRequiredClass "AVCaptureVideoDataOutput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | setSampleBufferDelegate:queue:
--
-- Sets the receiver's delegate that will accept captured buffers and dispatch queue on which the delegate will be called.
--
-- @sampleBufferDelegate@ — An object conforming to the AVCaptureVideoDataOutputSampleBufferDelegate protocol that will receive sample buffers after they are captured.
--
-- @sampleBufferCallbackQueue@ — A dispatch queue on which all sample buffer delegate methods will be called.
--
-- When a new video sample buffer is captured it will be vended to the sample buffer delegate using the captureOutput:didOutputSampleBuffer:fromConnection: delegate method. All delegate methods will be called on the specified dispatch queue. If the queue is blocked when new frames are captured, those frames will be automatically dropped at a time determined by the value of the alwaysDiscardsLateVideoFrames property. This allows clients to process existing frames on the same queue without having to manage the potential memory usage increases that would otherwise occur when that processing is unable to keep up with the rate of incoming frames. If their frame processing is consistently unable to keep up with the rate of incoming frames, clients should consider using the minFrameDuration property, which will generally yield better performance characteristics and more consistent frame rates than frame dropping alone.
--
-- Clients that need to minimize the chances of frames being dropped should specify a queue on which a sufficiently small amount of processing is being done outside of receiving sample buffers. However, if such clients migrate extra processing to another queue, they are responsible for ensuring that memory usage does not grow without bound from frames that have not been processed.
--
-- A serial dispatch queue must be used to guarantee that video frames will be delivered in order. The sampleBufferCallbackQueue parameter may not be NULL, except when setting the sampleBufferDelegate to nil otherwise -setSampleBufferDelegate:queue: throws an NSInvalidArgumentException.
--
-- ObjC selector: @- setSampleBufferDelegate:queue:@
setSampleBufferDelegate_queue :: (IsAVCaptureVideoDataOutput avCaptureVideoDataOutput, IsNSObject sampleBufferCallbackQueue) => avCaptureVideoDataOutput -> RawId -> sampleBufferCallbackQueue -> IO ()
setSampleBufferDelegate_queue avCaptureVideoDataOutput  sampleBufferDelegate sampleBufferCallbackQueue =
  withObjCPtr sampleBufferCallbackQueue $ \raw_sampleBufferCallbackQueue ->
      sendMsg avCaptureVideoDataOutput (mkSelector "setSampleBufferDelegate:queue:") retVoid [argPtr (castPtr (unRawId sampleBufferDelegate) :: Ptr ()), argPtr (castPtr raw_sampleBufferCallbackQueue :: Ptr ())]

-- | recommendedVideoSettingsForAssetWriterWithOutputFileType:
--
-- Specifies the recommended settings for use with an AVAssetWriterInput.
--
-- @outputFileType@ — Specifies the UTI of the file type to be written (see AVMediaFormat.h for a list of file format UTIs).
--
-- Returns: A fully populated dictionary of keys and values that are compatible with AVAssetWriter.
--
-- The value of this property is an NSDictionary containing values for compression settings keys defined in AVVideoSettings.h. This dictionary is suitable for use as the "outputSettings" parameter when creating an AVAssetWriterInput, such as,
--
-- [AVAssetWriterInput assetWriterInputWithMediaType:AVMediaTypeVideo outputSettings:outputSettings sourceFormatHint:hint];
--
-- The dictionary returned contains all necessary keys and values needed by AVAssetWriter (see AVAssetWriterInput.h, -initWithMediaType:outputSettings: for a more in depth discussion). For QuickTime movie and ISO file types, the recommended video settings will produce output comparable to that of AVCaptureMovieFileOutput.
--
-- Note that the dictionary of settings is dependent on the current configuration of the receiver's AVCaptureSession and its inputs. The settings dictionary may change if the session's configuration changes. As such, you should configure your session first, then query the recommended video settings. As of iOS 8.3, movies produced with these settings successfully import into the iOS camera roll and sync to and from like devices via iTunes.
--
-- ObjC selector: @- recommendedVideoSettingsForAssetWriterWithOutputFileType:@
recommendedVideoSettingsForAssetWriterWithOutputFileType :: (IsAVCaptureVideoDataOutput avCaptureVideoDataOutput, IsNSString outputFileType) => avCaptureVideoDataOutput -> outputFileType -> IO (Id NSDictionary)
recommendedVideoSettingsForAssetWriterWithOutputFileType avCaptureVideoDataOutput  outputFileType =
  withObjCPtr outputFileType $ \raw_outputFileType ->
      sendMsg avCaptureVideoDataOutput (mkSelector "recommendedVideoSettingsForAssetWriterWithOutputFileType:") (retPtr retVoid) [argPtr (castPtr raw_outputFileType :: Ptr ())] >>= retainedObject . castPtr

-- | availableVideoCodecTypesForAssetWriterWithOutputFileType:
--
-- Specifies the available video codecs for use with AVAssetWriter and a given file type.
--
-- @outputFileType@ — Specifies the UTI of the file type to be written (see AVMediaFormat.h for a list of file format UTIs).
--
-- Returns: An array of video codecs; see AVVideoSettings.h for a full list.
--
-- This method allows you to query the available video codecs that may be used when specifying an AVVideoCodecKey in -recommendedVideoSettingsForVideoCodecType:assetWriterOutputFileType:. When specifying an outputFileType of AVFileTypeQuickTimeMovie, video codecs are ordered identically to -[AVCaptureMovieFileOutput availableVideoCodecTypes].
--
-- ObjC selector: @- availableVideoCodecTypesForAssetWriterWithOutputFileType:@
availableVideoCodecTypesForAssetWriterWithOutputFileType :: (IsAVCaptureVideoDataOutput avCaptureVideoDataOutput, IsNSString outputFileType) => avCaptureVideoDataOutput -> outputFileType -> IO (Id NSArray)
availableVideoCodecTypesForAssetWriterWithOutputFileType avCaptureVideoDataOutput  outputFileType =
  withObjCPtr outputFileType $ \raw_outputFileType ->
      sendMsg avCaptureVideoDataOutput (mkSelector "availableVideoCodecTypesForAssetWriterWithOutputFileType:") (retPtr retVoid) [argPtr (castPtr raw_outputFileType :: Ptr ())] >>= retainedObject . castPtr

-- | recommendedVideoSettingsForVideoCodecType:assetWriterOutputFileType:
--
-- Specifies the recommended settings for a particular video codec type, to be used with an AVAssetWriterInput.
--
-- @videoCodecType@ — Specifies the desired AVVideoCodecKey to be used for compression (see AVVideoSettings.h).
--
-- @outputFileType@ — Specifies the UTI of the file type to be written (see AVMediaFormat.h for a list of file format UTIs).
--
-- Returns: A fully populated dictionary of keys and values that are compatible with AVAssetWriter.
--
-- The value of this property is an NSDictionary containing values for compression settings keys defined in AVVideoSettings.h. This dictionary is suitable for use as the "outputSettings" parameter when creating an AVAssetWriterInput, such as,
--
-- [AVAssetWriterInput assetWriterInputWithMediaType:AVMediaTypeVideo outputSettings:outputSettings sourceFormatHint:hint];
--
-- The dictionary returned contains all necessary keys and values needed by AVAssetWriter (see AVAssetWriterInput.h, -initWithMediaType:outputSettings: for a more in depth discussion). For QuickTime movie and ISO file types, the recommended video settings will produce output comparable to that of AVCaptureMovieFileOutput.
--
-- The videoCodecType string provided must be present in the availableVideoCodecTypesForAssetWriterWithOutputFileType: array, or an NSInvalidArgumentException is thrown.
--
-- Note that the dictionary of settings is dependent on the current configuration of the receiver's AVCaptureSession and its inputs. The settings dictionary may change if the session's configuration changes. As such, you should configure your session first, then query the recommended video settings. As of iOS 8.3, movies produced with these settings successfully import into the iOS camera roll and sync to and from like devices via iTunes.
--
-- ObjC selector: @- recommendedVideoSettingsForVideoCodecType:assetWriterOutputFileType:@
recommendedVideoSettingsForVideoCodecType_assetWriterOutputFileType :: (IsAVCaptureVideoDataOutput avCaptureVideoDataOutput, IsNSString videoCodecType, IsNSString outputFileType) => avCaptureVideoDataOutput -> videoCodecType -> outputFileType -> IO (Id NSDictionary)
recommendedVideoSettingsForVideoCodecType_assetWriterOutputFileType avCaptureVideoDataOutput  videoCodecType outputFileType =
  withObjCPtr videoCodecType $ \raw_videoCodecType ->
    withObjCPtr outputFileType $ \raw_outputFileType ->
        sendMsg avCaptureVideoDataOutput (mkSelector "recommendedVideoSettingsForVideoCodecType:assetWriterOutputFileType:") (retPtr retVoid) [argPtr (castPtr raw_videoCodecType :: Ptr ()), argPtr (castPtr raw_outputFileType :: Ptr ())] >>= retainedObject . castPtr

-- | recommendedVideoSettingsForVideoCodecType:assetWriterOutputFileType:outputFileURL:
--
-- Specifies the recommended settings for a particular video codec type with output file URL, to be used with an AVAssetWriterInput.
--
-- @videoCodecType@ — Specifies the desired AVVideoCodecKey to be used for compression (see AVVideoSettings.h).
--
-- @outputFileType@ — Specifies the UTI of the file type to be written (see AVMediaFormat.h for a list of file format UTIs).
--
-- @outputFileURL@ — Specifies the output URL of the file to be written.
--
-- If you wish to capture onto an external storage device get an externalStorageDevice of type AVExternalStorageDevice (as defined in AVExternalStorageDevice.h):		[AVExternalStorageDeviceDiscoverySession sharedSession] externalStorageDevices]
--
-- Then use [externalStorageDevice nextAvailableURLsWithPathExtensions:pathExtensions error:&error] to get the output file URL.
--
-- Returns: A fully populated dictionary of keys and values that are compatible with AVAssetWriter.
--
-- The value of this property is an NSDictionary containing values for compression settings keys defined in AVVideoSettings.h. This dictionary is suitable for use as the "outputSettings" parameter when creating an AVAssetWriterInput, such as,
--
-- [AVAssetWriterInput assetWriterInputWithMediaType:AVMediaTypeVideo outputSettings:outputSettings sourceFormatHint:hint];
--
-- The dictionary returned contains all necessary keys and values needed by AVAssetWriter (see AVAssetWriterInput.h, -initWithMediaType:outputSettings: for a more in depth discussion). For QuickTime movie and ISO file types, the recommended video settings will produce output comparable to that of AVCaptureMovieFileOutput.
--
-- The videoCodecType string provided must be present in the availableVideoCodecTypesForAssetWriterWithOutputFileType: array, or an NSInvalidArgumentException is thrown.
--
-- Note that the dictionary of settings is dependent on the current configuration of the receiver's AVCaptureSession and its inputs. The settings dictionary may change if the session's configuration changes. As such, you should configure your session first, then query the recommended video settings. As of iOS 8.3, movies produced with these settings successfully import into the iOS camera roll and sync to and from like devices via iTunes.
--
-- ObjC selector: @- recommendedVideoSettingsForVideoCodecType:assetWriterOutputFileType:outputFileURL:@
recommendedVideoSettingsForVideoCodecType_assetWriterOutputFileType_outputFileURL :: (IsAVCaptureVideoDataOutput avCaptureVideoDataOutput, IsNSString videoCodecType, IsNSString outputFileType, IsNSURL outputFileURL) => avCaptureVideoDataOutput -> videoCodecType -> outputFileType -> outputFileURL -> IO (Id NSDictionary)
recommendedVideoSettingsForVideoCodecType_assetWriterOutputFileType_outputFileURL avCaptureVideoDataOutput  videoCodecType outputFileType outputFileURL =
  withObjCPtr videoCodecType $ \raw_videoCodecType ->
    withObjCPtr outputFileType $ \raw_outputFileType ->
      withObjCPtr outputFileURL $ \raw_outputFileURL ->
          sendMsg avCaptureVideoDataOutput (mkSelector "recommendedVideoSettingsForVideoCodecType:assetWriterOutputFileType:outputFileURL:") (retPtr retVoid) [argPtr (castPtr raw_videoCodecType :: Ptr ()), argPtr (castPtr raw_outputFileType :: Ptr ()), argPtr (castPtr raw_outputFileURL :: Ptr ())] >>= retainedObject . castPtr

-- | Recommends movie-level metadata for a particular video codec type and output file type, to be used with an asset writer input.
--
-- - Parameter videoCodecType: The desired ``AVVideoCodecKey`` to be used for compression (see <doc://com.apple.documentation/documentation/avfoundation/video-settings>). - Parameter outputFileType: Specifies the UTI of the file type to be written (see <doc://com.apple.documentation/documentation/avfoundation/avfiletype>). - Returns: A fully populated array of ``AVMetadataItem`` objects compatible with ``AVAssetWriter``.
--
-- The value of this property is an array of ``AVMetadataItem`` objects representing the collection of top-level metadata to be written in each output file. This array is suitable to use as the ``AVAssetWriter/metadata`` property before you have called ``AVAssetWriter/startWriting``. For more details see <doc://com.apple.documentation/documentation/avfoundation/avassetwriter/startwriting()>.
--
-- The ``videoCodecType`` string you provide must be present in ``availableVideoCodecTypesForAssetWriterWithOutputFileType:`` array, or an @NSInvalidArgumentException@ is thrown.
--
-- For clients writing files using a ProRes Raw codec type, white balance must be locked (call ``AVCaptureDevice/setWhiteBalanceModeLockedWithDeviceWhiteBalanceGains:completionHandler:``) before querying this property, or an @NSIvalidArgumentException@ is thrown.
--
-- - Note: The array of metadata is dependent on the current configuration of the receiver's ``AVCaptureSession`` and its inputs. The array may change when the session's configuration changes. As such, you should configure and start your session first, then query this method.
--
-- ObjC selector: @- recommendedMovieMetadataForVideoCodecType:assetWriterOutputFileType:@
recommendedMovieMetadataForVideoCodecType_assetWriterOutputFileType :: (IsAVCaptureVideoDataOutput avCaptureVideoDataOutput, IsNSString videoCodecType, IsNSString outputFileType) => avCaptureVideoDataOutput -> videoCodecType -> outputFileType -> IO (Id NSArray)
recommendedMovieMetadataForVideoCodecType_assetWriterOutputFileType avCaptureVideoDataOutput  videoCodecType outputFileType =
  withObjCPtr videoCodecType $ \raw_videoCodecType ->
    withObjCPtr outputFileType $ \raw_outputFileType ->
        sendMsg avCaptureVideoDataOutput (mkSelector "recommendedMovieMetadataForVideoCodecType:assetWriterOutputFileType:") (retPtr retVoid) [argPtr (castPtr raw_videoCodecType :: Ptr ()), argPtr (castPtr raw_outputFileType :: Ptr ())] >>= retainedObject . castPtr

-- | sampleBufferDelegate
--
-- The receiver's delegate.
--
-- The value of this property is an object conforming to the AVCaptureVideoDataOutputSampleBufferDelegate protocol that will receive sample buffers after they are captured. The delegate is set using the setSampleBufferDelegate:queue: method.
--
-- ObjC selector: @- sampleBufferDelegate@
sampleBufferDelegate :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> IO RawId
sampleBufferDelegate avCaptureVideoDataOutput  =
    fmap (RawId . castPtr) $ sendMsg avCaptureVideoDataOutput (mkSelector "sampleBufferDelegate") (retPtr retVoid) []

-- | sampleBufferCallbackQueue
--
-- The dispatch queue on which all sample buffer delegate methods will be called.
--
-- The value of this property is a dispatch_queue_t. The queue is set using the setSampleBufferDelegate:queue: method.
--
-- ObjC selector: @- sampleBufferCallbackQueue@
sampleBufferCallbackQueue :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> IO (Id NSObject)
sampleBufferCallbackQueue avCaptureVideoDataOutput  =
    sendMsg avCaptureVideoDataOutput (mkSelector "sampleBufferCallbackQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoSettings
--
-- Specifies the settings used to decode or re-encode video before it is output by the receiver.
--
-- See AVVideoSettings.h for more information on how to construct a video settings dictionary. To receive samples in their device native format, set this property to an empty dictionary (i.e. [NSDictionary dictionary]). To receive samples in a default uncompressed format, set this property to nil. Note that after this property is set to nil, subsequent querying of this property will yield a non-nil dictionary reflecting the settings used by the AVCaptureSession's current sessionPreset.
--
-- On iOS versions prior to iOS 16.0, the only supported key is kCVPixelBufferPixelFormatTypeKey. Use -availableVideoCVPixelFormatTypes for the list of supported pixel formats. For apps linked on or after iOS 16.0, kCVPixelBufferPixelFormatTypeKey, kCVPixelBufferWidthKey, and kCVPixelBufferHeightKey are supported. The width and height must match the videoOrientation specified on the output's AVCaptureConnection or an NSInvalidArgumentException is thrown. The aspect ratio of width and height must match the aspect ratio of the source's activeFormat (corrected for the connection's videoOrientation) or an NSInvalidArgumentException is thrown. If width or height exceeds the source's activeFormat's width or height, an NSInvalidArgumentException is thrown. Changing width and height when deliversPreviewSizedOutputBuffers is set to YES is not supported and throws an NSInvalidArgumentException.
--
-- ObjC selector: @- videoSettings@
videoSettings :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> IO (Id NSDictionary)
videoSettings avCaptureVideoDataOutput  =
    sendMsg avCaptureVideoDataOutput (mkSelector "videoSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoSettings
--
-- Specifies the settings used to decode or re-encode video before it is output by the receiver.
--
-- See AVVideoSettings.h for more information on how to construct a video settings dictionary. To receive samples in their device native format, set this property to an empty dictionary (i.e. [NSDictionary dictionary]). To receive samples in a default uncompressed format, set this property to nil. Note that after this property is set to nil, subsequent querying of this property will yield a non-nil dictionary reflecting the settings used by the AVCaptureSession's current sessionPreset.
--
-- On iOS versions prior to iOS 16.0, the only supported key is kCVPixelBufferPixelFormatTypeKey. Use -availableVideoCVPixelFormatTypes for the list of supported pixel formats. For apps linked on or after iOS 16.0, kCVPixelBufferPixelFormatTypeKey, kCVPixelBufferWidthKey, and kCVPixelBufferHeightKey are supported. The width and height must match the videoOrientation specified on the output's AVCaptureConnection or an NSInvalidArgumentException is thrown. The aspect ratio of width and height must match the aspect ratio of the source's activeFormat (corrected for the connection's videoOrientation) or an NSInvalidArgumentException is thrown. If width or height exceeds the source's activeFormat's width or height, an NSInvalidArgumentException is thrown. Changing width and height when deliversPreviewSizedOutputBuffers is set to YES is not supported and throws an NSInvalidArgumentException.
--
-- ObjC selector: @- setVideoSettings:@
setVideoSettings :: (IsAVCaptureVideoDataOutput avCaptureVideoDataOutput, IsNSDictionary value) => avCaptureVideoDataOutput -> value -> IO ()
setVideoSettings avCaptureVideoDataOutput  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avCaptureVideoDataOutput (mkSelector "setVideoSettings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Indicates the recommended media timescale for the video track.
--
-- - Returns: The recommended media timescale based on the active capture session's inputs. It is never less than 600. It may or may not be a multiple of 600.
--
-- ObjC selector: @- recommendedMediaTimeScaleForAssetWriter@
recommendedMediaTimeScaleForAssetWriter :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> IO CInt
recommendedMediaTimeScaleForAssetWriter avCaptureVideoDataOutput  =
    sendMsg avCaptureVideoDataOutput (mkSelector "recommendedMediaTimeScaleForAssetWriter") retCInt []

-- | availableVideoCVPixelFormatTypes
--
-- Indicates the supported video pixel formats that can be specified in videoSettings.
--
-- The value of this property is an NSArray of NSNumbers that can be used as values for the kCVPixelBufferPixelFormatTypeKey in the receiver's videoSettings property. The formats are listed in an unspecified order. This list can may change if the activeFormat of the AVCaptureDevice connected to the receiver changes.
--
-- ObjC selector: @- availableVideoCVPixelFormatTypes@
availableVideoCVPixelFormatTypes :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> IO (Id NSArray)
availableVideoCVPixelFormatTypes avCaptureVideoDataOutput  =
    sendMsg avCaptureVideoDataOutput (mkSelector "availableVideoCVPixelFormatTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | availableVideoCodecTypes
--
-- Indicates the supported video codec formats that can be specified in videoSettings.
--
-- The value of this property is an NSArray of AVVideoCodecTypes that can be used as values for the AVVideoCodecKey in the receiver's videoSettings property.
--
-- ObjC selector: @- availableVideoCodecTypes@
availableVideoCodecTypes :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> IO (Id NSArray)
availableVideoCodecTypes avCaptureVideoDataOutput  =
    sendMsg avCaptureVideoDataOutput (mkSelector "availableVideoCodecTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | alwaysDiscardsLateVideoFrames
--
-- Specifies whether the receiver should always discard any video frame that is not processed before the next frame is captured.
--
-- When the value of this property is YES, the receiver will immediately discard frames that are captured while the dispatch queue handling existing frames is blocked in the captureOutput:didOutputSampleBuffer:fromConnection: delegate method. When the value of this property is NO, delegates will be allowed more time to process old frames before new frames are discarded, but application memory usage may increase significantly as a result. The default value is YES.
--
-- ObjC selector: @- alwaysDiscardsLateVideoFrames@
alwaysDiscardsLateVideoFrames :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> IO Bool
alwaysDiscardsLateVideoFrames avCaptureVideoDataOutput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureVideoDataOutput (mkSelector "alwaysDiscardsLateVideoFrames") retCULong []

-- | alwaysDiscardsLateVideoFrames
--
-- Specifies whether the receiver should always discard any video frame that is not processed before the next frame is captured.
--
-- When the value of this property is YES, the receiver will immediately discard frames that are captured while the dispatch queue handling existing frames is blocked in the captureOutput:didOutputSampleBuffer:fromConnection: delegate method. When the value of this property is NO, delegates will be allowed more time to process old frames before new frames are discarded, but application memory usage may increase significantly as a result. The default value is YES.
--
-- ObjC selector: @- setAlwaysDiscardsLateVideoFrames:@
setAlwaysDiscardsLateVideoFrames :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> Bool -> IO ()
setAlwaysDiscardsLateVideoFrames avCaptureVideoDataOutput  value =
    sendMsg avCaptureVideoDataOutput (mkSelector "setAlwaysDiscardsLateVideoFrames:") retVoid [argCULong (if value then 1 else 0)]

-- | automaticallyConfiguresOutputBufferDimensions
--
-- Indicates whether the receiver automatically configures the size of output buffers.
--
-- Default value is YES. In most configurations, AVCaptureVideoDataOutput delivers full-resolution buffers, that is, buffers with the same dimensions as the source AVCaptureDevice's activeFormat's videoDimensions. When this property is set to YES, the receiver is free to configure the dimensions of the buffers delivered to -captureOutput:didOutputSampleBuffer:fromConnection:, such that they are a smaller preview size (roughly the size of the screen). For instance, when the AVCaptureSession's sessionPreset is set to AVCaptureSessionPresetPhoto, it is assumed that video data output buffers are being delivered as a preview proxy. Likewise, if an AVCapturePhotoOutput is present in the session with livePhotoCaptureEnabled, it is assumed that video data output is being used for photo preview, and thus preview-sized buffers are a better choice than full-res buffers. You can query deliversPreviewSizedOutputBuffers to find out whether automatic configuration of output buffer dimensions is currently downscaling buffers to a preview size. You can also query the videoSettings property to find out the exact width and height being delivered. If you wish to manually set deliversPreviewSizedOutputBuffers, you must first set automaticallyConfiguresOutputBufferDimensions to NO.
--
-- ObjC selector: @- automaticallyConfiguresOutputBufferDimensions@
automaticallyConfiguresOutputBufferDimensions :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> IO Bool
automaticallyConfiguresOutputBufferDimensions avCaptureVideoDataOutput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureVideoDataOutput (mkSelector "automaticallyConfiguresOutputBufferDimensions") retCULong []

-- | automaticallyConfiguresOutputBufferDimensions
--
-- Indicates whether the receiver automatically configures the size of output buffers.
--
-- Default value is YES. In most configurations, AVCaptureVideoDataOutput delivers full-resolution buffers, that is, buffers with the same dimensions as the source AVCaptureDevice's activeFormat's videoDimensions. When this property is set to YES, the receiver is free to configure the dimensions of the buffers delivered to -captureOutput:didOutputSampleBuffer:fromConnection:, such that they are a smaller preview size (roughly the size of the screen). For instance, when the AVCaptureSession's sessionPreset is set to AVCaptureSessionPresetPhoto, it is assumed that video data output buffers are being delivered as a preview proxy. Likewise, if an AVCapturePhotoOutput is present in the session with livePhotoCaptureEnabled, it is assumed that video data output is being used for photo preview, and thus preview-sized buffers are a better choice than full-res buffers. You can query deliversPreviewSizedOutputBuffers to find out whether automatic configuration of output buffer dimensions is currently downscaling buffers to a preview size. You can also query the videoSettings property to find out the exact width and height being delivered. If you wish to manually set deliversPreviewSizedOutputBuffers, you must first set automaticallyConfiguresOutputBufferDimensions to NO.
--
-- ObjC selector: @- setAutomaticallyConfiguresOutputBufferDimensions:@
setAutomaticallyConfiguresOutputBufferDimensions :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> Bool -> IO ()
setAutomaticallyConfiguresOutputBufferDimensions avCaptureVideoDataOutput  value =
    sendMsg avCaptureVideoDataOutput (mkSelector "setAutomaticallyConfiguresOutputBufferDimensions:") retVoid [argCULong (if value then 1 else 0)]

-- | deliversPreviewSizedOutputBuffers
--
-- Indicates whether the receiver is currently configured to deliver preview sized buffers.
--
-- If you wish to manually set deliversPreviewSizedOutputBuffers, you must first set automaticallyConfiguresOutputBufferDimensions to NO. When deliversPreviewSizedOutputBuffers is set to YES, auto focus, exposure, and white balance changes are quicker. AVCaptureVideoDataOutput assumes that the buffers are being used for on-screen preview rather than recording.
--
-- When AVCaptureDevice.activeFormat supports ProRes Raw video, setting deliversPreviewSizedOutputBuffers gives out buffers with 422 format that can be used for proxy video recording.
--
-- ObjC selector: @- deliversPreviewSizedOutputBuffers@
deliversPreviewSizedOutputBuffers :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> IO Bool
deliversPreviewSizedOutputBuffers avCaptureVideoDataOutput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureVideoDataOutput (mkSelector "deliversPreviewSizedOutputBuffers") retCULong []

-- | deliversPreviewSizedOutputBuffers
--
-- Indicates whether the receiver is currently configured to deliver preview sized buffers.
--
-- If you wish to manually set deliversPreviewSizedOutputBuffers, you must first set automaticallyConfiguresOutputBufferDimensions to NO. When deliversPreviewSizedOutputBuffers is set to YES, auto focus, exposure, and white balance changes are quicker. AVCaptureVideoDataOutput assumes that the buffers are being used for on-screen preview rather than recording.
--
-- When AVCaptureDevice.activeFormat supports ProRes Raw video, setting deliversPreviewSizedOutputBuffers gives out buffers with 422 format that can be used for proxy video recording.
--
-- ObjC selector: @- setDeliversPreviewSizedOutputBuffers:@
setDeliversPreviewSizedOutputBuffers :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> Bool -> IO ()
setDeliversPreviewSizedOutputBuffers avCaptureVideoDataOutput  value =
    sendMsg avCaptureVideoDataOutput (mkSelector "setDeliversPreviewSizedOutputBuffers:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates whether the receiver should prepare the cellular radio for imminent network activity.
--
-- Apps that scan video data output buffers for information that will result in network activity (such as detecting a QRCode containing a URL) should set this property @true@ to allow the cellular radio to prepare for an imminent network request. Enabling this property requires a lengthy reconfiguration of the capture render pipeline, so you should set this property to @true@ before calling ``AVCaptureSession/startRunning``.
--
-- Using this API requires your app to adopt the entitlement @com.apple.developer.avfoundation.video-data-output-prepares-cellular-radio-for-machine-readable-code-scanning@.
--
-- ObjC selector: @- preparesCellularRadioForNetworkConnection@
preparesCellularRadioForNetworkConnection :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> IO Bool
preparesCellularRadioForNetworkConnection avCaptureVideoDataOutput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureVideoDataOutput (mkSelector "preparesCellularRadioForNetworkConnection") retCULong []

-- | Indicates whether the receiver should prepare the cellular radio for imminent network activity.
--
-- Apps that scan video data output buffers for information that will result in network activity (such as detecting a QRCode containing a URL) should set this property @true@ to allow the cellular radio to prepare for an imminent network request. Enabling this property requires a lengthy reconfiguration of the capture render pipeline, so you should set this property to @true@ before calling ``AVCaptureSession/startRunning``.
--
-- Using this API requires your app to adopt the entitlement @com.apple.developer.avfoundation.video-data-output-prepares-cellular-radio-for-machine-readable-code-scanning@.
--
-- ObjC selector: @- setPreparesCellularRadioForNetworkConnection:@
setPreparesCellularRadioForNetworkConnection :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> Bool -> IO ()
setPreparesCellularRadioForNetworkConnection avCaptureVideoDataOutput  value =
    sendMsg avCaptureVideoDataOutput (mkSelector "setPreparesCellularRadioForNetworkConnection:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates whether the receiver should preserve dynamic HDR metadata as an attachment on the output sample buffer's underlying pixel buffer.
--
-- Set this property to @true@ if you wish to use ``AVCaptureVideoDataOutput`` with ``AVAssetWriter`` to record HDR movies. You must also set ``kVTCompressionPropertyKey_PreserveDynamicHDRMetadata`` to @true@ in the compression settings you pass to your ``AVAssetWriterInput``. These compression settings are represented under the ``AVVideoCompressionPropertiesKey`` sub-dictionary of your top-level AVVideoSettings (see <doc://com.apple.documentation/documentation/avfoundation/video-settings>). When you set this key to @true@, performance improves, as the encoder is able to skip HDR metadata calculation for every frame. The default value is @false@.
--
-- ObjC selector: @- preservesDynamicHDRMetadata@
preservesDynamicHDRMetadata :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> IO Bool
preservesDynamicHDRMetadata avCaptureVideoDataOutput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureVideoDataOutput (mkSelector "preservesDynamicHDRMetadata") retCULong []

-- | Indicates whether the receiver should preserve dynamic HDR metadata as an attachment on the output sample buffer's underlying pixel buffer.
--
-- Set this property to @true@ if you wish to use ``AVCaptureVideoDataOutput`` with ``AVAssetWriter`` to record HDR movies. You must also set ``kVTCompressionPropertyKey_PreserveDynamicHDRMetadata`` to @true@ in the compression settings you pass to your ``AVAssetWriterInput``. These compression settings are represented under the ``AVVideoCompressionPropertiesKey`` sub-dictionary of your top-level AVVideoSettings (see <doc://com.apple.documentation/documentation/avfoundation/video-settings>). When you set this key to @true@, performance improves, as the encoder is able to skip HDR metadata calculation for every frame. The default value is @false@.
--
-- ObjC selector: @- setPreservesDynamicHDRMetadata:@
setPreservesDynamicHDRMetadata :: IsAVCaptureVideoDataOutput avCaptureVideoDataOutput => avCaptureVideoDataOutput -> Bool -> IO ()
setPreservesDynamicHDRMetadata avCaptureVideoDataOutput  value =
    sendMsg avCaptureVideoDataOutput (mkSelector "setPreservesDynamicHDRMetadata:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @setSampleBufferDelegate:queue:@
setSampleBufferDelegate_queueSelector :: Selector
setSampleBufferDelegate_queueSelector = mkSelector "setSampleBufferDelegate:queue:"

-- | @Selector@ for @recommendedVideoSettingsForAssetWriterWithOutputFileType:@
recommendedVideoSettingsForAssetWriterWithOutputFileTypeSelector :: Selector
recommendedVideoSettingsForAssetWriterWithOutputFileTypeSelector = mkSelector "recommendedVideoSettingsForAssetWriterWithOutputFileType:"

-- | @Selector@ for @availableVideoCodecTypesForAssetWriterWithOutputFileType:@
availableVideoCodecTypesForAssetWriterWithOutputFileTypeSelector :: Selector
availableVideoCodecTypesForAssetWriterWithOutputFileTypeSelector = mkSelector "availableVideoCodecTypesForAssetWriterWithOutputFileType:"

-- | @Selector@ for @recommendedVideoSettingsForVideoCodecType:assetWriterOutputFileType:@
recommendedVideoSettingsForVideoCodecType_assetWriterOutputFileTypeSelector :: Selector
recommendedVideoSettingsForVideoCodecType_assetWriterOutputFileTypeSelector = mkSelector "recommendedVideoSettingsForVideoCodecType:assetWriterOutputFileType:"

-- | @Selector@ for @recommendedVideoSettingsForVideoCodecType:assetWriterOutputFileType:outputFileURL:@
recommendedVideoSettingsForVideoCodecType_assetWriterOutputFileType_outputFileURLSelector :: Selector
recommendedVideoSettingsForVideoCodecType_assetWriterOutputFileType_outputFileURLSelector = mkSelector "recommendedVideoSettingsForVideoCodecType:assetWriterOutputFileType:outputFileURL:"

-- | @Selector@ for @recommendedMovieMetadataForVideoCodecType:assetWriterOutputFileType:@
recommendedMovieMetadataForVideoCodecType_assetWriterOutputFileTypeSelector :: Selector
recommendedMovieMetadataForVideoCodecType_assetWriterOutputFileTypeSelector = mkSelector "recommendedMovieMetadataForVideoCodecType:assetWriterOutputFileType:"

-- | @Selector@ for @sampleBufferDelegate@
sampleBufferDelegateSelector :: Selector
sampleBufferDelegateSelector = mkSelector "sampleBufferDelegate"

-- | @Selector@ for @sampleBufferCallbackQueue@
sampleBufferCallbackQueueSelector :: Selector
sampleBufferCallbackQueueSelector = mkSelector "sampleBufferCallbackQueue"

-- | @Selector@ for @videoSettings@
videoSettingsSelector :: Selector
videoSettingsSelector = mkSelector "videoSettings"

-- | @Selector@ for @setVideoSettings:@
setVideoSettingsSelector :: Selector
setVideoSettingsSelector = mkSelector "setVideoSettings:"

-- | @Selector@ for @recommendedMediaTimeScaleForAssetWriter@
recommendedMediaTimeScaleForAssetWriterSelector :: Selector
recommendedMediaTimeScaleForAssetWriterSelector = mkSelector "recommendedMediaTimeScaleForAssetWriter"

-- | @Selector@ for @availableVideoCVPixelFormatTypes@
availableVideoCVPixelFormatTypesSelector :: Selector
availableVideoCVPixelFormatTypesSelector = mkSelector "availableVideoCVPixelFormatTypes"

-- | @Selector@ for @availableVideoCodecTypes@
availableVideoCodecTypesSelector :: Selector
availableVideoCodecTypesSelector = mkSelector "availableVideoCodecTypes"

-- | @Selector@ for @alwaysDiscardsLateVideoFrames@
alwaysDiscardsLateVideoFramesSelector :: Selector
alwaysDiscardsLateVideoFramesSelector = mkSelector "alwaysDiscardsLateVideoFrames"

-- | @Selector@ for @setAlwaysDiscardsLateVideoFrames:@
setAlwaysDiscardsLateVideoFramesSelector :: Selector
setAlwaysDiscardsLateVideoFramesSelector = mkSelector "setAlwaysDiscardsLateVideoFrames:"

-- | @Selector@ for @automaticallyConfiguresOutputBufferDimensions@
automaticallyConfiguresOutputBufferDimensionsSelector :: Selector
automaticallyConfiguresOutputBufferDimensionsSelector = mkSelector "automaticallyConfiguresOutputBufferDimensions"

-- | @Selector@ for @setAutomaticallyConfiguresOutputBufferDimensions:@
setAutomaticallyConfiguresOutputBufferDimensionsSelector :: Selector
setAutomaticallyConfiguresOutputBufferDimensionsSelector = mkSelector "setAutomaticallyConfiguresOutputBufferDimensions:"

-- | @Selector@ for @deliversPreviewSizedOutputBuffers@
deliversPreviewSizedOutputBuffersSelector :: Selector
deliversPreviewSizedOutputBuffersSelector = mkSelector "deliversPreviewSizedOutputBuffers"

-- | @Selector@ for @setDeliversPreviewSizedOutputBuffers:@
setDeliversPreviewSizedOutputBuffersSelector :: Selector
setDeliversPreviewSizedOutputBuffersSelector = mkSelector "setDeliversPreviewSizedOutputBuffers:"

-- | @Selector@ for @preparesCellularRadioForNetworkConnection@
preparesCellularRadioForNetworkConnectionSelector :: Selector
preparesCellularRadioForNetworkConnectionSelector = mkSelector "preparesCellularRadioForNetworkConnection"

-- | @Selector@ for @setPreparesCellularRadioForNetworkConnection:@
setPreparesCellularRadioForNetworkConnectionSelector :: Selector
setPreparesCellularRadioForNetworkConnectionSelector = mkSelector "setPreparesCellularRadioForNetworkConnection:"

-- | @Selector@ for @preservesDynamicHDRMetadata@
preservesDynamicHDRMetadataSelector :: Selector
preservesDynamicHDRMetadataSelector = mkSelector "preservesDynamicHDRMetadata"

-- | @Selector@ for @setPreservesDynamicHDRMetadata:@
setPreservesDynamicHDRMetadataSelector :: Selector
setPreservesDynamicHDRMetadataSelector = mkSelector "setPreservesDynamicHDRMetadata:"

