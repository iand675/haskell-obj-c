{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureStillImageOutput
--
-- AVCaptureStillImageOutput is a concrete subclass of AVCaptureOutput that can be used to capture high-quality still images with accompanying metadata.
--
-- Instances of AVCaptureStillImageOutput can be used to capture, on demand, high quality snapshots from a realtime capture source. Clients can request a still image for the current time using the captureStillImageAsynchronouslyFromConnection:completionHandler: method. Clients can also configure still image outputs to produce still images in specific image formats.
--
-- Generated bindings for @AVCaptureStillImageOutput@.
module ObjC.AVFoundation.AVCaptureStillImageOutput
  ( AVCaptureStillImageOutput
  , IsAVCaptureStillImageOutput(..)
  , init_
  , new
  , captureStillImageAsynchronouslyFromConnection_completionHandler
  , jpegStillImageNSDataRepresentation
  , prepareToCaptureStillImageBracketFromConnection_withSettingsArray_completionHandler
  , captureStillImageBracketAsynchronouslyFromConnection_withSettingsArray_completionHandler
  , outputSettings
  , setOutputSettings
  , availableImageDataCVPixelFormatTypes
  , availableImageDataCodecTypes
  , stillImageStabilizationSupported
  , automaticallyEnablesStillImageStabilizationWhenAvailable
  , setAutomaticallyEnablesStillImageStabilizationWhenAvailable
  , stillImageStabilizationActive
  , highResolutionStillImageOutputEnabled
  , setHighResolutionStillImageOutputEnabled
  , cameraSensorOrientationCompensationSupported
  , cameraSensorOrientationCompensationEnabled
  , setCameraSensorOrientationCompensationEnabled
  , capturingStillImage
  , maxBracketedCaptureStillImageCount
  , lensStabilizationDuringBracketedCaptureSupported
  , lensStabilizationDuringBracketedCaptureEnabled
  , setLensStabilizationDuringBracketedCaptureEnabled
  , automaticallyEnablesStillImageStabilizationWhenAvailableSelector
  , availableImageDataCVPixelFormatTypesSelector
  , availableImageDataCodecTypesSelector
  , cameraSensorOrientationCompensationEnabledSelector
  , cameraSensorOrientationCompensationSupportedSelector
  , captureStillImageAsynchronouslyFromConnection_completionHandlerSelector
  , captureStillImageBracketAsynchronouslyFromConnection_withSettingsArray_completionHandlerSelector
  , capturingStillImageSelector
  , highResolutionStillImageOutputEnabledSelector
  , initSelector
  , jpegStillImageNSDataRepresentationSelector
  , lensStabilizationDuringBracketedCaptureEnabledSelector
  , lensStabilizationDuringBracketedCaptureSupportedSelector
  , maxBracketedCaptureStillImageCountSelector
  , newSelector
  , outputSettingsSelector
  , prepareToCaptureStillImageBracketFromConnection_withSettingsArray_completionHandlerSelector
  , setAutomaticallyEnablesStillImageStabilizationWhenAvailableSelector
  , setCameraSensorOrientationCompensationEnabledSelector
  , setHighResolutionStillImageOutputEnabledSelector
  , setLensStabilizationDuringBracketedCaptureEnabledSelector
  , setOutputSettingsSelector
  , stillImageStabilizationActiveSelector
  , stillImageStabilizationSupportedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO (Id AVCaptureStillImageOutput)
init_ avCaptureStillImageOutput =
  sendOwnedMessage avCaptureStillImageOutput initSelector

-- | @+ new@
new :: IO (Id AVCaptureStillImageOutput)
new  =
  do
    cls' <- getRequiredClass "AVCaptureStillImageOutput"
    sendOwnedClassMessage cls' newSelector

-- | captureStillImageAsynchronouslyFromConnection:completionHandler:
--
-- Initiates an asynchronous still image capture, returning the result to a completion handler.
--
-- @connection@ — The AVCaptureConnection object from which to capture the still image.
--
-- @handler@ — A block that will be called when the still image capture is complete. The block will be passed a CMSampleBuffer object containing the image data or an NSError object if an image could not be captured.
--
-- This method will return immediately after it is invoked, later calling the provided completion handler block when image data is ready. If the request could not be completed, the error parameter will contain an NSError object describing the failure.
--
-- Attachments to the image data sample buffer may contain metadata appropriate to the image data format. For instance, a sample buffer containing JPEG data may carry a kCGImagePropertyExifDictionary as an attachment. See <ImageIO/CGImageProperties.h> for a list of keys and value types.
--
-- Clients should not assume that the completion handler will be called on a specific thread.
--
-- Calls to captureStillImageAsynchronouslyFromConnection:completionHandler: are not synchronized with AVCaptureDevice manual control completion handlers. Setting a device manual control, waiting for its completion, then calling captureStillImageAsynchronouslyFromConnection:completionHandler: DOES NOT ensure that the still image returned reflects your manual control change. It may be from an earlier time. You can compare your manual control completion handler sync time to the returned still image's presentation time. You can retrieve the sample buffer's pts using CMSampleBufferGetPresentationTimestamp(). If the still image has an earlier timestamp, your manual control command does not apply to it.
--
-- ObjC selector: @- captureStillImageAsynchronouslyFromConnection:completionHandler:@
captureStillImageAsynchronouslyFromConnection_completionHandler :: (IsAVCaptureStillImageOutput avCaptureStillImageOutput, IsAVCaptureConnection connection) => avCaptureStillImageOutput -> connection -> Ptr () -> IO ()
captureStillImageAsynchronouslyFromConnection_completionHandler avCaptureStillImageOutput connection handler =
  sendMessage avCaptureStillImageOutput captureStillImageAsynchronouslyFromConnection_completionHandlerSelector (toAVCaptureConnection connection) handler

-- | jpegStillImageNSDataRepresentation:
--
-- Converts the still image data and metadata attachments in a JPEG sample buffer to an NSData representation.
--
-- @jpegSampleBuffer@ — The sample buffer carrying JPEG image data, optionally with Exif metadata sample buffer attachments. This method throws an NSInvalidArgumentException if jpegSampleBuffer is NULL or not in the JPEG format.
--
-- This method returns an NSData representation of a JPEG still image sample buffer, merging the image data and Exif metadata sample buffer attachments without recompressing the image. The returned NSData is suitable for writing to disk.
--
-- ObjC selector: @+ jpegStillImageNSDataRepresentation:@
jpegStillImageNSDataRepresentation :: Ptr () -> IO (Id NSData)
jpegStillImageNSDataRepresentation jpegSampleBuffer =
  do
    cls' <- getRequiredClass "AVCaptureStillImageOutput"
    sendClassMessage cls' jpegStillImageNSDataRepresentationSelector jpegSampleBuffer

-- | prepareToCaptureStillImageBracketFromConnection:withSettingsArray:completionHandler:
--
-- Allows the receiver to prepare resources in advance of capturing a still image bracket.
--
-- @connection@ — The connection through which the still image bracket should be captured.
--
-- @settings@ — An array of AVCaptureBracketedStillImageSettings objects. All must be of the same kind of AVCaptureBracketedStillImageSettings subclass, or an NSInvalidArgumentException is thrown.
--
-- @handler@ — A user provided block that will be called asynchronously once resources have successfully been allocated for the specified bracketed capture operation. If sufficient resources could not be allocated, the "prepared" parameter contains NO, and "error" parameter contains a non-nil error value. If [settings count] exceeds -maxBracketedCaptureStillImageCount, then AVErrorMaximumStillImageCaptureRequestsExceeded is returned. You should not assume that the completion handler will be called on a specific thread.
--
-- -maxBracketedCaptureStillImageCount tells you the maximum number of images that may be taken in a single bracket given the current AVCaptureDevice/AVCaptureSession/AVCaptureStillImageOutput configuration. But before taking a still image bracket, additional resources may need to be allocated. By calling -prepareToCaptureStillImageBracketFromConnection:withSettingsArray:completionHandler: first, you are able to deterministically know when the receiver is ready to capture the bracket with the specified settings array.
--
-- ObjC selector: @- prepareToCaptureStillImageBracketFromConnection:withSettingsArray:completionHandler:@
prepareToCaptureStillImageBracketFromConnection_withSettingsArray_completionHandler :: (IsAVCaptureStillImageOutput avCaptureStillImageOutput, IsAVCaptureConnection connection, IsNSArray settings) => avCaptureStillImageOutput -> connection -> settings -> Ptr () -> IO ()
prepareToCaptureStillImageBracketFromConnection_withSettingsArray_completionHandler avCaptureStillImageOutput connection settings handler =
  sendMessage avCaptureStillImageOutput prepareToCaptureStillImageBracketFromConnection_withSettingsArray_completionHandlerSelector (toAVCaptureConnection connection) (toNSArray settings) handler

-- | captureStillImageBracketAsynchronouslyFromConnection:withSettingsArray:completionHandler:
--
-- Captures a still image bracket.
--
-- @connection@ — The connection through which the still image bracket should be captured.
--
-- @settings@ — An array of AVCaptureBracketedStillImageSettings objects. All must be of the same kind of AVCaptureBracketedStillImageSettings subclass, or an NSInvalidArgumentException is thrown.
--
-- @handler@ — A user provided block that will be called asynchronously as each still image in the bracket is captured. If the capture request is successful, the "sampleBuffer" parameter contains a valid CMSampleBuffer, the "stillImageSettings" parameter contains the settings object corresponding to this still image, and a nil "error" parameter. If the bracketed capture fails, sample buffer is NULL and error is non-nil. If [settings count] exceeds -maxBracketedCaptureStillImageCount, then AVErrorMaximumStillImageCaptureRequestsExceeded is returned. You should not assume that the completion handler will be called on a specific thread.
--
-- If you have not called -prepareToCaptureStillImageBracketFromConnection:withSettingsArray:completionHandler: for this still image bracket request, the bracket may not be taken immediately, as the receiver may internally need to prepare resources.
--
-- ObjC selector: @- captureStillImageBracketAsynchronouslyFromConnection:withSettingsArray:completionHandler:@
captureStillImageBracketAsynchronouslyFromConnection_withSettingsArray_completionHandler :: (IsAVCaptureStillImageOutput avCaptureStillImageOutput, IsAVCaptureConnection connection, IsNSArray settings) => avCaptureStillImageOutput -> connection -> settings -> Ptr () -> IO ()
captureStillImageBracketAsynchronouslyFromConnection_withSettingsArray_completionHandler avCaptureStillImageOutput connection settings handler =
  sendMessage avCaptureStillImageOutput captureStillImageBracketAsynchronouslyFromConnection_withSettingsArray_completionHandlerSelector (toAVCaptureConnection connection) (toNSArray settings) handler

-- | outputSettings
--
-- Specifies the options the receiver uses to encode still images before they are delivered.
--
-- See AVVideoSettings.h for more information on how to construct an output settings dictionary.
--
-- On iOS, the only currently supported keys are AVVideoCodecKey and kCVPixelBufferPixelFormatTypeKey. Use -availableImageDataCVPixelFormatTypes and -availableImageDataCodecTypes to determine what codec keys and pixel formats are supported. AVVideoQualityKey is supported on iOS 6.0 and later and may only be used when AVVideoCodecKey is set to AVVideoCodecTypeJPEG.
--
-- ObjC selector: @- outputSettings@
outputSettings :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO (Id NSDictionary)
outputSettings avCaptureStillImageOutput =
  sendMessage avCaptureStillImageOutput outputSettingsSelector

-- | outputSettings
--
-- Specifies the options the receiver uses to encode still images before they are delivered.
--
-- See AVVideoSettings.h for more information on how to construct an output settings dictionary.
--
-- On iOS, the only currently supported keys are AVVideoCodecKey and kCVPixelBufferPixelFormatTypeKey. Use -availableImageDataCVPixelFormatTypes and -availableImageDataCodecTypes to determine what codec keys and pixel formats are supported. AVVideoQualityKey is supported on iOS 6.0 and later and may only be used when AVVideoCodecKey is set to AVVideoCodecTypeJPEG.
--
-- ObjC selector: @- setOutputSettings:@
setOutputSettings :: (IsAVCaptureStillImageOutput avCaptureStillImageOutput, IsNSDictionary value) => avCaptureStillImageOutput -> value -> IO ()
setOutputSettings avCaptureStillImageOutput value =
  sendMessage avCaptureStillImageOutput setOutputSettingsSelector (toNSDictionary value)

-- | availableImageDataCVPixelFormatTypes
--
-- Indicates the supported image pixel formats that can be specified in outputSettings.
--
-- The value of this property is an NSArray of NSNumbers that can be used as values for the kCVPixelBufferPixelFormatTypeKey in the receiver's outputSettings property. The first format in the returned list is the most efficient output format.
--
-- ObjC selector: @- availableImageDataCVPixelFormatTypes@
availableImageDataCVPixelFormatTypes :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO (Id NSArray)
availableImageDataCVPixelFormatTypes avCaptureStillImageOutput =
  sendMessage avCaptureStillImageOutput availableImageDataCVPixelFormatTypesSelector

-- | availableImageDataCodecTypes
--
-- Indicates the supported image codec formats that can be specified in outputSettings.
--
-- The value of this property is an NSArray of AVVideoCodecTypes that can be used as values for the AVVideoCodecKey in the receiver's outputSettings property.
--
-- ObjC selector: @- availableImageDataCodecTypes@
availableImageDataCodecTypes :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO (Id NSArray)
availableImageDataCodecTypes avCaptureStillImageOutput =
  sendMessage avCaptureStillImageOutput availableImageDataCodecTypesSelector

-- | stillImageStabilizationSupported
--
-- Indicates whether the receiver supports still image stabilization.
--
-- The receiver's automaticallyEnablesStillImageStabilizationWhenAvailable property can only be set if this property returns YES. Its value may change as the session's -sessionPreset or input device's -activeFormat changes.
--
-- ObjC selector: @- stillImageStabilizationSupported@
stillImageStabilizationSupported :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO Bool
stillImageStabilizationSupported avCaptureStillImageOutput =
  sendMessage avCaptureStillImageOutput stillImageStabilizationSupportedSelector

-- | automaticallyEnablesStillImageStabilizationWhenAvailable
--
-- Indicates whether the receiver should automatically use still image stabilization when necessary.
--
-- On a receiver where -isStillImageStabilizationSupported returns YES, image stabilization may be applied to reduce blur commonly found in low light photos. When stabilization is enabled, still image captures incur additional latency. The default value is YES when supported, NO otherwise. Setting this property throws an NSInvalidArgumentException if -isStillImageStabilizationSupported returns NO.
--
-- ObjC selector: @- automaticallyEnablesStillImageStabilizationWhenAvailable@
automaticallyEnablesStillImageStabilizationWhenAvailable :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO Bool
automaticallyEnablesStillImageStabilizationWhenAvailable avCaptureStillImageOutput =
  sendMessage avCaptureStillImageOutput automaticallyEnablesStillImageStabilizationWhenAvailableSelector

-- | automaticallyEnablesStillImageStabilizationWhenAvailable
--
-- Indicates whether the receiver should automatically use still image stabilization when necessary.
--
-- On a receiver where -isStillImageStabilizationSupported returns YES, image stabilization may be applied to reduce blur commonly found in low light photos. When stabilization is enabled, still image captures incur additional latency. The default value is YES when supported, NO otherwise. Setting this property throws an NSInvalidArgumentException if -isStillImageStabilizationSupported returns NO.
--
-- ObjC selector: @- setAutomaticallyEnablesStillImageStabilizationWhenAvailable:@
setAutomaticallyEnablesStillImageStabilizationWhenAvailable :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> Bool -> IO ()
setAutomaticallyEnablesStillImageStabilizationWhenAvailable avCaptureStillImageOutput value =
  sendMessage avCaptureStillImageOutput setAutomaticallyEnablesStillImageStabilizationWhenAvailableSelector value

-- | stillImageStabilizationActive
--
-- Indicates whether still image stabilization is in use for the current capture.
--
-- On a receiver where -isStillImageStabilizationSupported returns YES, and automaticallyEnablesStillImageStabilizationWhenAvailable is set to YES, this property may be key-value observed, or queried from inside your key-value observation callback for the "capturingStillImage" property, to find out if still image stabilization is being applied to the current capture.
--
-- ObjC selector: @- stillImageStabilizationActive@
stillImageStabilizationActive :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO Bool
stillImageStabilizationActive avCaptureStillImageOutput =
  sendMessage avCaptureStillImageOutput stillImageStabilizationActiveSelector

-- | highResolutionStillImageOutputEnabled
--
-- Indicates whether the receiver should emit still images at the highest resolution supported by its source AVCaptureDevice's activeFormat.
--
-- By default, AVCaptureStillImageOutput emits images with the same dimensions as its source AVCaptureDevice's activeFormat.formatDescription. However, if you set this property to YES, the receiver emits still images at its source AVCaptureDevice's activeFormat.highResolutionStillImageDimensions. Note that if you enable video stabilization (see AVCaptureConnection's preferredVideoStabilizationMode) for any output, the high resolution still images emitted by AVCaptureStillImageOutput may be smaller by 10 or more percent.
--
-- ObjC selector: @- highResolutionStillImageOutputEnabled@
highResolutionStillImageOutputEnabled :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO Bool
highResolutionStillImageOutputEnabled avCaptureStillImageOutput =
  sendMessage avCaptureStillImageOutput highResolutionStillImageOutputEnabledSelector

-- | highResolutionStillImageOutputEnabled
--
-- Indicates whether the receiver should emit still images at the highest resolution supported by its source AVCaptureDevice's activeFormat.
--
-- By default, AVCaptureStillImageOutput emits images with the same dimensions as its source AVCaptureDevice's activeFormat.formatDescription. However, if you set this property to YES, the receiver emits still images at its source AVCaptureDevice's activeFormat.highResolutionStillImageDimensions. Note that if you enable video stabilization (see AVCaptureConnection's preferredVideoStabilizationMode) for any output, the high resolution still images emitted by AVCaptureStillImageOutput may be smaller by 10 or more percent.
--
-- ObjC selector: @- setHighResolutionStillImageOutputEnabled:@
setHighResolutionStillImageOutputEnabled :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> Bool -> IO ()
setHighResolutionStillImageOutputEnabled avCaptureStillImageOutput value =
  sendMessage avCaptureStillImageOutput setHighResolutionStillImageOutputEnabledSelector value

-- | cameraSensorOrientationCompensationSupported
--
-- A read-only BOOL value indicating whether still image buffers may be rotated to match the sensor orientation of earlier generation hardware.
--
-- Value is YES for camera configurations which support compensation for the sensor orientation, which is applied to HEIC, JPEG, and uncompressed processed photos only; compensation is never applied to Bayer RAW or Apple ProRaw captures.
--
-- ObjC selector: @- cameraSensorOrientationCompensationSupported@
cameraSensorOrientationCompensationSupported :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO Bool
cameraSensorOrientationCompensationSupported avCaptureStillImageOutput =
  sendMessage avCaptureStillImageOutput cameraSensorOrientationCompensationSupportedSelector

-- | cameraSensorOrientationCompensationEnabled
--
-- A BOOL value indicating that still image buffers will be rotated to match the sensor orientation of earlier generation hardware.
--
-- Default is YES when cameraSensorOrientationCompensationSupported is YES. Set to NO if your app does not require sensor orientation compensation.
--
-- ObjC selector: @- cameraSensorOrientationCompensationEnabled@
cameraSensorOrientationCompensationEnabled :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO Bool
cameraSensorOrientationCompensationEnabled avCaptureStillImageOutput =
  sendMessage avCaptureStillImageOutput cameraSensorOrientationCompensationEnabledSelector

-- | cameraSensorOrientationCompensationEnabled
--
-- A BOOL value indicating that still image buffers will be rotated to match the sensor orientation of earlier generation hardware.
--
-- Default is YES when cameraSensorOrientationCompensationSupported is YES. Set to NO if your app does not require sensor orientation compensation.
--
-- ObjC selector: @- setCameraSensorOrientationCompensationEnabled:@
setCameraSensorOrientationCompensationEnabled :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> Bool -> IO ()
setCameraSensorOrientationCompensationEnabled avCaptureStillImageOutput value =
  sendMessage avCaptureStillImageOutput setCameraSensorOrientationCompensationEnabledSelector value

-- | capturingStillImage
--
-- A boolean value that becomes true when a still image is being captured.
--
-- The value of this property is a BOOL that becomes true when a still image is being captured, and false when no still image capture is underway. This property is key-value observable.
--
-- ObjC selector: @- capturingStillImage@
capturingStillImage :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO Bool
capturingStillImage avCaptureStillImageOutput =
  sendMessage avCaptureStillImageOutput capturingStillImageSelector

-- | maxBracketedCaptureStillImageCount
--
-- Specifies the maximum number of still images that may be taken in a single bracket.
--
-- AVCaptureStillImageOutput can only satisfy a limited number of image requests in a single bracket without exhausting system resources. The maximum number of still images that may be taken in a single bracket depends on the size of the images being captured, and consequently may vary with AVCaptureSession -sessionPreset and AVCaptureDevice -activeFormat. Some formats do not support bracketed capture and return a maxBracketedCaptureStillImageCount of 0. This read-only property is key-value observable. If you exceed -maxBracketedCaptureStillImageCount, then -captureStillImageBracketAsynchronouslyFromConnection:withSettingsArray:completionHandler: fails and the completionHandler is called [settings count] times with a NULL sample buffer and AVErrorMaximumStillImageCaptureRequestsExceeded.
--
-- ObjC selector: @- maxBracketedCaptureStillImageCount@
maxBracketedCaptureStillImageCount :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO CULong
maxBracketedCaptureStillImageCount avCaptureStillImageOutput =
  sendMessage avCaptureStillImageOutput maxBracketedCaptureStillImageCountSelector

-- | lensStabilizationDuringBracketedCaptureSupported
--
-- Indicates whether the receiver supports lens stabilization during bracketed captures.
--
-- The receiver's lensStabilizationDuringBracketedCaptureEnabled property can only be set if this property returns YES. Its value may change as the session's -sessionPreset or input device's -activeFormat changes. This read-only property is key-value observable.
--
-- ObjC selector: @- lensStabilizationDuringBracketedCaptureSupported@
lensStabilizationDuringBracketedCaptureSupported :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO Bool
lensStabilizationDuringBracketedCaptureSupported avCaptureStillImageOutput =
  sendMessage avCaptureStillImageOutput lensStabilizationDuringBracketedCaptureSupportedSelector

-- | lensStabilizationDuringBracketedCaptureEnabled
--
-- Indicates whether the receiver should use lens stabilization during bracketed captures.
--
-- On a receiver where -isLensStabilizationDuringBracketedCaptureSupported returns YES, lens stabilization may be applied to the bracket to reduce blur commonly found in low light photos. When lens stabilization is enabled, bracketed still image captures incur additional latency. Lens stabilization is more effective with longer-exposure captures, and offers limited or no benefit for exposure durations shorter than 1/30 of a second. It is possible that during the bracket, the lens stabilization module may run out of correction range and therefore will not be active for every frame in the bracket. Each emitted CMSampleBuffer from the bracket will have an attachment of kCMSampleBufferAttachmentKey_StillImageLensStabilizationInfo indicating additional information about stabilization was applied to the buffer, if any. The default value of -isLensStabilizationDuringBracketedCaptureEnabled is NO. This value will be set to NO when -isLensStabilizationDuringBracketedCaptureSupported changes to NO. Setting this property throws an NSInvalidArgumentException if -isLensStabilizationDuringBracketedCaptureSupported returns NO. This property is key-value observable.
--
-- ObjC selector: @- lensStabilizationDuringBracketedCaptureEnabled@
lensStabilizationDuringBracketedCaptureEnabled :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> IO Bool
lensStabilizationDuringBracketedCaptureEnabled avCaptureStillImageOutput =
  sendMessage avCaptureStillImageOutput lensStabilizationDuringBracketedCaptureEnabledSelector

-- | lensStabilizationDuringBracketedCaptureEnabled
--
-- Indicates whether the receiver should use lens stabilization during bracketed captures.
--
-- On a receiver where -isLensStabilizationDuringBracketedCaptureSupported returns YES, lens stabilization may be applied to the bracket to reduce blur commonly found in low light photos. When lens stabilization is enabled, bracketed still image captures incur additional latency. Lens stabilization is more effective with longer-exposure captures, and offers limited or no benefit for exposure durations shorter than 1/30 of a second. It is possible that during the bracket, the lens stabilization module may run out of correction range and therefore will not be active for every frame in the bracket. Each emitted CMSampleBuffer from the bracket will have an attachment of kCMSampleBufferAttachmentKey_StillImageLensStabilizationInfo indicating additional information about stabilization was applied to the buffer, if any. The default value of -isLensStabilizationDuringBracketedCaptureEnabled is NO. This value will be set to NO when -isLensStabilizationDuringBracketedCaptureSupported changes to NO. Setting this property throws an NSInvalidArgumentException if -isLensStabilizationDuringBracketedCaptureSupported returns NO. This property is key-value observable.
--
-- ObjC selector: @- setLensStabilizationDuringBracketedCaptureEnabled:@
setLensStabilizationDuringBracketedCaptureEnabled :: IsAVCaptureStillImageOutput avCaptureStillImageOutput => avCaptureStillImageOutput -> Bool -> IO ()
setLensStabilizationDuringBracketedCaptureEnabled avCaptureStillImageOutput value =
  sendMessage avCaptureStillImageOutput setLensStabilizationDuringBracketedCaptureEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureStillImageOutput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureStillImageOutput)
newSelector = mkSelector "new"

-- | @Selector@ for @captureStillImageAsynchronouslyFromConnection:completionHandler:@
captureStillImageAsynchronouslyFromConnection_completionHandlerSelector :: Selector '[Id AVCaptureConnection, Ptr ()] ()
captureStillImageAsynchronouslyFromConnection_completionHandlerSelector = mkSelector "captureStillImageAsynchronouslyFromConnection:completionHandler:"

-- | @Selector@ for @jpegStillImageNSDataRepresentation:@
jpegStillImageNSDataRepresentationSelector :: Selector '[Ptr ()] (Id NSData)
jpegStillImageNSDataRepresentationSelector = mkSelector "jpegStillImageNSDataRepresentation:"

-- | @Selector@ for @prepareToCaptureStillImageBracketFromConnection:withSettingsArray:completionHandler:@
prepareToCaptureStillImageBracketFromConnection_withSettingsArray_completionHandlerSelector :: Selector '[Id AVCaptureConnection, Id NSArray, Ptr ()] ()
prepareToCaptureStillImageBracketFromConnection_withSettingsArray_completionHandlerSelector = mkSelector "prepareToCaptureStillImageBracketFromConnection:withSettingsArray:completionHandler:"

-- | @Selector@ for @captureStillImageBracketAsynchronouslyFromConnection:withSettingsArray:completionHandler:@
captureStillImageBracketAsynchronouslyFromConnection_withSettingsArray_completionHandlerSelector :: Selector '[Id AVCaptureConnection, Id NSArray, Ptr ()] ()
captureStillImageBracketAsynchronouslyFromConnection_withSettingsArray_completionHandlerSelector = mkSelector "captureStillImageBracketAsynchronouslyFromConnection:withSettingsArray:completionHandler:"

-- | @Selector@ for @outputSettings@
outputSettingsSelector :: Selector '[] (Id NSDictionary)
outputSettingsSelector = mkSelector "outputSettings"

-- | @Selector@ for @setOutputSettings:@
setOutputSettingsSelector :: Selector '[Id NSDictionary] ()
setOutputSettingsSelector = mkSelector "setOutputSettings:"

-- | @Selector@ for @availableImageDataCVPixelFormatTypes@
availableImageDataCVPixelFormatTypesSelector :: Selector '[] (Id NSArray)
availableImageDataCVPixelFormatTypesSelector = mkSelector "availableImageDataCVPixelFormatTypes"

-- | @Selector@ for @availableImageDataCodecTypes@
availableImageDataCodecTypesSelector :: Selector '[] (Id NSArray)
availableImageDataCodecTypesSelector = mkSelector "availableImageDataCodecTypes"

-- | @Selector@ for @stillImageStabilizationSupported@
stillImageStabilizationSupportedSelector :: Selector '[] Bool
stillImageStabilizationSupportedSelector = mkSelector "stillImageStabilizationSupported"

-- | @Selector@ for @automaticallyEnablesStillImageStabilizationWhenAvailable@
automaticallyEnablesStillImageStabilizationWhenAvailableSelector :: Selector '[] Bool
automaticallyEnablesStillImageStabilizationWhenAvailableSelector = mkSelector "automaticallyEnablesStillImageStabilizationWhenAvailable"

-- | @Selector@ for @setAutomaticallyEnablesStillImageStabilizationWhenAvailable:@
setAutomaticallyEnablesStillImageStabilizationWhenAvailableSelector :: Selector '[Bool] ()
setAutomaticallyEnablesStillImageStabilizationWhenAvailableSelector = mkSelector "setAutomaticallyEnablesStillImageStabilizationWhenAvailable:"

-- | @Selector@ for @stillImageStabilizationActive@
stillImageStabilizationActiveSelector :: Selector '[] Bool
stillImageStabilizationActiveSelector = mkSelector "stillImageStabilizationActive"

-- | @Selector@ for @highResolutionStillImageOutputEnabled@
highResolutionStillImageOutputEnabledSelector :: Selector '[] Bool
highResolutionStillImageOutputEnabledSelector = mkSelector "highResolutionStillImageOutputEnabled"

-- | @Selector@ for @setHighResolutionStillImageOutputEnabled:@
setHighResolutionStillImageOutputEnabledSelector :: Selector '[Bool] ()
setHighResolutionStillImageOutputEnabledSelector = mkSelector "setHighResolutionStillImageOutputEnabled:"

-- | @Selector@ for @cameraSensorOrientationCompensationSupported@
cameraSensorOrientationCompensationSupportedSelector :: Selector '[] Bool
cameraSensorOrientationCompensationSupportedSelector = mkSelector "cameraSensorOrientationCompensationSupported"

-- | @Selector@ for @cameraSensorOrientationCompensationEnabled@
cameraSensorOrientationCompensationEnabledSelector :: Selector '[] Bool
cameraSensorOrientationCompensationEnabledSelector = mkSelector "cameraSensorOrientationCompensationEnabled"

-- | @Selector@ for @setCameraSensorOrientationCompensationEnabled:@
setCameraSensorOrientationCompensationEnabledSelector :: Selector '[Bool] ()
setCameraSensorOrientationCompensationEnabledSelector = mkSelector "setCameraSensorOrientationCompensationEnabled:"

-- | @Selector@ for @capturingStillImage@
capturingStillImageSelector :: Selector '[] Bool
capturingStillImageSelector = mkSelector "capturingStillImage"

-- | @Selector@ for @maxBracketedCaptureStillImageCount@
maxBracketedCaptureStillImageCountSelector :: Selector '[] CULong
maxBracketedCaptureStillImageCountSelector = mkSelector "maxBracketedCaptureStillImageCount"

-- | @Selector@ for @lensStabilizationDuringBracketedCaptureSupported@
lensStabilizationDuringBracketedCaptureSupportedSelector :: Selector '[] Bool
lensStabilizationDuringBracketedCaptureSupportedSelector = mkSelector "lensStabilizationDuringBracketedCaptureSupported"

-- | @Selector@ for @lensStabilizationDuringBracketedCaptureEnabled@
lensStabilizationDuringBracketedCaptureEnabledSelector :: Selector '[] Bool
lensStabilizationDuringBracketedCaptureEnabledSelector = mkSelector "lensStabilizationDuringBracketedCaptureEnabled"

-- | @Selector@ for @setLensStabilizationDuringBracketedCaptureEnabled:@
setLensStabilizationDuringBracketedCaptureEnabledSelector :: Selector '[Bool] ()
setLensStabilizationDuringBracketedCaptureEnabledSelector = mkSelector "setLensStabilizationDuringBracketedCaptureEnabled:"

