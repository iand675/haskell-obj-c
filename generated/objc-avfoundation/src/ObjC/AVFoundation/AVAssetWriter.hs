{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetWriter
--
-- AVAssetWriter provides services for writing media data to a new file,
--
-- Instances of AVAssetWriter can write media to new files in formats such as the QuickTime movie file format or the MPEG-4 file format. AVAssetWriter has support for automatic interleaving of media data for multiple concurrent tracks. Source media data can be obtained from instances of AVAssetReader for one or more assets or from other sources outside of AVFoundation.
--
-- Instances of AVAssetWriter can re-encode media samples as they are written. Instances of AVAssetWriter can also optionally write metadata collections to the output file.
--
-- A single instance of AVAssetWriter can be used once to write to a single file. Clients that wish to write to files multiple times must use a new instance of AVAssetWriter each time.
--
-- Generated bindings for @AVAssetWriter@.
module ObjC.AVFoundation.AVAssetWriter
  ( AVAssetWriter
  , IsAVAssetWriter(..)
  , init_
  , new
  , assetWriterWithURL_fileType_error
  , initWithURL_fileType_error
  , initWithContentType
  , canApplyOutputSettings_forMediaType
  , canAddInput
  , addInput
  , startWriting
  , cancelWriting
  , finishWriting
  , finishWritingWithCompletionHandler
  , flushSegment
  , canAddInputGroup
  , addInputGroup
  , outputURL
  , outputFileType
  , availableMediaTypes
  , status
  , error_
  , metadata
  , setMetadata
  , shouldOptimizeForNetworkUse
  , setShouldOptimizeForNetworkUse
  , inputs
  , outputFileTypeProfile
  , setOutputFileTypeProfile
  , initialMovieFragmentSequenceNumber
  , setInitialMovieFragmentSequenceNumber
  , producesCombinableFragments
  , setProducesCombinableFragments
  , movieTimeScale
  , setMovieTimeScale
  , initSelector
  , newSelector
  , assetWriterWithURL_fileType_errorSelector
  , initWithURL_fileType_errorSelector
  , initWithContentTypeSelector
  , canApplyOutputSettings_forMediaTypeSelector
  , canAddInputSelector
  , addInputSelector
  , startWritingSelector
  , cancelWritingSelector
  , finishWritingSelector
  , finishWritingWithCompletionHandlerSelector
  , flushSegmentSelector
  , canAddInputGroupSelector
  , addInputGroupSelector
  , outputURLSelector
  , outputFileTypeSelector
  , availableMediaTypesSelector
  , statusSelector
  , errorSelector
  , metadataSelector
  , setMetadataSelector
  , shouldOptimizeForNetworkUseSelector
  , setShouldOptimizeForNetworkUseSelector
  , inputsSelector
  , outputFileTypeProfileSelector
  , setOutputFileTypeProfileSelector
  , initialMovieFragmentSequenceNumberSelector
  , setInitialMovieFragmentSequenceNumberSelector
  , producesCombinableFragmentsSelector
  , setProducesCombinableFragmentsSelector
  , movieTimeScaleSelector
  , setMovieTimeScaleSelector

  -- * Enum types
  , AVAssetWriterStatus(AVAssetWriterStatus)
  , pattern AVAssetWriterStatusUnknown
  , pattern AVAssetWriterStatusWriting
  , pattern AVAssetWriterStatusCompleted
  , pattern AVAssetWriterStatusFailed
  , pattern AVAssetWriterStatusCancelled

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
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- init@
init_ :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id AVAssetWriter)
init_ avAssetWriter  =
  sendMsg avAssetWriter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetWriter)
new  =
  do
    cls' <- getRequiredClass "AVAssetWriter"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | assetWriterWithURL:fileType:error:
--
-- Returns an instance of AVAssetWriter configured to write to a file in a specified container format.
--
-- @URL@ — The location of the file to be written. The URL must be a file URL.
--
-- @fileType@ — A UTI indicating the format of the file to be written.
--
-- @outError@ — On return, if initialization of the AVAssetWriter fails, points to an NSError describing the nature of the failure.
--
-- Returns: An instance of AVAssetWriter.
--
-- Writing will fail if a file already exists at the specified URL.
--
-- UTIs for container formats that can be written are declared in AVMediaFormat.h.
--
-- ObjC selector: @+ assetWriterWithURL:fileType:error:@
assetWriterWithURL_fileType_error :: (IsNSURL outputURL, IsNSString outputFileType, IsNSError outError) => outputURL -> outputFileType -> outError -> IO (Id AVAssetWriter)
assetWriterWithURL_fileType_error outputURL outputFileType outError =
  do
    cls' <- getRequiredClass "AVAssetWriter"
    withObjCPtr outputURL $ \raw_outputURL ->
      withObjCPtr outputFileType $ \raw_outputFileType ->
        withObjCPtr outError $ \raw_outError ->
          sendClassMsg cls' (mkSelector "assetWriterWithURL:fileType:error:") (retPtr retVoid) [argPtr (castPtr raw_outputURL :: Ptr ()), argPtr (castPtr raw_outputFileType :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | initWithURL:fileType:error:
--
-- Creates an instance of AVAssetWriter configured to write to a file in a specified container format.
--
-- @URL@ — The location of the file to be written. The URL must be a file URL.
--
-- @fileType@ — A UTI indicating the format of the file to be written.
--
-- @outError@ — On return, if initialization of the AVAssetWriter fails, points to an NSError describing the nature of the failure.
--
-- Returns: An instance of AVAssetWriter.
--
-- Writing will fail if a file already exists at the specified URL.
--
-- This method throws an exception if the output file type is not declared in AVMediaFormat.h.
--
-- ObjC selector: @- initWithURL:fileType:error:@
initWithURL_fileType_error :: (IsAVAssetWriter avAssetWriter, IsNSURL outputURL, IsNSString outputFileType, IsNSError outError) => avAssetWriter -> outputURL -> outputFileType -> outError -> IO (Id AVAssetWriter)
initWithURL_fileType_error avAssetWriter  outputURL outputFileType outError =
withObjCPtr outputURL $ \raw_outputURL ->
  withObjCPtr outputFileType $ \raw_outputFileType ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg avAssetWriter (mkSelector "initWithURL:fileType:error:") (retPtr retVoid) [argPtr (castPtr raw_outputURL :: Ptr ()), argPtr (castPtr raw_outputFileType :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | initWithContentType:
--
-- Creates an instance of AVAssetWriter configured to output segment data in a specified container format.
--
-- @outputContentType@ — A UTType indicating the format of the segment data to be output.
--
-- Returns: An instance of AVAssetWriter.
--
-- Clients that want to receive segment data through the -assetWriter:didOutputSegmentData:segmentType:segmentReport: or -assetWriter:didOutputSegmentData:segmentType: delegate method should use this initializer instead of -initWithURL:fileType:error:.
--
-- Clients may use +typeWithIdentifier: with a UTI to create an instance of UTType. See <UniformTypeIdentifiers/UTType.h>.
--
-- This method throws an exception if the output content type UTI for container format is not declared in AVMediaFormat.h.
--
-- ObjC selector: @- initWithContentType:@
initWithContentType :: (IsAVAssetWriter avAssetWriter, IsUTType outputContentType) => avAssetWriter -> outputContentType -> IO (Id AVAssetWriter)
initWithContentType avAssetWriter  outputContentType =
withObjCPtr outputContentType $ \raw_outputContentType ->
    sendMsg avAssetWriter (mkSelector "initWithContentType:") (retPtr retVoid) [argPtr (castPtr raw_outputContentType :: Ptr ())] >>= ownedObject . castPtr

-- | canApplyOutputSettings:forMediaType:
--
-- Tests whether output settings for a specific media type are supported by the receiver's file format.
--
-- @outputSettings@ — The output settings that are to be tested.
--
-- @mediaType@ — The media type for which the output settings are to be tested. Media types are defined in AVMediaFormat.h.
--
-- Returns: A BOOL indicating whether the given output settings can be used for the given media type.
--
-- This method determines whether the output settings for the specified media type can be used with the receiver's file format. For example, video compression settings that specify H.264 compression are not compatible with file formats that cannot contain H.264-compressed video.
--
-- Attempting to add an input with output settings and a media type for which this method returns NO will cause an exception to be thrown.
--
-- ObjC selector: @- canApplyOutputSettings:forMediaType:@
canApplyOutputSettings_forMediaType :: (IsAVAssetWriter avAssetWriter, IsNSDictionary outputSettings, IsNSString mediaType) => avAssetWriter -> outputSettings -> mediaType -> IO Bool
canApplyOutputSettings_forMediaType avAssetWriter  outputSettings mediaType =
withObjCPtr outputSettings $ \raw_outputSettings ->
  withObjCPtr mediaType $ \raw_mediaType ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetWriter (mkSelector "canApplyOutputSettings:forMediaType:") retCULong [argPtr (castPtr raw_outputSettings :: Ptr ()), argPtr (castPtr raw_mediaType :: Ptr ())]

-- | canAddInput:
--
-- Tests whether an input can be added to the receiver.
--
-- @input@ — The AVAssetWriterInput object to be tested.
--
-- Returns: A BOOL indicating whether the input can be added to the receiver.
--
-- An input that accepts media data of a type that is not compatible with the receiver, or with output settings that are not compatible with the receiver, cannot be added.
--
-- ObjC selector: @- canAddInput:@
canAddInput :: (IsAVAssetWriter avAssetWriter, IsAVAssetWriterInput input) => avAssetWriter -> input -> IO Bool
canAddInput avAssetWriter  input =
withObjCPtr input $ \raw_input ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetWriter (mkSelector "canAddInput:") retCULong [argPtr (castPtr raw_input :: Ptr ())]

-- | addInput:
--
-- Adds an input to the receiver.
--
-- @input@ — The AVAssetWriterInput object to be added.
--
-- Inputs are created with a media type and output settings. These both must be compatible with the receiver.
--
-- Inputs cannot be added after writing has started.
--
-- This method throws an exception if any of the following conditions are satisfied:		- the input's media type is not allowed for this asset writer		- writing uncompressed video in a specific format		- passthrough* to files (other than AVFileTypeQuickTimeMovie) is missing a format hint in the AVAssetWriterInput initializer		- passthrough* is not supported for this media/file type combination (for example, AVFileTypeWAVE only supports AVMediaTypeAudio)
--
-- Passthrough is indicated when the input's output settings are nil.
--
-- ObjC selector: @- addInput:@
addInput :: (IsAVAssetWriter avAssetWriter, IsAVAssetWriterInput input) => avAssetWriter -> input -> IO ()
addInput avAssetWriter  input =
withObjCPtr input $ \raw_input ->
    sendMsg avAssetWriter (mkSelector "addInput:") retVoid [argPtr (castPtr raw_input :: Ptr ())]

-- | startWriting
--
-- Prepares the receiver for accepting input and for writing its output to its output file.
--
-- Returns: A BOOL indicating whether writing successfully started.
--
-- This method must be called after all inputs have been added and other configuration properties have been set in order to tell the receiver to prepare for writing. After this method is called, clients can start writing sessions using startSessionAtSourceTime: and can write media samples using the methods provided by each of the receiver's inputs.
--
-- If writing cannot be started, this method returns NO. Clients can check the values of the status and error properties for more information on why writing could not be started.
--
-- On iOS, if the status of an AVAssetWriter is AVAssetWriterStatusWriting when the client app goes into the background, its status will change to AVAssetWriterStatusFailed and appending to any of its inputs will fail.  You may want to use -[UIApplication beginBackgroundTaskWithExpirationHandler:] to avoid being interrupted in the middle of a writing session and to finish writing the data that has already been appended.  For more information about executing code in the background, see the iOS Application Programming Guide.
--
-- ObjC selector: @- startWriting@
startWriting :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO Bool
startWriting avAssetWriter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetWriter (mkSelector "startWriting") retCULong []

-- | cancelWriting
--
-- Cancels the creation of the output file.
--
-- If the status of the receiver is "failed" or "completed," -cancelWriting is a no-op.  Otherwise, this method will block until writing is canceled.
--
-- If an output file was created by the receiver during the writing process, -cancelWriting will delete the file.
--
-- This method should not be called concurrently with -[AVAssetWriterInput appendSampleBuffer:] or -[AVAssetWriterInputPixelBufferAdaptor appendPixelBuffer:withPresentationTime:].
--
-- ObjC selector: @- cancelWriting@
cancelWriting :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO ()
cancelWriting avAssetWriter  =
  sendMsg avAssetWriter (mkSelector "cancelWriting") retVoid []

-- | finishWriting
--
-- Completes the writing of the output file.
--
-- Returns: A BOOL indicating whether writing successfully finished.
--
-- This method is deprecated.  Use finishWritingWithCompletionHandler: instead.
--
-- This method will block until writing is finished. When this method returns successfully, the file being written by the receiver is complete and ready to use.
--
-- Because this method is blocking and can take a long time to execute (especially with shouldOptimizeForNetworkUse set to YES), it should not be called from the main thread.  Doing so can cause the finishWriting operation to fail.
--
-- If writing cannot be finished, this method returns NO. Clients can check the values of the status and error properties for more information on why writing could not be finished.
--
-- This method should not be called concurrently with -[AVAssetWriterInput appendSampleBuffer:] or -[AVAssetWriterInputPixelBufferAdaptor appendPixelBuffer:withPresentationTime:].
--
-- ObjC selector: @- finishWriting@
finishWriting :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO Bool
finishWriting avAssetWriter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetWriter (mkSelector "finishWriting") retCULong []

-- | finishWritingWithCompletionHandler:
--
-- Marks all unfinished inputs as finished and completes the writing of the output file.
--
-- This method returns immediately and causes its work to be performed asynchronously.
--
-- When the writing of the output file is finished, or if a failure or a cancellation occurs in the meantime, the specified handler will be invoked to indicate completion of the operation. To determine whether the operation succeeded, your handler can check the value of AVAssetWriter.status. If the status is AVAssetWriterStatusFailed, AVAsset.error will contain an instance of NSError that describes the failure.
--
-- To guarantee that all sample buffers are successfully written, ensure all calls to -[AVAssetWriterInput appendSampleBuffer:] or -[AVAssetWriterInputPixelBufferAdaptor appendPixelBuffer:withPresentationTime:] have returned before invoking this method.
--
-- ObjC selector: @- finishWritingWithCompletionHandler:@
finishWritingWithCompletionHandler :: IsAVAssetWriter avAssetWriter => avAssetWriter -> Ptr () -> IO ()
finishWritingWithCompletionHandler avAssetWriter  handler =
  sendMsg avAssetWriter (mkSelector "finishWritingWithCompletionHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | flushSegment
--
-- Closes the current segment and outputs it to the -assetWriter:didOutputSegmentData:segmentType:segmentReport: or -assetWriter:didOutputSegmentData:segmentType: delegate method.
--
-- This method throws an exception if the delegate method to output segment data is not implemented, or if the value of the preferredOutputSegmentInterval property is not kCMTimeIndefinite.
--
-- ObjC selector: @- flushSegment@
flushSegment :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO ()
flushSegment avAssetWriter  =
  sendMsg avAssetWriter (mkSelector "flushSegment") retVoid []

-- | canAddInputGroup:
--
-- Tests whether an input group can be added to the receiver.
--
-- @inputGroup@ — The AVAssetWriterInputGroup object to be tested.
--
-- Returns: A BOOL indicating whether the input group can be added to the receiver.
--
-- If outputFileType specifies a container format that does not support mutually exclusive relationships among tracks, or if the specified instance of AVAssetWriterInputGroup contains inputs with media types that cannot be related, the group cannot be added to the AVAssetWriter.
--
-- This method throws an exception if any of the following conditions are satisfied:		- this writer's output file type does not support mutually exclusive relationships among tracks (allowed types are AVFileTypeQuickTimeMovie, AVFileTypeAppleM4A, AVFileTypeAppleM4V, AVFileType3GPP, AVFileTypeMPEG4)		- any AVAssetWriterInput in the input group is also present in an input group already added
--
-- ObjC selector: @- canAddInputGroup:@
canAddInputGroup :: (IsAVAssetWriter avAssetWriter, IsAVAssetWriterInputGroup inputGroup) => avAssetWriter -> inputGroup -> IO Bool
canAddInputGroup avAssetWriter  inputGroup =
withObjCPtr inputGroup $ \raw_inputGroup ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetWriter (mkSelector "canAddInputGroup:") retCULong [argPtr (castPtr raw_inputGroup :: Ptr ())]

-- | @- addInputGroup:@
addInputGroup :: (IsAVAssetWriter avAssetWriter, IsAVAssetWriterInputGroup inputGroup) => avAssetWriter -> inputGroup -> IO ()
addInputGroup avAssetWriter  inputGroup =
withObjCPtr inputGroup $ \raw_inputGroup ->
    sendMsg avAssetWriter (mkSelector "addInputGroup:") retVoid [argPtr (castPtr raw_inputGroup :: Ptr ())]

-- | outputURL
--
-- The location of the file for which the instance of AVAssetWriter was initialized for writing.
--
-- You may use [[UTType typeWithIdentifier:outputFileType] preferredFilenameExtension] to obtain an appropriate path extension for the outputFileType you have specified. For more information, see <UniformTypeIdentifiers/UTType.h>.
--
-- ObjC selector: @- outputURL@
outputURL :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSURL)
outputURL avAssetWriter  =
  sendMsg avAssetWriter (mkSelector "outputURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | outputFileType
--
-- The UTI of the file format of the file for which the instance of AVAssetWriter was initialized for writing.
--
-- ObjC selector: @- outputFileType@
outputFileType :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSString)
outputFileType avAssetWriter  =
  sendMsg avAssetWriter (mkSelector "outputFileType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | availableMediaTypes
--
-- The media types for which inputs can be added to the receiver.
--
-- Some media types may not be accepted within the file format with which an AVAssetWriter was initialized.
--
-- ObjC selector: @- availableMediaTypes@
availableMediaTypes :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSArray)
availableMediaTypes avAssetWriter  =
  sendMsg avAssetWriter (mkSelector "availableMediaTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | status
--
-- The status of writing samples to the receiver's output file.
--
-- The value of this property is an AVAssetWriterStatus that indicates whether writing is in progress, has completed successfully, has been canceled, or has failed. Clients of AVAssetWriterInput objects should check the value of this property after appending samples fails to determine why no more samples could be written. This property is thread safe.
--
-- ObjC selector: @- status@
status :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO AVAssetWriterStatus
status avAssetWriter  =
  fmap (coerce :: CLong -> AVAssetWriterStatus) $ sendMsg avAssetWriter (mkSelector "status") retCLong []

-- | error
--
-- If the receiver's status is AVAssetWriterStatusFailed, this describes the error that caused the failure.
--
-- The value of this property is an NSError that describes what caused the receiver to no longer be able to write to its output file. If the receiver's status is not AVAssetWriterStatusFailed, the value of this property is nil. This property is thread safe.
--
-- ObjC selector: @- error@
error_ :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSError)
error_ avAssetWriter  =
  sendMsg avAssetWriter (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metadata
--
-- A collection of metadata to be written to the receiver's output file.
--
-- The value of this property is an array of AVMetadataItem objects representing the collection of top-level metadata to be written in the output file.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- metadata@
metadata :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSArray)
metadata avAssetWriter  =
  sendMsg avAssetWriter (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metadata
--
-- A collection of metadata to be written to the receiver's output file.
--
-- The value of this property is an array of AVMetadataItem objects representing the collection of top-level metadata to be written in the output file.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- setMetadata:@
setMetadata :: (IsAVAssetWriter avAssetWriter, IsNSArray value) => avAssetWriter -> value -> IO ()
setMetadata avAssetWriter  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAssetWriter (mkSelector "setMetadata:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | shouldOptimizeForNetworkUse
--
-- Specifies whether the output file should be written in way that makes it more suitable for playback over a network
--
-- When the value of this property is YES, the output file will be written in such a way that playback can start after only a small amount of the file is downloaded.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- shouldOptimizeForNetworkUse@
shouldOptimizeForNetworkUse :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO Bool
shouldOptimizeForNetworkUse avAssetWriter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetWriter (mkSelector "shouldOptimizeForNetworkUse") retCULong []

-- | shouldOptimizeForNetworkUse
--
-- Specifies whether the output file should be written in way that makes it more suitable for playback over a network
--
-- When the value of this property is YES, the output file will be written in such a way that playback can start after only a small amount of the file is downloaded.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- setShouldOptimizeForNetworkUse:@
setShouldOptimizeForNetworkUse :: IsAVAssetWriter avAssetWriter => avAssetWriter -> Bool -> IO ()
setShouldOptimizeForNetworkUse avAssetWriter  value =
  sendMsg avAssetWriter (mkSelector "setShouldOptimizeForNetworkUse:") retVoid [argCULong (if value then 1 else 0)]

-- | inputs
--
-- The inputs from which the asset writer receives media data.
--
-- The value of this property is an NSArray containing concrete instances of AVAssetWriterInput. Inputs can be added to the receiver using the addInput: method.
--
-- ObjC selector: @- inputs@
inputs :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSArray)
inputs avAssetWriter  =
  sendMsg avAssetWriter (mkSelector "inputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | outputFileTypeProfile
--
-- Specifies a file type profile for the specified file type.
--
-- The default value is nil, which means that the receiver will choose an appropriate default profile based on the specified file type.
--
-- Clients that want to receive segment data that is suitable for streaming through the -assetWriter:didOutputSegmentData:segmentType:segmentReport: or -assetWriter:didOutputSegmentData:segmentType: delegate method should set AVFileTypeProfileMPEG4AppleHLS, or AVFileTypeProfileMPEG4CMAFCompliant to require output that is specifically compliant with CMAF format, with AVFileTypeMPEG4 file type.
--
-- File type profiles are declared in AVMediaFormat.h.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- outputFileTypeProfile@
outputFileTypeProfile :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSString)
outputFileTypeProfile avAssetWriter  =
  sendMsg avAssetWriter (mkSelector "outputFileTypeProfile") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | outputFileTypeProfile
--
-- Specifies a file type profile for the specified file type.
--
-- The default value is nil, which means that the receiver will choose an appropriate default profile based on the specified file type.
--
-- Clients that want to receive segment data that is suitable for streaming through the -assetWriter:didOutputSegmentData:segmentType:segmentReport: or -assetWriter:didOutputSegmentData:segmentType: delegate method should set AVFileTypeProfileMPEG4AppleHLS, or AVFileTypeProfileMPEG4CMAFCompliant to require output that is specifically compliant with CMAF format, with AVFileTypeMPEG4 file type.
--
-- File type profiles are declared in AVMediaFormat.h.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- setOutputFileTypeProfile:@
setOutputFileTypeProfile :: (IsAVAssetWriter avAssetWriter, IsNSString value) => avAssetWriter -> value -> IO ()
setOutputFileTypeProfile avAssetWriter  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAssetWriter (mkSelector "setOutputFileTypeProfile:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | initialMovieFragmentSequenceNumber
--
-- For file types that support movie fragments, specifies the initial movie fragment sequence number.
--
-- The value must be equal to or greater than 1.
--
-- The default value is 1.
--
-- Note that if you combine movie fragments produced by an instance of AVAssetWriter with additional movie fragments, produced either by a different instance of AVAssetWriter or by some other means, it is necessary to ensure that movie fragment sequence numbers increase monotonically across the entire combined collection, in temporal order.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- initialMovieFragmentSequenceNumber@
initialMovieFragmentSequenceNumber :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO CLong
initialMovieFragmentSequenceNumber avAssetWriter  =
  sendMsg avAssetWriter (mkSelector "initialMovieFragmentSequenceNumber") retCLong []

-- | initialMovieFragmentSequenceNumber
--
-- For file types that support movie fragments, specifies the initial movie fragment sequence number.
--
-- The value must be equal to or greater than 1.
--
-- The default value is 1.
--
-- Note that if you combine movie fragments produced by an instance of AVAssetWriter with additional movie fragments, produced either by a different instance of AVAssetWriter or by some other means, it is necessary to ensure that movie fragment sequence numbers increase monotonically across the entire combined collection, in temporal order.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- setInitialMovieFragmentSequenceNumber:@
setInitialMovieFragmentSequenceNumber :: IsAVAssetWriter avAssetWriter => avAssetWriter -> CLong -> IO ()
setInitialMovieFragmentSequenceNumber avAssetWriter  value =
  sendMsg avAssetWriter (mkSelector "setInitialMovieFragmentSequenceNumber:") retVoid [argCLong (fromIntegral value)]

-- | producesCombinableFragments
--
-- For file types that support fragmented MPEG-4, specifies whether the movie fragments should be produced in way that makes them suitable for combining with movie fragments produced by one or more other instances of AVAssetWriter into a single fragment stream of uniform encoding.
--
-- The default value is NO.
--
-- When multiple instances of AVAssetWriter are used to produce distinct streams that complement each other, for example to create HLS encoding or bitrate variants, it’s not necessary to set this property to YES.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- producesCombinableFragments@
producesCombinableFragments :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO Bool
producesCombinableFragments avAssetWriter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetWriter (mkSelector "producesCombinableFragments") retCULong []

-- | producesCombinableFragments
--
-- For file types that support fragmented MPEG-4, specifies whether the movie fragments should be produced in way that makes them suitable for combining with movie fragments produced by one or more other instances of AVAssetWriter into a single fragment stream of uniform encoding.
--
-- The default value is NO.
--
-- When multiple instances of AVAssetWriter are used to produce distinct streams that complement each other, for example to create HLS encoding or bitrate variants, it’s not necessary to set this property to YES.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- setProducesCombinableFragments:@
setProducesCombinableFragments :: IsAVAssetWriter avAssetWriter => avAssetWriter -> Bool -> IO ()
setProducesCombinableFragments avAssetWriter  value =
  sendMsg avAssetWriter (mkSelector "setProducesCombinableFragments:") retVoid [argCULong (if value then 1 else 0)]

-- | movieTimeScale
--
-- For file types that contain a 'moov' atom, such as QuickTime Movie files, specifies the asset-level time scale to be used.
--
-- The default value is 0, which indicates that the receiver should choose a convenient value, if applicable.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- movieTimeScale@
movieTimeScale :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO CInt
movieTimeScale avAssetWriter  =
  sendMsg avAssetWriter (mkSelector "movieTimeScale") retCInt []

-- | movieTimeScale
--
-- For file types that contain a 'moov' atom, such as QuickTime Movie files, specifies the asset-level time scale to be used.
--
-- The default value is 0, which indicates that the receiver should choose a convenient value, if applicable.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- setMovieTimeScale:@
setMovieTimeScale :: IsAVAssetWriter avAssetWriter => avAssetWriter -> CInt -> IO ()
setMovieTimeScale avAssetWriter  value =
  sendMsg avAssetWriter (mkSelector "setMovieTimeScale:") retVoid [argCInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @assetWriterWithURL:fileType:error:@
assetWriterWithURL_fileType_errorSelector :: Selector
assetWriterWithURL_fileType_errorSelector = mkSelector "assetWriterWithURL:fileType:error:"

-- | @Selector@ for @initWithURL:fileType:error:@
initWithURL_fileType_errorSelector :: Selector
initWithURL_fileType_errorSelector = mkSelector "initWithURL:fileType:error:"

-- | @Selector@ for @initWithContentType:@
initWithContentTypeSelector :: Selector
initWithContentTypeSelector = mkSelector "initWithContentType:"

-- | @Selector@ for @canApplyOutputSettings:forMediaType:@
canApplyOutputSettings_forMediaTypeSelector :: Selector
canApplyOutputSettings_forMediaTypeSelector = mkSelector "canApplyOutputSettings:forMediaType:"

-- | @Selector@ for @canAddInput:@
canAddInputSelector :: Selector
canAddInputSelector = mkSelector "canAddInput:"

-- | @Selector@ for @addInput:@
addInputSelector :: Selector
addInputSelector = mkSelector "addInput:"

-- | @Selector@ for @startWriting@
startWritingSelector :: Selector
startWritingSelector = mkSelector "startWriting"

-- | @Selector@ for @cancelWriting@
cancelWritingSelector :: Selector
cancelWritingSelector = mkSelector "cancelWriting"

-- | @Selector@ for @finishWriting@
finishWritingSelector :: Selector
finishWritingSelector = mkSelector "finishWriting"

-- | @Selector@ for @finishWritingWithCompletionHandler:@
finishWritingWithCompletionHandlerSelector :: Selector
finishWritingWithCompletionHandlerSelector = mkSelector "finishWritingWithCompletionHandler:"

-- | @Selector@ for @flushSegment@
flushSegmentSelector :: Selector
flushSegmentSelector = mkSelector "flushSegment"

-- | @Selector@ for @canAddInputGroup:@
canAddInputGroupSelector :: Selector
canAddInputGroupSelector = mkSelector "canAddInputGroup:"

-- | @Selector@ for @addInputGroup:@
addInputGroupSelector :: Selector
addInputGroupSelector = mkSelector "addInputGroup:"

-- | @Selector@ for @outputURL@
outputURLSelector :: Selector
outputURLSelector = mkSelector "outputURL"

-- | @Selector@ for @outputFileType@
outputFileTypeSelector :: Selector
outputFileTypeSelector = mkSelector "outputFileType"

-- | @Selector@ for @availableMediaTypes@
availableMediaTypesSelector :: Selector
availableMediaTypesSelector = mkSelector "availableMediaTypes"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector
setMetadataSelector = mkSelector "setMetadata:"

-- | @Selector@ for @shouldOptimizeForNetworkUse@
shouldOptimizeForNetworkUseSelector :: Selector
shouldOptimizeForNetworkUseSelector = mkSelector "shouldOptimizeForNetworkUse"

-- | @Selector@ for @setShouldOptimizeForNetworkUse:@
setShouldOptimizeForNetworkUseSelector :: Selector
setShouldOptimizeForNetworkUseSelector = mkSelector "setShouldOptimizeForNetworkUse:"

-- | @Selector@ for @inputs@
inputsSelector :: Selector
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @outputFileTypeProfile@
outputFileTypeProfileSelector :: Selector
outputFileTypeProfileSelector = mkSelector "outputFileTypeProfile"

-- | @Selector@ for @setOutputFileTypeProfile:@
setOutputFileTypeProfileSelector :: Selector
setOutputFileTypeProfileSelector = mkSelector "setOutputFileTypeProfile:"

-- | @Selector@ for @initialMovieFragmentSequenceNumber@
initialMovieFragmentSequenceNumberSelector :: Selector
initialMovieFragmentSequenceNumberSelector = mkSelector "initialMovieFragmentSequenceNumber"

-- | @Selector@ for @setInitialMovieFragmentSequenceNumber:@
setInitialMovieFragmentSequenceNumberSelector :: Selector
setInitialMovieFragmentSequenceNumberSelector = mkSelector "setInitialMovieFragmentSequenceNumber:"

-- | @Selector@ for @producesCombinableFragments@
producesCombinableFragmentsSelector :: Selector
producesCombinableFragmentsSelector = mkSelector "producesCombinableFragments"

-- | @Selector@ for @setProducesCombinableFragments:@
setProducesCombinableFragmentsSelector :: Selector
setProducesCombinableFragmentsSelector = mkSelector "setProducesCombinableFragments:"

-- | @Selector@ for @movieTimeScale@
movieTimeScaleSelector :: Selector
movieTimeScaleSelector = mkSelector "movieTimeScale"

-- | @Selector@ for @setMovieTimeScale:@
setMovieTimeScaleSelector :: Selector
setMovieTimeScaleSelector = mkSelector "setMovieTimeScale:"

