{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , directoryForTemporaryFiles
  , setDirectoryForTemporaryFiles
  , inputs
  , outputFileTypeProfile
  , setOutputFileTypeProfile
  , delegate
  , setDelegate
  , inputGroups
  , initialMovieFragmentSequenceNumber
  , setInitialMovieFragmentSequenceNumber
  , producesCombinableFragments
  , setProducesCombinableFragments
  , movieTimeScale
  , setMovieTimeScale
  , addInputGroupSelector
  , addInputSelector
  , assetWriterWithURL_fileType_errorSelector
  , availableMediaTypesSelector
  , canAddInputGroupSelector
  , canAddInputSelector
  , canApplyOutputSettings_forMediaTypeSelector
  , cancelWritingSelector
  , delegateSelector
  , directoryForTemporaryFilesSelector
  , errorSelector
  , finishWritingSelector
  , finishWritingWithCompletionHandlerSelector
  , flushSegmentSelector
  , initSelector
  , initWithContentTypeSelector
  , initWithURL_fileType_errorSelector
  , initialMovieFragmentSequenceNumberSelector
  , inputGroupsSelector
  , inputsSelector
  , metadataSelector
  , movieTimeScaleSelector
  , newSelector
  , outputFileTypeProfileSelector
  , outputFileTypeSelector
  , outputURLSelector
  , producesCombinableFragmentsSelector
  , setDelegateSelector
  , setDirectoryForTemporaryFilesSelector
  , setInitialMovieFragmentSequenceNumberSelector
  , setMetadataSelector
  , setMovieTimeScaleSelector
  , setOutputFileTypeProfileSelector
  , setProducesCombinableFragmentsSelector
  , setShouldOptimizeForNetworkUseSelector
  , shouldOptimizeForNetworkUseSelector
  , startWritingSelector
  , statusSelector

  -- * Enum types
  , AVAssetWriterStatus(AVAssetWriterStatus)
  , pattern AVAssetWriterStatusUnknown
  , pattern AVAssetWriterStatusWriting
  , pattern AVAssetWriterStatusCompleted
  , pattern AVAssetWriterStatusFailed
  , pattern AVAssetWriterStatusCancelled

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
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- init@
init_ :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id AVAssetWriter)
init_ avAssetWriter =
  sendOwnedMessage avAssetWriter initSelector

-- | @+ new@
new :: IO (Id AVAssetWriter)
new  =
  do
    cls' <- getRequiredClass "AVAssetWriter"
    sendOwnedClassMessage cls' newSelector

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
    sendClassMessage cls' assetWriterWithURL_fileType_errorSelector (toNSURL outputURL) (toNSString outputFileType) (toNSError outError)

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
initWithURL_fileType_error avAssetWriter outputURL outputFileType outError =
  sendOwnedMessage avAssetWriter initWithURL_fileType_errorSelector (toNSURL outputURL) (toNSString outputFileType) (toNSError outError)

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
initWithContentType avAssetWriter outputContentType =
  sendOwnedMessage avAssetWriter initWithContentTypeSelector (toUTType outputContentType)

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
canApplyOutputSettings_forMediaType avAssetWriter outputSettings mediaType =
  sendMessage avAssetWriter canApplyOutputSettings_forMediaTypeSelector (toNSDictionary outputSettings) (toNSString mediaType)

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
canAddInput avAssetWriter input =
  sendMessage avAssetWriter canAddInputSelector (toAVAssetWriterInput input)

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
addInput avAssetWriter input =
  sendMessage avAssetWriter addInputSelector (toAVAssetWriterInput input)

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
startWriting avAssetWriter =
  sendMessage avAssetWriter startWritingSelector

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
cancelWriting avAssetWriter =
  sendMessage avAssetWriter cancelWritingSelector

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
finishWriting avAssetWriter =
  sendMessage avAssetWriter finishWritingSelector

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
finishWritingWithCompletionHandler avAssetWriter handler =
  sendMessage avAssetWriter finishWritingWithCompletionHandlerSelector handler

-- | flushSegment
--
-- Closes the current segment and outputs it to the -assetWriter:didOutputSegmentData:segmentType:segmentReport: or -assetWriter:didOutputSegmentData:segmentType: delegate method.
--
-- This method throws an exception if the delegate method to output segment data is not implemented, or if the value of the preferredOutputSegmentInterval property is not kCMTimeIndefinite.
--
-- ObjC selector: @- flushSegment@
flushSegment :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO ()
flushSegment avAssetWriter =
  sendMessage avAssetWriter flushSegmentSelector

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
canAddInputGroup avAssetWriter inputGroup =
  sendMessage avAssetWriter canAddInputGroupSelector (toAVAssetWriterInputGroup inputGroup)

-- | @- addInputGroup:@
addInputGroup :: (IsAVAssetWriter avAssetWriter, IsAVAssetWriterInputGroup inputGroup) => avAssetWriter -> inputGroup -> IO ()
addInputGroup avAssetWriter inputGroup =
  sendMessage avAssetWriter addInputGroupSelector (toAVAssetWriterInputGroup inputGroup)

-- | outputURL
--
-- The location of the file for which the instance of AVAssetWriter was initialized for writing.
--
-- You may use [[UTType typeWithIdentifier:outputFileType] preferredFilenameExtension] to obtain an appropriate path extension for the outputFileType you have specified. For more information, see <UniformTypeIdentifiers/UTType.h>.
--
-- ObjC selector: @- outputURL@
outputURL :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSURL)
outputURL avAssetWriter =
  sendMessage avAssetWriter outputURLSelector

-- | outputFileType
--
-- The UTI of the file format of the file for which the instance of AVAssetWriter was initialized for writing.
--
-- ObjC selector: @- outputFileType@
outputFileType :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSString)
outputFileType avAssetWriter =
  sendMessage avAssetWriter outputFileTypeSelector

-- | availableMediaTypes
--
-- The media types for which inputs can be added to the receiver.
--
-- Some media types may not be accepted within the file format with which an AVAssetWriter was initialized.
--
-- ObjC selector: @- availableMediaTypes@
availableMediaTypes :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSArray)
availableMediaTypes avAssetWriter =
  sendMessage avAssetWriter availableMediaTypesSelector

-- | status
--
-- The status of writing samples to the receiver's output file.
--
-- The value of this property is an AVAssetWriterStatus that indicates whether writing is in progress, has completed successfully, has been canceled, or has failed. Clients of AVAssetWriterInput objects should check the value of this property after appending samples fails to determine why no more samples could be written. This property is thread safe.
--
-- ObjC selector: @- status@
status :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO AVAssetWriterStatus
status avAssetWriter =
  sendMessage avAssetWriter statusSelector

-- | error
--
-- If the receiver's status is AVAssetWriterStatusFailed, this describes the error that caused the failure.
--
-- The value of this property is an NSError that describes what caused the receiver to no longer be able to write to its output file. If the receiver's status is not AVAssetWriterStatusFailed, the value of this property is nil. This property is thread safe.
--
-- ObjC selector: @- error@
error_ :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSError)
error_ avAssetWriter =
  sendMessage avAssetWriter errorSelector

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
metadata avAssetWriter =
  sendMessage avAssetWriter metadataSelector

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
setMetadata avAssetWriter value =
  sendMessage avAssetWriter setMetadataSelector (toNSArray value)

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
shouldOptimizeForNetworkUse avAssetWriter =
  sendMessage avAssetWriter shouldOptimizeForNetworkUseSelector

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
setShouldOptimizeForNetworkUse avAssetWriter value =
  sendMessage avAssetWriter setShouldOptimizeForNetworkUseSelector value

-- | directoryForTemporaryFiles
--
-- Specifies a directory that is suitable for containing temporary files generated during the process of writing an asset.
--
-- AVAssetWriter may need to write temporary files when configured in certain ways, such as when performsMultiPassEncodingIfSupported is set to YES on one or more of its inputs.  This property can be used to control where in the filesystem those temporary files are created.  All temporary files will be deleted when asset writing is completed, is canceled, or fails.
--
-- When the value of this property is nil, the asset writer will choose a suitable location when writing temporary files.  The default value is nil.
--
-- This property cannot be set after writing has started.  The asset writer will fail if a file cannot be created in this directory (for example, due to insufficient permissions).
--
-- ObjC selector: @- directoryForTemporaryFiles@
directoryForTemporaryFiles :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSURL)
directoryForTemporaryFiles avAssetWriter =
  sendMessage avAssetWriter directoryForTemporaryFilesSelector

-- | directoryForTemporaryFiles
--
-- Specifies a directory that is suitable for containing temporary files generated during the process of writing an asset.
--
-- AVAssetWriter may need to write temporary files when configured in certain ways, such as when performsMultiPassEncodingIfSupported is set to YES on one or more of its inputs.  This property can be used to control where in the filesystem those temporary files are created.  All temporary files will be deleted when asset writing is completed, is canceled, or fails.
--
-- When the value of this property is nil, the asset writer will choose a suitable location when writing temporary files.  The default value is nil.
--
-- This property cannot be set after writing has started.  The asset writer will fail if a file cannot be created in this directory (for example, due to insufficient permissions).
--
-- ObjC selector: @- setDirectoryForTemporaryFiles:@
setDirectoryForTemporaryFiles :: (IsAVAssetWriter avAssetWriter, IsNSURL value) => avAssetWriter -> value -> IO ()
setDirectoryForTemporaryFiles avAssetWriter value =
  sendMessage avAssetWriter setDirectoryForTemporaryFilesSelector (toNSURL value)

-- | inputs
--
-- The inputs from which the asset writer receives media data.
--
-- The value of this property is an NSArray containing concrete instances of AVAssetWriterInput. Inputs can be added to the receiver using the addInput: method.
--
-- ObjC selector: @- inputs@
inputs :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSArray)
inputs avAssetWriter =
  sendMessage avAssetWriter inputsSelector

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
outputFileTypeProfile avAssetWriter =
  sendMessage avAssetWriter outputFileTypeProfileSelector

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
setOutputFileTypeProfile avAssetWriter value =
  sendMessage avAssetWriter setOutputFileTypeProfileSelector (toNSString value)

-- | delegate
--
-- An object that implements one or more of the methods in the AVAssetWriterDelegate protocol.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- delegate@
delegate :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO RawId
delegate avAssetWriter =
  sendMessage avAssetWriter delegateSelector

-- | delegate
--
-- An object that implements one or more of the methods in the AVAssetWriterDelegate protocol.
--
-- This property cannot be set after writing has started.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsAVAssetWriter avAssetWriter => avAssetWriter -> RawId -> IO ()
setDelegate avAssetWriter value =
  sendMessage avAssetWriter setDelegateSelector value

-- | inputGroups
--
-- The instances of AVAssetWriterInputGroup that have been added to the AVAssetWriter.
--
-- The value of this property is an NSArray containing concrete instances of AVAssetWriterInputGroup.  Input groups can be added to the receiver using the addInputGroup: method.
--
-- ObjC selector: @- inputGroups@
inputGroups :: IsAVAssetWriter avAssetWriter => avAssetWriter -> IO (Id NSArray)
inputGroups avAssetWriter =
  sendMessage avAssetWriter inputGroupsSelector

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
initialMovieFragmentSequenceNumber avAssetWriter =
  sendOwnedMessage avAssetWriter initialMovieFragmentSequenceNumberSelector

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
setInitialMovieFragmentSequenceNumber avAssetWriter value =
  sendMessage avAssetWriter setInitialMovieFragmentSequenceNumberSelector value

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
producesCombinableFragments avAssetWriter =
  sendMessage avAssetWriter producesCombinableFragmentsSelector

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
setProducesCombinableFragments avAssetWriter value =
  sendMessage avAssetWriter setProducesCombinableFragmentsSelector value

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
movieTimeScale avAssetWriter =
  sendMessage avAssetWriter movieTimeScaleSelector

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
setMovieTimeScale avAssetWriter value =
  sendMessage avAssetWriter setMovieTimeScaleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetWriter)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetWriter)
newSelector = mkSelector "new"

-- | @Selector@ for @assetWriterWithURL:fileType:error:@
assetWriterWithURL_fileType_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSError] (Id AVAssetWriter)
assetWriterWithURL_fileType_errorSelector = mkSelector "assetWriterWithURL:fileType:error:"

-- | @Selector@ for @initWithURL:fileType:error:@
initWithURL_fileType_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSError] (Id AVAssetWriter)
initWithURL_fileType_errorSelector = mkSelector "initWithURL:fileType:error:"

-- | @Selector@ for @initWithContentType:@
initWithContentTypeSelector :: Selector '[Id UTType] (Id AVAssetWriter)
initWithContentTypeSelector = mkSelector "initWithContentType:"

-- | @Selector@ for @canApplyOutputSettings:forMediaType:@
canApplyOutputSettings_forMediaTypeSelector :: Selector '[Id NSDictionary, Id NSString] Bool
canApplyOutputSettings_forMediaTypeSelector = mkSelector "canApplyOutputSettings:forMediaType:"

-- | @Selector@ for @canAddInput:@
canAddInputSelector :: Selector '[Id AVAssetWriterInput] Bool
canAddInputSelector = mkSelector "canAddInput:"

-- | @Selector@ for @addInput:@
addInputSelector :: Selector '[Id AVAssetWriterInput] ()
addInputSelector = mkSelector "addInput:"

-- | @Selector@ for @startWriting@
startWritingSelector :: Selector '[] Bool
startWritingSelector = mkSelector "startWriting"

-- | @Selector@ for @cancelWriting@
cancelWritingSelector :: Selector '[] ()
cancelWritingSelector = mkSelector "cancelWriting"

-- | @Selector@ for @finishWriting@
finishWritingSelector :: Selector '[] Bool
finishWritingSelector = mkSelector "finishWriting"

-- | @Selector@ for @finishWritingWithCompletionHandler:@
finishWritingWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
finishWritingWithCompletionHandlerSelector = mkSelector "finishWritingWithCompletionHandler:"

-- | @Selector@ for @flushSegment@
flushSegmentSelector :: Selector '[] ()
flushSegmentSelector = mkSelector "flushSegment"

-- | @Selector@ for @canAddInputGroup:@
canAddInputGroupSelector :: Selector '[Id AVAssetWriterInputGroup] Bool
canAddInputGroupSelector = mkSelector "canAddInputGroup:"

-- | @Selector@ for @addInputGroup:@
addInputGroupSelector :: Selector '[Id AVAssetWriterInputGroup] ()
addInputGroupSelector = mkSelector "addInputGroup:"

-- | @Selector@ for @outputURL@
outputURLSelector :: Selector '[] (Id NSURL)
outputURLSelector = mkSelector "outputURL"

-- | @Selector@ for @outputFileType@
outputFileTypeSelector :: Selector '[] (Id NSString)
outputFileTypeSelector = mkSelector "outputFileType"

-- | @Selector@ for @availableMediaTypes@
availableMediaTypesSelector :: Selector '[] (Id NSArray)
availableMediaTypesSelector = mkSelector "availableMediaTypes"

-- | @Selector@ for @status@
statusSelector :: Selector '[] AVAssetWriterStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSArray)
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector '[Id NSArray] ()
setMetadataSelector = mkSelector "setMetadata:"

-- | @Selector@ for @shouldOptimizeForNetworkUse@
shouldOptimizeForNetworkUseSelector :: Selector '[] Bool
shouldOptimizeForNetworkUseSelector = mkSelector "shouldOptimizeForNetworkUse"

-- | @Selector@ for @setShouldOptimizeForNetworkUse:@
setShouldOptimizeForNetworkUseSelector :: Selector '[Bool] ()
setShouldOptimizeForNetworkUseSelector = mkSelector "setShouldOptimizeForNetworkUse:"

-- | @Selector@ for @directoryForTemporaryFiles@
directoryForTemporaryFilesSelector :: Selector '[] (Id NSURL)
directoryForTemporaryFilesSelector = mkSelector "directoryForTemporaryFiles"

-- | @Selector@ for @setDirectoryForTemporaryFiles:@
setDirectoryForTemporaryFilesSelector :: Selector '[Id NSURL] ()
setDirectoryForTemporaryFilesSelector = mkSelector "setDirectoryForTemporaryFiles:"

-- | @Selector@ for @inputs@
inputsSelector :: Selector '[] (Id NSArray)
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @outputFileTypeProfile@
outputFileTypeProfileSelector :: Selector '[] (Id NSString)
outputFileTypeProfileSelector = mkSelector "outputFileTypeProfile"

-- | @Selector@ for @setOutputFileTypeProfile:@
setOutputFileTypeProfileSelector :: Selector '[Id NSString] ()
setOutputFileTypeProfileSelector = mkSelector "setOutputFileTypeProfile:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @inputGroups@
inputGroupsSelector :: Selector '[] (Id NSArray)
inputGroupsSelector = mkSelector "inputGroups"

-- | @Selector@ for @initialMovieFragmentSequenceNumber@
initialMovieFragmentSequenceNumberSelector :: Selector '[] CLong
initialMovieFragmentSequenceNumberSelector = mkSelector "initialMovieFragmentSequenceNumber"

-- | @Selector@ for @setInitialMovieFragmentSequenceNumber:@
setInitialMovieFragmentSequenceNumberSelector :: Selector '[CLong] ()
setInitialMovieFragmentSequenceNumberSelector = mkSelector "setInitialMovieFragmentSequenceNumber:"

-- | @Selector@ for @producesCombinableFragments@
producesCombinableFragmentsSelector :: Selector '[] Bool
producesCombinableFragmentsSelector = mkSelector "producesCombinableFragments"

-- | @Selector@ for @setProducesCombinableFragments:@
setProducesCombinableFragmentsSelector :: Selector '[Bool] ()
setProducesCombinableFragmentsSelector = mkSelector "setProducesCombinableFragments:"

-- | @Selector@ for @movieTimeScale@
movieTimeScaleSelector :: Selector '[] CInt
movieTimeScaleSelector = mkSelector "movieTimeScale"

-- | @Selector@ for @setMovieTimeScale:@
setMovieTimeScaleSelector :: Selector '[CInt] ()
setMovieTimeScaleSelector = mkSelector "setMovieTimeScale:"

