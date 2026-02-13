{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetExportSession
--
-- An AVAssetExportSession creates a new timed media resource from the contents of an				existing AVAsset in the form described by a specified export preset.
--
-- Prior to initializing an instance of AVAssetExportSession, you can invoke				+allExportPresets to obtain the complete list of presets available. Use				+exportPresetsCompatibleWithAsset: to obtain a list of presets that are compatible				with a specific AVAsset.
--
-- To configure an export, initialize an AVAssetExportSession with an AVAsset that contains				the source media, an AVAssetExportPreset, the output file type, (a UTI string from				those defined in AVMediaFormat.h) and the output URL.
--
-- After configuration is complete, invoke exportAsynchronouslyWithCompletionHandler:				to start the export process. This method returns immediately; the export is performed				asynchronously. Invoke the -progress method to check on the progress. Note that in				some cases, depending on the capabilities of the device, when multiple exports are				attempted at the same time some may be queued until others have been completed. When				this happens, the status of a queued export will indicate that it's "waiting".
--
-- Whether the export fails, completes, or is cancelled, the completion handler you				supply to -exportAsynchronouslyWithCompletionHandler: will be called. Upon				completion, the status property indicates whether the export has completed				successfully. If it has failed, the value of the error property supplies additional				information about the reason for the failure.
--
-- Generated bindings for @AVAssetExportSession@.
module ObjC.AVFoundation.AVAssetExportSession
  ( AVAssetExportSession
  , IsAVAssetExportSession(..)
  , init_
  , new
  , exportSessionWithAsset_presetName
  , initWithAsset_presetName
  , exportAsynchronouslyWithCompletionHandler
  , cancelExport
  , estimateMaximumDurationWithCompletionHandler
  , estimateOutputFileLengthWithCompletionHandler
  , allExportPresets
  , exportPresetsCompatibleWithAsset
  , determineCompatibilityOfExportPreset_withAsset_outputFileType_completionHandler
  , presetName
  , asset
  , outputFileType
  , setOutputFileType
  , outputURL
  , setOutputURL
  , shouldOptimizeForNetworkUse
  , setShouldOptimizeForNetworkUse
  , allowsParallelizedExport
  , setAllowsParallelizedExport
  , status
  , error_
  , progress
  , canPerformMultiplePassesOverSourceMediaData
  , setCanPerformMultiplePassesOverSourceMediaData
  , directoryForTemporaryFiles
  , setDirectoryForTemporaryFiles
  , audioTimePitchAlgorithm
  , setAudioTimePitchAlgorithm
  , audioMix
  , setAudioMix
  , videoComposition
  , setVideoComposition
  , customVideoCompositor
  , audioTrackGroupHandling
  , setAudioTrackGroupHandling
  , metadata
  , setMetadata
  , metadataItemFilter
  , setMetadataItemFilter
  , estimatedOutputFileLength
  , fileLengthLimit
  , setFileLengthLimit
  , supportedFileTypes
  , allExportPresetsSelector
  , allowsParallelizedExportSelector
  , assetSelector
  , audioMixSelector
  , audioTimePitchAlgorithmSelector
  , audioTrackGroupHandlingSelector
  , canPerformMultiplePassesOverSourceMediaDataSelector
  , cancelExportSelector
  , customVideoCompositorSelector
  , determineCompatibilityOfExportPreset_withAsset_outputFileType_completionHandlerSelector
  , directoryForTemporaryFilesSelector
  , errorSelector
  , estimateMaximumDurationWithCompletionHandlerSelector
  , estimateOutputFileLengthWithCompletionHandlerSelector
  , estimatedOutputFileLengthSelector
  , exportAsynchronouslyWithCompletionHandlerSelector
  , exportPresetsCompatibleWithAssetSelector
  , exportSessionWithAsset_presetNameSelector
  , fileLengthLimitSelector
  , initSelector
  , initWithAsset_presetNameSelector
  , metadataItemFilterSelector
  , metadataSelector
  , newSelector
  , outputFileTypeSelector
  , outputURLSelector
  , presetNameSelector
  , progressSelector
  , setAllowsParallelizedExportSelector
  , setAudioMixSelector
  , setAudioTimePitchAlgorithmSelector
  , setAudioTrackGroupHandlingSelector
  , setCanPerformMultiplePassesOverSourceMediaDataSelector
  , setDirectoryForTemporaryFilesSelector
  , setFileLengthLimitSelector
  , setMetadataItemFilterSelector
  , setMetadataSelector
  , setOutputFileTypeSelector
  , setOutputURLSelector
  , setShouldOptimizeForNetworkUseSelector
  , setVideoCompositionSelector
  , shouldOptimizeForNetworkUseSelector
  , statusSelector
  , supportedFileTypesSelector
  , videoCompositionSelector

  -- * Enum types
  , AVAssetExportSessionStatus(AVAssetExportSessionStatus)
  , pattern AVAssetExportSessionStatusUnknown
  , pattern AVAssetExportSessionStatusWaiting
  , pattern AVAssetExportSessionStatusExporting
  , pattern AVAssetExportSessionStatusCompleted
  , pattern AVAssetExportSessionStatusFailed
  , pattern AVAssetExportSessionStatusCancelled
  , AVAssetTrackGroupOutputHandling(AVAssetTrackGroupOutputHandling)
  , pattern AVAssetTrackGroupOutputHandlingNone
  , pattern AVAssetTrackGroupOutputHandlingPreserveAlternateTracks
  , pattern AVAssetTrackGroupOutputHandlingDefaultPolicy

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
init_ :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO (Id AVAssetExportSession)
init_ avAssetExportSession =
  sendOwnedMessage avAssetExportSession initSelector

-- | @+ new@
new :: IO (Id AVAssetExportSession)
new  =
  do
    cls' <- getRequiredClass "AVAssetExportSession"
    sendOwnedClassMessage cls' newSelector

-- | exportSessionWithAsset:presetName:
--
-- Returns an instance of AVAssetExportSession for the specified source asset and preset.
--
-- @asset@ — An AVAsset object that is intended to be exported.
--
-- @presetName@ — An NSString specifying the name of the preset template for the export.
--
-- Returns: An instance of AVAssetExportSession.
--
-- If the specified asset belongs to a mutable subclass of AVAsset, AVMutableComposition or AVMutableMovie, the results of any export-related operation are undefined if you mutate the asset after the operation commences. These operations include but are not limited to: 1) testing the compatibility of export presets with the asset, 2) calculating the maximum duration or estimated length of the output file, and 3) the export operation itself.
--
-- ObjC selector: @+ exportSessionWithAsset:presetName:@
exportSessionWithAsset_presetName :: (IsAVAsset asset, IsNSString presetName) => asset -> presetName -> IO (Id AVAssetExportSession)
exportSessionWithAsset_presetName asset presetName =
  do
    cls' <- getRequiredClass "AVAssetExportSession"
    sendClassMessage cls' exportSessionWithAsset_presetNameSelector (toAVAsset asset) (toNSString presetName)

-- | initWithAsset:presetName:
--
-- Initialize an AVAssetExportSession with the specified preset and set the source to the contents of the asset.
--
-- @asset@ — An AVAsset object that is intended to be exported.
--
-- @presetName@ — An NSString specifying the name of the preset template for the export.
--
-- Returns: Returns the initialized AVAssetExportSession.
--
-- If the specified asset belongs to a mutable subclass of AVAsset, AVMutableComposition or AVMutableMovie, the results of any export-related operation are undefined if you mutate the asset after the operation commences. These operations include but are not limited to: 1) testing the compatibility of export presets with the asset, 2) calculating the maximum duration or estimated length of the output file, and 3) the export operation itself.
--
-- ObjC selector: @- initWithAsset:presetName:@
initWithAsset_presetName :: (IsAVAssetExportSession avAssetExportSession, IsAVAsset asset, IsNSString presetName) => avAssetExportSession -> asset -> presetName -> IO (Id AVAssetExportSession)
initWithAsset_presetName avAssetExportSession asset presetName =
  sendOwnedMessage avAssetExportSession initWithAsset_presetNameSelector (toAVAsset asset) (toNSString presetName)

-- | exportAsynchronouslyWithCompletionHandler:
--
-- Starts the asynchronous execution of an export session.
--
-- @handler@ — If internal preparation for export fails, the handler will be invoked synchronously.								The handler may also be called asynchronously after -exportAsynchronouslyWithCompletionHandler: returns,								in the following cases: 								1) if a failure occurs during the export, including failures of loading, re-encoding, or writing media data to the output,								2) if -cancelExport is invoked, 								3) if export session succeeds, having completely written its output to the outputURL. 								In each case, AVAssetExportSession.status will signal the terminal state of the asset reader, and if a failure occurs, the NSError 								that describes the failure can be obtained from the error property.
--
-- Initiates an asynchronous export operation and returns immediately.
--
-- ObjC selector: @- exportAsynchronouslyWithCompletionHandler:@
exportAsynchronouslyWithCompletionHandler :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> Ptr () -> IO ()
exportAsynchronouslyWithCompletionHandler avAssetExportSession handler =
  sendMessage avAssetExportSession exportAsynchronouslyWithCompletionHandlerSelector handler

-- | cancelExport
--
-- Cancels the execution of an export session.
--
-- Cancel can be invoked when the export is running.
--
-- ObjC selector: @- cancelExport@
cancelExport :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO ()
cancelExport avAssetExportSession =
  sendMessage avAssetExportSession cancelExportSelector

-- | estimateMaximumDurationWithCompletionHandler:
--
-- Starts the asynchronous execution of estimating the maximum duration of the export based on the asset, preset, and fileLengthLimit associated with the export session.
--
-- If fileLengthLimit is not set on the export session, fileLengthLimit will be assumed to be the maximum file size specified by the preset (if any); else infinite.
--
-- @handler@ — A block called with the estimated maximum duration, or kCMTimeInvalid if an error occurs.  The error parameter will be non-nil if an error occurs.
--
-- ObjC selector: @- estimateMaximumDurationWithCompletionHandler:@
estimateMaximumDurationWithCompletionHandler :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> Ptr () -> IO ()
estimateMaximumDurationWithCompletionHandler avAssetExportSession handler =
  sendMessage avAssetExportSession estimateMaximumDurationWithCompletionHandlerSelector handler

-- | estimateOutputFileLengthWithCompletionHandler:
--
-- Starts the asynchronous execution of estimating the output file length of the export based on the asset, preset, and timeRange associated with the export session.
--
-- If timeRange is not set on the export session, timeRange will be assumed to be the full time range of the asset.
--
-- @handler@ — A block called with the estimated output file length in bytes, if it can be determined; 0 otherwise.  The error parameter will be non-nil if an error occurs.
--
-- ObjC selector: @- estimateOutputFileLengthWithCompletionHandler:@
estimateOutputFileLengthWithCompletionHandler :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> Ptr () -> IO ()
estimateOutputFileLengthWithCompletionHandler avAssetExportSession handler =
  sendMessage avAssetExportSession estimateOutputFileLengthWithCompletionHandlerSelector handler

-- | allExportPresets
--
-- Returns all available export preset names.
--
-- Returns an array of NSStrings with the names of all available presets. Note that not all presets are 								compatible with all AVAssets.
--
-- Returns: An NSArray containing an NSString for each of the available preset names.
--
-- ObjC selector: @+ allExportPresets@
allExportPresets :: IO (Id NSArray)
allExportPresets  =
  do
    cls' <- getRequiredClass "AVAssetExportSession"
    sendClassMessage cls' allExportPresetsSelector

-- | exportPresetsCompatibleWithAsset:
--
-- Returns only the identifiers compatible with the given AVAsset object.
--
-- Not all export presets are compatible with all AVAssets. For example an video only asset is not compatible with an audio only preset.								This method returns only the identifiers for presets that will be compatible with the given asset. 								A client should pass in an AVAsset that is ready to be exported.								In order to ensure that the setup and running of an export operation will succeed using a given preset no significant changes 								(such as adding or deleting tracks) should be made to the asset between retrieving compatible identifiers and performing the export operation.								This method will access the tracks property of the AVAsset to build the returned NSArray.  To avoid blocking the calling thread, 								the tracks property should be loaded using the AVAsynchronousKeyValueLoading protocol before calling this method.
--
-- @asset@ — An AVAsset object that is intended to be exported.
--
-- Returns: An NSArray containing NSString values for the identifiers of compatible export types.  								The array is a complete list of the valid identifiers that can be used as arguments to 								initWithAsset:presetName: with the specified asset.
--
-- ObjC selector: @+ exportPresetsCompatibleWithAsset:@
exportPresetsCompatibleWithAsset :: IsAVAsset asset => asset -> IO (Id NSArray)
exportPresetsCompatibleWithAsset asset =
  do
    cls' <- getRequiredClass "AVAssetExportSession"
    sendClassMessage cls' exportPresetsCompatibleWithAssetSelector (toAVAsset asset)

-- | determineCompatibilityOfExportPreset:withAsset:outputFileType:completionHandler:
--
-- Performs an inspection on the compatibility of an export preset, AVAsset and output file type.  Calls the completion handler with YES if								the arguments are compatible; NO otherwise.
--
-- Not all export presets are compatible with all AVAssets and file types.  This method can be used to query compatibility.								In order to ensure that the setup and running of an export operation will succeed using a given preset no significant changes 								(such as adding or deleting tracks) should be made to the asset between retrieving compatible identifiers and performing the export operation.
--
-- @presetName@ — An NSString specifying the name of the preset template for the export.
--
-- @asset@ — An AVAsset object that is intended to be exported.
--
-- @outputFileType@ — An AVFileType indicating a file type to check; or nil, to query whether there are any compatible types.
--
-- @handler@ — A block called with the compatibility result.
--
-- ObjC selector: @+ determineCompatibilityOfExportPreset:withAsset:outputFileType:completionHandler:@
determineCompatibilityOfExportPreset_withAsset_outputFileType_completionHandler :: (IsNSString presetName, IsAVAsset asset, IsNSString outputFileType) => presetName -> asset -> outputFileType -> Ptr () -> IO ()
determineCompatibilityOfExportPreset_withAsset_outputFileType_completionHandler presetName asset outputFileType handler =
  do
    cls' <- getRequiredClass "AVAssetExportSession"
    sendClassMessage cls' determineCompatibilityOfExportPreset_withAsset_outputFileType_completionHandlerSelector (toNSString presetName) (toAVAsset asset) (toNSString outputFileType) handler

-- | @- presetName@
presetName :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO (Id NSString)
presetName avAssetExportSession =
  sendMessage avAssetExportSession presetNameSelector

-- | @- asset@
asset :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO (Id AVAsset)
asset avAssetExportSession =
  sendMessage avAssetExportSession assetSelector

-- | @- outputFileType@
outputFileType :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO (Id NSString)
outputFileType avAssetExportSession =
  sendMessage avAssetExportSession outputFileTypeSelector

-- | @- setOutputFileType:@
setOutputFileType :: (IsAVAssetExportSession avAssetExportSession, IsNSString value) => avAssetExportSession -> value -> IO ()
setOutputFileType avAssetExportSession value =
  sendMessage avAssetExportSession setOutputFileTypeSelector (toNSString value)

-- | @- outputURL@
outputURL :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO (Id NSURL)
outputURL avAssetExportSession =
  sendMessage avAssetExportSession outputURLSelector

-- | @- setOutputURL:@
setOutputURL :: (IsAVAssetExportSession avAssetExportSession, IsNSURL value) => avAssetExportSession -> value -> IO ()
setOutputURL avAssetExportSession value =
  sendMessage avAssetExportSession setOutputURLSelector (toNSURL value)

-- | @- shouldOptimizeForNetworkUse@
shouldOptimizeForNetworkUse :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO Bool
shouldOptimizeForNetworkUse avAssetExportSession =
  sendMessage avAssetExportSession shouldOptimizeForNetworkUseSelector

-- | @- setShouldOptimizeForNetworkUse:@
setShouldOptimizeForNetworkUse :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> Bool -> IO ()
setShouldOptimizeForNetworkUse avAssetExportSession value =
  sendMessage avAssetExportSession setShouldOptimizeForNetworkUseSelector value

-- | allowsParallelizedExport
--
-- Determines whether or not parallelization can be employed in the export.
--
-- On select platforms, there may be opportunities to expedite the export by using additional resources in parallel.				If set to YES, export parallelization will be enabled, only if parallelization requirements are met.  There will				be no error signaled if export parallelization is not achievable, and instead the export will proceed as normal				(without parallelization).				If set to NO, export parallelization will not be used.
--
-- ObjC selector: @- allowsParallelizedExport@
allowsParallelizedExport :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO Bool
allowsParallelizedExport avAssetExportSession =
  sendMessage avAssetExportSession allowsParallelizedExportSelector

-- | allowsParallelizedExport
--
-- Determines whether or not parallelization can be employed in the export.
--
-- On select platforms, there may be opportunities to expedite the export by using additional resources in parallel.				If set to YES, export parallelization will be enabled, only if parallelization requirements are met.  There will				be no error signaled if export parallelization is not achievable, and instead the export will proceed as normal				(without parallelization).				If set to NO, export parallelization will not be used.
--
-- ObjC selector: @- setAllowsParallelizedExport:@
setAllowsParallelizedExport :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> Bool -> IO ()
setAllowsParallelizedExport avAssetExportSession value =
  sendMessage avAssetExportSession setAllowsParallelizedExportSelector value

-- | @- status@
status :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO AVAssetExportSessionStatus
status avAssetExportSession =
  sendMessage avAssetExportSession statusSelector

-- | @- error@
error_ :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO (Id NSError)
error_ avAssetExportSession =
  sendMessage avAssetExportSession errorSelector

-- | @- progress@
progress :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO CFloat
progress avAssetExportSession =
  sendMessage avAssetExportSession progressSelector

-- | canPerformMultiplePassesOverSourceMediaData
--
-- Determines whether the export session can perform multiple passes over the source media to achieve better results.
--
-- When the value for this property is YES, the export session can produce higher quality results at the expense of longer export times.  Setting this property to YES may also require the export session to write temporary data to disk during the export.  To control the location of temporary data, use the property directoryForTemporaryFiles.
--
-- The default value is NO.  Not all export session configurations can benefit from performing multiple passes over the source media.  In these cases, setting this property to YES has no effect.
--
-- This property cannot be set after the export has started.
--
-- ObjC selector: @- canPerformMultiplePassesOverSourceMediaData@
canPerformMultiplePassesOverSourceMediaData :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO Bool
canPerformMultiplePassesOverSourceMediaData avAssetExportSession =
  sendMessage avAssetExportSession canPerformMultiplePassesOverSourceMediaDataSelector

-- | canPerformMultiplePassesOverSourceMediaData
--
-- Determines whether the export session can perform multiple passes over the source media to achieve better results.
--
-- When the value for this property is YES, the export session can produce higher quality results at the expense of longer export times.  Setting this property to YES may also require the export session to write temporary data to disk during the export.  To control the location of temporary data, use the property directoryForTemporaryFiles.
--
-- The default value is NO.  Not all export session configurations can benefit from performing multiple passes over the source media.  In these cases, setting this property to YES has no effect.
--
-- This property cannot be set after the export has started.
--
-- ObjC selector: @- setCanPerformMultiplePassesOverSourceMediaData:@
setCanPerformMultiplePassesOverSourceMediaData :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> Bool -> IO ()
setCanPerformMultiplePassesOverSourceMediaData avAssetExportSession value =
  sendMessage avAssetExportSession setCanPerformMultiplePassesOverSourceMediaDataSelector value

-- | directoryForTemporaryFiles
--
-- Specifies a directory that is suitable for containing temporary files generated during the export process
--
-- AVAssetExportSession may need to write temporary files when configured in certain ways, such as when canPerformMultiplePassesOverSourceMediaData is set to YES.  This property can be used to control where in the filesystem those temporary files are created.  All temporary files will be deleted when the export is completed, is canceled, or fails.
--
-- When the value of this property is nil, the export session will choose a suitable location when writing temporary files.  The default value is nil.
--
-- This property cannot be set after the export has started.  The export will fail if the URL points to a location that is not a directory, does not exist, is not on the local file system, or if a file cannot be created in this directory (for example, due to insufficient permissions or sandboxing restrictions).
--
-- ObjC selector: @- directoryForTemporaryFiles@
directoryForTemporaryFiles :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO (Id NSURL)
directoryForTemporaryFiles avAssetExportSession =
  sendMessage avAssetExportSession directoryForTemporaryFilesSelector

-- | directoryForTemporaryFiles
--
-- Specifies a directory that is suitable for containing temporary files generated during the export process
--
-- AVAssetExportSession may need to write temporary files when configured in certain ways, such as when canPerformMultiplePassesOverSourceMediaData is set to YES.  This property can be used to control where in the filesystem those temporary files are created.  All temporary files will be deleted when the export is completed, is canceled, or fails.
--
-- When the value of this property is nil, the export session will choose a suitable location when writing temporary files.  The default value is nil.
--
-- This property cannot be set after the export has started.  The export will fail if the URL points to a location that is not a directory, does not exist, is not on the local file system, or if a file cannot be created in this directory (for example, due to insufficient permissions or sandboxing restrictions).
--
-- ObjC selector: @- setDirectoryForTemporaryFiles:@
setDirectoryForTemporaryFiles :: (IsAVAssetExportSession avAssetExportSession, IsNSURL value) => avAssetExportSession -> value -> IO ()
setDirectoryForTemporaryFiles avAssetExportSession value =
  sendMessage avAssetExportSession setDirectoryForTemporaryFilesSelector (toNSURL value)

-- | @- audioTimePitchAlgorithm@
audioTimePitchAlgorithm :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO (Id NSString)
audioTimePitchAlgorithm avAssetExportSession =
  sendMessage avAssetExportSession audioTimePitchAlgorithmSelector

-- | @- setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithm :: (IsAVAssetExportSession avAssetExportSession, IsNSString value) => avAssetExportSession -> value -> IO ()
setAudioTimePitchAlgorithm avAssetExportSession value =
  sendMessage avAssetExportSession setAudioTimePitchAlgorithmSelector (toNSString value)

-- | @- audioMix@
audioMix :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO (Id AVAudioMix)
audioMix avAssetExportSession =
  sendMessage avAssetExportSession audioMixSelector

-- | @- setAudioMix:@
setAudioMix :: (IsAVAssetExportSession avAssetExportSession, IsAVAudioMix value) => avAssetExportSession -> value -> IO ()
setAudioMix avAssetExportSession value =
  sendMessage avAssetExportSession setAudioMixSelector (toAVAudioMix value)

-- | @- videoComposition@
videoComposition :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO (Id AVVideoComposition)
videoComposition avAssetExportSession =
  sendMessage avAssetExportSession videoCompositionSelector

-- | @- setVideoComposition:@
setVideoComposition :: (IsAVAssetExportSession avAssetExportSession, IsAVVideoComposition value) => avAssetExportSession -> value -> IO ()
setVideoComposition avAssetExportSession value =
  sendMessage avAssetExportSession setVideoCompositionSelector (toAVVideoComposition value)

-- | @- customVideoCompositor@
customVideoCompositor :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO RawId
customVideoCompositor avAssetExportSession =
  sendMessage avAssetExportSession customVideoCompositorSelector

-- | audioTrackGroupHandling
--
-- Defines export policy for handling alternate audio tracks
--
-- Specifies the handling of audio tracks that are members of the same alternate track group corresponding to an exported audio track in the source asset.				If no audio track group is present, the value of this property has no effect.				If necessary, use the trackGroups property of AVAsset to determine whether any audio track groups are present.				The AVAudioMix property is not allowed to be used when also specifying alternate track output handling.  An exception will be thrown if both are specified.
--
-- ObjC selector: @- audioTrackGroupHandling@
audioTrackGroupHandling :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO AVAssetTrackGroupOutputHandling
audioTrackGroupHandling avAssetExportSession =
  sendMessage avAssetExportSession audioTrackGroupHandlingSelector

-- | audioTrackGroupHandling
--
-- Defines export policy for handling alternate audio tracks
--
-- Specifies the handling of audio tracks that are members of the same alternate track group corresponding to an exported audio track in the source asset.				If no audio track group is present, the value of this property has no effect.				If necessary, use the trackGroups property of AVAsset to determine whether any audio track groups are present.				The AVAudioMix property is not allowed to be used when also specifying alternate track output handling.  An exception will be thrown if both are specified.
--
-- ObjC selector: @- setAudioTrackGroupHandling:@
setAudioTrackGroupHandling :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> AVAssetTrackGroupOutputHandling -> IO ()
setAudioTrackGroupHandling avAssetExportSession value =
  sendMessage avAssetExportSession setAudioTrackGroupHandlingSelector value

-- | @- metadata@
metadata :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO (Id NSArray)
metadata avAssetExportSession =
  sendMessage avAssetExportSession metadataSelector

-- | @- setMetadata:@
setMetadata :: (IsAVAssetExportSession avAssetExportSession, IsNSArray value) => avAssetExportSession -> value -> IO ()
setMetadata avAssetExportSession value =
  sendMessage avAssetExportSession setMetadataSelector (toNSArray value)

-- | @- metadataItemFilter@
metadataItemFilter :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO (Id AVMetadataItemFilter)
metadataItemFilter avAssetExportSession =
  sendMessage avAssetExportSession metadataItemFilterSelector

-- | @- setMetadataItemFilter:@
setMetadataItemFilter :: (IsAVAssetExportSession avAssetExportSession, IsAVMetadataItemFilter value) => avAssetExportSession -> value -> IO ()
setMetadataItemFilter avAssetExportSession value =
  sendMessage avAssetExportSession setMetadataItemFilterSelector (toAVMetadataItemFilter value)

-- | @- estimatedOutputFileLength@
estimatedOutputFileLength :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO CLong
estimatedOutputFileLength avAssetExportSession =
  sendMessage avAssetExportSession estimatedOutputFileLengthSelector

-- | @- fileLengthLimit@
fileLengthLimit :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO CLong
fileLengthLimit avAssetExportSession =
  sendMessage avAssetExportSession fileLengthLimitSelector

-- | @- setFileLengthLimit:@
setFileLengthLimit :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> CLong -> IO ()
setFileLengthLimit avAssetExportSession value =
  sendMessage avAssetExportSession setFileLengthLimitSelector value

-- | @- supportedFileTypes@
supportedFileTypes :: IsAVAssetExportSession avAssetExportSession => avAssetExportSession -> IO (Id NSArray)
supportedFileTypes avAssetExportSession =
  sendMessage avAssetExportSession supportedFileTypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetExportSession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetExportSession)
newSelector = mkSelector "new"

-- | @Selector@ for @exportSessionWithAsset:presetName:@
exportSessionWithAsset_presetNameSelector :: Selector '[Id AVAsset, Id NSString] (Id AVAssetExportSession)
exportSessionWithAsset_presetNameSelector = mkSelector "exportSessionWithAsset:presetName:"

-- | @Selector@ for @initWithAsset:presetName:@
initWithAsset_presetNameSelector :: Selector '[Id AVAsset, Id NSString] (Id AVAssetExportSession)
initWithAsset_presetNameSelector = mkSelector "initWithAsset:presetName:"

-- | @Selector@ for @exportAsynchronouslyWithCompletionHandler:@
exportAsynchronouslyWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
exportAsynchronouslyWithCompletionHandlerSelector = mkSelector "exportAsynchronouslyWithCompletionHandler:"

-- | @Selector@ for @cancelExport@
cancelExportSelector :: Selector '[] ()
cancelExportSelector = mkSelector "cancelExport"

-- | @Selector@ for @estimateMaximumDurationWithCompletionHandler:@
estimateMaximumDurationWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
estimateMaximumDurationWithCompletionHandlerSelector = mkSelector "estimateMaximumDurationWithCompletionHandler:"

-- | @Selector@ for @estimateOutputFileLengthWithCompletionHandler:@
estimateOutputFileLengthWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
estimateOutputFileLengthWithCompletionHandlerSelector = mkSelector "estimateOutputFileLengthWithCompletionHandler:"

-- | @Selector@ for @allExportPresets@
allExportPresetsSelector :: Selector '[] (Id NSArray)
allExportPresetsSelector = mkSelector "allExportPresets"

-- | @Selector@ for @exportPresetsCompatibleWithAsset:@
exportPresetsCompatibleWithAssetSelector :: Selector '[Id AVAsset] (Id NSArray)
exportPresetsCompatibleWithAssetSelector = mkSelector "exportPresetsCompatibleWithAsset:"

-- | @Selector@ for @determineCompatibilityOfExportPreset:withAsset:outputFileType:completionHandler:@
determineCompatibilityOfExportPreset_withAsset_outputFileType_completionHandlerSelector :: Selector '[Id NSString, Id AVAsset, Id NSString, Ptr ()] ()
determineCompatibilityOfExportPreset_withAsset_outputFileType_completionHandlerSelector = mkSelector "determineCompatibilityOfExportPreset:withAsset:outputFileType:completionHandler:"

-- | @Selector@ for @presetName@
presetNameSelector :: Selector '[] (Id NSString)
presetNameSelector = mkSelector "presetName"

-- | @Selector@ for @asset@
assetSelector :: Selector '[] (Id AVAsset)
assetSelector = mkSelector "asset"

-- | @Selector@ for @outputFileType@
outputFileTypeSelector :: Selector '[] (Id NSString)
outputFileTypeSelector = mkSelector "outputFileType"

-- | @Selector@ for @setOutputFileType:@
setOutputFileTypeSelector :: Selector '[Id NSString] ()
setOutputFileTypeSelector = mkSelector "setOutputFileType:"

-- | @Selector@ for @outputURL@
outputURLSelector :: Selector '[] (Id NSURL)
outputURLSelector = mkSelector "outputURL"

-- | @Selector@ for @setOutputURL:@
setOutputURLSelector :: Selector '[Id NSURL] ()
setOutputURLSelector = mkSelector "setOutputURL:"

-- | @Selector@ for @shouldOptimizeForNetworkUse@
shouldOptimizeForNetworkUseSelector :: Selector '[] Bool
shouldOptimizeForNetworkUseSelector = mkSelector "shouldOptimizeForNetworkUse"

-- | @Selector@ for @setShouldOptimizeForNetworkUse:@
setShouldOptimizeForNetworkUseSelector :: Selector '[Bool] ()
setShouldOptimizeForNetworkUseSelector = mkSelector "setShouldOptimizeForNetworkUse:"

-- | @Selector@ for @allowsParallelizedExport@
allowsParallelizedExportSelector :: Selector '[] Bool
allowsParallelizedExportSelector = mkSelector "allowsParallelizedExport"

-- | @Selector@ for @setAllowsParallelizedExport:@
setAllowsParallelizedExportSelector :: Selector '[Bool] ()
setAllowsParallelizedExportSelector = mkSelector "setAllowsParallelizedExport:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] AVAssetExportSessionStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

-- | @Selector@ for @progress@
progressSelector :: Selector '[] CFloat
progressSelector = mkSelector "progress"

-- | @Selector@ for @canPerformMultiplePassesOverSourceMediaData@
canPerformMultiplePassesOverSourceMediaDataSelector :: Selector '[] Bool
canPerformMultiplePassesOverSourceMediaDataSelector = mkSelector "canPerformMultiplePassesOverSourceMediaData"

-- | @Selector@ for @setCanPerformMultiplePassesOverSourceMediaData:@
setCanPerformMultiplePassesOverSourceMediaDataSelector :: Selector '[Bool] ()
setCanPerformMultiplePassesOverSourceMediaDataSelector = mkSelector "setCanPerformMultiplePassesOverSourceMediaData:"

-- | @Selector@ for @directoryForTemporaryFiles@
directoryForTemporaryFilesSelector :: Selector '[] (Id NSURL)
directoryForTemporaryFilesSelector = mkSelector "directoryForTemporaryFiles"

-- | @Selector@ for @setDirectoryForTemporaryFiles:@
setDirectoryForTemporaryFilesSelector :: Selector '[Id NSURL] ()
setDirectoryForTemporaryFilesSelector = mkSelector "setDirectoryForTemporaryFiles:"

-- | @Selector@ for @audioTimePitchAlgorithm@
audioTimePitchAlgorithmSelector :: Selector '[] (Id NSString)
audioTimePitchAlgorithmSelector = mkSelector "audioTimePitchAlgorithm"

-- | @Selector@ for @setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithmSelector :: Selector '[Id NSString] ()
setAudioTimePitchAlgorithmSelector = mkSelector "setAudioTimePitchAlgorithm:"

-- | @Selector@ for @audioMix@
audioMixSelector :: Selector '[] (Id AVAudioMix)
audioMixSelector = mkSelector "audioMix"

-- | @Selector@ for @setAudioMix:@
setAudioMixSelector :: Selector '[Id AVAudioMix] ()
setAudioMixSelector = mkSelector "setAudioMix:"

-- | @Selector@ for @videoComposition@
videoCompositionSelector :: Selector '[] (Id AVVideoComposition)
videoCompositionSelector = mkSelector "videoComposition"

-- | @Selector@ for @setVideoComposition:@
setVideoCompositionSelector :: Selector '[Id AVVideoComposition] ()
setVideoCompositionSelector = mkSelector "setVideoComposition:"

-- | @Selector@ for @customVideoCompositor@
customVideoCompositorSelector :: Selector '[] RawId
customVideoCompositorSelector = mkSelector "customVideoCompositor"

-- | @Selector@ for @audioTrackGroupHandling@
audioTrackGroupHandlingSelector :: Selector '[] AVAssetTrackGroupOutputHandling
audioTrackGroupHandlingSelector = mkSelector "audioTrackGroupHandling"

-- | @Selector@ for @setAudioTrackGroupHandling:@
setAudioTrackGroupHandlingSelector :: Selector '[AVAssetTrackGroupOutputHandling] ()
setAudioTrackGroupHandlingSelector = mkSelector "setAudioTrackGroupHandling:"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSArray)
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector '[Id NSArray] ()
setMetadataSelector = mkSelector "setMetadata:"

-- | @Selector@ for @metadataItemFilter@
metadataItemFilterSelector :: Selector '[] (Id AVMetadataItemFilter)
metadataItemFilterSelector = mkSelector "metadataItemFilter"

-- | @Selector@ for @setMetadataItemFilter:@
setMetadataItemFilterSelector :: Selector '[Id AVMetadataItemFilter] ()
setMetadataItemFilterSelector = mkSelector "setMetadataItemFilter:"

-- | @Selector@ for @estimatedOutputFileLength@
estimatedOutputFileLengthSelector :: Selector '[] CLong
estimatedOutputFileLengthSelector = mkSelector "estimatedOutputFileLength"

-- | @Selector@ for @fileLengthLimit@
fileLengthLimitSelector :: Selector '[] CLong
fileLengthLimitSelector = mkSelector "fileLengthLimit"

-- | @Selector@ for @setFileLengthLimit:@
setFileLengthLimitSelector :: Selector '[CLong] ()
setFileLengthLimitSelector = mkSelector "setFileLengthLimit:"

-- | @Selector@ for @supportedFileTypes@
supportedFileTypesSelector :: Selector '[] (Id NSArray)
supportedFileTypesSelector = mkSelector "supportedFileTypes"

