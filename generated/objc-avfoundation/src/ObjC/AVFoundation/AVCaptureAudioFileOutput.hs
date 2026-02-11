{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureAudioFileOutput
--
-- AVCaptureAudioFileOutput is a concrete subclass of AVCaptureFileOutput that writes captured audio to any audio file type supported by CoreAudio.
--
-- AVCaptureAudioFileOutput implements the complete file recording interface declared by AVCaptureFileOutput for writing media data to audio files. In addition, instances of AVCaptureAudioFileOutput allow clients to configure options specific to the audio file formats, including allowing them to write metadata collections to each file and specify audio encoding options.
--
-- Generated bindings for @AVCaptureAudioFileOutput@.
module ObjC.AVFoundation.AVCaptureAudioFileOutput
  ( AVCaptureAudioFileOutput
  , IsAVCaptureAudioFileOutput(..)
  , init_
  , new
  , availableOutputFileTypes
  , startRecordingToOutputFileURL_outputFileType_recordingDelegate
  , metadata
  , setMetadata
  , audioSettings
  , setAudioSettings
  , initSelector
  , newSelector
  , availableOutputFileTypesSelector
  , startRecordingToOutputFileURL_outputFileType_recordingDelegateSelector
  , metadataSelector
  , setMetadataSelector
  , audioSettingsSelector
  , setAudioSettingsSelector


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
init_ :: IsAVCaptureAudioFileOutput avCaptureAudioFileOutput => avCaptureAudioFileOutput -> IO (Id AVCaptureAudioFileOutput)
init_ avCaptureAudioFileOutput  =
  sendMsg avCaptureAudioFileOutput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureAudioFileOutput)
new  =
  do
    cls' <- getRequiredClass "AVCaptureAudioFileOutput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | availableOutputFileTypes
--
-- Provides the file types AVCaptureAudioFileOutput can write.
--
-- Returns: An NSArray of UTIs identifying the file types the AVCaptureAudioFileOutput class can write.
--
-- ObjC selector: @+ availableOutputFileTypes@
availableOutputFileTypes :: IO (Id NSArray)
availableOutputFileTypes  =
  do
    cls' <- getRequiredClass "AVCaptureAudioFileOutput"
    sendClassMsg cls' (mkSelector "availableOutputFileTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | startRecordingToOutputFileURL:outputFileType:recordingDelegate:
--
-- Tells the receiver to start recording to a new file of the specified format, and specifies a delegate that will be notified when recording is finished.
--
-- @outputFileURL@ — An NSURL object containing the URL of the output file. This method throws an NSInvalidArgumentException if the URL is not a valid file URL.
--
-- @fileType@ — A UTI indicating the format of the file to be written.
--
-- @delegate@ — An object conforming to the AVCaptureFileOutputRecordingDelegate protocol. Clients must specify a delegate so that they can be notified when recording to the given URL is finished.
--
-- The method sets the file URL to which the receiver is currently writing output media. If a file at the given URL already exists when capturing starts, recording to the new file will fail.
--
-- The fileType argument is a UTI corresponding to the audio file format that should be written. UTIs for common audio file types are declared in AVMediaFormat.h.
--
-- Clients need not call stopRecording before calling this method while another recording is in progress. If this method is invoked while an existing output file was already being recorded, no media samples will be discarded between the old file and the new file.
--
-- When recording is stopped either by calling stopRecording, by changing files using this method, or because of an error, the remaining data that needs to be included to the file will be written in the background. Therefore, clients must specify a delegate that will be notified when all data has been written to the file using the captureOutput:didFinishRecordingToOutputFileAtURL:fromConnections:error: method. The recording delegate can also optionally implement methods that inform it when data starts being written, when recording is paused and resumed, and when recording is about to be finished.
--
-- On macOS, if this method is called within the captureOutput:didOutputSampleBuffer:fromConnection: delegate method, the first samples written to the new file are guaranteed to be those contained in the sample buffer passed to that method.
--
-- ObjC selector: @- startRecordingToOutputFileURL:outputFileType:recordingDelegate:@
startRecordingToOutputFileURL_outputFileType_recordingDelegate :: (IsAVCaptureAudioFileOutput avCaptureAudioFileOutput, IsNSURL outputFileURL, IsNSString fileType) => avCaptureAudioFileOutput -> outputFileURL -> fileType -> RawId -> IO ()
startRecordingToOutputFileURL_outputFileType_recordingDelegate avCaptureAudioFileOutput  outputFileURL fileType delegate =
withObjCPtr outputFileURL $ \raw_outputFileURL ->
  withObjCPtr fileType $ \raw_fileType ->
      sendMsg avCaptureAudioFileOutput (mkSelector "startRecordingToOutputFileURL:outputFileType:recordingDelegate:") retVoid [argPtr (castPtr raw_outputFileURL :: Ptr ()), argPtr (castPtr raw_fileType :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ())]

-- | metadata
--
-- A collection of metadata to be written to the receiver's output files.
--
-- The value of this property is an array of AVMetadataItem objects representing the collection of top-level metadata to be written in each output file. Only ID3 v2.2, v2.3, or v2.4 style metadata items are supported.
--
-- ObjC selector: @- metadata@
metadata :: IsAVCaptureAudioFileOutput avCaptureAudioFileOutput => avCaptureAudioFileOutput -> IO (Id NSArray)
metadata avCaptureAudioFileOutput  =
  sendMsg avCaptureAudioFileOutput (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metadata
--
-- A collection of metadata to be written to the receiver's output files.
--
-- The value of this property is an array of AVMetadataItem objects representing the collection of top-level metadata to be written in each output file. Only ID3 v2.2, v2.3, or v2.4 style metadata items are supported.
--
-- ObjC selector: @- setMetadata:@
setMetadata :: (IsAVCaptureAudioFileOutput avCaptureAudioFileOutput, IsNSArray value) => avCaptureAudioFileOutput -> value -> IO ()
setMetadata avCaptureAudioFileOutput  value =
withObjCPtr value $ \raw_value ->
    sendMsg avCaptureAudioFileOutput (mkSelector "setMetadata:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | audioSettings
--
-- Specifies the options the receiver uses to re-encode audio as it is being recorded.
--
-- The output settings dictionary can contain values for keys from AVAudioSettings.h. A value of nil indicates that the format of the audio should not be changed before being written to the file.
--
-- ObjC selector: @- audioSettings@
audioSettings :: IsAVCaptureAudioFileOutput avCaptureAudioFileOutput => avCaptureAudioFileOutput -> IO (Id NSDictionary)
audioSettings avCaptureAudioFileOutput  =
  sendMsg avCaptureAudioFileOutput (mkSelector "audioSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | audioSettings
--
-- Specifies the options the receiver uses to re-encode audio as it is being recorded.
--
-- The output settings dictionary can contain values for keys from AVAudioSettings.h. A value of nil indicates that the format of the audio should not be changed before being written to the file.
--
-- ObjC selector: @- setAudioSettings:@
setAudioSettings :: (IsAVCaptureAudioFileOutput avCaptureAudioFileOutput, IsNSDictionary value) => avCaptureAudioFileOutput -> value -> IO ()
setAudioSettings avCaptureAudioFileOutput  value =
withObjCPtr value $ \raw_value ->
    sendMsg avCaptureAudioFileOutput (mkSelector "setAudioSettings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @availableOutputFileTypes@
availableOutputFileTypesSelector :: Selector
availableOutputFileTypesSelector = mkSelector "availableOutputFileTypes"

-- | @Selector@ for @startRecordingToOutputFileURL:outputFileType:recordingDelegate:@
startRecordingToOutputFileURL_outputFileType_recordingDelegateSelector :: Selector
startRecordingToOutputFileURL_outputFileType_recordingDelegateSelector = mkSelector "startRecordingToOutputFileURL:outputFileType:recordingDelegate:"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector
setMetadataSelector = mkSelector "setMetadata:"

-- | @Selector@ for @audioSettings@
audioSettingsSelector :: Selector
audioSettingsSelector = mkSelector "audioSettings"

-- | @Selector@ for @setAudioSettings:@
setAudioSettingsSelector :: Selector
setAudioSettingsSelector = mkSelector "setAudioSettings:"

