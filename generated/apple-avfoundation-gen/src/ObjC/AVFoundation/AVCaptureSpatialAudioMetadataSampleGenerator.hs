{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An interface for generating a spatial audio timed metadata sample.
--
-- Generated bindings for @AVCaptureSpatialAudioMetadataSampleGenerator@.
module ObjC.AVFoundation.AVCaptureSpatialAudioMetadataSampleGenerator
  ( AVCaptureSpatialAudioMetadataSampleGenerator
  , IsAVCaptureSpatialAudioMetadataSampleGenerator(..)
  , analyzeAudioSample
  , newTimedMetadataSampleBufferAndResetAnalyzer
  , resetAnalyzer
  , timedMetadataSampleBufferFormatDescription
  , analyzeAudioSampleSelector
  , newTimedMetadataSampleBufferAndResetAnalyzerSelector
  , resetAnalyzerSelector
  , timedMetadataSampleBufferFormatDescriptionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Analyzes the provided audio sample buffer for its contribution to the spatial audio timed metadata value.
--
-- - Parameter sbuf: a sample buffer containing spatial audio. - Returns: @noErr@ if the sample is successfully analyzed, otherwise a non-zero error code.
--
-- You must call this method with each and every spatial audio buffer you provide to ``AVAssetWriter``, so it can be analyzed for the generation of a proper spatial audio timed metadata value.
--
-- ObjC selector: @- analyzeAudioSample:@
analyzeAudioSample :: IsAVCaptureSpatialAudioMetadataSampleGenerator avCaptureSpatialAudioMetadataSampleGenerator => avCaptureSpatialAudioMetadataSampleGenerator -> Ptr () -> IO CInt
analyzeAudioSample avCaptureSpatialAudioMetadataSampleGenerator sbuf =
  sendMessage avCaptureSpatialAudioMetadataSampleGenerator analyzeAudioSampleSelector sbuf

-- | Creates a sample buffer containing a spatial audio timed metadata sample computed from all analyzed audio buffers, and resets the analyzer to its initial state.
--
-- - Returns: a ``CMSampleBufferRef`` containing the spatial audio timed metadata sample, or @NULL@ if no value can be computed.
--
-- Call this method after you pass the last audio sample buffer of your recording to ``analyzeAudioSample:``. Then pass the returned ``CMSampleBufferRef`` directly to your ``AVAssetWriterInput`` to add the sample to your recording's audio timed metadata track. Note that ``AVAssetWriter`` expects one and only one spatial audio metadata sample buffer to be present in the timed metadata track.
--
-- - Note: Calling this method also resets the analyzer, making it ready for another run of audio sample buffers. Thus one generator can be re-used for multiple recordings.
--
-- ObjC selector: @- newTimedMetadataSampleBufferAndResetAnalyzer@
newTimedMetadataSampleBufferAndResetAnalyzer :: IsAVCaptureSpatialAudioMetadataSampleGenerator avCaptureSpatialAudioMetadataSampleGenerator => avCaptureSpatialAudioMetadataSampleGenerator -> IO (Ptr ())
newTimedMetadataSampleBufferAndResetAnalyzer avCaptureSpatialAudioMetadataSampleGenerator =
  sendOwnedMessage avCaptureSpatialAudioMetadataSampleGenerator newTimedMetadataSampleBufferAndResetAnalyzerSelector

-- | Calling this method resets the analyzer to its initial state so that a new run of audio sample buffers can be analyzed.
--
-- Call this method if you need to abort generating the audio timed metadata buffer for audio already provided to ``analyzeAudioSample:``.
--
-- ObjC selector: @- resetAnalyzer@
resetAnalyzer :: IsAVCaptureSpatialAudioMetadataSampleGenerator avCaptureSpatialAudioMetadataSampleGenerator => avCaptureSpatialAudioMetadataSampleGenerator -> IO ()
resetAnalyzer avCaptureSpatialAudioMetadataSampleGenerator =
  sendMessage avCaptureSpatialAudioMetadataSampleGenerator resetAnalyzerSelector

-- | Returns the format description of the sample buffer returned from the ``newTimedMetadataSampleBufferAndResetAnalyzer`` method.
--
-- Use this format description when creating your ``AVAssetWriter`` track for spatial audio timed metadata.
--
-- ObjC selector: @- timedMetadataSampleBufferFormatDescription@
timedMetadataSampleBufferFormatDescription :: IsAVCaptureSpatialAudioMetadataSampleGenerator avCaptureSpatialAudioMetadataSampleGenerator => avCaptureSpatialAudioMetadataSampleGenerator -> IO RawId
timedMetadataSampleBufferFormatDescription avCaptureSpatialAudioMetadataSampleGenerator =
  sendMessage avCaptureSpatialAudioMetadataSampleGenerator timedMetadataSampleBufferFormatDescriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @analyzeAudioSample:@
analyzeAudioSampleSelector :: Selector '[Ptr ()] CInt
analyzeAudioSampleSelector = mkSelector "analyzeAudioSample:"

-- | @Selector@ for @newTimedMetadataSampleBufferAndResetAnalyzer@
newTimedMetadataSampleBufferAndResetAnalyzerSelector :: Selector '[] (Ptr ())
newTimedMetadataSampleBufferAndResetAnalyzerSelector = mkSelector "newTimedMetadataSampleBufferAndResetAnalyzer"

-- | @Selector@ for @resetAnalyzer@
resetAnalyzerSelector :: Selector '[] ()
resetAnalyzerSelector = mkSelector "resetAnalyzer"

-- | @Selector@ for @timedMetadataSampleBufferFormatDescription@
timedMetadataSampleBufferFormatDescriptionSelector :: Selector '[] RawId
timedMetadataSampleBufferFormatDescriptionSelector = mkSelector "timedMetadataSampleBufferFormatDescription"

