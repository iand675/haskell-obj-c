{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAudioMixInputParameters@.
module ObjC.AVFoundation.AVAudioMixInputParameters
  ( AVAudioMixInputParameters
  , IsAVAudioMixInputParameters(..)
  , trackID
  , audioTimePitchAlgorithm
  , audioTapProcessor
  , audioTapProcessorSelector
  , audioTimePitchAlgorithmSelector
  , trackIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | trackID
--
-- Indicates the trackID of the audio track to which the parameters should be applied.
--
-- ObjC selector: @- trackID@
trackID :: IsAVAudioMixInputParameters avAudioMixInputParameters => avAudioMixInputParameters -> IO CInt
trackID avAudioMixInputParameters =
  sendMessage avAudioMixInputParameters trackIDSelector

-- | audioTimePitchAlgorithm
--
-- Indicates the processing algorithm used to manage audio pitch at varying rates and for scaled audio edits.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchSpectral, are defined in AVAudioProcessingSettings.h.   Can be nil, in which case the audioTimePitchAlgorithm set on the AVPlayerItem, AVAssetExportSession, or AVAssetReaderAudioMixOutput on which the AVAudioMix is set will be used for the associated track.
--
-- ObjC selector: @- audioTimePitchAlgorithm@
audioTimePitchAlgorithm :: IsAVAudioMixInputParameters avAudioMixInputParameters => avAudioMixInputParameters -> IO (Id NSString)
audioTimePitchAlgorithm avAudioMixInputParameters =
  sendMessage avAudioMixInputParameters audioTimePitchAlgorithmSelector

-- | audioTapProcessor
--
-- Indicates the audio processing tap that will be used for the audio track.
--
-- ObjC selector: @- audioTapProcessor@
audioTapProcessor :: IsAVAudioMixInputParameters avAudioMixInputParameters => avAudioMixInputParameters -> IO RawId
audioTapProcessor avAudioMixInputParameters =
  sendMessage avAudioMixInputParameters audioTapProcessorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @trackID@
trackIDSelector :: Selector '[] CInt
trackIDSelector = mkSelector "trackID"

-- | @Selector@ for @audioTimePitchAlgorithm@
audioTimePitchAlgorithmSelector :: Selector '[] (Id NSString)
audioTimePitchAlgorithmSelector = mkSelector "audioTimePitchAlgorithm"

-- | @Selector@ for @audioTapProcessor@
audioTapProcessorSelector :: Selector '[] RawId
audioTapProcessorSelector = mkSelector "audioTapProcessor"

