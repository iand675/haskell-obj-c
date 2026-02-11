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
  , trackIDSelector
  , audioTimePitchAlgorithmSelector
  , audioTapProcessorSelector


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

-- | trackID
--
-- Indicates the trackID of the audio track to which the parameters should be applied.
--
-- ObjC selector: @- trackID@
trackID :: IsAVAudioMixInputParameters avAudioMixInputParameters => avAudioMixInputParameters -> IO CInt
trackID avAudioMixInputParameters  =
  sendMsg avAudioMixInputParameters (mkSelector "trackID") retCInt []

-- | audioTimePitchAlgorithm
--
-- Indicates the processing algorithm used to manage audio pitch at varying rates and for scaled audio edits.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchSpectral, are defined in AVAudioProcessingSettings.h.   Can be nil, in which case the audioTimePitchAlgorithm set on the AVPlayerItem, AVAssetExportSession, or AVAssetReaderAudioMixOutput on which the AVAudioMix is set will be used for the associated track.
--
-- ObjC selector: @- audioTimePitchAlgorithm@
audioTimePitchAlgorithm :: IsAVAudioMixInputParameters avAudioMixInputParameters => avAudioMixInputParameters -> IO (Id NSString)
audioTimePitchAlgorithm avAudioMixInputParameters  =
  sendMsg avAudioMixInputParameters (mkSelector "audioTimePitchAlgorithm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | audioTapProcessor
--
-- Indicates the audio processing tap that will be used for the audio track.
--
-- ObjC selector: @- audioTapProcessor@
audioTapProcessor :: IsAVAudioMixInputParameters avAudioMixInputParameters => avAudioMixInputParameters -> IO RawId
audioTapProcessor avAudioMixInputParameters  =
  fmap (RawId . castPtr) $ sendMsg avAudioMixInputParameters (mkSelector "audioTapProcessor") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @trackID@
trackIDSelector :: Selector
trackIDSelector = mkSelector "trackID"

-- | @Selector@ for @audioTimePitchAlgorithm@
audioTimePitchAlgorithmSelector :: Selector
audioTimePitchAlgorithmSelector = mkSelector "audioTimePitchAlgorithm"

-- | @Selector@ for @audioTapProcessor@
audioTapProcessorSelector :: Selector
audioTapProcessorSelector = mkSelector "audioTapProcessor"

