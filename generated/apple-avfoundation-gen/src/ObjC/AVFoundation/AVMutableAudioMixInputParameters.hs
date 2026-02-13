{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMutableAudioMixInputParameters@.
module ObjC.AVFoundation.AVMutableAudioMixInputParameters
  ( AVMutableAudioMixInputParameters
  , IsAVMutableAudioMixInputParameters(..)
  , audioMixInputParametersWithTrack
  , audioMixInputParameters
  , trackID
  , setTrackID
  , audioTimePitchAlgorithm
  , setAudioTimePitchAlgorithm
  , audioTapProcessor
  , setAudioTapProcessor
  , audioMixInputParametersSelector
  , audioMixInputParametersWithTrackSelector
  , audioTapProcessorSelector
  , audioTimePitchAlgorithmSelector
  , setAudioTapProcessorSelector
  , setAudioTimePitchAlgorithmSelector
  , setTrackIDSelector
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

-- | @+ audioMixInputParametersWithTrack:@
audioMixInputParametersWithTrack :: IsAVAssetTrack track => track -> IO (Id AVMutableAudioMixInputParameters)
audioMixInputParametersWithTrack track =
  do
    cls' <- getRequiredClass "AVMutableAudioMixInputParameters"
    sendClassMessage cls' audioMixInputParametersWithTrackSelector (toAVAssetTrack track)

-- | @+ audioMixInputParameters@
audioMixInputParameters :: IO (Id AVMutableAudioMixInputParameters)
audioMixInputParameters  =
  do
    cls' <- getRequiredClass "AVMutableAudioMixInputParameters"
    sendClassMessage cls' audioMixInputParametersSelector

-- | trackID
--
-- Indicates the trackID of the audio track to which the parameters should be applied.
--
-- ObjC selector: @- trackID@
trackID :: IsAVMutableAudioMixInputParameters avMutableAudioMixInputParameters => avMutableAudioMixInputParameters -> IO CInt
trackID avMutableAudioMixInputParameters =
  sendMessage avMutableAudioMixInputParameters trackIDSelector

-- | trackID
--
-- Indicates the trackID of the audio track to which the parameters should be applied.
--
-- ObjC selector: @- setTrackID:@
setTrackID :: IsAVMutableAudioMixInputParameters avMutableAudioMixInputParameters => avMutableAudioMixInputParameters -> CInt -> IO ()
setTrackID avMutableAudioMixInputParameters value =
  sendMessage avMutableAudioMixInputParameters setTrackIDSelector value

-- | audioTimePitchAlgorithm
--
-- Indicates the processing algorithm used to manage audio pitch at varying rates and for scaled audio edits.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchSpectral, are defined in AVAudioProcessingSettings.h.   Can be nil, in which case the audioTimePitchAlgorithm set on the AVPlayerItem, AVAssetExportSession, or AVAssetReaderAudioMixOutput on which the AVAudioMix is set will be used for the associated track.
--
-- ObjC selector: @- audioTimePitchAlgorithm@
audioTimePitchAlgorithm :: IsAVMutableAudioMixInputParameters avMutableAudioMixInputParameters => avMutableAudioMixInputParameters -> IO (Id NSString)
audioTimePitchAlgorithm avMutableAudioMixInputParameters =
  sendMessage avMutableAudioMixInputParameters audioTimePitchAlgorithmSelector

-- | audioTimePitchAlgorithm
--
-- Indicates the processing algorithm used to manage audio pitch at varying rates and for scaled audio edits.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchSpectral, are defined in AVAudioProcessingSettings.h.   Can be nil, in which case the audioTimePitchAlgorithm set on the AVPlayerItem, AVAssetExportSession, or AVAssetReaderAudioMixOutput on which the AVAudioMix is set will be used for the associated track.
--
-- ObjC selector: @- setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithm :: (IsAVMutableAudioMixInputParameters avMutableAudioMixInputParameters, IsNSString value) => avMutableAudioMixInputParameters -> value -> IO ()
setAudioTimePitchAlgorithm avMutableAudioMixInputParameters value =
  sendMessage avMutableAudioMixInputParameters setAudioTimePitchAlgorithmSelector (toNSString value)

-- | audioTapProcessor
--
-- Indicates the audio processing tap that will be used for the audio track.
--
-- ObjC selector: @- audioTapProcessor@
audioTapProcessor :: IsAVMutableAudioMixInputParameters avMutableAudioMixInputParameters => avMutableAudioMixInputParameters -> IO RawId
audioTapProcessor avMutableAudioMixInputParameters =
  sendMessage avMutableAudioMixInputParameters audioTapProcessorSelector

-- | audioTapProcessor
--
-- Indicates the audio processing tap that will be used for the audio track.
--
-- ObjC selector: @- setAudioTapProcessor:@
setAudioTapProcessor :: IsAVMutableAudioMixInputParameters avMutableAudioMixInputParameters => avMutableAudioMixInputParameters -> RawId -> IO ()
setAudioTapProcessor avMutableAudioMixInputParameters value =
  sendMessage avMutableAudioMixInputParameters setAudioTapProcessorSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @audioMixInputParametersWithTrack:@
audioMixInputParametersWithTrackSelector :: Selector '[Id AVAssetTrack] (Id AVMutableAudioMixInputParameters)
audioMixInputParametersWithTrackSelector = mkSelector "audioMixInputParametersWithTrack:"

-- | @Selector@ for @audioMixInputParameters@
audioMixInputParametersSelector :: Selector '[] (Id AVMutableAudioMixInputParameters)
audioMixInputParametersSelector = mkSelector "audioMixInputParameters"

-- | @Selector@ for @trackID@
trackIDSelector :: Selector '[] CInt
trackIDSelector = mkSelector "trackID"

-- | @Selector@ for @setTrackID:@
setTrackIDSelector :: Selector '[CInt] ()
setTrackIDSelector = mkSelector "setTrackID:"

-- | @Selector@ for @audioTimePitchAlgorithm@
audioTimePitchAlgorithmSelector :: Selector '[] (Id NSString)
audioTimePitchAlgorithmSelector = mkSelector "audioTimePitchAlgorithm"

-- | @Selector@ for @setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithmSelector :: Selector '[Id NSString] ()
setAudioTimePitchAlgorithmSelector = mkSelector "setAudioTimePitchAlgorithm:"

-- | @Selector@ for @audioTapProcessor@
audioTapProcessorSelector :: Selector '[] RawId
audioTapProcessorSelector = mkSelector "audioTapProcessor"

-- | @Selector@ for @setAudioTapProcessor:@
setAudioTapProcessorSelector :: Selector '[RawId] ()
setAudioTapProcessorSelector = mkSelector "setAudioTapProcessor:"

