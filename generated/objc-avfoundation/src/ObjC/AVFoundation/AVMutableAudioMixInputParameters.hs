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
  , audioMixInputParametersWithTrackSelector
  , audioMixInputParametersSelector
  , trackIDSelector
  , setTrackIDSelector
  , audioTimePitchAlgorithmSelector
  , setAudioTimePitchAlgorithmSelector
  , audioTapProcessorSelector
  , setAudioTapProcessorSelector


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

-- | @+ audioMixInputParametersWithTrack:@
audioMixInputParametersWithTrack :: IsAVAssetTrack track => track -> IO (Id AVMutableAudioMixInputParameters)
audioMixInputParametersWithTrack track =
  do
    cls' <- getRequiredClass "AVMutableAudioMixInputParameters"
    withObjCPtr track $ \raw_track ->
      sendClassMsg cls' (mkSelector "audioMixInputParametersWithTrack:") (retPtr retVoid) [argPtr (castPtr raw_track :: Ptr ())] >>= retainedObject . castPtr

-- | @+ audioMixInputParameters@
audioMixInputParameters :: IO (Id AVMutableAudioMixInputParameters)
audioMixInputParameters  =
  do
    cls' <- getRequiredClass "AVMutableAudioMixInputParameters"
    sendClassMsg cls' (mkSelector "audioMixInputParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | trackID
--
-- Indicates the trackID of the audio track to which the parameters should be applied.
--
-- ObjC selector: @- trackID@
trackID :: IsAVMutableAudioMixInputParameters avMutableAudioMixInputParameters => avMutableAudioMixInputParameters -> IO CInt
trackID avMutableAudioMixInputParameters  =
  sendMsg avMutableAudioMixInputParameters (mkSelector "trackID") retCInt []

-- | trackID
--
-- Indicates the trackID of the audio track to which the parameters should be applied.
--
-- ObjC selector: @- setTrackID:@
setTrackID :: IsAVMutableAudioMixInputParameters avMutableAudioMixInputParameters => avMutableAudioMixInputParameters -> CInt -> IO ()
setTrackID avMutableAudioMixInputParameters  value =
  sendMsg avMutableAudioMixInputParameters (mkSelector "setTrackID:") retVoid [argCInt (fromIntegral value)]

-- | audioTimePitchAlgorithm
--
-- Indicates the processing algorithm used to manage audio pitch at varying rates and for scaled audio edits.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchSpectral, are defined in AVAudioProcessingSettings.h.   Can be nil, in which case the audioTimePitchAlgorithm set on the AVPlayerItem, AVAssetExportSession, or AVAssetReaderAudioMixOutput on which the AVAudioMix is set will be used for the associated track.
--
-- ObjC selector: @- audioTimePitchAlgorithm@
audioTimePitchAlgorithm :: IsAVMutableAudioMixInputParameters avMutableAudioMixInputParameters => avMutableAudioMixInputParameters -> IO (Id NSString)
audioTimePitchAlgorithm avMutableAudioMixInputParameters  =
  sendMsg avMutableAudioMixInputParameters (mkSelector "audioTimePitchAlgorithm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | audioTimePitchAlgorithm
--
-- Indicates the processing algorithm used to manage audio pitch at varying rates and for scaled audio edits.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchSpectral, are defined in AVAudioProcessingSettings.h.   Can be nil, in which case the audioTimePitchAlgorithm set on the AVPlayerItem, AVAssetExportSession, or AVAssetReaderAudioMixOutput on which the AVAudioMix is set will be used for the associated track.
--
-- ObjC selector: @- setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithm :: (IsAVMutableAudioMixInputParameters avMutableAudioMixInputParameters, IsNSString value) => avMutableAudioMixInputParameters -> value -> IO ()
setAudioTimePitchAlgorithm avMutableAudioMixInputParameters  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableAudioMixInputParameters (mkSelector "setAudioTimePitchAlgorithm:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | audioTapProcessor
--
-- Indicates the audio processing tap that will be used for the audio track.
--
-- ObjC selector: @- audioTapProcessor@
audioTapProcessor :: IsAVMutableAudioMixInputParameters avMutableAudioMixInputParameters => avMutableAudioMixInputParameters -> IO RawId
audioTapProcessor avMutableAudioMixInputParameters  =
  fmap (RawId . castPtr) $ sendMsg avMutableAudioMixInputParameters (mkSelector "audioTapProcessor") (retPtr retVoid) []

-- | audioTapProcessor
--
-- Indicates the audio processing tap that will be used for the audio track.
--
-- ObjC selector: @- setAudioTapProcessor:@
setAudioTapProcessor :: IsAVMutableAudioMixInputParameters avMutableAudioMixInputParameters => avMutableAudioMixInputParameters -> RawId -> IO ()
setAudioTapProcessor avMutableAudioMixInputParameters  value =
  sendMsg avMutableAudioMixInputParameters (mkSelector "setAudioTapProcessor:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @audioMixInputParametersWithTrack:@
audioMixInputParametersWithTrackSelector :: Selector
audioMixInputParametersWithTrackSelector = mkSelector "audioMixInputParametersWithTrack:"

-- | @Selector@ for @audioMixInputParameters@
audioMixInputParametersSelector :: Selector
audioMixInputParametersSelector = mkSelector "audioMixInputParameters"

-- | @Selector@ for @trackID@
trackIDSelector :: Selector
trackIDSelector = mkSelector "trackID"

-- | @Selector@ for @setTrackID:@
setTrackIDSelector :: Selector
setTrackIDSelector = mkSelector "setTrackID:"

-- | @Selector@ for @audioTimePitchAlgorithm@
audioTimePitchAlgorithmSelector :: Selector
audioTimePitchAlgorithmSelector = mkSelector "audioTimePitchAlgorithm"

-- | @Selector@ for @setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithmSelector :: Selector
setAudioTimePitchAlgorithmSelector = mkSelector "setAudioTimePitchAlgorithm:"

-- | @Selector@ for @audioTapProcessor@
audioTapProcessorSelector :: Selector
audioTapProcessorSelector = mkSelector "audioTapProcessor"

-- | @Selector@ for @setAudioTapProcessor:@
setAudioTapProcessorSelector :: Selector
setAudioTapProcessorSelector = mkSelector "setAudioTapProcessor:"

