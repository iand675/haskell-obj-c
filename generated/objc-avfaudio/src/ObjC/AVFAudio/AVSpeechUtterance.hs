{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVSpeechUtterance
--
-- AVSpeechUtterance is the atom of speaking a string or pausing the synthesizer.
--
-- To start speaking, specify the AVSpeechSynthesisVoice and the string to be spoken, then optionally change the rate, pitch or volume if desired.
--
-- Generated bindings for @AVSpeechUtterance@.
module ObjC.AVFAudio.AVSpeechUtterance
  ( AVSpeechUtterance
  , IsAVSpeechUtterance(..)
  , speechUtteranceWithString
  , speechUtteranceWithAttributedString
  , speechUtteranceWithSSMLRepresentation
  , initWithString
  , initWithAttributedString
  , initWithSSMLRepresentation
  , voice
  , setVoice
  , speechString
  , attributedSpeechString
  , rate
  , setRate
  , pitchMultiplier
  , setPitchMultiplier
  , volume
  , setVolume
  , prefersAssistiveTechnologySettings
  , setPrefersAssistiveTechnologySettings
  , preUtteranceDelay
  , setPreUtteranceDelay
  , postUtteranceDelay
  , setPostUtteranceDelay
  , speechUtteranceWithStringSelector
  , speechUtteranceWithAttributedStringSelector
  , speechUtteranceWithSSMLRepresentationSelector
  , initWithStringSelector
  , initWithAttributedStringSelector
  , initWithSSMLRepresentationSelector
  , voiceSelector
  , setVoiceSelector
  , speechStringSelector
  , attributedSpeechStringSelector
  , rateSelector
  , setRateSelector
  , pitchMultiplierSelector
  , setPitchMultiplierSelector
  , volumeSelector
  , setVolumeSelector
  , prefersAssistiveTechnologySettingsSelector
  , setPrefersAssistiveTechnologySettingsSelector
  , preUtteranceDelaySelector
  , setPreUtteranceDelaySelector
  , postUtteranceDelaySelector
  , setPostUtteranceDelaySelector


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

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ speechUtteranceWithString:@
speechUtteranceWithString :: IsNSString string => string -> IO (Id AVSpeechUtterance)
speechUtteranceWithString string =
  do
    cls' <- getRequiredClass "AVSpeechUtterance"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "speechUtteranceWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ speechUtteranceWithAttributedString:@
speechUtteranceWithAttributedString :: IsNSAttributedString string => string -> IO (Id AVSpeechUtterance)
speechUtteranceWithAttributedString string =
  do
    cls' <- getRequiredClass "AVSpeechUtterance"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "speechUtteranceWithAttributedString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | A speech utterance that expects markup written using the Speech Synthesis Markup Language (SSML) standard. Returns nil if invalid SSML is passed in.
--
-- ObjC selector: @+ speechUtteranceWithSSMLRepresentation:@
speechUtteranceWithSSMLRepresentation :: IsNSString string => string -> IO (Id AVSpeechUtterance)
speechUtteranceWithSSMLRepresentation string =
  do
    cls' <- getRequiredClass "AVSpeechUtterance"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "speechUtteranceWithSSMLRepresentation:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithString:@
initWithString :: (IsAVSpeechUtterance avSpeechUtterance, IsNSString string) => avSpeechUtterance -> string -> IO (Id AVSpeechUtterance)
initWithString avSpeechUtterance  string =
withObjCPtr string $ \raw_string ->
    sendMsg avSpeechUtterance (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithAttributedString:@
initWithAttributedString :: (IsAVSpeechUtterance avSpeechUtterance, IsNSAttributedString string) => avSpeechUtterance -> string -> IO (Id AVSpeechUtterance)
initWithAttributedString avSpeechUtterance  string =
withObjCPtr string $ \raw_string ->
    sendMsg avSpeechUtterance (mkSelector "initWithAttributedString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | A speech utterance that expects markup written using the Speech Synthesis Markup Language (SSML)  standard.
--
-- Uses SSML markup to add attributes. If using SSML to request voices that fall under certain attributes, a single utterance may be split into multiple parts, each sent to the appropriate synthesizer. If no voice matches the properties, the voice in the @voice@ property of the utterance will be used. If no @voice@ is specified, the system's default will be used. @AVSpeechUtterance@ properties that affect the prosidy of a voice such as @rate,@ @pitchMultiplier,@ @pitchMultiplier@ will not apply to an utterance that uses an SSML representation.
--
-- Returns nil if invalid SSML is passed in.
--
-- ObjC selector: @- initWithSSMLRepresentation:@
initWithSSMLRepresentation :: (IsAVSpeechUtterance avSpeechUtterance, IsNSString string) => avSpeechUtterance -> string -> IO (Id AVSpeechUtterance)
initWithSSMLRepresentation avSpeechUtterance  string =
withObjCPtr string $ \raw_string ->
    sendMsg avSpeechUtterance (mkSelector "initWithSSMLRepresentation:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- voice@
voice :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO (Id AVSpeechSynthesisVoice)
voice avSpeechUtterance  =
  sendMsg avSpeechUtterance (mkSelector "voice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVoice:@
setVoice :: (IsAVSpeechUtterance avSpeechUtterance, IsAVSpeechSynthesisVoice value) => avSpeechUtterance -> value -> IO ()
setVoice avSpeechUtterance  value =
withObjCPtr value $ \raw_value ->
    sendMsg avSpeechUtterance (mkSelector "setVoice:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- speechString@
speechString :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO (Id NSString)
speechString avSpeechUtterance  =
  sendMsg avSpeechUtterance (mkSelector "speechString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributedSpeechString@
attributedSpeechString :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO (Id NSAttributedString)
attributedSpeechString avSpeechUtterance  =
  sendMsg avSpeechUtterance (mkSelector "attributedSpeechString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rate@
rate :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO CFloat
rate avSpeechUtterance  =
  sendMsg avSpeechUtterance (mkSelector "rate") retCFloat []

-- | @- setRate:@
setRate :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> CFloat -> IO ()
setRate avSpeechUtterance  value =
  sendMsg avSpeechUtterance (mkSelector "setRate:") retVoid [argCFloat (fromIntegral value)]

-- | @- pitchMultiplier@
pitchMultiplier :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO CFloat
pitchMultiplier avSpeechUtterance  =
  sendMsg avSpeechUtterance (mkSelector "pitchMultiplier") retCFloat []

-- | @- setPitchMultiplier:@
setPitchMultiplier :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> CFloat -> IO ()
setPitchMultiplier avSpeechUtterance  value =
  sendMsg avSpeechUtterance (mkSelector "setPitchMultiplier:") retVoid [argCFloat (fromIntegral value)]

-- | @- volume@
volume :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO CFloat
volume avSpeechUtterance  =
  sendMsg avSpeechUtterance (mkSelector "volume") retCFloat []

-- | @- setVolume:@
setVolume :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> CFloat -> IO ()
setVolume avSpeechUtterance  value =
  sendMsg avSpeechUtterance (mkSelector "setVolume:") retVoid [argCFloat (fromIntegral value)]

-- | @- prefersAssistiveTechnologySettings@
prefersAssistiveTechnologySettings :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO Bool
prefersAssistiveTechnologySettings avSpeechUtterance  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSpeechUtterance (mkSelector "prefersAssistiveTechnologySettings") retCULong []

-- | @- setPrefersAssistiveTechnologySettings:@
setPrefersAssistiveTechnologySettings :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> Bool -> IO ()
setPrefersAssistiveTechnologySettings avSpeechUtterance  value =
  sendMsg avSpeechUtterance (mkSelector "setPrefersAssistiveTechnologySettings:") retVoid [argCULong (if value then 1 else 0)]

-- | @- preUtteranceDelay@
preUtteranceDelay :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO CDouble
preUtteranceDelay avSpeechUtterance  =
  sendMsg avSpeechUtterance (mkSelector "preUtteranceDelay") retCDouble []

-- | @- setPreUtteranceDelay:@
setPreUtteranceDelay :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> CDouble -> IO ()
setPreUtteranceDelay avSpeechUtterance  value =
  sendMsg avSpeechUtterance (mkSelector "setPreUtteranceDelay:") retVoid [argCDouble (fromIntegral value)]

-- | @- postUtteranceDelay@
postUtteranceDelay :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO CDouble
postUtteranceDelay avSpeechUtterance  =
  sendMsg avSpeechUtterance (mkSelector "postUtteranceDelay") retCDouble []

-- | @- setPostUtteranceDelay:@
setPostUtteranceDelay :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> CDouble -> IO ()
setPostUtteranceDelay avSpeechUtterance  value =
  sendMsg avSpeechUtterance (mkSelector "setPostUtteranceDelay:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @speechUtteranceWithString:@
speechUtteranceWithStringSelector :: Selector
speechUtteranceWithStringSelector = mkSelector "speechUtteranceWithString:"

-- | @Selector@ for @speechUtteranceWithAttributedString:@
speechUtteranceWithAttributedStringSelector :: Selector
speechUtteranceWithAttributedStringSelector = mkSelector "speechUtteranceWithAttributedString:"

-- | @Selector@ for @speechUtteranceWithSSMLRepresentation:@
speechUtteranceWithSSMLRepresentationSelector :: Selector
speechUtteranceWithSSMLRepresentationSelector = mkSelector "speechUtteranceWithSSMLRepresentation:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @initWithAttributedString:@
initWithAttributedStringSelector :: Selector
initWithAttributedStringSelector = mkSelector "initWithAttributedString:"

-- | @Selector@ for @initWithSSMLRepresentation:@
initWithSSMLRepresentationSelector :: Selector
initWithSSMLRepresentationSelector = mkSelector "initWithSSMLRepresentation:"

-- | @Selector@ for @voice@
voiceSelector :: Selector
voiceSelector = mkSelector "voice"

-- | @Selector@ for @setVoice:@
setVoiceSelector :: Selector
setVoiceSelector = mkSelector "setVoice:"

-- | @Selector@ for @speechString@
speechStringSelector :: Selector
speechStringSelector = mkSelector "speechString"

-- | @Selector@ for @attributedSpeechString@
attributedSpeechStringSelector :: Selector
attributedSpeechStringSelector = mkSelector "attributedSpeechString"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @pitchMultiplier@
pitchMultiplierSelector :: Selector
pitchMultiplierSelector = mkSelector "pitchMultiplier"

-- | @Selector@ for @setPitchMultiplier:@
setPitchMultiplierSelector :: Selector
setPitchMultiplierSelector = mkSelector "setPitchMultiplier:"

-- | @Selector@ for @volume@
volumeSelector :: Selector
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @prefersAssistiveTechnologySettings@
prefersAssistiveTechnologySettingsSelector :: Selector
prefersAssistiveTechnologySettingsSelector = mkSelector "prefersAssistiveTechnologySettings"

-- | @Selector@ for @setPrefersAssistiveTechnologySettings:@
setPrefersAssistiveTechnologySettingsSelector :: Selector
setPrefersAssistiveTechnologySettingsSelector = mkSelector "setPrefersAssistiveTechnologySettings:"

-- | @Selector@ for @preUtteranceDelay@
preUtteranceDelaySelector :: Selector
preUtteranceDelaySelector = mkSelector "preUtteranceDelay"

-- | @Selector@ for @setPreUtteranceDelay:@
setPreUtteranceDelaySelector :: Selector
setPreUtteranceDelaySelector = mkSelector "setPreUtteranceDelay:"

-- | @Selector@ for @postUtteranceDelay@
postUtteranceDelaySelector :: Selector
postUtteranceDelaySelector = mkSelector "postUtteranceDelay"

-- | @Selector@ for @setPostUtteranceDelay:@
setPostUtteranceDelaySelector :: Selector
setPostUtteranceDelaySelector = mkSelector "setPostUtteranceDelay:"

