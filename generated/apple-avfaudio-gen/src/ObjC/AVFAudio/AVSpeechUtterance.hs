{-# LANGUAGE DataKinds #-}
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
  , attributedSpeechStringSelector
  , initWithAttributedStringSelector
  , initWithSSMLRepresentationSelector
  , initWithStringSelector
  , pitchMultiplierSelector
  , postUtteranceDelaySelector
  , preUtteranceDelaySelector
  , prefersAssistiveTechnologySettingsSelector
  , rateSelector
  , setPitchMultiplierSelector
  , setPostUtteranceDelaySelector
  , setPreUtteranceDelaySelector
  , setPrefersAssistiveTechnologySettingsSelector
  , setRateSelector
  , setVoiceSelector
  , setVolumeSelector
  , speechStringSelector
  , speechUtteranceWithAttributedStringSelector
  , speechUtteranceWithSSMLRepresentationSelector
  , speechUtteranceWithStringSelector
  , voiceSelector
  , volumeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ speechUtteranceWithString:@
speechUtteranceWithString :: IsNSString string => string -> IO (Id AVSpeechUtterance)
speechUtteranceWithString string =
  do
    cls' <- getRequiredClass "AVSpeechUtterance"
    sendClassMessage cls' speechUtteranceWithStringSelector (toNSString string)

-- | @+ speechUtteranceWithAttributedString:@
speechUtteranceWithAttributedString :: IsNSAttributedString string => string -> IO (Id AVSpeechUtterance)
speechUtteranceWithAttributedString string =
  do
    cls' <- getRequiredClass "AVSpeechUtterance"
    sendClassMessage cls' speechUtteranceWithAttributedStringSelector (toNSAttributedString string)

-- | A speech utterance that expects markup written using the Speech Synthesis Markup Language (SSML) standard. Returns nil if invalid SSML is passed in.
--
-- ObjC selector: @+ speechUtteranceWithSSMLRepresentation:@
speechUtteranceWithSSMLRepresentation :: IsNSString string => string -> IO (Id AVSpeechUtterance)
speechUtteranceWithSSMLRepresentation string =
  do
    cls' <- getRequiredClass "AVSpeechUtterance"
    sendClassMessage cls' speechUtteranceWithSSMLRepresentationSelector (toNSString string)

-- | @- initWithString:@
initWithString :: (IsAVSpeechUtterance avSpeechUtterance, IsNSString string) => avSpeechUtterance -> string -> IO (Id AVSpeechUtterance)
initWithString avSpeechUtterance string =
  sendOwnedMessage avSpeechUtterance initWithStringSelector (toNSString string)

-- | @- initWithAttributedString:@
initWithAttributedString :: (IsAVSpeechUtterance avSpeechUtterance, IsNSAttributedString string) => avSpeechUtterance -> string -> IO (Id AVSpeechUtterance)
initWithAttributedString avSpeechUtterance string =
  sendOwnedMessage avSpeechUtterance initWithAttributedStringSelector (toNSAttributedString string)

-- | A speech utterance that expects markup written using the Speech Synthesis Markup Language (SSML)  standard.
--
-- Uses SSML markup to add attributes. If using SSML to request voices that fall under certain attributes, a single utterance may be split into multiple parts, each sent to the appropriate synthesizer. If no voice matches the properties, the voice in the @voice@ property of the utterance will be used. If no @voice@ is specified, the system's default will be used. @AVSpeechUtterance@ properties that affect the prosidy of a voice such as @rate,@ @pitchMultiplier,@ @pitchMultiplier@ will not apply to an utterance that uses an SSML representation.
--
-- Returns nil if invalid SSML is passed in.
--
-- ObjC selector: @- initWithSSMLRepresentation:@
initWithSSMLRepresentation :: (IsAVSpeechUtterance avSpeechUtterance, IsNSString string) => avSpeechUtterance -> string -> IO (Id AVSpeechUtterance)
initWithSSMLRepresentation avSpeechUtterance string =
  sendOwnedMessage avSpeechUtterance initWithSSMLRepresentationSelector (toNSString string)

-- | @- voice@
voice :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO (Id AVSpeechSynthesisVoice)
voice avSpeechUtterance =
  sendMessage avSpeechUtterance voiceSelector

-- | @- setVoice:@
setVoice :: (IsAVSpeechUtterance avSpeechUtterance, IsAVSpeechSynthesisVoice value) => avSpeechUtterance -> value -> IO ()
setVoice avSpeechUtterance value =
  sendMessage avSpeechUtterance setVoiceSelector (toAVSpeechSynthesisVoice value)

-- | @- speechString@
speechString :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO (Id NSString)
speechString avSpeechUtterance =
  sendMessage avSpeechUtterance speechStringSelector

-- | @- attributedSpeechString@
attributedSpeechString :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO (Id NSAttributedString)
attributedSpeechString avSpeechUtterance =
  sendMessage avSpeechUtterance attributedSpeechStringSelector

-- | @- rate@
rate :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO CFloat
rate avSpeechUtterance =
  sendMessage avSpeechUtterance rateSelector

-- | @- setRate:@
setRate :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> CFloat -> IO ()
setRate avSpeechUtterance value =
  sendMessage avSpeechUtterance setRateSelector value

-- | @- pitchMultiplier@
pitchMultiplier :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO CFloat
pitchMultiplier avSpeechUtterance =
  sendMessage avSpeechUtterance pitchMultiplierSelector

-- | @- setPitchMultiplier:@
setPitchMultiplier :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> CFloat -> IO ()
setPitchMultiplier avSpeechUtterance value =
  sendMessage avSpeechUtterance setPitchMultiplierSelector value

-- | @- volume@
volume :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO CFloat
volume avSpeechUtterance =
  sendMessage avSpeechUtterance volumeSelector

-- | @- setVolume:@
setVolume :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> CFloat -> IO ()
setVolume avSpeechUtterance value =
  sendMessage avSpeechUtterance setVolumeSelector value

-- | @- prefersAssistiveTechnologySettings@
prefersAssistiveTechnologySettings :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO Bool
prefersAssistiveTechnologySettings avSpeechUtterance =
  sendMessage avSpeechUtterance prefersAssistiveTechnologySettingsSelector

-- | @- setPrefersAssistiveTechnologySettings:@
setPrefersAssistiveTechnologySettings :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> Bool -> IO ()
setPrefersAssistiveTechnologySettings avSpeechUtterance value =
  sendMessage avSpeechUtterance setPrefersAssistiveTechnologySettingsSelector value

-- | @- preUtteranceDelay@
preUtteranceDelay :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO CDouble
preUtteranceDelay avSpeechUtterance =
  sendMessage avSpeechUtterance preUtteranceDelaySelector

-- | @- setPreUtteranceDelay:@
setPreUtteranceDelay :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> CDouble -> IO ()
setPreUtteranceDelay avSpeechUtterance value =
  sendMessage avSpeechUtterance setPreUtteranceDelaySelector value

-- | @- postUtteranceDelay@
postUtteranceDelay :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> IO CDouble
postUtteranceDelay avSpeechUtterance =
  sendMessage avSpeechUtterance postUtteranceDelaySelector

-- | @- setPostUtteranceDelay:@
setPostUtteranceDelay :: IsAVSpeechUtterance avSpeechUtterance => avSpeechUtterance -> CDouble -> IO ()
setPostUtteranceDelay avSpeechUtterance value =
  sendMessage avSpeechUtterance setPostUtteranceDelaySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @speechUtteranceWithString:@
speechUtteranceWithStringSelector :: Selector '[Id NSString] (Id AVSpeechUtterance)
speechUtteranceWithStringSelector = mkSelector "speechUtteranceWithString:"

-- | @Selector@ for @speechUtteranceWithAttributedString:@
speechUtteranceWithAttributedStringSelector :: Selector '[Id NSAttributedString] (Id AVSpeechUtterance)
speechUtteranceWithAttributedStringSelector = mkSelector "speechUtteranceWithAttributedString:"

-- | @Selector@ for @speechUtteranceWithSSMLRepresentation:@
speechUtteranceWithSSMLRepresentationSelector :: Selector '[Id NSString] (Id AVSpeechUtterance)
speechUtteranceWithSSMLRepresentationSelector = mkSelector "speechUtteranceWithSSMLRepresentation:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] (Id AVSpeechUtterance)
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @initWithAttributedString:@
initWithAttributedStringSelector :: Selector '[Id NSAttributedString] (Id AVSpeechUtterance)
initWithAttributedStringSelector = mkSelector "initWithAttributedString:"

-- | @Selector@ for @initWithSSMLRepresentation:@
initWithSSMLRepresentationSelector :: Selector '[Id NSString] (Id AVSpeechUtterance)
initWithSSMLRepresentationSelector = mkSelector "initWithSSMLRepresentation:"

-- | @Selector@ for @voice@
voiceSelector :: Selector '[] (Id AVSpeechSynthesisVoice)
voiceSelector = mkSelector "voice"

-- | @Selector@ for @setVoice:@
setVoiceSelector :: Selector '[Id AVSpeechSynthesisVoice] ()
setVoiceSelector = mkSelector "setVoice:"

-- | @Selector@ for @speechString@
speechStringSelector :: Selector '[] (Id NSString)
speechStringSelector = mkSelector "speechString"

-- | @Selector@ for @attributedSpeechString@
attributedSpeechStringSelector :: Selector '[] (Id NSAttributedString)
attributedSpeechStringSelector = mkSelector "attributedSpeechString"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CFloat
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector '[CFloat] ()
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @pitchMultiplier@
pitchMultiplierSelector :: Selector '[] CFloat
pitchMultiplierSelector = mkSelector "pitchMultiplier"

-- | @Selector@ for @setPitchMultiplier:@
setPitchMultiplierSelector :: Selector '[CFloat] ()
setPitchMultiplierSelector = mkSelector "setPitchMultiplier:"

-- | @Selector@ for @volume@
volumeSelector :: Selector '[] CFloat
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector '[CFloat] ()
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @prefersAssistiveTechnologySettings@
prefersAssistiveTechnologySettingsSelector :: Selector '[] Bool
prefersAssistiveTechnologySettingsSelector = mkSelector "prefersAssistiveTechnologySettings"

-- | @Selector@ for @setPrefersAssistiveTechnologySettings:@
setPrefersAssistiveTechnologySettingsSelector :: Selector '[Bool] ()
setPrefersAssistiveTechnologySettingsSelector = mkSelector "setPrefersAssistiveTechnologySettings:"

-- | @Selector@ for @preUtteranceDelay@
preUtteranceDelaySelector :: Selector '[] CDouble
preUtteranceDelaySelector = mkSelector "preUtteranceDelay"

-- | @Selector@ for @setPreUtteranceDelay:@
setPreUtteranceDelaySelector :: Selector '[CDouble] ()
setPreUtteranceDelaySelector = mkSelector "setPreUtteranceDelay:"

-- | @Selector@ for @postUtteranceDelay@
postUtteranceDelaySelector :: Selector '[] CDouble
postUtteranceDelaySelector = mkSelector "postUtteranceDelay"

-- | @Selector@ for @setPostUtteranceDelay:@
setPostUtteranceDelaySelector :: Selector '[CDouble] ()
setPostUtteranceDelaySelector = mkSelector "setPostUtteranceDelay:"

