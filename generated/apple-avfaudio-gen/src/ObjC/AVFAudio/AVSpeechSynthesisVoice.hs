{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVSpeechSynthesisVoice
--
-- AVSpeechSynthesisVoice encapsulates the attributes of the voice used to synthesize speech on the system.
--
-- Retrieve a voice by specifying the language code your text should be spoken in, or by using voiceWithIdentifier for a known voice identifier.
--
-- Generated bindings for @AVSpeechSynthesisVoice@.
module ObjC.AVFAudio.AVSpeechSynthesisVoice
  ( AVSpeechSynthesisVoice
  , IsAVSpeechSynthesisVoice(..)
  , speechVoices
  , currentLanguageCode
  , voiceWithLanguage
  , voiceWithIdentifier
  , language
  , identifier
  , name
  , quality
  , gender
  , audioFileSettings
  , voiceTraits
  , audioFileSettingsSelector
  , currentLanguageCodeSelector
  , genderSelector
  , identifierSelector
  , languageSelector
  , nameSelector
  , qualitySelector
  , speechVoicesSelector
  , voiceTraitsSelector
  , voiceWithIdentifierSelector
  , voiceWithLanguageSelector

  -- * Enum types
  , AVSpeechSynthesisVoiceGender(AVSpeechSynthesisVoiceGender)
  , pattern AVSpeechSynthesisVoiceGenderUnspecified
  , pattern AVSpeechSynthesisVoiceGenderMale
  , pattern AVSpeechSynthesisVoiceGenderFemale
  , AVSpeechSynthesisVoiceQuality(AVSpeechSynthesisVoiceQuality)
  , pattern AVSpeechSynthesisVoiceQualityDefault
  , pattern AVSpeechSynthesisVoiceQualityEnhanced
  , pattern AVSpeechSynthesisVoiceQualityPremium
  , AVSpeechSynthesisVoiceTraits(AVSpeechSynthesisVoiceTraits)
  , pattern AVSpeechSynthesisVoiceTraitNone
  , pattern AVSpeechSynthesisVoiceTraitIsNoveltyVoice
  , pattern AVSpeechSynthesisVoiceTraitIsPersonalVoice

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ speechVoices@
speechVoices :: IO (Id NSArray)
speechVoices  =
  do
    cls' <- getRequiredClass "AVSpeechSynthesisVoice"
    sendClassMessage cls' speechVoicesSelector

-- | @+ currentLanguageCode@
currentLanguageCode :: IO (Id NSString)
currentLanguageCode  =
  do
    cls' <- getRequiredClass "AVSpeechSynthesisVoice"
    sendClassMessage cls' currentLanguageCodeSelector

-- | voiceWithLanguage:
--
-- Use a BCP-47 language tag to specify the desired language and region.
--
-- @languageCode@ — Specifies the BCP-47 language tag that represents the voice.
--
-- The default is the system's region and language. Passing in nil will return the default voice. Passing in an invalid languageCode will return nil. Will return enhanced quality voice if available, default quality otherwise. Examples: en-US (U.S. English), fr-CA (French Canadian)
--
-- ObjC selector: @+ voiceWithLanguage:@
voiceWithLanguage :: IsNSString languageCode => languageCode -> IO (Id AVSpeechSynthesisVoice)
voiceWithLanguage languageCode =
  do
    cls' <- getRequiredClass "AVSpeechSynthesisVoice"
    sendClassMessage cls' voiceWithLanguageSelector (toNSString languageCode)

-- | voiceWithIdentifier:
--
-- Retrieve a voice by its identifier.
--
-- @identifier@ — A unique identifier for a voice.
--
-- Passing in an invalid identifier will return nil. Returns nil if the identifier is valid, but the voice is not available on device (i.e. not yet downloaded by the user).
--
-- ObjC selector: @+ voiceWithIdentifier:@
voiceWithIdentifier :: IsNSString identifier => identifier -> IO (Id AVSpeechSynthesisVoice)
voiceWithIdentifier identifier =
  do
    cls' <- getRequiredClass "AVSpeechSynthesisVoice"
    sendClassMessage cls' voiceWithIdentifierSelector (toNSString identifier)

-- | @- language@
language :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO (Id NSString)
language avSpeechSynthesisVoice =
  sendMessage avSpeechSynthesisVoice languageSelector

-- | @- identifier@
identifier :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO (Id NSString)
identifier avSpeechSynthesisVoice =
  sendMessage avSpeechSynthesisVoice identifierSelector

-- | @- name@
name :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO (Id NSString)
name avSpeechSynthesisVoice =
  sendMessage avSpeechSynthesisVoice nameSelector

-- | @- quality@
quality :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO AVSpeechSynthesisVoiceQuality
quality avSpeechSynthesisVoice =
  sendMessage avSpeechSynthesisVoice qualitySelector

-- | @- gender@
gender :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO AVSpeechSynthesisVoiceGender
gender avSpeechSynthesisVoice =
  sendMessage avSpeechSynthesisVoice genderSelector

-- | @- audioFileSettings@
audioFileSettings :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO (Id NSDictionary)
audioFileSettings avSpeechSynthesisVoice =
  sendMessage avSpeechSynthesisVoice audioFileSettingsSelector

-- | @- voiceTraits@
voiceTraits :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO AVSpeechSynthesisVoiceTraits
voiceTraits avSpeechSynthesisVoice =
  sendMessage avSpeechSynthesisVoice voiceTraitsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @speechVoices@
speechVoicesSelector :: Selector '[] (Id NSArray)
speechVoicesSelector = mkSelector "speechVoices"

-- | @Selector@ for @currentLanguageCode@
currentLanguageCodeSelector :: Selector '[] (Id NSString)
currentLanguageCodeSelector = mkSelector "currentLanguageCode"

-- | @Selector@ for @voiceWithLanguage:@
voiceWithLanguageSelector :: Selector '[Id NSString] (Id AVSpeechSynthesisVoice)
voiceWithLanguageSelector = mkSelector "voiceWithLanguage:"

-- | @Selector@ for @voiceWithIdentifier:@
voiceWithIdentifierSelector :: Selector '[Id NSString] (Id AVSpeechSynthesisVoice)
voiceWithIdentifierSelector = mkSelector "voiceWithIdentifier:"

-- | @Selector@ for @language@
languageSelector :: Selector '[] (Id NSString)
languageSelector = mkSelector "language"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @quality@
qualitySelector :: Selector '[] AVSpeechSynthesisVoiceQuality
qualitySelector = mkSelector "quality"

-- | @Selector@ for @gender@
genderSelector :: Selector '[] AVSpeechSynthesisVoiceGender
genderSelector = mkSelector "gender"

-- | @Selector@ for @audioFileSettings@
audioFileSettingsSelector :: Selector '[] (Id NSDictionary)
audioFileSettingsSelector = mkSelector "audioFileSettings"

-- | @Selector@ for @voiceTraits@
voiceTraitsSelector :: Selector '[] AVSpeechSynthesisVoiceTraits
voiceTraitsSelector = mkSelector "voiceTraits"

