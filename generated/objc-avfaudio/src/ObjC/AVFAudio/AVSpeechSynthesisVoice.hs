{-# LANGUAGE PatternSynonyms #-}
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
  , speechVoicesSelector
  , currentLanguageCodeSelector
  , voiceWithLanguageSelector
  , voiceWithIdentifierSelector
  , languageSelector
  , identifierSelector
  , nameSelector
  , qualitySelector
  , genderSelector
  , audioFileSettingsSelector
  , voiceTraitsSelector

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
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ speechVoices@
speechVoices :: IO (Id NSArray)
speechVoices  =
  do
    cls' <- getRequiredClass "AVSpeechSynthesisVoice"
    sendClassMsg cls' (mkSelector "speechVoices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ currentLanguageCode@
currentLanguageCode :: IO (Id NSString)
currentLanguageCode  =
  do
    cls' <- getRequiredClass "AVSpeechSynthesisVoice"
    sendClassMsg cls' (mkSelector "currentLanguageCode") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    withObjCPtr languageCode $ \raw_languageCode ->
      sendClassMsg cls' (mkSelector "voiceWithLanguage:") (retPtr retVoid) [argPtr (castPtr raw_languageCode :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "voiceWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- language@
language :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO (Id NSString)
language avSpeechSynthesisVoice  =
  sendMsg avSpeechSynthesisVoice (mkSelector "language") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO (Id NSString)
identifier avSpeechSynthesisVoice  =
  sendMsg avSpeechSynthesisVoice (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO (Id NSString)
name avSpeechSynthesisVoice  =
  sendMsg avSpeechSynthesisVoice (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- quality@
quality :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO AVSpeechSynthesisVoiceQuality
quality avSpeechSynthesisVoice  =
  fmap (coerce :: CLong -> AVSpeechSynthesisVoiceQuality) $ sendMsg avSpeechSynthesisVoice (mkSelector "quality") retCLong []

-- | @- gender@
gender :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO AVSpeechSynthesisVoiceGender
gender avSpeechSynthesisVoice  =
  fmap (coerce :: CLong -> AVSpeechSynthesisVoiceGender) $ sendMsg avSpeechSynthesisVoice (mkSelector "gender") retCLong []

-- | @- audioFileSettings@
audioFileSettings :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO (Id NSDictionary)
audioFileSettings avSpeechSynthesisVoice  =
  sendMsg avSpeechSynthesisVoice (mkSelector "audioFileSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- voiceTraits@
voiceTraits :: IsAVSpeechSynthesisVoice avSpeechSynthesisVoice => avSpeechSynthesisVoice -> IO AVSpeechSynthesisVoiceTraits
voiceTraits avSpeechSynthesisVoice  =
  fmap (coerce :: CULong -> AVSpeechSynthesisVoiceTraits) $ sendMsg avSpeechSynthesisVoice (mkSelector "voiceTraits") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @speechVoices@
speechVoicesSelector :: Selector
speechVoicesSelector = mkSelector "speechVoices"

-- | @Selector@ for @currentLanguageCode@
currentLanguageCodeSelector :: Selector
currentLanguageCodeSelector = mkSelector "currentLanguageCode"

-- | @Selector@ for @voiceWithLanguage:@
voiceWithLanguageSelector :: Selector
voiceWithLanguageSelector = mkSelector "voiceWithLanguage:"

-- | @Selector@ for @voiceWithIdentifier:@
voiceWithIdentifierSelector :: Selector
voiceWithIdentifierSelector = mkSelector "voiceWithIdentifier:"

-- | @Selector@ for @language@
languageSelector :: Selector
languageSelector = mkSelector "language"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @quality@
qualitySelector :: Selector
qualitySelector = mkSelector "quality"

-- | @Selector@ for @gender@
genderSelector :: Selector
genderSelector = mkSelector "gender"

-- | @Selector@ for @audioFileSettings@
audioFileSettingsSelector :: Selector
audioFileSettingsSelector = mkSelector "audioFileSettings"

-- | @Selector@ for @voiceTraits@
voiceTraitsSelector :: Selector
voiceTraitsSelector = mkSelector "voiceTraits"

