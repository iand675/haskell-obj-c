{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The representation of a provided voice that is available for speech synthesis.
--
-- @AVSpeechSynthesisProviderVoice@ is distinct from @AVSpeechSynthesisVoice,@ in that it is a voice provided to the system by an @AVSpeechSynthesisProviderAudioUnit.@         An @AVSpeechSynthesisProviderVoice@ will surface as an @AVSpeechSynthesisVoice@ when using @AVSpeechSynthesisVoice.speechVoices().@ The quality will always be listed as @.enhanced@
--
-- Generated bindings for @AVSpeechSynthesisProviderVoice@.
module ObjC.AVFAudio.AVSpeechSynthesisProviderVoice
  ( AVSpeechSynthesisProviderVoice
  , IsAVSpeechSynthesisProviderVoice(..)
  , initWithName_identifier_primaryLanguages_supportedLanguages
  , init_
  , new
  , updateSpeechVoices
  , name
  , identifier
  , primaryLanguages
  , supportedLanguages
  , voiceSize
  , setVoiceSize
  , version
  , setVersion
  , gender
  , setGender
  , age
  , setAge
  , ageSelector
  , genderSelector
  , identifierSelector
  , initSelector
  , initWithName_identifier_primaryLanguages_supportedLanguagesSelector
  , nameSelector
  , newSelector
  , primaryLanguagesSelector
  , setAgeSelector
  , setGenderSelector
  , setVersionSelector
  , setVoiceSizeSelector
  , supportedLanguagesSelector
  , updateSpeechVoicesSelector
  , versionSelector
  , voiceSizeSelector

  -- * Enum types
  , AVSpeechSynthesisVoiceGender(AVSpeechSynthesisVoiceGender)
  , pattern AVSpeechSynthesisVoiceGenderUnspecified
  , pattern AVSpeechSynthesisVoiceGenderMale
  , pattern AVSpeechSynthesisVoiceGenderFemale

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

-- | @- initWithName:identifier:primaryLanguages:supportedLanguages:@
initWithName_identifier_primaryLanguages_supportedLanguages :: (IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice, IsNSString name, IsNSString identifier, IsNSArray primaryLanguages, IsNSArray supportedLanguages) => avSpeechSynthesisProviderVoice -> name -> identifier -> primaryLanguages -> supportedLanguages -> IO (Id AVSpeechSynthesisProviderVoice)
initWithName_identifier_primaryLanguages_supportedLanguages avSpeechSynthesisProviderVoice name identifier primaryLanguages supportedLanguages =
  sendOwnedMessage avSpeechSynthesisProviderVoice initWithName_identifier_primaryLanguages_supportedLanguagesSelector (toNSString name) (toNSString identifier) (toNSArray primaryLanguages) (toNSArray supportedLanguages)

-- | @- init@
init_ :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO (Id AVSpeechSynthesisProviderVoice)
init_ avSpeechSynthesisProviderVoice =
  sendOwnedMessage avSpeechSynthesisProviderVoice initSelector

-- | @+ new@
new :: IO (Id AVSpeechSynthesisProviderVoice)
new  =
  do
    cls' <- getRequiredClass "AVSpeechSynthesisProviderVoice"
    sendOwnedClassMessage cls' newSelector

-- | A call that indicates that a new voice or set of voices is available, or no longer available, for system use.
--
-- Call this method to indicate to the system that there has been change in the availability of the voices your application is providing to the system.
--
-- ObjC selector: @+ updateSpeechVoices@
updateSpeechVoices :: IO ()
updateSpeechVoices  =
  do
    cls' <- getRequiredClass "AVSpeechSynthesisProviderVoice"
    sendClassMessage cls' updateSpeechVoicesSelector

-- | The localized name of the voice
--
-- ObjC selector: @- name@
name :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO (Id NSString)
name avSpeechSynthesisProviderVoice =
  sendMessage avSpeechSynthesisProviderVoice nameSelector

-- | A unique identifier for the voice
--
-- The recommended format is reverse domain notation.        Behavior is undefined if identifiers are not unique for all voices within a given extension.
--
-- ObjC selector: @- identifier@
identifier :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO (Id NSString)
identifier avSpeechSynthesisProviderVoice =
  sendMessage avSpeechSynthesisProviderVoice identifierSelector

-- | A set of BCP 47 codes identifying the languages this synthesizer is primarily used for.
--
-- These languages are what a user would expect a synthesizer to fully support and be primarily used for.
--
-- ObjC selector: @- primaryLanguages@
primaryLanguages :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO (Id NSArray)
primaryLanguages avSpeechSynthesisProviderVoice =
  sendMessage avSpeechSynthesisProviderVoice primaryLanguagesSelector

-- | A superset of BCP 47 codes identifying the voice’s supported languages.
--
-- These languages are what a user would expect a voice to be able to speak such that if the voice is given a multi-lingual phrase, it would be able to speak the entire phrase without a need to to switch voices. For example, a zh-CN voice could have @["zh-CN"]@ as its @primaryLanguages,@ but in @supportedLanguages@ have @["zh-CN","en-US"]@ indicating if it received "你好 means Hello", it would be able to speak the entire phrase.
--
-- ObjC selector: @- supportedLanguages@
supportedLanguages :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO (Id NSArray)
supportedLanguages avSpeechSynthesisProviderVoice =
  sendMessage avSpeechSynthesisProviderVoice supportedLanguagesSelector

-- | The size of the voice (optional)
--
-- This reported size of the voice package on disk, in bytes. Defaults to 0.
--
-- ObjC selector: @- voiceSize@
voiceSize :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO CLong
voiceSize avSpeechSynthesisProviderVoice =
  sendMessage avSpeechSynthesisProviderVoice voiceSizeSelector

-- | The size of the voice (optional)
--
-- This reported size of the voice package on disk, in bytes. Defaults to 0.
--
-- ObjC selector: @- setVoiceSize:@
setVoiceSize :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> CLong -> IO ()
setVoiceSize avSpeechSynthesisProviderVoice value =
  sendMessage avSpeechSynthesisProviderVoice setVoiceSizeSelector value

-- | The voice version (optional)
--
-- This is an optional property for bookkeeping. This value does not affect system behavior.
--
-- ObjC selector: @- version@
version :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO (Id NSString)
version avSpeechSynthesisProviderVoice =
  sendMessage avSpeechSynthesisProviderVoice versionSelector

-- | The voice version (optional)
--
-- This is an optional property for bookkeeping. This value does not affect system behavior.
--
-- ObjC selector: @- setVersion:@
setVersion :: (IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice, IsNSString value) => avSpeechSynthesisProviderVoice -> value -> IO ()
setVersion avSpeechSynthesisProviderVoice value =
  sendMessage avSpeechSynthesisProviderVoice setVersionSelector (toNSString value)

-- | The gender of the voice (optional)
--
-- ObjC selector: @- gender@
gender :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO AVSpeechSynthesisVoiceGender
gender avSpeechSynthesisProviderVoice =
  sendMessage avSpeechSynthesisProviderVoice genderSelector

-- | The gender of the voice (optional)
--
-- ObjC selector: @- setGender:@
setGender :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> AVSpeechSynthesisVoiceGender -> IO ()
setGender avSpeechSynthesisProviderVoice value =
  sendMessage avSpeechSynthesisProviderVoice setGenderSelector value

-- | The age of the voice in years (optional)
--
-- This is an optional property that indicates the age of this voice, to be treated as a personality trait. Defaults to 0.
--
-- ObjC selector: @- age@
age :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO CLong
age avSpeechSynthesisProviderVoice =
  sendMessage avSpeechSynthesisProviderVoice ageSelector

-- | The age of the voice in years (optional)
--
-- This is an optional property that indicates the age of this voice, to be treated as a personality trait. Defaults to 0.
--
-- ObjC selector: @- setAge:@
setAge :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> CLong -> IO ()
setAge avSpeechSynthesisProviderVoice value =
  sendMessage avSpeechSynthesisProviderVoice setAgeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:identifier:primaryLanguages:supportedLanguages:@
initWithName_identifier_primaryLanguages_supportedLanguagesSelector :: Selector '[Id NSString, Id NSString, Id NSArray, Id NSArray] (Id AVSpeechSynthesisProviderVoice)
initWithName_identifier_primaryLanguages_supportedLanguagesSelector = mkSelector "initWithName:identifier:primaryLanguages:supportedLanguages:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVSpeechSynthesisProviderVoice)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVSpeechSynthesisProviderVoice)
newSelector = mkSelector "new"

-- | @Selector@ for @updateSpeechVoices@
updateSpeechVoicesSelector :: Selector '[] ()
updateSpeechVoicesSelector = mkSelector "updateSpeechVoices"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @primaryLanguages@
primaryLanguagesSelector :: Selector '[] (Id NSArray)
primaryLanguagesSelector = mkSelector "primaryLanguages"

-- | @Selector@ for @supportedLanguages@
supportedLanguagesSelector :: Selector '[] (Id NSArray)
supportedLanguagesSelector = mkSelector "supportedLanguages"

-- | @Selector@ for @voiceSize@
voiceSizeSelector :: Selector '[] CLong
voiceSizeSelector = mkSelector "voiceSize"

-- | @Selector@ for @setVoiceSize:@
setVoiceSizeSelector :: Selector '[CLong] ()
setVoiceSizeSelector = mkSelector "setVoiceSize:"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector '[Id NSString] ()
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @gender@
genderSelector :: Selector '[] AVSpeechSynthesisVoiceGender
genderSelector = mkSelector "gender"

-- | @Selector@ for @setGender:@
setGenderSelector :: Selector '[AVSpeechSynthesisVoiceGender] ()
setGenderSelector = mkSelector "setGender:"

-- | @Selector@ for @age@
ageSelector :: Selector '[] CLong
ageSelector = mkSelector "age"

-- | @Selector@ for @setAge:@
setAgeSelector :: Selector '[CLong] ()
setAgeSelector = mkSelector "setAge:"

