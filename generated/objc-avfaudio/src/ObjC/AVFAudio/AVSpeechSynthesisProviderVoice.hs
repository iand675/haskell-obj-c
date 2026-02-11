{-# LANGUAGE PatternSynonyms #-}
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
  , initWithName_identifier_primaryLanguages_supportedLanguagesSelector
  , initSelector
  , newSelector
  , updateSpeechVoicesSelector
  , nameSelector
  , identifierSelector
  , primaryLanguagesSelector
  , supportedLanguagesSelector
  , voiceSizeSelector
  , setVoiceSizeSelector
  , versionSelector
  , setVersionSelector
  , genderSelector
  , setGenderSelector
  , ageSelector
  , setAgeSelector

  -- * Enum types
  , AVSpeechSynthesisVoiceGender(AVSpeechSynthesisVoiceGender)
  , pattern AVSpeechSynthesisVoiceGenderUnspecified
  , pattern AVSpeechSynthesisVoiceGenderMale
  , pattern AVSpeechSynthesisVoiceGenderFemale

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

-- | @- initWithName:identifier:primaryLanguages:supportedLanguages:@
initWithName_identifier_primaryLanguages_supportedLanguages :: (IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice, IsNSString name, IsNSString identifier, IsNSArray primaryLanguages, IsNSArray supportedLanguages) => avSpeechSynthesisProviderVoice -> name -> identifier -> primaryLanguages -> supportedLanguages -> IO (Id AVSpeechSynthesisProviderVoice)
initWithName_identifier_primaryLanguages_supportedLanguages avSpeechSynthesisProviderVoice  name identifier primaryLanguages supportedLanguages =
withObjCPtr name $ \raw_name ->
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr primaryLanguages $ \raw_primaryLanguages ->
      withObjCPtr supportedLanguages $ \raw_supportedLanguages ->
          sendMsg avSpeechSynthesisProviderVoice (mkSelector "initWithName:identifier:primaryLanguages:supportedLanguages:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_primaryLanguages :: Ptr ()), argPtr (castPtr raw_supportedLanguages :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO (Id AVSpeechSynthesisProviderVoice)
init_ avSpeechSynthesisProviderVoice  =
  sendMsg avSpeechSynthesisProviderVoice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVSpeechSynthesisProviderVoice)
new  =
  do
    cls' <- getRequiredClass "AVSpeechSynthesisProviderVoice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A call that indicates that a new voice or set of voices is available, or no longer available, for system use.
--
-- Call this method to indicate to the system that there has been change in the availability of the voices your application is providing to the system.
--
-- ObjC selector: @+ updateSpeechVoices@
updateSpeechVoices :: IO ()
updateSpeechVoices  =
  do
    cls' <- getRequiredClass "AVSpeechSynthesisProviderVoice"
    sendClassMsg cls' (mkSelector "updateSpeechVoices") retVoid []

-- | The localized name of the voice
--
-- ObjC selector: @- name@
name :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO (Id NSString)
name avSpeechSynthesisProviderVoice  =
  sendMsg avSpeechSynthesisProviderVoice (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A unique identifier for the voice
--
-- The recommended format is reverse domain notation.        Behavior is undefined if identifiers are not unique for all voices within a given extension.
--
-- ObjC selector: @- identifier@
identifier :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO (Id NSString)
identifier avSpeechSynthesisProviderVoice  =
  sendMsg avSpeechSynthesisProviderVoice (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A set of BCP 47 codes identifying the languages this synthesizer is primarily used for.
--
-- These languages are what a user would expect a synthesizer to fully support and be primarily used for.
--
-- ObjC selector: @- primaryLanguages@
primaryLanguages :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO (Id NSArray)
primaryLanguages avSpeechSynthesisProviderVoice  =
  sendMsg avSpeechSynthesisProviderVoice (mkSelector "primaryLanguages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A superset of BCP 47 codes identifying the voice’s supported languages.
--
-- These languages are what a user would expect a voice to be able to speak such that if the voice is given a multi-lingual phrase, it would be able to speak the entire phrase without a need to to switch voices. For example, a zh-CN voice could have @["zh-CN"]@ as its @primaryLanguages,@ but in @supportedLanguages@ have @["zh-CN","en-US"]@ indicating if it received "你好 means Hello", it would be able to speak the entire phrase.
--
-- ObjC selector: @- supportedLanguages@
supportedLanguages :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO (Id NSArray)
supportedLanguages avSpeechSynthesisProviderVoice  =
  sendMsg avSpeechSynthesisProviderVoice (mkSelector "supportedLanguages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The size of the voice (optional)
--
-- This reported size of the voice package on disk, in bytes. Defaults to 0.
--
-- ObjC selector: @- voiceSize@
voiceSize :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO CLong
voiceSize avSpeechSynthesisProviderVoice  =
  sendMsg avSpeechSynthesisProviderVoice (mkSelector "voiceSize") retCLong []

-- | The size of the voice (optional)
--
-- This reported size of the voice package on disk, in bytes. Defaults to 0.
--
-- ObjC selector: @- setVoiceSize:@
setVoiceSize :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> CLong -> IO ()
setVoiceSize avSpeechSynthesisProviderVoice  value =
  sendMsg avSpeechSynthesisProviderVoice (mkSelector "setVoiceSize:") retVoid [argCLong (fromIntegral value)]

-- | The voice version (optional)
--
-- This is an optional property for bookkeeping. This value does not affect system behavior.
--
-- ObjC selector: @- version@
version :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO (Id NSString)
version avSpeechSynthesisProviderVoice  =
  sendMsg avSpeechSynthesisProviderVoice (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The voice version (optional)
--
-- This is an optional property for bookkeeping. This value does not affect system behavior.
--
-- ObjC selector: @- setVersion:@
setVersion :: (IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice, IsNSString value) => avSpeechSynthesisProviderVoice -> value -> IO ()
setVersion avSpeechSynthesisProviderVoice  value =
withObjCPtr value $ \raw_value ->
    sendMsg avSpeechSynthesisProviderVoice (mkSelector "setVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The gender of the voice (optional)
--
-- ObjC selector: @- gender@
gender :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO AVSpeechSynthesisVoiceGender
gender avSpeechSynthesisProviderVoice  =
  fmap (coerce :: CLong -> AVSpeechSynthesisVoiceGender) $ sendMsg avSpeechSynthesisProviderVoice (mkSelector "gender") retCLong []

-- | The gender of the voice (optional)
--
-- ObjC selector: @- setGender:@
setGender :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> AVSpeechSynthesisVoiceGender -> IO ()
setGender avSpeechSynthesisProviderVoice  value =
  sendMsg avSpeechSynthesisProviderVoice (mkSelector "setGender:") retVoid [argCLong (coerce value)]

-- | The age of the voice in years (optional)
--
-- This is an optional property that indicates the age of this voice, to be treated as a personality trait. Defaults to 0.
--
-- ObjC selector: @- age@
age :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> IO CLong
age avSpeechSynthesisProviderVoice  =
  sendMsg avSpeechSynthesisProviderVoice (mkSelector "age") retCLong []

-- | The age of the voice in years (optional)
--
-- This is an optional property that indicates the age of this voice, to be treated as a personality trait. Defaults to 0.
--
-- ObjC selector: @- setAge:@
setAge :: IsAVSpeechSynthesisProviderVoice avSpeechSynthesisProviderVoice => avSpeechSynthesisProviderVoice -> CLong -> IO ()
setAge avSpeechSynthesisProviderVoice  value =
  sendMsg avSpeechSynthesisProviderVoice (mkSelector "setAge:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:identifier:primaryLanguages:supportedLanguages:@
initWithName_identifier_primaryLanguages_supportedLanguagesSelector :: Selector
initWithName_identifier_primaryLanguages_supportedLanguagesSelector = mkSelector "initWithName:identifier:primaryLanguages:supportedLanguages:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @updateSpeechVoices@
updateSpeechVoicesSelector :: Selector
updateSpeechVoicesSelector = mkSelector "updateSpeechVoices"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @primaryLanguages@
primaryLanguagesSelector :: Selector
primaryLanguagesSelector = mkSelector "primaryLanguages"

-- | @Selector@ for @supportedLanguages@
supportedLanguagesSelector :: Selector
supportedLanguagesSelector = mkSelector "supportedLanguages"

-- | @Selector@ for @voiceSize@
voiceSizeSelector :: Selector
voiceSizeSelector = mkSelector "voiceSize"

-- | @Selector@ for @setVoiceSize:@
setVoiceSizeSelector :: Selector
setVoiceSizeSelector = mkSelector "setVoiceSize:"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @gender@
genderSelector :: Selector
genderSelector = mkSelector "gender"

-- | @Selector@ for @setGender:@
setGenderSelector :: Selector
setGenderSelector = mkSelector "setGender:"

-- | @Selector@ for @age@
ageSelector :: Selector
ageSelector = mkSelector "age"

-- | @Selector@ for @setAge:@
setAgeSelector :: Selector
setAgeSelector = mkSelector "setAge:"

