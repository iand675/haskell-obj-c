{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object describing the location of a custom language model and specialized vocabulary.
--
-- Pass this object to ``SFSpeechLanguageModel/prepareCustomLanguageModelForUrl:configuration:completion:`` to indicate where that method should create the custom language model file, and to ``SFSpeechRecognitionRequest/customizedLanguageModel`` or ``DictationTranscriber/ContentHint/customizedLanguage(modelConfiguration:)`` to indicate where the system should find that model to use.
--
-- Generated bindings for @SFSpeechLanguageModelConfiguration@.
module ObjC.Speech.SFSpeechLanguageModelConfiguration
  ( SFSpeechLanguageModelConfiguration
  , IsSFSpeechLanguageModelConfiguration(..)
  , initWithLanguageModel
  , initWithLanguageModel_vocabulary
  , initWithLanguageModel_vocabulary_weight
  , languageModel
  , vocabulary
  , weight
  , initWithLanguageModelSelector
  , initWithLanguageModel_vocabularySelector
  , initWithLanguageModel_vocabulary_weightSelector
  , languageModelSelector
  , vocabularySelector
  , weightSelector


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

import ObjC.Speech.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a configuration with the location of a language model file.
--
-- ObjC selector: @- initWithLanguageModel:@
initWithLanguageModel :: (IsSFSpeechLanguageModelConfiguration sfSpeechLanguageModelConfiguration, IsNSURL languageModel) => sfSpeechLanguageModelConfiguration -> languageModel -> IO (Id SFSpeechLanguageModelConfiguration)
initWithLanguageModel sfSpeechLanguageModelConfiguration  languageModel =
withObjCPtr languageModel $ \raw_languageModel ->
    sendMsg sfSpeechLanguageModelConfiguration (mkSelector "initWithLanguageModel:") (retPtr retVoid) [argPtr (castPtr raw_languageModel :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a configuration with the locations of language model and vocabulary files.
--
-- ObjC selector: @- initWithLanguageModel:vocabulary:@
initWithLanguageModel_vocabulary :: (IsSFSpeechLanguageModelConfiguration sfSpeechLanguageModelConfiguration, IsNSURL languageModel, IsNSURL vocabulary) => sfSpeechLanguageModelConfiguration -> languageModel -> vocabulary -> IO (Id SFSpeechLanguageModelConfiguration)
initWithLanguageModel_vocabulary sfSpeechLanguageModelConfiguration  languageModel vocabulary =
withObjCPtr languageModel $ \raw_languageModel ->
  withObjCPtr vocabulary $ \raw_vocabulary ->
      sendMsg sfSpeechLanguageModelConfiguration (mkSelector "initWithLanguageModel:vocabulary:") (retPtr retVoid) [argPtr (castPtr raw_languageModel :: Ptr ()), argPtr (castPtr raw_vocabulary :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a configuration with the locations of language model and vocabulary files, and custom weight.
--
-- ObjC selector: @- initWithLanguageModel:vocabulary:weight:@
initWithLanguageModel_vocabulary_weight :: (IsSFSpeechLanguageModelConfiguration sfSpeechLanguageModelConfiguration, IsNSURL languageModel, IsNSURL vocabulary, IsNSNumber weight) => sfSpeechLanguageModelConfiguration -> languageModel -> vocabulary -> weight -> IO (Id SFSpeechLanguageModelConfiguration)
initWithLanguageModel_vocabulary_weight sfSpeechLanguageModelConfiguration  languageModel vocabulary weight =
withObjCPtr languageModel $ \raw_languageModel ->
  withObjCPtr vocabulary $ \raw_vocabulary ->
    withObjCPtr weight $ \raw_weight ->
        sendMsg sfSpeechLanguageModelConfiguration (mkSelector "initWithLanguageModel:vocabulary:weight:") (retPtr retVoid) [argPtr (castPtr raw_languageModel :: Ptr ()), argPtr (castPtr raw_vocabulary :: Ptr ()), argPtr (castPtr raw_weight :: Ptr ())] >>= ownedObject . castPtr

-- | The location of a compiled language model file.
--
-- ObjC selector: @- languageModel@
languageModel :: IsSFSpeechLanguageModelConfiguration sfSpeechLanguageModelConfiguration => sfSpeechLanguageModelConfiguration -> IO (Id NSURL)
languageModel sfSpeechLanguageModelConfiguration  =
  sendMsg sfSpeechLanguageModelConfiguration (mkSelector "languageModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The location of a compiled vocabulary file.
--
-- ObjC selector: @- vocabulary@
vocabulary :: IsSFSpeechLanguageModelConfiguration sfSpeechLanguageModelConfiguration => sfSpeechLanguageModelConfiguration -> IO (Id NSURL)
vocabulary sfSpeechLanguageModelConfiguration  =
  sendMsg sfSpeechLanguageModelConfiguration (mkSelector "vocabulary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The relative weight of the language model customization. Value must be between 0.0 and 1.0 inclusive.
--
-- ObjC selector: @- weight@
weight :: IsSFSpeechLanguageModelConfiguration sfSpeechLanguageModelConfiguration => sfSpeechLanguageModelConfiguration -> IO (Id NSNumber)
weight sfSpeechLanguageModelConfiguration  =
  sendMsg sfSpeechLanguageModelConfiguration (mkSelector "weight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLanguageModel:@
initWithLanguageModelSelector :: Selector
initWithLanguageModelSelector = mkSelector "initWithLanguageModel:"

-- | @Selector@ for @initWithLanguageModel:vocabulary:@
initWithLanguageModel_vocabularySelector :: Selector
initWithLanguageModel_vocabularySelector = mkSelector "initWithLanguageModel:vocabulary:"

-- | @Selector@ for @initWithLanguageModel:vocabulary:weight:@
initWithLanguageModel_vocabulary_weightSelector :: Selector
initWithLanguageModel_vocabulary_weightSelector = mkSelector "initWithLanguageModel:vocabulary:weight:"

-- | @Selector@ for @languageModel@
languageModelSelector :: Selector
languageModelSelector = mkSelector "languageModel"

-- | @Selector@ for @vocabulary@
vocabularySelector :: Selector
vocabularySelector = mkSelector "vocabulary"

-- | @Selector@ for @weight@
weightSelector :: Selector
weightSelector = mkSelector "weight"

