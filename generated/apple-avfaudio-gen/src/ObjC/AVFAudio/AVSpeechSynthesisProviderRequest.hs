{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An @AVSpeechSynthesisProviderRequest@ gets delivered to an @AVSpeechSynthesisProviderAudioUnit@ in order to synthesize audio.    This is distinct from an @AVSpeechUtterance,@ which is a generic utterance to be spoken.
--
-- Generated bindings for @AVSpeechSynthesisProviderRequest@.
module ObjC.AVFAudio.AVSpeechSynthesisProviderRequest
  ( AVSpeechSynthesisProviderRequest
  , IsAVSpeechSynthesisProviderRequest(..)
  , initWithSSMLRepresentation_voice
  , init_
  , new
  , ssmlRepresentation
  , voice
  , initSelector
  , initWithSSMLRepresentation_voiceSelector
  , newSelector
  , ssmlRepresentationSelector
  , voiceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSSMLRepresentation:voice:@
initWithSSMLRepresentation_voice :: (IsAVSpeechSynthesisProviderRequest avSpeechSynthesisProviderRequest, IsNSString text, IsAVSpeechSynthesisProviderVoice voice) => avSpeechSynthesisProviderRequest -> text -> voice -> IO (Id AVSpeechSynthesisProviderRequest)
initWithSSMLRepresentation_voice avSpeechSynthesisProviderRequest text voice =
  sendOwnedMessage avSpeechSynthesisProviderRequest initWithSSMLRepresentation_voiceSelector (toNSString text) (toAVSpeechSynthesisProviderVoice voice)

-- | @- init@
init_ :: IsAVSpeechSynthesisProviderRequest avSpeechSynthesisProviderRequest => avSpeechSynthesisProviderRequest -> IO (Id AVSpeechSynthesisProviderRequest)
init_ avSpeechSynthesisProviderRequest =
  sendOwnedMessage avSpeechSynthesisProviderRequest initSelector

-- | @+ new@
new :: IO (Id AVSpeechSynthesisProviderRequest)
new  =
  do
    cls' <- getRequiredClass "AVSpeechSynthesisProviderRequest"
    sendOwnedClassMessage cls' newSelector

-- | The SSML representation of the text to be synthesized with the corresponding speech synthesis attributes for customization of pitch, rate, intonation, and more.
--
-- See: https://www.w3.org/TR/speech-synthesis11/
--
-- ObjC selector: @- ssmlRepresentation@
ssmlRepresentation :: IsAVSpeechSynthesisProviderRequest avSpeechSynthesisProviderRequest => avSpeechSynthesisProviderRequest -> IO (Id NSString)
ssmlRepresentation avSpeechSynthesisProviderRequest =
  sendMessage avSpeechSynthesisProviderRequest ssmlRepresentationSelector

-- | The voice to be used in this speech request
--
-- ObjC selector: @- voice@
voice :: IsAVSpeechSynthesisProviderRequest avSpeechSynthesisProviderRequest => avSpeechSynthesisProviderRequest -> IO (Id AVSpeechSynthesisProviderVoice)
voice avSpeechSynthesisProviderRequest =
  sendMessage avSpeechSynthesisProviderRequest voiceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSSMLRepresentation:voice:@
initWithSSMLRepresentation_voiceSelector :: Selector '[Id NSString, Id AVSpeechSynthesisProviderVoice] (Id AVSpeechSynthesisProviderRequest)
initWithSSMLRepresentation_voiceSelector = mkSelector "initWithSSMLRepresentation:voice:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVSpeechSynthesisProviderRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVSpeechSynthesisProviderRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @ssmlRepresentation@
ssmlRepresentationSelector :: Selector '[] (Id NSString)
ssmlRepresentationSelector = mkSelector "ssmlRepresentation"

-- | @Selector@ for @voice@
voiceSelector :: Selector '[] (Id AVSpeechSynthesisProviderVoice)
voiceSelector = mkSelector "voice"

