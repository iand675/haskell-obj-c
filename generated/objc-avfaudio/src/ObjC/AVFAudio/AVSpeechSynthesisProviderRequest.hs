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
  , initWithSSMLRepresentation_voiceSelector
  , initSelector
  , newSelector
  , ssmlRepresentationSelector
  , voiceSelector


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

-- | @- initWithSSMLRepresentation:voice:@
initWithSSMLRepresentation_voice :: (IsAVSpeechSynthesisProviderRequest avSpeechSynthesisProviderRequest, IsNSString text, IsAVSpeechSynthesisProviderVoice voice) => avSpeechSynthesisProviderRequest -> text -> voice -> IO (Id AVSpeechSynthesisProviderRequest)
initWithSSMLRepresentation_voice avSpeechSynthesisProviderRequest  text voice =
withObjCPtr text $ \raw_text ->
  withObjCPtr voice $ \raw_voice ->
      sendMsg avSpeechSynthesisProviderRequest (mkSelector "initWithSSMLRepresentation:voice:") (retPtr retVoid) [argPtr (castPtr raw_text :: Ptr ()), argPtr (castPtr raw_voice :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsAVSpeechSynthesisProviderRequest avSpeechSynthesisProviderRequest => avSpeechSynthesisProviderRequest -> IO (Id AVSpeechSynthesisProviderRequest)
init_ avSpeechSynthesisProviderRequest  =
  sendMsg avSpeechSynthesisProviderRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVSpeechSynthesisProviderRequest)
new  =
  do
    cls' <- getRequiredClass "AVSpeechSynthesisProviderRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The SSML representation of the text to be synthesized with the corresponding speech synthesis attributes for customization of pitch, rate, intonation, and more.
--
-- See: https://www.w3.org/TR/speech-synthesis11/
--
-- ObjC selector: @- ssmlRepresentation@
ssmlRepresentation :: IsAVSpeechSynthesisProviderRequest avSpeechSynthesisProviderRequest => avSpeechSynthesisProviderRequest -> IO (Id NSString)
ssmlRepresentation avSpeechSynthesisProviderRequest  =
  sendMsg avSpeechSynthesisProviderRequest (mkSelector "ssmlRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The voice to be used in this speech request
--
-- ObjC selector: @- voice@
voice :: IsAVSpeechSynthesisProviderRequest avSpeechSynthesisProviderRequest => avSpeechSynthesisProviderRequest -> IO (Id AVSpeechSynthesisProviderVoice)
voice avSpeechSynthesisProviderRequest  =
  sendMsg avSpeechSynthesisProviderRequest (mkSelector "voice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSSMLRepresentation:voice:@
initWithSSMLRepresentation_voiceSelector :: Selector
initWithSSMLRepresentation_voiceSelector = mkSelector "initWithSSMLRepresentation:voice:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @ssmlRepresentation@
ssmlRepresentationSelector :: Selector
ssmlRepresentationSelector = mkSelector "ssmlRepresentation"

-- | @Selector@ for @voice@
voiceSelector :: Selector
voiceSelector = mkSelector "voice"

