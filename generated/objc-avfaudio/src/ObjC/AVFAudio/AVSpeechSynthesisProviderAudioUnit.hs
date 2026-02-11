{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVSpeechSynthesisProviderAudioUnit@.
module ObjC.AVFAudio.AVSpeechSynthesisProviderAudioUnit
  ( AVSpeechSynthesisProviderAudioUnit
  , IsAVSpeechSynthesisProviderAudioUnit(..)
  , synthesizeSpeechRequest
  , cancelSpeechRequest
  , speechVoices
  , setSpeechVoices
  , speechSynthesisOutputMetadataBlock
  , setSpeechSynthesisOutputMetadataBlock
  , synthesizeSpeechRequestSelector
  , cancelSpeechRequestSelector
  , speechVoicesSelector
  , setSpeechVoicesSelector
  , speechSynthesisOutputMetadataBlockSelector
  , setSpeechSynthesisOutputMetadataBlockSelector


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
import ObjC.AudioToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Sends a new speech request to be synthesized
--
-- Sends a new speech request to the synthesizer to render. When the synthesizer audio unit is finished generating audio buffers for the speech request, it should indicate this within its internal render block, @AUInternalRenderBlock,@ specifically through the @AudioUnitRenderActionFlags@ flag @kAudioOfflineUnitRenderAction_Complete.@
--
-- ObjC selector: @- synthesizeSpeechRequest:@
synthesizeSpeechRequest :: (IsAVSpeechSynthesisProviderAudioUnit avSpeechSynthesisProviderAudioUnit, IsAVSpeechSynthesisProviderRequest speechRequest) => avSpeechSynthesisProviderAudioUnit -> speechRequest -> IO ()
synthesizeSpeechRequest avSpeechSynthesisProviderAudioUnit  speechRequest =
withObjCPtr speechRequest $ \raw_speechRequest ->
    sendMsg avSpeechSynthesisProviderAudioUnit (mkSelector "synthesizeSpeechRequest:") retVoid [argPtr (castPtr raw_speechRequest :: Ptr ())]

-- | Informs the audio unit that the speech request job should be discarded.
--
-- ObjC selector: @- cancelSpeechRequest@
cancelSpeechRequest :: IsAVSpeechSynthesisProviderAudioUnit avSpeechSynthesisProviderAudioUnit => avSpeechSynthesisProviderAudioUnit -> IO ()
cancelSpeechRequest avSpeechSynthesisProviderAudioUnit  =
  sendMsg avSpeechSynthesisProviderAudioUnit (mkSelector "cancelSpeechRequest") retVoid []

-- | Returns the voices this audio unit has available and ready for synthesis.
--
-- This method should fetch and return the voices ready to synthesize that a user can select from (usually through Settings).        Required for speech synthesizer audio unit extensions. An audio unit with a dynamic list of voices can override this property's getter to perform a more complex fetch.
--
-- ObjC selector: @- speechVoices@
speechVoices :: IsAVSpeechSynthesisProviderAudioUnit avSpeechSynthesisProviderAudioUnit => avSpeechSynthesisProviderAudioUnit -> IO (Id NSArray)
speechVoices avSpeechSynthesisProviderAudioUnit  =
  sendMsg avSpeechSynthesisProviderAudioUnit (mkSelector "speechVoices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the voices this audio unit has available and ready for synthesis.
--
-- This method should fetch and return the voices ready to synthesize that a user can select from (usually through Settings).        Required for speech synthesizer audio unit extensions. An audio unit with a dynamic list of voices can override this property's getter to perform a more complex fetch.
--
-- ObjC selector: @- setSpeechVoices:@
setSpeechVoices :: (IsAVSpeechSynthesisProviderAudioUnit avSpeechSynthesisProviderAudioUnit, IsNSArray value) => avSpeechSynthesisProviderAudioUnit -> value -> IO ()
setSpeechVoices avSpeechSynthesisProviderAudioUnit  value =
withObjCPtr value $ \raw_value ->
    sendMsg avSpeechSynthesisProviderAudioUnit (mkSelector "setSpeechVoices:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A property set by the host that is called by the audio unit to supply metadata for a speech request.
--
-- A synthesizer should call this method when it has produced relevant data to the audio buffers it is sending back to the host. In some cases speech output may be delayed until these markers are delivered. For example, word highlighting depends on marker data from synthesizers in order to properly time which words are highlighted. Many speech synthesizers generate this information on the fly, while synthesizing the audio. The array of markers can reference future audio buffers that have yet to be delivered.
--
-- There may be cases in which marker data is not fully known until further audio processing is done. In these cases, and other casers where marker data has changed, calling this block with marker data that contains perviously delivered audio buffer ranges will replace that audio buffer range's marker data, as it will be considered stale.
--
-- ObjC selector: @- speechSynthesisOutputMetadataBlock@
speechSynthesisOutputMetadataBlock :: IsAVSpeechSynthesisProviderAudioUnit avSpeechSynthesisProviderAudioUnit => avSpeechSynthesisProviderAudioUnit -> IO (Ptr ())
speechSynthesisOutputMetadataBlock avSpeechSynthesisProviderAudioUnit  =
  fmap castPtr $ sendMsg avSpeechSynthesisProviderAudioUnit (mkSelector "speechSynthesisOutputMetadataBlock") (retPtr retVoid) []

-- | A property set by the host that is called by the audio unit to supply metadata for a speech request.
--
-- A synthesizer should call this method when it has produced relevant data to the audio buffers it is sending back to the host. In some cases speech output may be delayed until these markers are delivered. For example, word highlighting depends on marker data from synthesizers in order to properly time which words are highlighted. Many speech synthesizers generate this information on the fly, while synthesizing the audio. The array of markers can reference future audio buffers that have yet to be delivered.
--
-- There may be cases in which marker data is not fully known until further audio processing is done. In these cases, and other casers where marker data has changed, calling this block with marker data that contains perviously delivered audio buffer ranges will replace that audio buffer range's marker data, as it will be considered stale.
--
-- ObjC selector: @- setSpeechSynthesisOutputMetadataBlock:@
setSpeechSynthesisOutputMetadataBlock :: IsAVSpeechSynthesisProviderAudioUnit avSpeechSynthesisProviderAudioUnit => avSpeechSynthesisProviderAudioUnit -> Ptr () -> IO ()
setSpeechSynthesisOutputMetadataBlock avSpeechSynthesisProviderAudioUnit  value =
  sendMsg avSpeechSynthesisProviderAudioUnit (mkSelector "setSpeechSynthesisOutputMetadataBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @synthesizeSpeechRequest:@
synthesizeSpeechRequestSelector :: Selector
synthesizeSpeechRequestSelector = mkSelector "synthesizeSpeechRequest:"

-- | @Selector@ for @cancelSpeechRequest@
cancelSpeechRequestSelector :: Selector
cancelSpeechRequestSelector = mkSelector "cancelSpeechRequest"

-- | @Selector@ for @speechVoices@
speechVoicesSelector :: Selector
speechVoicesSelector = mkSelector "speechVoices"

-- | @Selector@ for @setSpeechVoices:@
setSpeechVoicesSelector :: Selector
setSpeechVoicesSelector = mkSelector "setSpeechVoices:"

-- | @Selector@ for @speechSynthesisOutputMetadataBlock@
speechSynthesisOutputMetadataBlockSelector :: Selector
speechSynthesisOutputMetadataBlockSelector = mkSelector "speechSynthesisOutputMetadataBlock"

-- | @Selector@ for @setSpeechSynthesisOutputMetadataBlock:@
setSpeechSynthesisOutputMetadataBlockSelector :: Selector
setSpeechSynthesisOutputMetadataBlockSelector = mkSelector "setSpeechSynthesisOutputMetadataBlock:"

