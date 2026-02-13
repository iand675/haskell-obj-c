{-# LANGUAGE DataKinds #-}
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
  , cancelSpeechRequestSelector
  , setSpeechSynthesisOutputMetadataBlockSelector
  , setSpeechVoicesSelector
  , speechSynthesisOutputMetadataBlockSelector
  , speechVoicesSelector
  , synthesizeSpeechRequestSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
synthesizeSpeechRequest avSpeechSynthesisProviderAudioUnit speechRequest =
  sendMessage avSpeechSynthesisProviderAudioUnit synthesizeSpeechRequestSelector (toAVSpeechSynthesisProviderRequest speechRequest)

-- | Informs the audio unit that the speech request job should be discarded.
--
-- ObjC selector: @- cancelSpeechRequest@
cancelSpeechRequest :: IsAVSpeechSynthesisProviderAudioUnit avSpeechSynthesisProviderAudioUnit => avSpeechSynthesisProviderAudioUnit -> IO ()
cancelSpeechRequest avSpeechSynthesisProviderAudioUnit =
  sendMessage avSpeechSynthesisProviderAudioUnit cancelSpeechRequestSelector

-- | Returns the voices this audio unit has available and ready for synthesis.
--
-- This method should fetch and return the voices ready to synthesize that a user can select from (usually through Settings).        Required for speech synthesizer audio unit extensions. An audio unit with a dynamic list of voices can override this property's getter to perform a more complex fetch.
--
-- ObjC selector: @- speechVoices@
speechVoices :: IsAVSpeechSynthesisProviderAudioUnit avSpeechSynthesisProviderAudioUnit => avSpeechSynthesisProviderAudioUnit -> IO (Id NSArray)
speechVoices avSpeechSynthesisProviderAudioUnit =
  sendMessage avSpeechSynthesisProviderAudioUnit speechVoicesSelector

-- | Returns the voices this audio unit has available and ready for synthesis.
--
-- This method should fetch and return the voices ready to synthesize that a user can select from (usually through Settings).        Required for speech synthesizer audio unit extensions. An audio unit with a dynamic list of voices can override this property's getter to perform a more complex fetch.
--
-- ObjC selector: @- setSpeechVoices:@
setSpeechVoices :: (IsAVSpeechSynthesisProviderAudioUnit avSpeechSynthesisProviderAudioUnit, IsNSArray value) => avSpeechSynthesisProviderAudioUnit -> value -> IO ()
setSpeechVoices avSpeechSynthesisProviderAudioUnit value =
  sendMessage avSpeechSynthesisProviderAudioUnit setSpeechVoicesSelector (toNSArray value)

-- | A property set by the host that is called by the audio unit to supply metadata for a speech request.
--
-- A synthesizer should call this method when it has produced relevant data to the audio buffers it is sending back to the host. In some cases speech output may be delayed until these markers are delivered. For example, word highlighting depends on marker data from synthesizers in order to properly time which words are highlighted. Many speech synthesizers generate this information on the fly, while synthesizing the audio. The array of markers can reference future audio buffers that have yet to be delivered.
--
-- There may be cases in which marker data is not fully known until further audio processing is done. In these cases, and other casers where marker data has changed, calling this block with marker data that contains perviously delivered audio buffer ranges will replace that audio buffer range's marker data, as it will be considered stale.
--
-- ObjC selector: @- speechSynthesisOutputMetadataBlock@
speechSynthesisOutputMetadataBlock :: IsAVSpeechSynthesisProviderAudioUnit avSpeechSynthesisProviderAudioUnit => avSpeechSynthesisProviderAudioUnit -> IO (Ptr ())
speechSynthesisOutputMetadataBlock avSpeechSynthesisProviderAudioUnit =
  sendMessage avSpeechSynthesisProviderAudioUnit speechSynthesisOutputMetadataBlockSelector

-- | A property set by the host that is called by the audio unit to supply metadata for a speech request.
--
-- A synthesizer should call this method when it has produced relevant data to the audio buffers it is sending back to the host. In some cases speech output may be delayed until these markers are delivered. For example, word highlighting depends on marker data from synthesizers in order to properly time which words are highlighted. Many speech synthesizers generate this information on the fly, while synthesizing the audio. The array of markers can reference future audio buffers that have yet to be delivered.
--
-- There may be cases in which marker data is not fully known until further audio processing is done. In these cases, and other casers where marker data has changed, calling this block with marker data that contains perviously delivered audio buffer ranges will replace that audio buffer range's marker data, as it will be considered stale.
--
-- ObjC selector: @- setSpeechSynthesisOutputMetadataBlock:@
setSpeechSynthesisOutputMetadataBlock :: IsAVSpeechSynthesisProviderAudioUnit avSpeechSynthesisProviderAudioUnit => avSpeechSynthesisProviderAudioUnit -> Ptr () -> IO ()
setSpeechSynthesisOutputMetadataBlock avSpeechSynthesisProviderAudioUnit value =
  sendMessage avSpeechSynthesisProviderAudioUnit setSpeechSynthesisOutputMetadataBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @synthesizeSpeechRequest:@
synthesizeSpeechRequestSelector :: Selector '[Id AVSpeechSynthesisProviderRequest] ()
synthesizeSpeechRequestSelector = mkSelector "synthesizeSpeechRequest:"

-- | @Selector@ for @cancelSpeechRequest@
cancelSpeechRequestSelector :: Selector '[] ()
cancelSpeechRequestSelector = mkSelector "cancelSpeechRequest"

-- | @Selector@ for @speechVoices@
speechVoicesSelector :: Selector '[] (Id NSArray)
speechVoicesSelector = mkSelector "speechVoices"

-- | @Selector@ for @setSpeechVoices:@
setSpeechVoicesSelector :: Selector '[Id NSArray] ()
setSpeechVoicesSelector = mkSelector "setSpeechVoices:"

-- | @Selector@ for @speechSynthesisOutputMetadataBlock@
speechSynthesisOutputMetadataBlockSelector :: Selector '[] (Ptr ())
speechSynthesisOutputMetadataBlockSelector = mkSelector "speechSynthesisOutputMetadataBlock"

-- | @Selector@ for @setSpeechSynthesisOutputMetadataBlock:@
setSpeechSynthesisOutputMetadataBlockSelector :: Selector '[Ptr ()] ()
setSpeechSynthesisOutputMetadataBlockSelector = mkSelector "setSpeechSynthesisOutputMetadataBlock:"

