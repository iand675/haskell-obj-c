{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVSpeechSynthesizer
--
-- AVSpeechSynthesizer allows speaking of speech utterances with a basic queuing mechanism.
--
-- Create an instance of AVSpeechSynthesizer to start generating synthesized speech by using AVSpeechUtterance objects.
--
-- Generated bindings for @AVSpeechSynthesizer@.
module ObjC.AVFAudio.AVSpeechSynthesizer
  ( AVSpeechSynthesizer
  , IsAVSpeechSynthesizer(..)
  , speakUtterance
  , writeUtterance_toBufferCallback
  , writeUtterance_toBufferCallback_toMarkerCallback
  , stopSpeakingAtBoundary
  , pauseSpeakingAtBoundary
  , continueSpeaking
  , requestPersonalVoiceAuthorizationWithCompletionHandler
  , delegate
  , setDelegate
  , speaking
  , paused
  , outputChannels
  , setOutputChannels
  , usesApplicationAudioSession
  , setUsesApplicationAudioSession
  , mixToTelephonyUplink
  , setMixToTelephonyUplink
  , personalVoiceAuthorizationStatus
  , speakUtteranceSelector
  , writeUtterance_toBufferCallbackSelector
  , writeUtterance_toBufferCallback_toMarkerCallbackSelector
  , stopSpeakingAtBoundarySelector
  , pauseSpeakingAtBoundarySelector
  , continueSpeakingSelector
  , requestPersonalVoiceAuthorizationWithCompletionHandlerSelector
  , delegateSelector
  , setDelegateSelector
  , speakingSelector
  , pausedSelector
  , outputChannelsSelector
  , setOutputChannelsSelector
  , usesApplicationAudioSessionSelector
  , setUsesApplicationAudioSessionSelector
  , mixToTelephonyUplinkSelector
  , setMixToTelephonyUplinkSelector
  , personalVoiceAuthorizationStatusSelector

  -- * Enum types
  , AVSpeechBoundary(AVSpeechBoundary)
  , pattern AVSpeechBoundaryImmediate
  , pattern AVSpeechBoundaryWord
  , AVSpeechSynthesisPersonalVoiceAuthorizationStatus(AVSpeechSynthesisPersonalVoiceAuthorizationStatus)
  , pattern AVSpeechSynthesisPersonalVoiceAuthorizationStatusNotDetermined
  , pattern AVSpeechSynthesisPersonalVoiceAuthorizationStatusDenied
  , pattern AVSpeechSynthesisPersonalVoiceAuthorizationStatusUnsupported
  , pattern AVSpeechSynthesisPersonalVoiceAuthorizationStatusAuthorized

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

-- | @- speakUtterance:@
speakUtterance :: (IsAVSpeechSynthesizer avSpeechSynthesizer, IsAVSpeechUtterance utterance) => avSpeechSynthesizer -> utterance -> IO ()
speakUtterance avSpeechSynthesizer  utterance =
  withObjCPtr utterance $ \raw_utterance ->
      sendMsg avSpeechSynthesizer (mkSelector "speakUtterance:") retVoid [argPtr (castPtr raw_utterance :: Ptr ())]

-- | @- writeUtterance:toBufferCallback:@
writeUtterance_toBufferCallback :: (IsAVSpeechSynthesizer avSpeechSynthesizer, IsAVSpeechUtterance utterance) => avSpeechSynthesizer -> utterance -> Ptr () -> IO ()
writeUtterance_toBufferCallback avSpeechSynthesizer  utterance bufferCallback =
  withObjCPtr utterance $ \raw_utterance ->
      sendMsg avSpeechSynthesizer (mkSelector "writeUtterance:toBufferCallback:") retVoid [argPtr (castPtr raw_utterance :: Ptr ()), argPtr (castPtr bufferCallback :: Ptr ())]

-- | Use this method to receive audio buffers and associated metadata that can be used to store or further process synthesized speech. The dictionary provided by -[AVSpeechSynthesisVoice audioFileSettings] can be used to create an AVAudioFile.
--
-- ObjC selector: @- writeUtterance:toBufferCallback:toMarkerCallback:@
writeUtterance_toBufferCallback_toMarkerCallback :: (IsAVSpeechSynthesizer avSpeechSynthesizer, IsAVSpeechUtterance utterance) => avSpeechSynthesizer -> utterance -> Ptr () -> Ptr () -> IO ()
writeUtterance_toBufferCallback_toMarkerCallback avSpeechSynthesizer  utterance bufferCallback markerCallback =
  withObjCPtr utterance $ \raw_utterance ->
      sendMsg avSpeechSynthesizer (mkSelector "writeUtterance:toBufferCallback:toMarkerCallback:") retVoid [argPtr (castPtr raw_utterance :: Ptr ()), argPtr (castPtr bufferCallback :: Ptr ()), argPtr (castPtr markerCallback :: Ptr ())]

-- | @- stopSpeakingAtBoundary:@
stopSpeakingAtBoundary :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> AVSpeechBoundary -> IO Bool
stopSpeakingAtBoundary avSpeechSynthesizer  boundary =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSpeechSynthesizer (mkSelector "stopSpeakingAtBoundary:") retCULong [argCLong (coerce boundary)]

-- | @- pauseSpeakingAtBoundary:@
pauseSpeakingAtBoundary :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> AVSpeechBoundary -> IO Bool
pauseSpeakingAtBoundary avSpeechSynthesizer  boundary =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSpeechSynthesizer (mkSelector "pauseSpeakingAtBoundary:") retCULong [argCLong (coerce boundary)]

-- | @- continueSpeaking@
continueSpeaking :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO Bool
continueSpeaking avSpeechSynthesizer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSpeechSynthesizer (mkSelector "continueSpeaking") retCULong []

-- | Asks the user to allow your app to use personal voices for speech synthesis
--
-- Call this method before performing any other tasks associated with speech synthesis using personal voices. This method executes asynchronously, returning shortly after you call it. At some point later, the system calls the provided handler block with the results.
--
-- When your app's authorization status is PersonalVoiceAuthorizationStatus.notDetermined, this method causes the system to prompt the user to grant or deny permission for your app to use personal voices. The user's response is saved so that future calls to this method do not prompt the user again.
--
-- ObjC selector: @+ requestPersonalVoiceAuthorizationWithCompletionHandler:@
requestPersonalVoiceAuthorizationWithCompletionHandler :: Ptr () -> IO ()
requestPersonalVoiceAuthorizationWithCompletionHandler handler =
  do
    cls' <- getRequiredClass "AVSpeechSynthesizer"
    sendClassMsg cls' (mkSelector "requestPersonalVoiceAuthorizationWithCompletionHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @- delegate@
delegate :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO RawId
delegate avSpeechSynthesizer  =
    fmap (RawId . castPtr) $ sendMsg avSpeechSynthesizer (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> RawId -> IO ()
setDelegate avSpeechSynthesizer  value =
    sendMsg avSpeechSynthesizer (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- speaking@
speaking :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO Bool
speaking avSpeechSynthesizer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSpeechSynthesizer (mkSelector "speaking") retCULong []

-- | @- paused@
paused :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO Bool
paused avSpeechSynthesizer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSpeechSynthesizer (mkSelector "paused") retCULong []

-- | @- outputChannels@
outputChannels :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO (Id NSArray)
outputChannels avSpeechSynthesizer  =
    sendMsg avSpeechSynthesizer (mkSelector "outputChannels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOutputChannels:@
setOutputChannels :: (IsAVSpeechSynthesizer avSpeechSynthesizer, IsNSArray value) => avSpeechSynthesizer -> value -> IO ()
setOutputChannels avSpeechSynthesizer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avSpeechSynthesizer (mkSelector "setOutputChannels:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- usesApplicationAudioSession@
usesApplicationAudioSession :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO Bool
usesApplicationAudioSession avSpeechSynthesizer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSpeechSynthesizer (mkSelector "usesApplicationAudioSession") retCULong []

-- | @- setUsesApplicationAudioSession:@
setUsesApplicationAudioSession :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> Bool -> IO ()
setUsesApplicationAudioSession avSpeechSynthesizer  value =
    sendMsg avSpeechSynthesizer (mkSelector "setUsesApplicationAudioSession:") retVoid [argCULong (if value then 1 else 0)]

-- | @- mixToTelephonyUplink@
mixToTelephonyUplink :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO Bool
mixToTelephonyUplink avSpeechSynthesizer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSpeechSynthesizer (mkSelector "mixToTelephonyUplink") retCULong []

-- | @- setMixToTelephonyUplink:@
setMixToTelephonyUplink :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> Bool -> IO ()
setMixToTelephonyUplink avSpeechSynthesizer  value =
    sendMsg avSpeechSynthesizer (mkSelector "setMixToTelephonyUplink:") retVoid [argCULong (if value then 1 else 0)]

-- | Returns your app's current authorization to use personal voices.
--
-- The user can reject your app's request to use personal voices, but your request can also be denied if personal voices are not supported on the device. The app can also change your app's authorization status at any time from the Settings app.
--
-- The app's current authorization status value. For a list of values, see AVSpeechSynthesisPersonalVoiceAuthorizationStatus.
--
-- ObjC selector: @+ personalVoiceAuthorizationStatus@
personalVoiceAuthorizationStatus :: IO AVSpeechSynthesisPersonalVoiceAuthorizationStatus
personalVoiceAuthorizationStatus  =
  do
    cls' <- getRequiredClass "AVSpeechSynthesizer"
    fmap (coerce :: CULong -> AVSpeechSynthesisPersonalVoiceAuthorizationStatus) $ sendClassMsg cls' (mkSelector "personalVoiceAuthorizationStatus") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @speakUtterance:@
speakUtteranceSelector :: Selector
speakUtteranceSelector = mkSelector "speakUtterance:"

-- | @Selector@ for @writeUtterance:toBufferCallback:@
writeUtterance_toBufferCallbackSelector :: Selector
writeUtterance_toBufferCallbackSelector = mkSelector "writeUtterance:toBufferCallback:"

-- | @Selector@ for @writeUtterance:toBufferCallback:toMarkerCallback:@
writeUtterance_toBufferCallback_toMarkerCallbackSelector :: Selector
writeUtterance_toBufferCallback_toMarkerCallbackSelector = mkSelector "writeUtterance:toBufferCallback:toMarkerCallback:"

-- | @Selector@ for @stopSpeakingAtBoundary:@
stopSpeakingAtBoundarySelector :: Selector
stopSpeakingAtBoundarySelector = mkSelector "stopSpeakingAtBoundary:"

-- | @Selector@ for @pauseSpeakingAtBoundary:@
pauseSpeakingAtBoundarySelector :: Selector
pauseSpeakingAtBoundarySelector = mkSelector "pauseSpeakingAtBoundary:"

-- | @Selector@ for @continueSpeaking@
continueSpeakingSelector :: Selector
continueSpeakingSelector = mkSelector "continueSpeaking"

-- | @Selector@ for @requestPersonalVoiceAuthorizationWithCompletionHandler:@
requestPersonalVoiceAuthorizationWithCompletionHandlerSelector :: Selector
requestPersonalVoiceAuthorizationWithCompletionHandlerSelector = mkSelector "requestPersonalVoiceAuthorizationWithCompletionHandler:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @speaking@
speakingSelector :: Selector
speakingSelector = mkSelector "speaking"

-- | @Selector@ for @paused@
pausedSelector :: Selector
pausedSelector = mkSelector "paused"

-- | @Selector@ for @outputChannels@
outputChannelsSelector :: Selector
outputChannelsSelector = mkSelector "outputChannels"

-- | @Selector@ for @setOutputChannels:@
setOutputChannelsSelector :: Selector
setOutputChannelsSelector = mkSelector "setOutputChannels:"

-- | @Selector@ for @usesApplicationAudioSession@
usesApplicationAudioSessionSelector :: Selector
usesApplicationAudioSessionSelector = mkSelector "usesApplicationAudioSession"

-- | @Selector@ for @setUsesApplicationAudioSession:@
setUsesApplicationAudioSessionSelector :: Selector
setUsesApplicationAudioSessionSelector = mkSelector "setUsesApplicationAudioSession:"

-- | @Selector@ for @mixToTelephonyUplink@
mixToTelephonyUplinkSelector :: Selector
mixToTelephonyUplinkSelector = mkSelector "mixToTelephonyUplink"

-- | @Selector@ for @setMixToTelephonyUplink:@
setMixToTelephonyUplinkSelector :: Selector
setMixToTelephonyUplinkSelector = mkSelector "setMixToTelephonyUplink:"

-- | @Selector@ for @personalVoiceAuthorizationStatus@
personalVoiceAuthorizationStatusSelector :: Selector
personalVoiceAuthorizationStatusSelector = mkSelector "personalVoiceAuthorizationStatus"

