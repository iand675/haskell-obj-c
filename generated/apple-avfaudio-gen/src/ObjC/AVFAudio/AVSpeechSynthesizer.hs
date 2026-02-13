{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , continueSpeakingSelector
  , delegateSelector
  , mixToTelephonyUplinkSelector
  , outputChannelsSelector
  , pauseSpeakingAtBoundarySelector
  , pausedSelector
  , personalVoiceAuthorizationStatusSelector
  , requestPersonalVoiceAuthorizationWithCompletionHandlerSelector
  , setDelegateSelector
  , setMixToTelephonyUplinkSelector
  , setOutputChannelsSelector
  , setUsesApplicationAudioSessionSelector
  , speakUtteranceSelector
  , speakingSelector
  , stopSpeakingAtBoundarySelector
  , usesApplicationAudioSessionSelector
  , writeUtterance_toBufferCallbackSelector
  , writeUtterance_toBufferCallback_toMarkerCallbackSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- speakUtterance:@
speakUtterance :: (IsAVSpeechSynthesizer avSpeechSynthesizer, IsAVSpeechUtterance utterance) => avSpeechSynthesizer -> utterance -> IO ()
speakUtterance avSpeechSynthesizer utterance =
  sendMessage avSpeechSynthesizer speakUtteranceSelector (toAVSpeechUtterance utterance)

-- | @- writeUtterance:toBufferCallback:@
writeUtterance_toBufferCallback :: (IsAVSpeechSynthesizer avSpeechSynthesizer, IsAVSpeechUtterance utterance) => avSpeechSynthesizer -> utterance -> Ptr () -> IO ()
writeUtterance_toBufferCallback avSpeechSynthesizer utterance bufferCallback =
  sendMessage avSpeechSynthesizer writeUtterance_toBufferCallbackSelector (toAVSpeechUtterance utterance) bufferCallback

-- | Use this method to receive audio buffers and associated metadata that can be used to store or further process synthesized speech. The dictionary provided by -[AVSpeechSynthesisVoice audioFileSettings] can be used to create an AVAudioFile.
--
-- ObjC selector: @- writeUtterance:toBufferCallback:toMarkerCallback:@
writeUtterance_toBufferCallback_toMarkerCallback :: (IsAVSpeechSynthesizer avSpeechSynthesizer, IsAVSpeechUtterance utterance) => avSpeechSynthesizer -> utterance -> Ptr () -> Ptr () -> IO ()
writeUtterance_toBufferCallback_toMarkerCallback avSpeechSynthesizer utterance bufferCallback markerCallback =
  sendMessage avSpeechSynthesizer writeUtterance_toBufferCallback_toMarkerCallbackSelector (toAVSpeechUtterance utterance) bufferCallback markerCallback

-- | @- stopSpeakingAtBoundary:@
stopSpeakingAtBoundary :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> AVSpeechBoundary -> IO Bool
stopSpeakingAtBoundary avSpeechSynthesizer boundary =
  sendMessage avSpeechSynthesizer stopSpeakingAtBoundarySelector boundary

-- | @- pauseSpeakingAtBoundary:@
pauseSpeakingAtBoundary :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> AVSpeechBoundary -> IO Bool
pauseSpeakingAtBoundary avSpeechSynthesizer boundary =
  sendMessage avSpeechSynthesizer pauseSpeakingAtBoundarySelector boundary

-- | @- continueSpeaking@
continueSpeaking :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO Bool
continueSpeaking avSpeechSynthesizer =
  sendMessage avSpeechSynthesizer continueSpeakingSelector

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
    sendClassMessage cls' requestPersonalVoiceAuthorizationWithCompletionHandlerSelector handler

-- | @- delegate@
delegate :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO RawId
delegate avSpeechSynthesizer =
  sendMessage avSpeechSynthesizer delegateSelector

-- | @- setDelegate:@
setDelegate :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> RawId -> IO ()
setDelegate avSpeechSynthesizer value =
  sendMessage avSpeechSynthesizer setDelegateSelector value

-- | @- speaking@
speaking :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO Bool
speaking avSpeechSynthesizer =
  sendMessage avSpeechSynthesizer speakingSelector

-- | @- paused@
paused :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO Bool
paused avSpeechSynthesizer =
  sendMessage avSpeechSynthesizer pausedSelector

-- | @- outputChannels@
outputChannels :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO (Id NSArray)
outputChannels avSpeechSynthesizer =
  sendMessage avSpeechSynthesizer outputChannelsSelector

-- | @- setOutputChannels:@
setOutputChannels :: (IsAVSpeechSynthesizer avSpeechSynthesizer, IsNSArray value) => avSpeechSynthesizer -> value -> IO ()
setOutputChannels avSpeechSynthesizer value =
  sendMessage avSpeechSynthesizer setOutputChannelsSelector (toNSArray value)

-- | @- usesApplicationAudioSession@
usesApplicationAudioSession :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO Bool
usesApplicationAudioSession avSpeechSynthesizer =
  sendMessage avSpeechSynthesizer usesApplicationAudioSessionSelector

-- | @- setUsesApplicationAudioSession:@
setUsesApplicationAudioSession :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> Bool -> IO ()
setUsesApplicationAudioSession avSpeechSynthesizer value =
  sendMessage avSpeechSynthesizer setUsesApplicationAudioSessionSelector value

-- | @- mixToTelephonyUplink@
mixToTelephonyUplink :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> IO Bool
mixToTelephonyUplink avSpeechSynthesizer =
  sendMessage avSpeechSynthesizer mixToTelephonyUplinkSelector

-- | @- setMixToTelephonyUplink:@
setMixToTelephonyUplink :: IsAVSpeechSynthesizer avSpeechSynthesizer => avSpeechSynthesizer -> Bool -> IO ()
setMixToTelephonyUplink avSpeechSynthesizer value =
  sendMessage avSpeechSynthesizer setMixToTelephonyUplinkSelector value

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
    sendClassMessage cls' personalVoiceAuthorizationStatusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @speakUtterance:@
speakUtteranceSelector :: Selector '[Id AVSpeechUtterance] ()
speakUtteranceSelector = mkSelector "speakUtterance:"

-- | @Selector@ for @writeUtterance:toBufferCallback:@
writeUtterance_toBufferCallbackSelector :: Selector '[Id AVSpeechUtterance, Ptr ()] ()
writeUtterance_toBufferCallbackSelector = mkSelector "writeUtterance:toBufferCallback:"

-- | @Selector@ for @writeUtterance:toBufferCallback:toMarkerCallback:@
writeUtterance_toBufferCallback_toMarkerCallbackSelector :: Selector '[Id AVSpeechUtterance, Ptr (), Ptr ()] ()
writeUtterance_toBufferCallback_toMarkerCallbackSelector = mkSelector "writeUtterance:toBufferCallback:toMarkerCallback:"

-- | @Selector@ for @stopSpeakingAtBoundary:@
stopSpeakingAtBoundarySelector :: Selector '[AVSpeechBoundary] Bool
stopSpeakingAtBoundarySelector = mkSelector "stopSpeakingAtBoundary:"

-- | @Selector@ for @pauseSpeakingAtBoundary:@
pauseSpeakingAtBoundarySelector :: Selector '[AVSpeechBoundary] Bool
pauseSpeakingAtBoundarySelector = mkSelector "pauseSpeakingAtBoundary:"

-- | @Selector@ for @continueSpeaking@
continueSpeakingSelector :: Selector '[] Bool
continueSpeakingSelector = mkSelector "continueSpeaking"

-- | @Selector@ for @requestPersonalVoiceAuthorizationWithCompletionHandler:@
requestPersonalVoiceAuthorizationWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
requestPersonalVoiceAuthorizationWithCompletionHandlerSelector = mkSelector "requestPersonalVoiceAuthorizationWithCompletionHandler:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @speaking@
speakingSelector :: Selector '[] Bool
speakingSelector = mkSelector "speaking"

-- | @Selector@ for @paused@
pausedSelector :: Selector '[] Bool
pausedSelector = mkSelector "paused"

-- | @Selector@ for @outputChannels@
outputChannelsSelector :: Selector '[] (Id NSArray)
outputChannelsSelector = mkSelector "outputChannels"

-- | @Selector@ for @setOutputChannels:@
setOutputChannelsSelector :: Selector '[Id NSArray] ()
setOutputChannelsSelector = mkSelector "setOutputChannels:"

-- | @Selector@ for @usesApplicationAudioSession@
usesApplicationAudioSessionSelector :: Selector '[] Bool
usesApplicationAudioSessionSelector = mkSelector "usesApplicationAudioSession"

-- | @Selector@ for @setUsesApplicationAudioSession:@
setUsesApplicationAudioSessionSelector :: Selector '[Bool] ()
setUsesApplicationAudioSessionSelector = mkSelector "setUsesApplicationAudioSession:"

-- | @Selector@ for @mixToTelephonyUplink@
mixToTelephonyUplinkSelector :: Selector '[] Bool
mixToTelephonyUplinkSelector = mkSelector "mixToTelephonyUplink"

-- | @Selector@ for @setMixToTelephonyUplink:@
setMixToTelephonyUplinkSelector :: Selector '[Bool] ()
setMixToTelephonyUplinkSelector = mkSelector "setMixToTelephonyUplink:"

-- | @Selector@ for @personalVoiceAuthorizationStatus@
personalVoiceAuthorizationStatusSelector :: Selector '[] AVSpeechSynthesisPersonalVoiceAuthorizationStatus
personalVoiceAuthorizationStatusSelector = mkSelector "personalVoiceAuthorizationStatus"

