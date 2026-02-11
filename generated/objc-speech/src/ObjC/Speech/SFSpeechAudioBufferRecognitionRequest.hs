{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request to recognize speech from captured audio content, such as audio from the device's microphone.
--
-- Use an ``SFSpeechAudioBufferRecognitionRequest`` object to perform speech recognition on live audio, or on a set of existing audio buffers. For example, use this request object to route audio from a device's microphone to the speech recognizer.
--
-- The request object contains no audio initially. As you capture audio, call ``append(_:)`` or ``appendAudioSampleBuffer(_:)`` to add audio samples to the request object. The speech recognizer continuously analyzes the audio you appended, stopping only when you call the ``endAudio()`` method. You must call ``endAudio()`` explicitly to stop the speech recognition process.
--
-- For a complete example of how to use audio buffers with speech recognition, see [SpeakToMe: Using Speech Recognition with AVAudioEngine](https://developer.apple.com/library/archive/samplecode/SpeakToMe/Introduction/Intro.html#//apple_ref/doc/uid/TP40017110).
--
-- Generated bindings for @SFSpeechAudioBufferRecognitionRequest@.
module ObjC.Speech.SFSpeechAudioBufferRecognitionRequest
  ( SFSpeechAudioBufferRecognitionRequest
  , IsSFSpeechAudioBufferRecognitionRequest(..)
  , appendAudioPCMBuffer
  , appendAudioSampleBuffer
  , endAudio
  , nativeAudioFormat
  , appendAudioPCMBufferSelector
  , appendAudioSampleBufferSelector
  , endAudioSelector
  , nativeAudioFormatSelector


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
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Appends audio in the PCM format to the end of the recognition request.
--
-- The audio must be in a native format and uncompressed.
--
-- - Parameters:   - audioPCMBuffer: An audio buffer that contains audio in the PCM format.
--
-- ObjC selector: @- appendAudioPCMBuffer:@
appendAudioPCMBuffer :: (IsSFSpeechAudioBufferRecognitionRequest sfSpeechAudioBufferRecognitionRequest, IsAVAudioPCMBuffer audioPCMBuffer) => sfSpeechAudioBufferRecognitionRequest -> audioPCMBuffer -> IO ()
appendAudioPCMBuffer sfSpeechAudioBufferRecognitionRequest  audioPCMBuffer =
withObjCPtr audioPCMBuffer $ \raw_audioPCMBuffer ->
    sendMsg sfSpeechAudioBufferRecognitionRequest (mkSelector "appendAudioPCMBuffer:") retVoid [argPtr (castPtr raw_audioPCMBuffer :: Ptr ())]

-- | Appends audio to the end of the recognition request.
--
-- The audio must be in a native format.
--
-- - Parameters:   - sampleBuffer: A buffer of audio.
--
-- ObjC selector: @- appendAudioSampleBuffer:@
appendAudioSampleBuffer :: IsSFSpeechAudioBufferRecognitionRequest sfSpeechAudioBufferRecognitionRequest => sfSpeechAudioBufferRecognitionRequest -> Ptr () -> IO ()
appendAudioSampleBuffer sfSpeechAudioBufferRecognitionRequest  sampleBuffer =
  sendMsg sfSpeechAudioBufferRecognitionRequest (mkSelector "appendAudioSampleBuffer:") retVoid [argPtr sampleBuffer]

-- | Marks the end of audio input for the recognition request.
--
-- Call this method explicitly to let the speech recognizer know that no more audio input is coming.
--
-- ObjC selector: @- endAudio@
endAudio :: IsSFSpeechAudioBufferRecognitionRequest sfSpeechAudioBufferRecognitionRequest => sfSpeechAudioBufferRecognitionRequest -> IO ()
endAudio sfSpeechAudioBufferRecognitionRequest  =
  sendMsg sfSpeechAudioBufferRecognitionRequest (mkSelector "endAudio") retVoid []

-- | The preferred audio format for optimal speech recognition.
--
-- Use the audio format in this property as a hint for optimal recording, but don't depend on the value remaining unchanged.
--
-- ObjC selector: @- nativeAudioFormat@
nativeAudioFormat :: IsSFSpeechAudioBufferRecognitionRequest sfSpeechAudioBufferRecognitionRequest => sfSpeechAudioBufferRecognitionRequest -> IO (Id AVAudioFormat)
nativeAudioFormat sfSpeechAudioBufferRecognitionRequest  =
  sendMsg sfSpeechAudioBufferRecognitionRequest (mkSelector "nativeAudioFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @appendAudioPCMBuffer:@
appendAudioPCMBufferSelector :: Selector
appendAudioPCMBufferSelector = mkSelector "appendAudioPCMBuffer:"

-- | @Selector@ for @appendAudioSampleBuffer:@
appendAudioSampleBufferSelector :: Selector
appendAudioSampleBufferSelector = mkSelector "appendAudioSampleBuffer:"

-- | @Selector@ for @endAudio@
endAudioSelector :: Selector
endAudioSelector = mkSelector "endAudio"

-- | @Selector@ for @nativeAudioFormat@
nativeAudioFormatSelector :: Selector
nativeAudioFormatSelector = mkSelector "nativeAudioFormat"

