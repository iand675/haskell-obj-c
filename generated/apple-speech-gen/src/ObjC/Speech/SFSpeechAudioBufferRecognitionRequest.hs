{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
appendAudioPCMBuffer sfSpeechAudioBufferRecognitionRequest audioPCMBuffer =
  sendMessage sfSpeechAudioBufferRecognitionRequest appendAudioPCMBufferSelector (toAVAudioPCMBuffer audioPCMBuffer)

-- | Appends audio to the end of the recognition request.
--
-- The audio must be in a native format.
--
-- - Parameters:   - sampleBuffer: A buffer of audio.
--
-- ObjC selector: @- appendAudioSampleBuffer:@
appendAudioSampleBuffer :: IsSFSpeechAudioBufferRecognitionRequest sfSpeechAudioBufferRecognitionRequest => sfSpeechAudioBufferRecognitionRequest -> Ptr () -> IO ()
appendAudioSampleBuffer sfSpeechAudioBufferRecognitionRequest sampleBuffer =
  sendMessage sfSpeechAudioBufferRecognitionRequest appendAudioSampleBufferSelector sampleBuffer

-- | Marks the end of audio input for the recognition request.
--
-- Call this method explicitly to let the speech recognizer know that no more audio input is coming.
--
-- ObjC selector: @- endAudio@
endAudio :: IsSFSpeechAudioBufferRecognitionRequest sfSpeechAudioBufferRecognitionRequest => sfSpeechAudioBufferRecognitionRequest -> IO ()
endAudio sfSpeechAudioBufferRecognitionRequest =
  sendMessage sfSpeechAudioBufferRecognitionRequest endAudioSelector

-- | The preferred audio format for optimal speech recognition.
--
-- Use the audio format in this property as a hint for optimal recording, but don't depend on the value remaining unchanged.
--
-- ObjC selector: @- nativeAudioFormat@
nativeAudioFormat :: IsSFSpeechAudioBufferRecognitionRequest sfSpeechAudioBufferRecognitionRequest => sfSpeechAudioBufferRecognitionRequest -> IO (Id AVAudioFormat)
nativeAudioFormat sfSpeechAudioBufferRecognitionRequest =
  sendMessage sfSpeechAudioBufferRecognitionRequest nativeAudioFormatSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @appendAudioPCMBuffer:@
appendAudioPCMBufferSelector :: Selector '[Id AVAudioPCMBuffer] ()
appendAudioPCMBufferSelector = mkSelector "appendAudioPCMBuffer:"

-- | @Selector@ for @appendAudioSampleBuffer:@
appendAudioSampleBufferSelector :: Selector '[Ptr ()] ()
appendAudioSampleBufferSelector = mkSelector "appendAudioSampleBuffer:"

-- | @Selector@ for @endAudio@
endAudioSelector :: Selector '[] ()
endAudioSelector = mkSelector "endAudio"

-- | @Selector@ for @nativeAudioFormat@
nativeAudioFormatSelector :: Selector '[] (Id AVAudioFormat)
nativeAudioFormatSelector = mkSelector "nativeAudioFormat"

